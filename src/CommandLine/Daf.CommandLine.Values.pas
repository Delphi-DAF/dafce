unit Daf.CommandLine.Values;

{
  The value engine.

  It is written from scratch rather than carried over from Daf.CmdLn, and the two
  measured defects of that one say why. There an Integer was described by the regular
  expression \d+ and the match was anchored only at the start, so -5 could not be
  passed at all and --port 80xyz was accepted as the value 80: a number invented out
  of an invalid invocation, which is worse than refusing it.

  Here a conversion consumes the WHOLE token or fails. Nothing is anchored by hand,
  because nothing is matched by pattern: TryStrToInt and TryStrToFloat already answer
  "does this entire string denote one value", which is the question that was being
  asked badly.
}

interface

uses
  System.Rtti,
  System.TypInfo;

type
  /// <summary>
  ///   Converts the raw text of a token into the type a symbol was declared with.
  /// </summary>
  /// <remarks>
  ///   Every entry point is total: it answers True with a value, or False, and never
  ///   produces a value from text it did not fully consume.
  /// </remarks>
  TCommandLineValue = record
  public
    /// <summary>True when the engine can convert text into <c>TypeInf</c>.</summary>
    class function Handles(TypeInf: PTypeInfo): Boolean; static;
    /// <summary>Name of the declared type, for help and for error messages.</summary>
    class function TypeName(TypeInf: PTypeInfo): string; static;
    /// <summary>
    ///   True for a type that carries no value of its own, so the token is the whole
    ///   statement: <c>--verbose</c>, not <c>--verbose true</c>.
    /// </summary>
    class function IsFlag(TypeInf: PTypeInfo): Boolean; static;
    /// <summary>True for an enumerated type, which is the one that names its own values.</summary>
    class function IsEnum(TypeInf: PTypeInfo): Boolean; static;
    /// <summary>
    ///   The values the type itself admits, or empty when the type does not say. An
    ///   enumerated type already knows them, so nobody should have to write them again.
    /// </summary>
    class function AcceptedValuesOf(TypeInf: PTypeInfo): TArray<string>; static;
    /// <summary>True when <c>Raw</c> denotes a boolean, and only then.</summary>
    class function IsBooleanLiteral(const Raw: string): Boolean; static;
    /// <summary>Converts, or answers False. Never converts partially.</summary>
    class function TryParse(TypeInf: PTypeInfo; const Raw: string; out Value: TValue): Boolean; static;
  end;

implementation

uses
  System.SysUtils;

var
  // Invariant, so that 1.5 means the same on every machine. A command line is not
  // locale-dependent text: it is what a script wrote down.
  InvariantSettings: TFormatSettings;

class function TCommandLineValue.IsFlag(TypeInf: PTypeInfo): Boolean;
begin
  Result := TypeInf = System.TypeInfo(Boolean);
end;

class function TCommandLineValue.IsEnum(TypeInf: PTypeInfo): Boolean;
begin
  // Boolean is an enumeration too, and it is NOT one of these: it is the flag, with a
  // parse of its own that also takes 1 and 0.
  Result := (TypeInf <> nil) and (TypeInf.Kind = tkEnumeration) and not IsFlag(TypeInf) and
    (TypeInf <> System.TypeInfo(ByteBool)) and (TypeInf <> System.TypeInfo(WordBool)) and
    (TypeInf <> System.TypeInfo(LongBool));
end;

class function TCommandLineValue.Handles(TypeInf: PTypeInfo): Boolean;
begin
  Result := (TypeInf = System.TypeInfo(string)) or (TypeInf = System.TypeInfo(Integer)) or
    (TypeInf = System.TypeInfo(Boolean)) or (TypeInf = System.TypeInfo(Double)) or IsEnum(TypeInf);
end;

class function TCommandLineValue.AcceptedValuesOf(TypeInf: PTypeInfo): TArray<string>;
var
  Data: PTypeData;
  Ordinal: Integer;
begin
  Result := nil;
  if not IsEnum(TypeInf) then
    Exit;
  Data := GetTypeData(TypeInf);
  for Ordinal := Data.MinValue to Data.MaxValue do
    Result := Result + [GetEnumName(TypeInf, Ordinal)];
end;

class function TCommandLineValue.TypeName(TypeInf: PTypeInfo): string;
begin
  if TypeInf = System.TypeInfo(string) then
    Result := 'string'
  else if TypeInf = System.TypeInfo(Integer) then
    Result := 'integer'
  else if TypeInf = System.TypeInfo(Boolean) then
    Result := 'boolean'
  else if TypeInf = System.TypeInfo(Double) then
    Result := 'number'
  else if TypeInf <> nil then
    Result := string(TypeInf.Name)
  else
    Result := 'unknown';
end;

class function TCommandLineValue.IsBooleanLiteral(const Raw: string): Boolean;
begin
  Result := SameText(Raw, 'true') or SameText(Raw, 'false') or (Raw = '1') or (Raw = '0');
end;

class function TCommandLineValue.TryParse(TypeInf: PTypeInfo; const Raw: string; out Value: TValue): Boolean;
var
  IntValue: Integer;
  FloatValue: Double;
  Data: PTypeData;
  Ordinal: Integer;
begin
  Value := TValue.Empty;

  if TypeInf = System.TypeInfo(string) then
  begin
    Value := TValue.From<string>(Raw);
    Exit(True);
  end;

  if IsFlag(TypeInf) then
  begin
    if SameText(Raw, 'true') or (Raw = '1') then
    begin
      Value := TValue.From<Boolean>(True);
      Exit(True);
    end;
    if SameText(Raw, 'false') or (Raw = '0') then
    begin
      Value := TValue.From<Boolean>(False);
      Exit(True);
    end;
    Exit(False);
  end;

  if TypeInf = System.TypeInfo(Integer) then
  begin
    // TryStrToInt converts the whole token or fails. That is the anchoring at both
    // ends the parked engine lacked: -5 parses, 80xyz does not become 80.
    if not TryStrToInt(Raw, IntValue) then
      Exit(False);
    Value := TValue.From<Integer>(IntValue);
    Exit(True);
  end;

  if TypeInf = System.TypeInfo(Double) then
  begin
    if not TryStrToFloat(Raw, FloatValue, InvariantSettings) then
      Exit(False);
    Value := TValue.From<Double>(FloatValue);
    Exit(True);
  end;

  if IsEnum(TypeInf) then
  begin
    // Matched without regard to case, as .NET does for enum values: the casing of a
    // Pascal identifier is not something a script author should have to guess.
    Data := GetTypeData(TypeInf);
    for Ordinal := Data.MinValue to Data.MaxValue do
      if SameText(Raw, GetEnumName(TypeInf, Ordinal)) then
      begin
        Value := TValue.FromOrdinal(TypeInf, Ordinal);
        Exit(True);
      end;
    Exit(False);
  end;

  Result := False;
end;

initialization

InvariantSettings := TFormatSettings.Invariant;

end.
