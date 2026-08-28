unit Daf.CommandLine.Values;

{
  The value engine.

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
    ///   True for a collection type, which is a symbol that takes many values rather
    ///   than a value with many parts: its arity comes out of the type, as in .NET.
    /// </summary>
    class function IsCollection(TypeInf: PTypeInfo): Boolean; static;
    /// <summary>
    ///   The type ONE token converts into: the element type of a collection, and the
    ///   type itself for anything else.
    /// </summary>
    class function ElementTypeOf(TypeInf: PTypeInfo): PTypeInfo; static;
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
  System.SysUtils,
  System.DateUtils;

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

class function TCommandLineValue.IsCollection(TypeInf: PTypeInfo): Boolean;
var
  Element: PTypeInfo;
begin
  // A string is not a dynamic array as far as RTTI is concerned, so it does not fall in
  // here by accident.
  Result := False;
  if (TypeInf = nil) or (TypeInf.Kind <> tkDynArray) then
    Exit;
  Element := ElementTypeOf(TypeInf);
  // A collection of collections is not a command line: there is one level of "many
  // values", and it is the arity.
  Result := (Element <> TypeInf) and (Element <> nil) and (Element.Kind <> tkDynArray) and Handles(Element);
end;

class function TCommandLineValue.ElementTypeOf(TypeInf: PTypeInfo): PTypeInfo;
var
  Data: PTypeData;
begin
  Result := TypeInf;
  if (TypeInf = nil) or (TypeInf.Kind <> tkDynArray) then
    Exit;
  Data := GetTypeData(TypeInf);
  // elType2 is the one always filled in; elType only carries a type that needs
  // finalizing, so reading it instead answers nil for an array of integers.
  if Data.elType2 <> nil then
    Result := Data.elType2^;
end;

class function TCommandLineValue.Handles(TypeInf: PTypeInfo): Boolean;
begin
  // The list is translated from .NET's, not copied from it: what a command line needs is
  // numbers of the widths people use, dates, identifiers and paths. Here a
  // path is a string, so it is already in. DateOnly and TimeOnly have no Delphi type of
  // their own, and TDateTime covers what interested them -- a declared divergence.
  Result := (TypeInf = System.TypeInfo(string)) or (TypeInf = System.TypeInfo(Integer)) or
    (TypeInf = System.TypeInfo(Int64)) or (TypeInf = System.TypeInfo(Boolean)) or
    (TypeInf = System.TypeInfo(Double)) or (TypeInf = System.TypeInfo(Currency)) or
    (TypeInf = System.TypeInfo(TDateTime)) or (TypeInf = System.TypeInfo(TGUID)) or
    IsEnum(TypeInf) or IsCollection(TypeInf);
end;

class function TCommandLineValue.AcceptedValuesOf(TypeInf: PTypeInfo): TArray<string>;
var
  Data: PTypeData;
  Ordinal: Integer;
begin
  Result := nil;
  // Through the element, so a collection of an enumerated type still names its values.
  TypeInf := ElementTypeOf(TypeInf);
  if not IsEnum(TypeInf) then
    Exit;
  Data := GetTypeData(TypeInf);
  for Ordinal := Data.MinValue to Data.MaxValue do
    Result := Result + [GetEnumName(TypeInf, Ordinal)];
end;

class function TCommandLineValue.TypeName(TypeInf: PTypeInfo): string;
begin
  // The name of what ONE token has to be: the arity already says how many of them fit,
  // so a collection reads as its element in the help and in every error message.
  TypeInf := ElementTypeOf(TypeInf);
  if TypeInf = System.TypeInfo(string) then
    Result := 'string'
  else if (TypeInf = System.TypeInfo(Integer)) or (TypeInf = System.TypeInfo(Int64)) then
    Result := 'integer'
  else if TypeInf = System.TypeInfo(Boolean) then
    Result := 'boolean'
  else if TypeInf = System.TypeInfo(Double) then
    Result := 'number'
  else if TypeInf = System.TypeInfo(Currency) then
    // .NET calls it decimal, and so does the help: what tells it from 'number' is that
    // it does not round on its own, which is the only reason to choose it.
    Result := 'decimal'
  else if TypeInf = System.TypeInfo(TDateTime) then
    Result := 'date'
  else if TypeInf = System.TypeInfo(TGUID) then
    Result := 'guid'
  else if TypeInf <> nil then
    Result := string(TypeInf.Name)
  else
    Result := 'unknown';
end;

class function TCommandLineValue.IsBooleanLiteral(const Raw: string): Boolean;
begin
  Result := SameText(Raw, 'true') or SameText(Raw, 'false') or (Raw = '1') or (Raw = '0');
end;

// The braces StringToGUID demands are optional here: a command line writes an
// identifier either way, and both denote the same one. The shape is checked first so
// that StringToGUID cannot raise -- an exception escaping the engine would turn an
// invalid invocation into a crash, which is the failure this module is built against.
function TryParseGuid(const Raw: string; out Value: TGUID): Boolean;
const
  Shape: string = 'xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx';
var
  Body: string;
  I: Integer;
begin
  Result := False;
  Body := Raw;
  if (Length(Body) >= 2) and (Body[1] = '{') and (Body[Length(Body)] = '}') then
    Body := Copy(Body, 2, Length(Body) - 2);
  if Length(Body) <> Length(Shape) then
    Exit;
  for I := 1 to Length(Shape) do
    if Shape[I] = '-' then
    begin
      if Body[I] <> '-' then
        Exit;
    end
    else if not CharInSet(Body[I], ['0' .. '9', 'a' .. 'f', 'A' .. 'F']) then
      Exit;
  Value := StringToGUID('{' + Body + '}');
  Result := True;
end;

class function TCommandLineValue.TryParse(TypeInf: PTypeInfo; const Raw: string; out Value: TValue): Boolean;
var
  IntValue: Integer;
  BigValue: Int64;
  FloatValue: Double;
  CurrValue: Currency;
  DateValue: TDateTime;
  GuidValue: TGUID;
  Data: PTypeData;
  Ordinal: Integer;
begin
  Value := TValue.Empty;
  // Asked about what ONE token becomes. A collection never reaches the conversions
  // below: each of its tokens is converted as an element.
  TypeInf := ElementTypeOf(TypeInf);

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

  if TypeInf = System.TypeInfo(Int64) then
  begin
    if not TryStrToInt64(Raw, BigValue) then
      Exit(False);
    Value := TValue.From<Int64>(BigValue);
    Exit(True);
  end;

  if TypeInf = System.TypeInfo(Double) then
  begin
    if not TryStrToFloat(Raw, FloatValue, InvariantSettings) then
      Exit(False);
    Value := TValue.From<Double>(FloatValue);
    Exit(True);
  end;

  if TypeInf = System.TypeInfo(Currency) then
  begin
    // The exact decimal: a scaled 64-bit integer, so an amount written with up to four
    // decimals comes back with those four and no binary rounding of its own. That is the
    // whole reason to declare it instead of Double.
    if not TryStrToCurr(Raw, CurrValue, InvariantSettings) then
      Exit(False);
    Value := TValue.From<Currency>(CurrValue);
    Exit(True);
  end;

  if TypeInf = System.TypeInfo(TDateTime) then
  begin
    // ISO 8601 and nothing else, which is a declared divergence from .NET's DateTime.
    // Accepting a local convention as well would mean 01/02/2026 denoting two different
    // days depending on who read it, and a command line is what a script wrote down.
    // AReturnUTC is off so the date read is the date written, not one shifted by a zone
    // nobody named.
    if not TryISO8601ToDate(Raw, DateValue, False) then
      Exit(False);
    Value := TValue.From<TDateTime>(DateValue);
    Exit(True);
  end;

  if TypeInf = System.TypeInfo(TGUID) then
  begin
    if not TryParseGuid(Raw, GuidValue) then
      Exit(False);
    Value := TValue.From<TGUID>(GuidValue);
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
