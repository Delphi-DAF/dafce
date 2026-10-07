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
  /// <summary>The path of a file: a string the engine knows to be one, as .NET's <c>FileInfo</c>.</summary>
  TFilePath = type string;
  /// <summary>The path of a directory: a string the engine knows to be one, as .NET's <c>DirectoryInfo</c>.</summary>
  TDirectoryPath = type string;

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
    /// <remarks>A boolean, and a boolean that may be missing, as .NET's <c>bool?</c>.</remarks>
    class function IsFlag(TypeInf: PTypeInfo): Boolean; static;
    /// <summary>What a flag named with no value holds: true, in the type it was declared with.</summary>
    class function FlagValue(TypeInf: PTypeInfo): TValue; static;
    /// <summary>True for an enumerated type, which is the one that names its own values.</summary>
    class function IsEnum(TypeInf: PTypeInfo): Boolean; static;
    /// <summary>True for a set of an enumerated type, written as its names separated by commas.</summary>
    class function IsEnumSet(TypeInf: PTypeInfo): Boolean; static;
    /// <summary>The type a <c>TNullable&lt;T&gt;</c> holds, or <c>nil</c> for any other type.</summary>
    /// <remarks>
    ///   Recognised by its name and its two fields, both: the qualified name of a generic instance
    ///   raises <c>ENonPublicType</c>, and a record of the application with the same fields and another
    ///   name is not this type.
    /// </remarks>
    class function NullableValueTypeOf(TypeInf: PTypeInfo): PTypeInfo; static;
    /// <summary>True for what a path validation applies to: a string, a file path, a directory path.</summary>
    class function IsPath(TypeInf: PTypeInfo): Boolean; static;
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
    /// <remarks>
    ///   <c>true</c> and <c>false</c>, in any casing, which is what .NET's <c>bool.TryParse</c> takes.
    ///   <c>1</c> and <c>0</c> are not booleans: behind a flag they are the next positional.
    /// </remarks>
    class function IsBooleanLiteral(const Raw: string): Boolean; static;
    /// <summary>Converts, or answers False. Never converts partially.</summary>
    class function TryParse(TypeInf: PTypeInfo; const Raw: string; out Value: TValue): Boolean; static;
    /// <summary>A value as the help shows it: a missing one is empty, and a present one is what it holds.</summary>
    class function ToText(const Value: TValue): string; static;
  end;

implementation

uses
  System.SysUtils,
  System.Math,
  System.DateUtils,
  System.TimeSpan;

var
  // Invariant, so that 1.5 means the same on every machine. A command line is not
  // locale-dependent text: it is what a script wrote down.
  InvariantSettings: TFormatSettings;

// A TNullable<T> holding Inner, built through RTTI: the engine knows T only as type information.
function MakeNullable(TypeInf: PTypeInfo; const Inner: TValue): TValue;
var
  Context: TRttiContext;
  Declared: TRttiType;
begin
  TValue.Make(nil, TypeInf, Result);
  Declared := Context.GetType(TypeInf);
  Declared.GetField('FValue').SetValue(Result.GetReferenceToRawData, Inner);
  Declared.GetField('FHasValue').SetValue(Result.GetReferenceToRawData, True);
end;

class function TCommandLineValue.NullableValueTypeOf(TypeInf: PTypeInfo): PTypeInfo;
var
  Context: TRttiContext;
  Declared: TRttiType;
  ValueField, HasValueField: TRttiField;
begin
  Result := nil;
  // By kind first, so every other type leaves without asking RTTI. TNullable is tkMRecord, because it
  // declares an Initialize operator.
  if (TypeInf = nil) or not (TypeInf.Kind in [tkRecord, tkMRecord]) then
    Exit;
  Declared := Context.GetType(TypeInf);
  if (Declared = nil) or not Declared.Name.StartsWith('TNullable<') then
    Exit;
  ValueField := Declared.GetField('FValue');
  HasValueField := Declared.GetField('FHasValue');
  if (ValueField = nil) or (ValueField.FieldType = nil) or (HasValueField = nil) or
    (HasValueField.FieldType = nil) or (HasValueField.FieldType.Handle <> System.TypeInfo(Boolean)) then
    Exit;
  Result := ValueField.FieldType.Handle;
end;

class function TCommandLineValue.IsFlag(TypeInf: PTypeInfo): Boolean;
begin
  Result := (TypeInf = System.TypeInfo(Boolean)) or (NullableValueTypeOf(TypeInf) = System.TypeInfo(Boolean));
end;

class function TCommandLineValue.FlagValue(TypeInf: PTypeInfo): TValue;
begin
  Result := TValue.From<Boolean>(True);
  if NullableValueTypeOf(TypeInf) <> nil then
    Result := MakeNullable(TypeInf, Result);
end;

class function TCommandLineValue.IsPath(TypeInf: PTypeInfo): Boolean;
begin
  Result := (TypeInf = System.TypeInfo(string)) or (TypeInf = System.TypeInfo(TFilePath)) or
    (TypeInf = System.TypeInfo(TDirectoryPath));
end;

class function TCommandLineValue.IsEnumSet(TypeInf: PTypeInfo): Boolean;
var
  Data: PTypeData;
begin
  Result := False;
  if (TypeInf = nil) or (TypeInf.Kind <> tkSet) then
    Exit;
  Data := GetTypeData(TypeInf);
  Result := (Data.CompType <> nil) and IsEnum(Data.CompType^);
end;

class function TCommandLineValue.IsEnum(TypeInf: PTypeInfo): Boolean;
begin
  // Boolean is an enumeration too, and it is NOT one of these: it is the flag, with a
  // parse of its own.
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

// The integers read through Int64 and checked against their range. Int64 itself and UInt64 have
// conversions of their own, which already answer for the whole range.
function TryIntegerRange(TypeInf: PTypeInfo; out Min, Max: Int64): Boolean;
begin
  Result := True;
  if TypeInf = System.TypeInfo(ShortInt) then
  begin
    Min := Low(ShortInt);
    Max := High(ShortInt);
  end
  else if TypeInf = System.TypeInfo(Byte) then
  begin
    Min := Low(Byte);
    Max := High(Byte);
  end
  else if TypeInf = System.TypeInfo(SmallInt) then
  begin
    Min := Low(SmallInt);
    Max := High(SmallInt);
  end
  else if TypeInf = System.TypeInfo(Word) then
  begin
    Min := Low(Word);
    Max := High(Word);
  end
  else if TypeInf = System.TypeInfo(Cardinal) then
  begin
    Min := Low(Cardinal);
    Max := High(Cardinal);
  end
  else
    Result := False;
end;

function IsUnsigned(TypeInf: PTypeInfo): Boolean;
begin
  Result := (TypeInf = System.TypeInfo(Byte)) or (TypeInf = System.TypeInfo(Word)) or
    (TypeInf = System.TypeInfo(Cardinal)) or (TypeInf = System.TypeInfo(UInt64));
end;

function IsInteger(TypeInf: PTypeInfo): Boolean;
var
  Min, Max: Int64;
begin
  Result := (TypeInf = System.TypeInfo(Integer)) or (TypeInf = System.TypeInfo(Int64)) or
    (TypeInf = System.TypeInfo(UInt64)) or TryIntegerRange(TypeInf, Min, Max);
end;

class function TCommandLineValue.Handles(TypeInf: PTypeInfo): Boolean;
var
  Inner: PTypeInfo;
begin
  // A value that may be missing, of any type the engine converts. Not of a collection, which already
  // says "none" by being empty, and not of another one.
  Inner := NullableValueTypeOf(TypeInf);
  if Inner <> nil then
    Exit(not IsCollection(Inner) and (NullableValueTypeOf(Inner) = nil) and Handles(Inner));
  // The list is translated from .NET's, not copied from it. DateTimeOffset is the one left out:
  // Delphi has no type for a date with its offset -- a declared divergence.
  Result := IsPath(TypeInf) or IsInteger(TypeInf) or (TypeInf = System.TypeInfo(Boolean)) or
    (TypeInf = System.TypeInfo(Single)) or (TypeInf = System.TypeInfo(Double)) or
    (TypeInf = System.TypeInfo(Currency)) or (TypeInf = System.TypeInfo(TDateTime)) or
    (TypeInf = System.TypeInfo(TDate)) or (TypeInf = System.TypeInfo(TTime)) or
    (TypeInf = System.TypeInfo(TTimeSpan)) or (TypeInf = System.TypeInfo(TGUID)) or IsEnum(TypeInf) or
    IsEnumSet(TypeInf) or IsCollection(TypeInf);
end;

class function TCommandLineValue.AcceptedValuesOf(TypeInf: PTypeInfo): TArray<string>;
var
  Inner: PTypeInfo;
  Data: PTypeData;
  Ordinal: Integer;
begin
  Result := nil;
  // Through the element, so a collection of an enumerated type still names its values; through what
  // may be missing, and through a set, whose token is written with those same names.
  TypeInf := ElementTypeOf(TypeInf);
  Inner := NullableValueTypeOf(TypeInf);
  if Inner <> nil then
    TypeInf := Inner;
  if IsEnumSet(TypeInf) then
    TypeInf := GetTypeData(TypeInf).CompType^;
  if not IsEnum(TypeInf) then
    Exit;
  Data := GetTypeData(TypeInf);
  for Ordinal := Data.MinValue to Data.MaxValue do
    Result := Result + [GetEnumName(TypeInf, Ordinal)];
end;

class function TCommandLineValue.TypeName(TypeInf: PTypeInfo): string;
var
  Inner: PTypeInfo;
begin
  // The name of what ONE token has to be: the arity already says how many of them fit,
  // so a collection reads as its element in the help and in every error message. A value
  // that may be missing reads as what it holds when it is there.
  TypeInf := ElementTypeOf(TypeInf);
  Inner := NullableValueTypeOf(TypeInf);
  if Inner <> nil then
    Exit(TypeName(Inner));
  if TypeInf = System.TypeInfo(string) then
    Result := 'string'
  else if TypeInf = System.TypeInfo(TFilePath) then
    Result := 'file'
  else if TypeInf = System.TypeInfo(TDirectoryPath) then
    Result := 'directory'
  else if IsInteger(TypeInf) and IsUnsigned(TypeInf) then
    Result := 'unsigned integer'
  else if IsInteger(TypeInf) then
    Result := 'integer'
  else if TypeInf = System.TypeInfo(Boolean) then
    Result := 'boolean'
  else if (TypeInf = System.TypeInfo(Single)) or (TypeInf = System.TypeInfo(Double)) then
    Result := 'number'
  else if TypeInf = System.TypeInfo(Currency) then
    // .NET calls it decimal, and so does the help: what tells it from 'number' is that
    // it does not round on its own, which is the only reason to choose it.
    Result := 'decimal'
  else if (TypeInf = System.TypeInfo(TDateTime)) or (TypeInf = System.TypeInfo(TDate)) then
    Result := 'date'
  else if TypeInf = System.TypeInfo(TTime) then
    Result := 'time'
  else if TypeInf = System.TypeInfo(TTimeSpan) then
    Result := 'time span'
  else if TypeInf = System.TypeInfo(TGUID) then
    Result := 'guid'
  else if TypeInf <> nil then
    Result := string(TypeInf.Name)
  else
    Result := 'unknown';
end;

class function TCommandLineValue.IsBooleanLiteral(const Raw: string): Boolean;
begin
  Result := SameText(Raw, 'true') or SameText(Raw, 'false');
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

// A time of day in the extended format of ISO 8601: hh:mm, hh:mm:ss, or hh:mm:ss and a fraction.
// TTime holds milliseconds, so the digits of the fraction past the third are read and not kept.
function TryParseTime(const Raw: string; out Value: TTime): Boolean;

  function TwoDigits(At: Integer; out Number: Integer): Boolean;
  begin
    Result := (At + 1 <= Length(Raw)) and CharInSet(Raw[At], ['0' .. '9']) and CharInSet(Raw[At + 1], ['0' .. '9']);
    if Result then
      Number := (Ord(Raw[At]) - Ord('0')) * 10 + Ord(Raw[At + 1]) - Ord('0');
  end;

var
  Hours, Minutes, Seconds, Millis, Digits, I: Integer;
begin
  Result := False;
  Seconds := 0;
  Millis := 0;
  if not (TwoDigits(1, Hours) and (Length(Raw) >= 5) and (Raw[3] = ':') and TwoDigits(4, Minutes)) then
    Exit;
  if Length(Raw) > 5 then
  begin
    if (Raw[6] <> ':') or not TwoDigits(7, Seconds) then
      Exit;
    if Length(Raw) > 8 then
    begin
      if (Raw[9] <> '.') or (Length(Raw) = 9) then
        Exit;
      Digits := 0;
      for I := 10 to Length(Raw) do
      begin
        if not CharInSet(Raw[I], ['0' .. '9']) then
          Exit;
        if Digits < 3 then
          Millis := Millis * 10 + Ord(Raw[I]) - Ord('0');
        Inc(Digits);
      end;
      while Digits < 3 do
      begin
        Millis := Millis * 10;
        Inc(Digits);
      end;
    end;
  end;
  Result := (Hours < 24) and (Minutes < 60) and (Seconds < 60);
  if Result then
    Value := EncodeTime(Hours, Minutes, Seconds, Millis);
end;

// A set of an enumerated type, written as .NET writes a [Flags] value: names separated by commas,
// each one matched without regard to case and with the blanks around it ignored. Every name has to
// be one of the type's, so an empty token is refused too.
function TryParseEnumSet(TypeInf: PTypeInfo; const Raw: string; out Value: TValue): Boolean;
var
  Members: PTypeInfo;
  Data: PTypeData;
  Bits: array [0 .. 31] of Byte;
  Name: string;
  Ordinal, Found: Integer;
begin
  Result := False;
  Members := GetTypeData(TypeInf).CompType^;
  Data := GetTypeData(Members);
  FillChar(Bits, SizeOf(Bits), 0);
  for Name in Raw.Split([',']) do
  begin
    Found := -1;
    for Ordinal := Data.MinValue to Data.MaxValue do
      if SameText(Name.Trim, GetEnumName(Members, Ordinal)) then
      begin
        Found := Ordinal;
        Break;
      end;
    if Found < 0 then
      Exit;
    // The layout of a Delphi set: one bit per member, from the byte holding its lowest one.
    Bits[Found div 8 - Data.MinValue div 8] := Bits[Found div 8 - Data.MinValue div 8] or (1 shl (Found mod 8));
  end;
  TValue.Make(@Bits, TypeInf, Value);
  Result := True;
end;

class function TCommandLineValue.TryParse(TypeInf: PTypeInfo; const Raw: string; out Value: TValue): Boolean;
var
  IntValue: Integer;
  BigValue: Int64;
  UnsignedValue: UInt64;
  Min, Max: Int64;
  FloatValue: Double;
  SingleValue: Extended;
  CurrValue: Currency;
  DateValue: TDateTime;
  TimeValue: TTime;
  SpanValue: TTimeSpan;
  GuidValue: TGUID;
  Inner: PTypeInfo;
  Data: PTypeData;
  Ordinal: Integer;
begin
  Value := TValue.Empty;
  // Asked about what ONE token becomes. A collection never reaches the conversions
  // below: each of its tokens is converted as an element.
  TypeInf := ElementTypeOf(TypeInf);

  // A value that may be missing and came: what it holds, converted as its own type.
  Inner := NullableValueTypeOf(TypeInf);
  if Inner <> nil then
  begin
    Result := TryParse(Inner, Raw, Value);
    if Result then
      Value := MakeNullable(TypeInf, Value);
    Exit;
  end;

  if TypeInf = System.TypeInfo(string) then
  begin
    Value := TValue.From<string>(Raw);
    Exit(True);
  end;

  // Built with the declared type and not as a string, so reading it back as that type needs no cast.
  if (TypeInf = System.TypeInfo(TFilePath)) or (TypeInf = System.TypeInfo(TDirectoryPath)) then
  begin
    TValue.Make(@Raw, TypeInf, Value);
    Exit(True);
  end;

  if IsFlag(TypeInf) then
  begin
    if SameText(Raw, 'true') then
    begin
      Value := TValue.From<Boolean>(True);
      Exit(True);
    end;
    if SameText(Raw, 'false') then
    begin
      Value := TValue.From<Boolean>(False);
      Exit(True);
    end;
    Exit(False);
  end;

  if TypeInf = System.TypeInfo(Integer) then
  begin
    // TryStrToInt converts the whole token or fails, anchored at both ends: -5 parses,
    // and 80xyz does not become 80.
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

  // A value outside the range of its type is not converted, whatever the width: -1 is not an
  // unsigned integer, and 300 is not a byte.
  if TryIntegerRange(TypeInf, Min, Max) then
  begin
    if not TryStrToInt64(Raw, BigValue) or (BigValue < Min) or (BigValue > Max) then
      Exit(False);
    if TypeInf = System.TypeInfo(ShortInt) then
      Value := TValue.From<ShortInt>(ShortInt(BigValue))
    else if TypeInf = System.TypeInfo(Byte) then
      Value := TValue.From<Byte>(Byte(BigValue))
    else if TypeInf = System.TypeInfo(SmallInt) then
      Value := TValue.From<SmallInt>(SmallInt(BigValue))
    else if TypeInf = System.TypeInfo(Word) then
      Value := TValue.From<Word>(Word(BigValue))
    else
      Value := TValue.From<Cardinal>(Cardinal(BigValue));
    Exit(True);
  end;

  if TypeInf = System.TypeInfo(UInt64) then
  begin
    // The sign is refused before the conversion, which is not asked what it makes of one.
    if Raw.Contains('-') or not TryStrToUInt64(Raw, UnsignedValue) then
      Exit(False);
    Value := TValue.From<UInt64>(UnsignedValue);
    Exit(True);
  end;

  if TypeInf = System.TypeInfo(Double) then
  begin
    if not TryStrToFloat(Raw, FloatValue, InvariantSettings) then
      Exit(False);
    Value := TValue.From<Double>(FloatValue);
    Exit(True);
  end;

  if TypeInf = System.TypeInfo(Single) then
  begin
    // Read wider and narrowed here, so a magnitude Single cannot hold becomes an infinity, as .NET's
    // float.Parse does, instead of a refusal or an overflow raised by the assignment.
    if not TryStrToFloat(Raw, SingleValue, InvariantSettings) then
      Exit(False);
    if SingleValue > MaxSingle then
      Value := TValue.From<Single>(Infinity)
    else if SingleValue < -MaxSingle then
      Value := TValue.From<Single>(NegInfinity)
    else
      Value := TValue.From<Single>(SingleValue);
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
    // Written with no zone, it is read as local time, so the date read is the date written and not one
    // shifted by a zone nobody named. Written with one, it is converted to local time, as .NET does.
    if not TryISO8601ToDate(Raw, DateValue, [ioNoTZIsLocal]) then
      Exit(False);
    Value := TValue.From<TDateTime>(DateValue);
    Exit(True);
  end;

  if TypeInf = System.TypeInfo(TDate) then
  begin
    // A date and nothing else, in ISO 8601 as the date with a time: a time written behind it would be
    // read and thrown away, and that is a value the parser chose on its own.
    if (Raw.IndexOfAny(['T', 't', ' ', ':']) >= 0) or not TryISO8601ToDate(Raw, DateValue, [ioNoTZIsLocal]) then
      Exit(False);
    Value := TValue.From<TDate>(TDate(DateValue));
    Exit(True);
  end;

  if TypeInf = System.TypeInfo(TTime) then
  begin
    if not TryParseTime(Raw, TimeValue) then
      Exit(False);
    Value := TValue.From<TTime>(TimeValue);
    Exit(True);
  end;

  if TypeInf = System.TypeInfo(TTimeSpan) then
  begin
    // .NET's format, d.hh:mm:ss.f, read without regard to the locale.
    if not TTimeSpan.TryParse(Raw, SpanValue) then
      Exit(False);
    Value := TValue.From<TTimeSpan>(SpanValue);
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

  if IsEnumSet(TypeInf) then
    Exit(TryParseEnumSet(TypeInf, Raw, Value));

  Result := False;
end;

class function TCommandLineValue.ToText(const Value: TValue): string;
var
  Context: TRttiContext;
  Declared: TRttiType;
begin
  if NullableValueTypeOf(Value.TypeInfo) <> nil then
  begin
    Declared := Context.GetType(Value.TypeInfo);
    if not Declared.GetField('FHasValue').GetValue(Value.GetReferenceToRawData).AsBoolean then
      Exit('');
    Exit(ToText(Declared.GetField('FValue').GetValue(Value.GetReferenceToRawData)));
  end;
  if Value.TypeInfo = System.TypeInfo(TTimeSpan) then
    Exit(Value.AsType<TTimeSpan>().ToString);
  Result := Value.ToString;
end;

initialization

InvariantSettings := TFormatSettings.Invariant;

end.
