unit Daf.Nullable;

interface

uses
  System.SysUtils,
  System.TypInfo,
  System.Rtti;

type
  /// <summary>
  ///   Optional / Maybe value type. Represents either an absent value (SQL NULL)
  ///   or a present value of type <c>T</c>, distinguishing "no value" from a
  ///   present default value (e.g. <c>0</c> or <c>''</c>).
  /// </summary>
  /// <remarks>
  ///   Exposes <b>both</b> presence predicates: <see cref="IsNull"/> (the
  ///   data-domain idiom, matching Delphi's <c>TField.IsNull</c> and SQLCute's
  ///   <c>IS NULL</c>) and <see cref="HasValue"/> (the .NET idiom,
  ///   <c>HasValue = not IsNull</c>). Callers use whichever reads better in
  ///   context. This is a foundational type shared across the framework (DAL,
  ///   configuration, entities), not just querying.
  /// </remarks>
  TNullable<T> = record
  strict private
    FValue: T;
    FHasValue: Boolean;
    function GetValue: T;
  public
    class operator Initialize(out Dest: TNullable<T>);
    class operator Implicit(const Value: T): TNullable<T>;
    class operator Implicit(const Value: TNullable<T>): T;
    /// <summary>Returns an explicit absent (NULL) nullable.</summary>
    class function Null: TNullable<T>; static;
    /// <summary>Wraps a present value.</summary>
    class function From(const Value: T): TNullable<T>; static;
    /// <summary>True when no value is present (SQL NULL).</summary>
    function IsNull: Boolean; inline;
    /// <summary>True when a value is present. <c>HasValue = not IsNull</c>.</summary>
    function HasValue: Boolean; inline;
    /// <summary>Returns the value when present, otherwise the supplied fallback.</summary>
    function GetValueOrDefault(const Fallback: T): T;
    /// <summary>
    ///   The present value. Reading it when NULL raises <c>EInvalidOpException</c>, as does the
    ///   implicit conversion to <c>T</c>; <see cref="GetValueOrDefault"/> reads without raising.
    /// </summary>
    property Value: T read GetValue;
  end;

  /// <summary>
  ///   What RTTI can tell about a <c>TNullable&lt;X&gt;</c> without knowing <c>X</c>. It reads the
  ///   fields, because RTTI does not expose the properties of a record.
  /// </summary>
  TNullableRtti = record
  public
    /// <summary>The type of the value of a <c>TNullable&lt;X&gt;</c>, or nil when it is not one.</summary>
    class function ValueTypeOf(const TypeInf: PTypeInfo): PTypeInfo; static;
    /// <summary>
    ///   True when <c>Value</c> holds a <c>TNullable&lt;X&gt;</c>. <c>Inner</c> is then its value when
    ///   present, or an empty <c>TValue</c> when absent.
    /// </summary>
    class function TryUnwrap(const Value: TValue; out Inner: TValue): Boolean; static;
    /// <summary>An absent <c>TNullable&lt;X&gt;</c> of the given type.</summary>
    class function Absent(const TypeInf: PTypeInfo): TValue; static;
  end;

implementation

{ TNullable<T> }

class operator TNullable<T>.Initialize(out Dest: TNullable<T>);
begin
  Dest.FHasValue := False;
  Dest.FValue := Default(T);
end;

class operator TNullable<T>.Implicit(const Value: T): TNullable<T>;
begin
  Result.FValue := Value;
  Result.FHasValue := True;
end;

class operator TNullable<T>.Implicit(const Value: TNullable<T>): T;
begin
  Result := Value.GetValue;
end;

class function TNullable<T>.Null: TNullable<T>;
begin
  Result.FHasValue := False;
  Result.FValue := Default(T);
end;

class function TNullable<T>.From(const Value: T): TNullable<T>;
begin
  Result.FValue := Value;
  Result.FHasValue := True;
end;

function TNullable<T>.IsNull: Boolean;
begin
  Result := not FHasValue;
end;

function TNullable<T>.HasValue: Boolean;
begin
  Result := FHasValue;
end;

function TNullable<T>.GetValue: T;
begin
  if not FHasValue then
    raise EInvalidOpException.Create('Nullable object must have a value.');
  Result := FValue;
end;

function TNullable<T>.GetValueOrDefault(const Fallback: T): T;
begin
  if FHasValue then
    Result := FValue
  else
    Result := Fallback;
end;

{ TNullableRtti }

class function TNullableRtti.ValueTypeOf(const TypeInf: PTypeInfo): PTypeInfo;
var
  Context: TRttiContext;
begin
  Result := nil;
  // TNullable is tkMRecord, because it declares an Initialize operator: by kind first, so every other
  // type leaves without asking RTTI.
  if (TypeInf = nil) or not (TypeInf.Kind in [tkRecord, tkMRecord]) then
    Exit;
  var Declared := Context.GetType(TypeInf);
  if (Declared = nil) or not Declared.Name.StartsWith('TNullable<') then
    Exit;
  var ValueField := Declared.GetField('FValue');
  var HasValueField := Declared.GetField('FHasValue');
  if (ValueField = nil) or (ValueField.FieldType = nil) or (HasValueField = nil) or
    (HasValueField.FieldType = nil) or (HasValueField.FieldType.Handle <> System.TypeInfo(Boolean)) then
    Exit;
  Result := ValueField.FieldType.Handle;
end;

class function TNullableRtti.TryUnwrap(const Value: TValue; out Inner: TValue): Boolean;
var
  Context: TRttiContext;
begin
  Inner := TValue.Empty;
  if ValueTypeOf(Value.TypeInfo) = nil then
    Exit(False);
  var Declared := Context.GetType(Value.TypeInfo);
  var Data := Value.GetReferenceToRawData;
  if Declared.GetField('FHasValue').GetValue(Data).AsBoolean then
    Inner := Declared.GetField('FValue').GetValue(Data);
  Result := True;
end;

class function TNullableRtti.Absent(const TypeInf: PTypeInfo): TValue;
begin
  // Zero-filled is absent: FHasValue False and FValue its default.
  TValue.Make(nil, TypeInf, Result);
end;

end.
