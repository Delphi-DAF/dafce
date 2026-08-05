unit Daf.Nullable;

interface

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
    /// <summary>The present value (<c>Default(T)</c> when NULL).</summary>
    property Value: T read GetValue;
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
  Result := Value.FValue;
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
  Result := FValue;
end;

function TNullable<T>.GetValueOrDefault(const Fallback: T): T;
begin
  if FHasValue then
    Result := FValue
  else
    Result := Fallback;
end;

end.
