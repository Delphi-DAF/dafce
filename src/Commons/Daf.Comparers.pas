unit Daf.Comparers;

{
  NULL-aware comparison for the value currency used by the in-memory query
  engine. SQL NULL is modelled as an empty TValue (or a Variant-kind TValue
  holding Null/Unassigned). Two coordinated policies:

    * Ordering  — NULL sorts smaller than every non-NULL value (nulls-first
                  ASC / nulls-last DESC); two NULLs are equal.
    * Equality  — NULLs compare equal (so DISTINCT collapses duplicate NULLs).
}

interface

uses
  System.Rtti,
  System.Generics.Defaults,
  Daf.Nullable;

type
  /// <summary>NULL-aware comparers over <c>TValue</c> (the engine's value currency).</summary>
  TValueComparers = record
  public
    /// <summary>Order comparer: NULL &lt; every non-NULL value; NULLs equal.</summary>
    class function Ordinal: IComparer<TValue>; static;
    /// <summary>Equality comparer: values equal by content; NULLs equal.</summary>
    class function Equality: IEqualityComparer<TValue>; static;
  end;

  /// <summary>NULL-aware comparers over <c>TNullable&lt;T&gt;</c>.</summary>
  TNullableComparers = record
  public
    /// <summary>Order comparer: an absent value sorts smaller than any present one.</summary>
    class function Ordinal<T>: IComparer<TNullable<T>>; static;
    /// <summary>Equality comparer: two absent values are equal.</summary>
    class function Equality<T>: IEqualityComparer<TNullable<T>>; static;
  end;

/// <summary>True when the value represents SQL NULL (empty, or a Null/Unassigned Variant).</summary>
function IsNullValue(const V: TValue): Boolean;
/// <summary>Compares two values with NULL sorting smallest and NULLs equal.</summary>
function CompareValuesNullSmallest(const L, R: TValue): Integer;

implementation

uses
  System.TypInfo,
  System.Variants,
  System.SysUtils;

function IsNullValue(const V: TValue): Boolean;
begin
  Result := V.IsEmpty;
  if not Result and (V.Kind = tkVariant) then
  begin
    var Va := V.AsVariant;
    Result := VarIsNull(Va) or VarIsEmpty(Va);
  end;
end;

function CompareNonNull(const L, R: TValue): Integer;
var
  VL, VR: Variant;
begin
  try
    VL := L.AsVariant;
    VR := R.AsVariant;
  except
    // Not Variant-convertible (objects, records, …): fall back to textual order.
    Exit(CompareStr(L.ToString, R.ToString));
  end;
  case VarCompareValue(VL, VR) of
    vrLessThan:    Result := -1;
    vrGreaterThan: Result := 1;
    vrEqual:       Result := 0;
  else
    // vrNotEqual — incomparable variant kinds: stable textual fallback.
    Result := CompareStr(VarToStr(VL), VarToStr(VR));
  end;
end;

function CompareValuesNullSmallest(const L, R: TValue): Integer;
begin
  var LNull := IsNullValue(L);
  var RNull := IsNullValue(R);
  if LNull and RNull then
    Result := 0
  else if LNull then
    Result := -1
  else if RNull then
    Result := 1
  else
    Result := CompareNonNull(L, R);
end;

function ValueHash(const V: TValue): Integer;
begin
  if IsNullValue(V) then
    Exit(0);
  var Va: Variant;
  try
    Va := V.AsVariant;
  except
    Exit(TEqualityComparer<string>.Default.GetHashCode(V.ToString));
  end;
  // Normalise numeric/date kinds through Double so 1 and 1.0 hash alike
  // (keeping GetHashCode consistent with the value-equal comparison).
  if VarIsNumeric(Va) or VarIsType(Va, varDate) then
  begin
    var D: Double := Va;
    Result := TEqualityComparer<Double>.Default.GetHashCode(D);
  end
  else
    Result := TEqualityComparer<string>.Default.GetHashCode(VarToStr(Va));
end;

{ TValueComparers }

class function TValueComparers.Ordinal: IComparer<TValue>;
begin
  Result := TComparer<TValue>.Construct(
    function(const L, R: TValue): Integer
    begin
      Result := CompareValuesNullSmallest(L, R);
    end);
end;

class function TValueComparers.Equality: IEqualityComparer<TValue>;
begin
  Result := TEqualityComparer<TValue>.Construct(
    function(const L, R: TValue): Boolean
    begin
      Result := CompareValuesNullSmallest(L, R) = 0;
    end,
    function(const V: TValue): Integer
    begin
      Result := ValueHash(V);
    end);
end;

{ TNullableComparers }

class function TNullableComparers.Ordinal<T>: IComparer<TNullable<T>>;
begin
  Result := TComparer<TNullable<T>>.Construct(
    function(const L, R: TNullable<T>): Integer
    begin
      if L.IsNull and R.IsNull then
        Result := 0
      else if L.IsNull then
        Result := -1
      else if R.IsNull then
        Result := 1
      else
        Result := TComparer<T>.Default.Compare(L.Value, R.Value);
    end);
end;

class function TNullableComparers.Equality<T>: IEqualityComparer<TNullable<T>>;
begin
  Result := TEqualityComparer<TNullable<T>>.Construct(
    function(const L, R: TNullable<T>): Boolean
    begin
      if L.IsNull or R.IsNull then
        Result := L.IsNull = R.IsNull
      else
        Result := TEqualityComparer<T>.Default.Equals(L.Value, R.Value);
    end,
    function(const V: TNullable<T>): Integer
    begin
      if V.IsNull then
        Result := 0
      else
        Result := TEqualityComparer<T>.Default.GetHashCode(V.Value);
    end);
end;

end.
