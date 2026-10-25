unit Daf.Comparers;

{
  NULL-aware comparison for the value currency used by the in-memory query
  engine. SQL NULL is modelled as an empty TValue, a Variant-kind TValue
  holding Null/Unassigned, or an absent TNullable; a present TNullable counts
  as the value it holds.

  Every non-NULL value belongs to a family: Boolean, number (integer, floating
  point and Currency), date, text, or other.

    * Equality  — values of different families are never equal. Two numbers are
                  equal when they are worth the same, whatever their type; two
                  NULLs are equal. Equal values hash alike.
    * Ordering  — NULL sorts smaller than every non-NULL value (nulls-first
                  ASC / nulls-last DESC). Within a family the order is the
                  natural one: text ordinal, False before True, dates and
                  numbers by value, and other values by their text. Ordering two
                  values of different families raises EArgumentException.
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
    /// <summary>
    ///   Order comparer: NULL &lt; every non-NULL value; NULLs equal. Two values of different
    ///   families raise <c>EArgumentException</c>.
    /// </summary>
    class function Ordinal: IComparer<TValue>; static;
    /// <summary>
    ///   Equality comparer: values of different families are never equal, numbers are equal by
    ///   value whatever their type, NULLs are equal, and equal values share their hash.
    /// </summary>
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

/// <summary>
///   True when the value represents SQL NULL: empty, a Null/Unassigned Variant, or an absent
///   <c>TNullable</c>.
/// </summary>
function IsNullValue(const V: TValue): Boolean;
/// <summary>
///   Compares two values with NULL sorting smallest and NULLs equal; raises
///   <c>EArgumentException</c> for two values of different families.
/// </summary>
function CompareValuesNullSmallest(const L, R: TValue): Integer;

implementation

uses
  System.TypInfo,
  System.Variants,
  System.SysUtils,
  System.Math;

type
  TValueFamily = (vfNull, vfBoolean, vfNumber, vfDate, vfText, vfOther);

  // A value reduced to what equality, ordering and hashing look at.
  TClassified = record
    Family: TValueFamily;
    Bool: Boolean;
    // A number that is an integer within Int64 compares and hashes as Int64; any other as Double.
    IsIntegral: Boolean;
    Int: Int64;
    Num: Double;
    Text: string;
    function AsDouble: Double;
  end;

const
  FamilyNames: array[TValueFamily] of string = ('NULL', 'Boolean', 'number', 'date', 'text', 'other');

function TClassified.AsDouble: Double;
begin
  if IsIntegral then
    Result := Int
  else
    Result := Num;
end;

procedure SetNumber(var C: TClassified; const D: Double);
begin
  C.Family := vfNumber;
  C.IsIntegral := not IsNan(D) and not IsInfinite(D) and (Frac(D) = 0)
    and (D >= -9223372036854775808.0) and (D < 9223372036854775808.0);
  if C.IsIntegral then
    C.Int := Trunc(D)
  else
    C.Num := D;
end;

procedure SetInteger(var C: TClassified; const I: Int64);
begin
  C.Family := vfNumber;
  C.IsIntegral := True;
  C.Int := I;
end;

procedure ClassifyVariant(const Va: Variant; var C: TClassified);
begin
  case VarType(Va) and varTypeMask of
    varEmpty, varNull:
      C.Family := vfNull;
    varBoolean:
      begin
        C.Family := vfBoolean;
        C.Bool := Va;
      end;
    varShortInt, varSmallint, varInteger, varByte, varWord, varLongWord, varInt64, varUInt64:
      SetInteger(C, Va);
    varSingle, varDouble, varCurrency:
      SetNumber(C, Va);
    varDate:
      begin
        C.Family := vfDate;
        C.Num := VarToDateTime(Va);
      end;
    varString, varUString, varOleStr:
      begin
        C.Family := vfText;
        C.Text := VarToStr(Va);
      end;
  else
    C.Family := vfOther;
    C.Text := VarToStr(Va);
  end;
end;

function Classify(const Value: TValue): TClassified;
begin
  Result := Default(TClassified);
  var V := Value;
  var Inner: TValue;
  if TNullableRtti.TryUnwrap(V, Inner) then
    V := Inner;
  if V.IsEmpty then
    Exit;
  case V.Kind of
    tkEnumeration:
      if GetTypeData(V.TypeInfo).BaseType^ = System.TypeInfo(Boolean) then
      begin
        Result.Family := vfBoolean;
        Result.Bool := V.AsBoolean;
      end
      else
      begin
        Result.Family := vfOther;
        Result.Text := V.ToString;
      end;
    tkInteger, tkInt64:
      SetInteger(Result, V.AsInt64);
    tkFloat:
      if (V.TypeInfo = System.TypeInfo(TDateTime)) or (V.TypeInfo = System.TypeInfo(TDate))
         or (V.TypeInfo = System.TypeInfo(TTime)) then
      begin
        Result.Family := vfDate;
        Result.Num := V.AsExtended;
      end
      else if GetTypeData(V.TypeInfo).FloatType = ftCurr then
        SetNumber(Result, V.AsCurrency)
      else
        SetNumber(Result, V.AsExtended);
    tkChar, tkWChar, tkString, tkLString, tkWString, tkUString:
      begin
        Result.Family := vfText;
        Result.Text := V.AsString;
      end;
    tkVariant:
      ClassifyVariant(V.AsVariant, Result);
  else
    Result.Family := vfOther;
    Result.Text := V.ToString;
  end;
end;

// Within one family; both are non-NULL.
function CompareSameFamily(const L, R: TClassified): Integer;
begin
  case L.Family of
    vfBoolean:
      Result := Ord(L.Bool) - Ord(R.Bool);
    vfNumber:
      if L.IsIntegral and R.IsIntegral then
        Result := CompareValue(L.Int, R.Int)
      else
        Result := CompareValue(L.AsDouble, R.AsDouble);
    vfDate:
      Result := CompareValue(L.Num, R.Num);
  else
    Result := Sign(CompareStr(L.Text, R.Text));
  end;
end;

function IsNullValue(const V: TValue): Boolean;
begin
  Result := Classify(V).Family = vfNull;
end;

function CompareValuesNullSmallest(const L, R: TValue): Integer;
begin
  var CL := Classify(L);
  var CR := Classify(R);
  if (CL.Family = vfNull) and (CR.Family = vfNull) then
    Result := 0
  else if CL.Family = vfNull then
    Result := -1
  else if CR.Family = vfNull then
    Result := 1
  else if CL.Family <> CR.Family then
    raise EArgumentException.CreateFmt('Cannot order a %s value against a %s value',
      [FamilyNames[CL.Family], FamilyNames[CR.Family]])
  else
    Result := CompareSameFamily(CL, CR);
end;

function EqualValues(const L, R: TValue): Boolean;
begin
  var CL := Classify(L);
  var CR := Classify(R);
  if CL.Family <> CR.Family then
    Exit(False);
  Result := (CL.Family = vfNull) or (CompareSameFamily(CL, CR) = 0);
end;

function ValueHash(const V: TValue): Integer;
begin
  var C := Classify(V);
  case C.Family of
    vfNull:
      Result := 0;
    vfBoolean:
      Result := Ord(C.Bool) + 1;
    vfNumber:
      if C.IsIntegral then
        Result := TEqualityComparer<Int64>.Default.GetHashCode(C.Int)
      else
        Result := TEqualityComparer<Double>.Default.GetHashCode(C.Num);
    vfDate:
      Result := TEqualityComparer<Double>.Default.GetHashCode(C.Num);
  else
    Result := TEqualityComparer<string>.Default.GetHashCode(C.Text);
  end;
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
      Result := EqualValues(L, R);
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
