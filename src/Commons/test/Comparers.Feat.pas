unit Comparers.Feat;

interface

uses
  System.Rtti;

type
  TComparersWorld = class
  public
    Values: TArray<TValue>;
    DistinctCount: Integer;
  end;

implementation

uses
  System.Generics.Defaults,
  System.Generics.Collections,
  Daf.MiniSpec,
  Daf.Nullable,
  Daf.Comparers;

initialization

Feature('''
Feature Comparers @comparers @commons

  As the in-memory query engine
  I want NULL-aware ordering and equality over TValue and TNullable
  So ORDER BY and DISTINCT match SQL: NULL is smallest, and NULLs compare equal
''')

.UseWorld<TComparersWorld>

.Rule('TValue ordering — NULL is smallest')

  .Scenario('NULL sorts before any non-NULL value')
    .&Then('the ordinal comparer ranks NULL below values and equal to NULL',
      procedure(W: TComparersWorld)
      begin
        var Cmp := TValueComparers.Ordinal;
        Expect(Cmp.Compare(TValue.Empty, TValue.From<Integer>(5))).ToBeLessThan(0);
        Expect(Cmp.Compare(TValue.From<Integer>(5), TValue.Empty)).ToBeGreaterThan(0);
        Expect(Cmp.Compare(TValue.Empty, TValue.Empty)).ToEqual(0);
        Expect(Cmp.Compare(TValue.From<Integer>(3), TValue.From<Integer>(5))).ToBeLessThan(0);
      end)

  .Scenario('Sorting places NULLs first then ascending values')
    .When('an array of values with NULLs is sorted with the ordinal comparer',
      procedure(W: TComparersWorld)
      begin
        W.Values := [TValue.From<Integer>(5), TValue.Empty, TValue.From<Integer>(3),
                     TValue.Empty, TValue.From<Integer>(1)];
        TArray.Sort<TValue>(W.Values, TValueComparers.Ordinal);
      end)
    .&Then('the two NULLs come first, then 1, 3, 5',
      procedure(W: TComparersWorld)
      begin
        Expect(W.Values[0].IsEmpty).ToBeTrue;
        Expect(W.Values[1].IsEmpty).ToBeTrue;
        Expect(W.Values[2].AsInteger).ToEqual(1);
        Expect(W.Values[3].AsInteger).ToEqual(3);
        Expect(W.Values[4].AsInteger).ToEqual(5);
      end)

.Rule('TValue equality — NULLs are equal')

  .Scenario('NULLs compare equal; NULL and a value do not')
    .&Then('the equality comparer treats NULLs as equal',
      procedure(W: TComparersWorld)
      begin
        var Eq := TValueComparers.Equality;
        Expect(Eq.Equals(TValue.Empty, TValue.Empty)).ToBeTrue;
        Expect(Eq.Equals(TValue.From<Integer>(5), TValue.From<Integer>(5))).ToBeTrue;
        Expect(Eq.Equals(TValue.From<Integer>(5), TValue.From<Integer>(6))).ToBeFalse;
        Expect(Eq.Equals(TValue.Empty, TValue.From<Integer>(5))).ToBeFalse;
      end)

  .Scenario('DISTINCT collapses duplicate NULLs')
    .When('values [NULL, 1, NULL, 1, 2] are de-duplicated with the equality comparer',
      procedure(W: TComparersWorld)
      begin
        var Src: TArray<TValue> := [TValue.Empty, TValue.From<Integer>(1), TValue.Empty,
                                    TValue.From<Integer>(1), TValue.From<Integer>(2)];
        var Seen := TDictionary<TValue, Byte>.Create(TValueComparers.Equality);
        try
          for var V in Src do
            Seen.AddOrSetValue(V, 0);
          W.DistinctCount := Seen.Count;
        finally
          Seen.Free;
        end;
      end)
    .&Then('exactly three distinct values remain (NULL, 1, 2)',
      procedure(W: TComparersWorld)
      begin
        Expect(W.DistinctCount).ToEqual(3);
      end)

.Rule('TNullable comparers')

  .Scenario('An absent TNullable sorts smallest and equals another absent')
    .&Then('ordinal ranks Null below present, equality treats Nulls as equal',
      procedure(W: TComparersWorld)
      begin
        var C := TNullableComparers.Ordinal<Integer>;
        Expect(C.Compare(TNullable<Integer>.Null, TNullable<Integer>.From(1))).ToBeLessThan(0);
        Expect(C.Compare(TNullable<Integer>.From(1), TNullable<Integer>.Null)).ToBeGreaterThan(0);
        var E := TNullableComparers.Equality<Integer>;
        Expect(E.Equals(TNullable<Integer>.Null, TNullable<Integer>.Null)).ToBeTrue;
        Expect(E.Equals(TNullable<Integer>.From(1), TNullable<Integer>.From(1))).ToBeTrue;
        Expect(E.Equals(TNullable<Integer>.From(1), TNullable<Integer>.From(2))).ToBeFalse;
      end);

end.
