unit Comparers.Feat;

interface

uses
  System.Rtti,
  System.Generics.Defaults,
  Daf.Nullable;

type
  TComparersWorld = class
  public
    Values: TArray<TValue>;
    DistinctCount: Integer;
    Cmp: IComparer<TValue>;
    Eq: IEqualityComparer<TValue>;
    NCmp: IComparer<TNullable<Integer>>;
    NEq: IEqualityComparer<TNullable<Integer>>;
    Report: string;
  end;

implementation

uses
  System.SysUtils,
  System.Math,
  System.Generics.Collections,
  Daf.MiniSpec,
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
    .Given('the ordinal comparer for TValue',
      procedure(W: TComparersWorld)
      begin
        W.Cmp := TValueComparers.Ordinal;
      end)
    .When('NULL is compared with a value both ways, with NULL, and two values with each other',
      procedure(W: TComparersWorld)
      begin
        // Sign and not the raw result: a comparer owes a side, not a magnitude, and comparing
        // the side is what the four assertions said one by one.
        W.Report := Format('%d %d %d %d',
          [Sign(W.Cmp.Compare(TValue.Empty, TValue.From<Integer>(5))),
           Sign(W.Cmp.Compare(TValue.From<Integer>(5), TValue.Empty)),
           Sign(W.Cmp.Compare(TValue.Empty, TValue.Empty)),
           Sign(W.Cmp.Compare(TValue.From<Integer>(3), TValue.From<Integer>(5)))]);
      end)
    .&Then('the ordinal comparer ranks NULL below values and equal to NULL',
      procedure(W: TComparersWorld)
      begin
        Expect(W.Report).ToEqual('-1 1 0 -1');
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
    .Given('the equality comparer for TValue',
      procedure(W: TComparersWorld)
      begin
        W.Eq := TValueComparers.Equality;
      end)
    .When('two NULLs, two equal values, two different ones and a mixed pair are compared',
      procedure(W: TComparersWorld)
      begin
        W.Report := Format('%s %s %s %s',
          [BoolToStr(W.Eq.Equals(TValue.Empty, TValue.Empty), True),
           BoolToStr(W.Eq.Equals(TValue.From<Integer>(5), TValue.From<Integer>(5)), True),
           BoolToStr(W.Eq.Equals(TValue.From<Integer>(5), TValue.From<Integer>(6)), True),
           BoolToStr(W.Eq.Equals(TValue.Empty, TValue.From<Integer>(5)), True)]);
      end)
    .&Then('the equality comparer treats NULLs as equal',
      procedure(W: TComparersWorld)
      begin
        Expect(W.Report).ToEqual('True True False False');
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
    .Given('the two comparers TNullable declares, ordinal and equality',
      procedure(W: TComparersWorld)
      begin
        W.NCmp := TNullableComparers.Ordinal<Integer>;
        W.NEq := TNullableComparers.Equality<Integer>;
      end)
    .When('an absent is compared with a present, and absents and presents with each other',
      procedure(W: TComparersWorld)
      begin
        W.Report := Format('%d %d %s %s %s',
          [Sign(W.NCmp.Compare(TNullable<Integer>.Null, TNullable<Integer>.From(1))),
           Sign(W.NCmp.Compare(TNullable<Integer>.From(1), TNullable<Integer>.Null)),
           BoolToStr(W.NEq.Equals(TNullable<Integer>.Null, TNullable<Integer>.Null), True),
           BoolToStr(W.NEq.Equals(TNullable<Integer>.From(1), TNullable<Integer>.From(1)), True),
           BoolToStr(W.NEq.Equals(TNullable<Integer>.From(1), TNullable<Integer>.From(2)), True)]);
      end)
    .&Then('ordinal ranks Null below present, equality treats Nulls as equal',
      procedure(W: TComparersWorld)
      begin
        Expect(W.Report).ToEqual('-1 1 True True False');
      end);

end.
