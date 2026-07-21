unit Operators.Feat;

interface

uses
  System.Generics.Collections;

type
  TOpsWorld = class
  public
    Nums: TList<Integer>;
    Collected: TArray<Integer>;
    Strs: TArray<string>;
    Pulled: Integer;
    GroupKeys: TArray<Integer>;
    GroupSizes: TArray<Integer>;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,
  System.Generics.Defaults,
  Daf.MiniSpec,
  Daf.Enumerable,
  Daf.Arrays,
  Daf.Expression,
  Daf.Linq;

{ TOpsWorld }

constructor TOpsWorld.Create;
begin
  inherited;
  Nums := TList<Integer>.Create;
  Nums.AddRange([5, 3, 8, 3, 1, 8]);
end;

destructor TOpsWorld.Destroy;
begin
  Nums.Free;
  inherited;
end;

initialization

Feature('''
Feature Linq operators @linq @operators

  As a developer
  I want lazy fluent operators over a sequence
  So I can filter, map, page, de-duplicate, order, zip and group without SQL
''')

.UseWorld<TOpsWorld>

.Rule('Filter and project')

  .Scenario('Where keeps only matching elements')
    .When('Where(x > 3) is applied',
      procedure(W: TOpsWorld)
      begin
        W.Collected := Linq.From<Integer>(W.Nums)
          .Where(function(X: Integer): Boolean begin Result := X > 3; end)
          .ToArray;
      end)
    .&Then('only 5, 8, 8 remain',
      procedure(W: TOpsWorld)
      begin
        Expect(Length(W.Collected)).ToEqual(3);
        Expect(W.Collected[0]).ToEqual(5);
        Expect(W.Collected[1]).ToEqual(8);
        Expect(W.Collected[2]).ToEqual(8);
      end)

  .Scenario('Where, Select and Take chain lazily')
    .When('the source is filtered, mapped x10 and the first two taken',
      procedure(W: TOpsWorld)
      begin
        W.Collected := Linq.From<Integer>(W.Nums)
          .Where(function(X: Integer): Boolean begin Result := X > 2; end)
          .Select<Integer>(function(X: Integer): Integer begin Result := X * 10; end)
          .Take(2)
          .ToArray;
      end)
    .&Then('the result is [50, 30]',
      procedure(W: TOpsWorld)
      begin
        Expect(Length(W.Collected)).ToEqual(2);
        Expect(W.Collected[0]).ToEqual(50);
        Expect(W.Collected[1]).ToEqual(30);
      end)

  .Scenario('Select maps to another element type')
    .When('the first two elements are mapped to strings',
      procedure(W: TOpsWorld)
      begin
        W.Strs := Linq.From<Integer>(W.Nums)
          .Take(2)
          .Select<string>(function(X: Integer): string begin Result := IntToStr(X); end)
          .ToArray;
      end)
    .&Then('the result is the string forms',
      procedure(W: TOpsWorld)
      begin
        Expect(Length(W.Strs)).ToEqual(2);
        Expect(W.Strs[0]).ToEqual('5');
        Expect(W.Strs[1]).ToEqual('3');
      end)

.Rule('Page, de-duplicate and order')

  .Scenario('Take short-circuits and stops consuming the source')
    .When('Take(3) is applied to a large lazy generator',
      procedure(W: TOpsWorld)
      begin
        W.Pulled := 0;
        var Seq: IEnumerable<Integer> := TDeferredEnumerable<Integer>.Create(
          function: IEnumerator<Integer>
          var
            N: Integer;
          begin
            N := 0;
            Result := TGeneratorEnumerator<Integer>.Create(
              function(var Current: Integer): Boolean
              begin
                Inc(W.Pulled);
                Inc(N);
                Current := N;
                Result := N <= 1000000;
              end);
          end);
        W.Collected := Linq.From<Integer>(Seq).Take(3).ToArray;
      end)
    .&Then('exactly 3 elements are produced and only 3 are pulled',
      procedure(W: TOpsWorld)
      begin
        Expect(Length(W.Collected)).ToEqual(3);
        Expect(W.Pulled).ToEqual(3);
      end)

  .Scenario('Skip drops the first N elements')
    .When('Skip(2) is applied',
      procedure(W: TOpsWorld)
      begin
        W.Collected := Linq.From<Integer>(W.Nums).Skip(2).ToArray;
      end)
    .&Then('the first two are dropped',
      procedure(W: TOpsWorld)
      begin
        Expect(Length(W.Collected)).ToEqual(4);
        Expect(W.Collected[0]).ToEqual(8);
        Expect(W.Collected[3]).ToEqual(8);
      end)

  .Scenario('Distinct removes duplicates preserving first-seen order')
    .When('Distinct is applied',
      procedure(W: TOpsWorld)
      begin
        W.Collected := Linq.From<Integer>(W.Nums).Distinct.ToArray;
      end)
    .&Then('each value appears once in first-seen order',
      procedure(W: TOpsWorld)
      begin
        Expect(Length(W.Collected)).ToEqual(4);
        Expect(W.Collected[0]).ToEqual(5);
        Expect(W.Collected[1]).ToEqual(3);
        Expect(W.Collected[2]).ToEqual(8);
        Expect(W.Collected[3]).ToEqual(1);
      end)

  .Scenario('OrderBy sorts by a comparer')
    .When('OrderBy the default integer comparer is applied',
      procedure(W: TOpsWorld)
      begin
        W.Collected := Linq.From<Integer>(W.Nums)
          .OrderBy(TComparer<Integer>.Default)
          .ToArray;
      end)
    .&Then('the elements come out ascending',
      procedure(W: TOpsWorld)
      begin
        Expect(Length(W.Collected)).ToEqual(6);
        Expect(W.Collected[0]).ToEqual(1);
        Expect(W.Collected[5]).ToEqual(8);
      end)

.Rule('Zip and group')

  .Scenario('Zip pairs two sequences element-wise')
    .When('numbers are zipped with letters',
      procedure(W: TOpsWorld)
      begin
        W.Strs := Linq.From<Integer>(TArray<Integer>.Create(1, 2, 3))
          .Zip<string, string>(
            Linq.From<string>(TArray<string>.Create('a', 'b', 'c')),
            function(N: Integer; S: string): string begin Result := S + IntToStr(N); end)
          .ToArray;
      end)
    .&Then('the result is a1, b2, c3',
      procedure(W: TOpsWorld)
      begin
        Expect(Length(W.Strs)).ToEqual(3);
        Expect(W.Strs[0]).ToEqual('a1');
        Expect(W.Strs[1]).ToEqual('b2');
        Expect(W.Strs[2]).ToEqual('c3');
      end)

  .Scenario('GroupBy yields groups exposing key and members')
    .When('the source is grouped by parity',
      procedure(W: TOpsWorld)
      begin
        W.GroupKeys := nil;
        W.GroupSizes := nil;
        var Groups := Linq.From<Integer>(W.Nums).GroupBy<Integer>(
          function(X: Integer): Integer begin Result := X mod 2; end);
        for var G in Groups do
        begin
          W.GroupKeys := W.GroupKeys + [G.Key];
          W.GroupSizes := W.GroupSizes + [Length(G.Members)];
        end;
      end)
    .&Then('two groups form: odd (key 1, 4 members) then even (key 0, 2 members)',
      procedure(W: TOpsWorld)
      begin
        Expect(Length(W.GroupKeys)).ToEqual(2);
        Expect(W.GroupKeys[0]).ToEqual(1);
        Expect(W.GroupSizes[0]).ToEqual(4);
        Expect(W.GroupKeys[1]).ToEqual(0);
        Expect(W.GroupSizes[1]).ToEqual(2);
      end)

.Rule('Eager/lazy bridges and shared vocabulary')

  .Scenario('A smart array flows through the engine and back')
    .When('a TSmartArray is queried and materialized back to a smart array',
      procedure(W: TOpsWorld)
      begin
        var SA: TSmartArray<Integer> := TArray<Integer>.Create(5, 3, 8, 3, 1, 8);
        var Filtered := Linq.From<Integer>(SA)
          .Where(function(X: Integer): Boolean begin Result := X > 3; end)
          .ToSmartArray;
        W.Collected := Filtered;
      end)
    .&Then('the eager result carries the filtered values',
      procedure(W: TOpsWorld)
      begin
        Expect(Length(W.Collected)).ToEqual(3);
        Expect(W.Collected[0]).ToEqual(5);
        Expect(W.Collected[1]).ToEqual(8);
        Expect(W.Collected[2]).ToEqual(8);
      end)

  .Scenario('Where reuses the TFilterSpec vocabulary')
    .When('a TFilterSpec predicate is passed to Where',
      procedure(W: TOpsWorld)
      begin
        var Spec: TFilterSpec<Integer> := TPredicate<Integer>(
          function(X: Integer): Boolean begin Result := X = 8; end);
        W.Collected := Linq.From<Integer>(W.Nums).Where(Spec).ToArray;
      end)
    .&Then('only the 8s remain',
      procedure(W: TOpsWorld)
      begin
        Expect(Length(W.Collected)).ToEqual(2);
        Expect(W.Collected[0]).ToEqual(8);
        Expect(W.Collected[1]).ToEqual(8);
      end);

end.
