unit Operators.Feat;

interface

uses
  System.Generics.Collections,
  Daf.Linq,
  Ownership.Doubles;

type
  TOpsWorld = class
  public
    Terminal: string;
    Counted: TArray<ICounted>;
    CountedQuery: TQuery<ICounted>;
    Raised: string;
    Nums: TList<Integer>;
    Collected: TArray<Integer>;
    Strs: TArray<string>;
    LazyStrs: TArray<string>;
    EagerNums: TArray<Integer>;
    Pulled: Integer;
    GroupKeys: TArray<Integer>;
    GroupSizes: TArray<Integer>;
    Flag1: Boolean;
    Flag2: Boolean;
    Num1: Integer;
    Num2: Integer;
    Num3: Integer;
    constructor Create;
    destructor Destroy; override;
    /// <summary>Calls the terminal named by <c>Terminal</c> on <c>CountedQuery</c>, keeping what it raises.</summary>
    procedure CallTerminal;
  end;

implementation

uses
  System.SysUtils,
  System.Generics.Defaults,
  Daf.MiniSpec,
  Daf.Enumerable,
  Daf.Arrays,
  Daf.FilterSpec;

{ TOpsWorld }

constructor TOpsWorld.Create;
begin
  inherited;
  Nums := TList<Integer>.Create;
  Nums.AddRange([5, 3, 8, 3, 1, 8]);
end;

destructor TOpsWorld.Destroy;
begin
  CountedQuery := Default(TQuery<ICounted>);
  Counted := nil;
  Tracking.FreeSurvivors;
  Nums.Free;
  inherited;
end;

function JoinInts(const Values: TArray<Integer>): string;
begin
  Result := '';
  for var V in Values do
  begin
    if Result <> '' then
      Result := Result + ',';
    Result := Result + IntToStr(V);
  end;
end;

// The text after the slash of 'Name/Department'.
function DepartmentOf(const Person: string): string;
begin
  Result := Copy(Person, Pos('/', Person) + 1, MaxInt);
end;

procedure TOpsWorld.CallTerminal;
begin
  Raised := '';
  try
    if Terminal = 'ToList()' then
      CountedQuery.ToList.Free
    else if Terminal = 'ToArray' then
      CountedQuery.ToArray
    else if Terminal = 'ToSmartArray' then
      CountedQuery.ToSmartArray
    else
      raise Exception.Create('unknown terminal ' + Terminal);
  except
    on E: Exception do
      Raised := E.Message;
  end;
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

  .Scenario('OrderBy keeps the source order among many elements with few keys')
    .Given('a hundred elements numbered in order, whose key is the number modulo three',
      procedure(W: TOpsWorld)
      begin
        W.Nums.Clear;
        for var I := 0 to 99 do
          W.Nums.Add(I);
      end)
    .When('they are ordered with OrderBy by their key',
      procedure(W: TOpsWorld)
      begin
        W.Collected := Linq.From<Integer>(W.Nums)
          .OrderBy(TComparer<Integer>.Construct(
            function(const L, R: Integer): Integer
            begin
              Result := (L mod 3) - (R mod 3);
            end))
          .ToArray;
      end)
    .&Then('within each key they come out in the order of their number',
      procedure(W: TOpsWorld)
      begin
        var Expected: TArray<Integer> := [];
        for var Key := 0 to 2 do
          for var I := 0 to 99 do
            if I mod 3 = Key then
              Expected := Expected + [I];
        Expect(JoinInts(W.Collected)).ToEqual(JoinInts(Expected));
      end)

  .Scenario('Ordering by one key keeps the previous order as tie-breaker')
    .Given('a list of people already ordered by name, with repeated departments',
      procedure(W: TOpsWorld)
      begin
        W.Strs := ['Ana/Sales', 'Bea/IT', 'Carlos/Sales', 'Diana/IT', 'Elena/HR', 'Fede/IT', 'Gema/Sales'];
      end)
    .When('they are ordered with OrderBy by department',
      procedure(W: TOpsWorld)
      begin
        W.LazyStrs := Linq.From<string>(W.Strs)
          .OrderBy(TComparer<string>.Construct(
            function(const L, R: string): Integer
            begin
              Result := CompareStr(DepartmentOf(L), DepartmentOf(R));
            end))
          .ToArray;
      end)
    .&Then('they come out grouped by department, and by name within each one',
      procedure(W: TOpsWorld)
      begin
        Expect(String.Join(',', W.LazyStrs)).ToEqual(
          'Elena/HR,Bea/IT,Diana/IT,Fede/IT,Ana/Sales,Carlos/Sales,Gema/Sales');
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

.Rule('Existence and emptiness terminals')

  .Scenario('IsEmpty distinguishes an empty source from a populated one')
    .When('IsEmpty is asked of an empty array and of the world list',
      procedure(W: TOpsWorld)
      begin
        var Empty: TArray<Integer> := nil;
        W.Flag1 := Linq.From<Integer>(Empty).IsEmpty;
        W.Flag2 := Linq.From<Integer>(W.Nums).IsEmpty;
      end)
    .&Then('the empty source reports True and the populated one False',
      procedure(W: TOpsWorld)
      begin
        Expect(W.Flag1).ToBeTrue;
        Expect(W.Flag2).ToBeFalse;
      end)

  .Scenario('A terminal does not exhaust the query')
    .When('IsEmpty runs and the same query value is then enumerated in full',
      procedure(W: TOpsWorld)
      begin
        var Q := Linq.From<Integer>(W.Nums);
        W.Flag1 := Q.IsEmpty;
        W.Collected := Q.ToArray;       // the query is still usable
      end)
    .&Then('every element is still produced afterwards',
      procedure(W: TOpsWorld)
      begin
        Expect(W.Flag1).ToBeFalse;
        Expect(Length(W.Collected)).ToEqual(6);
      end)

  .Scenario('IsEmpty stops consuming the source after the first element')
    .When('IsEmpty is applied to a large lazy generator',
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
        W.Flag1 := Linq.From<Integer>(Seq).IsEmpty;
      end)
    .&Then('exactly one element was pulled',
      procedure(W: TOpsWorld)
      begin
        Expect(W.Flag1).ToBeFalse;
        Expect(W.Pulled).ToEqual(1);
      end)

  .Scenario('Any reports whether some element satisfies the filter')
    .When('Any is asked for an element over 7 and for one over 100',
      procedure(W: TOpsWorld)
      begin
        W.Flag1 := Linq.From<Integer>(W.Nums).Any(
          function(X: Integer): Boolean begin Result := X > 7; end);
        W.Flag2 := Linq.From<Integer>(W.Nums).Any(
          function(X: Integer): Boolean begin Result := X > 100; end);
      end)
    .&Then('the first is True and the second False',
      procedure(W: TOpsWorld)
      begin
        Expect(W.Flag1).ToBeTrue;
        Expect(W.Flag2).ToBeFalse;
      end)

.Rule('First, last and single access')

  .Scenario('First and last of a populated sequence')
    .When('FirstOrDefault and LastOrDefault run over 5,3,8,3,1,8',
      procedure(W: TOpsWorld)
      begin
        W.Num1 := Linq.From<Integer>(W.Nums).FirstOrDefault;
        W.Num2 := Linq.From<Integer>(W.Nums).LastOrDefault;
      end)
    .&Then('they yield the first and the last element in sequence order',
      procedure(W: TOpsWorld)
      begin
        Expect(W.Num1).ToEqual(5);
        Expect(W.Num2).ToEqual(8);
      end)

  .Scenario('An empty sequence yields the default or the supplied fallback')
    .When('the terminals run over an empty source',
      procedure(W: TOpsWorld)
      begin
        var Empty: TArray<Integer> := nil;
        W.Num1 := Linq.From<Integer>(Empty).FirstOrDefault;
        W.Num2 := Linq.From<Integer>(Empty).FirstOrDefault(-1);
        W.Num3 := Linq.From<Integer>(Empty).LastOrDefault(-2);
      end)
    .&Then('the default is 0 and the fallbacks are honored',
      procedure(W: TOpsWorld)
      begin
        Expect(W.Num1).ToEqual(0);
        Expect(W.Num2).ToEqual(-1);
        Expect(W.Num3).ToEqual(-2);
      end)

  .Scenario('TryFirst tells an empty sequence from a first element equal to the default')
    .When('TryFirst runs over an empty source and over one starting with 0',
      procedure(W: TOpsWorld)
      begin
        var Empty: TArray<Integer> := nil;
        var Zeros: TArray<Integer> := TArray<Integer>.Create(0, 7);
        W.Flag1 := Linq.From<Integer>(Empty).TryFirst(W.Num1);
        W.Flag2 := Linq.From<Integer>(Zeros).TryFirst(W.Num2);
      end)
    .&Then('the empty one reports False and the zero-leading one True, both yielding 0',
      procedure(W: TOpsWorld)
      begin
        Expect(W.Flag1).ToBeFalse;
        Expect(W.Flag2).ToBeTrue;
        Expect(W.Num1).ToEqual(0);
        Expect(W.Num2).ToEqual(0);
      end)

  .Scenario('Single yields the only element of a one-element sequence')
    .When('the Single family runs over a sequence filtered down to one element',
      procedure(W: TOpsWorld)
      begin
        var One := Linq.From<Integer>(W.Nums).Where(
          function(X: Integer): Boolean begin Result := X = 5; end);
        W.Num1 := One.Single;
        W.Num2 := One.SingleOrDefault;
        W.Flag1 := One.TrySingle(W.Num3);
      end)
    .&Then('all three agree on the element',
      procedure(W: TOpsWorld)
      begin
        Expect(W.Num1).ToEqual(5);
        Expect(W.Num2).ToEqual(5);
        Expect(W.Flag1).ToBeTrue;
        Expect(W.Num3).ToEqual(5);
      end)

  .Scenario('More than one element is an error, not a silent first')
    .When('TrySingle runs over a sequence holding two 8s',
      procedure(W: TOpsWorld)
      begin
        var Two := Linq.From<Integer>(W.Nums).Where(
          function(X: Integer): Boolean begin Result := X = 8; end);
        W.Flag1 := Two.TrySingle(W.Num1);
      end)
    .&Then('TrySingle reports False, and Single and SingleOrDefault raise',
      procedure(W: TOpsWorld)
      begin
        Expect(W.Flag1).ToBeFalse;
        ExpectException(
          procedure
          begin
            Linq.From<Integer>(W.Nums).Where(
              function(X: Integer): Boolean begin Result := X = 8; end).Single;
          end).ToBe(ELinqError);
        ExpectException(
          procedure
          begin
            Linq.From<Integer>(W.Nums).Where(
              function(X: Integer): Boolean begin Result := X = 8; end).SingleOrDefault;
          end).ToBe(ELinqError);
      end)

  .Scenario('An empty sequence raises for Single but not for SingleOrDefault')
    .When('the Single family runs over an empty source',
      procedure(W: TOpsWorld)
      begin
        var Empty: TArray<Integer> := nil;
        W.Num1 := Linq.From<Integer>(Empty).SingleOrDefault;
        W.Num2 := Linq.From<Integer>(Empty).SingleOrDefault(-9);
        W.Flag1 := Linq.From<Integer>(Empty).TrySingle(W.Num3);
      end)
    .&Then('SingleOrDefault tolerates empty, TrySingle reports False and Single raises',
      procedure(W: TOpsWorld)
      begin
        Expect(W.Num1).ToEqual(0);
        Expect(W.Num2).ToEqual(-9);
        Expect(W.Flag1).ToBeFalse;
        ExpectException(
          procedure
          begin
            var Empty: TArray<Integer> := nil;
            Linq.From<Integer>(Empty).Single;
          end).ToBe(ELinqError);
      end)

  .Scenario('ToList appends to a list the caller owns')
    .When('a query is materialized into a list that already holds an element',
      procedure(W: TOpsWorld)
      begin
        var Target := TList<Integer>.Create;
        try
          Target.Add(99);
          Linq.From<Integer>(W.Nums).Where(
            function(X: Integer): Boolean begin Result := X = 8; end).ToList(Target);
          W.Collected := Target.ToArray;
        finally
          Target.Free;      // ownership stayed with the caller
        end;
      end)
    .&Then('the pre-existing element is preserved and the sequence appended',
      procedure(W: TOpsWorld)
      begin
        Expect(Length(W.Collected)).ToEqual(3);
        Expect(W.Collected[0]).ToEqual(99);
        Expect(W.Collected[1]).ToEqual(8);
        Expect(W.Collected[2]).ToEqual(8);
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
      end)

  .Scenario('One filter value serves both the eager and the lazy API')
    .When('the same TFilterSpec is applied to a TSmartArray and to a query',
      procedure(W: TOpsWorld)
      begin
        var Spec: TFilterSpec<Integer> := TPredicate<Integer>(
          function(X: Integer): Boolean begin Result := X > 3; end);
        var Eager: TSmartArray<Integer> := W.Nums.ToArray;
        W.EagerNums := Eager.Where(Spec);                                 // Commons, eager
        W.Collected := Linq.From<Integer>(W.Nums).Where(Spec).ToArray;    // Daf.Linq, lazy
      end)
    .&Then('both APIs select the same elements',
      procedure(W: TOpsWorld)
      begin
        Expect(Length(W.EagerNums)).ToEqual(3);
        Expect(Length(W.Collected)).ToEqual(3);
        for var I := 0 to High(W.Collected) do
          Expect(W.Collected[I]).ToEqual(W.EagerNums[I]);
      end)

  .Scenario('A composed filter is accepted by the lazy API')
    .When('two specs are combined with and before being passed to Where',
      procedure(W: TOpsWorld)
      begin
        var Big: TFilterSpec<Integer> := TPredicate<Integer>(
          function(X: Integer): Boolean begin Result := X > 3; end);
        var Even: TFilterSpec<Integer> := TPredicate<Integer>(
          function(X: Integer): Boolean begin Result := X mod 2 = 0; end);
        W.Collected := Linq.From<Integer>(W.Nums).Where(Big and Even).ToArray;
      end)
    .&Then('only elements satisfying both remain',
      procedure(W: TOpsWorld)
      begin
        Expect(Length(W.Collected)).ToEqual(2);   // 8, 8
        Expect(W.Collected[0]).ToEqual(8);
        Expect(W.Collected[1]).ToEqual(8);
      end)

  .Scenario('One mapper value serves both the eager and the lazy API')
    .When('the same TFunc mapper is applied to a TSmartArray and to a query',
      procedure(W: TOpsWorld)
      begin
        var Mapper: TFunc<Integer, string> :=
          function(X: Integer): string
          begin
            Result := 'n' + IntToStr(X);
          end;
        var Eager: TSmartArray<Integer> := W.Nums.ToArray;
        W.Strs := Eager.Select<string>(Mapper);                                   // Commons, eager
        W.LazyStrs := Linq.From<Integer>(W.Nums).Select<string>(Mapper).ToArray;  // Daf.Linq, lazy
      end)
    .&Then('both APIs project the same values',
      procedure(W: TOpsWorld)
      begin
        Expect(Length(W.Strs)).ToEqual(6);
        Expect(Length(W.LazyStrs)).ToEqual(6);
        for var I := 0 to High(W.Strs) do
          Expect(W.LazyStrs[I]).ToEqual(W.Strs[I]);
      end)

  .Scenario('The declared filter parameter is TFilterSpec, not a bare closure type')
    .When('a constant Boolean is passed as the filter',
      procedure(W: TOpsWorld)
      begin
        // TFilterSpec<T> converts from Boolean; TPredicate<T> does not. This only
        // compiles while Where's declared parameter really is TFilterSpec<T>.
        W.Collected := Linq.From<Integer>(W.Nums).Where(True).ToArray;
      end)
    .&Then('the constant filter keeps every element',
      procedure(W: TOpsWorld)
      begin
        Expect(Length(W.Collected)).ToEqual(6);
      end)

.Rule('A terminal that builds its collection frees it when the enumeration raises')

  .ScenarioOutline('A condition that raises halfway through the enumeration, under <Terminal>')
    .Given('an array of three objects freed with their last reference, and a query over it whose Where raises on the third',
      procedure(W: TOpsWorld)
      begin
        W.Counted := [TCounted.Create('A'), TCounted.Create('B'), TCounted.Create('C')];
        W.CountedQuery := Linq.From<ICounted>(W.Counted).Where(
          function(X: ICounted): Boolean
          begin
            if X.Name = 'C' then
              raise Exception.Create('fails on C');
            Result := True;
          end);
        Expect(Tracking.AliveNames).ToEqual('A,B,C');
      end)
    .When('<Terminal> is called, and the array and the query are let go',
      procedure(W: TOpsWorld)
      begin
        W.CallTerminal;
        W.Counted := nil;
        W.CountedQuery := Default(TQuery<ICounted>);
      end)
    .&Then('the exception reached the caller, and none of the three objects is alive',
      procedure(W: TOpsWorld)
      begin
        Expect(W.Raised).ToEqual('fails on C');
        Expect(Tracking.AliveNames).ToEqual('');
      end)
    .Examples(
      [['Terminal'],
       ['ToList()'],
       ['ToArray'],
       ['ToSmartArray']]);

end.
