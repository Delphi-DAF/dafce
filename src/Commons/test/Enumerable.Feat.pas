unit Enumerable.Feat;

interface

uses
  System.Generics.Collections,
  Daf.Nullable;

type
  TKV = record
    Key: string;
    Num: Integer;
  end;

  TEnumerableWorld = class
  public
    FactoryCalls: Integer;
    Seq: IEnumerable<Integer>;
    Collected: TArray<Integer>;
    SecondCount: Integer;
    Records: TArray<TKV>;
    Nullables: TArray<TNullable<Integer>>;
  end;

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.Enumerable;

initialization

Feature('''
Feature Enumerable @enumerable @commons

  As a developer
  I want a lazy, deferred sequence primitive
  So I can build IEnumerable<T> from a generator closure without eager work
''')

.UseWorld<TEnumerableWorld>

.Rule('Deferred, re-iterable generation')

  .Scenario('A deferred enumerable does not run until iterated')
    .Given('a deferred enumerable over a lazy 1..3 generator',
      procedure(W: TEnumerableWorld)
      begin
        W.FactoryCalls := 0;
        W.Seq := TDeferredEnumerable<Integer>.Create(
          function: IEnumerator<Integer>
          var
            N: Integer;
          begin
            Inc(W.FactoryCalls);
            N := 0;
            Result := TGeneratorEnumerator<Integer>.Create(
              function(var Current: Integer): Boolean
              begin
                Result := N < 3;
                if Result then
                begin
                  Inc(N);
                  Current := N;
                end;
              end);
          end);
      end)
    .&Then('the factory has not run yet',
      procedure(W: TEnumerableWorld)
      begin
        Expect(W.FactoryCalls).ToEqual(0);
      end)
    .&And('iterating yields 1,2,3 and runs the factory exactly once',
      procedure(W: TEnumerableWorld)
      begin
        W.Collected := nil;
        for var X in W.Seq do
          W.Collected := W.Collected + [X];
        Expect(W.FactoryCalls).ToEqual(1);
        Expect(Length(W.Collected)).ToEqual(3);
        Expect(W.Collected[0]).ToEqual(1);
        Expect(W.Collected[1]).ToEqual(2);
        Expect(W.Collected[2]).ToEqual(3);
      end)
    .&And('iterating a second time re-runs the factory and yields the same values',
      procedure(W: TEnumerableWorld)
      begin
        W.SecondCount := 0;
        for var X in W.Seq do
          Inc(W.SecondCount);
        Expect(W.FactoryCalls).ToEqual(2);
        Expect(W.SecondCount).ToEqual(3);
      end)

.Rule('Value-type elements')

  .Scenario('A record sequence round-trips through the enumerator adapter')
    .When('a record list is adapted to an enumerable and iterated',
      procedure(W: TEnumerableWorld)
      begin
        var L := TList<TKV>.Create;
        var A: TKV; A.Key := 'a'; A.Num := 1; L.Add(A);
        var B: TKV; B.Key := 'b'; B.Num := 2; L.Add(B);
        W.Records := nil;
        var E: IEnumerable<TKV> := TEnumerableAdapter<TKV>.Create(L);
        for var KV in E do
          W.Records := W.Records + [KV];
      end)
    .&Then('each record preserves its fields',
      procedure(W: TEnumerableWorld)
      begin
        Expect(Length(W.Records)).ToEqual(2);
        Expect(W.Records[0].Key).ToEqual('a');
        Expect(W.Records[0].Num).ToEqual(1);
        Expect(W.Records[1].Key).ToEqual('b');
        Expect(W.Records[1].Num).ToEqual(2);
      end)

  .Scenario('A TNullable sequence round-trips through the generator')
    .When('a generator yields [null, 5]',
      procedure(W: TEnumerableWorld)
      begin
        W.Nullables := nil;
        var E: IEnumerable<TNullable<Integer>> := TDeferredEnumerable<TNullable<Integer>>.Create(
          function: IEnumerator<TNullable<Integer>>
          var
            I: Integer;
          begin
            I := 0;
            Result := TGeneratorEnumerator<TNullable<Integer>>.Create(
              function(var Current: TNullable<Integer>): Boolean
              begin
                Result := I < 2;
                if Result then
                begin
                  Inc(I);
                  if I = 1 then
                    Current := TNullable<Integer>.Null
                  else
                    Current := 5;
                end;
              end);
          end);
        for var N in E do
          W.Nullables := W.Nullables + [N];
      end)
    .&Then('the first is null and the second holds 5',
      procedure(W: TEnumerableWorld)
      begin
        Expect(Length(W.Nullables)).ToEqual(2);
        Expect(W.Nullables[0].IsNull).ToBeTrue;
        Expect(W.Nullables[1].IsNull).ToBeFalse;
        Expect(W.Nullables[1].Value).ToEqual(5);
      end)

  .Scenario('ToArray collects all elements in order')
    .When('ToArray is called over a five-element record sequence',
      procedure(W: TEnumerableWorld)
      begin
        var L := TList<TKV>.Create;
        for var K := 1 to 5 do
        begin
          var R: TKV; R.Key := 'k' + K.ToString; R.Num := K; L.Add(R);
        end;
        var E: IEnumerable<TKV> := TEnumerableAdapter<TKV>.Create(L);
        var Conv: ToIEnumerable<TKV> := E;
        W.Records := Conv.ToArray;
      end)
    .&Then('all five elements are present in order',
      procedure(W: TEnumerableWorld)
      begin
        Expect(Length(W.Records)).ToEqual(5);
        Expect(W.Records[0].Num).ToEqual(1);
        Expect(W.Records[4].Num).ToEqual(5);
      end)

  .Scenario('Reset is unsupported on lazy enumerators')
    .&Then('calling Reset raises ENotImplemented',
      procedure(W: TEnumerableWorld)
      begin
        ExpectException(
          procedure
          var
            E: IEnumerator<Integer>;
          begin
            E := TGeneratorEnumerator<Integer>.Create(
              function(var C: Integer): Boolean
              begin
                Result := False;
              end);
            E.Reset;
          end).ToBe(ENotImplemented);
      end);

end.
