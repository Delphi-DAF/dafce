unit Enumerable.Feat;

interface

uses
  System.Generics.Collections;

type
  TEnumerableWorld = class
  public
    FactoryCalls: Integer;
    Seq: IEnumerable<Integer>;
    Collected: TArray<Integer>;
    SecondCount: Integer;
  end;

implementation

uses
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
      end);

end.
