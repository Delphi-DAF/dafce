unit Queryable.Feat;

interface

uses
  System.Generics.Collections;

type
  TLinqWorld = class
  public
    Source: TList<Integer>;
    Collected: TArray<Integer>;
    Total: Integer;
    SecondPass: Integer;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.Linq;

{ TLinqWorld }

constructor TLinqWorld.Create;
begin
  inherited;
  Source := TList<Integer>.Create;
  Source.AddRange([10, 20, 30]);
end;

destructor TLinqWorld.Destroy;
begin
  Source.Free;
  inherited;
end;

// Consumes the sequence purely as System.IEnumerable<T> — proves assignability.
function SumAll(const Seq: IEnumerable<Integer>): Integer;
begin
  Result := 0;
  for var X in Seq do
    Inc(Result, X);
end;

initialization

Feature('''
Feature Linq @linq

  As a developer
  I want a deferred, ref-counted IQueryable<T> that extends IEnumerable<T>
  So I can iterate it with for..in and pass it wherever IEnumerable<T> is expected
''')

.UseWorld<TLinqWorld>

.Rule('IQueryable is a for..in-able, deferred sequence')

  .Scenario('for..in over a queryable yields every element')
    .Given('a queryable over a list [10,20,30]').NoAction
    .When('it is iterated with for..in',
      procedure(W: TLinqWorld)
      begin
        W.Collected := nil;
        for var X in Linq.From<Integer>(W.Source) do
          W.Collected := W.Collected + [X];
      end)
    .&Then('the loop yields 10, 20, 30',
      procedure(W: TLinqWorld)
      begin
        Expect(Length(W.Collected)).ToEqual(3);
        Expect(W.Collected[0]).ToEqual(10);
        Expect(W.Collected[1]).ToEqual(20);
        Expect(W.Collected[2]).ToEqual(30);
      end)

  .Scenario('A queryable is assignable where a plain enumerable is expected')
    .When('a queryable is passed to a function that consumes a plain enumerable',
      procedure(W: TLinqWorld)
      begin
        W.Total := SumAll(Linq.From<Integer>(W.Source));
      end)
    .&Then('it is consumed with no explicit bridge and sums to 60',
      procedure(W: TLinqWorld)
      begin
        Expect(W.Total).ToEqual(60);
      end)

  .Scenario('From over a list is non-owning and re-iterable')
    .When('the same queryable is iterated twice',
      procedure(W: TLinqWorld)
      begin
        var Q := Linq.From<Integer>(W.Source);
        var First := 0;
        for var X in Q do
          Inc(First, X);
        W.SecondPass := 0;
        for var X in Q do
          Inc(W.SecondPass, X);
        W.Total := First;
      end)
    .&Then('both passes see the full source and the list survives',
      procedure(W: TLinqWorld)
      begin
        Expect(W.Total).ToEqual(60);
        Expect(W.SecondPass).ToEqual(60);
        Expect(W.Source.Count).ToEqual(3);
      end);

end.
