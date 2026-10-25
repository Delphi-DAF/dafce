unit Repository.Feat;

{
  The in-memory adapter. Two things are being asserted, and only one of them is
  about results:

    * a specification resolves to exactly the satisfying elements, in the order
      and page it declares;
    * when the lane cannot execute a query, the error propagates instead of the
      adapter quietly filtering the collection in Delphi.

  The second is the one worth having a spec for. A client-side fallback is easy
  to add, invisible when it works, and catastrophic when it hides a query that
  should have been rewritten — EF Core shipped one and removed it in 3.0.
}

interface

uses
  System.SysUtils,
  System.SyncObjs,
  System.Generics.Collections,
  Daf.SQLCute,
  Daf.Specification,
  Daf.SQLCute.Specification,
  Emission.SpecHelpers;

type
  TRepositoryWorld = class
  public
    Products: TObjectList<TProduct>;
    Repo: TInMemoryRepository<TProduct>;
    Port: IRepository<TProduct>;
    Names: TArray<string>;
    Cheap: TArray<string>;
    Expensive: TArray<string>;
    Again: TArray<string>;
    CompiledCount: Integer;
    KeyFirst: string;
    KeyRepeat: string;
    /// The untranslatable query and the repository scoped to it, plus what each lane answered.
    WithExists: IQuery;
    Scoped: IRepository<TProduct>;
    Outcome: string;
    Sql: string;
    /// Three rules, what each answers alone, and what the threads reported.
    Specs: TArray<TSpecification<TProduct>>;
    Expected: TArray<string>;
    Failures: TArray<string>;
    constructor Create;
    destructor Destroy; override;
    procedure Race;
  end;

implementation

uses
  System.Classes,
  Daf.MiniSpec,
  Daf.SQLCute.InMemory.Compiler,   // EInMemoryQueryError
  Daf.SQLCute.Compiler;

constructor TRepositoryWorld.Create;
begin
  inherited;
  Products := SampleProducts;
  Repo := TInMemoryRepository<TProduct>.Create(Products);
  // Held as the port as well, since that is how a domain service sees it. The
  // interface reference owns the instance.
  Port := Repo;
end;

destructor TRepositoryWorld.Destroy;
begin
  Port := nil;
  Products.Free;
  inherited;
end;

function NamesFrom(const Items: TArray<TProduct>): TArray<string>;
begin
  Result := nil;
  for var P in Items do
    Result := Result + [P.Name];
end;

const
  RaceThreads = 4;
  RaceRounds = 200;

// The index comes in as a parameter: a captured loop variable is shared by every thread.
function RaceBody(W: TRepositoryWorld; const Idx: Integer; const Done: TCountdownEvent): TProc;
begin
  Result :=
    procedure
    begin
      try
        for var Round := 1 to RaceRounds do
          for var S := 0 to High(W.Specs) do
          begin
            var Got := string.Join(',', NamesFrom(W.Port.List(W.Specs[S])));
            if (Got <> W.Expected[S]) and (W.Failures[Idx] = '') then
              W.Failures[Idx] := Format('thread %d: %s', [Idx, Got]);
          end;
      except
        on E: Exception do W.Failures[Idx] := Format('thread %d: %s', [Idx, E.ClassName]);
      end;
      Done.Signal;
    end;
end;

procedure TRepositoryWorld.Race;
begin
  SetLength(Failures, RaceThreads);
  var Done := TCountdownEvent.Create(RaceThreads);
  try
    for var T := 0 to RaceThreads - 1 do
      TThread.CreateAnonymousThread(RaceBody(Self, T, Done)).Start;
    if Done.WaitFor(60000) <> wrSignaled then
      Failures := Failures + ['TIMEOUT'];
  finally
    Done.Free;
  end;
  Outcome := '';
  for var F in Failures do
    if F <> '' then
      Outcome := Outcome + F + ' ';
  if Outcome = '' then
    Outcome := '<all match>';
end;

initialization

Feature('''
Feature In-memory repository adapter @expressions @repository

  As an application composed against IRepository<T>
  I want a specification resolved over an already-loaded collection
  So hexagonal composition works with no database, and nothing degrades silently
''')

.UseWorld<TRepositoryWorld>

.Rule('A specification resolves to exactly the elements satisfying it')

  .Scenario('SpecificationSelectsFromList')
    .When('a rule with a filter, an ordering and a page is resolved',
      procedure(W: TRepositoryWorld)
      begin
        W.Names := NamesFrom(W.Port.List(
          TSpecification<TProduct>.New
            .Where((Prop('Price') > 5) and (Prop('Active') = True))
            .OrderByDesc('Price')));
      end)
    .&Then('the satisfying elements come back in the declared order',
      procedure(W: TRepositoryWorld)
      begin
        // Crane is inactive, Bolt and File are not over 5, Drill's price is NULL
        // so the comparison is unknown and it is excluded.
        Expect(Length(W.Names)).ToEqual(2);
        Expect(W.Names[0]).ToEqual('Engine');   // 150
        Expect(W.Names[1]).ToEqual('Anvil');    // 120
      end)

  .Scenario('The declared page is applied, not just the ordering')
    .When('the same rule takes one element after skipping one',
      procedure(W: TRepositoryWorld)
      begin
        W.Names := NamesFrom(W.Port.List(
          TSpecification<TProduct>.New
            .Where(Prop('Active') = True)
            .OrderByDesc('Price')
            .Skip(1)
            .Take(1)));
      end)
    .&Then('exactly that one element is returned',
      procedure(W: TRepositoryWorld)
      begin
        // Active, by price desc: Engine(150), Anvil(120), Bolt(5), Drill(NULL).
        Expect(Length(W.Names)).ToEqual(1);
        Expect(W.Names[0]).ToEqual('Anvil');
      end)

.Rule('One loaded set serves many rules')

  .Scenario('OneLoadedSetSeveralSpecifications')
    .When('three rules are resolved against the same repository, one of them twice',
      procedure(W: TRepositoryWorld)
      begin
        var Cheap := TSpecification<TProduct>.New.Where(Prop('Price') < 10);
        var Expensive := TSpecification<TProduct>.New.Where(Prop('Price') > 100);

        W.KeyFirst := Cheap.Key;
        W.Cheap := NamesFrom(W.Port.List(Cheap));
        W.Expensive := NamesFrom(W.Port.List(Expensive));
        W.Again := NamesFrom(W.Port.List(Cheap));
        W.KeyRepeat := Cheap.Key;
        W.CompiledCount := W.Repo.Compiled;
      end)
    .&Then('each is answered from the one collection, and a repeated rule reuses its plan',
      procedure(W: TRepositoryWorld)
      begin
        Expect(string.Join(',', W.Cheap)).ToEqual('Bolt,File');
        Expect(string.Join(',', W.Expensive)).ToEqual('Anvil,Crane,Engine');
        Expect(string.Join(',', W.Again)).ToEqual(string.Join(',', W.Cheap));

        // The repeated rule really is the same rule, by key.
        Expect(W.KeyFirst).ToEqual(W.KeyRepeat);
        // Two distinct rules, two plans. The third call was the first rule again,
        // so it compiled nothing: the plan is cached by the rule's key.
        Expect(W.CompiledCount).ToEqual(2);
      end)

.Rule('What the lane cannot execute is reported, never worked around')

  .Scenario('UntranslatableSpecificationFailsLoudly')
    .Given('a repository scoped to an EXISTS-carrying base query',
      procedure(W: TRepositoryWorld)
      begin
        // The expression model cannot say EXISTS. So
        // the untranslatable query arrives the only way it can: as the base query
        // this repository is scoped to.
        W.WithExists := TQuery.New.From('Product')
          .WhereExists(TQuery.New.From('Order').Where('total', '>', 0));

        W.Scoped := TInMemoryRepository<TProduct>.Create(W.Products, W.WithExists);
      end)
    .When('the same query is compiled to SQL, and then listed through the in-memory lane',
      procedure(W: TRepositoryWorld)
      begin
        // Compiled first, so what the second half asserts is this very query and not a
        // look-alike built again three lines further down.
        var Compiler: IQueryCompiler := TAnsiSqlCompiler.Create;
        W.Sql := W.WithExists.Compile(Compiler).SQL;

        try
          W.Scoped.List(TSpecification<TProduct>.New.Where(Prop('Active') = True));
          W.Outcome := '<the lane executed it>';
        except
          on E: Exception do
            W.Outcome := E.ClassName + ': ' + E.Message;
        end;
      end)
    .&Then('an EXISTS-carrying query raises when the plan is compiled',
      procedure(W: TRepositoryWorld)
      begin
        Expect(W.Outcome).ToStartWith('EInMemoryQueryError');
      end)

    .&Then('and the same query still compiles to SQL, which is the documented asymmetry',
      procedure(W: TRepositoryWorld)
      begin
        // Translatable is not the same as in-memory-executable. The failure above
        // is the lane's, not the emission's — and it is loud at plan-compile time.
        Expect(W.Sql).ToContain('EXISTS');
      end)

.Rule('An in-memory repository serves several threads')

  .Scenario('Four threads share a repository')
    .Given('an in-memory repository and three distinct specifications, resolved one by one beforehand',
      procedure(W: TRepositoryWorld)
      begin
        W.Specs := [
          TSpecification<TProduct>.New.Where(Prop('Active') = True).OrderBy('Name'),
          TSpecification<TProduct>.New.Where(Prop('Price') > 5).OrderByDesc('Price'),
          TSpecification<TProduct>.New.Where(Prop('Price') < 100).OrderBy('Name')];
        // Resolved on a repository of their own, so the shared one starts with no plan.
        var Alone: IRepository<TProduct> := TInMemoryRepository<TProduct>.Create(W.Products);
        W.Expected := nil;
        for var S in W.Specs do
          W.Expected := W.Expected + [string.Join(',', NamesFrom(Alone.List(S)))];
      end)
    .When('four threads ask for the three many times at once', procedure(W: TRepositoryWorld) begin W.Race; end)
    .&Then('every answer matches its specification resolved alone, and no thread gets an exception',
      procedure(W: TRepositoryWorld)
      begin
        Expect(W.Outcome).ToEqual('<all match>');
      end)
    .&Then('and the repository holds three plans',
      procedure(W: TRepositoryWorld)
      begin
        Expect(W.Repo.Compiled).ToEqual(3);
      end);

end.
