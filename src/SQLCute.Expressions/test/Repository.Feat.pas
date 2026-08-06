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
  System.Generics.Collections,
  Daf.SQLCute,
  Daf.Query.Expressions,
  Daf.SQLCute.Expressions,
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
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,
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
    .&Then('an EXISTS-carrying query raises when the plan is compiled',
      procedure(W: TRepositoryWorld)
      begin
        // The expression model cannot say EXISTS — that is Decision 6 working. So
        // the untranslatable query arrives the only way it can: as the base query
        // this repository is scoped to.
        var WithExists := TQuery.New.From('Product')
          .WhereExists(TQuery.New.From('Order').Where('total', '>', 0));

        var Scoped: IRepository<TProduct> :=
          TInMemoryRepository<TProduct>.Create(W.Products, WithExists);

        ExpectException(
          procedure
          begin
            Scoped.List(TSpecification<TProduct>.New.Where(Prop('Active') = True));
          end).ToBe(EInMemoryQueryError);
      end)

    .&Then('and the same query still compiles to SQL, which is the documented asymmetry',
      procedure(W: TRepositoryWorld)
      begin
        // Translatable is not the same as in-memory-executable. The failure above
        // is the lane's, not the emission's — and it is loud at plan-compile time.
        var Compiler: IQueryCompiler := TAnsiSqlCompiler.Create;
        var Sql := TQuery.New.From('Product')
          .WhereExists(TQuery.New.From('Order').Where('total', '>', 0))
          .Compile(Compiler).SQL;
        Expect(Sql).ToContain('EXISTS');
      end);

end.
