unit Parity.Feat;

{
  One specification, one emission, both back-ends.

  DAFce ships no database driver, so this harness cannot execute the emitted SQL
  — the same constraint the in-memory module's own parity harness works under.
  The oracle is therefore declared: the row set a SQL database returns for this
  query over this seed. What is proved is that one emission serves both lanes,
  that the SQL back-end accepts the very same AST with self-consistent bindings
  and the predicate intact, that the in-memory lane produces the declared rows,
  and that neither lane mutated the query.

  Parity itself is inherited rather than tested into existence: both lanes consume
  the SQLCute AST, so a single emission is the whole mechanism. The thing worth
  asserting is that there is only one.
}

interface

uses
  System.Generics.Collections,
  Daf.SQLCute,
  Daf.Specification,
  Emission.SpecHelpers;

type
  TSpecParityWorld = class
  public
    Products: TObjectList<TProduct>;
    /// The one query both lanes are handed.
    Query: IQuery;
    ClausesBefore: Integer;
    ClausesAfter: Integer;
    Sql: string;
    Bindings: Integer;
    Placeholders: Integer;
    InMemory: TArray<string>;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.SQLCute.Compiler,
  Daf.SQLCute.Specification;

destructor TSpecParityWorld.Destroy;
begin
  Products.Free;
  inherited;
end;

function PlaceholderCount(const Sql: string): Integer;
begin
  Result := 0;
  for var Ch in Sql do
    if Ch = '?' then
      Inc(Result);
end;

initialization

Feature('''
Feature One specification, two back-ends @expressions @parity

  As a developer who wants the same rule to run against a database or a list
  I want a single emission to serve both back-ends
  So parity is a property of the architecture rather than a standing test burden
''')

.UseWorld<TSpecParityWorld>

.Rule('One emission feeds both lanes')

  .Scenario('OneEmissionServesTheSqlCompilerAndTheInMemoryLane')
    .When('a specification is emitted once and the query handed to both lanes',
      procedure(W: TSpecParityWorld)
      begin
        W.Products := SampleProducts;

        var Spec := TSpecification<TProduct>.New
          .Where((Prop('Price') > 100) and (Prop('Active') = True))
          .OrderByDesc('Price');

        // Lowered exactly once, into one query, which is then shared.
        W.Query := TExprEmitter.Apply<TProduct>(TQuery.New.From('Product'), Spec);
        W.ClausesBefore := Length(W.Query.Clauses);

        var Compiler: IQueryCompiler := TAnsiSqlCompiler.Create;
        var Compiled := W.Query.Compile(Compiler);
        W.Sql := Compiled.SQL;
        W.Bindings := Length(Compiled.Bindings);
        W.Placeholders := PlaceholderCount(W.Sql);

        W.InMemory := NamesOf(W.Products, W.Query);

        W.ClausesAfter := Length(W.Query.Clauses);
      end)
    .&Then('the SQL lane accepted the same AST, with bindings matching its placeholders',
      procedure(W: TSpecParityWorld)
      begin
        Expect(W.Sql).ToStartWith('SELECT');
        Expect(W.Sql).ToContain('Price');
        Expect(W.Sql).ToContain('Active');
        Expect(W.Sql).ToContain('ORDER BY');
        Expect(W.Sql).ToContain('DESC');
        Expect(W.Placeholders).ToEqual(W.Bindings);
        Expect(W.Bindings).ToEqual(2);
      end)
    .&Then('the in-memory lane returned the declared row set',
      procedure(W: TSpecParityWorld)
      begin
        // The oracle: Price > 100 and Active, by price descending. Crane is over
        // 100 but inactive; Drill's NULL price makes the comparison unknown.
        Expect(string.Join(',', W.InMemory)).ToEqual('Engine,Anvil');
      end)
    .&Then('exactly one emission happened, and neither lane mutated the query',
      procedure(W: TSpecParityWorld)
      begin
        // A second emission onto the same query would have appended its clauses,
        // so the count is what makes "once" checkable rather than merely intended.
        // One FROM, two WHERE conditions, one ORDER BY.
        Expect(W.ClausesBefore).ToEqual(4);
        Expect(W.ClausesAfter).ToEqual(W.ClausesBefore);
      end);

end.
