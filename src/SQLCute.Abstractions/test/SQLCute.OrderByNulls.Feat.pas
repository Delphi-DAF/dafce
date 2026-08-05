unit SQLCute.OrderByNulls.Feat;

{
  The per-column NULL-ordering option: the AST field, the fluent surface that
  writes it, and the ANSI SQL it emits.
}

interface

uses
  Daf.SQLCute,
  Daf.SQLCute.Clauses,
  Daf.SQLCute.Compiler;

type
  /// <summary>World for the NULL-ordering scenarios: the query under test, the
  /// options read back from its AST, and the SQL two variants compile to.</summary>
  TOrderByNullsWorld = class
  public
    Query: IQuery;
    Cloned: IQuery;
    Compiler: IQueryCompiler;
    /// Option names ('Default' / 'First' / 'Last') read back from the AST.
    FirstNulls: string;
    SecondNulls: string;
    Sql: string;
    SecondSql: string;
    Bindings: Integer;
    SecondBindings: Integer;
    constructor Create;
  end;

implementation

uses
  System.SysUtils,
  Daf.MiniSpec;

constructor TOrderByNullsWorld.Create;
begin
  inherited;
  Compiler := TAnsiSqlCompiler.Create;
end;

// The Nth (0-based) non-raw ORDER BY clause of a query, or nil.
function OrderByAt(const Q: IQuery; Index: Integer): TOrderByClause;
begin
  Result := nil;
  var Seen := 0;
  for var C in Q.Clauses do
    if (C is TOrderByClause) and not TOrderByClause(C).IsRaw then
    begin
      if Seen = Index then
        Exit(TOrderByClause(C));
      Inc(Seen);
    end;
end;

// The option of the Nth ordered column, by name, so a failure reads plainly.
function NullsAt(const Q: IQuery; Index: Integer): string;
begin
  case OrderByAt(Q, Index).Nulls of
    TNullsOrder.First: Result := 'First';
    TNullsOrder.Last:  Result := 'Last';
  else
    Result := 'Default';
  end;
end;

initialization

Feature('''
TQuery — ORDER BY NULL ordering @unit @sqlcute

  Each ORDER BY column carries its own NULL-ordering option. Default emits
  nothing and leaves the placement to the database; First/Last say it out loud.
''')
.UseWorld<TOrderByNullsWorld>

// -------------------------------------------------------------------------

.Rule('The AST carries the option per ordered column')

  .Scenario('An unqualified ORDER BY keeps the default option')
    .When('a query orders by "age" with no NULL-ordering option',
      procedure(W: TOrderByNullsWorld)
      begin
        W.Query := TQuery.New.From('people').OrderBy('age');
        W.FirstNulls := NullsAt(W.Query, 0);
      end)
    .&Then('the clause reports Default',
      procedure(W: TOrderByNullsWorld)
      begin
        Expect(W.FirstNulls).ToEqual('Default');
      end)

  .Scenario('The option survives Clone')
    .When('a query whose ORDER BY carries Last is cloned',
      procedure(W: TOrderByNullsWorld)
      begin
        W.Query := TQuery.New.From('people').OrderBy('age');
        OrderByAt(W.Query, 0).Nulls := TNullsOrder.Last;
        W.Cloned := W.Query.Clone;
        W.FirstNulls := NullsAt(W.Cloned, 0);
      end)
    .&Then('the cloned clause carries the same option',
      procedure(W: TOrderByNullsWorld)
      begin
        Expect(W.FirstNulls).ToEqual('Last');
      end);

end.
