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
      end)

// -------------------------------------------------------------------------

.Rule('The fluent surface writes the option, per column')

  .Scenario('The overload sets the option on the column it adds')
    .When('a query calls OrderBy(age, Last) and OrderByDesc(name, First)',
      procedure(W: TOrderByNullsWorld)
      begin
        W.Query := TQuery.New.From('people')
          .OrderBy('age', TNullsOrder.Last)
          .OrderByDesc('name', TNullsOrder.First);
        W.FirstNulls := NullsAt(W.Query, 0);
        W.SecondNulls := NullsAt(W.Query, 1);
      end)
    .&Then('each column carries the option it was given',
      procedure(W: TOrderByNullsWorld)
      begin
        Expect(W.FirstNulls).ToEqual('Last');
        Expect(W.SecondNulls).ToEqual('First');
      end)

  .Scenario('A modifier qualifies only the preceding column')
    .When('a query calls OrderBy(age).NullsLast.OrderBy(name)',
      procedure(W: TOrderByNullsWorld)
      begin
        W.Query := TQuery.New.From('people')
          .OrderBy('age').NullsLast
          .OrderBy('name');
        W.FirstNulls := NullsAt(W.Query, 0);
        W.SecondNulls := NullsAt(W.Query, 1);
      end)
    .&Then('age carries Last and the later name column keeps Default',
      procedure(W: TOrderByNullsWorld)
      begin
        Expect(W.FirstNulls).ToEqual('Last');
        Expect(W.SecondNulls).ToEqual('Default');
      end)

  .Scenario('A modifier with no column to qualify fails fast')
    .&Then('NullsLast on a query with no ordered column raises',
      procedure(W: TOrderByNullsWorld)
      begin
        ExpectException(
          procedure
          begin
            TQuery.New.From('people').NullsLast;
          end).ToBe(EInvalidOpException);
      end)

  .Scenario('A raw ORDER BY is not a column a modifier can qualify')
    .&Then('NullsFirst after only an OrderByRaw raises',
      procedure(W: TOrderByNullsWorld)
      begin
        ExpectException(
          procedure
          begin
            TQuery.New.From('people').OrderByRaw('FIELD(status, ''a'', ''b'')').NullsFirst;
          end).ToBe(EInvalidOpException);
      end)

// -------------------------------------------------------------------------

.Rule('ANSI SQL emits the clause only when it was asked for')

  .Scenario('Default emits exactly the SQL it did before this option existed')
    .When('an unqualified OrderBy(name) is compiled with ANSI',
      procedure(W: TOrderByNullsWorld)
      begin
        W.Sql := TQuery.New.From('users').OrderBy('name').Compile(W.Compiler).SQL;
      end)
    .&Then('the SQL carries no NULLS clause',
      procedure(W: TOrderByNullsWorld)
      begin
        Expect(W.Sql).ToEqual('SELECT * FROM users ORDER BY name ASC');
      end)

  .Scenario('The requested placement is emitted after the direction')
    .When('OrderBy(age).NullsLast and OrderByDesc(age).NullsFirst are compiled',
      procedure(W: TOrderByNullsWorld)
      begin
        W.Sql := TQuery.New.From('people')
          .OrderBy('age').NullsLast.Compile(W.Compiler).SQL;
        W.SecondSql := TQuery.New.From('people')
          .OrderByDesc('age').NullsFirst.Compile(W.Compiler).SQL;
      end)
    .&Then('each column reads "col DIR NULLS FIRST|LAST"',
      procedure(W: TOrderByNullsWorld)
      begin
        Expect(W.Sql).ToEqual('SELECT * FROM people ORDER BY age ASC NULLS LAST');
        Expect(W.SecondSql).ToEqual('SELECT * FROM people ORDER BY age DESC NULLS FIRST');
      end)

  .Scenario('Only the qualified column of a multi-column sort is affected')
    .When('OrderBy(age).NullsLast.OrderBy(name) is compiled',
      procedure(W: TOrderByNullsWorld)
      begin
        W.Sql := TQuery.New.From('people')
          .OrderBy('age').NullsLast
          .OrderBy('name').Compile(W.Compiler).SQL;
      end)
    .&Then('the second column is emitted plainly',
      procedure(W: TOrderByNullsWorld)
      begin
        Expect(W.Sql).ToEqual(
          'SELECT * FROM people ORDER BY age ASC NULLS LAST, name ASC');
      end)

  .Scenario('The option adds no bindings')
    .When('the same filtered query is compiled unqualified and qualified',
      procedure(W: TOrderByNullsWorld)
      begin
        var Plain := TQuery.New.From('people').Where('country', 'US')
          .OrderBy('age').Compile(W.Compiler);
        var Qualified := TQuery.New.From('people').Where('country', 'US')
          .OrderBy('age').NullsLast.Compile(W.Compiler);
        W.Bindings := Length(Plain.Bindings);
        W.SecondBindings := Length(Qualified.Bindings);
      end)
    .&Then('both carry the same binding count',
      procedure(W: TOrderByNullsWorld)
      begin
        Expect(W.Bindings).ToEqual(1);
        Expect(W.SecondBindings).ToEqual(W.Bindings);
      end);

end.
