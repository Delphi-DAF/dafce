unit SQLCute.Where.Feat;

{
  TQuery: WHERE conditions — equality, comparison, IS NULL, AND/OR, BETWEEN,
          raw fragments, IN / NOT IN, EXISTS / NOT EXISTS.
}

interface

implementation

uses
  System.SysUtils,
  System.StrUtils,
  System.Variants,
  Daf.MiniSpec,
  Daf.SQLCute,
  Daf.SQLCute.Clauses,
  SQLCute.SpecHelpers;

// Every clause of Q, nested groups flattened, so a scenario that asks whether a
// clause exists cannot be answered "no" merely because it moved inside a group.
function AllClauses(const Clauses: TArray<TAbstractClause>): TArray<TAbstractClause>;
begin
  Result := nil;
  for var C in Clauses do
  begin
    Result := Result + [C];
    if C is TNestedWhereClause then
      Result := Result + AllClauses(TNestedWhereClause(C).SubClauses);
  end;
end;

function ValueLessInClauseCount(const Q: IQuery): Integer;
begin
  Result := 0;
  for var C in AllClauses(Q.Clauses) do
    if (C is TWhereInClause) and (Length(TWhereInClause(C).Values) = 0) then
      Inc(Result);
end;

function InClauseCount(const Q: IQuery): Integer;
begin
  Result := 0;
  for var C in AllClauses(Q.Clauses) do
    if C is TWhereInClause then
      Inc(Result);
end;

// A raw fragment is what the in-memory lane refuses outright, so counting them
// is how "the other back-end can read this query" is asserted from here.
function RawConditionCount(const Q: IQuery): Integer;
begin
  Result := 0;
  for var C in AllClauses(Q.Clauses) do
    if (C is TRawClause) or ((C is TWhereClause) and (TWhereClause(C).Op = TWhereOp.Raw)) then
      Inc(Result);
end;

function FirstInClause(const Q: IQuery): TWhereInClause;
begin
  Result := nil;
  for var C in AllClauses(Q.Clauses) do
    if C is TWhereInClause then
      Exit(TWhereInClause(C));
end;

function FirstNestedGroup(const Q: IQuery): TNestedWhereClause;
begin
  Result := nil;
  for var C in Q.Clauses do
    if C is TNestedWhereClause then
      Exit(TNestedWhereClause(C));
end;

initialization

Feature('''
TQuery — WHERE @unit @sqlcute

  TQuery builds parameterised WHERE clauses via a fluent API.
  These specs cover the general Where* methods -- comparisons, IN / NOT IN lists
  and EXISTS / NOT EXISTS subquery conditions.

  The string-matching family (WhereLike, WhereStarts, WhereEnds, WhereContains
  and their Not/Or variants) lives in SQLCute.StringOps.Feat.pas: it grew large
  enough to read on its own, and it compiles through a different path -- one that
  wraps the column in LOWER() unless asked not to.
''')
.UseWorld<TSQLCuteWorld>

// -------------------------------------------------------------------------

.Rule('Equality condition adds a positional binding')

  .Scenario('WHERE column = value')
    .Given('a query from "users" where "active" = True')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE active = true"')
    .&Then('has 0 bindings')

  .Scenario('WHERE column = integer value')
    .Given('a query from "users" where "age" = 30')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE age = ?"')
    .&Then('has 1 binding')

// -------------------------------------------------------------------------

.Rule('Comparison operators work for any orderable column')

  .Scenario('WHERE column > value')
    .Given('a query from "products" where "price" > 100')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM products WHERE price > ?"')
    .&Then('has 1 binding')

  .Scenario('WHERE column <= value')
    .Given('a query from "events" where "seats" <= 0')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM events WHERE seats <= ?"')
    .&Then('has 1 binding')

  // LIKE written as the operator of the general Where, which is NOT WhereLike:
  // this path emits the column as it stands, with no LOWER() around it.
  .Scenario('WHERE column LIKE pattern')
    .Given('a query from "users" where "email" LIKE %example%')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE email LIKE ?"')
    .&Then('has 1 binding')

// -------------------------------------------------------------------------

.Rule('IS NULL tests skip the binding list')

  .Scenario('WHERE column IS NULL produces no binding')
    .Given('a query from "users" where "deleted_at" is null')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE deleted_at IS NULL"')
    .&Then('has 0 bindings')

  .Scenario('WHERE column IS NOT NULL')
    .Given('a query from "users" where "email" is not null')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE email IS NOT NULL"')
    .&Then('has 0 bindings')

// -------------------------------------------------------------------------

.Rule('AND and OR connectors combine conditions')

  .Scenario('Two WHERE calls use AND by default')
    .Given('a query from "users" with two AND conditions')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE active = true AND age > ?"')
    .&Then('has 1 binding')

  .Scenario('OR WHERE inserts an OR connector')
    .Given('a query from "roles" with two OR conditions')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM roles WHERE code = ? OR code = ?"')
    .&Then('has 2 bindings')

// -------------------------------------------------------------------------

.Rule('BETWEEN adds two positional bindings')

  .Scenario('WHERE column BETWEEN low AND high')
    .Given('a query from "orders" where "total" between 100 and 500')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM orders WHERE total BETWEEN ? AND ?"')
    .&Then('has 2 bindings')

// -------------------------------------------------------------------------

.Rule('Raw SQL fragments pass through verbatim')

  .Scenario('WhereRaw injects a raw SQL fragment')
    .Given('a query from "logs" with raw where "severity IN (1, 2, 3)"')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM logs WHERE severity IN (1, 2, 3)"')
    .&Then('has 0 bindings')

// -------------------------------------------------------------------------

.Rule('WhereIn and WhereNotIn generate IN / NOT IN with positional bindings')

  .Scenario('WhereIn with a list of integers')
    .Given('a WhereIn query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE id IN (?, ?, ?)"')
    .&Then('has 3 bindings')

  .Scenario('WhereIn chained with another WHERE')
    .Given('a WhereIn combined with WHERE')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE role IN (?, ?) AND active = true"')
    .&Then('has 2 bindings')

  .Scenario('WhereNotIn with a list of strings')
    .Given('a WhereNotIn query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM products WHERE status NOT IN (?, ?)"')
    .&Then('has 2 bindings')

// -------------------------------------------------------------------------

.Rule('OrWhereIn and OrWhereNotIn use the OR connector')

  .Scenario('OrWhereIn appends with OR')
    .Given('an OrWhereIn query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE active = true OR id IN (?, ?)"')
    .&Then('has 2 bindings')

  .Scenario('OrWhereNotIn appends with OR')
    .Given('an OrWhereNotIn query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE active = true OR status NOT IN (?, ?)"')
    .&Then('has 2 bindings')

// -------------------------------------------------------------------------

.Rule('The in operator by name binds its values, like WhereIn')

  .Scenario('An array is bound element by element')
    .Given('a Where with the in operator and an array')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE id IN (?, ?, ?)"')
    .&Then('has 3 bindings')

  .Scenario('A value that is not an array is bound whole')
    .Given('a Where with the in operator and a string')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE id IN (?)"')
    .&Then('has 1 binding')
    .&Then('bindings in order are "1) OR (1=1"')

  .Scenario('not in is bound the same way')
    .Given('an OrWhere with the not in operator and an array')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE active = true OR id NOT IN (?, ?)"')
    .&Then('has 2 bindings')

// -------------------------------------------------------------------------

.Rule('WhereExists and WhereNotExists test subquery row existence')

  .Scenario('WhereExists produces EXISTS (subquery)')
    .Given('a WhereExists query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE EXISTS (SELECT * FROM orders WHERE user_id = ?)"')
    .&Then('has 1 binding')

  .Scenario('WhereNotExists produces NOT EXISTS (subquery)')
    .Given('a WhereNotExists query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE NOT EXISTS (SELECT * FROM orders WHERE user_id = ?)"')
    .&Then('has 1 binding')

// -------------------------------------------------------------------------

.Rule('WhereIn, WhereNotIn and Where can be combined in one query')

  .Scenario('Combined WhereIn, WhereNotIn and WHERE')
    .Given('a combined WhereIn WhereNotIn and WHERE query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT id, name FROM orders WHERE status IN (?, ?) AND user_id NOT IN (?, ?) AND created_at > ?"')
    .&Then('has 5 bindings')

// -------------------------------------------------------------------------

.Rule('OrWhereNull and OrWhereNotNull use OR connector')

  .Scenario('OrWhereNull adds OR IS NULL')
    .Given('an OrWhereNull query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE name = ? OR deleted_at IS NULL"')
    .&Then('has 1 binding')

  .Scenario('OrWhereNotNull adds OR IS NOT NULL')
    .Given('an OrWhereNotNull query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE name = ? OR email IS NOT NULL"')
    .&Then('has 1 binding')

// -------------------------------------------------------------------------

.Rule('WhereTrue and WhereFalse emit the dialect literal')

  .Scenario('WhereTrue in ANSI')
    .Given('a WhereTrue query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE active = true"')
    .&Then('has 0 bindings')

  .Scenario('WhereFalse in ANSI')
    .Given('a WhereFalse query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE active = false"')
    .&Then('has 0 bindings')

// -------------------------------------------------------------------------

.Rule('WhereNot and OrWhereNot wrap the condition in NOT(...)')

  .Scenario('WhereNot wraps equality in NOT')
    .Given('a WhereNot query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE NOT (status = ?)"')
    .&Then('has 1 binding')

  .Scenario('OrWhereNot appends with OR NOT')
    .Given('an OrWhereNot query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE status = ? OR NOT (archived = true)"')
    .&Then('has 1 binding')

  .Scenario('WhereNot with operator wraps condition in NOT')
    .Given('a WhereNot with operator query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE NOT (age > ?)"')
    .&Then('has 1 binding')

  .Scenario('OrWhereNot with operator appends with OR NOT')
    .Given('an OrWhereNot with operator query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE active = true OR NOT (score < ?)"')
    .&Then('has 1 binding')

// -------------------------------------------------------------------------

.Rule('WhereNotBetween / OrWhereBetween / OrWhereNotBetween')

  .Scenario('WhereNotBetween produces NOT BETWEEN')
    .Given('a WhereNotBetween query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM orders WHERE total NOT BETWEEN ? AND ?"')
    .&Then('has 2 bindings')

  .Scenario('OrWhereBetween appends with OR BETWEEN')
    .Given('an OrWhereBetween query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE age > ? OR score BETWEEN ? AND ?"')
    .&Then('has 3 bindings')

  .Scenario('OrWhereNotBetween appends with OR NOT BETWEEN')
    .Given('an OrWhereNotBetween query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE active = true OR score NOT BETWEEN ? AND ?"')
    .&Then('has 2 bindings')

// -------------------------------------------------------------------------

.Rule('OrWhereExists and OrWhereNotExists use OR connector')

  .Scenario('OrWhereExists appends with OR EXISTS')
    .Given('an OrWhereExists query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE active = true OR EXISTS (SELECT * FROM orders WHERE user_id = ?)"')
    .&Then('has 1 binding')

  .Scenario('OrWhereNotExists appends with OR NOT EXISTS')
    .Given('an OrWhereNotExists query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE active = true OR NOT EXISTS (SELECT * FROM orders WHERE user_id = ?)"')
    .&Then('has 1 binding')

// -------------------------------------------------------------------------

.Rule('WhereNotInQuery, OrWhereInQuery, OrWhereNotInQuery use subquery')

  .Scenario('WhereNotInQuery produces NOT IN (subquery)')
    .Given('a WhereNotInQuery query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE id NOT IN (SELECT user_id FROM banned)"')
    .&Then('has 0 bindings')

  .Scenario('OrWhereInQuery appends with OR IN (subquery)')
    .Given('an OrWhereInQuery query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE active = true OR id IN (SELECT id FROM vip)"')
    .&Then('has 0 bindings')

  .Scenario('WhereInQuery produces IN (subquery)')
    .Given('a WhereInQuery query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE id IN (SELECT id FROM vip)"')
    .&Then('has 0 bindings')

  .Scenario('OrWhereNotInQuery appends with OR NOT IN (subquery)')
    .Given('an OrWhereNotInQuery query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE active = true OR id NOT IN (SELECT user_id FROM banned)"')
    .&Then('has 0 bindings')

// -------------------------------------------------------------------------
// The empty value set. `x IN (a, b)` is `x = a OR x = b`, so an empty list is
// the empty disjunction — False without consulting the column. The builder
// resolves it when the condition is added, which is why nothing below has to
// ask what either compiler does with a value-less clause: there isn't one.
// The scenarios that need a lane that executes live in EmptyIn.Feat, in the
// in-memory suite; this suite links no back-end but the SQL compiler.

.Rule('An empty IN set never reaches the AST or the SQL')

  .Scenario('EmptyInCompilesToValidSql')
    .When('a query with an empty IN set is compiled with ANSI',
      procedure(W: TSQLCuteWorld)
      begin
        W.Query := TQuery.New.From('products').WhereIn('price', []);
        W.LastResult := W.Compiler.Compile(W.Query);
      end)
    .&Then('the SQL carries no empty IN list and names the filtered column',
      procedure(W: TSQLCuteWorld)
      begin
        Expect(W.LastResult.SQL).ToEqual(
          'SELECT * FROM products WHERE price IS NULL AND price IS NOT NULL');
        Expect(ContainsText(W.LastResult.SQL, 'IN ()')).ToBeFalse;
      end)
    .&Then('and it binds nothing, because there was nothing to bind',
      procedure(W: TSQLCuteWorld)
      begin
        Expect(Length(W.LastResult.Bindings)).ToEqual(0);
      end)
    .&Then('and no condition is a raw fragment, so the in-memory lane accepts it too',
      procedure(W: TSQLCuteWorld)
      begin
        Expect(RawConditionCount(W.Query)).ToEqual(0);
      end)

  .Scenario('EmptyNotInCompilesToAnAlwaysTrueCondition')
    .When('a query with an empty NOT IN set is compiled with ANSI',
      procedure(W: TSQLCuteWorld)
      begin
        W.Query := TQuery.New.From('products').WhereNotIn('price', []);
        W.LastResult := W.Compiler.Compile(W.Query);
      end)
    .&Then('the always-false group is negated whole',
      procedure(W: TSQLCuteWorld)
      begin
        Expect(W.LastResult.SQL).ToEqual(
          'SELECT * FROM products WHERE NOT (price IS NULL AND price IS NOT NULL)');
      end)

  .Scenario('ValueLessInClauseIsNotConstructible')
    .When('queries are built with an empty set through all four fluent verbs',
      procedure(W: TSQLCuteWorld)
      begin
        W.Query := TQuery.New.From('products')
          .WhereIn('a', [])
          .WhereNotIn('b', [])
          .OrWhereIn('c', [])
          .OrWhereNotIn('d', []);
      end)
    .&Then('the clause list holds no IN clause with an empty value set',
      procedure(W: TSQLCuteWorld)
      begin
        // Not "the compiler handles it": the degenerate state stopped existing.
        Expect(ValueLessInClauseCount(W.Query)).ToEqual(0);
        Expect(InClauseCount(W.Query)).ToEqual(0);
      end)

  .Scenario('EmptyOrFormsCarryTheOrConnector')
    .When('an OR-connected empty set follows another condition',
      procedure(W: TSQLCuteWorld)
      begin
        W.Query := TQuery.New.From('products')
          .Where('active', True)
          .OrWhereIn('price', []);
        W.LastResult := W.Compiler.Compile(W.Query);
      end)
    .&Then('the group joins its predecessor with OR',
      procedure(W: TSQLCuteWorld)
      begin
        Expect(W.LastResult.SQL).ToEqual(
          'SELECT * FROM products WHERE active = true OR (price IS NULL AND price IS NOT NULL)');
      end)
    .&Then('and it carries the connector as the enum the normalizer reads on a group',
      procedure(W: TSQLCuteWorld)
      begin
        // The delegation goes through OrWhere, so the connector is TBoolOp on a
        // nested group — not the 'AND'/'OR' string a TWhereInClause carries.
        Expect(FirstNestedGroup(W.Query) <> nil).ToBeTrue;
        Expect(FirstNestedGroup(W.Query).Connector = TBoolOp.opOr).ToBeTrue;
      end)

  .Scenario('NonEmptyInIsUnchanged')
    .When('a query with a three-value IN set is compiled with ANSI',
      procedure(W: TSQLCuteWorld)
      begin
        W.Query := TQuery.New.From('users').WhereIn('id', [1, 2, 3]);
        W.LastResult := W.Compiler.Compile(W.Query);
      end)
    .&Then('it still produces one IN clause carrying its three values',
      procedure(W: TSQLCuteWorld)
      begin
        Expect(InClauseCount(W.Query)).ToEqual(1);
        Expect(Length(FirstInClause(W.Query).Values)).ToEqual(3);
        Expect(FirstInClause(W.Query).Connector).ToEqual('AND');
      end)
    .&Then('and the same SQL and bindings as before the guard existed',
      procedure(W: TSQLCuteWorld)
      begin
        Expect(W.LastResult.SQL).ToEqual('SELECT * FROM users WHERE id IN (?, ?, ?)');
        Expect(Length(W.LastResult.Bindings)).ToEqual(3);
      end)

  .Scenario('NonEmptyOrInStillCarriesTheOrString')
    .When('a non-empty OrWhereIn follows another condition',
      procedure(W: TSQLCuteWorld)
      begin
        W.Query := TQuery.New.From('users')
          .Where('active', True)
          .OrWhereIn('id', [10, 20]);
      end)
    .&Then('its connector is the string the normalizer compares it as',
      procedure(W: TSQLCuteWorld)
      begin
        Expect(FirstInClause(W.Query).Connector).ToEqual('OR');
      end)

// -------------------------------------------------------------------------

.Rule('Comparing with Null emits IS NULL or IS NOT NULL')

  .Scenario('Where with Null emits IS NULL')
    .When('query calls From(''t'').Where(''deleted_at'', Null)',
      procedure(W: TSQLCuteWorld)
      begin
        W.LastResult := W.Compiler.Compile(TQuery.New.From('t').Where('deleted_at', Null));
      end)
    .&Then('SQL is SELECT * FROM t WHERE deleted_at IS NULL with 0 bindings',
      procedure(W: TSQLCuteWorld)
      begin
        Expect(Format('%s | %d', [W.LastResult.SQL, Length(W.LastResult.Bindings)]))
          .ToEqual('SELECT * FROM t WHERE deleted_at IS NULL | 0');
      end)

  .Scenario('The distinct operator emits IS NOT NULL')
    .When('query calls From(''t'').Where(''active'', 1).OrWhere(''deleted_at'', ''<>'', Null)',
      procedure(W: TSQLCuteWorld)
      begin
        W.LastResult := W.Compiler.Compile(TQuery.New.From('t').Where('active', 1).OrWhere('deleted_at', '<>', Null));
      end)
    .&Then('SQL is SELECT * FROM t WHERE active = ? OR deleted_at IS NOT NULL with 1 binding',
      procedure(W: TSQLCuteWorld)
      begin
        Expect(Format('%s | %d', [W.LastResult.SQL, Length(W.LastResult.Bindings)]))
          .ToEqual('SELECT * FROM t WHERE active = ? OR deleted_at IS NOT NULL | 1');
      end)

  .Scenario('Any other operator also emits IS NOT NULL')
    .When('query calls From(''t'').Where(''age'', ''>'', Null)',
      procedure(W: TSQLCuteWorld)
      begin
        W.LastResult := W.Compiler.Compile(TQuery.New.From('t').Where('age', '>', Null));
      end)
    .&Then('SQL is SELECT * FROM t WHERE age IS NOT NULL with 0 bindings',
      procedure(W: TSQLCuteWorld)
      begin
        Expect(Format('%s | %d', [W.LastResult.SQL, Length(W.LastResult.Bindings)]))
          .ToEqual('SELECT * FROM t WHERE age IS NOT NULL | 0');
      end)

  .Scenario('WhereNot keeps its negation')
    .When('query calls From(''t'').WhereNot(''deleted_at'', Null)',
      procedure(W: TSQLCuteWorld)
      begin
        W.LastResult := W.Compiler.Compile(TQuery.New.From('t').WhereNot('deleted_at', Null));
      end)
    .&Then('SQL is SELECT * FROM t WHERE NOT (deleted_at IS NULL) with 0 bindings',
      procedure(W: TSQLCuteWorld)
      begin
        Expect(Format('%s | %d', [W.LastResult.SQL, Length(W.LastResult.Bindings)]))
          .ToEqual('SELECT * FROM t WHERE NOT (deleted_at IS NULL) | 0');
      end)

;

end.
