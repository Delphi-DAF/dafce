unit SQLCute.Compilers.Feat;

{
  Dialect compiler specs: quoting, param placeholders, and pagination
  for SqlServer, Postgres, MySQL, SQLite, Oracle, and Firebird.
}

interface

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.SQLCute,
  Daf.SQLCute.Compiler,
  Daf.SQLCute.Compiler.Postgres,
  Daf.SQLCute.Compiler.SqlServer,
  Daf.SQLCute.Compiler.SQLite,
  SQLCute.SpecHelpers;

initialization

Feature('''
TQuery — Dialect Compilers @unit @sqlcute

  Each dialect compiler inherits TAnsiSqlCompiler and overrides only the
  parts that differ: identifier quoting, the parameter placeholders of the
  native style, and pagination.
''')
.UseWorld<TSQLCuteWorld>

// -------------------------------------------------------------------------

.Rule('SQL Server: bracket quoting, @pN params in native style, TOP / OFFSET-FETCH pagination @f6')

  .Scenario('SQL Server wraps identifiers with brackets and uses @pN params in native style @f6')
    .Given('a dialect query selecting "id, name" from "users" where "active" equals 1')
    .When('compiled with the "SqlServer" dialect in native style')
    .&Then('SQL is "SELECT [id], [name] FROM [users] WHERE [active] = @p0"')
    .&Then('has 1 binding')

  .Scenario('SQL Server injects TOP (N) for limit without offset @f6')
    .Given('a dialect query from "orders" limited to 10')
    .When('compiled with the "SqlServer" dialect')
    .&Then('SQL is "SELECT TOP (10) * FROM [orders]"')

  .Scenario('SQL Server uses OFFSET-FETCH for limit with offset, ordered by (SELECT 0) when the query has no order @f6')
    .Given('a dialect query from "orders" limited to 10 offset 20')
    .When('compiled with the "SqlServer" dialect')
    .&Then('SQL is "SELECT * FROM [orders] ORDER BY (SELECT 0) OFFSET 20 ROWS FETCH NEXT 10 ROWS ONLY"')

  .Scenario('SQL Server keeps the query order in front of OFFSET-FETCH @f6')
    .Given('a dialect query from "orders" ordered by "id" limited to 10 offset 20')
    .When('compiled with the "SqlServer" dialect')
    .&Then('SQL is "SELECT * FROM [orders] ORDER BY [id] ASC OFFSET 20 ROWS FETCH NEXT 10 ROWS ONLY"')

  .Scenario('SQL Server orders a DISTINCT page by its first column @f6')
    .Given('a distinct dialect query selecting "name" from "orders" limited to 10 offset 20')
    .When('compiled with the "SqlServer" dialect')
    .&Then('SQL is "SELECT DISTINCT [name] FROM [orders] ORDER BY 1 OFFSET 20 ROWS FETCH NEXT 10 ROWS ONLY"')

// -------------------------------------------------------------------------

.Rule('Postgres: double-quote quoting, $N params in native style, standard LIMIT/OFFSET @f6')

  .Scenario('Postgres wraps identifiers with double quotes and uses $N params in native style @f6')
    .Given('a dialect query selecting "id, name" from "users" where "active" equals 1')
    .When('compiled with the "Postgres" dialect in native style')
    .&Then('SQL is "SELECT "id", "name" FROM "users" WHERE "active" = $1"')
    .&Then('has 1 binding')

  .Scenario('Postgres uses standard LIMIT and OFFSET @f6')
    .Given('a dialect query from "orders" limited to 10 offset 20')
    .When('compiled with the "Postgres" dialect')
    .&Then('SQL is "SELECT * FROM "orders" LIMIT 10 OFFSET 20"')

// -------------------------------------------------------------------------

.Rule('MySQL: backtick quoting, ? params, standard LIMIT/OFFSET @f6')

  .Scenario('MySQL wraps identifiers with backticks @f6')
    .Given('a dialect query selecting "id, name" from "users" where "active" equals 1')
    .When('compiled with the "MySQL" dialect')
    .&Then('SQL is "SELECT `id`, `name` FROM `users` WHERE `active` = ?"')
    .&Then('has 1 binding')

// -------------------------------------------------------------------------

.Rule('SQLite: double-quote quoting, ? params, standard LIMIT/OFFSET @f6')

  .Scenario('SQLite wraps identifiers with double quotes @f6')
    .Given('a dialect query selecting "id, name" from "users" where "active" equals 1')
    .When('compiled with the "SQLite" dialect')
    .&Then('SQL is "SELECT "id", "name" FROM "users" WHERE "active" = ?"')
    .&Then('has 1 binding')

// -------------------------------------------------------------------------

.Rule('Oracle: uppercase double-quote quoting, :pN params in native style, FETCH FIRST / OFFSET-FETCH @f6')

  .Scenario('Oracle wraps identifiers in uppercase with double quotes and uses :pN params in native style @f6')
    .Given('a dialect query selecting "id, name" from "users" where "active" equals 1')
    .When('compiled with the "Oracle" dialect in native style')
    .&Then('SQL is "SELECT "ID", "NAME" FROM "USERS" WHERE "ACTIVE" = :p1"')
    .&Then('has 1 binding')

  .Scenario('Oracle uses FETCH FIRST for limit only @f6')
    .Given('a dialect query from "orders" limited to 10')
    .When('compiled with the "Oracle" dialect')
    .&Then('SQL is "SELECT * FROM "ORDERS" FETCH FIRST 10 ROWS ONLY"')

  .Scenario('Oracle uses OFFSET-FETCH for limit with offset @f6')
    .Given('a dialect query from "orders" limited to 10 offset 20')
    .When('compiled with the "Oracle" dialect')
    .&Then('SQL is "SELECT * FROM "ORDERS" OFFSET 20 ROWS FETCH NEXT 10 ROWS ONLY"')

// -------------------------------------------------------------------------

.Rule('Firebird: no quoting, ? params, ROWS M TO N pagination @f6')

  .Scenario('Firebird does not quote identifiers @f6')
    .Given('a dialect query selecting "id, name" from "users" where "active" equals 1')
    .When('compiled with the "Firebird" dialect')
    .&Then('SQL is "SELECT id, name FROM users WHERE active = ?"')
    .&Then('has 1 binding')

  .Scenario('Firebird uses ROWS 1 TO N for limit only @f6')
    .Given('a dialect query from "orders" limited to 10')
    .When('compiled with the "Firebird" dialect')
    .&Then('SQL is "SELECT * FROM orders ROWS 1 TO 10"')

  .Scenario('Firebird uses ROWS M TO N for limit with offset @f6')
    .Given('a dialect query from "orders" limited to 10 offset 20')
    .When('compiled with the "Firebird" dialect')
    .&Then('SQL is "SELECT * FROM orders ROWS 21 TO 30"')

// -------------------------------------------------------------------------

.Rule('A new compiler writes ? for every binding, in every dialect')

  .Scenario('PostgreSQL writes ?')
    .Given('a dialect query from "t" where "a" equals 1 and "b" equals 2')
    .When('compiled with the "Postgres" dialect')
    .&Then('SQL is "SELECT * FROM "t" WHERE "a" = ? AND "b" = ?"')
    .&Then('bindings in order are "1, 2"')

  .Scenario('SQL Server writes ?')
    .Given('a dialect query from "t" where "a" equals 1 and "b" equals 2')
    .When('compiled with the "SqlServer" dialect')
    .&Then('SQL is "SELECT * FROM [t] WHERE [a] = ? AND [b] = ?"')

  .Scenario('Oracle writes ?')
    .Given('a dialect query from "t" where "a" equals 1 and "b" equals 2')
    .When('compiled with the "Oracle" dialect')
    .&Then('SQL is "SELECT * FROM "T" WHERE "A" = ? AND "B" = ?"')

// -------------------------------------------------------------------------

.Rule('A literal ? comes out with the FireDAC escape')

  .Scenario('The PostgreSQL JSON operator')
    .When('query calls From(''docs'').WhereRaw(''data \? ''''tag'''''').Where(''id'', 1), compiled with a new TPostgresCompiler',
      procedure(W: TSQLCuteWorld)
      begin
        W.Compiler := TPostgresCompiler.Create;
        W.LastResult := W.Compiler.Compile(TQuery.New.From('docs').WhereRaw('data \? ''tag''').Where('id', 1));
      end)
    .&Then('SQL is "SELECT * FROM "docs" WHERE data ?? ''tag'' AND "id" = ?"')

  .Scenario('Inside a string')
    .When('query calls From(''notes'').WhereRaw(''body = ''''what\?'''''').Where(''id'', 1), compiled with a new TPostgresCompiler',
      procedure(W: TSQLCuteWorld)
      begin
        W.Compiler := TPostgresCompiler.Create;
        W.LastResult := W.Compiler.Compile(TQuery.New.From('notes').WhereRaw('body = ''what\?''').Where('id', 1));
      end)
    .&Then('SQL is "SELECT * FROM "notes" WHERE body = ''what?'' AND "id" = ?"')

// -------------------------------------------------------------------------

.Rule('The native style writes the engine placeholder')

  .Scenario('PostgreSQL in native style')
    .Given('a dialect query from "t" where "a" equals 1 and "b" equals 2')
    .When('compiled with the "Postgres" dialect in native style')
    .&Then('SQL is "SELECT * FROM "t" WHERE "a" = $1 AND "b" = $2"')

  .Scenario('An escaped ? in native style')
    .When('query calls From(''docs'').WhereRaw(''data \? ''''tag'''''').Where(''id'', 1), compiled with a TPostgresCompiler in native style',
      procedure(W: TSQLCuteWorld)
      begin
        var Compiler := TPostgresCompiler.Create;
        Compiler.Placeholders := TPlaceholderStyle.Native;
        W.Compiler := Compiler;
        W.LastResult := W.Compiler.Compile(TQuery.New.From('docs').WhereRaw('data \? ''tag''').Where('id', 1));
      end)
    .&Then('SQL is "SELECT * FROM "docs" WHERE data ? ''tag'' AND "id" = $1"')

// -------------------------------------------------------------------------

.Rule('Postgres, Oracle, SQLite and Firebird emit NULLS FIRST/LAST natively')

  .Scenario('Postgres emits the clause after the ordered column')
    .Given('a dialect query from "people" ordered by "age" nulls first')
    .When('compiled with the "Postgres" dialect')
    .&Then('SQL is "SELECT * FROM "people" ORDER BY "age" ASC NULLS FIRST"')
    .&Then('has 0 bindings')

  .Scenario('SQLite emits the clause after the ordered column')
    .Given('a dialect query from "people" ordered by "age" nulls first')
    .When('compiled with the "SQLite" dialect')
    .&Then('SQL is "SELECT * FROM "people" ORDER BY "age" ASC NULLS FIRST"')

  .Scenario('Oracle emits the clause after its uppercased column')
    .Given('a dialect query from "people" ordered by "age" nulls last')
    .When('compiled with the "Oracle" dialect')
    .&Then('SQL is "SELECT * FROM "PEOPLE" ORDER BY "AGE" ASC NULLS LAST"')

  .Scenario('Firebird emits the clause on its unquoted column')
    .Given('a dialect query from "people" ordered by "age" desc nulls first')
    .When('compiled with the "Firebird" dialect')
    .&Then('SQL is "SELECT * FROM people ORDER BY age DESC NULLS FIRST"')

  .Scenario('An unqualified sort emits no NULLS clause on a native dialect')
    .Given('a dialect query from "people" ordered by "age"')
    .When('compiled with the "Postgres" dialect')
    .&Then('SQL is "SELECT * FROM "people" ORDER BY "age" ASC"')

// -------------------------------------------------------------------------

.Rule('MySQL and SQL Server emulate the requested order with a leading sort key')

  .Scenario('MySQL sorts a boolean NULL test before the column')
    .Given('a dialect query from "people" ordered by "age" nulls last')
    .When('compiled with the "MySQL" dialect')
    .&Then('SQL is "SELECT * FROM `people` ORDER BY (`age` IS NULL) ASC, `age` ASC"')
    .&Then('SQL does not contain "NULLS LAST"')
    .&Then('has 0 bindings')

  .Scenario('MySQL puts NULLs first by sorting the NULL test descending')
    .Given('a dialect query from "people" ordered by "age" desc nulls first')
    .When('compiled with the "MySQL" dialect')
    .&Then('SQL is "SELECT * FROM `people` ORDER BY (`age` IS NULL) DESC, `age` DESC"')

  .Scenario('SQL Server uses a CASE key, since ORDER BY takes no boolean expression')
    .Given('a dialect query from "people" ordered by "age" nulls last')
    .When('compiled with the "SqlServer" dialect')
    .&Then('SQL is "SELECT * FROM [people] ORDER BY CASE WHEN [age] IS NULL THEN 1 ELSE 0 END ASC, [age] ASC"')
    .&Then('SQL does not contain "NULLS"')

  .Scenario('SQL Server puts NULLs first on a descending sort')
    .Given('a dialect query from "people" ordered by "age" desc nulls first')
    .When('compiled with the "SqlServer" dialect')
    .&Then('SQL is "SELECT * FROM [people] ORDER BY CASE WHEN [age] IS NULL THEN 1 ELSE 0 END DESC, [age] DESC"')

  .Scenario('An unqualified sort emits no NULL-testing key at all')
    .Given('a dialect query from "people" ordered by "age"')
    .When('compiled with the "MySQL" dialect')
    .&Then('SQL is "SELECT * FROM `people` ORDER BY `age` ASC"')
    .&Then('SQL does not contain "IS NULL"')

  .Scenario('An unqualified sort on SQL Server emits no CASE key')
    .Given('a dialect query from "people" ordered by "age"')
    .When('compiled with the "SqlServer" dialect')
    .&Then('SQL is "SELECT * FROM [people] ORDER BY [age] ASC"')
    .&Then('SQL does not contain "CASE"')

// -------------------------------------------------------------------------

.Rule('WhereTrue and Where with a logical value emit the dialect literal')

  .Scenario('WhereTrue in SQL Server')
    .When('query calls From(''users'').WhereTrue(''active''), compiled with TSqlServerCompiler',
      procedure(W: TSQLCuteWorld)
      begin
        W.Compiler := TSqlServerCompiler.Create;
        W.LastResult := W.Compiler.Compile(TQuery.New.From('users').WhereTrue('active'));
      end)
    .&Then('SQL is SELECT * FROM [users] WHERE [active] = cast(1 as bit) with 0 bindings',
      procedure(W: TSQLCuteWorld)
      begin
        Expect(Format('%s | %d', [W.LastResult.SQL, Length(W.LastResult.Bindings)]))
          .ToEqual('SELECT * FROM [users] WHERE [active] = cast(1 as bit) | 0');
      end)

  .Scenario('WhereTrue in SQLite')
    .When('query calls From(''users'').WhereTrue(''active''), compiled with TSQLiteCompiler',
      procedure(W: TSQLCuteWorld)
      begin
        W.Compiler := TSQLiteCompiler.Create;
        W.LastResult := W.Compiler.Compile(TQuery.New.From('users').WhereTrue('active'));
      end)
    .&Then('SQL is SELECT * FROM "users" WHERE "active" = 1 with 0 bindings',
      procedure(W: TSQLCuteWorld)
      begin
        Expect(Format('%s | %d', [W.LastResult.SQL, Length(W.LastResult.Bindings)]))
          .ToEqual('SELECT * FROM "users" WHERE "active" = 1 | 0');
      end)

  .Scenario('Where with True in SQL Server')
    .When('query calls From(''users'').Where(''active'', True), compiled with TSqlServerCompiler',
      procedure(W: TSQLCuteWorld)
      begin
        W.Compiler := TSqlServerCompiler.Create;
        W.LastResult := W.Compiler.Compile(TQuery.New.From('users').Where('active', True));
      end)
    .&Then('SQL is SELECT * FROM [users] WHERE [active] = cast(1 as bit) with 0 bindings',
      procedure(W: TSQLCuteWorld)
      begin
        Expect(Format('%s | %d', [W.LastResult.SQL, Length(W.LastResult.Bindings)]))
          .ToEqual('SELECT * FROM [users] WHERE [active] = cast(1 as bit) | 0');
      end)

;

end.
