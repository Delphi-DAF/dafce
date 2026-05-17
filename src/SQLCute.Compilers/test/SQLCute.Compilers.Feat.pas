unit SQLCute.Compilers.Feat;

{
  Dialect compiler specs: quoting, param placeholders, and pagination
  for SqlServer, Postgres, MySQL, SQLite, Oracle, and Firebird.
}

interface

implementation

uses
  Daf.MiniSpec,
  SQLCute.SpecHelpers;

initialization

Feature('''
TQuery — Dialect Compilers @unit @sqlcute

  Each dialect compiler inherits TAnsiSqlCompiler and overrides only the
  parts that differ: identifier quoting, parameter placeholders, and pagination.
''')
.UseWorld<TSQLCuteWorld>

// -------------------------------------------------------------------------

.Rule('SQL Server: bracket quoting, @pN params, TOP / OFFSET-FETCH pagination @f6')

  .Scenario('SQL Server wraps identifiers with brackets and uses @pN params @f6')
    .Given('a dialect query selecting "id, name" from "users" where "active" equals 1')
    .When('compiled with the "SqlServer" dialect')
    .&Then('SQL is "SELECT [id], [name] FROM [users] WHERE [active] = @p0"')
    .&Then('has 1 binding')

  .Scenario('SQL Server injects TOP (N) for limit without offset @f6')
    .Given('a dialect query from "orders" limited to 10')
    .When('compiled with the "SqlServer" dialect')
    .&Then('SQL is "SELECT TOP (10) * FROM [orders]"')

  .Scenario('SQL Server uses OFFSET-FETCH for limit with offset @f6')
    .Given('a dialect query from "orders" limited to 10 offset 20')
    .When('compiled with the "SqlServer" dialect')
    .&Then('SQL is "SELECT * FROM [orders] OFFSET 20 ROWS FETCH NEXT 10 ROWS ONLY"')

// -------------------------------------------------------------------------

.Rule('Postgres: double-quote quoting, $N params, standard LIMIT/OFFSET @f6')

  .Scenario('Postgres wraps identifiers with double quotes and uses $N params @f6')
    .Given('a dialect query selecting "id, name" from "users" where "active" equals 1')
    .When('compiled with the "Postgres" dialect')
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

.Rule('Oracle: uppercase double-quote quoting, :pN params, FETCH FIRST / OFFSET-FETCH @f6')

  .Scenario('Oracle wraps identifiers in uppercase with double quotes and uses :pN params @f6')
    .Given('a dialect query selecting "id, name" from "users" where "active" equals 1')
    .When('compiled with the "Oracle" dialect')
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

;

end.
