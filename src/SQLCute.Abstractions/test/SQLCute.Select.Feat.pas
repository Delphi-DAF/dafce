unit SQLCute.Select.Feat;

{
  Feature: SELECT compilation — Phase 1 scenarios
  Covers:  SELECT *, SELECT columns, FROM, LIMIT, OFFSET, ORDER BY, and the
           combined "full query" scenario from the acceptance criterion.
}

interface

implementation

uses
  Daf.MiniSpec,
  SQLCute.SpecHelpers;

initialization

Feature('''
Feature SELECT compilation @sqlcute @select

  As a developer
  I want to build SELECT queries with the SQLCute fluent API
  So I can retrieve data using readable, composable query objects
''')
.UseWorld<TSQLCuteWorld>

// -------------------------------------------------------------------------

.Rule('Default query selects all columns')

  .Scenario('Query with no clauses compiles to SELECT *')
    .Given('an empty query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT *"')

  .Scenario('FROM clause is appended after SELECT *')
    .Given('a query from "users"')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users"')

// -------------------------------------------------------------------------

.Rule('SELECT list narrows the result columns')

  .Scenario('SELECT a single column')
    .Given('a query selecting "id" from "users"')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT id FROM users"')

  .Scenario('SELECT multiple columns')
    .Given('a query selecting "id, name, email" from "accounts"')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT id, name, email FROM accounts"')

// -------------------------------------------------------------------------

.Rule('LIMIT and OFFSET control pagination')

  .Scenario('LIMIT restricts the number of rows')
    .Given('a query from "orders" with limit 20')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM orders LIMIT 20"')

  .Scenario('OFFSET skips rows when combined with LIMIT')
    .Given('a query from "orders" with limit 10 offset 30')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM orders LIMIT 10 OFFSET 30"')

// -------------------------------------------------------------------------

.Rule('ORDER BY sorts the result set')

  .Scenario('ORDER BY a single column ascending')
    .Given('a query from "users" ordered by "name"')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users ORDER BY name ASC"')

  .Scenario('ORDER BY a column descending')
    .Given('a query from "users" ordered by "created_at" desc')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users ORDER BY created_at DESC"')

// -------------------------------------------------------------------------

.Rule('Queries can be cloned into independent variants')

  .Scenario('Clone of a query produces identical SQL')
    .Given('a query selecting "id" from "users"')
    .When('I clone the query and compile the clone')
    .&Then('SQL is "SELECT id FROM users"')

// -------------------------------------------------------------------------

.Rule('Acceptance criterion: full Phase-1 combined query')

  .Scenario('SELECT id,name FROM users WHERE active=? LIMIT 10')
    .Given('the Phase-1 acceptance query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT id, name FROM users WHERE active = ? LIMIT 10"')
    .&Then('has 1 binding')

end.
