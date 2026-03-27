unit SQLCute.Where.Feat;

{
  Feature: WHERE compilation — Phase 1 scenarios
  Covers:  equality, comparison operators, IS NULL / IS NOT NULL, AND, OR,
           BETWEEN, and raw WHERE fragments.
}

interface

implementation

uses
  Daf.MiniSpec,
  SQLCute.SpecHelpers;

initialization

Feature('''
Feature WHERE conditions @sqlcute @where

  As a developer
  I want to express filter conditions using the fluent API
  So I can build parameterised, injection-safe WHERE clauses
''')
.UseWorld<TSQLCuteWorld>

// -------------------------------------------------------------------------

.Rule('Equality condition adds a positional binding')

  .Scenario('WHERE column = value')
    .Given('a query from "users" where "active" = True')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE active = ?"')
    .&Then('has 1 binding')

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
    .&Then('SQL is "SELECT * FROM users WHERE active = ? AND age > ?"')
    .&Then('has 2 bindings')

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

end.
