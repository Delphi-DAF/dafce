unit SQLCute.Where.Feat;

{
  TQuery: WHERE conditions — equality, comparison, IS NULL, AND/OR, BETWEEN,
          raw fragments, IN / NOT IN, EXISTS / NOT EXISTS.
}

interface

implementation

uses
  Daf.MiniSpec,
  SQLCute.SpecHelpers;

initialization

Feature('''
TQuery — WHERE @unit @sqlcute

  TQuery builds parameterised WHERE clauses via a fluent API.
  These specs cover all Where* methods, IN / NOT IN lists
  and EXISTS / NOT EXISTS subquery conditions.
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
    .&Then('SQL is "SELECT * FROM users WHERE role IN (?, ?) AND active = ?"')
    .&Then('has 3 bindings')

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
    .&Then('SQL is "SELECT * FROM users WHERE active = ? OR id IN (?, ?)"')
    .&Then('has 3 bindings')

  .Scenario('OrWhereNotIn appends with OR')
    .Given('an OrWhereNotIn query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE active = ? OR status NOT IN (?, ?)"')
    .&Then('has 3 bindings')

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

;

end.
