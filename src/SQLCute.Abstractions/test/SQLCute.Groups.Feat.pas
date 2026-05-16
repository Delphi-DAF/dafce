unit SQLCute.Groups.Feat;

{
  TQuery: F2 features — nested WHERE groups (callbacks), column-to-column
  comparisons (WhereColumns) and conditional query building (When).
}

interface

implementation

uses
  Daf.MiniSpec,
  SQLCute.SpecHelpers;

initialization

Feature('''
TQuery — WHERE groups, WhereColumns, When @unit @sqlcute @f2

  F2 extends TQuery with three orthogonal features:
    - Where(callback)   — nested AND/OR groups wrapped in parentheses
    - WhereColumns      — column-to-column comparisons without bindings
    - When              — conditional query building at build time
''')
.UseWorld<TSQLCuteWorld>

// -------------------------------------------------------------------------

.Rule('Where(callback) wraps inner conditions in parentheses')

  .Scenario('Single AND group produces parenthesised block')
    .Given('a WHERE group query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE (age > ? AND active = ?)"')
    .&Then('has 2 bindings')

  .Scenario('Outer condition AND group keeps correct connectors')
    .Given('a WHERE group with outer AND condition')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM orders WHERE status = ? AND (total > ? OR priority = ?)"')
    .&Then('has 3 bindings')

  .Scenario('Group first then outer condition chains correctly')
    .Given('a WHERE group followed by outer condition')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE (city = ? OR city = ?) AND age > ?"')
    .&Then('has 3 bindings')

  .Scenario('OrWhere(callback) connects the group with OR')
    .Given('an OR WHERE group query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE vip = ? OR (age > ? AND active = ?)"')
    .&Then('has 3 bindings')

// -------------------------------------------------------------------------

.Rule('WhereColumns compares two columns without adding bindings')

  .Scenario('Equality column-to-column comparison')
    .Given('a WhereColumns equality query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM employees WHERE manager_id = employee_id"')
    .&Then('has 0 bindings')

  .Scenario('WhereColumns with comparison operator')
    .Given('a WhereColumns with operator query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM products WHERE price > min_price"')
    .&Then('has 0 bindings')

  .Scenario('WhereColumns combined with a value WHERE')
    .Given('a WhereColumns combined with WHERE query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM products WHERE active = ? AND price > min_price"')
    .&Then('has 1 binding')

// -------------------------------------------------------------------------

.Rule('When evaluates the condition at build time')

  .Scenario('When condition is true — branch is applied')
    .Given('a When true query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE active = ?"')
    .&Then('has 1 binding')

  .Scenario('When condition is false — branch is skipped')
    .Given('a When false query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users"')
    .&Then('has 0 bindings')

  .Scenario('When condition is false — false callback is applied')
    .Given('a When with false callback query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE archived = ?"')
    .&Then('has 1 binding')

;

end.
