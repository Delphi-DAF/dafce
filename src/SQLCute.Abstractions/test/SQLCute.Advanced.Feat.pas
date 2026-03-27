unit SQLCute.Advanced.Feat;

{
  Feature: Phase-4 Advanced WHERE + raw expressions
  Covers: WhereIn, WhereNotIn, OrWhereIn, SelectRaw, OrderByRaw
}

interface

implementation

uses
  Daf.MiniSpec,
  SQLCute.SpecHelpers;

initialization

Feature('''
Feature Phase-4 Advanced WHERE and raw expressions @sqlcute @advanced @phase4

  As a developer
  I want WHERE IN / NOT IN conditions and raw expression support
  So I can build complex parameterised queries without leaving the fluent API
''')
.UseWorld<TSQLCuteWorld>

// -------------------------------------------------------------------------

.Rule('WHERE IN generates an IN clause with positional bindings')

  .Scenario('WhereIn with a list of integers')
    .Given('the Phase-4 where-in query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE id IN (?, ?, ?)"')
    .&Then('has 3 bindings')

  .Scenario('WhereIn chained with another WHERE')
    .Given('the Phase-4 where-in-and-where query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE role IN (?, ?) AND active = ?"')
    .&Then('has 3 bindings')

// -------------------------------------------------------------------------

.Rule('WHERE NOT IN generates a NOT IN clause')

  .Scenario('WhereNotIn with a list of strings')
    .Given('the Phase-4 where-not-in query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM products WHERE status NOT IN (?, ?)"')
    .&Then('has 2 bindings')

// -------------------------------------------------------------------------

.Rule('OR WHERE IN uses OR connector')

  .Scenario('OrWhereIn appends with OR')
    .Given('the Phase-4 or-where-in query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE active = ? OR id IN (?, ?)"')
    .&Then('has 3 bindings')

// -------------------------------------------------------------------------

.Rule('SelectRaw injects a raw expression into the SELECT list')

  .Scenario('SelectRaw adds a verbatim expression')
    .Given('the Phase-4 select-raw query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT id, UPPER(name) AS uname FROM users"')
    .&Then('has 0 bindings')

// -------------------------------------------------------------------------

.Rule('OrderByRaw injects a raw ORDER BY expression')

  .Scenario('OrderByRaw adds a verbatim ORDER BY clause')
    .Given('the Phase-4 order-by-raw query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users ORDER BY FIELD(status, ''active'', ''pending'', ''closed'')"')
    .&Then('has 0 bindings')

// -------------------------------------------------------------------------

.Rule('Acceptance criterion: combined Phase-4 query')

  .Scenario('WhereIn + WhereNotIn + WHERE in one query')
    .Given('the Phase-4 acceptance query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT id, name FROM orders WHERE status IN (?, ?) AND user_id NOT IN (?, ?) AND created_at > ?"')
    .&Then('has 5 bindings')

;

end.
