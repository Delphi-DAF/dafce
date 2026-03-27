unit SQLCute.DML.Feat;

{
  Feature: Phase-3 DML — INSERT / UPDATE / DELETE
}

interface

implementation

uses
  Daf.MiniSpec,
  SQLCute.SpecHelpers;

initialization

Feature('''
Feature Phase-3 DML @sqlcute @dml @phase3

  As a developer
  I want SQLCute to generate INSERT, UPDATE and DELETE statements
  So I can use one query builder for all DML operations
''')
.UseWorld<TSQLCuteWorld>

// -------------------------------------------------------------------------

.Rule('INSERT adds a single row')

  .Scenario('Single-row INSERT')
    .Given('the Phase-3 insert-single query')
    .When('I compile with ANSI')
    .&Then('SQL is "INSERT INTO users (name, email) VALUES (?, ?)"')
    .&Then('has 2 bindings')

// -------------------------------------------------------------------------

.Rule('INSERT can add multiple rows in one statement')

  .Scenario('Multi-row INSERT')
    .Given('the Phase-3 insert-multi query')
    .When('I compile with ANSI')
    .&Then('SQL is "INSERT INTO logs (level, msg) VALUES (?, ?), (?, ?)"')
    .&Then('has 4 bindings')

// -------------------------------------------------------------------------

.Rule('INSERT ... SELECT inserts from a sub-query result')

  .Scenario('INSERT FROM SELECT')
    .Given('the Phase-3 insert-select query')
    .When('I compile with ANSI')
    .&Then('SQL is "INSERT INTO archive (id, name) SELECT id, name FROM users WHERE active = ?"')
    .&Then('has 1 binding')

// -------------------------------------------------------------------------

.Rule('UPDATE modifies existing rows')

  .Scenario('UPDATE a single column')
    .Given('the Phase-3 update-single query')
    .When('I compile with ANSI')
    .&Then('SQL is "UPDATE users SET status = ? WHERE id = ?"')
    .&Then('has 2 bindings')

  .Scenario('UPDATE multiple columns')
    .Given('the Phase-3 update-multi query')
    .When('I compile with ANSI')
    .&Then('SQL is "UPDATE users SET name = ?, email = ? WHERE id = ?"')
    .&Then('has 3 bindings')

// -------------------------------------------------------------------------

.Rule('DELETE removes rows matching a condition')

  .Scenario('DELETE with WHERE')
    .Given('a query deleting from "users" where "id" = 42')
    .When('I compile with ANSI')
    .&Then('SQL is "DELETE FROM users WHERE id = ?"')
    .&Then('has 1 binding')

  .Scenario('DELETE all rows (no WHERE)')
    .Given('a query deleting all from "users"')
    .When('I compile with ANSI')
    .&Then('SQL is "DELETE FROM users"')
    .&Then('has 0 bindings')

// -------------------------------------------------------------------------

.Rule('Acceptance criterion: full Phase-3 UPDATE flow')

  .Scenario('UPDATE with multiple SET columns and multiple WHERE conditions')
    .Given('the Phase-3 acceptance query')
    .When('I compile with ANSI')
    .&Then('SQL is "UPDATE orders SET status = ?, updated_at = ? WHERE user_id = ? AND status = ?"')
    .&Then('has 4 bindings')

;

end.
