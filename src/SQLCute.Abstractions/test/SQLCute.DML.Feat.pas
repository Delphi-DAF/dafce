unit SQLCute.DML.Feat;

{
  TQuery: DML — INSERT (single row, multi-row, INSERT FROM SELECT),
          UPDATE, DELETE.
}

interface

implementation

uses
  Daf.MiniSpec,
  SQLCute.SpecHelpers;

initialization

Feature('''
Feature TQuery — DML @unit @sqlcute

  TQuery builds parameterised INSERT, UPDATE and DELETE statements.
''')
.UseWorld<TSQLCuteWorld>

// -------------------------------------------------------------------------

.Rule('AsInsert adds a single row')

  .Scenario('Single-row INSERT')
    .Given('a single-row INSERT query')
    .When('I compile with ANSI')
    .&Then('SQL is "INSERT INTO users (name, email) VALUES (?, ?)"')
    .&Then('has 2 bindings')

// -------------------------------------------------------------------------

.Rule('AsInsertRows adds multiple rows in one statement')

  .Scenario('Multi-row INSERT')
    .Given('a multi-row INSERT query')
    .When('I compile with ANSI')
    .&Then('SQL is "INSERT INTO logs (level, msg) VALUES (?, ?), (?, ?)"')
    .&Then('has 4 bindings')

// -------------------------------------------------------------------------

.Rule('AsInsertFrom inserts from a subquery result')

  .Scenario('INSERT FROM SELECT')
    .Given('an INSERT FROM SELECT query')
    .When('I compile with ANSI')
    .&Then('SQL is "INSERT INTO archive (id, name) SELECT id, name FROM users WHERE active = ?"')
    .&Then('has 1 binding')

// -------------------------------------------------------------------------

.Rule('AsUpdate modifies existing rows')

  .Scenario('UPDATE a single column')
    .Given('an UPDATE single-column query')
    .When('I compile with ANSI')
    .&Then('SQL is "UPDATE users SET status = ? WHERE id = ?"')
    .&Then('has 2 bindings')

  .Scenario('UPDATE multiple columns')
    .Given('an UPDATE multi-column query')
    .When('I compile with ANSI')
    .&Then('SQL is "UPDATE users SET name = ?, email = ? WHERE id = ?"')
    .&Then('has 3 bindings')

// -------------------------------------------------------------------------

.Rule('AsDelete removes rows matching a condition')

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

.Rule('AsInsert, AsUpdate and AsDelete can be combined with multiple conditions')

  .Scenario('UPDATE with multiple SET columns and multiple WHERE conditions')
    .Given('an UPDATE with multiple columns and conditions')
    .When('I compile with ANSI')
    .&Then('SQL is "UPDATE orders SET status = ?, updated_at = ? WHERE user_id = ? AND status = ?"')
    .&Then('has 4 bindings')

;

end.
