unit SQLCute.Select.Feat;

{
  TQuery: SELECT, FROM, DISTINCT, ORDER BY, LIMIT/OFFSET,
          raw SELECT expressions, aggregate functions, Clone.
}

interface

implementation

uses
  Daf.MiniSpec,
  SQLCute.SpecHelpers;

initialization

Feature('''
TQuery — SELECT @unit @sqlcute

  TQuery builds SELECT statements via a fluent API.
  These specs cover: column list, DISTINCT, ORDER BY, LIMIT/OFFSET,
  raw expressions, aggregate functions and cloning.
''')
.UseWorld<TSQLCuteWorld>

// -------------------------------------------------------------------------

.Rule('From defaults to SELECT * when no columns are specified')

  .Scenario('No clauses compiles to SELECT *')
    .Given('an empty query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT *"')

  .Scenario('From adds a FROM clause after SELECT *')
    .Given('a query from "users"')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users"')

// -------------------------------------------------------------------------

.Rule('Select narrows the column list')

  .Scenario('Single column')
    .Given('a query selecting "id" from "users"')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT id FROM users"')

  .Scenario('Multiple columns')
    .Given('a query selecting "id, name, email" from "accounts"')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT id, name, email FROM accounts"')

// -------------------------------------------------------------------------

.Rule('Distinct adds SELECT DISTINCT')

  .Scenario('Distinct emits SELECT DISTINCT')
    .Given('a distinct query from "tags"')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT DISTINCT * FROM tags"')

// -------------------------------------------------------------------------

.Rule('Limit and Offset control pagination')

  .Scenario('Limit restricts the number of rows')
    .Given('a query from "orders" with limit 20')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM orders LIMIT 20"')

  .Scenario('Offset skips rows when combined with Limit')
    .Given('a query from "orders" with limit 10 offset 30')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM orders LIMIT 10 OFFSET 30"')

// -------------------------------------------------------------------------

.Rule('OrderBy and OrderByDesc sort the result set')

  .Scenario('OrderBy emits ASC')
    .Given('a query from "users" ordered by "name"')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users ORDER BY name ASC"')

  .Scenario('OrderByDesc emits DESC')
    .Given('a query from "users" ordered by "created_at" desc')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users ORDER BY created_at DESC"')

// -------------------------------------------------------------------------

.Rule('OrderByRaw passes an expression verbatim')

  .Scenario('OrderByRaw emits the expression unchanged')
    .Given('an OrderByRaw query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users ORDER BY FIELD(status, ''active'', ''pending'', ''closed'')"')
    .&Then('has 0 bindings')

// -------------------------------------------------------------------------

.Rule('SelectRaw injects a raw expression into the SELECT list')

  .Scenario('SelectRaw adds a verbatim column expression')
    .Given('a SelectRaw query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT id, UPPER(name) AS uname FROM users"')
    .&Then('has 0 bindings')

// -------------------------------------------------------------------------

.Rule('SelectCount adds a COUNT aggregate')

  .Scenario('SelectCount emits COUNT(*) AS count')
    .Given('a count query from "orders"')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT COUNT(*) AS count FROM orders"')

// -------------------------------------------------------------------------

.Rule('Clone produces an independent copy of the query')

  .Scenario('Clone generates identical SQL')
    .Given('a query selecting "id" from "users"')
    .When('I clone the query and compile the clone')
    .&Then('SQL is "SELECT id FROM users"')

// -------------------------------------------------------------------------

.Rule('Select, Where and Limit can be combined in one query')

  .Scenario('Select with WHERE and LIMIT')
    .Given('a combined SELECT WHERE LIMIT query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT id, name FROM users WHERE active = ? LIMIT 10"')
    .&Then('has 1 binding')

// -------------------------------------------------------------------------

.Rule('SelectAs emits a column alias')

  .Scenario('SelectAs with alias')
    .Given('a SelectAs query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT name AS full_name FROM users"')
    .&Then('has 0 bindings')

// -------------------------------------------------------------------------

.Rule('Aggregate functions emit SUM / AVG / MIN / MAX with default alias')

  .Scenario('SelectSum emits SUM aggregate')
    .Given('a SelectSum query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT SUM(total) AS sum FROM orders"')
    .&Then('has 0 bindings')

  .Scenario('SelectAvg emits AVG aggregate')
    .Given('a SelectAvg query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT AVG(score) AS avg FROM results"')
    .&Then('has 0 bindings')

  .Scenario('SelectMin emits MIN aggregate')
    .Given('a SelectMin query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT MIN(price) AS min FROM products"')
    .&Then('has 0 bindings')

  .Scenario('SelectMax emits MAX aggregate')
    .Given('a SelectMax query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT MAX(price) AS max FROM products"')
    .&Then('has 0 bindings')

// -------------------------------------------------------------------------

.Rule('From with alias qualifies the table reference')

  .Scenario('From with alias emits AS alias')
    .Given('a From with alias query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT u.id FROM users AS u"')
    .&Then('has 0 bindings')

// -------------------------------------------------------------------------

.Rule('SELECT subquery, FromRaw and WithRaw support raw expressions @f5')

  .Scenario('Select subquery emits derived column @f5')
    .Given('a SELECT subquery column query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT (SELECT MAX(price) AS max FROM products) AS max_price FROM orders"')
    .&Then('has 0 bindings')

  .Scenario('FromRaw with alias emits raw FROM expression @f5')
    .Given('a FromRaw with alias query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM generate_series(1,10) AS t"')
    .&Then('has 0 bindings')

  .Scenario('FromRaw with bindings accumulates parameters @f5')
    .Given('a FromRaw with bindings query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM generate_series(?,?) AS t"')
    .&Then('has 2 bindings')

  .Scenario('FromRaw bindings precede WHERE bindings @f5')
    .Given('a FromRaw binding order query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM fn(?) AS t WHERE n > ?"')
    .&Then('has 2 bindings')

  .Scenario('WithRaw emits a raw CTE @f5')
    .Given('a WithRaw CTE query')
    .When('I compile with ANSI')
    .&Then('SQL is "WITH cte AS (SELECT 1 AS n) SELECT * FROM cte"')
    .&Then('has 0 bindings')

  .Scenario('WithRaw with bindings accumulates CTE parameters @f5')
    .Given('a WithRaw CTE with bindings query')
    .When('I compile with ANSI')
    .&Then('SQL is "WITH cte AS (SELECT ? AS n) SELECT * FROM cte"')
    .&Then('has 1 binding')

;

end.
