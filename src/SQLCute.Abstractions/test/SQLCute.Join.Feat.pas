unit SQLCute.Join.Feat;

{
  Feature: Phase-2 SELECT  — JOINs, DISTINCT, GROUP BY/HAVING,
           aggregates, UNION, CTEs, subqueries, WHERE EXISTS.
}

interface

implementation

uses
  Daf.MiniSpec,
  SQLCute.SpecHelpers;

initialization

Feature('''
Feature Phase-2 SELECT @sqlcute @join @phase2

  As a developer
  I want the full SQLKata-style SELECT capabilities in SQLCute
  So I can express joins, aggregations, set operations and subqueries
''')
.UseWorld<TSQLCuteWorld>

// -------------------------------------------------------------------------

.Rule('INNER JOIN connects two tables')

  .Scenario('INNER JOIN with raw ON condition')
    .Given('a query from "users" inner joined to "orders" on "users.id = orders.user_id"')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users INNER JOIN orders ON users.id = orders.user_id"')

  .Scenario('INNER JOIN using column pair')
    .Given('a query from "users" joining "orders" on columns "users.id" and "orders.user_id"')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users INNER JOIN orders ON users.id = orders.user_id"')

// -------------------------------------------------------------------------

.Rule('LEFT JOIN preserves all rows in the left table')

  .Scenario('LEFT JOIN with raw ON condition')
    .Given('a query from "users" left joined to "posts" on "users.id = posts.user_id"')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users LEFT JOIN posts ON users.id = posts.user_id"')

// -------------------------------------------------------------------------

.Rule('Multiple JOINs are chained in order')

  .Scenario('Two JOINs in sequence')
    .Given('the Phase-2 two-join query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM orders INNER JOIN users ON orders.user_id = users.id LEFT JOIN products ON orders.product_id = products.id"')

// -------------------------------------------------------------------------

.Rule('DISTINCT eliminates duplicate rows')

  .Scenario('DISTINCT query produces SELECT DISTINCT')
    .Given('a distinct query from "tags"')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT DISTINCT * FROM tags"')

// -------------------------------------------------------------------------

.Rule('GROUP BY aggregates result rows')

  .Scenario('GROUP BY a single column')
    .Given('a query from "orders" grouped by "status"')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM orders GROUP BY status"')

// -------------------------------------------------------------------------

.Rule('HAVING filters aggregated groups')

  .Scenario('GROUP BY with HAVING using aggregate expression')
    .Given('the Phase-2 group-having query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM orders GROUP BY user_id HAVING COUNT(*) > ?"')
    .&Then('has 1 binding')

// -------------------------------------------------------------------------

.Rule('COUNT aggregate summarises rows')

  .Scenario('SelectCount adds COUNT(*) AS count to SELECT list')
    .Given('a count query from "orders"')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT COUNT(*) AS count FROM orders"')

// -------------------------------------------------------------------------

.Rule('UNION combines two result sets')

  .Scenario('UNION of two queries produces UNION keyword')
    .Given('the Phase-2 union query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT id FROM active_users UNION SELECT id FROM archived_users"')

// -------------------------------------------------------------------------

.Rule('WITH names a common table expression')

  .Scenario('CTE used as FROM source')
    .Given('the Phase-2 CTE query')
    .When('I compile with ANSI')
    .&Then('SQL is "WITH recent AS (SELECT * FROM orders WHERE created > ?) SELECT * FROM recent"')
    .&Then('has 1 binding')

// -------------------------------------------------------------------------

.Rule('FROM (subquery) creates a derived table')

  .Scenario('Subquery in FROM with alias')
    .Given('the Phase-2 subquery-from query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM (SELECT id, name FROM users) u"')

// -------------------------------------------------------------------------

.Rule('WHERE EXISTS tests sub-query row existence')

  .Scenario('WHERE EXISTS with correlated subquery')
    .Given('the Phase-2 where-exists query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE EXISTS (SELECT * FROM orders WHERE user_id = ?)"')
    .&Then('has 1 binding')

// -------------------------------------------------------------------------

.Rule('Acceptance criterion: full Phase-2 combined query')

  .Scenario('JOIN + GROUP BY + HAVING + ORDER BY + LIMIT')
    .Given('the Phase-2 acceptance query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT user_id, COUNT(*) AS total FROM orders INNER JOIN users ON orders.user_id = users.id WHERE status = ? GROUP BY user_id HAVING COUNT(*) > ? ORDER BY total DESC LIMIT 10"')
    .&Then('has 2 bindings')

end.
