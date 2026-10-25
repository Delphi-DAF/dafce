unit SQLCute.Join.Feat;

{
  TQuery: JOIN (all types), GROUP BY, HAVING,
          CTE (WITH / WITH RECURSIVE), subqueries.
}

interface

implementation

uses
  System.SysUtils,
  System.Variants,
  Daf.MiniSpec,
  Daf.SQLCute,
  Daf.SQLCute.Compiler.Postgres,
  SQLCute.SpecHelpers;

initialization

Feature('''
TQuery — JOIN / GROUP BY / CTE @unit @sqlcute

  TQuery builds complex SELECT statements via a fluent API.
  These specs cover: all JOIN types, GROUP BY, HAVING,
  WITH / WITH RECURSIVE and FROM subqueries.
''')
.UseWorld<TSQLCuteWorld>

// -------------------------------------------------------------------------

.Rule('Join (INNER JOIN) connects two tables')

  .Scenario('INNER JOIN with raw ON condition')
    .Given('a query from "users" inner joined to "orders" on "users.id = orders.user_id"')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users INNER JOIN orders ON users.id = orders.user_id"')

  .Scenario('INNER JOIN using column pair')
    .Given('a query from "users" joining "orders" on columns "users.id" and "orders.user_id"')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users INNER JOIN orders ON users.id = orders.user_id"')

// -------------------------------------------------------------------------

.Rule('LeftJoin preserves all rows in the left table')

  .Scenario('LEFT JOIN with raw ON condition')
    .Given('a query from "users" left joined to "posts" on "users.id = posts.user_id"')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users LEFT JOIN posts ON users.id = posts.user_id"')

// -------------------------------------------------------------------------

.Rule('RightJoin preserves all rows in the right table')

  .Scenario('RightJoin with column pair')
    .Given('a RightJoin query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users RIGHT JOIN posts ON users.id = posts.user_id"')
    .&Then('has 0 bindings')

// -------------------------------------------------------------------------

.Rule('CrossJoin produces a cartesian product')

  .Scenario('CrossJoin without ON condition')
    .Given('a CrossJoin query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users CROSS JOIN tags"')
    .&Then('has 0 bindings')

// -------------------------------------------------------------------------

.Rule('FullOuterJoin combines both outer results')

  .Scenario('FullOuterJoin with column pair')
    .Given('a FullOuterJoin query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users FULL OUTER JOIN logs ON users.id = logs.user_id"')
    .&Then('has 0 bindings')

// -------------------------------------------------------------------------

.Rule('Multiple JOINs are chained in declaration order')

  .Scenario('Two JOINs in sequence')
    .Given('a query with two chained JOINs')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM orders INNER JOIN users ON orders.user_id = users.id LEFT JOIN products ON orders.product_id = products.id"')

// -------------------------------------------------------------------------


.Rule('JOIN with custom operator emits col1 op col2')

  .Scenario('Join with <> operator')
    .Given('a Join with not-equal operator query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM orders INNER JOIN users ON orders.user_id <> users.id"')
    .&Then('has 0 bindings')

  .Scenario('LeftJoin with >= operator')
    .Given('a LeftJoin with >= operator query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM orders LEFT JOIN tiers ON orders.amount >= tiers.min_amount"')
    .&Then('has 0 bindings')
.Rule('GroupBy aggregates result rows')

  .Scenario('GroupBy a single column')
    .Given('a query from "orders" grouped by "status"')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM orders GROUP BY status"')

  .Scenario('GroupBy multiple columns')
    .Given('a multi-column GroupBy query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM sales GROUP BY year, month, dept_id"')

// -------------------------------------------------------------------------

.Rule('GroupByRaw passes an expression verbatim into GROUP BY')

  .Scenario('GroupByRaw emits the expression unchanged')
    .Given('a GroupByRaw query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT event FROM events GROUP BY DATE(created_at)"')
    .&Then('has 0 bindings')

// -------------------------------------------------------------------------

.Rule('Having filters aggregated groups')

  .Scenario('Having with aggregate expression')
    .Given('a GROUP BY and HAVING query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM orders GROUP BY user_id HAVING COUNT(*) > 3"')
    .&Then('has 0 bindings')

// -------------------------------------------------------------------------

.Rule('HavingRaw passes an expression verbatim into HAVING')

  .Scenario('HavingRaw emits the expression unchanged')
    .Given('a HavingRaw query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT dept_id FROM orders GROUP BY dept_id HAVING SUM(total) > 1000"')
    .&Then('has 0 bindings')

.Rule('With names a common table expression')

  .Scenario('CTE used as FROM source')
    .Given('a CTE query')
    .When('I compile with ANSI')
    .&Then('SQL is "WITH recent AS (SELECT * FROM orders WHERE created > ?) SELECT * FROM recent"')
    .&Then('has 1 binding')

// -------------------------------------------------------------------------

.Rule('WithRecursive adds the RECURSIVE keyword')

  .Scenario('WithRecursive prepends WITH RECURSIVE')
    .Given('a WithRecursive query')
    .When('I compile with ANSI')
    .&Then('SQL is "WITH RECURSIVE nums AS (SELECT n FROM base) SELECT n FROM nums"')
    .&Then('has 0 bindings')

// -------------------------------------------------------------------------

.Rule('From (subquery) creates a derived table')

  .Scenario('Subquery in FROM with alias')
    .Given('a FROM subquery query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM (SELECT id, name FROM users) u"')

// -------------------------------------------------------------------------

.Rule('Join, GroupBy, Having and OrderBy can be combined in one query')

  .Scenario('JOIN + GROUP BY + HAVING + ORDER BY + LIMIT')
    .Given('a combined JOIN GROUP HAVING ORDER LIMIT query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT user_id, COUNT(*) AS total FROM orders INNER JOIN users ON orders.user_id = users.id WHERE status = ? GROUP BY user_id HAVING COUNT(*) > 5 ORDER BY total DESC LIMIT 10"')
    .&Then('has 1 binding')

// -------------------------------------------------------------------------

.Rule('Join callback: ON with multiple AND conditions')

  .Scenario('InnerJoinTwoConditions: JOIN with two column conditions @f4')
    .Given('a JOIN callback query with two AND conditions')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM orders INNER JOIN users ON (orders.user_id = users.id AND orders.tenant = users.tenant)"')
    .&Then('has 0 bindings')

// -------------------------------------------------------------------------

.Rule('Join callback: ON with OR conditions')

  .Scenario('LeftJoinOrCondition: LEFT JOIN with two OR conditions @f4')
    .Given('a LEFT JOIN callback query with OR conditions')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM orders LEFT JOIN promos ON (orders.promo_id = promos.id OR orders.alt_promo = promos.id)"')
    .&Then('has 0 bindings')

// -------------------------------------------------------------------------

.Rule('Join callback: ON with nested group')

  .Scenario('JoinWithNestedGroup: JOIN ON with nested OR group @f4')
    .Given('a JOIN callback query with a nested group condition')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM t INNER JOIN u ON (t.id = u.id AND (t.active = true OR u.role = ?))"')
    .&Then('has 1 binding')

// -------------------------------------------------------------------------

.Rule('Join subquery: JOIN against an inline subquery')

  .Scenario('SubqueryJoin: LEFT JOIN against a derived table @f4')
    .Given('a LEFT JOIN subquery callback query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM orders LEFT JOIN (SELECT id, name FROM users) AS u ON (orders.user_id = u.id)"')
    .&Then('has 0 bindings')

// -------------------------------------------------------------------------

.Rule('Join callback: cloned query is independent')

  .Scenario('CloneJoinCallback: clone of a callback JOIN is unchanged @f4')
    .Given('a JOIN callback query with two AND conditions')
    .When('I clone the query and compile the clone')
    .&Then('SQL is "SELECT * FROM orders INNER JOIN users ON (orders.user_id = users.id AND orders.tenant = users.tenant)"')

// -------------------------------------------------------------------------

.Rule('Having with Null emits IS NULL or IS NOT NULL')

  .Scenario('Having with Null')
    .When('query calls From(''tickets'').SelectMax(''closed_at'', '
      + '''last_closed'').GroupBy(''team'').Having(''last_closed'', ''='', Null)',
      procedure(W: TSQLCuteWorld)
      begin
        W.LastResult := W.Compiler.Compile(
          TQuery.New.From('tickets').SelectMax('closed_at', 'last_closed').GroupBy('team').Having('last_closed', '=', Null));
      end)
    .&Then('SQL is SELECT MAX(closed_at) AS last_closed FROM tickets GROUP BY team HAVING last_closed IS NULL with 0 bindings',
      procedure(W: TSQLCuteWorld)
      begin
        Expect(Format('%s | %d', [W.LastResult.SQL, Length(W.LastResult.Bindings)]))
          .ToEqual('SELECT MAX(closed_at) AS last_closed FROM tickets GROUP BY team HAVING last_closed IS NULL | 0');
      end)

// -------------------------------------------------------------------------

.Rule('The column of Having is an identifier')

  .Scenario('Postgres quotes the column of Having')
    .When('query calls From(''orders'').SelectCount(''*'', ''n'').GroupBy(''customer_id'').Having(''n'', ''>'', 5) on Postgres',
      procedure(W: TSQLCuteWorld)
      begin
        W.Compiler := TPostgresCompiler.Create;
        W.LastResult := W.Compiler.Compile(
          TQuery.New.From('orders').SelectCount('*', 'n').GroupBy('customer_id').Having('n', '>', 5));
      end)
    .&Then('SQL is SELECT COUNT(*) AS n FROM "orders" GROUP BY "customer_id" HAVING "n" > ?',
      procedure(W: TSQLCuteWorld)
      begin
        Expect(W.LastResult.SQL).ToEqual('SELECT COUNT(*) AS n FROM "orders" GROUP BY "customer_id" HAVING "n" > ?');
      end)

  .Scenario('An aggregate goes raw')
    .When('query calls From(''orders'').GroupBy(''customer_id'').HavingRaw(''count(*) > 5'') on Postgres',
      procedure(W: TSQLCuteWorld)
      begin
        W.Compiler := TPostgresCompiler.Create;
        W.LastResult := W.Compiler.Compile(TQuery.New.From('orders').GroupBy('customer_id').HavingRaw('count(*) > 5'));
      end)
    .&Then('SQL is SELECT * FROM "orders" GROUP BY "customer_id" HAVING count(*) > 5 with 0 bindings',
      procedure(W: TSQLCuteWorld)
      begin
        Expect(Format('%s | %d', [W.LastResult.SQL, Length(W.LastResult.Bindings)]))
          .ToEqual('SELECT * FROM "orders" GROUP BY "customer_id" HAVING count(*) > 5 | 0');
      end)

// -------------------------------------------------------------------------

.Rule('OrHaving adds a condition with OR')

  .Scenario('OrHaving in SQL')
    .When('query calls Having(''n'', ''>'', 5).OrHaving(''revenue'', ''>='', 1000) over orders grouped by customer_id',
      procedure(W: TSQLCuteWorld)
      begin
        W.LastResult := W.Compiler.Compile(
          TQuery.New.From('orders').SelectCount('*', 'n').SelectSum('total', 'revenue').GroupBy('customer_id')
            .Having('n', '>', 5).OrHaving('revenue', '>=', 1000));
      end)
    .&Then('SQL is ... HAVING n > ? OR revenue >= ? with 2 bindings',
      procedure(W: TSQLCuteWorld)
      begin
        Expect(Format('%s | %d', [W.LastResult.SQL, Length(W.LastResult.Bindings)]))
          .ToEqual('SELECT COUNT(*) AS n, SUM(total) AS revenue FROM orders GROUP BY customer_id HAVING n > ? OR revenue >= ? | 2');
      end)

;

end.
