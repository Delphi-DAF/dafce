unit SQLCute.Coverage.Feat;

{
  Feature: Phase-5 Full Coverage
  Covers: RightJoin, CrossJoin, FullOuterJoin, UnionAll, Intersect, Except,
          WhereNotExists, GroupByRaw, HavingRaw, WithRecursive, OrWhereNotIn
}

interface

implementation

uses
  Daf.MiniSpec,
  SQLCute.SpecHelpers;

initialization

Feature('''
Feature Phase-5 Full Coverage @sqlcute @coverage @phase5

  As a developer
  I want full coverage of all implemented query-builder features
  So I can trust the entire fluent API surface is tested
''')
.UseWorld<TSQLCuteWorld>

// -------------------------------------------------------------------------

.Rule('RightJoin generates a RIGHT JOIN clause')

  .Scenario('RightJoin with column pair')
    .Given('the Phase-5 right-join query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users RIGHT JOIN posts ON users.id = posts.user_id"')
    .&Then('has 0 bindings')

// -------------------------------------------------------------------------

.Rule('CrossJoin generates a CROSS JOIN clause without ON')

  .Scenario('CrossJoin produces no ON condition')
    .Given('the Phase-5 cross-join query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users CROSS JOIN tags"')
    .&Then('has 0 bindings')

// -------------------------------------------------------------------------

.Rule('FullOuterJoin generates a FULL OUTER JOIN clause')

  .Scenario('FullOuterJoin with column pair')
    .Given('the Phase-5 full-outer-join query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users FULL OUTER JOIN logs ON users.id = logs.user_id"')
    .&Then('has 0 bindings')

// -------------------------------------------------------------------------

.Rule('UnionAll generates a UNION ALL set operation')

  .Scenario('UnionAll appends UNION ALL subquery')
    .Given('the Phase-5 union-all query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT id FROM a UNION ALL SELECT id FROM b"')
    .&Then('has 0 bindings')

// -------------------------------------------------------------------------

.Rule('Intersect generates an INTERSECT set operation')

  .Scenario('Intersect appends INTERSECT subquery')
    .Given('the Phase-5 intersect query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT id FROM a INTERSECT SELECT id FROM b"')
    .&Then('has 0 bindings')

// -------------------------------------------------------------------------

.Rule('Except generates an EXCEPT set operation')

  .Scenario('Except appends EXCEPT subquery')
    .Given('the Phase-5 except query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT id FROM a EXCEPT SELECT id FROM b"')
    .&Then('has 0 bindings')

// -------------------------------------------------------------------------

.Rule('WhereNotExists generates a NOT EXISTS subquery condition')

  .Scenario('WhereNotExists wraps subquery with NOT EXISTS')
    .Given('the Phase-5 where-not-exists query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE NOT EXISTS (SELECT * FROM orders WHERE user_id = ?)"')
    .&Then('has 1 binding')

// -------------------------------------------------------------------------

.Rule('GroupByRaw emits a raw GROUP BY expression')

  .Scenario('GroupByRaw passes expression verbatim')
    .Given('the Phase-5 group-by-raw query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT event FROM events GROUP BY DATE(created_at)"')
    .&Then('has 0 bindings')

// -------------------------------------------------------------------------

.Rule('HavingRaw emits a raw HAVING expression')

  .Scenario('HavingRaw passes expression verbatim without binding')
    .Given('the Phase-5 having-raw query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT dept_id FROM orders GROUP BY dept_id HAVING SUM(total) > 1000"')
    .&Then('has 0 bindings')

// -------------------------------------------------------------------------

.Rule('WithRecursive prepends WITH RECURSIVE to the query')

  .Scenario('WithRecursive adds the RECURSIVE keyword')
    .Given('the Phase-5 with-recursive query')
    .When('I compile with ANSI')
    .&Then('SQL is "WITH RECURSIVE nums AS (SELECT n FROM base) SELECT n FROM nums"')
    .&Then('has 0 bindings')

// -------------------------------------------------------------------------

.Rule('OrWhereNotIn uses OR connector for NOT IN clause')

  .Scenario('OrWhereNotIn appends with OR')
    .Given('the Phase-5 or-where-not-in query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users WHERE active = ? OR status NOT IN (?, ?)"')
    .&Then('has 3 bindings')

;

end.
