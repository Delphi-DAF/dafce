unit SQLCute.Phase1.Feat;

{
  TQuery: Fase 1 — Set operations (IntersectAll, ExceptAll, CombineRaw),
          paginación (ForPage, Take, Skip) y JOIN con operador personalizado.
}

interface

implementation

uses
  Daf.MiniSpec,
  SQLCute.SpecHelpers;

initialization

Feature('''
TQuery — Fase 1: Set ops + Paginación + JOIN op @unit @sqlcute
''')
.UseWorld<TSQLCuteWorld>

// -------------------------------------------------------------------------

.Rule('IntersectAll and ExceptAll emit INTERSECT ALL / EXCEPT ALL')

  .Scenario('IntersectAll produces INTERSECT ALL')
    .Given('an IntersectAll query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT id FROM a INTERSECT ALL SELECT id FROM b"')
    .&Then('has 0 bindings')

  .Scenario('ExceptAll produces EXCEPT ALL')
    .Given('an ExceptAll query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT id FROM a EXCEPT ALL SELECT id FROM b"')
    .&Then('has 0 bindings')

  .Scenario('CombineRaw emits the raw fragment verbatim')
    .Given('a CombineRaw query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT id FROM a UNION SELECT id FROM b WHERE status = ''active''"')
    .&Then('has 0 bindings')

// -------------------------------------------------------------------------

.Rule('ForPage, Take and Skip control paging')

  .Scenario('ForPage(2, 15) sets LIMIT 15 OFFSET 15')
    .Given('a ForPage query page 2 per-page 15')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users LIMIT 15 OFFSET 15"')
    .&Then('has 0 bindings')

  .Scenario('ForPage(1, 10) sets LIMIT 10 OFFSET 0')
    .Given('a ForPage query page 1 per-page 10')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users LIMIT 10 OFFSET 0"')
    .&Then('has 0 bindings')

  .Scenario('Take is an alias for Limit')
    .Given('a Take query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users LIMIT 5"')
    .&Then('has 0 bindings')

  .Scenario('Skip is an alias for Offset')
    .Given('a Skip query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM users LIMIT 10 OFFSET 20"')
    .&Then('has 0 bindings')

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

;

end.
