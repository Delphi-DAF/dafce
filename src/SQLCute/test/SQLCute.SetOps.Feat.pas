unit SQLCute.SetOps.Feat;

{
  TQuery: operaciones de conjuntos (UNION, UNION ALL, INTERSECT, EXCEPT,
          INTERSECT ALL, EXCEPT ALL y el fragmento crudo de CombineRaw).
}

interface

implementation

uses
  Daf.MiniSpec,
  SQLCute.SpecHelpers;

initialization

Feature('''
TQuery — set operations @unit @sqlcute

  Dos consultas se combinan en una sola sentencia. Estas specs cubren UNION y
  UNION ALL, INTERSECT y EXCEPT con y sin ALL, y CombineRaw, que emite el
  fragmento tal cual para lo que el vocabulario todavía no nombra.
''')
.UseWorld<TSQLCuteWorld>

// -------------------------------------------------------------------------

.Rule('Union combines two result sets')

  .Scenario('Union produces UNION keyword')
    .Given('a UNION query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT id FROM active_users UNION SELECT id FROM archived_users"')

// -------------------------------------------------------------------------

// -------------------------------------------------------------------------

.Rule('UnionAll, Intersect and Except produce their respective set operations')

  .Scenario('UnionAll appends UNION ALL')
    .Given('a UnionAll query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT id FROM a UNION ALL SELECT id FROM b"')
    .&Then('has 0 bindings')

  .Scenario('Intersect appends INTERSECT')
    .Given('an Intersect query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT id FROM a INTERSECT SELECT id FROM b"')
    .&Then('has 0 bindings')

  .Scenario('Except appends EXCEPT')
    .Given('an Except query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT id FROM a EXCEPT SELECT id FROM b"')
    .&Then('has 0 bindings')

// -------------------------------------------------------------------------

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

;

end.
