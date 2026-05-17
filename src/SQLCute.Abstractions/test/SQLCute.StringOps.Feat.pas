unit SQLCute.StringOps.Feat;

{
  TQuery: F3 features — string/LIKE operations.
  WhereLike, WhereNotLike, OrWhereLike, OrWhereNotLike, and the
  WhereStarts / WhereEnds / WhereContains families (+ Not / Or / OrNot variants).
}

interface

implementation

uses
  Daf.MiniSpec,
  SQLCute.SpecHelpers;

initialization

Feature('''
TQuery — String / LIKE operations @unit @sqlcute @f3

  F3 extends TQuery with fluent LIKE-based string matching methods.
  By default all methods are case-insensitive: the compiler wraps the column
  in LOWER() and lowercases the binding value. Pass CaseSensitive=True to
  bypass this and emit a plain LIKE.
''')
.UseWorld<TSQLCuteWorld>

// -------------------------------------------------------------------------

.Rule('WhereLike emits a LIKE condition')

  .Scenario('Case-insensitive LIKE (default) wraps column in LOWER()')
    .Given('a WhereLike case-insensitive query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM t WHERE LOWER(name) LIKE ?"')
    .&Then('has 1 binding')

  .Scenario('Case-sensitive LIKE emits plain col LIKE ?')
    .Given('a WhereLike case-sensitive query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM t WHERE name LIKE ?"')
    .&Then('has 1 binding')

// -------------------------------------------------------------------------

.Rule('WhereNotLike emits NOT LIKE')

  .Scenario('Case-insensitive NOT LIKE wraps column in LOWER()')
    .Given('a WhereNotLike query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM t WHERE LOWER(name) NOT LIKE ?"')
    .&Then('has 1 binding')

// -------------------------------------------------------------------------

.Rule('OrWhereLike connects with OR')

  .Scenario('OrWhereLike joins with OR')
    .Given('an OrWhereLike query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM t WHERE active = ? OR LOWER(name) LIKE ?"')
    .&Then('has 2 bindings')

// -------------------------------------------------------------------------

.Rule('OrWhereNotLike connects NOT LIKE with OR')

  .Scenario('OrWhereNotLike joins NOT LIKE with OR')
    .Given('an OrWhereNotLike query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM t WHERE role = ? OR LOWER(email) NOT LIKE ?"')
    .&Then('has 2 bindings')

// -------------------------------------------------------------------------

.Rule('WhereStarts appends % and emits LIKE')

  .Scenario('Case-insensitive WhereStarts lowercases binding and wraps column')
    .Given('a WhereStarts case-insensitive query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM t WHERE LOWER(code) LIKE ?"')
    .&Then('has 1 binding')

  .Scenario('Case-sensitive WhereStarts keeps original case and emits plain LIKE')
    .Given('a WhereStarts case-sensitive query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM t WHERE code LIKE ?"')
    .&Then('has 1 binding')

// -------------------------------------------------------------------------

.Rule('WhereEnds prepends % and emits LIKE')

  .Scenario('WhereEnds produces %suffix binding')
    .Given('a WhereEnds query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM t WHERE LOWER(email) LIKE ?"')
    .&Then('has 1 binding')

// -------------------------------------------------------------------------

.Rule('WhereContains wraps with % on both sides')

  .Scenario('WhereContains produces %value% binding')
    .Given('a WhereContains query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM t WHERE LOWER(bio) LIKE ?"')
    .&Then('has 1 binding')

// -------------------------------------------------------------------------

.Rule('WhereNotContains emits NOT LIKE with %value%')

  .Scenario('WhereNotContains produces NOT LIKE %value%')
    .Given('a WhereNotContains query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM t WHERE LOWER(title) NOT LIKE ?"')
    .&Then('has 1 binding')

// -------------------------------------------------------------------------

.Rule('OrWhereStarts connects with OR')

  .Scenario('OrWhereStarts connects prefix LIKE with OR')
    .Given('an OrWhereStarts query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM t WHERE active = ? OR LOWER(name) LIKE ?"')
    .&Then('has 2 bindings')

;

end.
