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

  Starts, Ends and Contains all delegate to Like, so they emit the SAME SQL and
  differ only in where the % lands -- which travels in the binding, not in the
  statement. That is why every scenario here asserts the binding: a Then that
  only reads the SQL cannot tell a prefix match from a suffix one.
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
    .&Then('SQL is "SELECT * FROM t WHERE active = true OR LOWER(name) LIKE ?"')
    .&Then('has 1 binding')

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
    .&Then('bindings in order are "abc%"')

  .Scenario('Case-sensitive WhereStarts keeps original case and emits plain LIKE')
    .Given('a WhereStarts case-sensitive query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM t WHERE code LIKE ?"')
    .&Then('bindings in order are "ABC%"')

  .Scenario('WhereNotStarts negates the prefix match')
    .Given('a WhereNotStarts query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM t WHERE LOWER(code) NOT LIKE ?"')
    .&Then('bindings in order are "tmp%"')

  .Scenario('OrWhereNotStarts connects the negated prefix with OR')
    .Given('an OrWhereNotStarts query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM t WHERE active = true OR LOWER(code) NOT LIKE ?"')
    .&Then('bindings in order are "tmp%"')

// -------------------------------------------------------------------------

.Rule('WhereEnds prepends % and emits LIKE')

  .Scenario('WhereEnds produces %suffix binding')
    .Given('a WhereEnds query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM t WHERE LOWER(email) LIKE ?"')
    .&Then('bindings in order are "%@corp.com"')

  .Scenario('WhereNotEnds negates the suffix match')
    .Given('a WhereNotEnds query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM t WHERE LOWER(file) NOT LIKE ?"')
    .&Then('bindings in order are "%.bak"')

  .Scenario('OrWhereEnds connects the suffix match with OR')
    .Given('an OrWhereEnds query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM t WHERE active = true OR LOWER(file) LIKE ?"')
    .&Then('bindings in order are "%.pdf"')

  .Scenario('OrWhereNotEnds connects the negated suffix with OR')
    .Given('an OrWhereNotEnds query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM t WHERE active = true OR LOWER(file) NOT LIKE ?"')
    .&Then('bindings in order are "%.bak"')

// -------------------------------------------------------------------------

.Rule('WhereContains wraps with % on both sides')

  .Scenario('WhereContains produces %value% binding')
    .Given('a WhereContains query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM t WHERE LOWER(bio) LIKE ?"')
    .&Then('bindings in order are "%engineer%"')

  .Scenario('OrWhereContains connects the substring match with OR')
    .Given('an OrWhereContains query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM t WHERE active = true OR LOWER(bio) LIKE ?"')
    .&Then('bindings in order are "%engineer%"')

// -------------------------------------------------------------------------

.Rule('WhereNotContains emits NOT LIKE with %value%')

  .Scenario('WhereNotContains produces NOT LIKE %value%')
    .Given('a WhereNotContains query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM t WHERE LOWER(title) NOT LIKE ?"')
    .&Then('bindings in order are "%spam%"')

  .Scenario('OrWhereNotContains connects the negated substring with OR')
    .Given('an OrWhereNotContains query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM t WHERE active = true OR LOWER(title) NOT LIKE ?"')
    .&Then('bindings in order are "%spam%"')

// -------------------------------------------------------------------------

.Rule('OrWhereStarts connects with OR')

  .Scenario('OrWhereStarts connects prefix LIKE with OR')
    .Given('an OrWhereStarts query')
    .When('I compile with ANSI')
    .&Then('SQL is "SELECT * FROM t WHERE active = true OR LOWER(name) LIKE ?"')
    .&Then('bindings in order are "super%"')

;

end.
