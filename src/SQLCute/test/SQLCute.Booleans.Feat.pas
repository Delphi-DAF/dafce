unit SQLCute.Booleans.Feat;

{
  Un valor logico en una condicion se compara con el literal del dialecto, sin enlace.
}

interface

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.SQLCute,
  SQLCute.SpecHelpers;

initialization

Feature('''
TQuery — logical values in conditions @unit @sqlcute

  Where, OrWhere, WhereNot and OrWhereNot with a logical value compare the column with the literal
  of the dialect, with no binding, the way WhereTrue and WhereFalse do.
''')
.UseWorld<TSQLCuteWorld>

// -------------------------------------------------------------------------

.Rule('Where with a logical value emits the dialect literal')

  .Scenario('Where with True in ANSI')
    .When('query calls From(''users'').Where(''active'', True).Where(''age'', ''>'', 18)',
      procedure(W: TSQLCuteWorld)
      begin
        W.LastResult := W.Compiler.Compile(TQuery.New.From('users').Where('active', True).Where('age', '>', 18));
      end)
    .&Then('SQL is SELECT * FROM users WHERE active = true AND age > ? with 1 binding',
      procedure(W: TSQLCuteWorld)
      begin
        Expect(Format('%s | %d', [W.LastResult.SQL, Length(W.LastResult.Bindings)]))
          .ToEqual('SELECT * FROM users WHERE active = true AND age > ? | 1');
      end)

  .Scenario('Another operator negates the comparison')
    .When('query calls From(''users'').Where(''active'', ''<>'', False)',
      procedure(W: TSQLCuteWorld)
      begin
        W.LastResult := W.Compiler.Compile(TQuery.New.From('users').Where('active', '<>', False));
      end)
    .&Then('SQL is SELECT * FROM users WHERE NOT (active = false) with 0 bindings',
      procedure(W: TSQLCuteWorld)
      begin
        Expect(Format('%s | %d', [W.LastResult.SQL, Length(W.LastResult.Bindings)]))
          .ToEqual('SELECT * FROM users WHERE NOT (active = false) | 0');
      end)

;

end.
