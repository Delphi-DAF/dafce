unit SQLCute.Reentrancy.Feat;

{
  Una instancia de compilador no guarda estado entre compilaciones: sucesivas, anidadas y
  concurrentes dan lo mismo que con una instancia nueva cada vez.
}

interface

uses
  System.SysUtils,
  System.SyncObjs,
  Daf.SQLCute,
  Daf.SQLCute.Compiler,
  Daf.SQLCute.Compiler.Postgres;

type
  /// Un dialecto cuyo entrecomillado de tablas compila otra consulta con la misma instancia.
  TNestingCompiler = class(TPostgresCompiler)
  private
    // La consulta de dentro tambien entrecomilla su tabla: sin esto, anidaria sin fondo.
    FNesting: Boolean;
  protected
    function WrapTable(const Table: string): string; override;
  end;

  TReentrancyWorld = class
  private
    FFailures: TArray<string>;
    function RaceBody(const Idx: Integer; const Done: TCountdownEvent): TProc;
  public
    Compiler: IQueryCompiler;
    Other: IQueryCompiler;
    Queries: TArray<IQuery>;
    Expected: TArray<string>;
    Report: string;
    /// El SQL y los enlaces de un resultado, en una linea.
    class function Describe(const R: TSQLResult): string; static;
    /// Un juego de consultas con un numero distinto de enlaces cada una, y lo que da cada una
    /// compilada sola con un TPostgresCompiler nuevo en estilo nativo.
    procedure PrepareQuerySet;
    /// Cuatro hilos compilan muchas veces el juego sobre Compiler; deja en Report los resultados
    /// que no coinciden con los de cada consulta sola.
    procedure RaceOnSharedCompiler;
  end;

implementation

uses
  System.Variants,
  System.Classes,
  Daf.MiniSpec,
  Daf.SQLCute.Compiler.SqlServer,
  Daf.SQLCute.Compiler.Oracle;

const
  Threads = 4;
  Rounds = 500;

// En estilo nativo los marcadores se numeran: la numeracion es el estado que se veria si una
// compilacion dejara algo en la instancia.
function Native(const Compiler: TAnsiSqlCompiler): IQueryCompiler;
begin
  Compiler.Placeholders := TPlaceholderStyle.Native;
  Result := Compiler;
end;

{ TNestingCompiler }

function TNestingCompiler.WrapTable(const Table: string): string;
begin
  if not FNesting then
  begin
    FNesting := True;
    try
      (Self as IQueryCompiler).Compile(TQuery.New.From('inner').Where('z', 9));
    finally
      FNesting := False;
    end;
  end;
  Result := inherited WrapTable(Table);
end;

{ TReentrancyWorld }

class function TReentrancyWorld.Describe(const R: TSQLResult): string;
begin
  Result := R.SQL + ' |';
  for var B in R.Bindings do
    Result := Result + ' ' + VarToStr(B);
end;

procedure TReentrancyWorld.PrepareQuerySet;
begin
  Queries := [
    TQuery.New.From('a').Where('x', 1),
    TQuery.New.From('b').Where('x', 1).Where('y', 2).Where('z', 3),
    TQuery.New.From('c').WhereIn('id', [1, 2, 3, 4, 5]).Where('k', 6),
    TQuery.New.From('d')];
  SetLength(Expected, Length(Queries));
  var Alone := Native(TPostgresCompiler.Create);
  for var I := 0 to High(Queries) do
    Expected[I] := Describe(Alone.Compile(Queries[I]));
end;

function TReentrancyWorld.RaceBody(const Idx: Integer; const Done: TCountdownEvent): TProc;
begin
  // El indice entra como parametro: una variable de bucle capturada la comparten todos los hilos.
  Result :=
    procedure
    begin
      try
        for var Round := 1 to Rounds do
          for var Q := 0 to High(Queries) do
          begin
            var Got := Describe(Compiler.Compile(Queries[Q]));
            if (Got <> Expected[Q]) and (FFailures[Idx] = '') then
              FFailures[Idx] := Format('hilo %d: %s', [Idx, Got]);
          end;
      except
        on E: Exception do FFailures[Idx] := Format('hilo %d: %s', [Idx, E.ClassName]);
      end;
      Done.Signal;
    end;
end;

procedure TReentrancyWorld.RaceOnSharedCompiler;
begin
  SetLength(FFailures, Threads);
  var Done := TCountdownEvent.Create(Threads);
  try
    for var T := 0 to Threads - 1 do
      TThread.CreateAnonymousThread(RaceBody(T, Done)).Start;
    if Done.WaitFor(60000) <> wrSignaled then
      FFailures := FFailures + ['TIMEOUT'];
  finally
    Done.Free;
  end;
  Report := '';
  for var F in FFailures do
    if F <> '' then
      Report := Report + F + ' ';
  if Report = '' then
    Report := '<todos coinciden>';
end;

initialization

Feature('''
Feature A compiler instance keeps no state between compilations @unit @sqlcute

  As the author of an application that registers its compiler as a singleton
  I want one instance to serve successive, nested and concurrent compilations
  So that every compilation gives the same result as a fresh compiler would
''')

.UseWorld<TReentrancyWorld>

.Rule('A compiler instance serves successive compilations')

  .Scenario('The second Postgres query starts at $1')
    .Given('a TPostgresCompiler in native style held as IQueryCompiler that already compiled one query',
      procedure(W: TReentrancyWorld)
      begin
        W.Compiler := Native(TPostgresCompiler.Create);
        W.Compiler.Compile(TQuery.New.From('t').Where('a', 1));
      end)
    .When('it compiles From(''t'').Where(''b'', 2)',
      procedure(W: TReentrancyWorld)
      begin
        W.Report := TReentrancyWorld.Describe(W.Compiler.Compile(TQuery.New.From('t').Where('b', 2)));
      end)
    .&Then('the SQL starts its placeholders at $1, with one binding',
      procedure(W: TReentrancyWorld)
      begin
        Expect(W.Report).ToEqual('SELECT * FROM "t" WHERE "b" = $1 | 2');
      end)

  .Scenario('SQL Server and Oracle restart their numbering')
    .Given('a TSqlServerCompiler and a TOracleCompiler in native style held as IQueryCompiler, each with a query already compiled',
      procedure(W: TReentrancyWorld)
      begin
        W.Compiler := Native(TSqlServerCompiler.Create);
        W.Other := Native(TOracleCompiler.Create);
        W.Compiler.Compile(TQuery.New.From('t').Where('a', 1));
        W.Other.Compile(TQuery.New.From('t').Where('a', 1));
      end)
    .When('each one compiles From(''t'').Where(''b'', 2)',
      procedure(W: TReentrancyWorld)
      begin
        W.Report := W.Compiler.Compile(TQuery.New.From('t').Where('b', 2)).SQL + ' / '
          + W.Other.Compile(TQuery.New.From('t').Where('b', 2)).SQL;
      end)
    .&Then('SQL Server emits @p0 and Oracle :p1',
      procedure(W: TReentrancyWorld)
      begin
        Expect(W.Report).ToEqual('SELECT * FROM [t] WHERE [b] = @p0 / SELECT * FROM "T" WHERE "B" = :p1');
      end)

.Rule('A nested compilation does not alter the one that contains it')

  .Scenario('Compiling from an overridden method')
    .Given('a derived compiler in native style whose table quoting compiles another query with the same instance',
      procedure(W: TReentrancyWorld)
      begin
        W.Compiler := Native(TNestingCompiler.Create);
      end)
    .When('it compiles From(''t'').Where(''a'', 1).Where(''b'', 2)',
      procedure(W: TReentrancyWorld)
      begin
        var Query := TQuery.New.From('t').Where('a', 1).Where('b', 2);
        var Alone := Native(TPostgresCompiler.Create);
        var Plain := TReentrancyWorld.Describe(Alone.Compile(Query));
        try
          W.Report := TReentrancyWorld.Describe(W.Compiler.Compile(Query));
        except
          on E: Exception do W.Report := E.ClassName;
        end;
        W.Report := W.Report + ' # plain: ' + Plain;
      end)
    .&Then('the SQL and the two bindings are those of a compiler that compiles nothing else',
      procedure(W: TReentrancyWorld)
      begin
        Expect(W.Report).ToEqual('SELECT * FROM "t" WHERE "a" = $1 AND "b" = $2 | 1 2'
          + ' # plain: SELECT * FROM "t" WHERE "a" = $1 AND "b" = $2 | 1 2');
      end)

.Rule('A compiler instance serves concurrent compilations')

  .Scenario('Four threads share a compiler')
    .Given('a shared TPostgresCompiler in native style and a set of queries with a different number of bindings each, compiled alone before',
      procedure(W: TReentrancyWorld)
      begin
        W.Compiler := Native(TPostgresCompiler.Create);
        W.PrepareQuerySet;
      end)
    .When('four threads compile the set many times at once on that instance',
      procedure(W: TReentrancyWorld)
      begin
        W.RaceOnSharedCompiler;
      end)
    .&Then('every result matches that of its query compiled alone, and no thread got an exception',
      procedure(W: TReentrancyWorld)
      begin
        Expect(W.Report).ToEqual('<todos coinciden>');
      end);

end.
