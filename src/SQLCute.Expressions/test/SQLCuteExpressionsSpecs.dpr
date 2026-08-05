program SQLCuteExpressionsSpecs;

{
  The SQL-side runner: the emitter, the breadth pass-throughs and the in-memory
  repository adapter. Daf.SQLCute.Core is linked explicitly so its initialization
  registers the IQuery factory TQuery.New delegates to.
}

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.MiniSpec,
  Daf.SQLCute.Core in '..\..\SQLCute\Daf.SQLCute.Core.pas',
  Daf.Query.Expressions,
  Daf.SQLCute.Expressions,
  Lowering.SpecHelpers in 'Lowering.SpecHelpers.pas',
  Emitter.Feat in 'Emitter.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('SQLCute.Expressions')
    .Run;
end.
