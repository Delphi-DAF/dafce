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
  Emission.SpecHelpers in 'Emission.SpecHelpers.pas',
  Emitter.Feat in 'Emitter.Feat.pas',
  Repository.Feat in 'Repository.Feat.pas',
  Parity.Feat in 'Parity.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('SQLCute.Expressions')
    .Run;
end.
