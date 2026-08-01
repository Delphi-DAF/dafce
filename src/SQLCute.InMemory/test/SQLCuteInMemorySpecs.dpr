program SQLCuteInMemorySpecs;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.MiniSpec,
  Daf.SQLCute.Core in '..\..\SQLCute\Daf.SQLCute.Core.pas',
  Daf.SQLCute.InMemory,
  FastPath.Feat in 'FastPath.Feat.pas',
  Projection.Feat in 'Projection.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('SQLCute.InMemory')
    .Run;
end.
