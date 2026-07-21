program SQLCuteInMemorySpecs;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.MiniSpec,
  Daf.SQLCute.InMemory;

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('SQLCute.InMemory')
    .Run;
end.
