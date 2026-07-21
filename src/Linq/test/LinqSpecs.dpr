program LinqSpecs;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.MiniSpec;

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('Linq')
    .Run;
end.
