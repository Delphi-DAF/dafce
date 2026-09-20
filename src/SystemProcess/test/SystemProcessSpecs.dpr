program SystemProcessSpecs;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.MiniSpec,
  Process.Feat in 'Process.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('SystemProcess')
    .Run;
end.
