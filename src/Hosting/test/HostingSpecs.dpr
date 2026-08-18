program HostingSpecs;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.MiniSpec,
  StartupTasks.Feat in 'StartupTasks.Feat.pas',
  HostComposition.Feat in 'HostComposition.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('Hosting')
    .Run;
end.
