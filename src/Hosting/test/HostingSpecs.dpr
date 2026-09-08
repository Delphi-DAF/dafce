program HostingSpecs;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.MiniSpec,
  StartupTasks.Feat in 'StartupTasks.Feat.pas',
  HostComposition.Feat in 'HostComposition.Feat.pas',
  StartupReporting.Feat in 'StartupReporting.Feat.pas',
  BuilderStyle.Feat in 'BuilderStyle.Feat.pas',
  ShutdownRequest.Feat in 'ShutdownRequest.Feat.pas',
  ShadowedHelper.Feat in 'ShadowedHelper.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('Hosting')
    .Run;
end.
