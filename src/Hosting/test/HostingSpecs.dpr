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
  Lifecycle.Feat in 'Lifecycle.Feat.pas',
  ShadowedHelper.Feat in 'ShadowedHelper.Feat.pas',
  ContainerValidation.Feat in 'ContainerValidation.Feat.pas',
  DefaultSources.Feat in 'DefaultSources.Feat.pas',
  StopFailure.Feat in 'StopFailure.Feat.pas',
  StopAndRelease.Feat in 'StopAndRelease.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('Hosting')
    .Run;
end.
