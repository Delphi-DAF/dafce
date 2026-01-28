program MiniSpecSpecs;

{$APPTYPE CONSOLE}

uses
  Daf.MiniSpec,
  FeatureWorld.Feat in 'FeatureWorld.Feat.pas',
  WorldLifecycle.Feat in 'WorldLifecycle.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Run;
end.
