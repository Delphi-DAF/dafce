program MiniSpecSpecs;

{$APPTYPE CONSOLE}

uses
  Daf.MiniSpec,
  Daf.MiniSpec.DataTable,
  FeatureWorld.Feat in 'FeatureWorld.Feat.pas',
  WorldLifecycle.Feat in 'WorldLifecycle.Feat.pas',
  FeatureHooks.Feat in 'FeatureHooks.Feat.pas',
  FeatureDataTable.Feat in 'FeatureDataTable.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Run;
end.
