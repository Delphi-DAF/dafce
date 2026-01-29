program MiniSpecSpecs;

{$APPTYPE CONSOLE}

uses
  Daf.MiniSpec,
  Daf.MiniSpec.DataTable,
  FeatureWorld.Feat in 'FeatureWorld.Feat.pas',
  WorldLifecycle.Feat in 'WorldLifecycle.Feat.pas',
  FeatureHooks.Feat in 'FeatureHooks.Feat.pas',
  FeatureDataTable.Feat in 'FeatureDataTable.Feat.pas',
  FeatureSpecSuite.Feat in 'FeatureSpecSuite.Feat.pas',
  SuiteContext.Feat in 'SuiteContext.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('MiniSpec Framework Tests')
    .Before('Initialize test environment',
      procedure
      begin
        SuiteBeforeHookCalled := True;
        SuiteTitle := MiniSpec.Suite.Title;
      end)
    .After('Cleanup test environment',
      procedure
      begin
        SuiteAfterHookCalled := True;
      end)
    .Run;

  //Assert(SuiteAfterHookCalled, 'Suite After hook was not called');
end.
