program MiniSpecSpecs;

{$APPTYPE CONSOLE}

uses
  Daf.MiniSpec,
  Daf.MiniSpec.DataTable,
  Daf.MiniSpec.Doubles,
  SpecContextAccess.Feat in 'SpecContextAccess.Feat.pas',
  Lifecycle.Feat in 'Lifecycle.Feat.pas',
  Hooks.Feat in 'Hooks.Feat.pas',
  DataTable.Feat in 'DataTable.Feat.pas',
  SpecSuite.Feat in 'SpecSuite.Feat.pas',
  FeatureContext.Feat in 'FeatureContext.Feat.pas',
  PropertyInjection.Feat in 'PropertyInjection.Feat.pas',
  StepBinding.Feat in 'StepBinding.Feat.pas',
  PendingSteps.Feat in 'PendingSteps.Feat.pas',
  NoActionSteps.Feat in 'NoActionSteps.Feat.pas',
  Doubles.Feat in 'Doubles.Feat.pas';

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
