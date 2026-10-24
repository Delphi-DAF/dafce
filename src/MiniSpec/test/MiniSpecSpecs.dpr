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
  StepStatus.Feat in 'StepStatus.Feat.pas',
  OutlinePlaceholderStep.Feat in 'OutlinePlaceholderStep.Feat.pas',
  Doubles.Feat in 'Doubles.Feat.pas',
  ExitCode.Feat in 'ExitCode.Feat.pas',
  SuiteContext.Feat in 'SuiteContext.Feat.pas',
  InjectionPrecedence.Feat in 'InjectionPrecedence.Feat.pas',
  NestedRun.Feat in 'NestedRun.Feat.pas',
  FailureVisibility.Feat in 'FailureVisibility.Feat.pas',
  FailureReason.Feat in 'FailureReason.Feat.pas',
  BackgroundSteps.Feat in 'BackgroundSteps.Feat.pas',
  ScenarioOutline.Feat in 'ScenarioOutline.Feat.pas',
  SuiteHooks.Feat in 'SuiteHooks.Feat.pas',
  JUnitReport.Feat in 'JUnitReport.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    // StepStatus.Feat marca un step como Undefined a proposito, que es la unica forma de
    // comprobar que los siguientes se saltan. Sin esto la suite no puede salir verde nunca.
    // Lo declara aqui el programa que lo necesita, y no el script que lo lanza.
    .AllowUndefined(True)
    .UseSuiteContext<TSuiteResources>
    .Category('MiniSpec Framework Tests')
    .Before('Initialize test environment',
      procedure
      begin
        SuiteBeforeHookCalled := True;
        SuiteTitle := MiniSpec.Suite.Title;
      end)
    .Run;

end.
