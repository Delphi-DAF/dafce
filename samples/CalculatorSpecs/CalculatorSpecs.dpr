program CalculatorSpecs;

{$APPTYPE CONSOLE}

uses
  Daf.MiniSpec,
  Calculator.Add.Feat in 'Calculator.Add.Feat.pas',
  Calculator.Engine in 'Calculator.Engine.pas',
  Calculator.SpecHelpers in 'Calculator.SpecHelpers.pas',
  Calculator.Mult.Feat in 'Calculator.Mult.Feat.pas',
  FeatureWorld.Feat in 'FeatureWorld.Feat.pas',
  WorldLifecycle.Feat in 'WorldLifecycle.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
//  .Reporter('html')
    .Run;
end.
