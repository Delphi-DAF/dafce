program CalculatorSpecs;

{$APPTYPE CONSOLE}

uses
  Daf.MiniSpec,
  Calculator.Add.Feature in 'Calculator.Add.Feature.pas',
  Calculator.Engine in 'Calculator.Engine.pas',
  Calculator.SpecHelpers in 'Calculator.SpecHelpers.pas',
  Calculator.Mult.Feature in 'Calculator.Mult.Feature.pas',
  FeatureWorld.Feat in 'FeatureWorld.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
//  .Reporter('html')
    .Run;
end.
