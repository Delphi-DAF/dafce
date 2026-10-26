program FirstSpec;

{$APPTYPE CONSOLE}

uses
  Daf.MiniSpec,
  Calculator.Engine in '..\CalculatorSpecs\Calculator.Engine.pas',
  Addition.Feat in 'Addition.Feat.pas';

begin
  MiniSpec.Run;
end.
