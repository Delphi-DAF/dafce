program MasterMind.Specs;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Daf.MiniSpec,
  Mastermind.DetectGameOver.Feat in 'Mastermind.DetectGameOver.Feat.pas',
  Mastermind.Game in 'Mastermind.Game.pas',
  Mastermind.ComputePoints.Feat in 'Mastermind.ComputePoints.Feat.pas',
  Mastermind.SpecHelpers in 'Mastermind.SpecHelpers.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
//  .Reporter('html')
    .Run;
end.
