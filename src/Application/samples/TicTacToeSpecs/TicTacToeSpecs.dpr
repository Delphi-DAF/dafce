program TicTacToeSpecs;

{$APPTYPE CONSOLE}

uses
  Daf.MiniSpec,
  TicTacToe.Game in 'Domain\TicTacToe.Game.pas',
  TicTacToe.ViewPort in 'Presentation\TicTacToe.ViewPort.pas',
  TicTacToe.ViewModel in 'Presentation\TicTacToe.ViewModel.pas',
  TicTacToe.SpecHelpers in 'Specs\TicTacToe.SpecHelpers.pas',
  // Step bindings (registrados antes que los Features)
  TicTacToe.Placement.Steps in 'Specs\TicTacToe.Placement.Steps.pas',
  TicTacToe.Movement.Steps in 'Specs\TicTacToe.Movement.Steps.pas',
  TicTacToe.Victory.Steps in 'Specs\TicTacToe.Victory.Steps.pas',
  TicTacToe.UX.Steps in 'Specs\TicTacToe.UX.Steps.pas',
  // Features (Gherkin)
  TicTacToe.Placement.Feat in 'Specs\TicTacToe.Placement.Feat.pas',
  TicTacToe.Movement.Feat in 'Specs\TicTacToe.Movement.Feat.pas',
  TicTacToe.Victory.Feat in 'Specs\TicTacToe.Victory.Feat.pas',
  TicTacToe.UX.Feat in 'Specs\TicTacToe.UX.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
//  .Reporter('html')
    .Run;
end.
