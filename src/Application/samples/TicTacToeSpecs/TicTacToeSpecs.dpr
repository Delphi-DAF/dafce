program TicTacToeSpecs;

{$APPTYPE CONSOLE}

uses
  Daf.MiniSpec,
  TicTacToe.Game in 'TicTacToe.Game.pas',
  TicTacToe.ViewPort in 'TicTacToe.ViewPort.pas',
  TicTacToe.ViewModel in 'TicTacToe.ViewModel.pas',
  TicTacToe.SpecHelpers in 'TicTacToe.SpecHelpers.pas',
  // Step bindings (registrados antes que los Features)
  TicTacToe.Placement.Steps in 'TicTacToe.Placement.Steps.pas',
  TicTacToe.Movement.Steps in 'TicTacToe.Movement.Steps.pas',
  TicTacToe.Victory.Steps in 'TicTacToe.Victory.Steps.pas',
  // Features (Gherkin)
  TicTacToe.Placement.Feat in 'TicTacToe.Placement.Feat.pas',
  TicTacToe.Movement.Feat in 'TicTacToe.Movement.Feat.pas',
  TicTacToe.Victory.Feat in 'TicTacToe.Victory.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
//  .Reporter('html')
    .Run;
end.
