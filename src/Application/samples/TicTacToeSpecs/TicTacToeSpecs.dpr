program TicTacToeSpecs;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Daf.MiniSpec,
  TicTacToe.Game in 'TicTacToe.Game.pas',
  TicTacToe.SpecHelpers in 'TicTacToe.SpecHelpers.pas',
  TicTacToe.Placement.Feat in 'TicTacToe.Placement.Feat.pas',
  TicTacToe.Movement.Feat in 'TicTacToe.Movement.Feat.pas',
  TicTacToe.Victory.Feat in 'TicTacToe.Victory.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
//  .Reporter('html')
    .Run;
end.
