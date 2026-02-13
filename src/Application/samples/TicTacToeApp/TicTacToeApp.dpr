program TicTacToeApp;

uses
  Vcl.Forms,
  TicTacToe.Game in 'Domain\TicTacToe.Game.pas',
  TicTacToe.ViewPort in 'Presentation\TicTacToe.ViewPort.pas',
  TicTacToe.ViewModel in 'Presentation\TicTacToe.ViewModel.pas',
  TicTacToe.MainForm in 'UI\TicTacToe.MainForm.pas' {MainForm};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
