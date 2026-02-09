program TicTacToeApp;

uses
  Vcl.Forms,
  TicTacToe.Game in 'TicTacToe.Game.pas',
  TicTacToe.ViewPort in 'TicTacToe.ViewPort.pas',
  TicTacToe.ViewModel in 'TicTacToe.ViewModel.pas',
  TicTacToe.MainForm in 'TicTacToe.MainForm.pas' {MainForm};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
