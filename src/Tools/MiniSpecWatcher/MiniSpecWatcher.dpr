program MiniSpecWatcher;

uses
  WinApi.Windows,
  System.SysUtils,
  System.IOUtils,
  Vcl.Forms,
  MiniSpec.ResultsView in 'MiniSpec.ResultsView.pas' {ResultsView},
  Vcl.Themes,
  Vcl.Styles,
  MiniSpec.WatcherEngine in 'MiniSpec.WatcherEngine.pas',
  MiniSpec.DirectoryWatcher in 'MiniSpec.DirectoryWatcher.pas';

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := False;
  Application.ShowMainForm := False;
  TStyleManager.TrySetStyle('Windows10 SlateGray');

  var WatcherEngine: TWatcherEngine := nil;
  try
    if ParamCount = 0 then
      raise Exception.Create('Usage: MiniSpecWatcher.exe [directory]');
    WatcherEngine := TWatcherEngine.Create;
    WatcherEngine.WatchDir := ParamStr(1);
  except
    on E: Exception do
    begin
      WatcherEngine.Free;
      Application.ShowException(E);
      Application.Terminate;
      Exit;
    end;
  end;
  Application.CreateForm(TResultsView, ResultsView);
  ResultsView.WatcherEngine := WatcherEngine;
  Application.Run;
end.
