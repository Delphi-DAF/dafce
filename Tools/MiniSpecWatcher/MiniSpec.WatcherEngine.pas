unit MiniSpec.WatcherEngine;

interface
uses
  System.Classes,
  System.SysUtils,
  System.Threading,
  System.Generics.Collections,
  MiniSpec.DirectoryWatcher;

type
  TReportInfo = record
    ExeFile: string;
    PassCount: Cardinal;
    FailsCount: Cardinal;
    LinkToReport: string;
    IsEmpty: Boolean;
  end;

  TWatcherEngine = class;
  TReportReadyEvent = reference to procedure(Sender: TWatcherEngine; const ReportInfo: TReportInfo);
  TWatcherEngine = class
  strict private
    Watcher: TDirectoryWatcher;
    FOnReportReady: TReportReadyEvent;
    procedure DoReportReady(const ExeFile, Report: string);
  private
    FBussy: Boolean;
    FWatchDir: string;
    procedure SetWatchDir(const Value: string);
    procedure OnFileChanged(Sender: TObject; const FileName: string; Action: TFileAction);
  public
    destructor Destroy;override;
    procedure RunSpecs(const ExeFile: string);
    procedure ViewReport(const LinkToReport: string);
    procedure Start;
    procedure Stop;
    property WatchDir: string read FWatchDir write SetWatchDir;
    property OnReportReady: TReportReadyEvent read FOnReportReady write FOnReportReady;
  end;

procedure Log(const Text: string);overload;
procedure Log(const FmtText: string; Args: array of const);overload;
implementation
uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ShellAPI,
  System.IOUtils;

procedure Log(const FmtText: string; Args: array of const);
begin
  Log(Format(FmtText, Args));
end;

procedure Log(const Text: string);
begin
  OutputDebugString(PChar('[' + TPath.GetFileNameWithoutExtension(Paramstr(0)) + ']' + Text));
end;

{ TWatcherEngine }

destructor TWatcherEngine.Destroy;
begin
  Stop;
  inherited;
end;

procedure TWatcherEngine.DoReportReady(const ExeFile, Report: string);
begin
  Log('DoReportReady(%s, %s)', [ExeFile, Report]);
  var
    ReportInfo: TReportInfo;

  ReportInfo.ExeFile := ExeFile;
  ReportInfo.IsEmpty  := Report.IsEmpty;

  if not Report.IsEmpty then
  begin
    var lines := Report.Split([SLineBreak]);
    Delete(Lines[1], 1, length('report detail: '));
    ReportInfo.LinkToReport := Lines[1];
    Log('ReportInfo.LinkToReport = %s', [ReportInfo.LinkToReport]);
    var Counters := Lines[0].Split(['|']);
    ReportInfo.PassCount := Cardinal.Parse(Counters[0].Split([':'])[1].Trim);
    ReportInfo.FailsCount := Cardinal.Parse(Counters[1].Split([':'])[1].Trim);
  end;

  if Assigned(FOnReportReady) then
    FOnReportReady(Self, ReportInfo);
end;

procedure TWatcherEngine.ViewReport(const LinkToReport: string);
begin
  Log('ViewReport(%s)', [LinkToReport]);
  ShellExecute(0, 'open', PChar(LinkToReport), nil, nil, SW_SHOWNORMAL);
end;

procedure TWatcherEngine.OnFileChanged(Sender: TObject; const FileName: string; Action: TFileAction);
const
  ActionStr: array[TFileAction] of string = ('Added', 'Removed', 'Modified', 'RenamedOldName', 'RenamedNewName');
begin
  if FBussy then Exit;
  FBussy := True;
  try
    Log('File %s has been %s', [FileName, ActionStr[Action]]);
    RunSpecs(FileName);
  finally
    FBussy := False;
  end;
end;

procedure TWatcherEngine.Start;
begin
  Watcher := TDirectoryWatcher.Create(WatchDir, '*Specs.exe', OnFileChanged, False);
  Watcher.Start;
end;

procedure TWatcherEngine.Stop;
begin
  if not Assigned(Watcher) then Exit;
  try
    Watcher.StopWatching;
    Watcher.WaitFor;
  finally
    FreeAndNil(Watcher);
  end;
end;

procedure TWatcherEngine.RunSpecs(const ExeFile: string);
const
  BUF_SIZE = 2048;
begin
  var SI: TStartupInfo;
  var SecAttr: TSecurityAttributes;
  FillChar(SI, SizeOf(SI), 0);
  SI.cb := SizeOf(SI);
  SI.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
  SI.wShowWindow := SW_HIDE;
  SecAttr.nLength := SizeOf(SecAttr);
  SecAttr.bInheritHandle := True;
  SecAttr.lpSecurityDescriptor := nil;

  var hRead, hWrite: THandle;
  if not CreatePipe(hRead, hWrite, @SecAttr, 0) then Exit;

  try
    SI.hStdOutput := hWrite;
    SI.hStdError := hWrite;
    SI.hStdInput := GetStdHandle(STD_INPUT_HANDLE);

    var PI: TProcessInformation;
    var CmdLine := '"' + ExeFile + '" -r html';
    if not CreateProcess(nil, PChar(CmdLine), nil, nil, True, 0, nil, nil, SI, PI) then Exit;
    Log('RunSpecs for ' + ExeFIle);
    try
      CloseHandle(hWrite);
      var BytesRead := DWORD(0);
      var S: AnsiString;
      var Buffer: array [0 .. BUF_SIZE - 1] of AnsiChar;
      while ReadFile(hRead, Buffer, BUF_SIZE - 1, BytesRead, nil) and (BytesRead > 0) do
      begin
        SetString(S, Buffer, BytesRead);
      end;
      var Output := Trim(string(S));
      Log('output: ' + output);
      DoReportReady(ExeFile, Output);
    finally
      CloseHandle(PI.hProcess);
      CloseHandle(PI.hThread);
    end;
  finally
    CloseHandle(hRead);
  end;
end;

procedure TWatcherEngine.SetWatchDir(const Value: string);
begin
  FWatchDir := Value;
  if TPath.IsRelativePath(WatchDir) then
    FWatchDir := TPath.GetFullPath(TPath.Combine(TDirectory.GetCurrentDirectory, FWatchDir));
  if not TDirectory.Exists(FWatchDir) then
   raise Exception.Create('Directory ' + FWatchDir + ' not found');
end;

end.
