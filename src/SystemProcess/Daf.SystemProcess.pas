unit Daf.SystemProcess;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Threading,
  System.TimeSpan,
  System.Diagnostics,
  Winapi.Windows,
  Winapi.Messages,
  Daf.Threading;

type

{$SCOPEDENUMS ON}
  TProcessResult = record
  public type
    TStatus = (Failed, Timeout, Running, Completed, Canceled);
  public
    Status: TStatus;
    ExitCode: DWord;
    Duration: TTimeSpan;
    LastError: string;
    function Succeeded: Boolean;
  end;
{$SCOPEDENUMS OFF}

  TProcess = class
  private
  public
    class function KillById(ProcessInfo: TProcessInformation; TimeoutMS: Cardinal = 5000): Boolean;overload;
    class function KillById(PID: Cardinal; TimeoutMS: Cardinal = 5000): Boolean;overload;
    class function HasExited(ProcessInfo: TProcessInformation): Boolean;overload;
    class function HasExited(PID: Cardinal): Boolean;overload;
  end;

  TProcessOutputProc = reference to procedure(Text: string);
  TProcessNotifyProc = reference to procedure(Result: TProcessResult);
  TIdleProc = reference to procedure;

  TEnvironmentVariables = TDictionary<string, string>;

  ISystemProcess = interface(IInvokable)
  ['{176C16BB-AFB7-4425-811D-051590125272}']
    function GetIsRunning: Boolean;
    function GetCommandLine: string;
    function GetWorkingDirectory: string;
    function GetTimeoutMS: Cardinal;
    function GetKillAfterTimeout: Boolean;
    function GetHideWindow: Boolean;
    function GetOnStdOut: TProcessOutputProc;
    function GetOnStdErr: TProcessOutputProc;
    function GetOnIdle: TProcessNotifyProc;
    function GetOnCancelled: TProcessNotifyProc;
    function GetOnFailed: TProcessNotifyProc;
    function GetOnKilled: TProcessNotifyProc;
    function GetOnCompleted: TProcessNotifyProc;

    function Execute(CancellationToken: ICancellationToken = nil): TProcessResult;
    function ExecuteAsync(const CancellationToken: ICancellationToken = nil): IFuture<TProcessResult>;
    procedure Kill;
    property IsRunning: Boolean read GetIsRunning;
    property CommandLine: string read GetCommandLine;
    property WorkingDirectory: string read GetWorkingDirectory;
    property TimeoutMS: Cardinal read GetTimeoutMS;
    property KillAfterTimeout: Boolean read GetKillAfterTimeout;
    property HideWindow: Boolean read GetHideWindow;
    property OnStdOut: TProcessOutputProc read GetOnStdOut;
    property OnStdErr: TProcessOutputProc read GetOnStdErr;
    property OnIdle: TProcessNotifyProc read GetOnIdle;
    property OnCancelled: TProcessNotifyProc read GetOnCancelled;
    property OnFailed: TProcessNotifyProc read GetOnFailed;
    property OnKilled: TProcessNotifyProc read GetOnKilled;
    property OnCompleted: TProcessNotifyProc read GetOnCompleted;
  end;

  TSystemProcess = class;
  TBuilder = class
  private
    FInstance: TSystemProcess;
  public
    constructor Create;
    function Command(const Value: string): TBuilder;
    function CmdArgs(const Args: array of string): TBuilder;
    function WorkingDir(const Value: string): TBuilder;
    function Timeout(Milliseconds: Cardinal): TBuilder;
    function KillAfterTimeout(Value: Boolean = True): TBuilder;
    function HideWindow(Value: Boolean = True): TBuilder;
    function OnStdOut(Proc: TProcessOutputProc): TBuilder;
    function OnStdErr(Proc: TProcessOutputProc): TBuilder;
    function OnIdle(Proc: TProcessNotifyProc): TBuilder;
    function OnFailed(Proc: TProcessNotifyProc): TBuilder;
    function OnCancelled(Proc: TProcessNotifyProc): TBuilder;
    function OnKilled(Proc: TProcessNotifyProc): TBuilder;
    function OnCompleted(Proc: TProcessNotifyProc): TBuilder;
    function EnvVar(const Key, Value: string): TBuilder;
    function Build: ISystemProcess;
    procedure ExecuteAndFree;
    procedure ExecuteAsyncAndFree(const Token: ICancellationToken = nil);
  end;

  TSystemProcess = class(TInterfacedObject, ISystemProcess)
  private
    FCommandLine: string;
    FWorkingDirectory: string;
    FTimeoutMS: Cardinal;
    FKillAfterTimeout: Boolean;
    FHideWindow: Boolean;
    FCustomEnvVars: TEnvironmentVariables;

    FOnStdOut: TProcessOutputProc;
    FOnStdErr: TProcessOutputProc;

    FOnIdle: TProcessNotifyProc;
    FOnCompleted: TProcessNotifyProc;
    FOnFailed: TProcessNotifyProc;
    FOnCancelled: TProcessNotifyProc;
    FOnKilled: TProcessNotifyProc;

    FStopWatch: TStopwatch;
    FProcessInfo: TProcessInformation;
    FStdOutRead, FStdOutWrite: THandle;
    FStdErrRead, FStdErrWrite: THandle;
    [volatile] FKillRequested: Boolean;
    FReadTasks: array of ITask;
    procedure CleanupHandles;
    procedure CreatePipes;
    procedure ClosePipes;
    procedure SetupStartupInfo(var SI: TStartupInfo);
    procedure StartReadingPipes;
    procedure WaitForReaders;

    procedure DoOutput(const Text: string; IsError: Boolean);
    procedure DoProcessCompleted(var Result: TProcessResult);
    procedure DoProcessFailed(var Result: TProcessResult);
    procedure DoProcessCanceled(var Result: TProcessResult);
    procedure DoProcessKilled(var Result: TProcessResult);
    procedure DoProcessIdle(var Result: TProcessResult);

    function TryCreateProcess(var ProcessResult: TProcessResult): Boolean;
    function GetIsRunning: Boolean;
    procedure BuildEnvBuffer(var EnvBuffer: PWideChar);
    function GetCommandLine: string;
    function GetWorkingDirectory: string;
    function GetTimeoutMS: Cardinal;
    function GetKillAfterTimeout: Boolean;
    function GetHideWindow: Boolean;
    function GetOnStdOut: TProcessOutputProc;
    function GetOnStdErr: TProcessOutputProc;
    function GetOnIdle: TProcessNotifyProc;
    function GetOnCancelled: TProcessNotifyProc;
    function GetOnFailed: TProcessNotifyProc;
    function GetOnKilled: TProcessNotifyProc;
    function GetOnCompleted: TProcessNotifyProc;

  public
    class function Builder: TBuilder;

    constructor Create;
    destructor Destroy; override;

    function Execute(CancellationToken: ICancellationToken = nil): TProcessResult;
    function ExecuteAsync(const CancellationToken: ICancellationToken = nil): IFuture<TProcessResult>;
    procedure Kill;

    property IsRunning: Boolean read GetIsRunning;
    property CommandLine: string read GetCommandLine;
    property WorkingDirectory: string read GetWorkingDirectory;
    property TimeoutMS: Cardinal read GetTimeoutMS;
    property KillAfterTimeout: Boolean read GetKillAfterTimeout;
    property HideWindow: Boolean read GetHideWindow;
    property OnStdOut: TProcessOutputProc read GetOnStdOut;
    property OnStdErr: TProcessOutputProc read GetOnStdErr;
    property OnIdle: TProcessNotifyProc read GetOnIdle;
    property OnCancelled: TProcessNotifyProc read GetOnCancelled;
    property OnFailed: TProcessNotifyProc read GetOnFailed;
    property OnKilled: TProcessNotifyProc read GetOnKilled;
    property OnCompleted: TProcessNotifyProc read GetOnCompleted;
  end;

implementation
uses
  Daf.Types;

{ TProcessResult }

function TProcessResult.Succeeded: Boolean;
begin
  Result := (Status = TStatus.Completed) and (ExitCode = 0);
end;

{ TProcess }

class function TProcess.HasExited(ProcessInfo: TProcessInformation): Boolean;
begin
  Result := HasExited(ProcessInfo.dwProcessId);
end;

class function TProcess.HasExited(PID: Cardinal): Boolean;
var
  hProcess: THandle;
  ExitCode: DWORD;
begin
  hProcess := OpenProcess(PROCESS_QUERY_INFORMATION, False, PID);
  if hProcess = 0 then
    Exit(True);

  try
    if not GetExitCodeProcess(hProcess, ExitCode) then
      Exit(True);
    Result := ExitCode <> STILL_ACTIVE;
  finally
    CloseHandle(hProcess);
  end;
end;

class function TProcess.KillById(ProcessInfo: TProcessInformation;
  TimeoutMS: Cardinal): Boolean;
begin
  Result := KillById(ProcessInfo.dwProcessId, TimeoutMS)
end;

class function TProcess.KillById(PID: Cardinal; TimeoutMS: Cardinal = 5000): Boolean;
begin
  var hProcess: THandle := OpenProcess(PROCESS_TERMINATE or SYNCHRONIZE, false, PID);
  if hProcess = 0 then
    Exit(False);
  try
    if TerminateProcess(hProcess, 0) then
    begin
      if WaitForSingleObject(hProcess, TimeoutMS) = WAIT_OBJECT_0 then
        Exit(True);
    end
    else
      RaiseLastOSError;
  finally
    CloseHandle(hProcess);
  end;

  Result := False;
end;

{ TBuilder }

constructor TBuilder.Create;
begin
  inherited Create;
  FInstance := TSystemProcess.Create;
end;

function TBuilder.Command(const Value: string): TBuilder;
begin
  FInstance.FCommandLine := Value;
  Result := Self;
end;

function TBuilder.CmdArgs(const Args: array of string): TBuilder;
begin
  FInstance.FCommandLine := FInstance.CommandLine + ' ' + string.Join(' ', Args);
  Result := Self;
end;

function TBuilder.WorkingDir(const Value: string): TBuilder;
begin
  FInstance.FWorkingDirectory := Value;
  Result := Self;
end;

function TBuilder.Timeout(Milliseconds: Cardinal): TBuilder;
begin
  FInstance.FTimeoutMS := Milliseconds;
  Result := Self;
end;

function TBuilder.OnStdOut(Proc: TProcessOutputProc): TBuilder;
begin
  FInstance.FOnStdOut := Proc;
  Result := Self;
end;

function TBuilder.OnStdErr(Proc: TProcessOutputProc): TBuilder;
begin
  FInstance.FOnStdErr := Proc;
  Result := Self;
end;

function TBuilder.OnFailed(Proc: TProcessNotifyProc): TBuilder;
begin
  FInstance.FOnFailed := Proc;
  Result := Self;
end;

function TBuilder.OnIdle(Proc: TProcessNotifyProc): TBuilder;
begin
  FInstance.FOnIdle := Proc;
  Result := Self;
end;

function TBuilder.OnCancelled( Proc: TProcessNotifyProc): TBuilder;
begin
  FInstance.FOnCancelled := Proc;
  Result := Self;
end;

function TBuilder.OnKilled(Proc: TProcessNotifyProc): TBuilder;
begin
  FInstance.FOnKilled:= Proc;
  Result := Self;
end;

function TBuilder.OnCompleted(Proc: TProcessNotifyProc)
  : TBuilder;
begin
  FInstance.FOnCompleted := Proc;
  Result := Self;
end;

function TBuilder.KillAfterTimeout(Value: Boolean = True): TBuilder;
begin
  Result := Self;
  FInstance.FKillAfterTimeout := Value;
end;

function TBuilder.HideWindow(Value: Boolean = True): TBuilder;
begin
  Result := Self;
  FInstance.FHideWindow := Value;
end;

function TBuilder.EnvVar(const Key, Value: string): TBuilder;
begin
  FInstance.FCustomEnvVars.AddOrSetValue(Key, Value);
  Result := Self;
end;

function TBuilder.Build: ISystemProcess;
begin
  Result := FInstance;
  Free;
end;

procedure TBuilder.ExecuteAndFree;
begin
  try
    FInstance.Execute;
  finally
    FInstance.Free;
  end;
end;

procedure TBuilder.ExecuteAsyncAndFree(const Token: ICancellationToken = nil);
begin
  TTask.Run(
    procedure
    begin
      try
        FInstance.ExecuteAsync(Token);
      finally
        FInstance.Free;
      end;
    end);
end;

{ TSystemProcess }

class function TSystemProcess.Builder: TBuilder;
begin
  Result := TBuilder.Create;
end;

constructor TSystemProcess.Create;
begin
  inherited Create;
  FTimeoutMS := INFINITE;
  FKillAfterTimeout := False;
  FCustomEnvVars := TEnvironmentVariables.Create;
end;

destructor TSystemProcess.Destroy;
begin
  FCustomEnvVars.Free;
  ClosePipes;
  inherited;
end;

procedure TSystemProcess.DoOutput(const Text: string; IsError: Boolean);
begin
  if IsError then
  begin
    if Assigned(FOnStdErr) then
      FOnStdErr(Text);
  end
  else
  begin
    if Assigned(FOnStdOut) then
      FOnStdOut(Text);
  end;
end;

procedure TSystemProcess.CleanupHandles;
begin
  if FProcessInfo.hProcess <> 0 then
    CloseHandle(FProcessInfo.hProcess);
  if FProcessInfo.hThread <> 0 then
    CloseHandle(FProcessInfo.hThread);

  FProcessInfo.hProcess := 0;
  FProcessInfo.hThread := 0;
end;

procedure TSystemProcess.CreatePipes;
var
  sa: TSecurityAttributes;
begin
  sa.nLength := SizeOf(sa);
  sa.bInheritHandle := True;
  sa.lpSecurityDescriptor := nil;

  if not CreatePipe(FStdOutRead, FStdOutWrite, @sa, 0) then
    RaiseLastOSError;
  if not CreatePipe(FStdErrRead, FStdErrWrite, @sa, 0) then
    RaiseLastOSError;

  SetHandleInformation(FStdOutRead, HANDLE_FLAG_INHERIT, 0);
  SetHandleInformation(FStdErrRead, HANDLE_FLAG_INHERIT, 0);
end;

procedure TSystemProcess.ClosePipes;
begin
  WaitForReaders;

  if FStdOutRead <> 0 then
    CloseHandle(FStdOutRead);
  if FStdOutWrite <> 0 then
    CloseHandle(FStdOutWrite);
  if FStdErrRead <> 0 then
    CloseHandle(FStdErrRead);
  if FStdErrWrite <> 0 then
    CloseHandle(FStdErrWrite);

  FStdOutRead := 0;
  FStdOutWrite := 0;

  FStdErrRead := 0;
  FStdErrWrite := 0;
end;

procedure TSystemProcess.SetupStartupInfo(var SI: TStartupInfo);
begin
  FillChar(SI, SizeOf(SI), 0);
  SI.cb := SizeOf(SI);
  if (FStdOutWrite <> 0) or (FStdErrWrite <> 0) then
  begin
    SI.dwFlags := STARTF_USESTDHANDLES;
    SI.hStdInput  := GetStdHandle(STD_INPUT_HANDLE);
    SI.hStdOutput := FStdOutWrite;
    SI.hStdError := FStdErrWrite;
  end;
end;

procedure TSystemProcess.StartReadingPipes;
  function StartReader(Pipe: THandle; IsError: Boolean): ITask;
  const
    BUF_SIZE = 2048;
  begin
    Result := TTask.Run(
      procedure
      var
        BytesRead: DWORD;
        Buffer: array [0 .. BUF_SIZE - 1] of AnsiChar;
        S: AnsiString;
      begin
        while True do
        begin
          if not ReadFile(Pipe, Buffer, BUF_SIZE - 1, BytesRead, nil) or (BytesRead = 0) then
            Break;

          SetString(S, Buffer, BytesRead);
          DoOutput(string(S), IsError);
        end;
      end);
  end;
begin

  if Assigned(OnStdOut) then
  begin
    SetLength(FReadTasks, 1 + Length(FReadTasks));
    FReadTasks[Length(FReadTasks) - 1] := StartReader(FStdOutRead, False);
  end;

  if Assigned(OnStdErr) then
  begin
    SetLength(FReadTasks, 1 + Length(FReadTasks));
    FReadTasks[Length(FReadTasks) - 1] := StartReader(FStdErrRead, True);
  end;
end;

procedure TSystemProcess.WaitForReaders;
begin
  TTask.WaitForAll(FReadTasks);
  SetLength(FReadTasks, 0);
end;

procedure TSystemProcess.BuildEnvBuffer(var EnvBuffer: PWideChar);
begin
  var SysEnvVars := TEnvVars.Create;
  try
    for var Pair in FCustomEnvVars do
      SysEnvVars.Vars.AddOrSetValue(Pair.Key, Pair.Value);
    SysEnvVars.WriteTo(EnvBuffer);
  finally
    SysEnvVars.Free;
  end;
end;

procedure TSystemProcess.DoProcessCompleted(var Result: TProcessResult);
begin
  GetExitCodeProcess(FProcessInfo.hProcess, Result.ExitCode);
  Result.Status := TProcessResult.TStatus.Completed;
  Result.Duration := FStopWatch.Elapsed;
  if Assigned(FOnCompleted) then
    OnCompleted(Result);
end;

procedure TSystemProcess.DoProcessFailed(var Result: TProcessResult);
begin
  Result.ExitCode := GetLastError;
  Result.Status := TProcessResult.TStatus.Failed;
  Result.Duration := FStopWatch.Elapsed;
  Result.LastError := SysErrorMessage(GetLastError);
  if Assigned(FOnFailed) then
    OnFailed(Result);
end;

procedure TSystemProcess.DoProcessCanceled(var Result: TProcessResult);
begin
  Result.ExitCode := 0;
  Result.Status := TProcessResult.TStatus.Canceled;
  Result.Duration := FStopWatch.Elapsed;

  if not TProcess.KillById(FProcessInfo) then
    Result.Status := TProcessResult.TStatus.Failed;

  if Assigned(FOnCancelled) then
    OnCancelled(Result);
end;

procedure TSystemProcess.DoProcessKilled(var Result: TProcessResult);
begin
  Result.ExitCode := 0;
  Result.Status := TProcessResult.TStatus.Timeout;
  Result.Duration := FStopWatch.Elapsed;
  TProcess.KillById(FProcessInfo);
  if Assigned(FOnKilled) then
    OnKilled(Result);
end;

procedure TSystemProcess.DoProcessIdle(var Result: TProcessResult);
begin
  Result.ExitCode := 0;
  Result.Status := TProcessResult.TStatus.Running;
  Result.Duration := FStopWatch.Elapsed;

  if Assigned(FOnIdle) then
    FOnIdle(Result);
end;

function TSystemProcess.TryCreateProcess(var ProcessResult: TProcessResult): Boolean;
const
  DefaultFlags = CREATE_NEW_PROCESS_GROUP or NORMAL_PRIORITY_CLASS or
                 CREATE_SUSPENDED or CREATE_UNICODE_ENVIRONMENT;
begin
  ProcessResult.Status := TProcessResult.TStatus.Failed;
  ProcessResult.ExitCode := 0;

  var
    WorkDir: PWideChar := nil;
  if not  FWorkingDirectory.IsEmpty then
    WorkDir := PWideChar(FWorkingDirectory);

  var
    StartInfo: TStartupInfo;
  SetupStartupInfo(StartInfo);
  FillChar(FProcessInfo, SizeOf(FProcessInfo), 0);

  var EnvBuffer: PWideChar := nil;
  BuildEnvBuffer(EnvBuffer);

(* To quick check EnvBuffer & CreateProcess:

  Writeln('[Begin of env block]');
  var p := EnvBuffer;
  while not ((p^ = #0) and ((p + 1)^ = #0)) do
  begin
    Writeln(p);
    Inc(p, Length(p) + 1);
  end;
  Writeln('[End of env block]');

  FCommandLine := 'cmd.exe /C echo PATH=%PATH%';
*)

  var Flags := DefaultFlags;
  if HideWindow then
  begin
    Flags := Flags or CREATE_NO_WINDOW;
    StartInfo.dwFlags := StartInfo.dwFlags or STARTF_USESHOWWINDOW;
    StartInfo.wShowWindow := SW_HIDE;
  end
  else
    Flags := Flags or CREATE_NEW_CONSOLE;

  try
    Result := CreateProcessW(nil, PWideChar(FCommandLine), nil, nil, True, Flags,
      EnvBuffer, WorkDir, StartInfo, FProcessInfo);
  finally
    FreeMem(EnvBuffer);
  end;
end;

function TSystemProcess.Execute(CancellationToken: ICancellationToken = nil): TProcessResult;
begin
  CreatePipes;
  try
    if not TryCreateProcess(Result) then
    begin
      DoProcessFailed(Result);
      Exit;
    end;
    FStopWatch := TStopwatch.StartNew;
    ResumeThread(FProcessInfo.hThread);
    CloseHandle(FProcessInfo.hThread);
    FProcessInfo.hThread := 0;

    CloseHandle(FStdOutWrite); FStdOutWrite := 0;
    CloseHandle(FStdErrWrite); FStdErrWrite := 0;
    StartReadingPipes;

    var WaitRes: DWord;
    repeat
      WaitRes := WaitForSingleObject(FProcessInfo.hProcess, 250);

      if (CancellationToken <> nil) and CancellationToken.IsCancellationRequested then
      begin
        DoProcessCanceled(Result);
        Break;
      end;

      if FKillRequested or ((FTimeoutMS <> INFINITE) and (FStopWatch.ElapsedMilliseconds >= FTimeoutMS)) then
      begin
        DoProcessKilled(Result);
        Break;
      end;

      DoProcessIdle(Result);

    until (WaitRes = WAIT_OBJECT_0);

    if Result.Status = TProcessResult.TStatus.Running then
    begin
      if FKillRequested then
         DoProcessKilled(Result)
      else
      if WaitRes = WAIT_OBJECT_0 then
        DoProcessCompleted(Result)
      else
        DoProcessFailed(Result);
    end;

  finally
    FStopWatch.Stop;
    CleanupHandles;
    ClosePipes;
  end;
end;

function TSystemProcess.GetCommandLine: string;
begin
  REsult := FCommandLine;
end;

function TSystemProcess.GetHideWindow: Boolean;
begin
  Result := FHideWindow;
end;

function TSystemProcess.GetIsRunning: Boolean;
begin
  if FProcessInfo.hProcess = 0 then Exit(False);
  Result := not TProcess.HasExited(FProcessInfo);
end;

function TSystemProcess.GetKillAfterTimeout: Boolean;
begin
  Result := FKillAfterTimeout;
end;

function TSystemProcess.GetOnCancelled: TProcessNotifyProc;
begin
  REsult := FOnCancelled;
end;

function TSystemProcess.GetOnCompleted: TProcessNotifyProc;
begin
  Result := FOnCompleted;
end;

function TSystemProcess.GetOnFailed: TProcessNotifyProc;
begin
  REsult :=  FOnFailed;
end;

function TSystemProcess.GetOnIdle: TProcessNotifyProc;
begin
  REsult := FOnIdle;
end;

function TSystemProcess.GetOnKilled: TProcessNotifyProc;
begin
  Result := FOnKilled;
end;

function TSystemProcess.GetOnStdErr: TProcessOutputProc;
begin
  Result := FOnStdErr;
end;

function TSystemProcess.GetOnStdOut: TProcessOutputProc;
begin
  REsult := FOnStdOut;
end;

function TSystemProcess.GetTimeoutMS: Cardinal;
begin
  Result := FTimeoutMS;
end;

function TSystemProcess.GetWorkingDirectory: string;
begin
  REsult := FWorkingDirectory;
end;

procedure TSystemProcess.Kill;
begin
  FKillRequested := True;
  TProcess.KillById(Self.FProcessInfo);
end;

function TSystemProcess.ExecuteAsync(const CancellationToken: ICancellationToken = nil): IFuture<TProcessResult>;
begin
  Result := TTask.Future<TProcessResult>(
    function: TProcessResult
    begin
      Result := Execute(CancellationToken);
    end);
end;

end.
