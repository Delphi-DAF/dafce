unit Daf.Threading;

interface

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
{$IFDEF POSIX}
  Posix.Signal, Posix.Unistd,
{$ENDIF}
  System.Classes,
  System.Threading,
  System.SyncObjs,
  System.SysUtils,
  System.Generics.Collections,
  System.Types;

type
  TTimerProc = reference to procedure;
  TThreadTimer = class
  private
    FInterval: Cardinal;
    FThread: TThread;
    FEnabled: Boolean;
    FLock: TCriticalSection;
    FOnTimer: TTimerProc;
    procedure StartTimerThread;
    procedure StopTimerThread;
  public
    constructor Create(TimeoutMs: Cardinal; const OnTimer: TTimerProc);
    destructor Destroy; override;
    procedure SetEnabled(Value: Boolean);
    procedure SetInterval(Value: Cardinal);
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Interval: Cardinal read FInterval write SetInterval;
    property OnTimer: TTimerProc read FOnTimer;
  end;

  ICancellationToken = interface
    ['{A503E6E7-CA5D-4BE2-8900-10F3BC73B138}']
    function IsCancellationRequested: Boolean;
    function WaitFor(Timeout: Cardinal): TWaitResult;
    procedure Register(const Callback: TProc);
  end;

  ICancellationTokenSource = interface
    ['{DAD5A7C0-6377-4D79-B7DC-4C3298F7D4ED}']
    function Token: ICancellationToken;
    function WaitFor(Timeout: Cardinal): TWaitResult;
    procedure Cancel;
    function IsCancellationRequested: Boolean;
  end;

  TShutdownHook = class
  private
    class var FCtrlHandlerEntered: Boolean;
    class var FTerminated: Boolean;
    class var FEvent: TEvent;
    {$IFDEF MSWINDOWS}
    class function WinHandleSignal(dwCtrlType: DWORD): BOOL; static; stdcall;
    class procedure DoSignalHandled; static;
    {$ENDIF}
    {$IFDEF POSIX}
    class procedure HandleSignal(SigNum: Integer); static;
    {$ENDIF}
  public
    // You can translate this message on app start
    class var PresCtlCMsg: string;
    class var OnShutdownRequested: TProc;
    class constructor Create;
    class destructor Destroy;
    class procedure WaitForShutdown;
    class property Terminated: Boolean read FTerminated;
    class procedure CreateHook;
  end;

  IRunnable = interface(IInvokable)
    ['{2D6E50DB-296D-48CE-BD3A-BEF7A74DB9A1}']
    procedure Run;
  end;

  TRunnable = class (TInterfacedObject, IRunnable)
  protected
    procedure BeforeRun;virtual;
    procedure DoRun;virtual;
    procedure AfterRun;virtual;
    procedure DoException(const E: Exception);virtual;
  public
    procedure Run;
  end;

  TRunQueue = record
  strict private
    FTasks: TArray<ITask>;
    FTThreadPool: TThreadPool;
  private
    function Run(const Runnable: IRunnable): TProc;
    procedure Add(const Task: ITask);overload;
  public
    class operator Finalize(var Dest: TRunQueue);
    constructor Create(const MaxSize: Integer);
    procedure Add(const Runnable: IRunnable);overload;
    procedure Start;
    procedure WaitForAll;
  end;

function CreateCancellationTokenSource: ICancellationTokenSource;
function CreateCanceledCancellationTokenSource: ICancellationTokenSource;
function CreateCancellationTokenSourceWithTimeout(Timeout: Cardinal): ICancellationTokenSource;
function CreateLinkedCancellationTokenSource(const Tokens: array of ICancellationToken): ICancellationTokenSource;

implementation

type
  TCancellationToken = class(TInterfacedObject, ICancellationToken)
  private
    FSource: ICancellationTokenSource;
  public
    constructor Create(const Source: ICancellationTokenSource);
    function IsCancellationRequested: Boolean;
    function WaitFor(Timeout: Cardinal): TWaitResult;
    procedure Register(const Callback: TProc);
  end;

  TCancellationTokenSource = class(TInterfacedObject, ICancellationTokenSource)
  private
    FEvent: TEvent;
    FCallbacks: TList<TProc>;
  public
    constructor Create;
    destructor Destroy; override;
    function Token: ICancellationToken;
    function IsCancellationRequested: Boolean;
    function WaitFor(Timeout: Cardinal): TWaitResult;
    procedure Cancel;
    procedure Register(const Callback: TProc);
  end;

  TTimeoutCancellationSource = class(TCancellationTokenSource)
  private
    FTimer: TThreadTimer;
    procedure OnTimer;
  public
    constructor Create(Timeout: Cardinal);
    destructor Destroy; override;
  end;

  TLinkedCancellationSource = class(TCancellationTokenSource)
  private
    FRegistrations: TObjectList<TThread>;
    procedure MonitorToken(const Token: ICancellationToken);
  public
    constructor Create(const Tokens: array of ICancellationToken);
    destructor Destroy; override;
  end;

{ Factory Functions }

function CreateCanceledCancellationTokenSource: ICancellationTokenSource;
begin
  Result := TCancellationTokenSource.Create;
  Result.Cancel;
end;

function CreateCancellationTokenSourceWithTimeout(Timeout: Cardinal): ICancellationTokenSource;
begin
  Result := TTimeoutCancellationSource.Create(Timeout);
end;

function CreateLinkedCancellationTokenSource(const Tokens: array of ICancellationToken): ICancellationTokenSource;
begin
  Result := TLinkedCancellationSource.Create(Tokens);
end;

function CreateCancellationTokenSource: ICancellationTokenSource;
begin
  Result := TCancellationTokenSource.Create;
end;


{ TThreadTimer }

constructor TThreadTimer.Create(TimeoutMs: Cardinal; const OnTimer: TTimerProc);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FInterval := TimeoutMs;
  FOnTimer := OnTimer;
end;

destructor TThreadTimer.Destroy;
begin
  SetEnabled(False);
  FLock.Free;
  inherited;
end;

procedure TThreadTimer.StartTimerThread;
begin
  FLock.Enter;
  try
    if Assigned(FThread) then
      Exit;

    FThread := TThread.CreateAnonymousThread(
      procedure
      begin
        TThread.Sleep(FInterval);
        FLock.Enter;
        try
          if Assigned(FOnTimer) and FEnabled then
            OnTimer();
        finally
          FLock.Leave;
        end;
      end
    );
    FThread.FreeOnTerminate := True;
    FThread.Start;
  finally
    FLock.Leave;
  end;
end;

procedure TThreadTimer.StopTimerThread;
begin
  FLock.Enter;
  try
    if Assigned(FThread) then
    begin
      FThread.Terminate;
      FThread := nil;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TThreadTimer.SetEnabled(Value: Boolean);
begin
  FLock.Enter;
  try
    if FEnabled = Value then
      Exit;

    FEnabled := Value;

    if FEnabled then
      StartTimerThread
    else
      StopTimerThread;
  finally
    FLock.Leave;
  end;
end;

procedure TThreadTimer.SetInterval(Value: Cardinal);
begin
  FLock.Enter;
  try
    FInterval := Value;
    if FEnabled then
    begin
      StopTimerThread;
      StartTimerThread;
    end;
  finally
    FLock.Leave;
  end;
end;

{ TCancellationToken }

constructor TCancellationToken.Create(const Source: ICancellationTokenSource);
begin
  inherited Create;
  FSource := Source;
end;

function TCancellationToken.IsCancellationRequested: Boolean;
begin
  Result := FSource.IsCancellationRequested
end;

procedure TCancellationToken.Register(const Callback: TProc);
begin
  (FSource as TCancellationTokenSource).Register(Callback)
end;

function TCancellationToken.WaitFor(Timeout: Cardinal): TWaitResult;
begin
  Result := FSource.WaitFor(Timeout);
end;

{ TCancellationTokenSource }

constructor TCancellationTokenSource.Create;
begin
  inherited Create;
  FEvent := TEvent.Create(nil, True, False, '');
  FCallbacks := TList<TProc>.Create;
end;

destructor TCancellationTokenSource.Destroy;
begin
  FCallbacks.Free;
  FEvent.Free;
  inherited;
end;

function TCancellationTokenSource.Token: ICancellationToken;
begin
  Result := TCancellationToken.Create(Self);
end;

procedure TCancellationTokenSource.Cancel;
begin
  FEvent.SetEvent;
  TMonitor.Enter(FCallbacks);
  try
    for var Callback in FCallbacks do
      Callback();
    FCallbacks.Clear;
  finally
    TMonitor.Exit(FCallbacks);
  end;
end;

function TCancellationTokenSource.IsCancellationRequested: Boolean;
begin
  Result := FEvent.WaitFor(0) = wrSignaled;
end;

procedure TCancellationTokenSource.Register(const Callback: TProc);
begin
 if IsCancellationRequested then
    Callback()
  else
  begin
    TMonitor.Enter(FCallbacks);
    try
      FCallbacks.Add(Callback);
    finally
      TMonitor.Exit(FCallbacks);
    end;
  end;
end;

function TCancellationTokenSource.WaitFor(Timeout: Cardinal): TWaitResult;
begin
  Result := FEvent.WaitFor(Timeout);
end;

{ TTimeoutCancellationSource }

constructor TTimeoutCancellationSource.Create(Timeout: Cardinal);
begin
  inherited Create;
  FTimer := TThreadTimer.Create(Timeout, OnTimer);
  FTimer.Enabled := False;
  FTimer.Enabled := True;
end;

destructor TTimeoutCancellationSource.Destroy;
begin
  FTimer.Free;
  inherited;
end;

procedure TTimeoutCancellationSource.OnTimer;
begin
  Cancel;
  FTimer.Enabled := False;
end;

{ TLinkedCancellationSource }

constructor TLinkedCancellationSource.Create(const Tokens: array of ICancellationToken);
begin
  inherited Create;
  FRegistrations := TObjectList<TThread>.Create(True);
  for var Token in Tokens do
    if Assigned(Token) then
      MonitorToken(Token);
end;

destructor TLinkedCancellationSource.Destroy;
begin
  FRegistrations.Free;
  inherited;
end;

procedure TLinkedCancellationSource.MonitorToken(const Token: ICancellationToken);
begin
  var MonitorThread := TThread.CreateAnonymousThread(
    procedure
    begin
      Token.WaitFor(INFINITE);
      Cancel;
    end
  );
  MonitorThread.FreeOnTerminate := True;
  FRegistrations.Add(MonitorThread);
  MonitorThread.Start;
end;

{ TShutdownHook }

class constructor TShutdownHook.Create;
begin
  PresCtlCMsg := 'Press Ctrl+C to exit...';
  FEvent := TEvent.Create(nil, True, False, '');
  CreateHook;
end;

class destructor TShutdownHook.Destroy;
begin
  FEvent.Free;
end;

class procedure TShutdownHook.DoSignalHandled;
begin
  FCtrlHandlerEntered := True;
  FTerminated := True;
  FEvent.SetEvent;
  if Assigned(OnShutdownRequested) then
    OnShutdownRequested;
end;

{$IFDEF MSWINDOWS}
class function TShutdownHook.WinHandleSignal(dwCtrlType: DWORD): BOOL;
begin
  if (dwCtrlType = CTRL_C_EVENT) and not FCtrlHandlerEntered then
    DoSignalHandled;
  Result := True;
end;
{$ENDIF}

{$IFDEF POSIX}
class procedure TShutdownHook.HandleSignal(SigNum: Integer);
begin
  if not FCtrlHandlerEntered then
  begin
    case SigNum of
      SIGINT, SIGTERM: DoSignalHandled;
    end;
  end;
end;
{$ENDIF}

class procedure TShutdownHook.CreateHook;
begin
  {$IFDEF MSWINDOWS}
  SetConsoleCtrlHandler(@WinHandleSignal, True);
  {$ENDIF}
  {$IFDEF POSIX}
  fpSignal(SIGINT, @HandleSignal);
  fpSignal(SIGTERM, @HandleSignal);
  {$ENDIF}
end;

class procedure TShutdownHook.WaitForShutdown;
begin
  Writeln(PresCtlCMsg);
  FEvent.WaitFor(INFINITE);
end;

{ TRunQueue }

class operator TRunQueue.Finalize(var Dest: TRunQueue);
begin
  Dest.FTasks := nil;
  Dest.FTThreadPool.Free;
end;

constructor TRunQueue.Create(const MaxSize: Integer);
begin
  FTThreadPool := TThreadPool.Create;
  FTThreadPool.MaxWorkerThreads := MaxSize;
end;

procedure TRunQueue.WaitForAll;
begin
  TTask.WaitForAll(FTasks);
end;

function TRunQueue.Run(const Runnable: IRunnable): TProc;
begin
  Result := procedure
            begin
              Runnable.Run;
            end
end;

procedure TRunQueue.Start;
begin
  for var T in FTasks do T.Start;
end;

procedure TRunQueue.Add(const Task: ITask);
begin
  SetLength(FTasks, 1 + Length(FTasks));
  FTasks[Length(FTasks) - 1] := Task;
end;

procedure TRunQueue.Add(const Runnable: IRunnable);
begin
  Add(TTask.Create(Run(Runnable), FTThreadPool));
end;

{ TRunnable }

procedure TRunnable.AfterRun;
begin

end;

procedure TRunnable.BeforeRun;
begin

end;

procedure TRunnable.DoException(const E: Exception);
begin

end;

procedure TRunnable.DoRun;
begin

end;

procedure TRunnable.Run;
begin
  try
    BeforeRun;
    DoRun;
  except
    on E: Exception do
        DoException(E);
  end;
    AfterRun;
end;

end.

