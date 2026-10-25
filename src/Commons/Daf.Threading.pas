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
    // Despierta al hilo antes de su plazo: es lo que lo para.
    FWake: TEvent;
    FOnTimer: TTimerProc;
    procedure StartTimerThread;
    function DetachTimerThread: TThread;
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
    class var FLock: TCriticalSection;
    class var FOnShutdownRequested: TProc;
    // A request made while nobody listened, waiting for the next one who subscribes.
    class var FPending: Boolean;
    class procedure SetOnShutdownRequested(const Value: TProc); static;
    {$IFDEF MSWINDOWS}
    class function WinHandleSignal(dwCtrlType: DWORD): BOOL; static; stdcall;
    {$ENDIF}
    {$IFDEF POSIX}
    class procedure HandleSignal(SigNum: Integer); static;
    {$ENDIF}
  public
    // You can translate this message on app start
    class var PresCtlCMsg: string;
    class constructor Create;
    class destructor Destroy;
    class procedure WaitForShutdown;
    class property Terminated: Boolean read FTerminated;
    /// <summary>
    ///   Who hears a shutdown request. A request made while nobody listened is delivered to the
    ///   next listener on assignment, once.
    /// </summary>
    class property OnShutdownRequested: TProc read FOnShutdownRequested write SetOnShutdownRequested;
    /// <summary>Asks for the shutdown the way the console signal does.</summary>
    class procedure RequestShutdown; static;
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

function CreateCancellationTokenSource: ICancellationTokenSource;
begin
  Result := TCancellationTokenSource.Create;
end;


{ TThreadTimer }

constructor TThreadTimer.Create(TimeoutMs: Cardinal; const OnTimer: TTimerProc);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FWake := TEvent.Create(nil, True, False, '');
  FInterval := TimeoutMs;
  FOnTimer := OnTimer;
end;

// Espera a un hilo ya despertado y lo libera, fuera del cerrojo: dentro, el hilo no podría tomarlo
// para decidir si dispara. Desde el propio hilo —un callback que para el temporizador— no puede
// esperarse a sí mismo, así que el hilo se libera solo al terminar.
procedure JoinTimerThread(Thread: TThread);
begin
  if Thread = nil then
    Exit;
  if Thread.ThreadID = TThread.CurrentThread.ThreadID then
    Thread.FreeOnTerminate := True
  else
  begin
    Thread.WaitFor;
    Thread.Free;
  end;
end;

destructor TThreadTimer.Destroy;
begin
  // Parar espera al hilo: al volver, nada del temporizador se ejecuta ya.
  SetEnabled(False);
  FWake.Free;
  FLock.Free;
  inherited;
end;

// Con FLock tomado y sin hilo vivo, que es lo que garantiza que nadie espera ya en FWake.
procedure TThreadTimer.StartTimerThread;
begin
  if Assigned(FThread) then
    Exit;
  FWake.ResetEvent;
  var Wake := FWake;
  var Interval := FInterval;
  FThread := TThread.CreateAnonymousThread(
    procedure
    var
      Fire: Boolean;
    begin
      if Wake.WaitFor(Interval) <> wrTimeout then
        Exit;
      FLock.Enter;
      try
        Fire := FEnabled and Assigned(FOnTimer);
      finally
        FLock.Leave;
      end;
      // El callback puede parar o liberar el temporizador: después ya no se toca nada suyo.
      if Fire then
        FOnTimer();
    end);
  FThread.FreeOnTerminate := False;
  FThread.Start;
end;

// Con FLock tomado: despierta al hilo y lo entrega para esperarlo fuera; nil si no hay.
function TThreadTimer.DetachTimerThread: TThread;
begin
  Result := FThread;
  FThread := nil;
  if Assigned(Result) then
    FWake.SetEvent;
end;

procedure TThreadTimer.SetEnabled(Value: Boolean);
var
  Stopped: TThread;
begin
  Stopped := nil;
  FLock.Enter;
  try
    if FEnabled = Value then
      Exit;
    FEnabled := Value;
    if FEnabled then
      StartTimerThread
    else
      Stopped := DetachTimerThread;
  finally
    FLock.Leave;
  end;
  JoinTimerThread(Stopped);
end;

procedure TThreadTimer.SetInterval(Value: Cardinal);
var
  Stopped: TThread;
  Restart: Boolean;
begin
  FLock.Enter;
  try
    FInterval := Value;
    Restart := FEnabled;
    Stopped := DetachTimerThread;
  finally
    FLock.Leave;
  end;
  // El hilo viejo termina antes de que el nuevo vuelva a armar FWake.
  JoinTimerThread(Stopped);
  if Restart then
  begin
    FLock.Enter;
    try
      if FEnabled then
        StartTimerThread;
    finally
      FLock.Leave;
    end;
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

// Como Cancel() de .NET: los callbacks se sacan de la lista bajo el cerrojo, así que ninguno corre dos
// veces, y se ejecutan fuera de él, del último registrado al primero. Uno que falla no deja sin ejecutar
// a los demás: sus excepciones salen juntas en una EAggregateException, también si es una sola.
procedure TCancellationTokenSource.Cancel;
var
  Pending: TArray<TProc>;
  Errors: TArray<Exception>;
begin
  TMonitor.Enter(FCallbacks);
  try
    FEvent.SetEvent;
    Pending := FCallbacks.ToArray;
    FCallbacks.Clear;
  finally
    TMonitor.Exit(FCallbacks);
  end;
  Errors := nil;
  for var I := High(Pending) downto 0 do
    try
      Pending[I]();
    except
      Errors := Errors + [Exception(AcquireExceptionObject)];
    end;
  if Errors <> nil then
    raise EAggregateException.Create(Errors);
end;

function TCancellationTokenSource.IsCancellationRequested: Boolean;
begin
  Result := FEvent.WaitFor(0) = wrSignaled;
end;

// Mirar el estado y añadir comparten el cerrojo de Cancel: un callback registrado a la vez que se
// cancela lo ejecuta la cancelación o, si llegó tarde, el registro, y nunca ninguno de los dos.
procedure TCancellationTokenSource.Register(const Callback: TProc);
var
  AlreadyCanceled: Boolean;
begin
  TMonitor.Enter(FCallbacks);
  try
    AlreadyCanceled := IsCancellationRequested;
    if not AlreadyCanceled then
      FCallbacks.Add(Callback);
  finally
    TMonitor.Exit(FCallbacks);
  end;
  if AlreadyCanceled then
    Callback();
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

// El temporizador se para antes de cancelar: un callback de la cancelación puede liberar la fuente, y
// después de Cancel ya no se toca nada suyo.
procedure TTimeoutCancellationSource.OnTimer;
begin
  FTimer.Enabled := False;
  Cancel;
end;

{ TShutdownHook }

class constructor TShutdownHook.Create;
begin
  PresCtlCMsg := 'Press Ctrl+C to exit...';
  FEvent := TEvent.Create(nil, True, False, '');
  FLock := TCriticalSection.Create;
  CreateHook;
end;

class destructor TShutdownHook.Destroy;
begin
  FLock.Free;
  FEvent.Free;
end;

class procedure TShutdownHook.SetOnShutdownRequested(const Value: TProc);
begin
  var Deliver := False;
  FLock.Acquire;
  try
    FOnShutdownRequested := Value;
    Deliver := Assigned(Value) and FPending;
    if Deliver then
      FPending := False;
  finally
    FLock.Release;
  end;
  // Outside the lock, as in RequestShutdown: the listener runs whatever hangs on the shutdown.
  if Deliver then
    Value();
end;

class procedure TShutdownHook.RequestShutdown;
var
  Listener: TProc;
begin
  // The signal arrives on a thread the system creates, so reading the listener and leaving the
  // request pending share the lock with the assignment: otherwise a request that found nobody an
  // instant before somebody subscribed would be lost.
  FLock.Acquire;
  try
    FTerminated := True;
    FEvent.SetEvent;
    Listener := FOnShutdownRequested;
    if not Assigned(Listener) then
      FPending := True;
  finally
    FLock.Release;
  end;
  if Assigned(Listener) then
    Listener();
end;

{$IFDEF MSWINDOWS}
class function TShutdownHook.WinHandleSignal(dwCtrlType: DWORD): BOOL;
begin
  if (dwCtrlType = CTRL_C_EVENT) and not FCtrlHandlerEntered then
  begin
    FCtrlHandlerEntered := True;
    RequestShutdown;
  end;
  Result := True;
end;
{$ENDIF}

{$IFDEF POSIX}
class procedure TShutdownHook.HandleSignal(SigNum: Integer);
begin
  if not FCtrlHandlerEntered then
  begin
    case SigNum of
      SIGINT, SIGTERM:
        begin
          FCtrlHandlerEntered := True;
          RequestShutdown;
        end;
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

