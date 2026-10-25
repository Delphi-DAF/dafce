unit Daf.SystemProcess;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Threading,
  System.TimeSpan,
  System.Diagnostics,
  System.SyncObjs,
  Winapi.Windows,
  Winapi.Messages,
  Daf.Threading;

type

{$SCOPEDENUMS ON}
  TProcessResult = record
  public type
    /// <summary>
    ///   <c>Timeout</c> es que venció el plazo; <c>Killed</c>, que alguien llamó a <c>Kill</c>.
    ///   Los dos terminan con el hijo muerto, y quien lee el resultado necesita separarlos.
    /// </summary>
    TStatus = (Failed, Timeout, Running, Completed, Canceled, Killed);
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

{$SCOPEDENUMS ON}
  /// <summary>
  ///   Adónde va un flujo de salida del hijo. El valor por defecto es <c>Inherit</c>.
  /// </summary>
  TProcessOutput = record
  public type
    TKind = (Inherit, Discard, Capture, ToFile);
  private
    FKind: TKind;
    FProc: TProcessOutputProc;
    FPath: string;
    FAppend: Boolean;
  public
    /// <summary>El handle estándar que el padre tenga al lanzar al hijo.</summary>
    class function Inherit: TProcessOutput; static;
    /// <summary>El dispositivo nulo.</summary>
    class function Discard: TProcessOutput; static;
    /// <summary>Una tubería que se lee mientras el hijo vive; lo leído llega a <c>Proc</c>.</summary>
    class function Capture(const Proc: TProcessOutputProc): TProcessOutput; static;
    /// <summary>
    ///   Un fichero, con la ruta resuelta contra el directorio actual del padre. Sin <c>Append</c>
    ///   se vacía al abrirlo.
    /// </summary>
    class function ToFile(const Path: string; Append: Boolean = False): TProcessOutput; static;
    property Kind: TKind read FKind;
    property Proc: TProcessOutputProc read FProc;
    property Path: string read FPath;
    property Append: Boolean read FAppend;
  end;
{$SCOPEDENUMS OFF}

  TEnvironmentVariables = TDictionary<string, string>;

  ISystemProcess = interface(IInvokable)
  ['{176C16BB-AFB7-4425-811D-051590125272}']
    function GetId: Cardinal;
    function GetIsRunning: Boolean;
    function GetCommandLine: string;
    function GetWorkingDirectory: string;
    function GetTimeoutMS: Cardinal;
    function GetHideWindow: Boolean;
    function GetStandardOutput: TProcessOutput;
    function GetStandardError: TProcessOutput;
    function GetStandardOutputEncoding: TEncoding;
    function GetStandardErrorEncoding: TEncoding;
    function GetOnStdOut: TProcessOutputProc;
    function GetOnStdErr: TProcessOutputProc;
    function GetOnIdle: TProcessNotifyProc;
    function GetOnCancelled: TProcessNotifyProc;
    function GetOnFailed: TProcessNotifyProc;
    function GetOnKilled: TProcessNotifyProc;
    function GetOnCompleted: TProcessNotifyProc;

    function Execute(CancellationToken: ICancellationToken = nil): TProcessResult;
    /// <summary>
    ///   Lanza al hijo antes de volver, en el hilo de quien llama, y devuelve la tarea que espera a
    ///   que termine. Si el lanzamiento falla, la tarea ya está resuelta con <c>Failed</c> y
    ///   <c>OnFailed</c> ha corrido en el hilo de quien llama.
    /// </summary>
    function ExecuteAsync(const CancellationToken: ICancellationToken = nil): IFuture<TProcessResult>;
    procedure Kill;
    /// <summary>
    ///   El identificador que el sistema dio al hijo, desde que se lanza y también después de que
    ///   termine. Sin un hijo lanzado —antes de ejecutar, o si el lanzamiento falló— lanza
    ///   <c>EInvalidOpException</c>.
    /// </summary>
    property Id: Cardinal read GetId;
    property IsRunning: Boolean read GetIsRunning;
    property CommandLine: string read GetCommandLine;
    property WorkingDirectory: string read GetWorkingDirectory;
    property TimeoutMS: Cardinal read GetTimeoutMS;
    property HideWindow: Boolean read GetHideWindow;
    property StandardOutput: TProcessOutput read GetStandardOutput;
    property StandardError: TProcessOutput read GetStandardError;
    /// <summary>
    ///   Con qué se decodifica la salida estándar capturada. <c>nil</c> es la página de la consola en que
    ///   corre el hijo: la de salida del padre si la comparte, la OEM del sistema si tiene una propia.
    /// </summary>
    property StandardOutputEncoding: TEncoding read GetStandardOutputEncoding;
    /// <summary>Lo mismo que <c>StandardOutputEncoding</c>, para la salida de error.</summary>
    property StandardErrorEncoding: TEncoding read GetStandardErrorEncoding;
    /// <summary>El manejador de la salida estándar cuando se captura; <c>nil</c> si no.</summary>
    property OnStdOut: TProcessOutputProc read GetOnStdOut;
    /// <summary>El manejador de la salida de error cuando se captura; <c>nil</c> si no.</summary>
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
    procedure Declare(var Slot: TProcessOutput; var Declared: Boolean; const Value: TProcessOutput;
      const Stream: string);
  public
    constructor Create;
    function Command(const Value: string): TBuilder;
    function CmdArgs(const Args: array of string): TBuilder;
    function WorkingDir(const Value: string): TBuilder;
    function Timeout(Milliseconds: Cardinal): TBuilder;
    function HideWindow(Value: Boolean = True): TBuilder;
    /// <summary>
    ///   El destino de la salida estándar. Un flujo admite una sola declaración: la segunda lanza
    ///   <c>EInvalidOpException</c>.
    /// </summary>
    function StandardOutput(const Value: TProcessOutput): TBuilder;
    /// <summary>El destino de la salida de error, con la misma regla que <c>StandardOutput</c>.</summary>
    function StandardError(const Value: TProcessOutput): TBuilder;
    /// <summary>
    ///   La codificación de la salida estándar capturada. El proceso no la libera: <c>TEncoding.UTF8</c> y
    ///   las demás estándar son de la RTL.
    /// </summary>
    function StandardOutputEncoding(Value: TEncoding): TBuilder;
    /// <summary>La codificación de la salida de error capturada, con la misma regla.</summary>
    function StandardErrorEncoding(Value: TEncoding): TBuilder;
    /// <summary>Equivale a <c>StandardOutput(TProcessOutput.Capture(Proc))</c>.</summary>
    function OnStdOut(Proc: TProcessOutputProc): TBuilder;
    /// <summary>Equivale a <c>StandardError(TProcessOutput.Capture(Proc))</c>.</summary>
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
    FHideWindow: Boolean;
    FCustomEnvVars: TEnvironmentVariables;

    FStdOut: TProcessOutput;
    FStdErr: TProcessOutput;
    FStdOutDeclared: Boolean;
    FStdErrDeclared: Boolean;
    FStdOutEncoding: TEncoding;
    FStdErrEncoding: TEncoding;
    FConsoleEncoding: TEncoding;

    FOnIdle: TProcessNotifyProc;
    FOnCompleted: TProcessNotifyProc;
    FOnFailed: TProcessNotifyProc;
    FOnCancelled: TProcessNotifyProc;
    FOnKilled: TProcessNotifyProc;

    FStopWatch: TStopwatch;
    // El proceso del último hijo se conserva hasta el siguiente lanzamiento o la destrucción, para
    // que Kill actúe sobre él y su identificador no pase a otro proceso.
    FProcessInfo: TProcessInformation;
    // Cubre matar al hijo, preguntar si corre y decidir el resultado de la espera.
    FLock: TCriticalSection;
    FLaunchError: DWORD;
    // Lo que recibe el hijo, y el extremo de lectura de cada flujo que se captura.
    FChildIn, FChildOut, FChildErr: THandle;
    FStdOutRead, FStdErrRead: THandle;
    [volatile] FKillRequested: Boolean;
    // Pide a los lectores que entreguen lo que queda en su tubería y paren.
    [volatile] FStopReading: Boolean;
    FReadTasks: array of ITask;
    procedure CleanupHandles;
    function OpenStreams(var ProcessResult: TProcessResult): Boolean;
    function OpenOutput(const Output: TProcessOutput; StdHandle: DWORD; out Child, ReadEnd: THandle;
      out ErrorCode: DWORD; out Detail: string): Boolean;
    procedure CloseChildHandles;
    procedure ClosePipes;
    procedure SetupStartupInfo(var SI: TStartupInfo);
    procedure StartReadingPipes;
    procedure WaitForReaders;
    procedure StopReaders;
    // Con el hijo terminado o cortado, espera a que se cierren sus flujos capturados.
    procedure WaitForCapture(CancellationToken: ICancellationToken; var ProcessResult: TProcessResult);

    procedure DoOutput(const Text: string; IsError: Boolean);
    // El hijo terminó: Killed si lo mató Kill, Completed si no, con su código de salida.
    procedure DoProcessEnded(var Result: TProcessResult);
    procedure DoProcessFailed(var Result: TProcessResult; ErrorCode: DWORD; const Detail: string = '');
    // La cancelación o el plazo cortan al hijo; si ya había terminado, el resultado es el suyo.
    procedure DoProcessCut(var Result: TProcessResult; Status: TProcessResult.TStatus);
    procedure DoProcessIdle(var Result: TProcessResult);

    function TryCreateProcess(var ProcessResult: TProcessResult): Boolean;
    // Deja al hijo corriendo y sus flujos leyéndose. Si no lo consigue, devuelve False con el
    // resultado en Failed, OnFailed ya llamado y todo lo abierto cerrado.
    function Launch(var ProcessResult: TProcessResult): Boolean;
    // Espera al hijo que dejó Launch y cierra lo que quedaba abierto.
    procedure WaitForChild(CancellationToken: ICancellationToken; var ProcessResult: TProcessResult);
    procedure Release;
    function GetId: Cardinal;
    function GetIsRunning: Boolean;
    procedure BuildEnvBuffer(var EnvBuffer: PWideChar);
    function GetCommandLine: string;
    function GetWorkingDirectory: string;
    function GetTimeoutMS: Cardinal;
    function GetHideWindow: Boolean;
    function GetStandardOutput: TProcessOutput;
    function GetStandardError: TProcessOutput;
    function GetStandardOutputEncoding: TEncoding;
    function GetStandardErrorEncoding: TEncoding;
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

    property Id: Cardinal read GetId;
    property IsRunning: Boolean read GetIsRunning;
    property CommandLine: string read GetCommandLine;
    property WorkingDirectory: string read GetWorkingDirectory;
    property TimeoutMS: Cardinal read GetTimeoutMS;
    property HideWindow: Boolean read GetHideWindow;
    property StandardOutput: TProcessOutput read GetStandardOutput;
    property StandardError: TProcessOutput read GetStandardError;
    property StandardOutputEncoding: TEncoding read GetStandardOutputEncoding;
    property StandardErrorEncoding: TEncoding read GetStandardErrorEncoding;
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
  System.IOUtils,
  Daf.Types;

function IsValidHandle(Handle: THandle): Boolean;
begin
  Result := (Handle <> 0) and (Handle <> INVALID_HANDLE_VALUE);
end;

procedure CloseValidHandle(var Handle: THandle);
begin
  if IsValidHandle(Handle) then
    CloseHandle(Handle);
  Handle := 0;
end;

function CancelPendingIo(hFile: THandle; lpOverlapped: POverlapped): BOOL; stdcall;
  external kernel32 name 'CancelIoEx';

const
  PROCESS_QUERY_LIMITED_INFORMATION = $1000;
  // El código de salida con que .NET termina un proceso.
  KilledExitCode = $FFFFFFFF;

type
  TTerminateOutcome = (toTerminated, toAlreadyExited, toFailed);

// Termina un proceso por su handle con el código -1. Un proceso que ya terminó no se mata:
// TerminateProcess falla con acceso denegado, y su código de salida ya no es STILL_ACTIVE.
function TerminateByHandle(Handle: THandle; out ErrorCode: DWORD): TTerminateOutcome;
var
  Code: DWORD;
begin
  ErrorCode := 0;
  if TerminateProcess(Handle, KilledExitCode) then
    Exit(toTerminated);
  ErrorCode := GetLastError;
  if (ErrorCode = ERROR_ACCESS_DENIED) and GetExitCodeProcess(Handle, Code) and (Code <> STILL_ACTIVE) then
    Exit(toAlreadyExited);
  Result := toFailed;
end;

var
  // Mientras los handles de un hijo son heredables, ningún otro lanzamiento crea su proceso: un hijo
  // lanzado en ese momento heredaría la tubería o el fichero de otro, y los retendría mientras viva.
  LaunchLock: TCriticalSection;

// Los handles del hijo nacen no heredables; solo lo son mientras se crea su proceso.
function ChildAttributes: TSecurityAttributes;
begin
  Result.nLength := SizeOf(Result);
  Result.bInheritHandle := False;
  Result.lpSecurityDescriptor := nil;
end;

// Un handle que el padre no tiene llega al hijo tal cual: si el hijo tiene consola propia, el sistema
// pone en ese flujo el de ella.
function DuplicateForChild(Handle: THandle; out Duplicate: THandle): Boolean;
begin
  if not IsValidHandle(Handle) then
  begin
    Duplicate := Handle;
    Exit(True);
  end;
  Result := DuplicateHandle(GetCurrentProcess, Handle, GetCurrentProcess, @Duplicate, 0, False,
    DUPLICATE_SAME_ACCESS);
end;

procedure MakeInheritable(Handle: THandle);
begin
  if IsValidHandle(Handle) then
    SetHandleInformation(Handle, HANDLE_FLAG_INHERIT, HANDLE_FLAG_INHERIT);
end;

{ TProcessResult }

function TProcessResult.Succeeded: Boolean;
begin
  Result := (Status = TStatus.Completed) and (ExitCode = 0);
end;

{ TProcessOutput }

class function TProcessOutput.Inherit: TProcessOutput;
begin
  Result := Default(TProcessOutput);
  Result.FKind := TKind.Inherit;
end;

class function TProcessOutput.Discard: TProcessOutput;
begin
  Result := Default(TProcessOutput);
  Result.FKind := TKind.Discard;
end;

class function TProcessOutput.Capture(const Proc: TProcessOutputProc): TProcessOutput;
begin
  Result := Default(TProcessOutput);
  Result.FKind := TKind.Capture;
  Result.FProc := Proc;
end;

class function TProcessOutput.ToFile(const Path: string; Append: Boolean): TProcessOutput;
begin
  Result := Default(TProcessOutput);
  Result.FKind := TKind.ToFile;
  Result.FPath := Path;
  Result.FAppend := Append;
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
var
  ErrorCode: DWORD;
begin
  var hProcess: THandle := OpenProcess(PROCESS_TERMINATE or SYNCHRONIZE or PROCESS_QUERY_LIMITED_INFORMATION,
    False, PID);
  if hProcess = 0 then
    Exit(False);
  Result := False;
  try
    case TerminateByHandle(hProcess, ErrorCode) of
      toTerminated:
        Result := WaitForSingleObject(hProcess, TimeoutMS) = WAIT_OBJECT_0;
      toFailed:
        RaiseLastOSError(ErrorCode);
    end;
  finally
    CloseHandle(hProcess);
  end;
end;

{ TBuilder }

constructor TBuilder.Create;
begin
  inherited Create;
  FInstance := TSystemProcess.Create;
end;

procedure TBuilder.Declare(var Slot: TProcessOutput; var Declared: Boolean; const Value: TProcessOutput;
  const Stream: string);
begin
  if Declared then
  begin
    // Una cadena fluida que falla no deja a nadie que libere el builder ni el proceso que declaraba.
    FInstance.Free;
    Free;
    raise EInvalidOpException.CreateFmt('%s already has a destination: a stream takes exactly one', [Stream]);
  end;
  Slot := Value;
  Declared := True;
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

function TBuilder.StandardOutput(const Value: TProcessOutput): TBuilder;
begin
  Declare(FInstance.FStdOut, FInstance.FStdOutDeclared, Value, 'StandardOutput');
  Result := Self;
end;

function TBuilder.StandardError(const Value: TProcessOutput): TBuilder;
begin
  Declare(FInstance.FStdErr, FInstance.FStdErrDeclared, Value, 'StandardError');
  Result := Self;
end;

function TBuilder.StandardOutputEncoding(Value: TEncoding): TBuilder;
begin
  FInstance.FStdOutEncoding := Value;
  Result := Self;
end;

function TBuilder.StandardErrorEncoding(Value: TEncoding): TBuilder;
begin
  FInstance.FStdErrEncoding := Value;
  Result := Self;
end;

function TBuilder.OnStdOut(Proc: TProcessOutputProc): TBuilder;
begin
  Result := StandardOutput(TProcessOutput.Capture(Proc));
end;

function TBuilder.OnStdErr(Proc: TProcessOutputProc): TBuilder;
begin
  Result := StandardError(TProcessOutput.Capture(Proc));
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
    Free;
  end;
end;

procedure TBuilder.ExecuteAsyncAndFree(const Token: ICancellationToken = nil);
begin
  // Execute y no ExecuteAsync: esta tarea es la que tiene que esperar al hijo, porque libera la
  // instancia al acabar.
  TTask.Run(
    procedure
    begin
      try
        FInstance.Execute(Token);
      finally
        FInstance.Free;
        Free;
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
  FCustomEnvVars := TEnvironmentVariables.Create;
  FLock := TCriticalSection.Create;
end;

destructor TSystemProcess.Destroy;
begin
  FCustomEnvVars.Free;
  CloseChildHandles;
  ClosePipes;
  CleanupHandles;
  FreeAndNil(FConsoleEncoding);
  FLock.Free;
  inherited;
end;

procedure TSystemProcess.DoOutput(const Text: string; IsError: Boolean);
begin
  if IsError then
  begin
    if Assigned(FStdErr.Proc) then
      FStdErr.Proc(Text);
  end
  else
  begin
    if Assigned(FStdOut.Proc) then
      FStdOut.Proc(Text);
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

// Los dos modos escriben con FILE_APPEND_DATA: dos flujos dirigidos al mismo fichero se intercalan en
// vez de pisarse. Sin Append, el fichero se vacía antes.
function OpenFile(const Output: TProcessOutput; SA: PSecurityAttributes; out Child: THandle;
  out Path: string): Boolean;
begin
  Child := 0;
  Path := Output.Path;
  try
    Path := TPath.GetFullPath(Output.Path);
  except
    on EArgumentException do
    begin
      SetLastError(ERROR_INVALID_NAME);
      Exit(False);
    end;
  end;
  if not Output.Append then
  begin
    var Truncated := CreateFile(PChar(Path), GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
      CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
    if Truncated = INVALID_HANDLE_VALUE then
      Exit(False);
    CloseHandle(Truncated);
  end;
  Child := CreateFile(PChar(Path), FILE_APPEND_DATA or SYNCHRONIZE, FILE_SHARE_READ or FILE_SHARE_WRITE, SA,
    OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
  Result := Child <> INVALID_HANDLE_VALUE;
  if not Result then
    Child := 0;
end;

function TSystemProcess.OpenOutput(const Output: TProcessOutput; StdHandle: DWORD; out Child, ReadEnd: THandle;
  out ErrorCode: DWORD; out Detail: string): Boolean;
begin
  Child := 0;
  ReadEnd := 0;
  ErrorCode := 0;
  Detail := '';
  var SA := ChildAttributes;
  case Output.Kind of
    TProcessOutput.TKind.Inherit:
      Result := DuplicateForChild(GetStdHandle(StdHandle), Child);
    TProcessOutput.TKind.Discard:
      begin
        Child := CreateFile('NUL', GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, @SA, OPEN_EXISTING, 0, 0);
        Result := Child <> INVALID_HANDLE_VALUE;
        if not Result then
          Child := 0;
      end;
    TProcessOutput.TKind.Capture:
      begin
        Result := CreatePipe(ReadEnd, Child, @SA, 0);
      end;
    TProcessOutput.TKind.ToFile:
      Result := OpenFile(Output, @SA, Child, Detail);
  else
    Result := False;
  end;
  if Result then
    Detail := ''
  else
    ErrorCode := GetLastError;
end;

function TSystemProcess.OpenStreams(var ProcessResult: TProcessResult): Boolean;
var
  ErrorCode: DWORD;
  Detail: string;
begin
  ErrorCode := 0;
  Detail := '';
  Result := DuplicateForChild(GetStdHandle(STD_INPUT_HANDLE), FChildIn);
  if not Result then
    ErrorCode := GetLastError;
  if Result then
    Result := OpenOutput(FStdOut, STD_OUTPUT_HANDLE, FChildOut, FStdOutRead, ErrorCode, Detail);
  if Result then
    Result := OpenOutput(FStdErr, STD_ERROR_HANDLE, FChildErr, FStdErrRead, ErrorCode, Detail);
  if not Result then
  begin
    CloseChildHandles;
    ClosePipes;
    DoProcessFailed(ProcessResult, ErrorCode, Detail);
  end;
end;

procedure TSystemProcess.CloseChildHandles;
begin
  CloseValidHandle(FChildIn);
  CloseValidHandle(FChildOut);
  CloseValidHandle(FChildErr);
end;

procedure TSystemProcess.ClosePipes;
begin
  WaitForReaders;
  CloseValidHandle(FStdOutRead);
  CloseValidHandle(FStdErrRead);
end;

procedure TSystemProcess.SetupStartupInfo(var SI: TStartupInfo);
begin
  FillChar(SI, SizeOf(SI), 0);
  SI.cb := SizeOf(SI);
  // Los tres siempre: sin STARTF_USESTDHANDLES el hijo usaría los de su propia consola, y heredar
  // dependería de si la tiene.
  SI.dwFlags := STARTF_USESTDHANDLES;
  SI.hStdInput := FChildIn;
  SI.hStdOutput := FChildOut;
  SI.hStdError := FChildErr;
end;

// Cuántos bytes del principio de Data forman caracteres completos en Encoding. Los que quedan son el
// principio de un carácter que termina la lectura siguiente.
function CompleteLength(Encoding: TEncoding; const Data: TBytes): Integer;
var
  CodePage: Cardinal;
  Info: TCPInfo;
  I, Back, Need: Integer;
  W: Word;
begin
  Result := Length(Data);
  CodePage := Encoding.CodePage;
  case CodePage of
    CP_UTF8:
      begin
        I := Result - 1;
        Back := 0;
        while (I >= 0) and (Back < 3) and ((Data[I] and $C0) = $80) do
        begin
          Dec(I);
          Inc(Back);
        end;
        if I < 0 then
          Exit;
        if Data[I] >= $F0 then
          Need := 4
        else if Data[I] >= $E0 then
          Need := 3
        else if Data[I] >= $C0 then
          Need := 2
        else
          Need := 1;
        if Result - I < Need then
          Result := I;
      end;
    1200, 1201:
      begin
        Result := Result and not 1;
        if Result >= 2 then
        begin
          if CodePage = 1200 then
            W := Data[Result - 2] or (Data[Result - 1] shl 8)
          else
            W := (Data[Result - 2] shl 8) or Data[Result - 1];
          if (W >= $D800) and (W <= $DBFF) then
            Dec(Result, 2);
        end;
      end;
  else
    if GetCPInfo(CodePage, Info) and (Info.MaxCharSize = 2) then
    begin
      I := 0;
      while I < Length(Data) do
        if IsDBCSLeadByteEx(CodePage, Data[I]) then
        begin
          if I + 1 >= Length(Data) then
            Exit(I);
          Inc(I, 2);
        end
        else
          Inc(I);
    end;
  end;
end;

// Un byte que no encaja en la página se sustituye, y no lanza: un lector que lanza deja sin leer su
// tubería.
function Decode(Encoding: TEncoding; const Data: TBytes; Count: Integer): string;
begin
  try
    Result := Encoding.GetString(Data, 0, Count);
  except
    on EEncodingError do
    begin
      var Lenient := TMBCSEncoding.Create(Encoding.CodePage, 0, 0);
      try
        Result := Lenient.GetString(Data, 0, Count);
      finally
        Lenient.Free;
      end;
    end;
  end;
end;

procedure TSystemProcess.StartReadingPipes;
  function StartReader(Pipe: THandle; IsError: Boolean; Encoding: TEncoding): ITask;
  const
    BUF_SIZE = 2048;
  begin
    Result := TTask.Run(
      procedure
      var
        BytesRead, Available: DWORD;
        Buffer, Data, Pending: TBytes;
        Complete: Integer;

        procedure Deliver;
        begin
          Data := Pending + Copy(Buffer, 0, BytesRead);
          Complete := CompleteLength(Encoding, Data);
          if Complete > 0 then
            DoOutput(Decode(Encoding, Data, Complete), IsError);
          Pending := Copy(Data, Complete, Length(Data) - Complete);
        end;

      begin
        SetLength(Buffer, BUF_SIZE);
        while True do
        begin
          if FStopReading then
          begin
            // Lo que ya está en la tubería se entrega; lo que llegue después, no.
            while PeekNamedPipe(Pipe, nil, 0, nil, @Available, nil) and (Available > 0)
              and ReadFile(Pipe, Buffer[0], BUF_SIZE, BytesRead, nil) and (BytesRead > 0) do
              Deliver;
            Break;
          end;
          if ReadFile(Pipe, Buffer[0], BUF_SIZE, BytesRead, nil) and (BytesRead > 0) then
            Deliver
          else if GetLastError <> ERROR_OPERATION_ABORTED then
            Break;
        end;
        if Length(Pending) > 0 then
          DoOutput(Decode(Encoding, Pending, Length(Pending)), IsError);
      end);
  end;

  function EncodingOf(Declared: TEncoding): TEncoding;
  begin
    if Assigned(Declared) then
      Result := Declared
    else
      Result := FConsoleEncoding;
  end;
begin
  FStopReading := False;
  if IsValidHandle(FStdOutRead) or IsValidHandle(FStdErrRead) then
  begin
    // La página de la consola en que corre el hijo: la del padre si la comparte, y la OEM si tiene una
    // propia, que es lo que pasa oculto o cuando el padre no tiene consola.
    var CodePage := GetConsoleOutputCP;
    if FHideWindow or (CodePage = 0) then
      CodePage := GetOEMCP;
    FConsoleEncoding := TMBCSEncoding.Create(CodePage, 0, 0);
  end;

  if IsValidHandle(FStdOutRead) then
  begin
    SetLength(FReadTasks, 1 + Length(FReadTasks));
    FReadTasks[Length(FReadTasks) - 1] := StartReader(FStdOutRead, False, EncodingOf(FStdOutEncoding));
  end;

  if IsValidHandle(FStdErrRead) then
  begin
    SetLength(FReadTasks, 1 + Length(FReadTasks));
    FReadTasks[Length(FReadTasks) - 1] := StartReader(FStdErrRead, True, EncodingOf(FStdErrEncoding));
  end;
end;

procedure TSystemProcess.WaitForReaders;
begin
  TTask.WaitForAll(FReadTasks);
  SetLength(FReadTasks, 0);
end;

// Una cancelación que cae entre la mirada del lector al indicador y su ReadFile no encuentra nada
// pendiente; por eso se repite hasta que los lectores acaban.
procedure TSystemProcess.StopReaders;
begin
  FStopReading := True;
  repeat
    if IsValidHandle(FStdOutRead) then
      CancelPendingIo(FStdOutRead, nil);
    if IsValidHandle(FStdErrRead) then
      CancelPendingIo(FStdErrRead, nil);
  until TTask.WaitForAll(FReadTasks, 50);
end;

// Sin plazo ni token se espera sin límite, como WaitForExit() de .NET: un descendiente que herede un
// flujo capturado retiene la ejecución hasta que lo cierra. Con ellos, al cumplirse con un flujo aún
// abierto, la captura termina con lo que ya hay en las tuberías y el resultado es Timeout o Canceled,
// como RunAndCaptureText.
procedure TSystemProcess.WaitForCapture(CancellationToken: ICancellationToken;
  var ProcessResult: TProcessResult);
begin
  if (Length(FReadTasks) = 0) or ((FTimeoutMS = INFINITE) and (CancellationToken = nil)) then
    Exit;
  while not TTask.WaitForAll(FReadTasks, 250) do
  begin
    var Canceled := (CancellationToken <> nil) and CancellationToken.IsCancellationRequested;
    var Expired := (FTimeoutMS <> INFINITE) and (FStopWatch.ElapsedMilliseconds >= FTimeoutMS);
    if not (Canceled or Expired) then
      Continue;
    StopReaders;
    if ProcessResult.Status in [TProcessResult.TStatus.Completed, TProcessResult.TStatus.Killed] then
    begin
      if Canceled then
        ProcessResult.Status := TProcessResult.TStatus.Canceled
      else
        ProcessResult.Status := TProcessResult.TStatus.Timeout;
      ProcessResult.Duration := FStopWatch.Elapsed;
    end;
    Break;
  end;
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

procedure TSystemProcess.DoProcessEnded(var Result: TProcessResult);
var
  Killed: Boolean;
begin
  FLock.Enter;
  try
    Killed := FKillRequested;
  finally
    FLock.Leave;
  end;
  GetExitCodeProcess(FProcessInfo.hProcess, Result.ExitCode);
  Result.Duration := FStopWatch.Elapsed;
  if Killed then
  begin
    Result.Status := TProcessResult.TStatus.Killed;
    if Assigned(FOnKilled) then
      FOnKilled(Result);
  end
  else
  begin
    Result.Status := TProcessResult.TStatus.Completed;
    if Assigned(FOnCompleted) then
      FOnCompleted(Result);
  end;
end;

procedure TSystemProcess.DoProcessFailed(var Result: TProcessResult; ErrorCode: DWORD; const Detail: string);
begin
  Result.ExitCode := ErrorCode;
  Result.Status := TProcessResult.TStatus.Failed;
  Result.Duration := FStopWatch.Elapsed;
  Result.LastError := SysErrorMessage(ErrorCode);
  if Detail <> '' then
    Result.LastError := Detail + ': ' + Result.LastError;
  if Assigned(FOnFailed) then
    OnFailed(Result);
end;

procedure TSystemProcess.DoProcessCut(var Result: TProcessResult; Status: TProcessResult.TStatus);
var
  ErrorCode: DWORD;
  Outcome: TTerminateOutcome;
begin
  FLock.Enter;
  try
    Outcome := TerminateByHandle(FProcessInfo.hProcess, ErrorCode);
  finally
    FLock.Leave;
  end;
  case Outcome of
    toAlreadyExited:
      DoProcessEnded(Result);
    toFailed:
      DoProcessFailed(Result, ErrorCode);
  else
    // TerminateProcess no espera: el código de salida es el de matarlo cuando el hijo ya no corre.
    WaitForSingleObject(FProcessInfo.hProcess, 5000);
    GetExitCodeProcess(FProcessInfo.hProcess, Result.ExitCode);
    Result.Status := Status;
    Result.Duration := FStopWatch.Elapsed;
    if Status = TProcessResult.TStatus.Canceled then
    begin
      if Assigned(FOnCancelled) then
        FOnCancelled(Result);
    end
    else if Assigned(FOnKilled) then
      FOnKilled(Result);
  end;
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
  var ProcessInfo: TProcessInformation;
  FillChar(ProcessInfo, SizeOf(ProcessInfo), 0);

  var EnvBuffer: PWideChar := nil;
  BuildEnvBuffer(EnvBuffer);


  var Flags := DefaultFlags;
  if HideWindow then
  begin
    Flags := Flags or CREATE_NO_WINDOW;
    StartInfo.dwFlags := StartInfo.dwFlags or STARTF_USESHOWWINDOW;
    StartInfo.wShowWindow := SW_HIDE;
  end;

  try
    LaunchLock.Enter;
    try
      MakeInheritable(FChildIn);
      MakeInheritable(FChildOut);
      MakeInheritable(FChildErr);
      Result := CreateProcessW(nil, PWideChar(FCommandLine), nil, nil, True, Flags,
        EnvBuffer, WorkDir, StartInfo, ProcessInfo);
      if not Result then
        FLaunchError := GetLastError;
      // El hijo ya tiene los suyos; sin cerrar estos, una tubería no llegaría nunca a su fin.
      CloseChildHandles;
    finally
      LaunchLock.Leave;
    end;
    if Result then
    begin
      FLock.Enter;
      try
        FProcessInfo := ProcessInfo;
      finally
        FLock.Leave;
      end;
    end;
  finally
    FreeMem(EnvBuffer);
  end;
end;

function TSystemProcess.Launch(var ProcessResult: TProcessResult): Boolean;
begin
  FStopWatch := TStopwatch.Create;
  // Lanzar suelta el proceso del hijo anterior. Sin hijo, FProcessInfo a cero es lo que hace que Id y
  // Kill lancen: también cuando falla abrir un flujo, antes de llegar a CreateProcessW.
  FLock.Enter;
  try
    CleanupHandles;
    FillChar(FProcessInfo, SizeOf(FProcessInfo), 0);
    FKillRequested := False;
  finally
    FLock.Leave;
  end;
  Result := False;
  try
    if not OpenStreams(ProcessResult) then
      Exit;
    if not TryCreateProcess(ProcessResult) then
    begin
      DoProcessFailed(ProcessResult, FLaunchError);
      Exit;
    end;
    FStopWatch := TStopwatch.StartNew;
    ResumeThread(FProcessInfo.hThread);
    CloseHandle(FProcessInfo.hThread);
    FProcessInfo.hThread := 0;
    StartReadingPipes;
    Result := True;
  finally
    if not Result then
      Release;
  end;
end;

procedure TSystemProcess.WaitForChild(CancellationToken: ICancellationToken;
  var ProcessResult: TProcessResult);
begin
  try
    repeat
      var WaitRes := WaitForSingleObject(FProcessInfo.hProcess, 250);

      // Un hijo que ya terminó no se mata: ni la cancelación ni el plazo llegan a tiempo.
      if WaitRes = WAIT_OBJECT_0 then
      begin
        DoProcessEnded(ProcessResult);
        Break;
      end;

      if WaitRes <> WAIT_TIMEOUT then
      begin
        DoProcessFailed(ProcessResult, GetLastError);
        Break;
      end;

      if (CancellationToken <> nil) and CancellationToken.IsCancellationRequested then
      begin
        DoProcessCut(ProcessResult, TProcessResult.TStatus.Canceled);
        Break;
      end;

      if (FTimeoutMS <> INFINITE) and (FStopWatch.ElapsedMilliseconds >= FTimeoutMS) then
      begin
        DoProcessCut(ProcessResult, TProcessResult.TStatus.Timeout);
        Break;
      end;

      DoProcessIdle(ProcessResult);
    until False;
    WaitForCapture(CancellationToken, ProcessResult);
  finally
    Release;
  end;
end;

procedure TSystemProcess.Release;
begin
  FStopWatch.Stop;
  CloseChildHandles;
  try
    ClosePipes;
  finally
    FreeAndNil(FConsoleEncoding);
  end;
end;

function TSystemProcess.Execute(CancellationToken: ICancellationToken = nil): TProcessResult;
begin
  if Launch(Result) then
    WaitForChild(CancellationToken, Result);
end;

function TSystemProcess.GetCommandLine: string;
begin
  REsult := FCommandLine;
end;

function TSystemProcess.GetHideWindow: Boolean;
begin
  Result := FHideWindow;
end;

function TSystemProcess.GetId: Cardinal;
begin
  if FProcessInfo.dwProcessId = 0 then
    raise EInvalidOpException.Create('No process is associated with this object.');
  Result := FProcessInfo.dwProcessId;
end;

function TSystemProcess.GetIsRunning: Boolean;
begin
  FLock.Enter;
  try
    Result := (FProcessInfo.hProcess <> 0) and (WaitForSingleObject(FProcessInfo.hProcess, 0) = WAIT_TIMEOUT);
  finally
    FLock.Leave;
  end;
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

function TSystemProcess.GetStandardOutput: TProcessOutput;
begin
  Result := FStdOut;
end;

function TSystemProcess.GetStandardError: TProcessOutput;
begin
  Result := FStdErr;
end;

function TSystemProcess.GetStandardOutputEncoding: TEncoding;
begin
  Result := FStdOutEncoding;
end;

function TSystemProcess.GetStandardErrorEncoding: TEncoding;
begin
  Result := FStdErrEncoding;
end;

function TSystemProcess.GetOnStdErr: TProcessOutputProc;
begin
  if FStdErr.Kind = TProcessOutput.TKind.Capture then
    Result := FStdErr.Proc
  else
    Result := nil;
end;

function TSystemProcess.GetOnStdOut: TProcessOutputProc;
begin
  if FStdOut.Kind = TProcessOutput.TKind.Capture then
    Result := FStdOut.Proc
  else
    Result := nil;
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
var
  ErrorCode: DWORD;
begin
  FLock.Enter;
  try
    if FProcessInfo.hProcess = 0 then
      raise EInvalidOpException.Create('No process is associated with this object.');
    case TerminateByHandle(FProcessInfo.hProcess, ErrorCode) of
      toTerminated:
        FKillRequested := True;
      toFailed:
        RaiseLastOSError(ErrorCode);
    end;
  finally
    FLock.Leave;
  end;
end;

function TSystemProcess.ExecuteAsync(const CancellationToken: ICancellationToken = nil): IFuture<TProcessResult>;
var
  Launched: TProcessResult;
begin
  if not Launch(Launched) then
  begin
    // La RTL no tiene una tarea ya resuelta: se resuelve esta antes de devolverla.
    Result := TTask.Future<TProcessResult>(
      function: TProcessResult
      begin
        Result := Launched;
      end);
    Result.Wait;
    Exit;
  end;
  // La tarea sostiene la instancia hasta que termina la espera. Una instancia que se maneja como
  // objeto tiene la cuenta a cero, y una referencia tomada ahí la liberaría debajo de su dueño.
  var Keep: ISystemProcess := nil;
  if RefCount > 0 then
    Keep := Self;
  Result := TTask.Future<TProcessResult>(
    function: TProcessResult
    begin
      try
        Result := Launched;
        WaitForChild(CancellationToken, Result);
      finally
        Keep := nil;
      end;
    end);
end;

initialization
  LaunchLock := TCriticalSection.Create;

finalization
  LaunchLock.Free;

end.
