unit Daf.Hosting.Edge;

interface

uses
  System.SysUtils,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Hosting.Failures;

type
  /// <summary>
  /// El unico sitio donde se decide lo que se escapa de una variante dueña del proceso. Lo
  /// comparten su builder, por lo que le llega a HandleException, y su aplicacion, por lo que se
  /// escapa dentro del Run.
  /// </summary>
  /// <remarks>
  ///   Es pieza de las variantes, no de quien escribe una aplicacion: este declara la politica con
  ///   OnException y le entrega lo que se escapa antes del Run con HandleException.
  /// </remarks>
  IAppFailures = interface
    ['{6B1F0D2A-8C4E-4E77-9A35-2F0B7C1D9E48}']
    procedure SetPolicy(const Policy: TAppExceptionPolicy);
    /// <summary>
    /// Un EAbort termina con Failure; lo demas lo decide la politica. Le llega informado lo que
    /// Reported dice, y tambien lo que ya salio por el canal de arranque.
    /// </summary>
    function Decide(const E: Exception; const Services: IServiceProvider;
      const Reported: Boolean): Integer;
    /// <summary>Lo que se escapa antes del Run, o lo que la politica ya dejo salir de el.</summary>
    function Handle(const E: Exception): Integer;
  end;

  TAppFailures = class(TInterfacedObject, IAppFailures)
  private
    FPolicy: TAppExceptionPolicy;
    // Lo que la politica dejo salir, hacia el except del .dpr. Se compara por identidad y no se
    // consume: la misma excepcion puede volver mas de una vez, desde el gancho del RTL, a traves
    // del Run y hasta el except del .dpr.
    FLetOut: TObject;
  public
    /// <summary>Con la politica por defecto, ExitPolicy.LogAndFail.</summary>
    constructor Create;
    procedure SetPolicy(const Policy: TAppExceptionPolicy);
    function Decide(const E: Exception; const Services: IServiceProvider;
      const Reported: Boolean): Integer;
    function Handle(const E: Exception): Integer;
  end;

  /// <summary>
  /// El cerrojo de instancia unica: el mismo identificador no puede tenerlo mas de un proceso a la
  /// vez en la maquina.
  /// </summary>
  TInstanceLock = record
  public
    /// <summary>
    /// Toma el cerrojo de ese identificador. Sin identificador no hay cerrojo: devuelve True con
    /// Handle a 0. Devuelve False si otro proceso ya lo tiene.
    /// </summary>
    class function Acquire(const Id: string; out Handle: THandle): Boolean; static;
    /// <summary>Lo suelta, si se tomo, y deja Handle a 0.</summary>
    class procedure Release(var Handle: THandle); static;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
  Daf.Extensions.Logging;

{ TAppFailures }

constructor TAppFailures.Create;
begin
  inherited Create;
  FPolicy := ExitPolicy.LogAndFail();
end;

procedure TAppFailures.SetPolicy(const Policy: TAppExceptionPolicy);
begin
  FPolicy := Policy;
end;

function TAppFailures.Decide(const E: Exception; const Services: IServiceProvider;
  const Reported: Boolean): Integer;
begin
  // Lo que la politica ya dejo salir sigue saliendo, sin decidirse otra vez.
  if (FLetOut <> nil) and (FLetOut = E) then
  begin
    // Es la excepcion que maneja el except de quien llama, y la liberaria al salir por otra.
    AcquireExceptionObject;
    raise E at ReturnAddress;
  end;
  // Como en TApplication.HandleException: Abort es la excepcion silenciosa, una salida y no un
  // fallo que informar.
  if E is EAbort then
    Exit(TAppExit.Failure);
  try
    Result := FPolicy(TAppFailure.Create(E, Services, Reported or BootstrapLog.WasReported(E)));
  except
    FLetOut := ExceptObject;
    raise;
  end;
end;

function TAppFailures.Handle(const E: Exception): Integer;
begin
  Result := Decide(E, nil, False);
end;

{ TInstanceLock }

class function TInstanceLock.Acquire(const Id: string; out Handle: THandle): Boolean;
begin
  Handle := 0;
  if Id.IsEmpty then Exit(True);
{$IFDEF MSWINDOWS}
  Handle := CreateMutex(nil, True, PChar('Global\DAF.Instance.' + Id));
  Result := (Handle <> 0) and (GetLastError <> ERROR_ALREADY_EXISTS);
  if not Result and (Handle <> 0) then
  begin
    CloseHandle(Handle);
    Handle := 0;
  end;
{$ELSE}
  raise ENotSupportedException.Create('A single-instance lock is only available on Windows');
{$ENDIF}
end;

class procedure TInstanceLock.Release(var Handle: THandle);
begin
  if Handle = 0 then Exit;
{$IFDEF MSWINDOWS}
  ReleaseMutex(Handle);
  CloseHandle(Handle);
{$ENDIF}
  Handle := 0;
end;

end.
