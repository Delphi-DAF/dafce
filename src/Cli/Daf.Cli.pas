unit Daf.Cli;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.Rtti,
  Daf.Extensions.Configuration,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Hosting,
  DAF.Extensions.Logging,
  Daf.CommandLine,
  Daf.Hosting;

type
  /// <summary>
  /// Componer mal la variante: dos formas de decir cual es el trabajo, no una sola.
  /// </summary>
  ECliCompositionError = class(Exception);

  /// <summary>
  /// Los desenlaces que la variante decide por su cuenta. Los demas los devuelve el trabajo.
  /// </summary>
  /// <remarks>
  ///   Estan aqui y no repartidos por el codigo porque son contrato con el sistema operativo:
  ///   quien invoca la herramienta desde un script lee ESTO, y un numero cambiado en silencio
  ///   rompe el script sin romper ninguna compilacion.
  /// </remarks>
  TCliExit = record
  public
    const Ok = 0;
    /// <summary>Lo que devuelve la politica por defecto ante una excepcion no capturada.</summary>
    const Failure = 1;
    /// <summary>La invocacion no encaja con la gramatica declarada.</summary>
    const InvocationError = 2;
    /// <summary>Ya habia otra instancia, y se declaro que solo puede haber una.</summary>
    const AlreadyRunning = 3;
  end;

  /// <summary>
  /// El trabajo, resuelto del contenedor. Se registra como cualquier otro servicio --
  /// <c>B.Services.AddSingleton&lt;ICliHandler, TMiHandler&gt;</c>-- y no hace falta una via
  /// de registro propia: la que hay ya sirve.
  /// </summary>
  ICliHandler = interface(IInvokable)
    ['{52B72A0C-07F3-479D-998B-970DBCA7CFD4}']
    function Execute(const Parsed: ICommandLineParseResult): Integer;
  end;

  /// <summary>El trabajo escrito en el sitio, para el .dpr que no necesita una clase.</summary>
  TCliExecuteHandler = reference to function(const Services: IServiceProvider;
    const Parsed: ICommandLineParseResult): Integer;

  /// <summary>
  /// Que hacer con lo que se escapa, y con que codigo termina el proceso.
  /// </summary>
  /// <remarks>
  ///   Es un callback y no un enumerado cerrado porque una suite de aplicaciones tiene su
  ///   propio criterio de codigos de salida, y con un enumerado cada criterio nuevo seria un
  ///   cambio en el framework. Las de siempre estan escritas en <see cref="ExitPolicy"/>.
  /// </remarks>
  TCliExceptionPolicy = reference to function(const E: Exception;
    const Services: IServiceProvider): Integer;

  /// <summary>
  /// Las politicas de siempre, ya escritas. Fachada <c>class</c> y no <c>record</c>: se hereda
  /// para agrupar la de una suite, que es justo lo que un record no deja hacer.
  /// </summary>
  ExitPolicy = class
  public
    /// <summary>Informa por el canal de arranque y devuelve Failure.</summary>
    class function LogAndFail: TCliExceptionPolicy; virtual;
    /// <summary>Propaga: la excepcion sale del Run y la ve quien invoco.</summary>
    class function Rethrow: TCliExceptionPolicy; virtual;
    /// <summary>Se traga la excepcion y devuelve Failure, sin decir nada.</summary>
    /// <remarks>Existe para que callar sea una DECISION escrita y no un except vacio.</remarks>
    class function Silent: TCliExceptionPolicy; virtual;
  end;

  ICliApplication = interface;

  /// <summary>
  /// El builder de la variante CLI: el contrato comun de la familia, mas la gramatica y el
  /// borde del Run.
  /// </summary>
  ICliApplicationBuilder = interface(IHostApplicationBuilder)
    ['{71536E8D-49F8-45D8-AB43-C5F726B2F25A}']
    function GetRootCommand: IRootCommand;
    /// <summary>
    /// La gramatica de esta herramienta. Es la UNICA duena de argv en esta variante: el
    /// builder no anade los argumentos como fuente de configuracion por su cuenta, porque las
    /// dos lecturas discrepan por construccion y ninguna de las dos da error. Quien quiera la
    /// vista de configuracion la pide: <c>CommandLineConfig.AddArgs(B.Configuration, Args)</c>.
    /// </summary>
    property RootCommand: IRootCommand read GetRootCommand;

    /// <summary>
    /// Declara que solo puede correr una instancia con ese identificador. Se comprueba ANTES
    /// de construir el contenedor: una aplicacion que decide no arrancar no paga por arrancar.
    /// </summary>
    function SingleInstance(const Id: string): ICliApplicationBuilder;
    /// <summary>La politica que envuelve al Run. Sin ella, LogAndFail.</summary>
    function OnUnhandled(const Policy: TCliExceptionPolicy): ICliApplicationBuilder;
    /// <summary>
    /// El trabajo, escrito aqui mismo. Alternativa a registrar un <see cref="ICliHandler"/>;
    /// declarar los dos es un error de composicion y no una precedencia que haya que recordar.
    /// </summary>
    function OnExecute(const Handler: TCliExecuteHandler): ICliApplicationBuilder;

    function Build: ICliApplication;
  end;

  /// <summary>
  /// La aplicacion CLI. DAF es dueno del proceso: Run acaba y el .dpr con el.
  /// </summary>
  /// <remarks>
  ///   NO ofrece acceso global ni a si misma ni a su proveedor de servicios, y eso es
  ///   deliberado: en consola la plataforma no tiene ningun Application global que el usuario
  ///   espere, .NET tampoco lo tiene, y anadirlo seria un service locator sin nada que lo
  ///   justifique. Donde si lo hay --VCL y FMX-- es porque la plataforma ya lo puso.
  /// </remarks>
  ICliApplication = interface(IInvokable)
    ['{EEADF167-67E2-4B7A-BB34-12DC35EF412E}']
    function GetServices: IServiceProvider;
    /// <summary>
    /// Parse, cortocircuitos, trabajo y parada. Devuelve un Integer y no es un detalle: una
    /// aplicacion CLI que no puede decirle al sistema como termino no sirve en un script.
    /// </summary>
    function Run: Integer;
    /// <summary>
    /// El contenedor, o nil si la invocacion se resolvio sin construirlo --otra instancia ya
    /// corriendo, o <c>--version</c> y <c>--help</c>, que los contesta la gramatica--.
    /// </summary>
    property Services: IServiceProvider read GetServices;
  end;

  TCliApplication = class(TInterfacedObject, ICliApplication)
  private
    FHost: IHost;
    FRootCommand: IRootCommand;
    FParsed: ICommandLineParseResult;
    FPolicy: TCliExceptionPolicy;
    FExecute: TCliExecuteHandler;
    FSettledCode: Integer;
    FSettled: Boolean;
    FInstanceLock: THandle;
    function DispatchWork: Integer;
  public
    /// <summary>El punto de entrada de una aplicacion CLI de la familia.</summary>
    class function CreateBuilder(const Args: TArray<string> = nil): ICliApplicationBuilder; static;
    constructor Create(const Host: IHost; const RootCommand: IRootCommand;
      const Parsed: ICommandLineParseResult; const Policy: TCliExceptionPolicy;
      const Execute: TCliExecuteHandler; const InstanceLock: THandle);
    constructor CreateSettled(const Code: Integer);
    destructor Destroy; override;
    function GetServices: IServiceProvider;
    function Run: Integer;
  end;

  TCliApplicationBuilder = class(TInterfacedObject, ICliApplicationBuilder, IHostApplicationBuilder)
  private
    // La maquinaria comun. Se sostiene por el contrato comun --que es quien la refcuenta-- y
    // Build se le llama por la clase, porque el tipo que devuelve es de ESTA variante.
    FImpl: THostBuilder;
    FOwned: IHostApplicationBuilder;
    FArgs: TArray<string>;
    FRootCommand: IRootCommand;
    FSingleInstanceId: string;
    FPolicy: TCliExceptionPolicy;
    FExecute: TCliExecuteHandler;
    function GetRootCommand: IRootCommand;
    function GetServices: IServiceCollection;
    function GetConfiguration: IConfigurationManager;
    function GetEnvironment: IHostEnvironment;
    function GetLogging: ILoggingBuilder;
    function GetProperties: TDictionary<string, TValue>;
    function AcquireInstanceLock(out Lock: THandle): Boolean;
  public
    constructor Create(const Args: TArray<string>);
    destructor Destroy; override;
    function SingleInstance(const Id: string): ICliApplicationBuilder;
    function OnUnhandled(const Policy: TCliExceptionPolicy): ICliApplicationBuilder;
    function OnExecute(const Handler: TCliExecuteHandler): ICliApplicationBuilder;
    function Build: ICliApplication;
    property RootCommand: IRootCommand read GetRootCommand;
    property Services: IServiceCollection read GetServices;
    property Configuration: IConfigurationManager read GetConfiguration;
    property Environment: IHostEnvironment read GetEnvironment;
    property Logging: ILoggingBuilder read GetLogging;
    property Properties: TDictionary<string, TValue> read GetProperties;
  end;

implementation

uses
  Winapi.Windows,
  System.IOUtils;

{ ExitPolicy }

class function ExitPolicy.LogAndFail: TCliExceptionPolicy;
begin
  Result :=
    function(const E: Exception; const Services: IServiceProvider): Integer
    begin
      // Por el canal de arranque y no por el pipeline: a estas alturas el proveedor puede estar
      // ya soltandose, y este informe es lo ultimo que se puede decir.
      BootstrapLog.LoggerFor('TCliApplication').LogError(E, 'the application ended on an unhandled exception');
      Result := TCliExit.Failure;
    end;
end;

class function ExitPolicy.Rethrow: TCliExceptionPolicy;
begin
  Result :=
    function(const E: Exception; const Services: IServiceProvider): Integer
    begin
      raise E at ReturnAddress;
    end;
end;

class function ExitPolicy.Silent: TCliExceptionPolicy;
begin
  Result :=
    function(const E: Exception; const Services: IServiceProvider): Integer
    begin
      Result := TCliExit.Failure;
    end;
end;

{ TCliApplicationBuilder }

constructor TCliApplicationBuilder.Create(const Args: TArray<string>);
begin
  inherited Create;
  FArgs := Args;

  // Sin argumentos para la maquinaria comun: en esta variante la gramatica es la duena de argv.
  // Las variantes sin gramatica SI se los pasan, que alli no hay con que chocar.
  FImpl := THostBuilder.Create;
  FOwned := FImpl;

  FRootCommand := CommandLine.Root(TPath.GetFileNameWithoutExtension(ParamStr(0)));
  FPolicy := ExitPolicy.LogAndFail();
end;

destructor TCliApplicationBuilder.Destroy;
begin
  inherited;
end;

function TCliApplicationBuilder.GetRootCommand: IRootCommand;
begin
  Result := FRootCommand;
end;

function TCliApplicationBuilder.GetServices: IServiceCollection;
begin
  Result := FImpl.Services;
end;

function TCliApplicationBuilder.GetConfiguration: IConfigurationManager;
begin
  Result := FImpl.Configuration;
end;

function TCliApplicationBuilder.GetEnvironment: IHostEnvironment;
begin
  Result := FImpl.Environment;
end;

function TCliApplicationBuilder.GetLogging: ILoggingBuilder;
begin
  Result := FImpl.Logging;
end;

function TCliApplicationBuilder.GetProperties: TDictionary<string, TValue>;
begin
  Result := FImpl.Properties;
end;

function TCliApplicationBuilder.SingleInstance(const Id: string): ICliApplicationBuilder;
begin
  FSingleInstanceId := Id;
  Result := Self;
end;

function TCliApplicationBuilder.OnUnhandled(const Policy: TCliExceptionPolicy): ICliApplicationBuilder;
begin
  FPolicy := Policy;
  Result := Self;
end;

function TCliApplicationBuilder.OnExecute(const Handler: TCliExecuteHandler): ICliApplicationBuilder;
begin
  FExecute := Handler;
  Result := Self;
end;

function TCliApplicationBuilder.AcquireInstanceLock(out Lock: THandle): Boolean;
begin
  Lock := 0;
  if FSingleInstanceId.IsEmpty then Exit(True);

  Lock := CreateMutex(nil, True, PChar('Global\DAF.Cli.' + FSingleInstanceId));
  Result := (Lock <> 0) and (GetLastError <> ERROR_ALREADY_EXISTS);
  if not Result and (Lock <> 0) then
  begin
    CloseHandle(Lock);
    Lock := 0;
  end;
end;

function TCliApplicationBuilder.Build: ICliApplication;
begin
  if Assigned(FExecute) and FImpl.Services.Contains<ICliHandler> then
    raise ECliCompositionError.Create('This application declares its work twice: an OnExecute ' +
      'handler and a registered ICliHandler. Two ways of saying the same thing is a precedence ' +
      'rule nobody remembers -- keep one.');

  // Lo de ANTES del contenedor. Una aplicacion que decide no arrancar termina sin haber
  // construido nada, y lo dice por su codigo de salida.
  var Lock: THandle;
  if not AcquireInstanceLock(Lock) then
    Exit(TCliApplication.CreateSettled(TCliExit.AlreadyRunning));

  // El parse va aqui, antes de construir: --version y --help los contesta la gramatica, y no
  // hay razon para levantar un contenedor entero para imprimir una linea.
  var Parsed := FRootCommand.Parse(FArgs);
  if Parsed.VersionRequested or Parsed.HelpRequested or not Parsed.Succeeded then
    Exit(TCliApplication.Create(nil, FRootCommand, Parsed, FPolicy, FExecute, Lock));

  Result := TCliApplication.Create(FImpl.Build, FRootCommand, Parsed, FPolicy, FExecute, Lock);
end;

{ TCliApplication }

class function TCliApplication.CreateBuilder(const Args: TArray<string>): ICliApplicationBuilder;
begin
  Result := TCliApplicationBuilder.Create(Args);
end;

constructor TCliApplication.Create(const Host: IHost; const RootCommand: IRootCommand;
  const Parsed: ICommandLineParseResult; const Policy: TCliExceptionPolicy;
  const Execute: TCliExecuteHandler; const InstanceLock: THandle);
begin
  inherited Create;
  FHost := Host;
  FRootCommand := RootCommand;
  FParsed := Parsed;
  FPolicy := Policy;
  FExecute := Execute;
  FInstanceLock := InstanceLock;
end;

constructor TCliApplication.CreateSettled(const Code: Integer);
begin
  inherited Create;
  FSettled := True;
  FSettledCode := Code;
end;

destructor TCliApplication.Destroy;
begin
  if FInstanceLock <> 0 then
  begin
    ReleaseMutex(FInstanceLock);
    CloseHandle(FInstanceLock);
  end;
  inherited;
end;

function TCliApplication.GetServices: IServiceProvider;
begin
  if Assigned(FHost) then
    Result := FHost.Services
  else
    Result := nil;
end;

function TCliApplication.DispatchWork: Integer;
begin
  if Assigned(FExecute) then
    Exit(FExecute(FHost.Services, FParsed));

  var Handler := FHost.Services.GetService<ICliHandler>;
  if Assigned(Handler) then
    Exit(Handler.Execute(FParsed));

  // Ni una cosa ni la otra: la aplicacion es sus tareas de arranque y sus servicios alojados,
  // que ya han corrido. Es un desenlace valido y no un descuido.
  Result := TCliExit.Ok;
end;

function TCliApplication.Run: Integer;
begin
  if FSettled then Exit(FSettledCode);

  if FParsed.VersionRequested then
  begin
    Writeln(FParsed.VersionText);
    Exit(TCliExit.Ok);
  end;

  if FParsed.HelpRequested then
  begin
    Writeln(FParsed.HelpText);
    Exit(TCliExit.Ok);
  end;

  if not FParsed.Succeeded then
  begin
    // Al canal de error, que es donde un script lo espera, y con codigo propio: "la invocacion
    // no encajaba" no es lo mismo que "el trabajo fallo".
    Writeln(ErrOutput, FParsed.ErrorText);
    Exit(TCliExit.InvocationError);
  end;

  FHost.Start;
  try
    try
      Result := DispatchWork;
    except
      on E: Exception do
        Result := FPolicy(E, FHost.Services);
    end;
  finally
    FHost.Stop;
    // Stop LIBERA el host: THost es TNoRefCountObject y se hace Free a si mismo al parar. Soltar
    // la referencia aqui no es higiene, es lo que evita que el finalizador del campo llame a
    // _Release sobre memoria ya devuelta -- que con el gestor en modo depuracion es una violacion
    // de acceso, y sin el, un acierto por casualidad.
    Pointer(FHost) := nil;
  end;
end;

end.
