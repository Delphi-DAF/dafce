unit Daf.Cli;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.Rtti,
  Daf.Extensions.Configuration,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Hosting,
  Daf.Extensions.Logging,
  Daf.CommandLine,
  Daf.Application.Version,
  Daf.Hosting;

type
  /// <summary>
  /// Componer mal la variante: declarar en el builder la accion de un comando que ya tiene una.
  /// </summary>
  ECliCompositionError = class(Exception);

  /// <summary>
  /// Los desenlaces que la variante decide por su cuenta. Los demas los devuelve la accion.
  /// </summary>
  /// <remarks>
  ///   Estan aqui y no repartidos por el codigo porque son contrato con el sistema operativo:
  ///   quien invoca la herramienta desde un script lee ESTO, y un numero cambiado en silencio
  ///   rompe el script sin romper ninguna compilacion. Los tres primeros son los de la invocacion,
  ///   y se definen a partir de ellos para que no haya dos literales que puedan divergir.
  /// </remarks>
  TCliExit = record
  public
    const Ok = TCommandLineExit.Ok;
    /// <summary>Lo que devuelve la politica por defecto ante una excepcion no capturada.</summary>
    const Failure = TCommandLineExit.Failure;
    /// <summary>La invocacion no encaja con la gramatica declarada.</summary>
    const InvocationError = TCommandLineExit.InvocationError;
    /// <summary>Ya habia otra instancia, y se declaro que solo puede haber una.</summary>
    const AlreadyRunning = 3;
  end;

  /// <summary>
  /// La accion de un comando que necesita servicios: recibe el proveedor del contenedor ya
  /// arrancado y el resultado del parse, y devuelve el codigo con el que termina el proceso.
  /// </summary>
  /// <remarks>
  ///   El proveedor llega por parametro, y no capturado, porque un cierre que capturase la
  ///   aplicacion para leer sus Services formaria con ella un ciclo de referencias: la aplicacion
  ///   sostiene la gramatica, la gramatica la accion, y la accion la aplicacion.
  /// </remarks>
  TCliAction = reference to function(const Services: IServiceProvider;
    const Parsed: ICommandLineParseResult): Integer;

  /// <summary>
  /// Que hacer con lo que se escapa, y con que codigo termina el proceso.
  /// </summary>
  /// <remarks>
  ///   Es un callback y no un enumerado cerrado porque una suite de aplicaciones tiene su
  ///   propio criterio de codigos de salida, y con un enumerado cada criterio nuevo seria un
  ///   cambio en el framework. Las de siempre estan escritas en <see cref="ExitPolicy"/>.
  ///
  ///   Services llega vacio cuando lo que se escapa es un fallo de la parada: el contenedor ya se
  ///   apago.
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
    /// La gramatica de esta herramienta, que parsea los argumentos en <c>Build</c>. Los mismos
    /// argumentos estan tambien en la configuracion, como en .NET, y alli se leen sin la
    /// gramatica: una bandera seguida de un posicional queda como clave con el posicional por
    /// valor. Lo que la gramatica declara se lee del resultado del parseo.
    /// </summary>
    property RootCommand: IRootCommand read GetRootCommand;

    /// <summary>
    /// Declara que solo puede correr una instancia con ese identificador. Se comprueba ANTES
    /// de construir el contenedor: una aplicacion que decide no arrancar no paga por arrancar.
    /// </summary>
    function SingleInstance(const Id: string): ICliApplicationBuilder;
    /// <summary>
    /// La politica que envuelve al Run, y la que decide lo que se escapa de una accion o de la
    /// parada. Sin ella, LogAndFail.
    /// </summary>
    function OnUnhandled(const Policy: TCliExceptionPolicy): ICliApplicationBuilder;
    /// <summary>La accion de la raiz. Ver la sobrecarga.</summary>
    function SetAction(const Action: TCliAction): ICliApplicationBuilder; overload;
    /// <summary>
    /// Declara la accion de un comando que necesita servicios. Corre cuando la invocacion alcanza
    /// ese comando, con el contenedor ya arrancado.
    /// </summary>
    /// <remarks>
    ///   Se instala en el comando en el acto, asi que un comando con subcomandos y accion declarada
    ///   aqui es invocable por si mismo.
    ///
    ///   Si el comando ya tiene accion, lanza <see cref="ECliCompositionError"/> en el acto: son dos
    ///   formas de decir lo mismo, y cual gana no es algo que haya que recordar. Un
    ///   <c>Command.SetAction</c> posterior la sustituye, como sustituye cualquier accion.
    ///
    ///   Sin ninguna accion declarada, la aplicacion es sus tareas de arranque y sus servicios
    ///   alojados, y termina con Ok.
    /// </remarks>
    function SetAction(const Command: ICommand; const Action: TCliAction): ICliApplicationBuilder; overload;

    function Build: ICliApplication;
  end;

  /// <summary>
  /// La aplicacion CLI. DAF es dueño del proceso: Run acaba y el .dpr con el.
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
    /// Arranca el host, invoca el resultado del parse y para. Devuelve un Integer y no es un
    /// detalle: una aplicacion CLI que no puede decirle al sistema como termino no sirve en un
    /// script.
    /// </summary>
    /// <remarks>
    ///   La version, la ayuda y los errores de parse los resuelve la invocacion sin arrancar
    ///   nada. Lo que se escapa de la accion lo decide la politica de <c>OnUnhandled</c>, y no el
    ///   manejador por defecto de la invocacion.
    ///
    ///   Un fallo de la parada lo decide tambien la politica, que lo recibe sin proveedor. Si la
    ///   accion ya habia fallado, el codigo es el que la politica dio a ese primer fallo, y el de la
    ///   parada solo se informa.
    /// </remarks>
    function Run: Integer;
    /// <summary>
    /// El contenedor, o nil si la invocacion se resolvio sin construirlo --otra instancia ya
    /// corriendo, o <c>--version</c> y <c>--help</c>, que los contesta la gramatica--.
    /// </summary>
    property Services: IServiceProvider read GetServices;
  end;

  TCliApplication = class(TInterfacedObject, ICliApplication)
  private type
    /// <summary>
    /// El proveedor tal como lo ven las acciones declaradas en el builder: lleno mientras el host
    /// corre, vacio antes y despues.
    /// </summary>
    /// <remarks>
    ///   No apunta ni a la gramatica ni a la aplicacion, y por eso la accion que lo captura no
    ///   cierra ningun ciclo con ellas.
    /// </remarks>
    IServicesSlot = interface
      function Services: IServiceProvider;
      procedure Fill(const Value: IServiceProvider);
      procedure Clear;
    end;

    TServicesSlot = class(TInterfacedObject, IServicesSlot)
    private
      FServices: IServiceProvider;
    public
      function Services: IServiceProvider;
      procedure Fill(const Value: IServiceProvider);
      procedure Clear;
    end;
  private
    FHost: IHost;
    FRootCommand: IRootCommand;
    FParsed: ICommandLineParseResult;
    FPolicy: TCliExceptionPolicy;
    FSlot: IServicesSlot;
    FSettledCode: Integer;
    FSettled: Boolean;
    FInstanceLock: THandle;
  public
    /// <summary>El punto de entrada de una aplicacion CLI de la familia.</summary>
    class function CreateBuilder(const Args: TArray<string> = nil): ICliApplicationBuilder; static;
    constructor Create(const Host: IHost; const RootCommand: IRootCommand;
      const Parsed: ICommandLineParseResult; const Policy: TCliExceptionPolicy;
      const Slot: IServicesSlot; const InstanceLock: THandle);
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
    FSlot: TCliApplication.IServicesSlot;
    function GetRootCommand: IRootCommand;
    function GetServices: IServiceCollection;
    function GetConfiguration: IConfigurationManager;
    function GetEnvironment: IHostEnvironment;
    function GetLogging: ILoggingBuilder;
    function GetProperties: TDictionary<string, TValue>;
    function GetServiceProviderOptions: TServiceProviderOptions;
    procedure SetServiceProviderOptions(const Value: TServiceProviderOptions);
    function AcquireInstanceLock(out Lock: THandle): Boolean;
  public
    constructor Create(const Args: TArray<string>);
    destructor Destroy; override;
    function SingleInstance(const Id: string): ICliApplicationBuilder;
    function OnUnhandled(const Policy: TCliExceptionPolicy): ICliApplicationBuilder;
    function SetAction(const Action: TCliAction): ICliApplicationBuilder; overload;
    function SetAction(const Command: ICommand; const Action: TCliAction): ICliApplicationBuilder; overload;
    function Build: ICliApplication;
    property RootCommand: IRootCommand read GetRootCommand;
    property Services: IServiceCollection read GetServices;
    property Configuration: IConfigurationManager read GetConfiguration;
    property Environment: IHostEnvironment read GetEnvironment;
    property Logging: ILoggingBuilder read GetLogging;
    property Properties: TDictionary<string, TValue> read GetProperties;
    property ServiceProviderOptions: TServiceProviderOptions read GetServiceProviderOptions
      write SetServiceProviderOptions;
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
      // E es la excepcion que el handler de Run esta manejando, y el handler la libera al salir por
      // otra. Sin quitarsela antes, quien la capture fuera recibe memoria ya devuelta.
      AcquireExceptionObject;
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

{ TCliApplication.TServicesSlot }

function TCliApplication.TServicesSlot.Services: IServiceProvider;
begin
  Result := FServices;
end;

procedure TCliApplication.TServicesSlot.Fill(const Value: IServiceProvider);
begin
  FServices := Value;
end;

procedure TCliApplication.TServicesSlot.Clear;
begin
  FServices := nil;
end;

{ TCliApplicationBuilder }

// Fuera de las clases, para que el cierre capture la accion y el soporte y nada mas: ni el builder
// ni el comando en el que se instala.
function ServiceAction(const Action: TCliAction; const Slot: TCliApplication.IServicesSlot): TCommandAction;
begin
  Result :=
    function(const Parsed: ICommandLineParseResult): Integer
    begin
      Result := Action(Slot.Services, Parsed);
    end;
end;

constructor TCliApplicationBuilder.Create(const Args: TArray<string>);
begin
  inherited Create;
  FArgs := Args;

  // Como en .NET: los argumentos van a la gramatica y tambien, como fuente, a la configuracion.
  FImpl := THostBuilder.Create(Args);
  FOwned := FImpl;

  FRootCommand := CommandLine.Root(TPath.GetFileNameWithoutExtension(ParamStr(0)));
  FPolicy := ExitPolicy.LogAndFail();
  FSlot := TCliApplication.TServicesSlot.Create;
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

function TCliApplicationBuilder.GetServiceProviderOptions: TServiceProviderOptions;
begin
  Result := FImpl.ServiceProviderOptions;
end;

procedure TCliApplicationBuilder.SetServiceProviderOptions(const Value: TServiceProviderOptions);
begin
  FImpl.ServiceProviderOptions := Value;
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

function TCliApplicationBuilder.SetAction(const Action: TCliAction): ICliApplicationBuilder;
begin
  Result := SetAction(FRootCommand, Action);
end;

function TCliApplicationBuilder.SetAction(const Command: ICommand; const Action: TCliAction): ICliApplicationBuilder;
begin
  var Existing := Command.Action();
  if Assigned(Existing) then
    raise ECliCompositionError.CreateFmt('Command "%s" already has an action. Two ways of saying the ' +
      'same thing is a precedence rule nobody remembers -- keep one.', [Command.FullPath]);
  Command.SetAction(ServiceAction(Action, FSlot));
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
  // Lo de ANTES del contenedor. Una aplicacion que decide no arrancar termina sin haber
  // construido nada, y lo dice por su codigo de salida.
  var Lock: THandle;
  if not AcquireInstanceLock(Lock) then
    Exit(TCliApplication.CreateSettled(TCliExit.AlreadyRunning));

  // El parse va aqui, antes de construir: --version y --help los contesta la gramatica, y no
  // hay razon para levantar un contenedor entero para imprimir una linea.
  var Parsed := FRootCommand.Parse(FArgs);
  if Parsed.VersionRequested or Parsed.HelpRequested or not Parsed.Succeeded then
    Exit(TCliApplication.Create(nil, FRootCommand, Parsed, FPolicy, FSlot, Lock));

  Result := TCliApplication.Create(FImpl.Build, FRootCommand, Parsed, FPolicy, FSlot, Lock);
end;

{ TCliApplication }

class function TCliApplication.CreateBuilder(const Args: TArray<string>): ICliApplicationBuilder;
begin
  Result := TCliApplicationBuilder.Create(Args);
end;

constructor TCliApplication.Create(const Host: IHost; const RootCommand: IRootCommand;
  const Parsed: ICommandLineParseResult; const Policy: TCliExceptionPolicy;
  const Slot: IServicesSlot; const InstanceLock: THandle);
begin
  inherited Create;
  FHost := Host;
  FRootCommand := RootCommand;
  FParsed := Parsed;
  FPolicy := Policy;
  FSlot := Slot;
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

function TCliApplication.Run: Integer;
var
  Configuration: TInvocationConfiguration;
begin
  if FSettled then Exit(FSettledCode);

  // Lo que se escapa de la accion es de la politica: con el manejador de la invocacion puesto, la
  // politica no veria nunca la excepcion.
  Configuration.EnableDefaultExceptionHandler := False;

  // Sin host, Build ya vio que el parse pidio la version o la ayuda, o que no encajaba: la
  // invocacion lo resuelve sin ejecutar ninguna accion.
  if not Assigned(FHost) then
    Exit(FParsed.Invoke(Configuration));

  var ActionFailed := False;
  FHost.Start;
  try
    FSlot.Fill(FHost.Services);
    try
      Result := FParsed.Invoke(Configuration);
    except
      on E: Exception do
      begin
        ActionFailed := True;
        Result := FPolicy(E, FHost.Services);
      end;
    end;
  finally
    // Antes de parar: el soporte vive lo que vivan los comandos, y el proveedor no debe
    // sobrevivir a Stop dentro de el.
    FSlot.Clear;
    try
      try
        FHost.Stop;
      finally
        // Stop LIBERA el host, tambien cuando lanza: THost es TNoRefCountObject y se hace Free a si
        // mismo al parar. Soltar la referencia aqui no es higiene, es lo que evita que el
        // finalizador del campo llame a _Release sobre memoria ya devuelta -- que con el gestor en
        // modo depuracion es una violacion de acceso, y sin el, un acierto por casualidad.
        Pointer(FHost) := nil;
      end;
    except
      // Si la accion ya habia fallado, el codigo es el que la politica dio a ese fallo, y Stop ya
      // informo el suyo.
      on E: Exception do
        if not ActionFailed then
          Result := FPolicy(E, Default(IServiceProvider));
    end;
  end;
end;

end.
