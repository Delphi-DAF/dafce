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
  Daf.Extensions.Hosting.Failures,
  Daf.Hosting,
  Daf.Hosting.Edge;

type
  /// <summary>
  /// Componer mal la variante: declarar en el builder la accion de un comando que ya tiene una.
  /// </summary>
  ECliCompositionError = class(Exception);

  /// <summary>
  /// Los desenlaces que la variante decide por su cuenta. Los demas los devuelve la accion.
  /// </summary>
  /// <remarks>
  ///   Los de la familia, mas el de una invocacion que no encaja con la gramatica, que solo existe
  ///   donde hay gramatica. Se definen a partir de los de la familia y de los de la invocacion para
  ///   que no haya dos literales que puedan divergir.
  /// </remarks>
  TCliExit = record
  public
    const Ok = TAppExit.Ok;
    /// <summary>Lo que devuelve la politica por defecto ante una excepcion no capturada.</summary>
    const Failure = TAppExit.Failure;
    /// <summary>La invocacion no encaja con la gramatica declarada.</summary>
    const InvocationError = TCommandLineExit.InvocationError;
    /// <summary>Ya habia otra instancia, y se declaro que solo puede haber una.</summary>
    const AlreadyRunning = TAppExit.AlreadyRunning;
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

  // Los de la familia, para que un .dpr de CLI los escriba con esta unit sola.
  TAppExit = Daf.Extensions.Hosting.Failures.TAppExit;
  TAppFailure = Daf.Extensions.Hosting.Failures.TAppFailure;
  TAppExceptionPolicy = Daf.Extensions.Hosting.Failures.TAppExceptionPolicy;
  ExitPolicy = Daf.Extensions.Hosting.Failures.ExitPolicy;

  ICliApplication = interface;

  /// <summary>
  /// El builder de la variante CLI: el contrato comun de la familia, mas la gramatica y el
  /// borde del Run.
  /// </summary>
  ICliApplicationBuilder = interface(IHostApplicationBuilder)
    ['{71536E8D-49F8-45D8-AB43-C5F726B2F25A}']
    function GetRootCommand: IRootCommand;
    /// <summary>
    /// La gramatica de esta herramienta, que parsea los argumentos en <c>ParseArgs</c>, o en
    /// <c>Build</c> antes que nada mas si la composicion no lo pidio. Una invocacion que llega a una
    /// accion tiene el resultado del parse tambien en el contenedor, como
    /// <c>ICommandLineParseResult</c>, y un servicio lo pide por constructor.
    ///
    /// Los mismos argumentos estan tambien en la configuracion, como en .NET, y alli se leen sin la
    /// gramatica: una bandera seguida de un posicional queda como clave con el posicional por
    /// valor. Lo que la gramatica declara se lee del resultado del parseo.
    /// </summary>
    property RootCommand: IRootCommand read GetRootCommand;

    /// <summary>
    /// Declara que solo puede correr una instancia con ese identificador. Sin declararla no hay
    /// cerrojo, y pueden correr varias a la vez. Se comprueba despues del parse y antes de cargar
    /// la configuracion y de construir el contenedor: una aplicacion que decide no arrancar no paga
    /// por arrancar.
    /// </summary>
    function SingleInstance(const Id: string): ICliApplicationBuilder;
    /// <summary>
    /// La politica que decide lo que se escapa, y con que codigo termina el proceso: dentro del Run,
    /// lo que se escapa del arranque, de una accion o de la parada; fuera, lo que se le pase por
    /// <see cref="HandleException"/>. Sin ella, LogAndFail.
    /// </summary>
    /// <remarks>
    ///   Un EAbort no llega a la politica: termina con <c>TCliExit.Failure</c> sin informar nada.
    /// </remarks>
    function OnException(const Policy: TAppExceptionPolicy): ICliApplicationBuilder;
    /// <summary>
    /// Pasa por la politica de <c>OnException</c> lo que se escapa antes del Run --de la
    /// composicion o del Build-- y devuelve el codigo con el que termina el proceso. Es lo que
    /// llama el except del .dpr.
    /// </summary>
    /// <remarks>
    ///   Se llama desde el except que maneja <c>E</c>. Lo que la politica ya dejo salir del Run se
    ///   relanza sin volver a pasar por ella, y lo que el host ya informo al construirse le llega
    ///   con <c>Reported</c>.
    /// </remarks>
    function HandleException(const E: Exception): Integer;
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
    /// <summary>La accion standalone de la raiz. Ver la sobrecarga.</summary>
    function SetStandaloneAction(const Action: TCommandAction): ICliApplicationBuilder; overload;
    /// <summary>
    /// Declara la accion de un comando que corre sin host: cuando la invocacion alcanza ese
    /// comando no hay cerrojo de instancia unica, ni tareas de arranque, ni servicios alojados, ni
    /// configuracion, ni contenedor. Recibe solo el resultado del parse.
    /// </summary>
    /// <remarks>
    ///   Es para lo que se contesta con los argumentos solos, como un <c>init</c> que escribe la
    ///   configuracion que una ejecucion normal exige, y que por eso no puede depender de ella.
    ///
    ///   Como <c>SetAction</c>: si el comando ya tiene accion, lanza
    ///   <see cref="ECliCompositionError"/> en el acto. Un <c>Command.SetAction</c> posterior
    ///   sustituye la accion, y el comando sigue sin host. Lo que se escapa de ella va a la
    ///   politica de <c>OnException</c>, sin proveedor.
    /// </remarks>
    function SetStandaloneAction(const Command: ICommand; const Action: TCommandAction): ICliApplicationBuilder; overload;

    /// <summary>
    /// Parsea los argumentos con la gramatica y la congela. Es para la composicion que decide con
    /// lo parseado: que fuente anadir, que implementacion registrar.
    /// </summary>
    /// <remarks>
    ///   Parsea una sola vez: llamarlo de nuevo, o el Build despues, devuelve el mismo resultado. A
    ///   partir de aqui, cambiar la gramatica lanza ECommandLine; las acciones siguen abiertas.
    ///
    ///   Una composicion que corta cuando el parse se contesta solo --ayuda, version o una
    ///   invocacion que no encaja-- y va directa al Build deja contestar a la gramatica aunque lo
    ///   que iba a anadir despues no se pueda leer.
    /// </remarks>
    function ParseArgs: ICommandLineParseResult;

    /// <summary>
    /// Parsea, si no lo hizo ya ParseArgs; registra el resultado del parse; toma el cerrojo, si se declaro instancia unica; y
    /// construye el host, que carga la configuracion que la composicion no leyo.
    /// </summary>
    /// <remarks>
    ///   La version, la ayuda, una invocacion que no encaja y un comando standalone terminan en el
    ///   parse: la aplicacion se construye sin host, y un fichero de configuracion del host que no
    ///   se puede leer no le impide contestar. Si la invocacion llega a una accion con host, ese
    ///   fichero hace fallar el Build.
    /// </remarks>
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
    ///   La version, la ayuda, los errores de parse y un comando standalone los resuelve la
    ///   invocacion sin arrancar nada. Todo lo que se escapa lo decide la politica de
    ///   <c>OnException</c>, y no el manejador por defecto de la invocacion: el arranque, la accion,
    ///   la parada, y tambien una accion de ayuda o de version sustituida o una accion standalone,
    ///   que llegan sin proveedor.
    ///
    ///   Un fallo del arranque llega con el proveedor y ya informado, la accion no corre, y el host
    ///   se para igual. Un fallo de la parada llega sin proveedor, y si ya habia fallado algo antes
    ///   solo se informa.
    ///
    ///   Mientras corre, el gancho ApplicationHandleException del RTL apunta a la politica, como en
    ///   una aplicacion VCL: lo que lanza un procedimiento encolado al procesarse en
    ///   CheckSynchronize, o el OnCreate o el OnDestroy de un modulo de datos, le llega con el
    ///   proveedor, y un OnCreate que lanza no impide crear el modulo.
    ///
    ///   El codigo de salida es el que la politica dio al primer fallo, aunque la accion termine
    ///   despues bien.
    /// </remarks>
    function Run: Integer;
    /// <summary>
    /// El contenedor, o nil si la invocacion se resolvio sin construirlo --otra instancia ya
    /// corriendo, <c>--version</c> y <c>--help</c>, que los contesta la gramatica, o un comando
    /// standalone--.
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
    FFailures: IAppFailures;
    FSlot: IServicesSlot;
    FSettledCode: Integer;
    FSettled: Boolean;
    FInstanceLock: THandle;
    FFailed: Boolean;
    FFailedCode: Integer;
    /// <summary>El primer codigo que decide la politica es el del proceso.</summary>
    procedure Settle(const Code: Integer);
    /// <summary>El gancho del RTL mientras corre el Run.</summary>
    procedure HandleRtlException(Sender: TObject);
  public
    /// <summary>El punto de entrada de una aplicacion CLI de la familia.</summary>
    class function CreateBuilder(const Args: TArray<string> = nil): ICliApplicationBuilder; static;
    constructor Create(const Host: IHost; const RootCommand: IRootCommand;
      const Parsed: ICommandLineParseResult; const Failures: IAppFailures;
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
    FFailures: IAppFailures;
    FSlot: TCliApplication.IServicesSlot;
    FParsed: ICommandLineParseResult;
    FStandalone: TArray<ICommand>;
    function GetRootCommand: IRootCommand;
    procedure CheckNoAction(const Command: ICommand);
    function IsStandalone(const Command: ICommand): Boolean;
    function GetServices: IServiceCollection;
    function GetConfiguration: IConfigurationManager;
    function GetEnvironment: IHostEnvironment;
    function GetLogging: ILoggingBuilder;
    function GetProperties: TDictionary<string, TValue>;
    function GetServiceProviderOptions: TServiceProviderOptions;
    procedure SetServiceProviderOptions(const Value: TServiceProviderOptions);
  public
    constructor Create(const Args: TArray<string>);
    destructor Destroy; override;
    function SingleInstance(const Id: string): ICliApplicationBuilder;
    function OnException(const Policy: TAppExceptionPolicy): ICliApplicationBuilder;
    function HandleException(const E: Exception): Integer;
    function SetAction(const Action: TCliAction): ICliApplicationBuilder; overload;
    function SetAction(const Command: ICommand; const Action: TCliAction): ICliApplicationBuilder; overload;
    function SetStandaloneAction(const Action: TCommandAction): ICliApplicationBuilder; overload;
    function SetStandaloneAction(const Command: ICommand; const Action: TCommandAction): ICliApplicationBuilder; overload;
    function ParseArgs: ICommandLineParseResult;
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
  System.Classes,
  System.IOUtils;

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
  FFailures := TAppFailures.Create;
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

function TCliApplicationBuilder.OnException(const Policy: TAppExceptionPolicy): ICliApplicationBuilder;
begin
  FFailures.SetPolicy(Policy);
  Result := Self;
end;

function TCliApplicationBuilder.HandleException(const E: Exception): Integer;
begin
  Result := FFailures.Handle(E);
end;

function TCliApplicationBuilder.SetAction(const Action: TCliAction): ICliApplicationBuilder;
begin
  Result := SetAction(FRootCommand, Action);
end;

procedure TCliApplicationBuilder.CheckNoAction(const Command: ICommand);
begin
  var Existing := Command.Action();
  if Assigned(Existing) then
    raise ECliCompositionError.CreateFmt('Command "%s" already has an action. Two ways of saying the ' +
      'same thing is a precedence rule nobody remembers -- keep one.', [Command.FullPath]);
end;

function TCliApplicationBuilder.SetAction(const Command: ICommand; const Action: TCliAction): ICliApplicationBuilder;
begin
  CheckNoAction(Command);
  Command.SetAction(ServiceAction(Action, FSlot));
  Result := Self;
end;

function TCliApplicationBuilder.SetStandaloneAction(const Action: TCommandAction): ICliApplicationBuilder;
begin
  Result := SetStandaloneAction(FRootCommand, Action);
end;

function TCliApplicationBuilder.SetStandaloneAction(const Command: ICommand;
  const Action: TCommandAction): ICliApplicationBuilder;
begin
  CheckNoAction(Command);
  Command.SetAction(Action);
  // Lo que queda sin host es el comando, no la accion: una que se le asigne despues lo sustituye
  // sin devolverle el host.
  FStandalone := FStandalone + [Command];
  Result := Self;
end;

function TCliApplicationBuilder.IsStandalone(const Command: ICommand): Boolean;
begin
  for var Each in FStandalone do
    if (Each as IInterface) = (Command as IInterface) then
      Exit(True);
  Result := False;
end;

function TCliApplicationBuilder.ParseArgs: ICommandLineParseResult;
begin
  if not Assigned(FParsed) then
  begin
    // Antes de parsear: lo que se parsea es lo que ya no puede cambiar.
    FRootCommand.Freeze;
    FParsed := FRootCommand.Parse(FArgs);
  end;
  Result := FParsed;
end;

function TCliApplicationBuilder.Build: ICliApplication;
begin
  // El parse va primero: la version, la ayuda, una invocacion que no encaja y un comando
  // standalone se resuelven sin el cerrojo de instancia unica, sin contenedor y sin cargar la
  // configuracion que la composicion no leyo.
  var Parsed := ParseArgs;
  if Parsed.VersionRequested or Parsed.HelpRequested or not Parsed.Succeeded or
    IsStandalone(Parsed.Command) then
    Exit(TCliApplication.Create(nil, FRootCommand, Parsed, FFailures, FSlot, 0));

  // En el contenedor, para que un servicio lo pida por constructor: es el mismo que recibe la
  // accion.
  FImpl.Services.AddSingleton<ICommandLineParseResult>(Parsed);

  // Antes de cargar y de construir el contenedor. Una aplicacion que decide no arrancar termina
  // sin haber construido nada, y lo dice por su codigo de salida.
  var Lock: THandle;
  if not TInstanceLock.Acquire(FSingleInstanceId, Lock) then
    Exit(TCliApplication.CreateSettled(TCliExit.AlreadyRunning));

  var Host: IHost;
  try
    Host := FImpl.Build;
  except
    on E: Exception do
    begin
      // Sin aplicacion que lo suelte al destruirse: una construccion que falla no deja el cerrojo
      // tomado para el resto del proceso.
      TInstanceLock.Release(Lock);
      // El host informa por el canal de arranque todo lo que hace fallar su construccion.
      FFailures.MarkReported(E);
      raise;
    end;
  end;
  Result := TCliApplication.Create(Host, FRootCommand, Parsed, FFailures, FSlot, Lock);
end;

{ TCliApplication }

class function TCliApplication.CreateBuilder(const Args: TArray<string>): ICliApplicationBuilder;
begin
  Result := TCliApplicationBuilder.Create(Args);
end;

constructor TCliApplication.Create(const Host: IHost; const RootCommand: IRootCommand;
  const Parsed: ICommandLineParseResult; const Failures: IAppFailures;
  const Slot: IServicesSlot; const InstanceLock: THandle);
begin
  inherited Create;
  FHost := Host;
  FRootCommand := RootCommand;
  FParsed := Parsed;
  FFailures := Failures;
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
  TInstanceLock.Release(FInstanceLock);
  inherited;
end;

function TCliApplication.GetServices: IServiceProvider;
begin
  if Assigned(FHost) then
    Result := FHost.Services
  else
    Result := nil;
end;

procedure TCliApplication.Settle(const Code: Integer);
begin
  // El gancho del RTL puede llegar desde otro hilo: el OnDestroy de un modulo de datos que se
  // libera alli.
  TMonitor.Enter(Self);
  try
    if not FFailed then
    begin
      FFailed := True;
      FFailedCode := Code;
    end;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TCliApplication.HandleRtlException(Sender: TObject);
begin
  // Lo que el RTL entrega aqui es la excepcion que su except esta manejando.
  if ExceptObject is Exception then
    Settle(FFailures.Decide(Exception(ExceptObject), FSlot.Services, False));
end;

function TCliApplication.Run: Integer;
var
  Configuration: TInvocationConfiguration;
  PreviousHook: TNotifyEvent;
  Started: Boolean;
begin
  if FSettled then Exit(FSettledCode);
  Result := TCliExit.Ok;

  // Lo que se escapa es de la politica: con el manejador de la invocacion puesto, la politica no
  // lo veria nunca.
  Configuration.EnableDefaultExceptionHandler := False;

  // Lo que el RTL no tiene a quien entregar va a la politica mientras corre, como hace la VCL.
  PreviousHook := ApplicationHandleException;
  ApplicationHandleException := HandleRtlException;
  try
    // Sin host, Build ya vio que el parse pidio la version o la ayuda, o que no encajaba: lo
    // resuelve la invocacion sin ejecutar la accion de ningun comando.
    if not Assigned(FHost) then
      try
        Result := FParsed.Invoke(Configuration);
      except
        on E: Exception do
          Settle(FFailures.Decide(E, nil, False));
      end
    else
      try
        Started := False;
        try
          FHost.Start;
          Started := True;
          FSlot.Fill(FHost.Services);
          Result := FParsed.Invoke(Configuration);
        except
          // El host informa por el canal de arranque lo que hace fallar su arranque.
          on E: Exception do
            Settle(FFailures.Decide(E, FHost.Services, not Started));
        end;
      finally
        // Antes de parar: el soporte vive lo que vivan los comandos, y el proveedor no debe
        // sobrevivir a Stop dentro de el.
        FSlot.Clear;
        try
          StopAndNil(FHost);
        except
          // Si ya habia fallado algo, el codigo es el que la politica dio a ese fallo, y Stop ya
          // informo el suyo.
          on E: Exception do
            if not FFailed then
              Settle(FFailures.Decide(E, nil, True));
        end;
      end;
  finally
    ApplicationHandleException := PreviousHook;
  end;

  if FFailed then
    Result := FFailedCode;
end;
end.
