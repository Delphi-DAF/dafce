unit Daf.Hosting;

interface

uses
  System.Generics.Collections,
  System.Rtti,
  System.SysUtils,
  Daf.Threading,
  Daf.Extensions.Hosting,
  Daf.Extensions.Configuration,
  Daf.Extensions.Logging,
  // TLoggingBuilder es campo del componedor, asi que la unit concreta hace falta ya aqui.
  Daf.Logging.Builder,
  Daf.Extensions.DependencyInjection;

type
  THostEnvironment = class(TInterfacedObject, IHostEnvironment)
  strict private
    FEnvironmentName: TEnvironmentName;
    FApplicationName: string;
    FContentRootPath: string;
    function GetEnvironmentName: TEnvironmentName;
    procedure SetEnvironmentName(const Value: TEnvironmentName);
    function GetApplicationName: string;
    procedure SetApplicationName(const Value: string);
    function GetContentRootPath: string;
    procedure SetContentRootPath(const Value: string);
  private
    function GetBinPath: string;
    function GetEnvVar(VarName: string): string;
  public
    /// <summary>
    /// El entorno SALE de la configuracion de host, como en el linaje: las tres claves las
    /// resuelve cualquier proveedor que este en ella, incluida la linea de comandos.
    /// </summary>
    constructor Create(const HostConfiguration: IConfiguration);
    function IsDevelopment: Boolean;
    function IsStaging: Boolean;
    function IsProduction: Boolean;
    function IsTesting: Boolean;
    property EnvironmentName: TEnvironmentName read GetEnvironmentName write SetEnvironmentName;
    property ApplicationName: string read GetApplicationName write SetApplicationName;
    property ContentRootPath: string read GetContentRootPath write SetContentRootPath;
    property BinPath: string read GetBinPath;
    property EnvVar[VarName: string]: string read GetEnvVar;default;
  end;

  /// <summary>El componedor del host generico: el contrato compartido mas su propio Build.</summary>
  /// <remarks>
  ///   Cada variante de la familia declara su Build en su propia interfaz porque cada uno devuelve
  ///   un tipo distinto. Un Build comun en IHostApplicationBuilder se puede DECLARAR y no se puede
  ///   implementar: Delphi distingue sobrecargas por parametros y no por tipo de retorno, y una
  ///   interfaz no admite covarianza de retorno, asi que ninguna clase cumple los dos.
  /// </remarks>
  IGenericHostBuilder = interface(IHostApplicationBuilder)
    ['{4851D11B-9A47-4C5F-BF9D-EB46C278517D}']
    /// <summary>Construye el host con lo registrado hasta aqui.</summary>
    function Build: IHost;
  end;

  THost = class(TNoRefCountObject, IHost)
  private
    FServices: IServiceProviderImpl;
    [Weak]
    FContainerAccess: IContainerAccess;
    FHostedServices: TList<IHostedService>;
    // Estado de este host, no del proceso: con class var, dos hosts en un mismo proceso
    // se pisaban el "ya parado" el uno al otro.
    FStopped: Boolean;
    FReleased: Boolean;
    procedure SetServiceProvider(const Services: IServiceProvider);
  public
    /// <summary>El punto de entrada del host generico: entrega el componedor ya con dueño, asi que
    /// una sola variable lo posee y llega a Build.</summary>
    class function CreateBuilder(const Args: TArray<string> = nil): IGenericHostBuilder; static;
    constructor Create;
    destructor Destroy; override;
    function GetServices: IServiceProvider;
    procedure Start;
    procedure Stop;
    procedure WaitForShutdown;
  end;

  THostBuilderContext = class(TInterfacedObject, IHostBuilderContext)
  private
    FProperties: TDictionary<string, TValue>;
    FConfiguration: IConfiguration;
    FEnvironment: IHostEnvironment;
    function GetProperties: TDictionary<string, TValue>;
    function GetConfiguration: IConfiguration;
    function GetEnvironment: IHostEnvironment;
    procedure SetConfiguration(const Value: IConfiguration);
  public
    constructor Create(const Environment: IHostEnvironment; const Configuration: IConfiguration);
    destructor Destroy; override;
    property Properties: TDictionary<string, TValue> read GetProperties;
    property Configuration: IConfiguration read GetConfiguration write SetConfiguration;
  end;

  /// <summary>
  /// El componedor generico de la familia, en estilo inmediato: sus propiedades responden en
  /// cuanto se leen, y lo registrado ocurre donde esta escrito.
  /// </summary>
  /// <remarks>
  ///   La configuracion de host, el entorno y las fuentes por defecto se resuelven en la primera
  ///   lectura de Configuration, Environment, Properties o del valor por defecto de
  ///   ServiceProviderOptions, o en Build si la composicion no los lee: crear el builder no abre
  ///   ningun fichero. Una fuente que el host anade por su cuenta y falla se informa al abrirla, y
  ///   su excepcion sale en Build.
  /// </remarks>
  THostBuilder = class(TInterfacedObject, IGenericHostBuilder, IHostApplicationBuilder)
  private
    // "Build ya se llamo" es de este builder, no del proceso: como class var, un proceso
    // solo podia construir un host en toda su vida, y un arnes de specs construye varios.
    FHostBuilt: Boolean;
    // Sigue en True despues de Build, que suelta los campos: un getter posterior no vuelve a cargar.
    FConfigurationResolved: Boolean;
    // La primera fuente del host que fallo. La relanza Build; si Build no corre, se libera con el
    // builder.
    FSourceFailure: Exception;
    FArgs: TArray<string>;
    FEnvironment: IHostEnvironment;
    FHostBuilderContext: IHostBuilderContext;
    // UNA sola configuracion, y nace prepoblada.
    FConfiguration: IConfigurationManager;
    FServices: IServiceCollection;
    FAppServices: IServiceProvider;
    FLogging: ILoggingBuilder;
    // La misma instancia que FLogging, por su clase: HasProviders no esta en la interfaz a
    // proposito --lo pregunta quien acaba de componer, no el consumidor--.
    FLoggingImpl: TLoggingBuilder;
    // Componer logging sin proveedores es un error, asi que solo se compone si alguien toco la
    // propiedad. Llamar a AddLogging por sistema romperia toda aplicacion que no lo configure.
    FLoggingTouched: Boolean;
    FServiceProviderOptions: TServiceProviderOptions;
    FServiceProviderOptionsFixed: Boolean;
    function GetProperties: TDictionary<string, TValue>;
    function GetServices: IServiceCollection;
    function GetConfiguration: IConfigurationManager;
    function GetEnvironment: IHostEnvironment;
    function GetLogging: ILoggingBuilder;
    function GetServiceProviderOptions: TServiceProviderOptions;
    procedure SetServiceProviderOptions(const Value: TServiceProviderOptions);
    procedure EnsureConfiguration;
    procedure AddHostSource(const Add: TProc);
    procedure AddHostFile(const FileName: string);
    procedure InitializeHostConfiguration;
    procedure InitializeHostingEnvironment;
    procedure InitializeDefaultAppConfiguration;
    procedure InitializeHostBuilderContext;
    procedure InitializeAppConfiguration;
    procedure InitializeServiceProvider;
  public
    /// <param name="Args">
    ///   Los argumentos del proceso, que entran como fuente de configuracion, como en .NET. Una
    ///   variante con gramatica los pasa tambien: la configuracion y la gramatica son dos lecturas
    ///   del mismo argv con reglas distintas, y lo que la gramatica declara se lee de su parse.
    /// </param>
    /// <param name="Configuration">
    ///   Un manager ya sembrado, para que una variante pueda aportar sus fuentes ANTES de que
    ///   el entorno se derive de el. Si no se da ninguno, se fabrica.
    /// </param>
    constructor Create(const Args: TArray<string> = nil;
      const Configuration: IConfigurationManager = nil);
    destructor Destroy;override;
    function Build: IHost;
    property Services: IServiceCollection read GetServices;
    property Configuration: IConfigurationManager read GetConfiguration;
    property Environment: IHostEnvironment read GetEnvironment;
    property Logging: ILoggingBuilder read GetLogging;
    property Properties: TDictionary<string, TValue> read GetProperties;
    property ServiceProviderOptions: TServiceProviderOptions read GetServiceProviderOptions
      write SetServiceProviderOptions;
  end;

  THostApplicationLifetime = class(TInterfacedObject, IHostApplicationLifetime)
  private
    FServices: IServiceProviderImpl;
    FStartedCTS: ICancellationTokenSource;
    FStoppingCTS: ICancellationTokenSource;
    FStoppedCTS: ICancellationTokenSource;
    procedure Notify(const Source: ICancellationTokenSource; const Failure: string);
  public
    constructor Create(const Services: IServiceProvider);
    destructor Destroy; override;

    function ApplicationStarted: ICancellationToken;
    function ApplicationStopping: ICancellationToken;
    function ApplicationStopped: ICancellationToken;
    procedure StopApplication;

    procedure NotifyStarted;
    procedure NotifyStopping;
    procedure NotifyStopped;
  end;

implementation
uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  System.IOUtils,
  System.SyncObjs,
  System.Threading,
  System.Types,
  Daf.DependencyInjection,
  Daf.Configuration.Manager,
  Daf.Configuration.Env,
  Daf.Configuration.Json,
  Daf.Configuration.CommandLine;

// El relevo del arranque. Antes de BuildServiceProvider informa el canal de arranque, porque no hay
// otra cosa; en cuanto el proveedor existe, informar es cosa del pipeline de ILogger, con los
// proveedores y la configuracion que el usuario escribio -- seguir en el canal seria saltarsela.
//
// El canal se queda de respaldo y no de sustituido: que el proveedor exista no garantiza que se
// pueda resolver un logger, y quedarse callado justo ahi seria reabrir el mismo agujero un paso mas
// adelante.
function ReportLogger(const Services: IServiceProviderImpl; const Category: string): ILogger;
begin
  Result := nil;
  try
    if Assigned(Services) then
    begin
      var Provider: IServiceProvider := Services;
      var Factory := Provider.GetService<ILoggerFactory>;
      if Assigned(Factory) then
        Result := Factory.CreateLogger(Category);
    end;
  except
    Result := nil;
  end;
  if not Assigned(Result) then
    Result := BootstrapLog.LoggerFor(Category);
end;


{ THostEnvironment }

// La de .NET: el directorio de trabajo, salvo que sea el del sistema, que es donde arranca un
// servicio de Windows y donde no hay nada suyo; entonces, la carpeta del ejecutable.
function DefaultContentRoot: string;
begin
  Result := TDirectory.GetCurrentDirectory;
  {$IFDEF MSWINDOWS}
  var Buffer: array[0..MAX_PATH] of Char;
  var SystemDir: string;
  SetString(SystemDir, Buffer, GetSystemDirectory(Buffer, Length(Buffer)));
  if SameText(ExcludeTrailingPathDelimiter(Result), ExcludeTrailingPathDelimiter(SystemDir)) then
    Result := TPath.GetAppPath;
  {$ENDIF}
end;

constructor THostEnvironment.Create(const HostConfiguration: IConfiguration);
begin
  Assert(Assigned(HostConfiguration), 'THostEnvironment needs the host configuration to derive from');

  // Como en .NET: el nombre se toma tal cual, y cualquiera que no sea uno de los conocidos es un
  // entorno mas.
  EnvironmentName := HostConfiguration[THostDefaults.EnvironmentKey];
  if EnvironmentName.IsEmpty then
    EnvironmentName := TEnvironments.Production;

  ApplicationName := HostConfiguration[THostDefaults.ApplicationKey];
  if ApplicationName.IsEmpty then
    ApplicationName := TPath.GetFileNameWithoutExtension(ParamStr(0));

  // Como en .NET: una raiz relativa cuelga de la carpeta del ejecutable, no del directorio de
  // trabajo.
  ContentRootPath := HostConfiguration[THostDefaults.ContentRootKey];
  if ContentRootPath.IsEmpty then
    ContentRootPath := DefaultContentRoot
  else if not TPath.IsPathRooted(ContentRootPath) then
    ContentRootPath := TPath.Combine(BinPath, ContentRootPath);
  ContentRootPath := IncludeTrailingPathDelimiter(ContentRootPath);
end;

function THostEnvironment.GetEnvironmentName: TEnvironmentName;
begin
  Result := FEnvironmentName;
end;

procedure THostEnvironment.SetEnvironmentName(const Value: TEnvironmentName);
begin
  FEnvironmentName := Trim(Value);
end;

function THostEnvironment.GetEnvVar(VarName: string): string;
begin
  Result := GetEnvironmentVariable(VarName);
end;

function THostEnvironment.GetBinPath: string;
begin
  Result := IncludeTrailingPathDelimiter(TPath.GetAppPath);
end;

function THostEnvironment.GetApplicationName: string;
begin
  Result := FApplicationName;
end;

procedure THostEnvironment.SetApplicationName(const Value: string);
begin
  FApplicationName := Trim(Value);
end;

function THostEnvironment.GetContentRootPath: string;
begin
  Result := FContentRootPath;
end;

procedure THostEnvironment.SetContentRootPath(const Value: string);
begin
  FContentRootPath := Trim(Value);
end;

function THostEnvironment.IsDevelopment: Boolean;
begin
  Result := EnvironmentName.IsDevelopment;
end;

function THostEnvironment.IsProduction: Boolean;
begin
  Result := EnvironmentName.IsProduction;
end;

function THostEnvironment.IsStaging: Boolean;
begin
  Result := EnvironmentName.IsStaging;
end;

function THostEnvironment.IsTesting: Boolean;
begin
  Result := EnvironmentName.IsTesting;
end;

{ THost }

class function THost.CreateBuilder(const Args: TArray<string>): IGenericHostBuilder;
begin
  Result := THostBuilder.Create(Args);
end;

constructor THost.Create;
begin
  inherited Create;
  FHostedServices := TList<IHostedService>.Create;;
  FStopped := False;
  FReleased := False;
end;

destructor THost.Destroy;
begin
  // Antes de Stop: es lo que le dice que no se libere, porque ya se esta liberando.
  FReleased := True;
  try
    Stop;
  except
    on E: Exception do
      ; // Stop ya lo informo al ocurrir, y un destructor que lanza deja el objeto a medio liberar.
  end;
  FHostedServices.Free;
  inherited;
end;

procedure THost.SetServiceProvider(const Services: IServiceProvider);
begin
  if Assigned(FServices) then
    raise Exception.Create('ServiceProvider already assigned');
  FServices := Services;
  if Assigned(FServices) then
  begin
    FContainerAccess := (Services.GetRequiredService<IServiceScopeFactory> as IContainerAccess);
    Assert(Assigned(FContainerAccess));
  end;
end;

function THost.GetServices: IServiceProvider;
begin
  Result := FServices;
end;

procedure THost.Start;
begin
  try
    // Fase 2 del arranque, en orden: primero las tareas de inicializacion, porque un
    // servicio alojado puede depender del estado que preparan; despues los servicios
    // alojados; y solo entonces se declara arrancada la aplicacion. Si una tarea lanza,
    // la excepcion sale de aqui y no se arranca nada.
    for var StartupTask in GetServices.GetServices<IStartupTask> do
      StartupTask.Run;
    for var HostedService in FHostedServices do
      HostedService.Start;
    var Lifetime := GetServices.GetRequiredService<IHostApplicationLifetime>;
    if Assigned(Lifetime) then
      (Lifetime as THostApplicationLifetime).NotifyStarted;

    // UNA sola señal de apagado: el manejador de consola pide la parada por el mismo camino
    // que la pide el codigo --StopApplication--, y quien la espera observa ApplicationStopping.
    //
    // El TProc es de clase, asi que con dos hosts vivos a la vez el ultimo en arrancar atiende
    // la señal y el primero en pararse la libera. Un proceso con dos hosts es cosa de un arnes
    // de specs, y el peor caso es un Ctrl+C que no para nada.
    TShutdownHook.OnShutdownRequested :=
      procedure
      begin
        Lifetime.StopApplication;
      end;
  except
    on E: Exception do
    begin
      ReportLogger(FServices, ClassName).LogError(E, 'the application could not start');
      raise;
    end;
  end;
end;

procedure THost.Stop;
var
  Logger: ILogger;
  Failures: TArray<Exception>;

  // Cada paso que falla se informa y se guarda, y la parada sigue: un paso no impide los
  // siguientes.
  procedure Collect(const E: Exception);
  begin
    AcquireExceptionObject;
    Failures := Failures + [E];
    Logger.LogError(E, 'the application could not stop cleanly');
  end;

begin
  if FStopped then Exit;
  // El logger se resuelve antes de empezar: a mitad de la parada el proveedor ya se ha soltado, y
  // entonces no habria de donde sacarlo.
  Logger := ReportLogger(FServices, ClassName);
  TMonitor.Enter(Self);
  try
    FStopped := True;

    // Antes de cualquier cosa que pueda lanzar: el TProc de clase sostiene el lifetime de
    // este host, asi que dejarlo puesto deja un host parado atendiendo la señal.
    TShutdownHook.OnShutdownRequested := nil;

    var Lifetime: IHostApplicationLifetime := nil;
    try
      Lifetime := GetServices.GetService<IHostApplicationLifetime>;
    except
      on E: Exception do Collect(E);
    end;
    if Assigned(Lifetime) then
      (Lifetime as THostApplicationLifetime).NotifyStopping;

    // LIFO, el mismo orden en que cierra el contenedor: un servicio arrancado despues puede
    // estar usando lo que suelta al parar uno arrancado antes.
    for var Idx := FHostedServices.Count - 1 downto 0 do
      try
        FHostedServices[Idx].Stop;
      except
        on E: Exception do Collect(E);
      end;
    FHostedServices.Clear;

    if Assigned(Lifetime) then
      (Lifetime as THostApplicationLifetime).NotifyStopped;

    try
      FServices := nil;
      FContainerAccess.Shutdown;
    except
      on E: Exception do Collect(E);
    end;
  finally
    TMonitor.Exit(Self);
  end;

  if not FReleased then
    Free;

  // Con el host ya liberado. Una sola se lanza tal cual, para que quien la captura por su clase la
  // siga capturando; varias, dentro de una agregada, que es dueña de ellas.
  case Length(Failures) of
    0: ;
    1: raise Failures[0];
  else
    raise EAggregateException.Create(Format('the application could not stop cleanly: %d steps failed',
      [Length(Failures)]), Failures);
  end;
end;

procedure THost.WaitForShutdown;
begin
  // Lo primero que se ve por consola es como salir. El texto es traducible por la aplicacion.
  Writeln(TShutdownHook.PresCtlCMsg);
  // High(Cardinal) es INFINITE sin arrastrar Winapi.Windows a este unit.
  GetServices.GetRequiredService<IHostApplicationLifetime>
    .ApplicationStopping.WaitFor(High(Cardinal));
end;

{ THostBuilderContext }

constructor THostBuilderContext.Create(const Environment: IHostEnvironment; const Configuration: IConfiguration);
begin
  inherited Create;
  FProperties := TDictionary<string, TValue>.Create;
  FConfiguration := Configuration;
  FEnvironment := Environment;
end;

destructor THostBuilderContext.Destroy;
begin
  FProperties.Free;
  inherited;
end;

function THostBuilderContext.GetProperties: TDictionary<string, TValue>;
begin
  Result := FProperties;
end;

procedure THostBuilderContext.SetConfiguration(const Value: IConfiguration);
begin
  FConfiguration := Value;
end;

function THostBuilderContext.GetConfiguration: IConfiguration;
begin
  Result := FConfiguration;
end;

function THostBuilderContext.GetEnvironment: IHostEnvironment;
begin
  Result := FEnvironment;
end;

{THostBuilder}

constructor THostBuilder.Create(const Args: TArray<string>; const Configuration: IConfigurationManager);
begin
  inherited Create;
  FArgs := Args;
  FConfiguration := Configuration;

  // Vivos desde ya, que es lo que el estilo inmediato promete: Services y Logging responden al
  // volver del constructor, sin haber construido nada. La configuracion espera a su primera
  // lectura, en EnsureConfiguration.
  FServices := TServiceCollection.Create;
  FLoggingImpl := TLoggingBuilder.Create;
  FLogging := FLoggingImpl;
end;

destructor THostBuilder.Destroy;
begin
  // Una fuente que fallo sin que Build llegara a correr: su informe ya salio al abrirla.
  FSourceFailure.Free;
  inherited;
end;

function THostBuilder.GetProperties: TDictionary<string, TValue>;
begin
  EnsureConfiguration;
  Result := FHostBuilderContext.Properties
end;

function THostBuilder.GetServices: IServiceCollection;
begin
  Result := FServices;
end;

function THostBuilder.GetConfiguration: IConfigurationManager;
begin
  EnsureConfiguration;
  Result := FConfiguration;
end;

function THostBuilder.GetEnvironment: IHostEnvironment;
begin
  EnsureConfiguration;
  Result := FEnvironment;
end;

function THostBuilder.GetLogging: ILoggingBuilder;
begin
  // Pedirla ES tocarla: no hay otra razon para pedir el componedor de logging que anadirle
  // algo. Lo que se evita es llamar a AddLogging cuando nadie lo pidio, que levantaria el error
  // de composicion en toda aplicacion que no configure logging.
  FLoggingTouched := True;
  Result := FLogging;
end;

function THostBuilder.GetServiceProviderOptions: TServiceProviderOptions;
begin
  if FServiceProviderOptionsFixed then
    Exit(FServiceProviderOptions);
  EnsureConfiguration;
  Result := TServiceProviderOptions.Create(FEnvironment.IsDevelopment, FEnvironment.IsDevelopment);
end;

procedure THostBuilder.SetServiceProviderOptions(const Value: TServiceProviderOptions);
begin
  FServiceProviderOptions := Value;
  FServiceProviderOptionsFixed := True;
end;

procedure THostBuilder.EnsureConfiguration;
begin
  // El unico punto por el que se carga la configuracion. Una variante con gramatica contesta la
  // ayuda sin pasar por aqui si su composicion no la lee.
  if FConfigurationResolved then Exit;
  FConfigurationResolved := True;
  InitializeHostConfiguration;
  InitializeHostingEnvironment;
  InitializeDefaultAppConfiguration;
  InitializeHostBuilderContext;
end;

procedure THostBuilder.AddHostSource(const Add: TProc);
begin
  try
    Add;
  except
    // El manager ya lo informo al abrirla. Se guarda la primera y se sigue con las demas, asi que
    // una lectura durante la composicion responde con lo que si se pudo cargar.
    on E: Exception do
      if not Assigned(FSourceFailure) then
      begin
        AcquireExceptionObject;
        FSourceFailure := E;
      end;
  end;
end;

procedure THostBuilder.AddHostFile(const FileName: string);
begin
  AddHostSource(
    procedure
    begin
      JsonConfig.AddFile(FConfiguration, TPath.Combine(FEnvironment.ContentRootPath, FileName), [csoOptional]);
    end);
end;

procedure THostBuilder.InitializeHostConfiguration;
begin
  // Una variante aporta sus fuentes SEMBRANDO el manager y entregandolo, no encadenando una
  // segunda configuracion. Si el componedor generico lo fabricase siempre y acto seguido
  // derivase el entorno de el, lo que la variante anadiera despues llegaria tarde.
  if not Assigned(FConfiguration) then
    FConfiguration := TConfigurationManager.Create;

  // El orden es el del linaje, y decide quien gana: primero el entorno del proceso, despues los
  // argumentos. Las variables DAF_ se leen sin el prefijo, como las DOTNET_ en .NET, asi que
  // DAF_ENVIRONMENT y --environment fijan la misma clave.
  AddHostSource(
    procedure
    begin
      FConfiguration.Add(TEnvironmentVariablesConfigurationSource.Create('DAF_'));
    end);

  if Length(FArgs) > 0 then
    AddHostSource(
      procedure
      begin
        CommandLineConfig.AddArgs(FConfiguration, FArgs);
      end);
end;

procedure THostBuilder.InitializeHostingEnvironment;
begin
  FEnvironment := THostEnvironment.Create(FConfiguration as IConfiguration);
end;

procedure THostBuilder.InitializeDefaultAppConfiguration;
begin
  // Las fuentes de aplicacion de Host.CreateApplicationBuilder, en su orden: los ficheros del
  // content root, que necesitan el entorno ya resuelto; todas las variables de entorno; y otra
  // vez los argumentos, que tienen que ganar a todo lo anterior.
  AddHostFile('appsettings.json');
  AddHostFile('appsettings.' + FEnvironment.EnvironmentName + '.json');
  if FEnvironment.ApplicationName <> '' then
  begin
    // Un separador de ruta en el nombre sacaria el fichero del content root.
    var App := FEnvironment.ApplicationName.Replace('\', '_').Replace('/', '_');
    AddHostFile(App + '.settings.json');
    AddHostFile(App + '.settings.' + FEnvironment.EnvironmentName + '.json');
  end;
  AddHostSource(
    procedure
    begin
      FConfiguration.Add(TEnvironmentVariablesConfigurationSource.Create);
    end);
  if Length(FArgs) > 0 then
    AddHostSource(
      procedure
      begin
        CommandLineConfig.AddArgs(FConfiguration, FArgs);
      end);
end;

procedure THostBuilder.InitializeHostBuilderContext;
begin
  FHostBuilderContext := THostBuilderContext.Create(FEnvironment, FConfiguration as IConfiguration);
end;

procedure THostBuilder.InitializeAppConfiguration;
begin
  // Queda una fase, no un objeto: la configuracion es la misma de siempre, y este es el momento
  // en que lo que el usuario anadio entre el constructor y Build pasa a ser lo definitivo para
  // el contexto.
  THostBuilderContext(FHostBuilderContext).Configuration := FConfiguration as IConfiguration;
end;

procedure THostBuilder.InitializeServiceProvider;
begin
  // Se inyecta un placeholder de host, para que este disponible en el arranque
  // ya que al registrar no se deben solicitar servicios, solo registrar.
  FServices.AddSingleton<IHost, THost>;
  FServices.AddSingleton<IHostEnvironment>(FEnvironment);
  FServices.AddSingleton<IHostBuilderContext>(FHostBuilderContext);
  FServices.AddSingleton<IConfiguration>(FConfiguration as IConfiguration);
  FServices.AddSingleton<IHostApplicationLifetime, THostApplicationLifetime>;

  // Solo si alguien pidio la propiedad. AddLogging levanta el error de composicion cuando no se
  // le anadio ningun proveedor, asi que llamarlo por sistema romperia toda aplicacion que no
  // configure logging.
  if FLoggingTouched then
    AddLogging(FServices, FLoggingImpl);

  FAppServices := FServices.BuildServiceProvider(GetServiceProviderOptions);
end;

function THostBuilder.Build: IHost;
begin
  // Lo que la composicion no leyo se carga aqui. Una fuente del host que fallo ya se informo al
  // abrirla, asi que sale antes del try de abajo, que la informaria otra vez como fallo del host.
  EnsureConfiguration;
  if Assigned(FSourceFailure) then
  begin
    FHostBuilt := True;
    var Failure := FSourceFailure;
    FSourceFailure := nil;
    raise Failure;
  end;

  try
    if (FHostBuilt) then
      raise Exception.Create('Build already called');
    FHostBuilt := True;
    try
      InitializeAppConfiguration;
      InitializeServiceProvider;

      var Host := THost(FAppServices.GetRequiredService<IHost>);
      Host.SetServiceProvider(FAppServices);

      for var HostedService in FAppServices.GetServices<IHostedService> do
        Host.FHostedServices.Add(HostedService);

      Result := Host;

    finally
      // Lo que rompe los ciclos de refcount, y tiene que sobrevivir a la mudanza: el host
      // sostiene el proveedor y el proveedor construyo el host.
      FEnvironment := nil;
      FHostBuilderContext := nil;
      FConfiguration := nil;
      FLogging := nil;
      FLoggingImpl := nil;
      FServices := Default(IServiceCollection);
      FAppServices := nil;
    end;
  except
    on E: Exception do
    begin
      // Aqui se informa por el canal de arranque y no por el pipeline, y no es un descuido: este es
      // justo el tramo en el que el proveedor todavia no existe. Y despues se relanza, porque la
      // propagacion es la otra mitad del informe: es lo que hace que una aplicacion de escritorio
      // --sin consola y sin depurador-- lo ensene por el mecanismo de su plataforma.
      //
      // Y se pide por la misma via que se le ofrece a la aplicacion: la categoria dice quien y el
      // mensaje dice que, a los dos lados del relevo.
      BootstrapLog.LoggerFor(ClassName).LogError(E, 'the host could not be built');
      raise;
    end;
  end;
end;

{ THostApplicationLifetime }

constructor THostApplicationLifetime.Create(const Services: IServiceProvider);
begin
  inherited Create;
  FServices := Services;
  FStartedCTS := CreateCancellationTokenSource;
  FStoppingCTS := CreateCancellationTokenSource;
  FStoppedCTS := CreateCancellationTokenSource;end;

destructor THostApplicationLifetime.Destroy;
begin
  inherited;
end;

function THostApplicationLifetime.ApplicationStarted: ICancellationToken;
begin
  Result := FStartedCTS.Token;
end;

function THostApplicationLifetime.ApplicationStopping: ICancellationToken;
begin
  Result := FStoppingCTS.Token;
end;

function THostApplicationLifetime.ApplicationStopped: ICancellationToken;
begin
  Result := FStoppedCTS.Token;
end;

// Como ApplicationLifetime de .NET: lo que lanza un callback se informa y no sale de aqui, asi que no
// interrumpe el arranque ni la parada. Cada excepcion de la agregada va en su propia entrada, porque el
// mensaje de la agregada no nombra el fallo de ningun callback.
procedure THostApplicationLifetime.Notify(const Source: ICancellationTokenSource; const Failure: string);
begin
  try
    Source.Cancel;
  except
    on E: EAggregateException do
      for var I := 0 to E.Count - 1 do
        ReportLogger(FServices, ClassName).LogCritical(E.InnerExceptions[I], Failure);
    on E: Exception do
      ReportLogger(FServices, ClassName).LogCritical(E, Failure);
  end;
end;

procedure THostApplicationLifetime.StopApplication;
begin
  Notify(FStoppingCTS, 'An error occurred stopping the application');
end;

procedure THostApplicationLifetime.NotifyStarted;
begin
  Notify(FStartedCTS, 'An error occurred starting the application');
end;

procedure THostApplicationLifetime.NotifyStopping;
begin
  Notify(FStoppingCTS, 'An error occurred stopping the application');
end;

procedure THostApplicationLifetime.NotifyStopped;
begin
  Notify(FStoppedCTS, 'An error occurred stopping the application');
end;

end.

