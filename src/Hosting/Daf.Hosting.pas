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
  /// El componedor generico de la familia, en estilo inmediato: al volver del constructor sus
  /// propiedades ya estan vivas, y lo registrado ocurre donde esta escrito.
  /// </summary>
  /// <remarks>
  ///   Baja a detalle de implementacion: la maquinaria de construccion es la misma --las cinco
  ///   Initialize*, las dos fases y el informe por el canal de arranque--, lo que cambia es
  ///   quien la llama y cuando. Tres Initialize* suben al constructor; las dos que dependen de
  ///   lo que el usuario anada despues se quedan en Build.
  /// </remarks>
  THostBuilder = class(TInterfacedObject, IGenericHostBuilder, IHostApplicationBuilder)
  private
    // "Build ya se llamo" es de este builder, no del proceso: como class var, un proceso
    // solo podia construir un host en toda su vida, y un arnes de specs construye varios.
    FHostBuilt: Boolean;
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
    procedure InitializeHostConfiguration;
    procedure InitializeHostingEnvironment;
    procedure InitializeDefaultAppConfiguration;
    procedure InitializeHostBuilderContext;
    procedure InitializeAppConfiguration;
    procedure InitializeServiceProvider;
  public
    /// <param name="Args">
    ///   Los argumentos del proceso, que entran como fuente de configuracion. Una variante que
    ///   declare gramatica de linea de comandos NO debe pasarlos aqui: serian dos lecturas del
    ///   mismo argv con reglas distintas, y discrepan por construccion.
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
  private
    FStartedCTS: ICancellationTokenSource;
    FStoppingCTS: ICancellationTokenSource;
    FStoppedCTS: ICancellationTokenSource;
  public
    constructor Create;
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
  System.IOUtils,
  System.SyncObjs,
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

constructor THostEnvironment.Create(const HostConfiguration: IConfiguration);
begin
  Assert(Assigned(HostConfiguration), 'THostEnvironment needs the host configuration to derive from');

  // Las tres claves conservan el nombre que ya tenian, asi que la variable de entorno de
  // siempre sigue valiendo -- entra por el proveedor de entorno, que las publica sin quitarles
  // el prefijo-- y gana ademas la forma --DAF_APP_ENV=Development.
  TEnvironmentName.TryParse(HostConfiguration[TDafEnvVars.APP_ENV], FEnvironmentName);
  if EnvironmentName.IsEmpty then
    EnvironmentName := TEnvironments.Production;

  ApplicationName := HostConfiguration[TDafEnvVars.APP_NAME];
  if ApplicationName.IsEmpty then
    ApplicationName := TPath.GetFileNameWithoutExtension(ParamStr(0));

  ContentRootPath := HostConfiguration[TDafEnvVars.CONTENT_ROOT];
  if ContentRootPath.IsEmpty then
    ContentRootPath := TPath.GetAppPath;
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
  FReleased := True;
  try
    Stop;
  except
    on E: Exception do
      ; // Ignorar errores en shutdown forzado
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
begin
  if FStopped then Exit;
  // El logger se resuelve antes de empezar: a mitad de la parada el proveedor ya se ha soltado, y
  // entonces no habria de donde sacarlo.
  var Logger := ReportLogger(FServices, ClassName);
  TMonitor.Enter(Self);
  try
    try
      FStopped := True;

      // Antes de cualquier cosa que pueda lanzar: el TProc de clase sostiene el lifetime de
      // este host, asi que dejarlo puesto deja un host parado atendiendo la señal.
      TShutdownHook.OnShutdownRequested := nil;

      var Lifetime := GetServices.GetService<IHostApplicationLifetime>;

      if Assigned(Lifetime) then
        (Lifetime as THostApplicationLifetime).NotifyStopping;

      // LIFO, el mismo orden en que cierra el contenedor: un servicio arrancado despues puede
      // estar usando lo que suelta al parar uno arrancado antes.
      for var Idx := FHostedServices.Count - 1 downto 0 do
        FHostedServices[Idx].Stop;
      FHostedServices.Clear;

      if Assigned(Lifetime) then
        (Lifetime as THostApplicationLifetime).NotifyStopped;
      FServices := nil;
      FContainerAccess.Shutdown;
    except
      on E: Exception do
      begin
        Logger.LogError(E, 'the application could not stop cleanly');
        raise;
      end;
    end;
  finally
    TMonitor.Exit(Self);
  end;
  Free;
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

  // Las tres que no dependen de lo que el usuario anada despues suben aqui: al volver, la
  // configuracion y el entorno YA responden. Es lo que distingue al estilo inmediato, y lo que
  // permite leer el entorno para decidir que fuente anadir sin ningun callback.
  InitializeHostConfiguration;
  InitializeHostingEnvironment;
  InitializeDefaultAppConfiguration;
  InitializeHostBuilderContext;

  // Vivos desde ya, que es lo que el estilo inmediato promete: Services y Logging responden al
  // volver del constructor, sin haber construido nada.
  FServices := TServiceCollection.Create;
  FLoggingImpl := TLoggingBuilder.Create;
  FLogging := FLoggingImpl;
end;

destructor THostBuilder.Destroy;
begin
  inherited;
end;

function THostBuilder.GetProperties: TDictionary<string, TValue>;
begin
  Result := FHostBuilderContext.Properties
end;

function THostBuilder.GetServices: IServiceCollection;
begin
  Result := FServices;
end;

function THostBuilder.GetConfiguration: IConfigurationManager;
begin
  Result := FConfiguration;
end;

function THostBuilder.GetEnvironment: IHostEnvironment;
begin
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
  Result := TServiceProviderOptions.Create(FEnvironment.IsDevelopment, FEnvironment.IsDevelopment);
end;

procedure THostBuilder.SetServiceProviderOptions(const Value: TServiceProviderOptions);
begin
  FServiceProviderOptions := Value;
  FServiceProviderOptionsFixed := True;
end;

procedure THostBuilder.InitializeHostConfiguration;
begin
  // Una variante aporta sus fuentes SEMBRANDO el manager y entregandolo, no encadenando una
  // segunda configuracion. Si el componedor generico lo fabricase siempre y acto seguido
  // derivase el entorno de el, lo que la variante anadiera despues llegaria tarde.
  if not Assigned(FConfiguration) then
    FConfiguration := TConfigurationManager.Create;

  // El orden es el del linaje, y decide quien gana: primero el entorno del proceso, despues los
  // argumentos. Sin quitar el prefijo, para que DAF_APP_ENV se lea con ese mismo nombre venga
  // de donde venga.
  FConfiguration.Add(TEnvironmentVariablesConfigurationSource.Create('DAF_', True, False));

  if Length(FArgs) > 0 then
    CommandLineConfig.AddArgs(FConfiguration, FArgs);
end;

procedure THostBuilder.InitializeHostingEnvironment;
begin
  FEnvironment := THostEnvironment.Create(FConfiguration as IConfiguration);
end;

procedure THostBuilder.InitializeDefaultAppConfiguration;
begin
  // Las fuentes de aplicacion de Host.CreateApplicationBuilder, en su orden: los dos ficheros
  // del content root, que necesitan el entorno ya resuelto; todas las variables de entorno; y
  // otra vez los argumentos, que tienen que ganar a todo lo anterior.
  JsonConfig.AddFile(FConfiguration,
    TPath.Combine(FEnvironment.ContentRootPath, 'appsettings.json'), [csoOptional]);
  JsonConfig.AddFile(FConfiguration,
    TPath.Combine(FEnvironment.ContentRootPath, 'appsettings.' + FEnvironment.EnvironmentName + '.json'),
    [csoOptional]);
  FConfiguration.Add(TEnvironmentVariablesConfigurationSource.Create('', True, False));
  if Length(FArgs) > 0 then
    CommandLineConfig.AddArgs(FConfiguration, FArgs);
end;

procedure THostBuilder.InitializeHostBuilderContext;
begin
  FHostBuilderContext := THostBuilderContext.Create(FEnvironment, FConfiguration as IConfiguration);
end;

procedure THostBuilder.InitializeAppConfiguration;
begin
  // Queda una fase, no un objeto: la configuracion es la misma de siempre, y este es el momento
  // en que lo que el usuario anadio entre el constructor y Build pasa a ser lo definitivo para
  // el contexto. Antes aqui se construia una segunda configuracion y se encadenaba la de host.
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
  // configure logging -- que hoy son todas menos las que lo hacen a proposito.
  if FLoggingTouched then
    AddLogging(FServices, FLoggingImpl);

  FAppServices := FServices.BuildServiceProvider(GetServiceProviderOptions);
end;

function THostBuilder.Build: IHost;
begin
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

constructor THostApplicationLifetime.Create;
begin
  inherited;
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

procedure THostApplicationLifetime.StopApplication;
begin
  FStoppingCTS.Cancel;
end;

procedure THostApplicationLifetime.NotifyStarted;
begin
  FStartedCTS.Cancel;
end;

procedure THostApplicationLifetime.NotifyStopping;
begin
  FStoppingCTS.Cancel;
end;

procedure THostApplicationLifetime.NotifyStopped;
begin
  FStoppedCTS.Cancel;
end;

end.

