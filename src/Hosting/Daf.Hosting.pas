unit Daf.Hosting;

interface

uses
  System.Generics.Collections,
  System.Rtti,
  System.SysUtils,
  Daf.Threading,
  Daf.Extensions.Hosting,
  Daf.Extensions.Configuration,
  Daf.Extensions.DependencyInjection;

type
  TSingleton<T: class> = class(TInterfacedObject)
  {$IFOPT D+}
  strict private
    class var FInstance: T;
  public
    class property Instance: T read FInstance;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  {$ENDIF}
  end;

  THostEnvironment = class(TSingleton<THostEnvironment>, IHostEnvironment)
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
    constructor Create;
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
    constructor Create;
    destructor Destroy; override;
    function GetServices: IServiceProvider;
    procedure Start;
    procedure Stop;
    procedure WaitForShutdown;
  end;

  THostBuilderContext = class(TSingleton<THostBuilderContext>, IHostBuilderContext)
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

  THostBuilder = class(TSingleton<THostBuilder>, IHostBuilder)
  private
    // "Build ya se llamo" es de este builder, no del proceso: como class var, un proceso
    // solo podia construir un host en toda su vida, y un arnes de specs construye varios.
    FHostBuilt: Boolean;
    FConfigureHostConfigActions: TList< TConfigureHostConfigAction>;
    FConfigureServicesActions: TList<TConfigureServicesAction>;
    FConfigureAppConfigActions: TList<TConfigureAppConfigAction>;
    FEnvironment: IHostEnvironment;
    FHostBuilderContext: IHostBuilderContext;
    FHostConfiguration: IConfiguration;
    FAppConfiguration: IConfiguration;
    FServices: IServiceCollection;
    FAppServices: IServiceProvider;
    function GetProperties: TDictionary<string, TValue>;
    procedure InitializeHostConfiguration;
    procedure InitializeHostingEnvironment;
    procedure InitializeHostBuilderContext;
    procedure InitializeAppConfiguration;
    procedure InitializeServiceProvider;
  protected
    procedure InternalConfigureServices;
    procedure InternalHostConfiguration(const ConfigBuilder: IConfigurationBuilder);
    procedure InternalAppConfiguration(ConfigBuilder: IConfigurationBuilder);

    procedure DoConfigureServices(Context: IHostBuilderContext; Services: IServiceCollection);virtual;
    procedure DoConfigureAppConfiguration(Context: IHostBuilderContext; Builder: IConfigurationBuilder);virtual;
    procedure DoConfigureHostConfiguration(Builder: IConfigurationBuilder);virtual;
  public
    constructor Create;
    destructor Destroy;override;
    function ConfigureServices(const Action: TConfigureServicesAction): IHostBuilder;
    function ConfigureAppConfiguration(const Action: TConfigureAppConfigAction): IHostBuilder;
    function ConfigureHostConfiguration(const ConfigAction:TConfigureHostConfigAction): IHostBuilder;
    function AddHostedService<T: class, IHostedService>: IHostBuilder;
    function UseProperty(const Key: string; const Value: TValue): IHostBuilder;
    function Build: IHost;
    property Properties: TDictionary<string, TValue> read GetProperties;
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
  Daf.Extensions.Logging,
  Daf.DependencyInjection,
  Daf.Configuration.Builder,
  Daf.Configuration.Chained,
  Daf.Configuration.Memory;

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


{$IFOPT D+}
{ TSingleton<T> }
procedure TSingleton<T>.AfterConstruction;
begin
  inherited;
  Assert(FInstance = nil);
  FInstance := Self as T;
end;

procedure TSingleton<T>.BeforeDestruction;
begin
  FInstance := nil;
  inherited;
end;
{$ENDIF}

{ THostEnvironment }

constructor THostEnvironment.Create;
begin
  TEnvironmentName.TryParse(GetEnvironmentVariable(TDafEnvVars.APP_ENV), FEnvironmentName);
  if EnvironmentName.IsEmpty then
    EnvironmentName := TEnvironments.Production;

  ApplicationName := GetEnvironmentVariable(TDafEnvVars.APP_NAME);
  if ApplicationName.IsEmpty then
    ApplicationName := TPath.GetFileNameWithoutExtension(ParamStr(0));

  ContentRootPath := GetEnvironmentVariable(TDafEnvVars.CONTENT_ROOT);
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

      var Lifetime := GetServices.GetService<IHostApplicationLifetime>;

      if Assigned(Lifetime) then
        (Lifetime as THostApplicationLifetime).NotifyStopping;

      for var HostedService in FHostedServices do
        HostedService.Stop;
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
  TShutdownHook.WaitForShutdown;
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

constructor THostBuilder.Create;
begin
  inherited;
  FConfigureHostConfigActions := TList<TConfigureHostConfigAction>.Create;
  FConfigureServicesActions := TList<TConfigureServicesAction>.Create;
  FConfigureAppConfigActions := TList<TConfigureAppConfigAction>.Create;
end;

destructor THostBuilder.Destroy;
begin
  FConfigureHostConfigActions.Free;
  FConfigureServicesActions.Free;
  FConfigureAppConfigActions.Free;
  inherited;
end;

function THostBuilder.GetProperties: TDictionary<string, TValue>;
begin
  Result := FHostBuilderContext.Properties
end;

function THostBuilder.AddHostedService<T>: IHostBuilder;
begin
  Assert(Supports(T, IHostedService));
  Result := Self;
  ConfigureServices(
    procedure(Builder: IHostBuilderContext; Services: IServiceCollection)
    begin
      Services.AddSingleton<IHostedService, T>;
    end);
end;

procedure THostBuilder.InitializeHostingEnvironment;
begin
  FEnvironment := THostEnvironment.Create;
end;

procedure THostBuilder.InitializeHostBuilderContext;
begin
  FHostBuilderContext := THostBuilderContext.Create(FEnvironment, FHostConfiguration);
end;

function THostBuilder.ConfigureHostConfiguration(const ConfigAction: TConfigureHostConfigAction): IHostBuilder;
begin
  FConfigureHostConfigActions.Add(ConfigAction);
  Result := Self;
end;

procedure THostBuilder.InternalHostConfiguration(const ConfigBuilder: IConfigurationBuilder);
begin
   ConfigureHostConfiguration( procedure(Builder: IConfigurationBuilder)
     begin
      DoConfigureHostConfiguration(Builder);
     end
  );
 for var Action in FConfigureHostConfigActions do
    Action(ConfigBuilder);
end;

procedure THostBuilder.DoConfigureHostConfiguration(Builder: IConfigurationBuilder);
begin

end;

procedure THostBuilder.InitializeHostConfiguration;
begin
  var ConfigBuilder: IConfigurationBuilder := TConfigurationBuilder.Create;

  MemoryConfig.AddCollection(ConfigBuilder);

  InternalHostConfiguration(ConfigBuilder);

  FHostConfiguration := ConfigBuilder.Build;
end;

function THostBuilder.ConfigureAppConfiguration(const Action: TConfigureAppConfigAction): IHostBuilder;
begin
  FConfigureAppConfigActions.Add(Action);
  Result := Self;
end;

procedure THostBuilder.InternalAppConfiguration(ConfigBuilder: IConfigurationBuilder);
begin
  ConfigureAppConfiguration( procedure(Context: IHostBuilderContext; Builder: IConfigurationBuilder)
     begin
      DoConfigureAppConfiguration(Context, Builder);
     end
  );

  for var Action in FConfigureAppConfigActions do
    Action(FHostBuilderContext, ConfigBuilder);
end;

procedure THostBuilder.DoConfigureAppConfiguration(Context: IHostBuilderContext; Builder: IConfigurationBuilder);
begin

end;

procedure THostBuilder.InitializeAppConfiguration;
begin
  var ConfigBuilder: IConfigurationBuilder := TConfigurationBuilder.Create;

  AddChainedConfiguration(ConfigBuilder, FHostConfiguration);

  InternalAppConfiguration(ConfigBuilder);

  FAppConfiguration := ConfigBuilder.Build();
  THostBuilderContext(FHostBuilderContext).Configuration := FAppConfiguration;
end;


function THostBuilder.ConfigureServices(const Action: TConfigureServicesAction): IHostBuilder;
begin
  FConfigureServicesActions.Add(Action);
  Result := Self;
end;

procedure THostBuilder.InternalConfigureServices;
begin
  ConfigureServices( procedure(Context: IHostBuilderContext; Services: IServiceCollection)
     begin
      DoConfigureServices(Context, Services);
     end
  );

  for var Action in FConfigureServicesActions do
    Action(FHostBuilderContext, FServices);
end;

procedure THostBuilder.DoConfigureServices(Context: IHostBuilderContext;  Services: IServiceCollection);
begin
end;

procedure THostBuilder.InitializeServiceProvider;
begin
  FServices := TServiceCollection.Create;
  // Se inyecta un placeholder de host, para que este disponible en ConfigureServices
  // ya que en ConfigureServices no se debe solicitar servicios, solo registrar.
  FServices.AddSingleton<IHost, THost>;
  FServices.AddSingleton<IHostEnvironment>(FEnvironment);
  FServices.AddSingleton<IHostBuilderContext>(FHostBuilderContext);
  FServices.AddSingleton<IConfiguration>(FAppConfiguration);
  FServices.AddSingleton<IHostApplicationLifetime, THostApplicationLifetime>;

  InternalConfigureServices;

  FAppServices := FServices.BuildServiceProvider;
end;

function THostBuilder.Build: IHost;
begin
  try
    if (FHostBuilt) then
      raise Exception.Create('IHostBuilder.Build already called');
    FHostBuilt := True;
    try
      InitializeHostConfiguration;
      InitializeHostingEnvironment;
      InitializeHostBuilderContext;
      InitializeAppConfiguration;
      InitializeServiceProvider;

      var Host := THost(FAppServices.GetRequiredService<IHost>);
      Host.SetServiceProvider(FAppServices);

      for var HostedService in FAppServices.GetServices<IHostedService> do
        Host.FHostedServices.Add(HostedService);

      Result := Host;

    finally
      FEnvironment := nil;
      FHostBuilderContext := nil;
      FHostConfiguration := nil;
      FAppConfiguration := nil;
      FServices := nil;
      FAppServices := nil;
      FreeAndNil(FConfigureHostConfigActions);
      FreeAndNil(FConfigureServicesActions);
      FreeAndNil(FConfigureAppConfigActions);
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

function THostBuilder.UseProperty(const Key: string; const Value: TValue): IHostBuilder;
begin
  FHostBuilderContext.Properties.AddOrSetValue(Key, Value);
  Result := Self;
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

