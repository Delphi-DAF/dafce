unit Daf.Web.Application;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Rtti,
  Web.HTTPApp,
  Daf.Extensions.Configuration,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Hosting,
  Daf.Extensions.Logging,
  Daf.Extensions.HttpServer,
  Daf.Web.Router,
  Daf.Hosting;

type
  /// <summary>Las claves con las que la variante se configura, y su valor por defecto.</summary>
  /// <remarks>
  ///   Se SIEMBRAN en el manager antes de entregarselo al componedor comun, asi que cualquier
  ///   variable de entorno o argumento posterior las pisa: es lo que hace que
  ///   <c>--Web:Port=9000</c> funcione sin que la variante tenga que mirar argv.
  /// </remarks>
  TWebDefaults = record
  public
    const PortKey = 'Web:Port';
    const DefaultPort = 8080;
  end;

  IWebApplication = interface(IInvokable)
    ['{43FF48C1-A0D8-4D53-89BC-1A8081F24EE5}']
    function GetServices: IServiceProvider;
    function GetServer: IDafHttpServer;
    /// <summary>
    /// Conecta el enrutado por atributos. Es MIEMBRO de la variante y no via de extension: lo
    /// que define a esta variante le pertenece, y asi el paquete generico no nombra a nadie.
    /// </summary>
    procedure MapControllers;
    /// <summary>
    /// DAF es dueño del proceso: arranca, sirve hasta que le digan que pare, y para. El .dpr
    /// acaba cuando Run acaba -- al contrario que en VCL y FMX, donde el bucle es de la
    /// plataforma y la variante solo delega.
    /// </summary>
    function Run: Integer;
    property Services: IServiceProvider read GetServices;
    property Server: IDafHttpServer read GetServer;
  end;

  IWebApplicationBuilder = interface(IHostApplicationBuilder)
    ['{027C56B1-3555-4E04-8186-8A8313D48D9E}']
    function Build: IWebApplication;
  end;

  /// <summary>
  /// La fachada con la que <c>Daf.Web</c> aporta su composicion de arranque.
  /// </summary>
  /// <remarks>
  ///   Fachada <c>class</c> y no un miembro del componedor generico, y la razon es de grafo: un
  ///   <c>AddHttpServer</c> en el builder comun obligaria al paquete generico a nombrar al
  ///   especializado, que es justo la direccion que rompe el grafo de paquetes. Aqui la
  ///   direccion es la buena -- el generico no nombra a nadie y el especializado nombra lo que
  ///   necesita. Es <c>class</c> y no <c>record</c> para que se pueda heredar y agrupar el
  ///   arranque repetido de una suite.
  /// </remarks>
  WebApp = class
  public
    /// <param name="WebModuleClass">
    ///   El modulo de WebBroker que atiende las peticiones. Por defecto el de la variante, que
    ///   se conecta al enrutado el solo.
    /// </param>
    class procedure AddHttpServer(const Services: IServiceCollection;
      const WebModuleClass: TComponentClass = nil); virtual;
  end;

  TWebApplicationBuilder = class(TInterfacedObject, IWebApplicationBuilder, IHostApplicationBuilder)
  private
    FImpl: THostBuilder;
    FOwned: IHostApplicationBuilder;
    function GetServices: IServiceCollection;
    function GetConfiguration: IConfigurationManager;
    function GetEnvironment: IHostEnvironment;
    function GetLogging: ILoggingBuilder;
    function GetProperties: TDictionary<string, TValue>;
    function GetServiceProviderOptions: TServiceProviderOptions;
    procedure SetServiceProviderOptions(const Value: TServiceProviderOptions);
  public
    constructor Create(const Args: TArray<string>);
    function Build: IWebApplication;
    property Services: IServiceCollection read GetServices;
    property Configuration: IConfigurationManager read GetConfiguration;
    property Environment: IHostEnvironment read GetEnvironment;
    property Logging: ILoggingBuilder read GetLogging;
    property Properties: TDictionary<string, TValue> read GetProperties;
    property ServiceProviderOptions: TServiceProviderOptions read GetServiceProviderOptions
      write SetServiceProviderOptions;
  end;

  TWebApplication = class(TInterfacedObject, IWebApplication)
  private
    FHost: IHost;
    FServer: IDafHttpServer;
    FRouter: IWebRouter;
  public
    class function CreateBuilder(const Args: TArray<string> = nil): IWebApplicationBuilder; static;
    constructor Create(const Host: IHost);
    destructor Destroy; override;
    function GetServices: IServiceProvider;
    function GetServer: IDafHttpServer;
    procedure MapControllers;
    function Run: Integer;
  end;

implementation

uses
  Daf.Configuration.Manager,
  Daf.Configuration.Memory,
  Daf.HttpServer,
  Daf.Threading;

var
  /// El puente con WebBroker, y vive AQUI a proposito. WebBroker construye el modulo web por su
  /// cuenta, sin pasar por el contenedor, asi que la unica forma de que llegue al enrutado es
  /// una referencia que la aplicacion deja puesta al mapear. No esta en la seccion de interfaz
  /// porque no es una via de acceso que la variante ofrezca -- es fontaneria de WebBroker, y la
  /// variante Web NO tiene acceso global, igual que la CLI.
  MappedRouter: IWebRouter = nil;

type
  /// El modulo web que la variante registra por defecto: se engancha al enrutado cuando WebBroker
  /// lo construye, y contesta 404 a lo que el enrutado declina.
  /// Desciende de TCustomWebDispatcher y no de TWebModule porque su constructor no busca un .dfm,
  /// y TWebModule.Create lo exige. CreateNew no sirve de atajo: se salta el constructor del
  /// despachador y deja sin crear las Actions que DispatchAction recorre en cuanto el enrutado
  /// declina una peticion.
  TDafWebModule = class(TCustomWebDispatcher)
  private
    procedure NotFound(Sender: TObject; Request: TWebRequest; Response: TWebResponse;
      var Handled: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
  end;

constructor TDafWebModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  var Fallback := Actions.Add;
  Fallback.Default := True;
  Fallback.OnAction := NotFound;
  if Assigned(MappedRouter) then
    MappedRouter.UseDispatcher(Self);
end;

procedure TDafWebModule.NotFound(Sender: TObject; Request: TWebRequest; Response: TWebResponse;
  var Handled: Boolean);
begin
  Response.StatusCode := 404;
  Response.Content := 'Not found';
  Handled := True;
end;

{ WebApp }

class procedure WebApp.AddHttpServer(const Services: IServiceCollection; const WebModuleClass: TComponentClass);
begin
  var ModuleClass := WebModuleClass;
  if ModuleClass = nil then
    ModuleClass := TDafWebModule;

  Services.AddSingleton<IWebRouter>(
    function(Provider: IServiceProvider): IWebRouter
    begin
      // El router descubre los controladores al construirse, asi que resolverlo ES mapearlos.
      Result := TWebRouter.Create(Provider.GetRequiredService<IServiceScopeFactory>,
        Provider.GetService<ILoggerFactory>);
    end);

  Services.AddSingleton<IDafHttpServer>(
    function(Provider: IServiceProvider): IDafHttpServer
    begin
      Result := TDafHttpServer.Create(ModuleClass,
        Provider.GetRequiredService<IConfiguration>.GetSection('Web'),
        Provider.GetService<ILoggerFactory>);
    end);
end;

{ TWebApplicationBuilder }

constructor TWebApplicationBuilder.Create(const Args: TArray<string>);
begin
  inherited Create;

  // La variante aporta sus fuentes SEMBRANDO el manager y entregandolo ya sembrado, no
  // encadenando una segunda configuracion. Si el componedor comun lo fabricase y acto seguido
  // derivase el entorno de el, lo que se anadiera despues llegaria tarde para el entorno.
  var Manager: IConfigurationManager := TConfigurationManager.Create;
  var Defaults := TDictionary<string, string>.Create;
  Defaults.Add(TWebDefaults.PortKey, TWebDefaults.DefaultPort.ToString);
  MemoryConfig.AddCollection(Manager, Defaults);

  // Los argumentos SI entran como configuracion aqui: esta variante no declara gramatica, asi
  // que no hay dos lecturas de argv con que chocar.
  FImpl := THostBuilder.Create(Args, Manager);
  FOwned := FImpl;
end;

function TWebApplicationBuilder.GetServices: IServiceCollection;
begin
  Result := FImpl.Services;
end;

function TWebApplicationBuilder.GetConfiguration: IConfigurationManager;
begin
  Result := FImpl.Configuration;
end;

function TWebApplicationBuilder.GetEnvironment: IHostEnvironment;
begin
  Result := FImpl.Environment;
end;

function TWebApplicationBuilder.GetLogging: ILoggingBuilder;
begin
  Result := FImpl.Logging;
end;

function TWebApplicationBuilder.GetProperties: TDictionary<string, TValue>;
begin
  Result := FImpl.Properties;
end;

function TWebApplicationBuilder.GetServiceProviderOptions: TServiceProviderOptions;
begin
  Result := FImpl.ServiceProviderOptions;
end;

procedure TWebApplicationBuilder.SetServiceProviderOptions(const Value: TServiceProviderOptions);
begin
  FImpl.ServiceProviderOptions := Value;
end;

function TWebApplicationBuilder.Build: IWebApplication;
begin
  Result := TWebApplication.Create(FImpl.Build);
end;

{ TWebApplication }

class function TWebApplication.CreateBuilder(const Args: TArray<string>): IWebApplicationBuilder;
begin
  Result := TWebApplicationBuilder.Create(Args);
end;

constructor TWebApplication.Create(const Host: IHost);
begin
  inherited Create;
  FHost := Host;
end;

function TWebApplication.GetServices: IServiceProvider;
begin
  Result := FHost.Services;
end;

function TWebApplication.GetServer: IDafHttpServer;
begin
  if not Assigned(FServer) then
    FServer := FHost.Services.GetRequiredService<IDafHttpServer>;
  Result := FServer;
end;

procedure TWebApplication.MapControllers;
begin
  FRouter := FHost.Services.GetRequiredService<IWebRouter>;
  MappedRouter := FRouter;
end;

destructor TWebApplication.Destroy;
begin
  // Soltarla aqui y no solo al final de Run: una aplicacion construida y abandonada --lo que
  // hace un spec-- dejaria viva la referencia global, y con ella el contenedor entero. Se
  // comprueba que siga siendo la nuestra, porque en un proceso puede haber dos solapadas.
  if Assigned(FRouter) and (MappedRouter = FRouter) then
    MappedRouter := nil;
  FRouter := nil;
  FServer := nil;

  // Stop es lo que suelta el contenedor, y el host se libera a si mismo al pararse.
  if Assigned(FHost) then
  begin
    FHost.Stop;
    Pointer(FHost) := nil;
  end;
  inherited;
end;

function TWebApplication.Run: Integer;
begin
  FHost.Start;
  try
    GetServer.Start;
    try
      // Por el host y no por el hook del proceso: es lo que hace que la parada pedida
      // por codigo --StopApplication-- despierte tambien a una aplicacion web.
      FHost.WaitForShutdown;
    finally
      GetServer.Stop;
    end;
    Result := 0;
  finally
    FServer := nil;
    FHost.Stop;
    // Stop libera el host: THost se hace Free a si mismo al parar, asi que soltar la referencia
    // aqui es lo que evita que el finalizador del campo toque memoria ya devuelta.
    Pointer(FHost) := nil;
  end;
end;

end.
