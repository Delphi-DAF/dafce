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
  Daf.Extensions.Hosting.Failures,
  Daf.Hosting,
  Daf.Hosting.Edge;

type
  /// <summary>Las claves con las que la variante se configura, y su valor por defecto.</summary>
  /// <remarks>
  ///   Se SIEMBRAN en el manager antes de entregarselo al componedor comun, asi que cualquier
  ///   variable de entorno o argumento posterior las pisa: es lo que hace que
  ///   <c>--Web:Port=9000</c> funcione sin que la variante tenga que mirar argv.
  /// </remarks>
  // Los de la familia, para que un .dpr web los escriba con esta unit sola.
  TAppExit = Daf.Extensions.Hosting.Failures.TAppExit;
  TAppFailure = Daf.Extensions.Hosting.Failures.TAppFailure;
  TAppExceptionPolicy = Daf.Extensions.Hosting.Failures.TAppExceptionPolicy;
  ExitPolicy = Daf.Extensions.Hosting.Failures.ExitPolicy;

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
    /// <remarks>
    ///   Devuelve <c>TAppExit.Ok</c> si termina bien. Lo que se escapa del arranque del host, del
    ///   arranque del servidor o de la parada lo decide la politica de <c>OnException</c>, y el
    ///   codigo es el que ella dio al primer fallo. Un fallo del arranque llega ya informado, el
    ///   servidor no arranca y el host se para igual; uno de la parada llega sin proveedor.
    ///
    ///   No toca el gancho ApplicationHandleException del RTL: es del manejador de peticiones de
    ///   WebBroker, que atiende con el lo que falla dentro de una peticion.
    /// </remarks>
    function Run: Integer;
    property Services: IServiceProvider read GetServices;
    property Server: IDafHttpServer read GetServer;
  end;

  IWebApplicationBuilder = interface(IHostApplicationBuilder)
    ['{027C56B1-3555-4E04-8186-8A8313D48D9E}']
    /// <summary>
    /// La politica que decide lo que se escapa, y con que codigo termina el proceso: dentro del Run,
    /// lo que se escapa del arranque del host, del arranque del servidor o de la parada; fuera, lo
    /// que se le pase por <see cref="HandleException"/>. Sin ella, LogAndFail.
    /// </summary>
    /// <remarks>
    ///   Un EAbort no llega a la politica: termina con <c>TAppExit.Failure</c> sin informar nada. Lo
    ///   que falla dentro de una peticion no llega: lo resuelve el servidor con su respuesta de error.
    /// </remarks>
    function OnException(const Policy: TAppExceptionPolicy): IWebApplicationBuilder;
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
    FFailures: IAppFailures;
    function GetServices: IServiceCollection;
    function GetConfiguration: IConfigurationManager;
    function GetEnvironment: IHostEnvironment;
    function GetLogging: ILoggingBuilder;
    function GetProperties: TDictionary<string, TValue>;
    function GetServiceProviderOptions: TServiceProviderOptions;
    procedure SetServiceProviderOptions(const Value: TServiceProviderOptions);
  public
    constructor Create(const Args: TArray<string>);
    function OnException(const Policy: TAppExceptionPolicy): IWebApplicationBuilder;
    function HandleException(const E: Exception): Integer;
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
    FFailures: IAppFailures;
  public
    class function CreateBuilder(const Args: TArray<string> = nil): IWebApplicationBuilder; static;
    constructor Create(const Host: IHost; const Failures: IAppFailures);
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

  // Los argumentos entran como configuracion, como en .NET.
  FImpl := THostBuilder.Create(Args, Manager);
  FOwned := FImpl;
  FFailures := TAppFailures.Create;
end;

function TWebApplicationBuilder.OnException(const Policy: TAppExceptionPolicy): IWebApplicationBuilder;
begin
  FFailures.SetPolicy(Policy);
  Result := Self;
end;

function TWebApplicationBuilder.HandleException(const E: Exception): Integer;
begin
  Result := FFailures.Handle(E);
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
  var Host: IHost := FImpl.Build;
  Result := TWebApplication.Create(Host, FFailures);
end;

{ TWebApplication }

class function TWebApplication.CreateBuilder(const Args: TArray<string>): IWebApplicationBuilder;
begin
  Result := TWebApplicationBuilder.Create(Args);
end;

constructor TWebApplication.Create(const Host: IHost; const Failures: IAppFailures);
begin
  inherited Create;
  FHost := Host;
  FFailures := Failures;
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

  // Parar el host es lo que suelta el contenedor.
  if Assigned(FHost) then
    try
      StopAndNil(FHost);
    except
      on E: Exception do
        ; // El host ya lo informo al ocurrir, y un destructor que lanza deja el objeto a medio liberar.
    end;
  inherited;
end;

function TWebApplication.Run: Integer;
var
  Started: Boolean;
  Failed: Boolean;
begin
  Result := TAppExit.Ok;
  Started := False;
  Failed := False;
  try
    try
      FHost.Start;
      Started := True;
      GetServer.Start;
      try
        // Por el host y no por el hook del proceso: es lo que hace que la parada pedida
        // por codigo --StopApplication-- despierte tambien a una aplicacion web.
        FHost.WaitForShutdown;
      finally
        GetServer.Stop;
      end;
    except
      // El host informa por el canal de arranque lo que hace fallar su arranque.
      on E: Exception do
      begin
        Failed := True;
        Result := FFailures.Decide(E, FHost.Services, not Started);
      end;
    end;
  finally
    FServer := nil;
    try
      StopAndNil(FHost);
    except
      // Si ya habia fallado algo, el codigo es el que la politica dio a ese fallo, y Stop ya
      // informo el suyo.
      on E: Exception do
        if not Failed then
          Result := FFailures.Decide(E, nil, True);
    end;
  end;
end;

end.
