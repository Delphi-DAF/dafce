unit Daf.FMX.Application;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Rtti,
  FMX.Forms,
  Daf.Extensions.Configuration,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Hosting,
  Daf.Extensions.Logging,
  Daf.Hosting;

type
  IFmxApplication = interface(IInvokable)
    ['{6C05D446-4645-42D3-BA7E-ABE72CF905F6}']
    function GetServices: IServiceProvider;
    /// <summary>
    /// Crea el formulario principal y DELEGA en FMX.Forms.Application, que es quien tiene el
    /// message loop. Aqui DAF no es dueño del proceso: lo era en CLI y en Web, donde Run era
    /// arrancar, trabajar y parar, y el .dpr acababa cuando Run acababa. Son dos contratos de
    /// ciclo de vida distintos, no cuatro variaciones de uno.
    /// </summary>
    /// <remarks>
    ///   Toma una clase de formulario y no un parametro de tipo porque una interfaz de Delphi no
    ///   admite metodos genericos. Es la misma razon por la que ConfigureContainer no se porta.
    /// </remarks>
    procedure Run(const MainFormClass: TComponentClass);
    /// <summary>Arranca el host y crea el formulario, SIN entrar en el bucle.</summary>
    /// <remarks>
    ///   Existe porque un spec no puede llamar a Run: entraria en el message loop y se quedaria
    ///   ahi. Una spec colgada no es una spec roja, asi que el arranque tiene que poder
    ///   ejercitarse hasta el borde del bucle y no mas alla.
    /// </remarks>
    procedure StartUpTo(const MainFormClass: TComponentClass);
    procedure Stop;
    function GetMainForm: TComponent;
    property Services: IServiceProvider read GetServices;
    /// <summary>El formulario principal, una vez creado. Nil antes.</summary>
    /// <remarks>
    ///   La variante lo publica en vez de dejar que se lea del global de la plataforma: ese
    ///   global es de la plataforma y lo llena cuando le parece --en un proceso sin contexto
    ///   grafico puede seguir vacio-- y entonces lo que se lee no dice nada sobre si la variante
    ///   creo el formulario o no.
    /// </remarks>
    property MainForm: TComponent read GetMainForm;
  end;

  IFmxApplicationBuilder = interface(IHostApplicationBuilder)
    ['{A2475EAF-CB83-4DE2-8C95-952B8030AD42}']
    function Build: IFmxApplication;
  end;

  TFmxApplication = class(TInterfacedObject, IFmxApplication)
  private
    FHost: IHost;
    FMainForm: TComponent;
  public
    class function CreateBuilder(const Args: TArray<string> = nil): IFmxApplicationBuilder; static;
    constructor Create(const Host: IHost);
    destructor Destroy; override;
    function GetServices: IServiceProvider;
    function GetMainForm: TComponent;
    procedure Run(const MainFormClass: TComponentClass);
    procedure StartUpTo(const MainFormClass: TComponentClass);
    procedure Stop;
  end;

  TFmxApplicationBuilder = class(TInterfacedObject, IFmxApplicationBuilder, IHostApplicationBuilder)
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
    function Build: IFmxApplication;
    property Services: IServiceCollection read GetServices;
    property Configuration: IConfigurationManager read GetConfiguration;
    property Environment: IHostEnvironment read GetEnvironment;
    property Logging: ILoggingBuilder read GetLogging;
    property Properties: TDictionary<string, TValue> read GetProperties;
    property ServiceProviderOptions: TServiceProviderOptions read GetServiceProviderOptions
      write SetServiceProviderOptions;
  end;

/// <summary>
/// La aplicacion DAF en curso. Existe SOLO porque la plataforma ya tiene un global que el
/// usuario espera --FMX.Forms.Application-- y negarselo obligaria a inventar un mecanismo peor.
/// En CLI y en Web no lo hay, y alli no falta: no hay ningun global de plataforma con el que
/// convivir y anadirlo seria un service locator sin nada que lo justifique.
/// </summary>
/// <remarks>
///   <b>El nombre no es Application, y el choque no es hipotetico:</b> en el mismo .dpr conviven
///   este y el de FireMonkey, y hasta hoy habia un sample del propio modulo de aplicacion escribiendo
///   <c>Application.Run</c> refiriendose al de FireMonkey. <b>La via preferente sigue siendo la
///   inyeccion:</b> esto es para el codigo de formulario que la plataforma construye por su
///   cuenta y al que nadie le puede inyectar nada.
/// </remarks>
function DafApp: IFmxApplication;

implementation

var
  CurrentApp: IFmxApplication = nil;

function DafApp: IFmxApplication;
begin
  Result := CurrentApp;
end;

{ TFmxApplicationBuilder }

constructor TFmxApplicationBuilder.Create(const Args: TArray<string>);
begin
  inherited Create;
  // Los argumentos SI entran como configuracion: esta variante no declara gramatica, asi que no
  // hay dos lecturas de argv con que chocar.
  FImpl := THostBuilder.Create(Args);
  FOwned := FImpl;
end;

function TFmxApplicationBuilder.GetServices: IServiceCollection;
begin
  Result := FImpl.Services;
end;

function TFmxApplicationBuilder.GetConfiguration: IConfigurationManager;
begin
  Result := FImpl.Configuration;
end;

function TFmxApplicationBuilder.GetEnvironment: IHostEnvironment;
begin
  Result := FImpl.Environment;
end;

function TFmxApplicationBuilder.GetLogging: ILoggingBuilder;
begin
  Result := FImpl.Logging;
end;

function TFmxApplicationBuilder.GetProperties: TDictionary<string, TValue>;
begin
  Result := FImpl.Properties;
end;

function TFmxApplicationBuilder.GetServiceProviderOptions: TServiceProviderOptions;
begin
  Result := FImpl.ServiceProviderOptions;
end;

procedure TFmxApplicationBuilder.SetServiceProviderOptions(const Value: TServiceProviderOptions);
begin
  FImpl.ServiceProviderOptions := Value;
end;

function TFmxApplicationBuilder.Build: IFmxApplication;
begin
  Result := TFmxApplication.Create(FImpl.Build);
  CurrentApp := Result;
end;

{ TFmxApplication }

class function TFmxApplication.CreateBuilder(const Args: TArray<string>): IFmxApplicationBuilder;
begin
  Result := TFmxApplicationBuilder.Create(Args);
end;

constructor TFmxApplication.Create(const Host: IHost);
begin
  inherited Create;
  FHost := Host;
end;

destructor TFmxApplication.Destroy;
begin
  Stop;
  inherited;
end;

function TFmxApplication.GetServices: IServiceProvider;
begin
  Result := FHost.Services;
end;

function TFmxApplication.GetMainForm: TComponent;
begin
  Result := FMainForm;
end;

procedure TFmxApplication.StartUpTo(const MainFormClass: TComponentClass);
begin
  FHost.Start;
  FMX.Forms.Application.Initialize;
  FMX.Forms.Application.CreateForm(MainFormClass, FMainForm);
end;

procedure TFmxApplication.Run(const MainFormClass: TComponentClass);
begin
  StartUpTo(MainFormClass);
  try
    // A partir de aqui manda la plataforma. Run no vuelve hasta que su bucle termina.
    FMX.Forms.Application.Run;
  finally
    Stop;
  end;
end;

procedure TFmxApplication.Stop;
begin
  if CurrentApp = (Self as IFmxApplication) then
    CurrentApp := nil;
  FMainForm := nil;
  if Assigned(FHost) then
    try
      FHost.Stop;
    finally
      // Stop libera el host: se hace Free a si mismo al parar, tambien cuando lanza.
      Pointer(FHost) := nil;
    end;
end;

end.
