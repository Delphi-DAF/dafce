unit Daf.VCL.Application;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Rtti,
  Vcl.Forms,
  Daf.Extensions.Configuration,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Hosting,
  Daf.Extensions.Logging,
  Daf.Extensions.Hosting.Failures,
  Daf.Hosting;

type
  // El de la familia, para que un .dpr de escritorio lea el codigo con esta unit sola.
  TAppExit = Daf.Extensions.Hosting.Failures.TAppExit;

  IVclApplication = interface(IInvokable)
    ['{B2918C19-A3F1-4201-B5FE-C09A0BDAAE8E}']
    function GetServices: IServiceProvider;
    /// <summary>
    /// Crea el formulario principal y DELEGA en Vcl.Forms.Application, que es quien tiene el
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

  IVclApplicationBuilder = interface(IHostApplicationBuilder)
    ['{5F7C5FB6-0B3B-44F3-9898-655B8637CA00}']
    /// <summary>
    /// Declara que solo puede correr una instancia con ese identificador. Sin declararla no hay
    /// cerrojo, y pueden correr varias a la vez. Se comprueba en el Build, antes de cargar la
    /// configuracion y de construir el contenedor.
    /// </summary>
    /// <remarks>
    ///   La segunda instancia se construye resuelta: Run y StartUpTo vuelven sin crear el formulario
    ///   ni entrar en el bucle, Services y MainForm valen nil, no se publica como DafApp, y el codigo
    ///   de salida del proceso pasa a ser <c>TAppExit.AlreadyRunning</c>.
    /// </remarks>
    function SingleInstance(const Id: string): IVclApplicationBuilder;
    function Build: IVclApplication;
  end;

  TVclApplication = class(TInterfacedObject, IVclApplication)
  private
    FHost: IHost;
    FMainForm: TComponent;
    FInstanceLock: THandle;
    FSettled: Boolean;
  public
    class function CreateBuilder(const Args: TArray<string> = nil): IVclApplicationBuilder; static;
    constructor Create(const Host: IHost; const InstanceLock: THandle);
    /// <summary>Otra instancia ya corre: no hay host, ni formulario, ni bucle.</summary>
    constructor CreateSettled;
    destructor Destroy; override;
    function GetServices: IServiceProvider;
    function GetMainForm: TComponent;
    procedure Run(const MainFormClass: TComponentClass);
    procedure StartUpTo(const MainFormClass: TComponentClass);
    procedure Stop;
  end;

  TVclApplicationBuilder = class(TInterfacedObject, IVclApplicationBuilder, IHostApplicationBuilder)
  private
    FImpl: THostBuilder;
    FOwned: IHostApplicationBuilder;
    FSingleInstanceId: string;
    function GetServices: IServiceCollection;
    function GetConfiguration: IConfigurationManager;
    function GetEnvironment: IHostEnvironment;
    function GetLogging: ILoggingBuilder;
    function GetProperties: TDictionary<string, TValue>;
    function GetServiceProviderOptions: TServiceProviderOptions;
    procedure SetServiceProviderOptions(const Value: TServiceProviderOptions);
  public
    constructor Create(const Args: TArray<string>);
    function SingleInstance(const Id: string): IVclApplicationBuilder;
    function Build: IVclApplication;
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
/// usuario espera --Vcl.Forms.Application-- y negarselo obligaria a inventar un mecanismo peor.
/// En CLI y en Web no lo hay, y alli no falta: no hay ningun global de plataforma con el que
/// convivir y anadirlo seria un service locator sin nada que lo justifique.
/// </summary>
/// <remarks>
///   <b>El nombre no es Application, y el choque no es hipotetico:</b> en el mismo .dpr conviven
///   este y el de la VCL. <b>La via preferente sigue siendo la
///   inyeccion:</b> esto es para el codigo de formulario que la plataforma construye por su
///   cuenta y al que nadie le puede inyectar nada.
/// </remarks>
function DafApp: IVclApplication;

implementation

uses
  Daf.Hosting.Edge;

var
  CurrentApp: IVclApplication = nil;

function DafApp: IVclApplication;
begin
  Result := CurrentApp;
end;

{ TVclApplicationBuilder }

constructor TVclApplicationBuilder.Create(const Args: TArray<string>);
begin
  inherited Create;
  // Los argumentos entran como configuracion, como en .NET.
  FImpl := THostBuilder.Create(Args);
  FOwned := FImpl;
end;

function TVclApplicationBuilder.GetServices: IServiceCollection;
begin
  Result := FImpl.Services;
end;

function TVclApplicationBuilder.GetConfiguration: IConfigurationManager;
begin
  Result := FImpl.Configuration;
end;

function TVclApplicationBuilder.GetEnvironment: IHostEnvironment;
begin
  Result := FImpl.Environment;
end;

function TVclApplicationBuilder.GetLogging: ILoggingBuilder;
begin
  Result := FImpl.Logging;
end;

function TVclApplicationBuilder.GetProperties: TDictionary<string, TValue>;
begin
  Result := FImpl.Properties;
end;

function TVclApplicationBuilder.GetServiceProviderOptions: TServiceProviderOptions;
begin
  Result := FImpl.ServiceProviderOptions;
end;

procedure TVclApplicationBuilder.SetServiceProviderOptions(const Value: TServiceProviderOptions);
begin
  FImpl.ServiceProviderOptions := Value;
end;

function TVclApplicationBuilder.SingleInstance(const Id: string): IVclApplicationBuilder;
begin
  FSingleInstanceId := Id;
  Result := Self;
end;

function TVclApplicationBuilder.Build: IVclApplication;
var
  Lock: THandle;
  Host: IHost;
begin
  // Antes de cargar y de construir el contenedor: una instancia que no debe arrancar no paga por
  // arrancar.
  if not TInstanceLock.Acquire(FSingleInstanceId, Lock) then
    Exit(TVclApplication.CreateSettled);
  try
    Host := FImpl.Build;
  except
    // Sin aplicacion que lo suelte al destruirse: una construccion que falla no deja el cerrojo
    // tomado para el resto del proceso.
    TInstanceLock.Release(Lock);
    raise;
  end;
  Result := TVclApplication.Create(Host, Lock);
  CurrentApp := Result;
end;

{ TVclApplication }

class function TVclApplication.CreateBuilder(const Args: TArray<string>): IVclApplicationBuilder;
begin
  Result := TVclApplicationBuilder.Create(Args);
end;

constructor TVclApplication.Create(const Host: IHost; const InstanceLock: THandle);
begin
  inherited Create;
  FHost := Host;
  FInstanceLock := InstanceLock;
end;

constructor TVclApplication.CreateSettled;
begin
  inherited Create;
  FSettled := True;
end;

destructor TVclApplication.Destroy;
begin
  try
    Stop;
  except
    on E: Exception do
      ; // El host ya lo informo al ocurrir, y un destructor que lanza deja el objeto a medio liberar.
  end;
  TInstanceLock.Release(FInstanceLock);
  inherited;
end;

function TVclApplication.GetServices: IServiceProvider;
begin
  if Assigned(FHost) then
    Result := FHost.Services
  else
    Result := nil;
end;

function TVclApplication.GetMainForm: TComponent;
begin
  Result := FMainForm;
end;

procedure TVclApplication.StartUpTo(const MainFormClass: TComponentClass);
begin
  if FSettled then
  begin
    System.ExitCode := TAppExit.AlreadyRunning;
    Exit;
  end;
  FHost.Start;
  Vcl.Forms.Application.Initialize;
  Vcl.Forms.Application.CreateForm(MainFormClass, FMainForm);
end;

procedure TVclApplication.Run(const MainFormClass: TComponentClass);
begin
  if FSettled then
  begin
    System.ExitCode := TAppExit.AlreadyRunning;
    Exit;
  end;
  StartUpTo(MainFormClass);
  try
    // A partir de aqui manda la plataforma. Run no vuelve hasta que su bucle termina.
    Vcl.Forms.Application.Run;
  finally
    Stop;
  end;
end;

procedure TVclApplication.Stop;
begin
  if CurrentApp = (Self as IVclApplication) then
    CurrentApp := nil;
  FMainForm := nil;
  if Assigned(FHost) then
    StopAndNil(FHost);
end;

end.
