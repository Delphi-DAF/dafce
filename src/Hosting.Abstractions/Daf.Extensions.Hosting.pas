
unit Daf.Extensions.Hosting;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Rtti,
  Daf.Threading,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Logging,
  Daf.Extensions.Configuration;

type
  TEnvironmentName = type string;
  IHostEnvironment = interface(IInvokable)
    ['{A2E5D5C3-9147-4C9E-9354-27C1B3F88F76}']
    function GetEnvironmentName: TEnvironmentName;
    procedure SetEnvironmentName(const Value: TEnvironmentName);
    function GetApplicationName: string;
    procedure SetApplicationName(const Value: string);
    procedure SetContentRootPath(const Value: string);
    function GetContentRootPath: string;
    function GetBinPath: string;
    function GetEnvVar(VarName: string): string;
    function IsDevelopment: Boolean;
    function IsStaging: Boolean;
    function IsProduction: Boolean;
    function IsTesting: Boolean;
    property EnvironmentName: TEnvironmentName read GetEnvironmentName write SetEnvironmentName;
    property ApplicationName: string read GetApplicationName write SetApplicationName;
    // Paths always end with path delimiter
    property ContentRootPath: string read GetContentRootPath write SetContentRootPath;
    property BinPath: string read GetBinPath;
    property EnvVar[VarName: string]: string read GetEnvVar;default;
  end;

   IHostBuilderContext = interface(IInvokable)
    ['{B452E97D-895E-44A6-8B48-2C49D8A1F77A}']
    function GetProperties: TDictionary<string, TValue>;
    function GetConfiguration: IConfiguration;
    function GetEnvironment: IHostEnvironment;
    property Properties: TDictionary<string, TValue> read GetProperties;
    property Configuration: IConfiguration read GetConfiguration;
    property Environment: IHostEnvironment read GetEnvironment;
  end;

  IHost = interface(IInvokable)
    ['{E17B8B80-9F18-4D4D-AB26-44F1C5E7C18F}']
    function GetServices: IServiceProvider;
    procedure Start;
    procedure Stop;
    procedure WaitForShutdown;
    property Services: IServiceProvider read GetServices;
  end;

  IHostedService = interface(IInvokable)
    ['{82D3A7D3-AB3A-41A5-B1E1-59ECAF5C9D5B}']
    procedure Start;
    procedure Stop;
  end;

  /// <summary>
  /// Codigo que se ejecuta una vez al arrancar, con el proveedor ya construido, y que no
  /// tiene ciclo de vida: no hay nada que parar, y por eso no hay Stop.
  /// </summary>
  /// <remarks>
  /// Divergencia deliberada de Microsoft.Extensions.*, que no trae este contrato. Su
  /// consecuencia conocida es que se usa IHostedService como inicializador, con un StopAsync
  /// vacio que miente sobre lo que el objeto es. DAF clona .NET por sus decisiones, no por
  /// sus huecos. La regla en una frase: si tiene algo que parar es IHostedService; si solo
  /// se ejecuta una vez, IStartupTask.
  /// </remarks>
  IStartupTask = interface(IInvokable)
    ['{6F864468-A26C-4AEA-988D-C2E279A1DCF4}']
    procedure Run;
  end;

  /// <summary>
  /// El contrato que comparten los miembros de la familia de aplicaciones --CLI, Web, VCL y
  /// FMX--, y lo que hace que sean una familia y no cuatro cosas parecidas.
  /// </summary>
  /// <remarks>
  ///   <para>
  ///   Es el estilo <b>inmediato</b>: todo lo que declara es una propiedad ya viva al volver
  ///   del constructor, asi que lo registrado ocurre donde esta escrito. El estilo diferido
  ///   --acumular acciones en listas y dispararlas cuando decida el builder-- no convive con
  ///   este: dos formas de expresar lo mismo es el defecto, no la comodidad.
  ///   </para>
  ///   <para>
  ///   <b>Build no esta aqui, y no es un olvido:</b> cada variante devuelve su propia clase de
  ///   aplicacion, y una interfaz de Delphi no admite retorno covariante. Es tambien lo que
  ///   hace .NET, donde IHostApplicationBuilder tampoco lo declara.
  ///   </para>
  ///   <para>
  ///   <b>Dos miembros del linaje no se portan, y quedan declarados como divergencia:</b>
  ///   Metrics, porque no hay pieza equivalente en DAF a la que apuntar; y
  ///   ConfigureContainer, porque es un metodo generico y una interfaz de Delphi no los
  ///   admite --no es que se descarte, es que no es expresable--.
  ///   </para>
  /// </remarks>
  IHostApplicationBuilder = interface(IInvokable)
    ['{8D8B3E08-3CC9-402A-86C3-188FE0682B08}']
    function GetServices: IServiceCollection;
    function GetConfiguration: IConfigurationManager;
    function GetEnvironment: IHostEnvironment;
    function GetLogging: ILoggingBuilder;
    function GetProperties: TDictionary<string, TValue>;
    function GetServiceProviderOptions: TServiceProviderOptions;
    procedure SetServiceProviderOptions(const Value: TServiceProviderOptions);

    /// <summary>Los registros de servicio de la aplicacion.</summary>
    property Services: IServiceCollection read GetServices;
    /// <summary>
    /// Constructor y lectura a la vez: lo anadido se lee acto seguido, que es lo que permite
    /// elegir una fuente a partir de lo ya configurado sin un callback diferido.
    /// </summary>
    property Configuration: IConfigurationManager read GetConfiguration;
    /// <summary>Ya resuelto, y derivado de la configuracion de host.</summary>
    property Environment: IHostEnvironment read GetEnvironment;
    /// <summary>
    /// Solo se compone si alguien la toca: componer sin proveedores es un error, asi que
    /// tocarla por sistema rompeia toda aplicacion que no configure logging.
    /// </summary>
    property Logging: ILoggingBuilder read GetLogging;
    /// <summary>Datos que comparten entre si las piezas que componen el arranque.</summary>
    property Properties: TDictionary<string, TValue> read GetProperties;
    /// <summary>
    /// Las opciones con que se construira el contenedor. Sin fijarlas, ValidateOnBuild y
    /// ValidateScopes valen Environment.IsDevelopment en el momento de leerlas; fijadas, ganan al
    /// entorno. Se fija el record entero: devuelto por una propiedad, no se modifica en su sitio.
    /// </summary>
    property ServiceProviderOptions: TServiceProviderOptions read GetServiceProviderOptions
      write SetServiceProviderOptions;
  end;

  IHostApplicationLifetime = interface(IInvokable)
    ['{30A46EB0-7D18-414C-B723-4F9B32043D81}']
    function ApplicationStarted: ICancellationToken;
    function ApplicationStopping: ICancellationToken;
    function ApplicationStopped: ICancellationToken;
    procedure StopApplication;
  end;

  TEnvironments = record
  public
    const Development = 'Development';
    const Staging = 'Staging';
    const Production = 'Production';
    const Testing = 'Testing';
  end;

  /// Las claves de la configuracion de host de las que sale IHostEnvironment, con los nombres de
  /// .NET. Se fijan por argumento (--environment Staging) o con una variable de entorno con DAF_
  /// delante, que se lee sin el prefijo: DAF_ENVIRONMENT, DAF_APPLICATIONNAME, DAF_CONTENTROOT.
  THostDefaults = record
    const EnvironmentKey = 'environment';
    const ApplicationKey = 'applicationName';
    const ContentRootKey = 'contentRoot';
  end;

  TEnvironmentNameHelper = record Helper for TEnvironmentName
    class function TryParse(const Source: string; out EnvName: TEnvironmentName ): Boolean;static;
    function ShortHand: string;
    function IsEmpty: Boolean;
    function IsNullOrWhiteSpace: Boolean;
    function IsDevelopment: Boolean;
    function IsStaging: Boolean;
    function IsProduction: Boolean;
    function IsTesting: Boolean;
  end;

  IServiceCollectionHelper = record helper for IServiceCollection
  public
    procedure AddHostedService<T:IHostedService>;
    procedure AddStartupTask<T:IStartupTask>;
  end;

  // Por si el helper anterior queda ensombrecido: en Delphi solo hay un record helper
  // visible por tipo, y quien haga uses de dos units que extiendan IServiceCollection
  // solo vera uno.
  Hosting = class
  public
    class procedure AddHostedService(const ServiceCollection: IServiceCollection; const TImpl: TClass);
    class procedure AddStartupTask(const ServiceCollection: IServiceCollection; const TImpl: TClass);
  end;


implementation
uses System.StrUtils, System.TypInfo;

procedure IServiceCollectionHelper.AddHostedService<T>;
begin
  AddSingleton<IHostedService, T>;
end;

procedure IServiceCollectionHelper.AddStartupTask<T>;
begin
  AddSingleton<IStartupTask, T>;
end;

{ Hosting }

class procedure Hosting.AddHostedService(const ServiceCollection: IServiceCollection; const TImpl: TClass);
begin
  ServiceCollection.AddSingleton(TypeInfo(IHostedService), TImpl);
end;

class procedure Hosting.AddStartupTask(const ServiceCollection: IServiceCollection; const TImpl: TClass);
begin
  ServiceCollection.AddSingleton(TypeInfo(IStartupTask), TImpl);
end;

{ TEnvironmentNameHelper }

function TEnvironmentNameHelper.IsDevelopment: Boolean;
begin
  Result := SameText(Self, TEnvironments.Development);
end;

function TEnvironmentNameHelper.IsEmpty: Boolean;
begin
  Result := string(Self).IsEmpty;
end;

function TEnvironmentNameHelper.IsNullOrWhiteSpace: Boolean;
begin
  Result := string.IsNullOrWhiteSpace(Self);
end;

function TEnvironmentNameHelper.IsProduction: Boolean;
begin
  Result := SameText(Self, TEnvironments.Production);
end;

function TEnvironmentNameHelper.IsStaging: Boolean;
begin
  Result := SameText(Self, TEnvironments.Staging);
end;

function TEnvironmentNameHelper.IsTesting: Boolean;
begin
  Result := SameText(Self, TEnvironments.Testing);
end;

class function TEnvironmentNameHelper.TryParse(const Source: string; out EnvName: TEnvironmentName ): Boolean;
  const Values: array[0..7] of string = ('deve', 'Development', 'stag', 'Staging', 'prod', 'Production', 'test', 'Testing');
begin
  var offset := IndexText(Source, Values);
  Result := offset > -1;
  if not Result then
  begin
    EnvName := '';
    Exit;
  end;

  if offset mod 2 = 0 then
    Inc(offset);
  EnvName := Values[offset];
end;

function TEnvironmentNameHelper.ShortHand: string;
begin
  Result := string(Copy(Self, 1, 4)).ToLower;
end;

end.

