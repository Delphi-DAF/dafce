unit Daf.Extensions.Logging;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.Rtti,
  System.TypInfo,
  System.Threading,
  System.RegularExpressions;

type
  {
    LogLevel	  Value	  Method	        Description
    Trace	      0	      LogTrace	      Contain the most detailed messages. These messages may contain sensitive app data. These messages are disabled by default and should not be enabled in production.
    Debug	      1	      LogDebug	      For debugging and development. Use with caution in production due to the high volume.
    Information	2	      LogInformation	Tracks the general flow of the app. May have long-term value.
    Warning	    3	      LogWarning	    For abnormal or unexpected events. Typically includes errors or conditions that don't cause the app to fail.
    Error	      4	      LogError	      For errors and exceptions that cannot be handled. These messages indicate a failure in the current operation or request, not an app-wide failure.
    Critical	  5	      LogCritical	    For failures that require immediate attention. Examples: data loss scenarios, out of disk space.
    None	      6		        N/A         Specifies that no messages should be written.
  }
{$SCOPEDENUMS ON}
  TLogLevel = (Trace, Debug, Information, Warning, Error, Critical, None);
{$SCOPEDENUMS OFF}

  TLogLevelHelper = record helper for TLogLevel
    function ToString: string;
    class function TryParse(Source: string; out Level: TLogLevel): Boolean; static;
    class function Parse(Source: string): TLogLevel; overload; static;
    class function Parse(Source: string; const Def: TLogLevel): TLogLevel; overload; static;
    class function ParseList(SourceList: string): TArray<TLogLevel>; static;
  end;

  TEventId = Integer;

  TLogState = record
  public type
    TFormatter = reference to function(State: TLogState; Ex: Exception): string;
  strict private
    FTemplate: string;
    FArgs: TArray<TValue>;
    FKeyValuePairs: TArray<TPair<string, TValue>>;
    FIsStructured: Boolean;
    FExtracted: Boolean;
    procedure EnsureExtracted;
    function GetKeyValuePairs: TArray<TPair<string, TValue>>;
  private
    function GetFormatter: TFormatter;
    class function StructuredFormat(State: TLogState; Ex: Exception): string; static;
    class function UnstructuredFormat(State: TLogState; Ex: Exception): string; static;
    property IsStructured: Boolean read FIsStructured;
  public
    constructor Create(const Template: string; const Args: TArray<TValue> = nil);
    property Template: string read FTemplate;
    property Arguments: TArray<TValue> read FArgs;
    property KeyValuePairs: TArray < TPair < string, TValue >> read GetKeyValuePairs;
    property Formatter: TFormatter read GetFormatter;
    function TryGetValue(const Key: string; out Value: TValue): Boolean;
  end;

  TLogFormatter = TLogState.TFormatter;

  TLogEntry = record
  private
    FMessage: string;
    function GetMessage: string;
  public
    Level: TLogLevel;
    Category: string;
    EventId: TEventId;
    Exception: Exception;
    Scope: string;
    State: TLogState;
    Formatter: TLogFormatter;
    property Message: string read GetMessage;
  end;

  // Safe base interface for use with `with ... do`: avoids variable shadowing and identifier leakage
  ILogScopeVoid = interface(IInvokable)
    ['{7C346BD8-863C-4605-8EFA-5659894E0ACA}']
  end;

  // Actual logging scope interface
  ILogScope = interface(ILogScopeVoid)
    ['{D3244D1B-78BD-48DD-BC65-86E132119D37}']
    function GetState: TLogState;
    function GetMessage: string;
    property State: TLogState read GetState;
    property Message: string read GetMessage;
  end;

  ILogger = interface(IInvokable)
    // La forma de .NET: contesta si una entrada de ese nivel llegaria a algun destino, para que el
    // consumidor pueda saltarse la construccion de sus argumentos --que puede costar mas que la
    // propia linea-- cuando la respuesta es que no.
    function IsEnabled(const Level: TLogLevel): Boolean;
    function BeginScope(const Msg: string; const Args: TArray<TValue>; const ScopedProc: TProc = nil): ILogScopeVoid;overload;
    function BeginScope(const Msg: string; const ScopedProc: TProc = nil): ILogScopeVoid;overload;
    procedure Log(const Entry: TLogEntry); overload;
    /// <summary>
    /// Writes a log entry.
    /// </summary>
    /// <param name="logLevel">Entry will be written on this level.</param>
    /// <param name="eventId">Id of the Entry.</param>
    /// <param name="state">The entry to be written. Can be also an object.</param>
    /// <param name="exception">The exception related to this entry.</param>
    /// <typeparam name="TState">The type of the object to be written.</typeparam>
    procedure Log(const Level: TLogLevel; const EventId: TEventId; const Ex: Exception; const Msg: string;
      const Args: TArray<TValue> = nil); overload;
    procedure Log(const Level: TLogLevel; const EventId: TEventId; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure Log(const Level: TLogLevel; const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure Log(const Level: TLogLevel; const Msg: string; const Args: TArray<TValue> = nil); overload;

    /// <summary>
    /// Formats and writes a debug log message.
    /// </summary>
    /// <param name="logger">The <see cref="ILogger"/> to write to.</param>
    /// <param name="eventId">The event id associated with the log.</param>
    /// <param name="exception">The exception to log.</param>
    /// <param name="message">Format string of the log message in message template format. Example: <c>"User {User} logged in from {Address}"</c>.</param>
    /// <param name="args">An object array that contains zero or more objects to format.</param>
    /// <example>
    /// <code language="csharp">
    /// logger.LogDebug(0, exception, "Error while processing request from {Address}", address)
    /// </code>
    /// </example>
    procedure LogDebug(const EventId: TEventId; const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;

    /// <summary>
    /// Formats and writes a debug log message.
    /// </summary>
    /// <param name="logger">The <see cref="ILogger"/> to write to.</param>
    /// <param name="eventId">The event id associated with the log.</param>
    /// <param name="message">Format string of the log message in message template format. Example: <c>"User {User} logged in from {Address}"</c>.</param>
    /// <param name="args">An object array that contains zero or more objects to format.</param>
    /// <example>
    /// <code language="csharp">
    /// logger.LogDebug(0, "Processing request from {Address}", address)
    /// </code>
    /// </example>
    procedure LogDebug(const EventId: TEventId; const Msg: string; const Args: TArray<TValue> = nil); overload;

    /// <summary>
    /// Formats and writes a debug log message.
    /// </summary>
    /// <param name="logger">The <see cref="ILogger"/> to write to.</param>
    /// <param name="exception">The exception to log.</param>
    /// <param name="message">Format string of the log message in message template format. Example: <c>"User {User} logged in from {Address}"</c>.</param>
    /// <param name="args">An object array that contains zero or more objects to format.</param>
    /// <example>
    /// <code language="csharp">
    /// logger.LogDebug(exception, "Error while processing request from {Address}", address)
    /// </code>
    /// </example>
    procedure LogDebug(const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;

    /// <summary>
    /// Formats and writes a debug log message.
    /// </summary>
    /// <param name="logger">The <see cref="ILogger"/> to write to.</param>
    /// <param name="message">Format string of the log message in message template format. Example: <c>"User {User} logged in from {Address}"</c>.</param>
    /// <param name="args">An object array that contains zero or more objects to format.</param>
    /// <example>
    /// <code language="csharp">
    /// logger.LogDebug("Processing request from {Address}", address)
    /// </code>
    /// </example>
    procedure LogDebug(const Msg: string; const Args: TArray<TValue> = nil); overload;

    procedure LogTrace(const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogTrace(const EventId: TEventId; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogTrace(const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogTrace(const EventId: TEventId; const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;

    procedure LogInformation(const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogInformation(const EventId: TEventId; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogInformation(const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogInformation(const EventId: TEventId; const Ex: Exception; const Msg: string;
      const Args: TArray<TValue> = nil); overload;

    procedure LogWarning(const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogWarning(const EventId: TEventId; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogWarning(const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogWarning(const EventId: TEventId; const Ex: Exception; const Msg: string;
      const Args: TArray<TValue> = nil); overload;

    procedure LogError(const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogError(const EventId: TEventId; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogError(const Ex: Exception; const Msg: string = ''; const Args: TArray<TValue> = nil); overload;
    procedure LogError(const EventId: TEventId; const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;

    procedure LogCritical(const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogCritical(const EventId: TEventId; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogCritical(const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogCritical(const EventId: TEventId; const Ex: Exception; const Msg: string;
      const Args: TArray<TValue> = nil); overload;
  end;

  ILogger<T> = interface(ILogger)
    ['{BDCC3739-B5C5-4DC4-8660-1E29B630972A}']
  end;

  ILoggerFactory = interface(IInvokable)
    ['{A16DDBEF-08A9-44FF-919A-607A0FBB8B58}']
    function CreateLogger(const Category: string): ILogger; overload;
    function CreateLogger(const AClass: TClass): ILogger; overload;
    function CreateLogger(const AType: PTypeInfo): ILogger; overload;
  end;

  ILoggerProvider = interface(IInvokable)
    ['{672E4EF7-7C9D-4E8C-BCA3-AB08B2E67D8F}']
    function CreateLogger(const Category: string): ILogger;
  end;

  // Aqui y no en el paquete concreto, que es donde .NET lo tiene
  // (Microsoft.Extensions.Logging.Abstractions).
  // La implementacion --TLoggingBuilder--, el error de composicion y AddLogging se quedan en
  // Daf.Logging
  ILoggingBuilder = interface(IInvokable)
    ['{F989B61A-8A02-4C1C-AF3D-DA94CF097B1C}']
    procedure AddProvider(const Provider: ILoggerProvider);
    // Descartar a proposito. Sin esto, componer sin proveedores es indistinguible de olvidarse el
    // AddProvider, y lo segundo se paga meses despues buscando unos registros que nunca existieron.
    procedure AddNullProvider;
    function Build: ILoggerFactory;
  end;

  TNullLoggerFactory = class(TInterfacedObject, ILoggerFactory)
  public
    function CreateLogger(const Category: string): ILogger; overload;
    function CreateLogger(const AClass: TClass): ILogger; overload;
    function CreateLogger(const AType: PTypeInfo): ILogger; overload;
  end;

  // Descartar a proposito, dicho en voz alta. Existe para que el descuido y la decision no se
  // escriban igual: un pipeline sin proveedores es un error de composicion, y este es el proveedor
  // con el que se declara que el silencio se queria.
  TNullLoggerProvider = class(TInterfacedObject, ILoggerProvider)
  public
    function CreateLogger(const Category: string): ILogger;
  end;

  TNullLogger = class(TInterfacedObject, ILogger)
  private
  public
    function IsEnabled(const Level: TLogLevel): Boolean;
    function BeginScope(const Msg: string; const Args: TArray<TValue>; const ScopedProc: TProc = nil): ILogScopeVoid;overload;
    function BeginScope(const Msg: string; const ScopedProc: TProc = nil): ILogScopeVoid;overload;
    procedure Log(const Entry: TLogEntry); overload;
    procedure Log(const Level: TLogLevel; const EventId: TEventId; const Ex: Exception; const Msg: string;
      const Args: TArray<TValue> = nil); overload;
    procedure Log(const Level: TLogLevel; const EventId: TEventId; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure Log(const Level: TLogLevel; const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure Log(const Level: TLogLevel; const Msg: string; const Args: TArray<TValue> = nil); overload;

    procedure LogDebug(const EventId: TEventId; const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogDebug(const EventId: TEventId; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogDebug(const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogDebug(const Msg: string; const Args: TArray<TValue> = nil); overload;

    procedure LogTrace(const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogTrace(const EventId: TEventId; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogTrace(const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogTrace(const EventId: TEventId; const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;

    procedure LogInformation(const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogInformation(const EventId: TEventId; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogInformation(const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogInformation(const EventId: TEventId; const Ex: Exception; const Msg: string;
      const Args: TArray<TValue> = nil); overload;

    procedure LogWarning(const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogWarning(const EventId: TEventId; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogWarning(const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogWarning(const EventId: TEventId; const Ex: Exception; const Msg: string;
      const Args: TArray<TValue> = nil); overload;

    procedure LogError(const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogError(const EventId: TEventId; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogError(const Ex: Exception; const Msg: string = ''; const Args: TArray<TValue> = nil); overload;
    procedure LogError(const EventId: TEventId; const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;

    procedure LogCritical(const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogCritical(const EventId: TEventId; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogCritical(const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogCritical(const EventId: TEventId; const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;
  end;

  // ================================================================================================
  // Canal de arranque
  //
  // Hay una fase del arranque en la que no existe IServiceProvider: nada de lo registrado puede
  // informar de un fallo todavia. Este canal cubre ese hueco y solo ese. No es un segundo framework
  // de logging: sin niveles por categoria, sin layouts configurables, sin escritura asincrona. En
  // cuanto existe el proveedor, informar es cosa del pipeline de ILogger.
  //
  // Y no se configura por fichero, a proposito: leer configuracion para saber donde informar de que
  // fallo la lectura de la configuracion es el mismo huevo y la misma gallina.
  // ================================================================================================

  // Un destino del canal de arranque. Recibe una linea ya formada --nunca un TLogEntry--, asi que no
  // puede inventarse su propio formato ni filtrar: no ve nada que se lo permita.
  IBootstrapSink = interface(IInvokable)
    ['{67D6ED65-6742-48D2-B7C6-CE61AB216872}']
    // Si el destino puede escribir aqui y ahora. Un destino que no puede escribir se declara no
    // disponible en vez de aceptar la linea y perderla en silencio.
    function IsAvailable: Boolean;
    procedure Write(const Line: string);
  end;

  // El canal: registro de destinos y reparto. Disponible desde initialization, sin DI y sin
  // configuracion. Nunca lanza -- un canal cuya razon de ser es informar de un fallo no puede
  // provocar un segundo fallo.
  BootstrapLog = record
    // Los destinos se pueden anadir y quitar desde initialization o desde el main, antes de
    // construir el host. Anadir es aditivo: los informes de DAF siguen saliendo por los suyos.
    class procedure AddSink(const Sink: IBootstrapSink); static;
    class procedure RemoveSink(const Sink: IBootstrapSink); static;
    class procedure ClearSinks; static;
    class function Sinks: TArray<IBootstrapSink>; static;
    // Los destinos que ahora mismo pueden escribir. Que este vacio es el caso peor --nadie va a
    // enterarse de nada-- y hay que poder distinguirlo de haber informado.
    class function AvailableSinks: TArray<IBootstrapSink>; static;
    class function HasAvailableSink: Boolean; static;
    // Reparte una linea ya formada a los destinos disponibles. No lanza pase lo que pase.
    class procedure Write(const Line: string); static;
    // Si esa excepcion ya llego a algun destino del canal. Responde por la ultima que llego, y la
    // reconoce por su identidad, su clase y su mensaje: otra que solo ocupe la direccion de una ya
    // liberada no se confunde con ella. Un informe que el umbral descarta, o que no encuentra ningun
    // destino disponible, no cuenta.
    class function WasReported(const E: Exception): Boolean; static;
    // El layout del canal, aplicado antes de repartir para que todos los destinos escriban lo mismo.
    class function FormatLine(const Entry: TLogEntry): string; static;
    // Un ILogger de arranque con la categoria de quien llama: la aplicacion informa de su propio
    // arranque sin disfrazarse de DAF.
    class function LoggerFor(const Category: string): ILogger; static;
  end;

  // Salida de depuracion del sistema. En Windows es OutputDebugString, que es del sistema y no del
  // depurador: se captura tambien sin uno conectado. Fuera de Windows Debugger.Write no escribe, y
  // el destino lo dice --ver Daf.FMX.BootstrapSink para el equivalente FireMonkey.
  TDebugOutputBootstrapSink = class(TInterfacedObject, IBootstrapSink)
  public
    function IsAvailable: Boolean;
    procedure Write(const Line: string);
  end;

  // Salida de error del proceso. Es lo que cubre el caso desatendido en la practica: un envoltorio
  // o el programador de tareas la capturan. En una aplicacion sin consola no hay handle de error, y
  // entonces el destino se declara no disponible en vez de lanzar al escribir.
  TStdErrBootstrapSink = class(TInterfacedObject, IBootstrapSink)
  public
    function IsAvailable: Boolean;
    procedure Write(const Line: string);
  end;

  // Destino duradero. NO se registra por defecto, y esa es la decision: una ruta duradera la conoce
  // la aplicacion --donde guarda sus datos-- y no el framework. DAF ofrece el mecanismo; la ruta la
  // pone quien despliega:
  //
  //   BootstrapLog.AddSink(TFileBootstrapSink.Create(TPath.Combine(MiCarpetaDeDatos, 'startup.log')));
  TFileBootstrapSink = class(TInterfacedObject, IBootstrapSink)
  private
    FFileName: string;
  public
    constructor Create(const AFileName: string);
    function IsAvailable: Boolean;
    procedure Write(const Line: string);
    property FileName: string read FFileName;
  end;

/// <summary>
///   La categoria de un tipo: su unit y su nombre. Vive aqui, en una sola funcion, porque es
///   la regla que decide bajo que nombre aparece lo que escribe un servicio
/// </summary>
function LogCategoryOf(const AType: PTypeInfo): string;

implementation
uses
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF MSWINDOWS}
  System.IOUtils,
  Daf.Types;

type
  // La cache de plantilla -> nombres. Las plantillas son literales del codigo, asi que el conjunto
  // de plantillas distintas de un programa es pequeño y cerrado: el regex corre una vez por
  // plantilla en toda la vida del proceso, no una por linea ni una por hilo. Se cachea el RESULTADO
  // del analisis, no el TRegEx: un threadvar con un tipo gestionado dentro no se finaliza al morir
  // el hilo, y cada hilo que aparece y se va dejaria el suyo.
  TTemplateNamesCache = class
  strict private
    FMap: TDictionary<string, TArray<string>>;
    FLock: TObject;
  public
    constructor Create;
    destructor Destroy; override;
    function NamesOf(const Template: string): TArray<string>;
  end;

const
  // Techo de la cache. Una plantilla construida en tiempo de ejecucion es ya un antipatron del
  // logging estructurado --el mensaje deja de ser agrupable--; pasado el techo se analiza sin
  // cachear, en vez de dejar crecer la cache sin limite.
  TemplateCacheLimit = 1024;

var
  GTemplateNames: TTemplateNamesCache = nil;

// El analisis en si: los nombres que declara una plantilla, en orden. La clase de caracteres no
// cambia (\w sin UCP es ASCII)
function ParseTemplateNames(const Template: string): TArray<string>;
begin
  var Matches := TRegEx.Matches(Template, '\{(\w+)\}');
  SetLength(Result, Matches.Count);
  var I := 0;
  for var Match in Matches do
  begin
    Result[I] := Match.Groups[1].Value;
    Inc(I);
  end;
end;

{ TTemplateNamesCache }

constructor TTemplateNamesCache.Create;
begin
  inherited Create;
  FMap := TDictionary<string, TArray<string>>.Create;
  FLock := TObject.Create;
end;

destructor TTemplateNamesCache.Destroy;
begin
  FMap.Free;
  FLock.Free;
  inherited;
end;

function TTemplateNamesCache.NamesOf(const Template: string): TArray<string>;
begin
  TMonitor.Enter(FLock);
  try
    if FMap.TryGetValue(Template, Result) then Exit;
  finally
    TMonitor.Exit(FLock);
  end;
  // El regex --lo caro-- corre FUERA del cerrojo: sostenerlo mientras se compila serializaria todo
  // el logging. Dos hilos que lleguen a la vez con la misma plantilla la analizan los dos una vez;
  // el resultado es el mismo y el segundo pisa al primero sin daño.
  Result := ParseTemplateNames(Template);
  TMonitor.Enter(FLock);
  try
    if FMap.Count < TemplateCacheLimit then
      FMap.AddOrSetValue(Template, Result);
  finally
    TMonitor.Exit(FLock);
  end;
end;

// El registro puede no existir todavia si alguien informa desde la initialization de una unit
// anterior --el canal de arranque lo hace--: en ese hueco se analiza sin cachear en vez de lanzar.
function TemplateNames(const Template: string): TArray<string>;
begin
  if Assigned(GTemplateNames) then
    Result := GTemplateNames.NamesOf(Template)
  else
    Result := ParseTemplateNames(Template);
end;


type
  // El logger del canal de arranque. No se expone: la unica via de obtener uno es
  // BootstrapLog.LoggerFor, que toma la categoria de quien informa -- tambien cuando quien
  // informa es DAF.
  TBootstrapLogger = class(TInterfacedObject, ILogger)
  private
    FCategory: string;
  public
    constructor Create(const Category: string);
    function IsEnabled(const Level: TLogLevel): Boolean;
    function BeginScope(const Msg: string; const Args: TArray<TValue>; const ScopedProc: TProc = nil): ILogScopeVoid; overload;
    function BeginScope(const Msg: string; const ScopedProc: TProc = nil): ILogScopeVoid; overload;
    procedure Log(const Entry: TLogEntry); overload;
    procedure Log(const Level: TLogLevel; const EventId: TEventId; const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure Log(const Level: TLogLevel; const EventId: TEventId; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure Log(const Level: TLogLevel; const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure Log(const Level: TLogLevel; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogDebug(const EventId: TEventId; const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogDebug(const EventId: TEventId; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogDebug(const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogDebug(const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogTrace(const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogTrace(const EventId: TEventId; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogTrace(const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogTrace(const EventId: TEventId; const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogInformation(const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogInformation(const EventId: TEventId; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogInformation(const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogInformation(const EventId: TEventId; const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogWarning(const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogWarning(const EventId: TEventId; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogWarning(const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogWarning(const EventId: TEventId; const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogError(const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogError(const EventId: TEventId; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogError(const Ex: Exception; const Msg: string = ''; const Args: TArray<TValue> = nil); overload;
    procedure LogError(const EventId: TEventId; const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogCritical(const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogCritical(const EventId: TEventId; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogCritical(const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;
    procedure LogCritical(const EventId: TEventId; const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;
  end;

{ TLogLevelHelper }

class function TLogLevelHelper.TryParse(Source: string; out Level: TLogLevel): Boolean;
begin
  Source := Source.Trim;
  Result := True;
  for var L := Low(TLogLevel) to High(TLogLevel) do
    if SameText(GetEnumName(TypeInfo(TLogLevel), Ord(L)), Source) then
    begin
      Level := L;
      Exit;
    end;
  Result := False;
end;

class function TLogLevelHelper.Parse(Source: string): TLogLevel;
begin
  if not TryParse(Source, Result) then
    raise Exception.CreateFmt('Invalid log level: %s', [Source]);
end;

class function TLogLevelHelper.Parse(Source: string; const Def: TLogLevel): TLogLevel;
begin
  if not TryParse(Source, Result) then
    Result := Def;
end;

class function TLogLevelHelper.ParseList(SourceList: string): TArray<TLogLevel>;
begin
  SetLength(Result, 0);
  var
  Sources := SourceList.Split([',']);
  var
    Level: TLogLevel;
  for var Source in Sources do
  begin
    if TryParse(Source, Level) then
      Result := Result + [Level];
  end;
end;

function LogCategoryOf(const AType: PTypeInfo): string;
begin
  Result := AType.TypeData.UnitNameFld.ToString + '.' + AType.NameFld.ToString;
end;

function TLogLevelHelper.ToString: string;
const
  LevelNames: array [TLogLevel] of string = ('TRACE', 'DEBUG', 'INFO', 'WARN', 'ERROR', 'CRIT', '');
begin
  Result := LevelNames[Self];
end;

{ TLogState }

constructor TLogState.Create(const Template: string; const Args: TArray<TValue> = nil);
begin
  FTemplate := Template;
  FArgs := Args;
  FIsStructured := FTemplate.Contains('{');
  FExtracted := False;
  FKeyValuePairs := nil;
  // El analisis de la plantilla se aplaza al primer lector (GetKeyValuePairs), asi
  // una linea que ningun destino acepta nunca lo paga.
end;

// Memoizado por instancia: se calcula una vez, al primer lector, y se guarda. Si esta memoizacion
// se rompe --que una segunda lectura devuelva vacio-- el spec de equivalencia tiene que caer.
procedure TLogState.EnsureExtracted;
begin
  if FExtracted then Exit;
  FExtracted := True;
  var Names := TemplateNames(FTemplate);
  SetLength(FKeyValuePairs, Length(Names));
  var I := 0;
  for var Name in Names do
    if I <= High(FArgs) then
    begin
      FKeyValuePairs[I] := TPair<string, TValue>.Create(Name, FArgs[I]);
      Inc(I);
    end;
end;

function TLogState.GetKeyValuePairs: TArray<TPair<string, TValue>>;
begin
  EnsureExtracted;
  Result := FKeyValuePairs;
end;

function TLogState.GetFormatter: TLogState.TFormatter;
begin
  if IsStructured then
    Result := TLogState.StructuredFormat
  else
    Result := TLogState.UnstructuredFormat;
end;

// Surprise: the version in Rtti, raises on values of type double
function TValueArrayToArrayOfConst(const Params: TArray<TValue>): TArray<TVarRec>;
begin
  var ConvertArgs := Params;
  for var idx := 0 to High(ConvertArgs) do
    if ConvertArgs[idx].TypeInfo^.Kind in [tkFloat] then
      ConvertArgs[idx] := ConvertArgs[idx].AsExtended;

  Result := System.Rtti.TValueArrayToArrayOfConst(ConvertArgs);
end;

class function TLogState.StructuredFormat(State: TLogState; Ex: Exception): string;
begin
  Result := State.Template;
  for var Pair in State.KeyValuePairs do
    Result := Result.Replace('{' + Pair.Key + '}', Pair.Value.ToString, [rfReplaceAll]);
  Result := Format(Result, TValueArrayToArrayOfConst(State.Arguments));
end;

class function TLogState.UnstructuredFormat(State: TLogState; Ex: Exception): string;
begin
  Result := Format(State.Template, TValueArrayToArrayOfConst(State.Arguments));
end;

function TLogState.TryGetValue(const Key: string; out Value: TValue): Boolean;
var
  Pair: TPair<string, TValue>;
begin
  EnsureExtracted;
  for Pair in FKeyValuePairs do
    if SameText(Pair.Key, Key) then
    begin
      Value := Pair.Value;
      Exit(True);
    end;
  Result := False;
end;

{ TLogEntry }

function TLogEntry.GetMessage: string;
begin
  // build message only first time
  if FMessage.IsEmpty then
    FMessage := Formatter(State, Exception);
  Result := FMessage;
end;

{ TNullLoggerFactory }

function TNullLoggerFactory.CreateLogger(const Category: string): ILogger;
begin
  Result := TNullLogger.Create;
end;

function TNullLoggerFactory.CreateLogger(const AClass: TClass): ILogger;
begin
  Result := CreateLogger(AClass.ClassInfo);
end;

function TNullLoggerFactory.CreateLogger(const AType: PTypeInfo): ILogger;
begin
  Result := CreateLogger(LogCategoryOf(AType));
end;

{ TNullLoggerProvider }

function TNullLoggerProvider.CreateLogger(const Category: string): ILogger;
begin
  Result := TNullLogger.Create;
end;

{ TNullLogger }

function TNullLogger.IsEnabled(const Level: TLogLevel): Boolean;
begin
  // El silencio declarado: quien pide este logger no quiere que se componga nada. Contesta que no,
  // y asi la puerta de TCompositeLogger se cierra sin construir la entrada.
  Result := False;
end;

function TNullLogger.BeginScope(const Msg: string;
  const ScopedProc: TProc): ILogScopeVoid;
begin
  Result := nil;
end;

function TNullLogger.BeginScope(const Msg: string; const Args: TArray<TValue>;
  const ScopedProc: TProc): ILogScopeVoid;
begin
  Result := nil;
end;

procedure TNullLogger.Log(const Entry: TLogEntry);
begin

end;

procedure TNullLogger.Log(const Level: TLogLevel; const EventId: TEventId;
  const Ex: Exception; const Msg: string; const Args: TArray<TValue>);
begin

end;

procedure TNullLogger.Log(const Level: TLogLevel; const Msg: string;
  const Args: TArray<TValue>);
begin

end;

procedure TNullLogger.Log(const Level: TLogLevel; const EventId: TEventId;
  const Msg: string; const Args: TArray<TValue>);
begin

end;

procedure TNullLogger.Log(const Level: TLogLevel; const Ex: Exception;
  const Msg: string; const Args: TArray<TValue>);
begin

end;

procedure TNullLogger.LogCritical(const Ex: Exception; const Msg: string;
  const Args: TArray<TValue>);
begin

end;

procedure TNullLogger.LogCritical(const EventId: TEventId; const Ex: Exception;
  const Msg: string; const Args: TArray<TValue>);
begin

end;

procedure TNullLogger.LogCritical(const EventId: TEventId; const Msg: string;
  const Args: TArray<TValue>);
begin

end;

procedure TNullLogger.LogCritical(const Msg: string;
  const Args: TArray<TValue>);
begin

end;

procedure TNullLogger.LogDebug(const EventId: TEventId; const Ex: Exception;
  const Msg: string; const Args: TArray<TValue>);
begin

end;

procedure TNullLogger.LogDebug(const EventId: TEventId; const Msg: string;
  const Args: TArray<TValue>);
begin

end;

procedure TNullLogger.LogDebug(const Msg: string; const Args: TArray<TValue>);
begin

end;

procedure TNullLogger.LogDebug(const Ex: Exception; const Msg: string;
  const Args: TArray<TValue>);
begin

end;

procedure TNullLogger.LogError(const EventId: TEventId; const Msg: string;
  const Args: TArray<TValue>);
begin

end;

procedure TNullLogger.LogError(const Msg: string; const Args: TArray<TValue>);
begin

end;

procedure TNullLogger.LogError(const Ex: Exception; const Msg: string = '';
  const Args: TArray<TValue> = nil);
begin

end;

procedure TNullLogger.LogError(const EventId: TEventId; const Ex: Exception;
  const Msg: string; const Args: TArray<TValue>);
begin

end;

procedure TNullLogger.LogInformation(const Ex: Exception; const Msg: string;
  const Args: TArray<TValue>);
begin

end;

procedure TNullLogger.LogInformation(const EventId: TEventId;
  const Ex: Exception; const Msg: string; const Args: TArray<TValue>);
begin

end;

procedure TNullLogger.LogInformation(const Msg: string;
  const Args: TArray<TValue>);
begin

end;

procedure TNullLogger.LogInformation(const EventId: TEventId; const Msg: string;
  const Args: TArray<TValue>);
begin

end;

procedure TNullLogger.LogTrace(const EventId: TEventId; const Msg: string;
  const Args: TArray<TValue>);
begin

end;

procedure TNullLogger.LogTrace(const Msg: string; const Args: TArray<TValue>);
begin

end;

procedure TNullLogger.LogTrace(const Ex: Exception; const Msg: string;
  const Args: TArray<TValue>);
begin

end;

procedure TNullLogger.LogTrace(const EventId: TEventId; const Ex: Exception;
  const Msg: string; const Args: TArray<TValue>);
begin

end;

procedure TNullLogger.LogWarning(const Msg: string; const Args: TArray<TValue>);
begin

end;

procedure TNullLogger.LogWarning(const EventId: TEventId; const Msg: string;
  const Args: TArray<TValue>);
begin

end;

procedure TNullLogger.LogWarning(const Ex: Exception; const Msg: string;
  const Args: TArray<TValue>);
begin

end;

procedure TNullLogger.LogWarning(const EventId: TEventId; const Ex: Exception;
  const Msg: string; const Args: TArray<TValue>);
begin

end;

{ Canal de arranque }

type
  // El estado por destino que el reparto necesita. Un destino que lanzo se marca aqui y deja de
  // intentarse: el canal no puede provocar un segundo fallo mientras informa del primero.
  TBootstrapSinkEntry = record
    Sink: IBootstrapSink;
    Failed: Boolean;
  end;

  // La ultima excepcion que llego a algun destino. Instance se compara y nunca se lee: la excepcion
  // puede estar ya liberada.
  TBootstrapReported = record
    Instance: Pointer;
    ClassType: TClass;
    Message: string;
  end;

var
  BootstrapSinkList: TList<TBootstrapSinkEntry> = nil;
  BootstrapSinkLock: TObject = nil;
  BootstrapReported: TBootstrapReported;

// El registro puede no existir todavia --o ya no existir-- si alguien informa desde la
// initialization de una unit anterior o desde una finalization posterior. Ni un caso ni el otro
// puede acabar en excepcion.
function BootstrapSinksReady: Boolean;
begin
  Result := Assigned(BootstrapSinkList) and Assigned(BootstrapSinkLock);
end;

// Preguntar tambien puede lanzar, y preguntar no es informar: si el destino no sabe responder, se
// le da por no disponible.
function SinkIsAvailable(const Sink: IBootstrapSink): Boolean;
begin
  try
    Result := Assigned(Sink) and Sink.IsAvailable;
  except
    Result := False;
  end;
end;

// El reparto, y a cuantos destinos llego la linea. Un destino que lanza al escribir no cuenta.
function WriteToSinks(const Line: string): Integer;
begin
  Result := 0;
  if not BootstrapSinksReady then Exit;
  TMonitor.Enter(BootstrapSinkLock);
  try
    for var I := 0 to BootstrapSinkList.Count - 1 do
    begin
      var Entry := BootstrapSinkList[I];
      if Entry.Failed then Continue;
      try
        if SinkIsAvailable(Entry.Sink) then
        begin
          Entry.Sink.Write(Line);
          Inc(Result);
        end;
      except
        // Fichero bloqueado, disco lleno, permisos: se traga aqui y ese destino queda fuera. Los
        // demas siguen escribiendo, que es de lo que se trataba.
        Entry.Failed := True;
        BootstrapSinkList[I] := Entry;
      end;
    end;
  finally
    TMonitor.Exit(BootstrapSinkLock);
  end;
end;

procedure RememberReported(const E: Exception);
begin
  if not BootstrapSinksReady then Exit;
  TMonitor.Enter(BootstrapSinkLock);
  try
    BootstrapReported.Instance := Pointer(E);
    BootstrapReported.ClassType := E.ClassType;
    BootstrapReported.Message := E.Message;
  finally
    TMonitor.Exit(BootstrapSinkLock);
  end;
end;

class procedure BootstrapLog.AddSink(const Sink: IBootstrapSink);
begin
  if not Assigned(Sink) or not BootstrapSinksReady then Exit;
  TMonitor.Enter(BootstrapSinkLock);
  try
    var Entry: TBootstrapSinkEntry;
    Entry.Sink := Sink;
    Entry.Failed := False;
    BootstrapSinkList.Add(Entry);
  finally
    TMonitor.Exit(BootstrapSinkLock);
  end;
end;

class procedure BootstrapLog.RemoveSink(const Sink: IBootstrapSink);
begin
  if not Assigned(Sink) or not BootstrapSinksReady then Exit;
  TMonitor.Enter(BootstrapSinkLock);
  try
    for var I := BootstrapSinkList.Count - 1 downto 0 do
      if BootstrapSinkList[I].Sink = Sink then
        BootstrapSinkList.Delete(I);
  finally
    TMonitor.Exit(BootstrapSinkLock);
  end;
end;

class procedure BootstrapLog.ClearSinks;
begin
  if not BootstrapSinksReady then Exit;
  TMonitor.Enter(BootstrapSinkLock);
  try
    BootstrapSinkList.Clear;
  finally
    TMonitor.Exit(BootstrapSinkLock);
  end;
end;

class function BootstrapLog.Sinks: TArray<IBootstrapSink>;
begin
  Result := nil;
  if not BootstrapSinksReady then Exit;
  TMonitor.Enter(BootstrapSinkLock);
  try
    SetLength(Result, BootstrapSinkList.Count);
    for var I := 0 to BootstrapSinkList.Count - 1 do
      Result[I] := BootstrapSinkList[I].Sink;
  finally
    TMonitor.Exit(BootstrapSinkLock);
  end;
end;

class function BootstrapLog.AvailableSinks: TArray<IBootstrapSink>;
begin
  Result := nil;
  if not BootstrapSinksReady then Exit;
  TMonitor.Enter(BootstrapSinkLock);
  try
    for var I := 0 to BootstrapSinkList.Count - 1 do
    begin
      var Entry := BootstrapSinkList[I];
      if not Entry.Failed and SinkIsAvailable(Entry.Sink) then
        Result := Result + [Entry.Sink];
    end;
  finally
    TMonitor.Exit(BootstrapSinkLock);
  end;
end;

class function BootstrapLog.HasAvailableSink: Boolean;
begin
  Result := Length(AvailableSinks) > 0;
end;

class procedure BootstrapLog.Write(const Line: string);
begin
  WriteToSinks(Line);
end;

class function BootstrapLog.WasReported(const E: Exception): Boolean;
begin
  Result := False;
  if not Assigned(E) or not BootstrapSinksReady then Exit;
  TMonitor.Enter(BootstrapSinkLock);
  try
    Result := (BootstrapReported.Instance = Pointer(E)) and
      (BootstrapReported.ClassType = E.ClassType) and (BootstrapReported.Message = E.Message);
  finally
    TMonitor.Exit(BootstrapSinkLock);
  end;
end;

class function BootstrapLog.FormatLine(const Entry: TLogEntry): string;
begin
  // layout '${timestamp} #${pid}:${thread} [${level}] ${category} | ${message} ${exception}'
  var ExceptionStr := '';
  if Assigned(Entry.Exception) then
    ExceptionStr := Format(' Exception: %s', [Entry.Exception.Message]);
  Result := Format('%s #%d:%d [%s] %s | %s%s',
    [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now), Debugger.CurrentProcessId, Debugger.CurrentThreadId,
     Entry.Level.ToString, Entry.Category, Entry.Message, ExceptionStr]);
end;

class function BootstrapLog.LoggerFor(const Category: string): ILogger;
begin
  Result := TBootstrapLogger.Create(Category);
end;

{ TDebugOutputBootstrapSink }

function TDebugOutputBootstrapSink.IsAvailable: Boolean;
begin
{$IFDEF MSWINDOWS}
  Result := True;
{$ELSE}
  // Debugger.Write tiene las ramas POSIX vacias: fuera de Windows aceptar la linea seria perderla.
  Result := False;
{$ENDIF MSWINDOWS}
end;

procedure TDebugOutputBootstrapSink.Write(const Line: string);
begin
  Debugger.Write(Line);
end;

{ TStdErrBootstrapSink }

function TStdErrBootstrapSink.IsAvailable: Boolean;
begin
{$IFDEF MSWINDOWS}
  // La pregunta es del proceso, no de la plataforma: una aplicacion VCL sin consola no tiene handle
  // de error, y escribir en ErrOutput ahi lanza.
  var Handle := GetStdHandle(STD_ERROR_HANDLE);
  Result := (Handle <> 0) and (Handle <> INVALID_HANDLE_VALUE);
{$ELSE}
  Result := True;
{$ENDIF MSWINDOWS}
end;

procedure TStdErrBootstrapSink.Write(const Line: string);
begin
  Writeln(ErrOutput, Line);
  Flush(ErrOutput);
end;

{ TFileBootstrapSink }

constructor TFileBootstrapSink.Create(const AFileName: string);
begin
  inherited Create;
  FFileName := AFileName;
end;

function TFileBootstrapSink.IsAvailable: Boolean;
begin
  // La carpeta la eligio quien construyo el destino. Si no esta, el destino lo dice en vez de
  // crearla por su cuenta: DAF no inventa rutas.
  Result := False;
  if FFileName = '' then Exit;
  try
    Result := TDirectory.Exists(TPath.GetDirectoryName(TPath.GetFullPath(FFileName)));
  except
    Result := False;
  end;
end;

procedure TFileBootstrapSink.Write(const Line: string);
begin
  TFile.AppendAllText(FFileName, Line + sLineBreak, TEncoding.UTF8);
end;

{ TBootstrapLogger }

function TBootstrapLogger.IsEnabled(const Level: TLogLevel): Boolean;
begin
  // El canal de arranque es el backstop cuando aun no hay pipeline: contesta que si para no
  // descartar nunca por no saber. El umbral (Warning y por encima) lo aplica su propio Log.
  Result := True;
end;

constructor TBootstrapLogger.Create(const Category: string);
begin
  inherited Create;
  FCategory := Category;
end;

function TBootstrapLogger.BeginScope(const Msg: string; const Args: TArray<TValue>; const ScopedProc: TProc = nil): ILogScopeVoid;
begin
  Result := nil;
  if Assigned(ScopedProc) then ScopedProc;
end;

function TBootstrapLogger.BeginScope(const Msg: string; const ScopedProc: TProc = nil): ILogScopeVoid;
begin
  Result := BeginScope(Msg, nil, ScopedProc);
end;

procedure TBootstrapLogger.Log(const Entry: TLogEntry);
begin
  // Filtrar aqui; formatear y repartir, en el canal. El umbral se queda --el arranque no es sitio
  // para charla de depuracion, y un canal que escribe siempre acaba ignorado--
  if not (Entry.Level in [TLogLevel.Warning, TLogLevel.Error, TLogLevel.Critical]) then Exit;
  if (WriteToSinks(BootstrapLog.FormatLine(Entry)) > 0) and Assigned(Entry.Exception) then
    RememberReported(Entry.Exception);
end;

procedure TBootstrapLogger.Log(const Level: TLogLevel; const EventId: TEventId; const Ex: Exception; const Msg: string; const Args: TArray<TValue>);
begin
  var Entry: TLogEntry;
  Entry.Level := Level;
  Entry.Category := FCategory;
  Entry.EventId := EventId;
  Entry.Exception := Ex;
  Entry.State := TLogState.Create(Msg, Args);
  Entry.Formatter := Entry.State.Formatter;
  Log(Entry);
end;

procedure TBootstrapLogger.Log(const Level: TLogLevel; const EventId: TEventId; const Msg: string; const Args: TArray<TValue>);
begin
  Log(Level, EventId, nil, Msg, Args);
end;

procedure TBootstrapLogger.Log(const Level: TLogLevel; const Ex: Exception; const Msg: string; const Args: TArray<TValue>);
begin
  Log(Level, 0, Ex, Msg, Args);
end;

procedure TBootstrapLogger.Log(const Level: TLogLevel; const Msg: string; const Args: TArray<TValue>);
begin
  Log(Level, 0, nil, Msg, Args);
end;

procedure TBootstrapLogger.LogDebug(const EventId: TEventId; const Ex: Exception; const Msg: string; const Args: TArray<TValue>);
begin
  Log(TLogLevel.Debug, EventId, Ex, Msg, Args);
end;
procedure TBootstrapLogger.LogDebug(const EventId: TEventId; const Msg: string; const Args: TArray<TValue>);
begin
  Log(TLogLevel.Debug, EventId, Msg, Args);
end;
procedure TBootstrapLogger.LogDebug(const Ex: Exception; const Msg: string; const Args: TArray<TValue>);
begin
  Log(TLogLevel.Debug, Ex, Msg, Args);
end;
procedure TBootstrapLogger.LogDebug(const Msg: string; const Args: TArray<TValue>);
begin
  Log(TLogLevel.Debug, Msg, Args);
end;
procedure TBootstrapLogger.LogTrace(const Msg: string; const Args: TArray<TValue>);
begin
  Log(TLogLevel.Trace, Msg, Args);
end;
procedure TBootstrapLogger.LogTrace(const EventId: TEventId; const Msg: string; const Args: TArray<TValue>);
begin
  Log(TLogLevel.Trace, EventId, Msg, Args);
end;
procedure TBootstrapLogger.LogTrace(const Ex: Exception; const Msg: string; const Args: TArray<TValue>);
begin
  Log(TLogLevel.Trace, Ex, Msg, Args);
end;
procedure TBootstrapLogger.LogTrace(const EventId: TEventId; const Ex: Exception; const Msg: string; const Args: TArray<TValue>);
begin
  Log(TLogLevel.Trace, EventId, Ex, Msg, Args);
end;
procedure TBootstrapLogger.LogInformation(const Msg: string; const Args: TArray<TValue>);
begin
  Log(TLogLevel.Information, Msg, Args);
end;
procedure TBootstrapLogger.LogInformation(const EventId: TEventId; const Msg: string; const Args: TArray<TValue>);
begin
  Log(TLogLevel.Information, EventId, Msg, Args);
end;
procedure TBootstrapLogger.LogInformation(const Ex: Exception; const Msg: string; const Args: TArray<TValue>);
begin
  Log(TLogLevel.Information, Ex, Msg, Args);
end;
procedure TBootstrapLogger.LogInformation(const EventId: TEventId; const Ex: Exception; const Msg: string; const Args: TArray<TValue>);
begin
  Log(TLogLevel.Information, EventId, Ex, Msg, Args);
end;
procedure TBootstrapLogger.LogWarning(const Msg: string; const Args: TArray<TValue>);
begin
  Log(TLogLevel.Warning, Msg, Args);
end;
procedure TBootstrapLogger.LogWarning(const EventId: TEventId; const Msg: string; const Args: TArray<TValue>);
begin
  Log(TLogLevel.Warning, EventId, Msg, Args);
end;
procedure TBootstrapLogger.LogWarning(const Ex: Exception; const Msg: string; const Args: TArray<TValue>);
begin
  Log(TLogLevel.Warning, Ex, Msg, Args);
end;
procedure TBootstrapLogger.LogWarning(const EventId: TEventId; const Ex: Exception; const Msg: string; const Args: TArray<TValue>);
begin
  Log(TLogLevel.Warning, EventId, Ex, Msg, Args);
end;
procedure TBootstrapLogger.LogError(const Msg: string; const Args: TArray<TValue>);
begin
  Log(TLogLevel.Error, Msg, Args);
end;
procedure TBootstrapLogger.LogError(const EventId: TEventId; const Msg: string; const Args: TArray<TValue>);
begin
  Log(TLogLevel.Error, EventId, Msg, Args);
end;
procedure TBootstrapLogger.LogError(const Ex: Exception; const Msg: string = ''; const Args: TArray<TValue> = nil);
begin
  Log(TLogLevel.Error, Ex, Msg, Args);
end;
procedure TBootstrapLogger.LogError(const EventId: TEventId; const Ex: Exception; const Msg: string; const Args: TArray<TValue>);
begin
  Log(TLogLevel.Error, EventId, Ex, Msg, Args);
end;
procedure TBootstrapLogger.LogCritical(const Msg: string; const Args: TArray<TValue>);
begin
  Log(TLogLevel.Critical, Msg, Args);
end;
procedure TBootstrapLogger.LogCritical(const EventId: TEventId; const Msg: string; const Args: TArray<TValue>);
begin
  Log(TLogLevel.Critical, EventId, Msg, Args);
end;
procedure TBootstrapLogger.LogCritical(const Ex: Exception; const Msg: string; const Args: TArray<TValue>);
begin
  Log(TLogLevel.Critical, Ex, Msg, Args);
end;
procedure TBootstrapLogger.LogCritical(const EventId: TEventId; const Ex: Exception; const Msg: string; const Args: TArray<TValue>);
begin
  Log(TLogLevel.Critical, EventId, Ex, Msg, Args);
end;

initialization
  GTemplateNames := TTemplateNamesCache.Create;
  BootstrapSinkLock := TObject.Create;
  BootstrapSinkList := TList<TBootstrapSinkEntry>.Create;
  // Los dos destinos que DAF trae: ninguno elige donde escribe. El duradero
  // --TFileBootstrapSink-- no se registra aqui a proposito: su ruta la pone quien despliega.
  BootstrapLog.AddSink(TDebugOutputBootstrapSink.Create);
  BootstrapLog.AddSink(TStdErrBootstrapSink.Create);

finalization
  FreeAndNil(BootstrapSinkList);
  FreeAndNil(BootstrapSinkLock);
  FreeAndNil(GTemplateNames);
end.
