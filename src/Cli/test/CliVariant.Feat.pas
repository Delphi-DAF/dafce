unit CliVariant.Feat;

interface

uses
  System.SysUtils,
  System.StrUtils,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Hosting,
  Daf.CommandLine,
  Daf.Cli;

type
  // En la seccion de interfaz por obligacion: el contenedor resuelve por RTTI, y un tipo que
  // solo esta en implementation no esta ahi para encontrarlo.
  IGreeter = interface(IInvokable)
    ['{9C5F3F81-1E63-4C70-9725-BFC4073C6324}']
    function Greet: string;
  end;

  TGreeter = class(TInterfacedObject, IGreeter)
  public
    function Greet: string;
  end;

  TLoudGreeter = class(TInterfacedObject, IGreeter)
  public
    function Greet: string;
  end;

  IParsedEcho = interface(IInvokable)
    ['{E2B7A415-3C9D-4F08-8B61-5D2F7C0A9E13}']
    function Parsed: ICommandLineParseResult;
  end;

  /// Un trabajo que recibe sus dependencias por constructor, entre ellas el resultado del parse.
  TParsedEcho = class(TInterfacedObject, IParsedEcho)
  private
    FParsed: ICommandLineParseResult;
  public
    constructor Create(const Parsed: ICommandLineParseResult);
    function Parsed: ICommandLineParseResult;
  end;

  IWelcome = interface(IInvokable)
    ['{3D0A8E27-5B1C-4F6E-9A42-7C1E0B9D5F31}']
    function Welcome: string;
  end;

  /// Depende de IGreeter, para que la validacion al construir tenga una dependencia que nombrar.
  TWelcome = class(TInterfacedObject, IWelcome)
  private
    FGreeter: IGreeter;
  public
    constructor Create(const Greeter: IGreeter);
    function Welcome: string;
  end;

  ICounted = interface(IInvokable)
    ['{A61C2F0E-8D47-4B93-B5E2-0F3C9A7D1E64}']
  end;

  /// Cuenta sus instancias vivas, asi que el escenario ve si el contenedor sigue en pie sin esperar
  /// al informe de fugas del runner.
  TCounted = class(TInterfacedObject, ICounted)
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function Alive: Integer; static;
  private
    class var FAlive: Integer;
  end;

  /// Anotan en la bitacora cuando corren, que es lo que dice que la aplicacion fue su arranque.
  TStartupNote = class(TInterfacedObject, IStartupTask)
  public
    procedure Run;
  end;

  THostedNote = class(TInterfacedObject, IHostedService)
  public
    procedure Start;
    procedure Stop;
  end;

  /// Falla al pararse, que es lo que pone a prueba el borde del Run despues de la accion.
  TRefusingStop = class(TInterfacedObject, IHostedService)
  public
    procedure Start;
    procedure Stop;
  end;

  /// Falla al arrancar, que es lo que pone a prueba el borde del Run antes de la accion.
  TRefusingStart = class(TInterfacedObject, IHostedService)
  public
    procedure Start;
    procedure Stop;
  end;

  /// El estado que el Given deja montado y el When usa. Un World de solo texto obliga al Given
  /// a ejecutar la accion para tener algo que dejar escrito, y entonces lo que el Then compara
  /// es la cadena que el propio montaje acaba de componer, no el comportamiento.
  TCliWorld = class
  strict private
    FContentRoot: string;
    FAmbientContentRoot: string;
  public
    Builder: ICliApplicationBuilder;
    App: ICliApplication;
    /// La primera instancia vive aqui y no en el Given porque es ella quien sostiene el
    /// cerrojo: si muriera al acabar el montaje, la segunda arrancaria tan tranquila.
    Holder: ICliApplication;
    /// Se pone a nil sola cuando la aplicacion se destruye: es lo que mide que no quede viva.
    [Weak] Watched: ICliApplication;
    Verbose: IOption<Boolean>;
    FileArg: IArgument<string>;
    Greet: ICommand;
    Farewell: ICommand;
    Parsed: ICommandLineParseResult;
    /// Empieza en -1 y no en 0: "termino con 0" es justo lo que tendria un campo que el When no
    /// llego a escribir.
    ExitCode: Integer;
    Report: string;
    PolicySaw: string;
    PolicyCalls: Integer;
    CountedBase: Integer;
    CountedDuring: Integer;
    Loud: IOption<Boolean>;
    ConfigFile: string;
    /// Lo que llego al canal de arranque durante Invoke.
    Reported: string;
    ActionRan: Boolean;
    /// El codigo de la segunda instancia, la que arranca mientras corre la primera.
    SecondExit: Integer;
    constructor Create;
    destructor Destroy; override;
    /// Un appsettings.json que no se puede leer, en la raiz de contenido del host. Llega por
    /// DAF_CONTENTROOT y no por argumento: un argumento que la gramatica no declara es una
    /// invocacion que no encaja.
    procedure WriteBrokenHostConfig;
    /// Compone con una accion, construye y ejecuta, como lo haria el .dpr, con el canal de
    /// arranque recogido en Reported. Devuelve el mensaje de lo que se escapo, o NoEscape.
    function Invoke(const Args: TArray<string>; const ReadsConfiguration: Boolean): string;
  end;

implementation

uses
  Winapi.Windows,
  System.Classes,
  System.Rtti,
  System.TypInfo,
  System.SyncObjs,
  System.IOUtils,
  Daf.MiniSpec,
  Daf.Extensions.Configuration,
  Daf.Configuration.CommandLine,
  Daf.Configuration.Json,
  Daf.Extensions.Logging;

const
  NoEscape = '<nada>';
  ContentRootVar = 'DAF_CONTENTROOT';

var
  Log: string;

function TGreeter.Greet: string;
begin
  Result := 'hello';
end;

function TLoudGreeter.Greet: string;
begin
  Result := 'HELLO';
end;

constructor TParsedEcho.Create(const Parsed: ICommandLineParseResult);
begin
  inherited Create;
  FParsed := Parsed;
end;

function TParsedEcho.Parsed: ICommandLineParseResult;
begin
  Result := FParsed;
end;

constructor TWelcome.Create(const Greeter: IGreeter);
begin
  inherited Create;
  FGreeter := Greeter;
end;

function TWelcome.Welcome: string;
begin
  Result := FGreeter.Greet;
end;

procedure TCounted.AfterConstruction;
begin
  inherited;
  TInterlocked.Increment(FAlive);
end;

procedure TCounted.BeforeDestruction;
begin
  TInterlocked.Decrement(FAlive);
  inherited;
end;

class function TCounted.Alive: Integer;
begin
  Result := TInterlocked.CompareExchange(FAlive, 0, 0);
end;

procedure TStartupNote.Run;
begin
  Log := Log + 'startup;';
end;

procedure THostedNote.Start;
begin
  Log := Log + 'hosted:start;';
end;

procedure THostedNote.Stop;
begin
  Log := Log + 'hosted:stop;';
end;

procedure TRefusingStop.Start;
begin
end;

procedure TRefusingStop.Stop;
begin
  raise Exception.Create('la parada fallo');
end;

procedure TRefusingStart.Start;
begin
  raise Exception.Create('el arranque fallo');
end;

procedure TRefusingStart.Stop;
begin
end;

type
  /// Un modulo de datos cuyo OnCreate lanza. Sin DFM: CreateNew no lo pide, y AfterConstruction
  /// dispara el OnCreate igual.
  TFailingModule = class(TDataModule)
  private
    procedure Refuse(Sender: TObject);
  public
    constructor CreateFailing;
  end;

constructor TFailingModule.CreateFailing;
begin
  OnCreate := Refuse;
  inherited CreateNew(nil);
end;

procedure TFailingModule.Refuse(Sender: TObject);
begin
  raise Exception.Create('el modulo fallo al crearse');
end;

constructor TCliWorld.Create;
begin
  inherited;
  ExitCode := -1;
  SecondExit := -1;
end;

destructor TCliWorld.Destroy;
begin
  if FContentRoot <> '' then
  begin
    var VarName: string := ContentRootVar;
    if FAmbientContentRoot <> '' then
      Winapi.Windows.SetEnvironmentVariable(PChar(VarName), PChar(FAmbientContentRoot))
    else
      Winapi.Windows.SetEnvironmentVariable(PChar(VarName), nil);
    TDirectory.Delete(FContentRoot, True);
  end;
  inherited;
end;

procedure TCliWorld.WriteBrokenHostConfig;
begin
  FContentRoot := TPath.Combine(TPath.GetTempPath, 'daf-cli-hostcfg-' + TGuid.NewGuid.ToString);
  TDirectory.CreateDirectory(FContentRoot);
  ConfigFile := TPath.Combine(FContentRoot, 'appsettings.json');
  TFile.WriteAllText(ConfigFile, '{ this is not json');

  var VarName: string := ContentRootVar;
  FAmbientContentRoot := System.SysUtils.GetEnvironmentVariable(VarName);
  Winapi.Windows.SetEnvironmentVariable(PChar(VarName), PChar(FContentRoot));
end;

/// Busca un acceso global: cualquier metodo de clase que devuelva la aplicacion o su proveedor
/// de servicios. Es la unica forma de escribir "esto NO existe" que se pone roja cuando alguien
/// lo anade -- comprobarlo leyendo el codigo no lo haria.
function GlobalAccessorsIn(const AClass: TClass): string;
begin
  Result := '';
  var Ctx := TRttiContext.Create;
  try
    for var M in Ctx.GetType(AClass).GetDeclaredMethods do
    begin
      if not (M.MethodKind in [mkClassFunction]) then Continue;
      if M.ReturnType = nil then Continue;
      if (M.ReturnType.Handle = TypeInfo(ICliApplication)) or
         (M.ReturnType.Handle = TypeInfo(IServiceProvider)) then
        Result := Result + M.Name + ' ';
    end;
  finally
    Ctx.Free;
  end;
  Result := Result.Trim;
end;

/// La parada informa su fallo por el canal de arranque, y eso sale por la salida de error del
/// arnes. Es el comportamiento que se quiere; aqui solo se aparta mientras corre, para que no se
/// confunda con la salida de las specs.
function RunQuietly(const App: ICliApplication): Integer;
begin
  var Saved := BootstrapLog.Sinks;
  BootstrapLog.ClearSinks;
  try
    Result := App.Run;
  finally
    for var Sink in Saved do
      BootstrapLog.AddSink(Sink);
  end;
end;

type
  /// Guarda lo que llega al canal de arranque, para contar cuantas veces se informa algo.
  TLinesSink = class(TInterfacedObject, IBootstrapSink)
  public
    Lines: string;
    function IsAvailable: Boolean;
    procedure Write(const Line: string);
  end;

function TLinesSink.IsAvailable: Boolean;
begin
  Result := True;
end;

procedure TLinesSink.Write(const Line: string);
begin
  Lines := Lines + Line + sLineBreak;
end;

/// Ejecuta con el canal de arranque recogido en Sink en vez de en la salida de error del arnes.
function RunReporting(const App: ICliApplication; const Sink: TLinesSink): Integer;
begin
  var Keep: IBootstrapSink := Sink;
  var Saved := BootstrapLog.Sinks;
  BootstrapLog.ClearSinks;
  BootstrapLog.AddSink(Keep);
  try
    Result := App.Run;
  finally
    BootstrapLog.ClearSinks;
    for var Each in Saved do
      BootstrapLog.AddSink(Each);
  end;
end;

function TCliWorld.Invoke(const Args: TArray<string>; const ReadsConfiguration: Boolean): string;
begin
  var Sink := TLinesSink.Create;
  var Keep: IBootstrapSink := Sink;
  var Saved := BootstrapLog.Sinks;
  BootstrapLog.ClearSinks;
  BootstrapLog.AddSink(Keep);
  try
    try
      Builder := TCliApplication.CreateBuilder(Args);
      if ReadsConfiguration then
        Builder.Configuration.Exists('Db:Server');
      Builder.SetAction(
        function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
        begin
          ActionRan := True;
          Result := TCliExit.Ok;
        end);
      App := Builder.Build;
      ExitCode := App.Run;
      Result := NoEscape;
    except
      on E: Exception do Result := E.Message;
    end;
  finally
    Reported := Sink.Lines;
    BootstrapLog.ClearSinks;
    for var Each in Saved do
      BootstrapLog.AddSink(Each);
  end;
end;

/// Una aplicacion cuya accion arranca una segunda igual mientras corre, las dos con el mismo
/// identificador de instancia unica si se da uno.
function NestedInstances(const W: TCliWorld; const Id: string): ICliApplicationBuilder;
begin
  Result := TCliApplication.CreateBuilder;
  if Id <> '' then
    Result.SingleInstance(Id);
  Result.SetAction(
    function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
    begin
      var Second := TCliApplication.CreateBuilder;
      if Id <> '' then
        Second.SingleInstance(Id);
      Second.SetAction(
        function(const SecondServices: IServiceProvider; const SecondParsed: ICommandLineParseResult): Integer
        begin
          W.Report := 'corrio';
          Result := TCliExit.Ok;
        end);
      W.SecondExit := Second.Build.Run;
      Result := TCliExit.Ok;
    end);
end;

// --- Feature definition ---

initialization

Feature('''
Feature La variante CLI de la familia @cli @application-family

  Como quien escribe una herramienta de linea de comandos
  Quiero que el arranque cubra el borde del Run
  Para no repetir en cada .dpr la instancia unica, la version y el codigo de salida
''')

.UseWorld<TCliWorld>

.Rule('Los argumentos llegan a la gramatica y, como en .NET, tambien a la configuracion')

  .Scenario('Un argumento clave=valor llega a la configuracion')
    .When('se crea el builder de consola con un argumento clave=valor',
      procedure(W: TCliWorld)
      begin
        W.Builder := TCliApplication.CreateBuilder(['--Db:Server=localhost']);
      end)
    .&Then('la clave se lee de su configuracion',
      procedure(W: TCliWorld)
      begin
        Expect(W.Builder.Configuration['Db:Server']).ToEqual('localhost');
      end)

  // La configuracion no conoce la gramatica: no sabe que --verbose no lleva valor, y lo empareja
  // con el token que le sigue.
  .Scenario('La configuracion lee una bandera seguida de un posicional como clave y valor')
    .Given('una gramatica con una bandera y un argumento posicional',
      procedure(W: TCliWorld)
      begin
        W.Verbose := CommandLine.Option<Boolean>('--verbose', 'Cuenta lo que hace');
        W.FileArg := CommandLine.Argument<string>('file', 'Que fichero');

        W.Builder := TCliApplication.CreateBuilder(['--verbose', 'informe.txt']);
        W.Builder.RootCommand.Add([W.Verbose, W.FileArg]);
      end)
    .When('se invoca con la bandera y el posicional detras',
      procedure(W: TCliWorld)
      begin
        W.Parsed := W.Builder.RootCommand.Parse(['--verbose', 'informe.txt']);
      end)
    .&Then('la gramatica liga los dos simbolos, y la configuracion lee verbose con el posicional',
      procedure(W: TCliWorld)
      begin
        var Config := '<ausente>';
        if W.Builder.Configuration.Exists('verbose') then
          Config := W.Builder.Configuration['verbose'];
        Expect(Format('verbose=%s file=%s config=%s',
          [BoolToStr(W.Verbose.ValueIn(W.Parsed), True), W.FileArg.ValueIn(W.Parsed), Config]))
          .ToEqual('verbose=True file=informe.txt config=informe.txt');
      end)

  .Scenario('Una clave que la gramatica no declara no deja arrancar la aplicacion')
    .Given('una aplicacion con una clave de configuracion en la linea de comandos',
      procedure(W: TCliWorld)
      begin
        W.Builder := TCliApplication.CreateBuilder(['--Db:Server=localhost']);
        W.Builder.SetAction(
          function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
          begin
            Result := 0;
          end);
      end)
    .When('se construye y se ejecuta',
      procedure(W: TCliWorld)
      begin
        W.App := W.Builder.Build;
        W.ExitCode := W.App.Run;
      end)
    .&Then('termina con el codigo de error de invocacion',
      procedure(W: TCliWorld)
      begin
        Expect(W.ExitCode).ToEqual(TCliExit.InvocationError);
      end)

  .Scenario('Con AllowUnmatchedTokens, la clave llega a la configuracion de la aplicacion')
    .Given('la misma aplicacion, cuya raiz admite lo que no declara',
      procedure(W: TCliWorld)
      begin
        W.Builder := TCliApplication.CreateBuilder(['--Db:Server=localhost']);
        W.Builder.RootCommand.AllowUnmatchedTokens;
        W.Builder.SetAction(
          function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
          begin
            W.Report := Services.GetRequiredService<IConfiguration>['Db:Server'];
            Result := 0;
          end);
      end)
    .When('se construye y se ejecuta',
      procedure(W: TCliWorld)
      begin
        W.App := W.Builder.Build;
        W.ExitCode := W.App.Run;
      end)
    .&Then('arranca, y la accion lee la clave de su configuracion',
      procedure(W: TCliWorld)
      begin
        Expect(Format('exit=%d Db:Server=%s', [W.ExitCode, W.Report])).ToEqual('exit=0 Db:Server=localhost');
      end)

.Rule('La variante CLI ejecuta la accion del comando alcanzado')

  .Scenario('Una herramienta con subcomandos')
    .Given('una aplicacion CLI con dos subcomandos, cada uno con su accion declarada en el builder',
      procedure(W: TCliWorld)
      begin
        W.Builder := TCliApplication.CreateBuilder(['greet']);
        W.Builder.Services.AddSingleton<IGreeter, TGreeter>;
        W.Greet := CommandLine.Command('greet');
        W.Farewell := CommandLine.Command('farewell');
        W.Builder.RootCommand.Add([W.Greet, W.Farewell]);
        W.Builder.SetAction(W.Greet,
          function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
          begin
            W.Report := W.Report + 'greet:' + Services.GetRequiredService<IGreeter>.Greet + ';';
            Result := 7;
          end);
        W.Builder.SetAction(W.Farewell,
          function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
          begin
            W.Report := W.Report + 'farewell;';
            Result := 8;
          end);
      end)
    .When('se ejecuta con una invocacion que nombra uno de ellos',
      procedure(W: TCliWorld)
      begin
        W.App := W.Builder.Build;
        W.ExitCode := W.App.Run;
      end)
    .&Then('corre solo la accion de ese subcomando, con los servicios del contenedor',
      procedure(W: TCliWorld)
      begin
        Expect(W.Report).ToEqual('greet:hello;');
      end)
    .&And('el proceso termina con el codigo que devolvio',
      procedure(W: TCliWorld)
      begin
        Expect(W.ExitCode).ToEqual(7);
      end)

  .Scenario('La accion de la raiz')
    .Given('una aplicacion CLI sin subcomandos y con una accion declarada para la raiz',
      procedure(W: TCliWorld)
      begin
        W.Builder := TCliApplication.CreateBuilder;
        W.Builder.Services.AddSingleton<IGreeter, TGreeter>;
        W.Builder.SetAction(
          function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
          begin
            W.Report := Services.GetRequiredService<IGreeter>.Greet;
            Result := 5;
          end);
      end)
    .When('se ejecuta',
      procedure(W: TCliWorld)
      begin
        W.App := W.Builder.Build;
        W.ExitCode := W.App.Run;
      end)
    .&Then('corre esa accion, con los servicios del contenedor',
      procedure(W: TCliWorld)
      begin
        Expect(Format('%s exit=%d', [W.Report, W.ExitCode])).ToEqual('hello exit=5');
      end)

  .Scenario('Sin accion, la aplicacion es su arranque')
    .Given('una aplicacion CLI que no declara ninguna accion',
      procedure(W: TCliWorld)
      begin
        Log := '';
        W.Builder := TCliApplication.CreateBuilder;
        Hosting.AddStartupTask(W.Builder.Services, TStartupNote);
        Hosting.AddHostedService(W.Builder.Services, THostedNote);
      end)
    .When('se ejecuta',
      procedure(W: TCliWorld)
      begin
        W.App := W.Builder.Build;
        W.ExitCode := W.App.Run;
      end)
    .&Then('corren sus tareas de arranque y sus servicios alojados',
      procedure(W: TCliWorld)
      begin
        Expect(Log).ToEqual('startup;hosted:start;hosted:stop;');
      end)
    .&And('el proceso termina con 0',
      procedure(W: TCliWorld)
      begin
        Expect(W.ExitCode).ToEqual(TCliExit.Ok);
      end)

  .Scenario('Lo que se escapa de la accion lo decide la politica')
    .Given('una aplicacion CLI con una politica de excepciones declarada, y una accion que lanza',
      procedure(W: TCliWorld)
      begin
        W.Builder := TCliApplication.CreateBuilder;
        W.Builder.OnException(
          function(const Failure: TCliFailure): Integer
          begin
            W.PolicySaw := Failure.Error.Message;
            Result := 42;
          end);
        W.Builder.SetAction(
          function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
          begin
            raise Exception.Create('el trabajo fallo');
          end);
        W.App := W.Builder.Build;
      end)
    .When('se ejecuta',
      procedure(W: TCliWorld)
      begin
        W.ExitCode := W.App.Run;
      end)
    .&Then('se aplica esa politica',
      procedure(W: TCliWorld)
      begin
        Expect(W.PolicySaw).ToEqual('el trabajo fallo');
      end)
    .&And('el proceso termina con el codigo que ella devuelve',
      procedure(W: TCliWorld)
      begin
        Expect(W.ExitCode).ToEqual(42);
      end)

  .Scenario('La politica que deja salir la excepcion la entrega entera')
    .Given('una aplicacion CLI con la politica que deja salir la excepcion, y una accion que lanza',
      procedure(W: TCliWorld)
      begin
        W.Builder := TCliApplication.CreateBuilder;
        W.Builder.OnException(ExitPolicy.Rethrow());
        W.Builder.SetAction(
          function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
          begin
            raise Exception.Create('el trabajo fallo');
          end);
        W.App := W.Builder.Build;
      end)
    .When('se ejecuta',
      procedure(W: TCliWorld)
      begin
        try
          W.ExitCode := W.App.Run;
          W.Report := '<no lanzo>';
        except
          on E: Exception do
            W.Report := E.ClassName + ': ' + E.Message;
        end;
      end)
    .&Then('la excepcion sale del Run, con su clase y su mensaje',
      procedure(W: TCliWorld)
      begin
        Expect(W.Report).ToEqual('Exception: el trabajo fallo');
      end)

  .Scenario('Un Abort termina con Failure sin pasar por la politica')
    .Given('una aplicacion CLI con una politica de excepciones declarada, y una accion que llama a Abort',
      procedure(W: TCliWorld)
      begin
        W.Builder := TCliApplication.CreateBuilder;
        W.Builder.OnException(
          function(const Failure: TCliFailure): Integer
          begin
            W.PolicySaw := Failure.Error.ClassName;
            Result := 42;
          end);
        W.Builder.SetAction(
          function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
          begin
            Abort;
            Result := TCliExit.Ok;
          end);
        W.App := W.Builder.Build;
      end)
    .When('se ejecuta',
      procedure(W: TCliWorld)
      begin
        W.ExitCode := W.App.Run;
      end)
    .&Then('el proceso termina con Failure, y la politica no lo ve',
      procedure(W: TCliWorld)
      begin
        Expect(Format('exit=%d politica=%s', [W.ExitCode, W.PolicySaw]))
          .ToEqual(Format('exit=%d politica=', [TCliExit.Failure]));
      end)

  // Dos formas de decir lo mismo son el defecto, no una precedencia que haya que recordar.
  .Scenario('Dos acciones para el mismo comando')
    .Given('un comando que ya tiene accion',
      procedure(W: TCliWorld)
      begin
        W.Builder := TCliApplication.CreateBuilder;
        W.Greet := CommandLine.Command('greet').SetAction(
          function(const Parsed: ICommandLineParseResult): Integer
          begin
            Result := 0;
          end);
        W.Builder.RootCommand.Add(W.Greet);
      end)
    .When('se le declara otra en el builder',
      procedure(W: TCliWorld)
      begin
        try
          W.Builder.SetAction(W.Greet,
            function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
            begin
              Result := 0;
            end);
          W.Report := '<declaro sin protestar>';
        except
          on E: Exception do
            W.Report := E.ClassName + ': ' + E.Message;
        end;
      end)
    .&Then('la declaracion falla con un error de composicion que nombra el comando',
      procedure(W: TCliWorld)
      begin
        Expect(W.Report).ToStartWith('ECliCompositionError: ');
        Expect(W.Report).ToContain('greet');
      end)

  .Scenario('Ejecutar no deja viva la aplicacion')
    .Given('una aplicacion CLI con una accion que usa los servicios del contenedor',
      procedure(W: TCliWorld)
      begin
        W.CountedBase := TCounted.Alive;
        W.Builder := TCliApplication.CreateBuilder;
        W.Builder.Services.AddSingleton<ICounted, TCounted>;
        W.Builder.SetAction(
          function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
          begin
            Services.GetRequiredService<ICounted>;
            W.CountedDuring := TCounted.Alive - W.CountedBase;
            Result := 0;
          end);
        W.App := W.Builder.Build;
        W.Watched := W.App;
      end)
    .When('se ejecuta y se suelta',
      procedure(W: TCliWorld)
      begin
        W.ExitCode := W.App.Run;
        W.Report := 'before=' + IfThen(Assigned(W.Watched), 'alive', 'released');
        W.App := nil;
        W.Builder := nil;
      end)
    .&Then('no queda viva ni la aplicacion ni su contenedor',
      procedure(W: TCliWorld)
      begin
        // "during" y "before" son los controles: sin ellos, una accion que no llego a correr y una
        // referencia que nunca apunto a nada darian el mismo verde.
        Expect(Format('during=%d %s after=%s container=%d', [W.CountedDuring, W.Report,
          IfThen(Assigned(W.Watched), 'alive', 'released'), TCounted.Alive - W.CountedBase]))
          .ToEqual('during=1 before=alive after=released container=0');
      end)

.Rule('El borde del Run')

  .Scenario('Una aplicacion que decide no arrancar termina sin contenedor')
    .Given('una instancia ya corriendo con ese identificador, y una segunda construida igual',
      procedure(W: TCliWorld)
      begin
        var First := TCliApplication.CreateBuilder;
        First.SingleInstance('daf-cli-spec');
        W.Holder := First.Build;
        // Se ejecuta: Stop es lo que suelta el contenedor, y una aplicacion construida y
        // abandonada lo deja vivo -- el informe de fugas lo dice. El cerrojo, en cambio, lo
        // sigue sosteniendo W.Holder hasta que se destruya, que es lo que este escenario mide.
        W.Holder.Run;

        W.Builder := TCliApplication.CreateBuilder;
        W.Builder.SingleInstance('daf-cli-spec');
        W.App := W.Builder.Build;
      end)
    .When('arranca la segunda',
      procedure(W: TCliWorld)
      begin
        W.ExitCode := W.App.Run;
      end)
    .&Then('termina sin haber construido el contenedor y lo dice por su codigo de salida',
      procedure(W: TCliWorld)
      begin
        // Sin contenedor: no se llego a construir nada.
        Expect(Format('%d/%s', [W.ExitCode, BoolToStr(W.App.Services = nil, True)]))
          .ToEqual('3/True');
      end)

  .Scenario('La version la resuelve la gramatica, no la aplicacion')
    .Given('una aplicacion invocada pidiendo la version',
      procedure(W: TCliWorld)
      begin
        W.Builder := TCliApplication.CreateBuilder(['--version']);
        W.Builder.RootCommand.WithVersion('2.0.0');
        W.App := W.Builder.Build;
      end)
    .When('se ejecuta',
      procedure(W: TCliWorld)
      begin
        W.ExitCode := W.App.Run;
      end)
    .&Then('contesta el parse y termina bien, sin levantar contenedor',
      procedure(W: TCliWorld)
      begin
        // Ni siquiera se construyo el contenedor: la gramatica contesto sola.
        Expect(Format('%d/%s', [W.ExitCode, BoolToStr(W.App.Services = nil, True)]))
          .ToEqual('0/True');
      end)

  .Scenario('Lo que se escapa de una accion de ayuda sustituida lo decide la politica')
    .Given('una aplicacion invocada pidiendo la ayuda, con una politica declarada y una accion de ayuda que lanza',
      procedure(W: TCliWorld)
      begin
        W.Builder := TCliApplication.CreateBuilder(['--help']);
        W.Builder.OnException(
          function(const Failure: TCliFailure): Integer
          begin
            W.PolicySaw := Failure.Error.Message + IfThen(Failure.Services = nil, ' sin', ' con') +
              ' proveedor';
            Result := 42;
          end);
        W.Builder.RootCommand.HelpAction :=
          function(const Parsed: ICommandLineParseResult): Integer
          begin
            raise Exception.Create('sin banner');
          end;
        W.App := W.Builder.Build;
      end)
    .When('se ejecuta',
      procedure(W: TCliWorld)
      begin
        try
          W.ExitCode := W.App.Run;
          W.Report := '<no lanzo>';
        except
          on E: Exception do
            W.Report := E.ClassName + ': ' + E.Message;
        end;
      end)
    .&Then('el Run no lanza, y el proceso termina con el codigo de la politica, que la recibe sin proveedor',
      procedure(W: TCliWorld)
      begin
        Expect(Format('%s exit=%d politica=%s', [W.Report, W.ExitCode, W.PolicySaw]))
          .ToEqual('<no lanzo> exit=42 politica=sin banner sin proveedor');
      end)

  .Scenario('La version contesta aunque otra instancia sostenga el cerrojo')
    .Given('una instancia ya corriendo con ese identificador, y una segunda que pide la version',
      procedure(W: TCliWorld)
      begin
        var First := TCliApplication.CreateBuilder;
        First.SingleInstance('daf-cli-spec');
        W.Holder := First.Build;
        W.Holder.Run;

        W.Builder := TCliApplication.CreateBuilder(['--version']);
        W.Builder.SingleInstance('daf-cli-spec');
        W.Builder.RootCommand.WithVersion('2.0.0');
        W.App := W.Builder.Build;
      end)
    .When('se ejecuta la segunda',
      procedure(W: TCliWorld)
      begin
        W.ExitCode := W.App.Run;
      end)
    .&Then('contesta el parse y termina bien, sin levantar contenedor',
      procedure(W: TCliWorld)
      begin
        Expect(Format('%d/%s', [W.ExitCode, BoolToStr(W.App.Services = nil, True)]))
          .ToEqual('0/True');
      end)

  .Scenario('La ayuda contesta aunque otra instancia sostenga el cerrojo')
    .Given('una instancia ya corriendo con ese identificador, y una segunda que pide la ayuda',
      procedure(W: TCliWorld)
      begin
        var First := TCliApplication.CreateBuilder;
        First.SingleInstance('daf-cli-spec');
        W.Holder := First.Build;
        W.Holder.Run;

        W.Builder := TCliApplication.CreateBuilder(['--help']);
        W.Builder.SingleInstance('daf-cli-spec');
        W.App := W.Builder.Build;
      end)
    .When('se ejecuta la segunda',
      procedure(W: TCliWorld)
      begin
        W.ExitCode := W.App.Run;
      end)
    .&Then('contesta el parse y termina bien, sin levantar contenedor',
      procedure(W: TCliWorld)
      begin
        Expect(Format('%d/%s', [W.ExitCode, BoolToStr(W.App.Services = nil, True)]))
          .ToEqual('0/True');
      end)

  .Scenario('Sin instancia unica declarada, corren varias a la vez')
    .Given('una aplicacion sin instancia unica, cuya accion arranca una segunda igual mientras corre',
      procedure(W: TCliWorld)
      begin
        W.Report := '<no corrio>';
        W.Builder := NestedInstances(W, '');
      end)
    .When('se ejecuta la primera',
      procedure(W: TCliWorld)
      begin
        W.App := W.Builder.Build;
        W.ExitCode := W.App.Run;
      end)
    .&Then('la segunda ejecuta su accion y termina bien',
      procedure(W: TCliWorld)
      begin
        Expect(Format('segunda=%d %s', [W.SecondExit, W.Report])).ToEqual('segunda=0 corrio');
      end)

  // El control del anterior: la misma pareja, declarando instancia unica.
  .Scenario('Con instancia unica declarada, la segunda no arranca mientras corre la primera')
    .Given('la misma aplicacion, con las dos instancias declaradas con el mismo identificador',
      procedure(W: TCliWorld)
      begin
        W.Report := '<no corrio>';
        W.Builder := NestedInstances(W, 'daf-cli-spec-nested');
      end)
    .When('se ejecuta la primera',
      procedure(W: TCliWorld)
      begin
        W.App := W.Builder.Build;
        W.ExitCode := W.App.Run;
      end)
    .&Then('la segunda termina sin ejecutar su accion, con el codigo de que ya habia otra',
      procedure(W: TCliWorld)
      begin
        Expect(Format('segunda=%d %s', [W.SecondExit, W.Report]))
          .ToEqual(Format('segunda=%d <no corrio>', [TCliExit.AlreadyRunning]));
      end)

  .Scenario('Una construccion que falla suelta el cerrojo')
    .Given('una aplicacion con instancia unica cuya construccion fallo',
      procedure(W: TCliWorld)
      begin
        var First := TCliApplication.CreateBuilder;
        First.SingleInstance('daf-cli-spec-failed');
        First.Services.AddSingleton<IWelcome, TWelcome>;
        First.ServiceProviderOptions := TServiceProviderOptions.Create(True, True);
        var Saved := BootstrapLog.Sinks;
        BootstrapLog.ClearSinks;
        try
          try
            First.Build;
            W.PolicySaw := '<construyo sin protestar>';
          except
            on E: Exception do W.PolicySaw := '';
          end;
        finally
          for var Sink in Saved do
            BootstrapLog.AddSink(Sink);
        end;
      end)
    .When('arranca una segunda con el mismo identificador',
      procedure(W: TCliWorld)
      begin
        W.Report := '<no corrio>';
        W.Builder := TCliApplication.CreateBuilder;
        W.Builder.SingleInstance('daf-cli-spec-failed');
        W.Builder.SetAction(
          function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
          begin
            W.Report := 'corrio';
            Result := TCliExit.Ok;
          end);
        W.App := W.Builder.Build;
        W.ExitCode := W.App.Run;
      end)
    .&Then('la segunda ejecuta su accion y termina bien',
      procedure(W: TCliWorld)
      begin
        // PolicySaw es el control de que la primera construccion fallo de verdad.
        Expect(Format('exit=%d %s%s', [W.ExitCode, W.Report, W.PolicySaw])).ToEqual('exit=0 corrio');
      end)

  .Scenario('Una invocacion que no encaja tiene su texto y su codigo')
    .Given('una invocacion con un token que la gramatica no reconoce',
      procedure(W: TCliWorld)
      begin
        W.Builder := TCliApplication.CreateBuilder(['--que-no-existe']);
        W.App := W.Builder.Build;
      end)
    .When('se ejecuta',
      procedure(W: TCliWorld)
      begin
        W.ExitCode := W.App.Run;
      end)
    .&Then('sale con el codigo de error de invocacion, distinto del de fallo del trabajo',
      procedure(W: TCliWorld)
      begin
        Expect(W.ExitCode).ToEqual(2);
      end)

.Rule('La gramatica contesta antes de que se cargue la configuracion')

  .Scenario('La ayuda contesta con un fichero de configuracion roto')
    .Given('un appsettings.json que no se puede leer en la raiz de contenido del host',
      procedure(W: TCliWorld)
      begin
        W.WriteBrokenHostConfig;
      end)
    .When('se invoca pidiendo la ayuda, con una composicion que no lee la configuracion',
      procedure(W: TCliWorld)
      begin
        W.Report := W.Invoke(['--help'], False);
      end)
    .&Then('la responde y termina bien',
      procedure(W: TCliWorld)
      begin
        Expect(Format('exit=%d escapo=%s', [W.ExitCode, W.Report]))
          .ToEqual('exit=0 escapo=' + NoEscape);
      end)

  .Scenario('La ayuda contesta aunque la composicion haya leido la configuracion')
    .Given('un appsettings.json que no se puede leer en la raiz de contenido del host',
      procedure(W: TCliWorld)
      begin
        W.WriteBrokenHostConfig;
      end)
    .When('se invoca pidiendo la ayuda, con una composicion que lee la configuracion',
      procedure(W: TCliWorld)
      begin
        W.Report := W.Invoke(['--help'], True);
      end)
    .&Then('la responde y termina bien',
      procedure(W: TCliWorld)
      begin
        Expect(Format('exit=%d escapo=%s', [W.ExitCode, W.Report]))
          .ToEqual('exit=0 escapo=' + NoEscape);
      end)
    .&And('el fallo del fichero quedo informado por el canal de arranque',
      procedure(W: TCliWorld)
      begin
        Expect(W.Reported).ToContain(W.ConfigFile);
      end)

  // Es verde tambien antes de que la carga se aplace --falla al crear el builder--, asi que lo que
  // lo sostiene es que el mensaje nombre el fichero y que la accion no haya corrido.
  .Scenario('Una accion no corre sobre una configuracion rota')
    .Given('un appsettings.json que no se puede leer en la raiz de contenido del host',
      procedure(W: TCliWorld)
      begin
        W.WriteBrokenHostConfig;
      end)
    .When('se invoca de modo que llega a la accion',
      procedure(W: TCliWorld)
      begin
        W.Report := W.Invoke([], False);
      end)
    .&Then('la construccion falla nombrando el fichero',
      procedure(W: TCliWorld)
      begin
        Expect(W.Report).ToContain(W.ConfigFile);
      end)
    .&And('la accion no corre',
      procedure(W: TCliWorld)
      begin
        Expect(BoolToStr(W.ActionRan, True)).ToEqual('False');
      end)

.Rule('El resultado del parse esta en el contenedor')

  .Scenario('Un trabajo recibe el resultado del parse por constructor')
    .Given('un servicio cuyo constructor pide el resultado del parse',
      procedure(W: TCliWorld)
      begin
        W.Builder := TCliApplication.CreateBuilder;
        W.Builder.Services.AddSingleton<IParsedEcho, TParsedEcho>;
        W.Builder.OnException(
          function(const Failure: TCliFailure): Integer
          begin
            W.PolicySaw := Failure.Error.Message;
            Result := TCliExit.Failure;
          end);
        W.Builder.SetAction(
          function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
          begin
            // Sin un constructor que el contenedor pueda satisfacer, el activador construye con el
            // del ancestro, y el servicio queda sin resultado: eso no es lo mismo que otro.
            var Echoed := Services.GetRequiredService<IParsedEcho>.Parsed;
            if Echoed = nil then
              W.Report := 'sin resultado'
            else
              W.Report := IfThen((Echoed as IInterface) = (Parsed as IInterface), 'el mismo', 'otro');
            Result := TCliExit.Ok;
          end);
      end)
    .When('la accion lo resuelve',
      procedure(W: TCliWorld)
      begin
        W.App := W.Builder.Build;
        W.ExitCode := W.App.Run;
      end)
    .&Then('el servicio tiene el mismo resultado que recibio la accion',
      procedure(W: TCliWorld)
      begin
        Expect(W.Report + W.PolicySaw).ToEqual('el mismo');
      end)

  .Scenario('Lo que se registra depende de un argumento')
    .Given('un registro por factory que elige la implementacion segun una opcion de la gramatica',
      procedure(W: TCliWorld)
      begin
        W.Loud := CommandLine.Option<Boolean>('--loud', 'Saluda en alto');
        W.Builder := TCliApplication.CreateBuilder(['--loud']);
        W.Builder.RootCommand.Add(W.Loud);
        var Loud := W.Loud;
        W.Builder.Services.AddSingleton<IGreeter>(
          function(Provider: IServiceProvider): IGreeter
          begin
            if Loud.ValueIn(Provider.GetRequiredService<ICommandLineParseResult>) then
              Result := TLoudGreeter.Create
            else
              Result := TGreeter.Create;
          end);
        W.Builder.OnException(
          function(const Failure: TCliFailure): Integer
          begin
            W.PolicySaw := Failure.Error.Message;
            Result := TCliExit.Failure;
          end);
        W.Builder.SetAction(
          function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
          begin
            W.Report := Services.GetRequiredService<IGreeter>.Greet;
            Result := TCliExit.Ok;
          end);
      end)
    .When('se invoca con esa opcion',
      procedure(W: TCliWorld)
      begin
        W.App := W.Builder.Build;
        W.ExitCode := W.App.Run;
      end)
    .&Then('se resuelve la implementacion que la opcion elige',
      procedure(W: TCliWorld)
      begin
        Expect(W.Report + W.PolicySaw).ToEqual('HELLO');
      end)

.Rule('Un fallo al arrancar lo decide la politica')

  .Scenario('Falla un servicio alojado al arrancar')
    .Given('una aplicacion CLI con una politica de excepciones declarada, y un servicio alojado que falla al arrancar detras de otro que arranca bien',
      procedure(W: TCliWorld)
      begin
        Log := '';
        W.Builder := TCliApplication.CreateBuilder;
        W.Builder.OnException(
          function(const Failure: TCliFailure): Integer
          begin
            W.PolicySaw := W.PolicySaw + Failure.Error.Message +
              IfThen(Failure.Reported, ' informado', ' sin informar') +
              IfThen(Failure.Services = nil, ' sin', ' con') + ' proveedor;';
            Result := 42;
          end);
        Hosting.AddHostedService(W.Builder.Services, THostedNote);
        Hosting.AddHostedService(W.Builder.Services, TRefusingStart);
        W.Builder.SetAction(
          function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
          begin
            W.ActionRan := True;
            Result := TCliExit.Ok;
          end);
        W.App := W.Builder.Build;
      end)
    .When('se ejecuta',
      procedure(W: TCliWorld)
      begin
        try
          W.ExitCode := RunQuietly(W.App);
          W.Report := '<no lanzo>';
        except
          on E: Exception do
            W.Report := E.Message;
        end;
      end)
    .&Then('el Run no lanza, y la politica la recibe ya informada y con proveedor',
      procedure(W: TCliWorld)
      begin
        Expect(Format('%s exit=%d %s', [W.Report, W.ExitCode, W.PolicySaw]))
          .ToEqual('<no lanzo> exit=42 el arranque fallo informado con proveedor;');
      end)
    .&And('la accion no corre, y el host se para',
      procedure(W: TCliWorld)
      begin
        Expect(Format('accion=%s %s', [BoolToStr(W.ActionRan, True), Log]))
          .ToEqual('accion=False hosted:start;hosted:stop;');
      end)

  .Scenario('Con la politica por defecto, un fallo al arrancar se informa una sola vez')
    .Given('una aplicacion CLI sin politica declarada, y un servicio alojado que falla al arrancar',
      procedure(W: TCliWorld)
      begin
        W.Builder := TCliApplication.CreateBuilder;
        Hosting.AddHostedService(W.Builder.Services, TRefusingStart);
        W.Builder.SetAction(
          function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
          begin
            Result := TCliExit.Ok;
          end);
        W.App := W.Builder.Build;
      end)
    .When('se ejecuta',
      procedure(W: TCliWorld)
      begin
        var Sink := TLinesSink.Create;
        var Keep: IBootstrapSink := Sink;
        try
          W.ExitCode := RunReporting(W.App, Sink);
        finally
          W.Report := Sink.Lines;
        end;
      end)
    .&Then('el canal de arranque lo recibe una vez, y el proceso termina con Failure',
      procedure(W: TCliWorld)
      begin
        var Times := 0;
        var From := Pos('el arranque fallo', W.Report);
        while From > 0 do
        begin
          Inc(Times);
          From := PosEx('el arranque fallo', W.Report, From + 1);
        end;
        Expect(Format('%d informe(s), codigo %d', [Times, W.ExitCode]))
          .ToEqual(Format('1 informe(s), codigo %d', [TCliExit.Failure]));
      end)

.Rule('Un fallo al parar lo decide la politica')

  .Scenario('La accion termina bien y la parada falla')
    .Given('una aplicacion CLI con una politica de excepciones declarada, y un servicio alojado que falla al parar',
      procedure(W: TCliWorld)
      begin
        W.Builder := TCliApplication.CreateBuilder;
        W.Builder.OnException(
          function(const Failure: TCliFailure): Integer
          begin
            W.PolicySaw := W.PolicySaw + Failure.Error.Message +
              IfThen(Failure.Services = nil, ' sin', ' con') + ' proveedor;';
            Result := 42;
          end);
        Hosting.AddHostedService(W.Builder.Services, TRefusingStop);
        W.Builder.SetAction(
          function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
          begin
            Result := TCliExit.Ok;
          end);
        W.App := W.Builder.Build;
      end)
    .When('se ejecuta y su accion termina bien',
      procedure(W: TCliWorld)
      begin
        W.ExitCode := RunQuietly(W.App);
      end)
    .&Then('la politica recibe la excepcion de la parada, sin proveedor',
      procedure(W: TCliWorld)
      begin
        Expect(W.PolicySaw).ToEqual('la parada fallo sin proveedor;');
      end)
    .&And('el proceso termina con el codigo que ella devuelve',
      procedure(W: TCliWorld)
      begin
        Expect(W.ExitCode).ToEqual(42);
      end)

  .Scenario('Falla la accion y despues la parada')
    .Given('una aplicacion CLI con una politica de excepciones declarada, una accion que lanza y un servicio alojado que falla al parar',
      procedure(W: TCliWorld)
      begin
        W.Builder := TCliApplication.CreateBuilder;
        // Un codigo distinto para cada fallo: con uno solo, el de la accion y el de la parada
        // darian el mismo verde.
        W.Builder.OnException(
          function(const Failure: TCliFailure): Integer
          begin
            W.PolicySaw := W.PolicySaw + Failure.Error.Message + ';';
            if Failure.Error.Message = 'el trabajo fallo' then
              Result := 42
            else
              Result := 43;
          end);
        Hosting.AddHostedService(W.Builder.Services, TRefusingStop);
        W.Builder.SetAction(
          function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
          begin
            raise Exception.Create('el trabajo fallo');
          end);
        W.App := W.Builder.Build;
      end)
    .When('se ejecuta',
      procedure(W: TCliWorld)
      begin
        W.ExitCode := RunQuietly(W.App);
      end)
    .&Then('el proceso termina con el codigo que la politica dio a la excepcion de la accion',
      procedure(W: TCliWorld)
      begin
        Expect(Format('%d %s', [W.ExitCode, W.PolicySaw])).ToEqual('42 el trabajo fallo;');
      end)

  .Scenario('Con la politica por defecto, un fallo al parar se informa una sola vez')
    .Given('una aplicacion CLI sin politica declarada, y un servicio alojado que falla al parar',
      procedure(W: TCliWorld)
      begin
        W.Builder := TCliApplication.CreateBuilder;
        Hosting.AddHostedService(W.Builder.Services, TRefusingStop);
        W.Builder.SetAction(
          function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
          begin
            Result := TCliExit.Ok;
          end);
        W.App := W.Builder.Build;
      end)
    .When('se ejecuta',
      procedure(W: TCliWorld)
      begin
        var Sink := TLinesSink.Create;
        var Keep: IBootstrapSink := Sink;
        W.ExitCode := RunReporting(W.App, Sink);
        W.Report := Sink.Lines;
      end)
    .&Then('el canal de arranque lo recibe una vez, y el proceso termina con Failure',
      procedure(W: TCliWorld)
      begin
        var Times := 0;
        var From := Pos('la parada fallo', W.Report);
        while From > 0 do
        begin
          Inc(Times);
          From := PosEx('la parada fallo', W.Report, From + 1);
        end;
        Expect(Format('%d informe(s), codigo %d', [Times, W.ExitCode]))
          .ToEqual(Format('1 informe(s), codigo %d', [TCliExit.Failure]));
      end)

  .Scenario('Soltar la aplicacion tras un fallo de la parada')
    .Given('una aplicacion CLI cuyo servicio alojado falla al parar, con la politica que deja salir la excepcion',
      procedure(W: TCliWorld)
      begin
        W.CountedBase := TCounted.Alive;
        W.Builder := TCliApplication.CreateBuilder;
        W.Builder.OnException(ExitPolicy.Rethrow());
        W.Builder.Services.AddSingleton<ICounted, TCounted>;
        Hosting.AddHostedService(W.Builder.Services, TRefusingStop);
        W.Builder.SetAction(
          function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
          begin
            Services.GetRequiredService<ICounted>;
            W.CountedDuring := TCounted.Alive - W.CountedBase;
            Result := TCliExit.Ok;
          end);
        W.App := W.Builder.Build;
        W.Watched := W.App;
      end)
    .When('se ejecuta y se suelta',
      procedure(W: TCliWorld)
      begin
        try
          W.ExitCode := RunQuietly(W.App);
          W.Report := '<no lanzo>';
        except
          on E: Exception do
            W.Report := E.Message;
        end;
        W.Report := W.Report + ' before=' + IfThen(Assigned(W.Watched), 'alive', 'released');
        W.App := nil;
        W.Builder := nil;
      end)
    .&Then('no queda viva ni la aplicacion ni su contenedor',
      procedure(W: TCliWorld)
      begin
        // El mensaje es el control de que la excepcion salio del Run, que es el camino donde la
        // referencia al host tiene que soltarse igual.
        Expect(Format('during=%d %s after=%s container=%d', [W.CountedDuring, W.Report,
          IfThen(Assigned(W.Watched), 'alive', 'released'), TCounted.Alive - W.CountedBase]))
          .ToEqual('during=1 la parada fallo before=alive after=released container=0');
      end)

.Rule('Lo que se escapa antes del Run lo decide la misma politica')

  .Scenario('La composicion anade un fichero obligatorio que no existe')
    .Given('una aplicacion CLI con una politica de excepciones declarada, y un fichero de configuracion que no existe',
      procedure(W: TCliWorld)
      begin
        W.ConfigFile := TPath.Combine(TPath.GetTempPath, 'daf-cli-missing-' + TGuid.NewGuid.ToString + '.json');
        W.Builder := TCliApplication.CreateBuilder;
        W.Builder.OnException(
          function(const Failure: TCliFailure): Integer
          begin
            W.Report := Failure.Error.Message;
            W.PolicySaw := IfThen(Failure.Reported, 'informado', 'sin informar') +
              IfThen(Failure.Services = nil, ' sin', ' con') + ' proveedor';
            Result := 42;
          end);
      end)
    .When('la composicion lo anade como obligatorio, y el except del .dpr delega en el builder',
      procedure(W: TCliWorld)
      begin
        // La carga de la fuente tambien informa por el canal de arranque; se aparta para que no se
        // confunda con la salida de las specs.
        var Saved := BootstrapLog.Sinks;
        BootstrapLog.ClearSinks;
        try
          try
            JsonConfig.AddFile(W.Builder.Configuration, W.ConfigFile);
            W.ExitCode := W.Builder.Build.Run;
          except
            on E: Exception do
              W.ExitCode := W.Builder.HandleException(E);
          end;
        finally
          for var Sink in Saved do
            BootstrapLog.AddSink(Sink);
        end;
      end)
    .&Then('la politica la recibe sin proveedor, y el proceso termina con su codigo',
      procedure(W: TCliWorld)
      begin
        Expect(Format('exit=%d %s', [W.ExitCode, W.PolicySaw])).ToEqual('exit=42 sin informar sin proveedor');
      end)
    .&And('lo que recibe nombra el fichero',
      procedure(W: TCliWorld)
      begin
        Expect(W.Report).ToContain(W.ConfigFile);
      end)

  .Scenario('Con la politica por defecto, un error de composicion se informa')
    .Given('una aplicacion CLI sin politica declarada, con un comando que ya tiene accion',
      procedure(W: TCliWorld)
      begin
        W.Builder := TCliApplication.CreateBuilder;
        W.Greet := CommandLine.Command('greet').SetAction(
          function(const Parsed: ICommandLineParseResult): Integer
          begin
            Result := 0;
          end);
        W.Builder.RootCommand.Add(W.Greet);
      end)
    .When('la composicion le declara otra, y el except del .dpr delega en el builder',
      procedure(W: TCliWorld)
      begin
        var Sink := TLinesSink.Create;
        var Keep: IBootstrapSink := Sink;
        var Saved := BootstrapLog.Sinks;
        BootstrapLog.ClearSinks;
        BootstrapLog.AddSink(Keep);
        try
          try
            W.Builder.SetAction(W.Greet,
              function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
              begin
                Result := 0;
              end);
            W.ExitCode := W.Builder.Build.Run;
          except
            on E: Exception do
              W.ExitCode := W.Builder.HandleException(E);
          end;
        finally
          W.Report := Sink.Lines;
          BootstrapLog.ClearSinks;
          for var Each in Saved do
            BootstrapLog.AddSink(Each);
        end;
      end)
    .&Then('el canal de arranque lo recibe, y el proceso termina con Failure',
      procedure(W: TCliWorld)
      begin
        Expect(Format('codigo %d, informa=%s', [W.ExitCode,
          BoolToStr(W.Report.Contains('already has an action'), True)]))
          .ToEqual(Format('codigo %d, informa=True', [TCliExit.Failure]));
      end)

  .Scenario('Un fallo de la construccion llega a la politica ya informado')
    .Given('una aplicacion CLI con una politica de excepciones declarada, y un servicio cuya dependencia nadie registra, con la validacion fijada',
      procedure(W: TCliWorld)
      begin
        W.Builder := TCliApplication.CreateBuilder;
        W.Builder.OnException(
          function(const Failure: TCliFailure): Integer
          begin
            W.PolicySaw := IfThen(Failure.Reported, 'informado', 'sin informar') +
              IfThen(Failure.Services = nil, ' sin', ' con') + ' proveedor';
            Result := 42;
          end);
        W.Builder.Services.AddSingleton<IWelcome, TWelcome>;
        W.Builder.ServiceProviderOptions := TServiceProviderOptions.Create(True, True);
      end)
    .When('se construye, y el except del .dpr delega en el builder',
      procedure(W: TCliWorld)
      begin
        var Saved := BootstrapLog.Sinks;
        BootstrapLog.ClearSinks;
        try
          try
            W.ExitCode := W.Builder.Build.Run;
          except
            on E: Exception do
              W.ExitCode := W.Builder.HandleException(E);
          end;
        finally
          for var Sink in Saved do
            BootstrapLog.AddSink(Sink);
        end;
      end)
    .&Then('la politica la recibe informada y sin proveedor, y el proceso termina con su codigo',
      procedure(W: TCliWorld)
      begin
        Expect(Format('exit=%d %s', [W.ExitCode, W.PolicySaw])).ToEqual('exit=42 informado sin proveedor');
      end)

  .Scenario('Lo que la politica deja salir del Run no se decide dos veces')
    .Given('una aplicacion CLI cuya politica cuenta sus llamadas y deja salir la excepcion, y una accion que lanza',
      procedure(W: TCliWorld)
      begin
        W.Builder := TCliApplication.CreateBuilder;
        W.Builder.OnException(
          function(const Failure: TCliFailure): Integer
          begin
            Inc(W.PolicyCalls);
            Result := ExitPolicy.Rethrow()(Failure);
          end);
        W.Builder.SetAction(
          function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
          begin
            raise Exception.Create('el trabajo fallo');
          end);
        W.App := W.Builder.Build;
      end)
    .When('se ejecuta, y el except del .dpr delega en el builder lo que sale',
      procedure(W: TCliWorld)
      begin
        try
          try
            W.ExitCode := W.App.Run;
          except
            on E: Exception do
              W.ExitCode := W.Builder.HandleException(E);
          end;
          W.Report := '<no lanzo>';
        except
          on E: Exception do
            W.Report := E.Message;
        end;
      end)
    .&Then('la politica la decide una vez, y la excepcion sigue saliendo',
      procedure(W: TCliWorld)
      begin
        Expect(Format('llamadas=%d salio=%s', [W.PolicyCalls, W.Report]))
          .ToEqual('llamadas=1 salio=el trabajo fallo');
      end)

.Rule('Mientras corre el Run, lo que el RTL no tiene a quien entregar va a la politica')

  .Scenario('Un modulo de datos cuyo OnCreate lanza')
    .Given('una aplicacion CLI con una politica de excepciones declarada, y una accion que crea un modulo de datos cuyo OnCreate lanza',
      procedure(W: TCliWorld)
      begin
        W.Builder := TCliApplication.CreateBuilder;
        W.Builder.OnException(
          function(const Failure: TCliFailure): Integer
          begin
            W.PolicySaw := Failure.Error.Message;
            Result := 42;
          end);
        W.Builder.SetAction(
          function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
          begin
            var Module := TFailingModule.CreateFailing;
            W.Report := 'creado';
            Module.Free;
            Result := TCliExit.Ok;
          end);
        W.App := W.Builder.Build;
      end)
    .When('se ejecuta',
      procedure(W: TCliWorld)
      begin
        W.ExitCode := W.App.Run;
      end)
    .&Then('la politica lo decide, el modulo se crea igual, y el proceso termina con el codigo de ella',
      procedure(W: TCliWorld)
      begin
        Expect(Format('exit=%d %s politica=%s', [W.ExitCode, W.Report, W.PolicySaw]))
          .ToEqual('exit=42 creado politica=el modulo fallo al crearse');
      end)

  .Scenario('Un procedimiento encolado que lanza')
    .Given('una aplicacion CLI con una politica de excepciones declarada, y una accion que encola un procedimiento que lanza',
      procedure(W: TCliWorld)
      begin
        W.Builder := TCliApplication.CreateBuilder;
        W.Builder.OnException(
          function(const Failure: TCliFailure): Integer
          begin
            W.PolicySaw := Failure.Error.Message + IfThen(Failure.Services = nil, ' sin', ' con') +
              ' proveedor';
            Result := 42;
          end);
        W.Builder.SetAction(
          function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
          begin
            TThread.ForceQueue(nil,
              procedure
              begin
                raise Exception.Create('lo encolado fallo');
              end);
            CheckSynchronize;
            W.Report := 'siguio';
            Result := TCliExit.Ok;
          end);
        W.App := W.Builder.Build;
      end)
    .When('se ejecuta',
      procedure(W: TCliWorld)
      begin
        W.ExitCode := W.App.Run;
      end)
    .&Then('la politica lo decide con proveedor, la accion sigue, y el proceso termina con el codigo de ella',
      procedure(W: TCliWorld)
      begin
        Expect(Format('exit=%d %s politica=%s', [W.ExitCode, W.Report, W.PolicySaw]))
          .ToEqual('exit=42 siguio politica=lo encolado fallo con proveedor');
      end)

  .Scenario('Al terminar el Run, el RTL recupera el gancho que tenia')
    .Given('una aplicacion CLI cuya accion mira el gancho del RTL',
      procedure(W: TCliWorld)
      begin
        W.Builder := TCliApplication.CreateBuilder;
        W.Builder.SetAction(
          function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
          begin
            W.Report := W.Report + ' durante=' + IfThen(Assigned(ApplicationHandleException), 'puesto', 'nil');
            Result := TCliExit.Ok;
          end);
        W.App := W.Builder.Build;
      end)
    .When('se ejecuta',
      procedure(W: TCliWorld)
      begin
        W.Report := 'antes=' + IfThen(Assigned(ApplicationHandleException), 'puesto', 'nil');
        W.ExitCode := W.App.Run;
        W.Report := W.Report + ' despues=' + IfThen(Assigned(ApplicationHandleException), 'puesto', 'nil');
      end)
    .&Then('el gancho esta puesto solo mientras corre',
      procedure(W: TCliWorld)
      begin
        Expect(W.Report).ToEqual('antes=nil durante=puesto despues=nil');
      end)

.Rule('Las opciones del contenedor llegan por el builder de la variante')

  .Scenario('Fijar la validacion en el builder CLI la aplica al construir')
    .Given('una aplicacion CLI con un servicio cuya dependencia nadie registra, y la validacion fijada',
      procedure(W: TCliWorld)
      begin
        W.Builder := TCliApplication.CreateBuilder;
        W.Builder.Services.AddSingleton<IWelcome, TWelcome>;
        W.Builder.ServiceProviderOptions := TServiceProviderOptions.Create(True, True);
      end)
    .When('se construye',
      procedure(W: TCliWorld)
      begin
        // El fallo del host sale tambien por el canal de arranque; se aparta para que no se
        // confunda con la salida de las specs.
        var Saved := BootstrapLog.Sinks;
        BootstrapLog.ClearSinks;
        try
          try
            W.App := W.Builder.Build;
            W.Report := '<construyo sin protestar>';
          except
            on E: Exception do
              W.Report := E.Message;
          end;
        finally
          for var Sink in Saved do
            BootstrapLog.AddSink(Sink);
        end;
      end)
    .&Then('falla nombrando la dependencia que falta',
      procedure(W: TCliWorld)
      begin
        Expect(W.Report).ToContain('IGreeter');
      end)

.Rule('Sin acceso global')

  // En consola la plataforma no tiene ningun Application global que el usuario espere, .NET
  // tampoco lo tiene, y anadirlo seria un service locator sin nada que lo justifique. Donde si
  // lo hay --VCL y FMX-- es porque la plataforma ya lo puso.
  .Scenario('Una aplicacion de consola no tiene donde pedir servicios por la puerta de atras')
    .Given('la variante CLI compilada').NoAction
    .When('se busca un acceso global a la aplicacion o a su proveedor',
      procedure(W: TCliWorld)
      begin
        W.Report := (GlobalAccessorsIn(TCliApplication) + ' ' +
                     GlobalAccessorsIn(TCliApplicationBuilder)).Trim;
        if W.Report.IsEmpty then
          W.Report := '<ninguno>';
      end)
    .&Then('no hay ninguno',
      procedure(W: TCliWorld)
      begin
        Expect(W.Report).ToEqual('<ninguno>');
      end);

end.
