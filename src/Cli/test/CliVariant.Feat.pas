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

  /// El estado que el Given deja montado y el When usa. Un World de solo texto obliga al Given
  /// a ejecutar la accion para tener algo que dejar escrito, y entonces lo que el Then compara
  /// es la cadena que el propio montaje acaba de componer, no el comportamiento.
  TCliWorld = class
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
    CountedBase: Integer;
    CountedDuring: Integer;
    constructor Create;
  end;

implementation

uses
  System.Rtti,
  System.TypInfo,
  System.SyncObjs,
  Daf.MiniSpec,
  Daf.Extensions.Configuration,
  Daf.Configuration.CommandLine,
  Daf.Extensions.Logging;

var
  Log: string;

function TGreeter.Greet: string;
begin
  Result := 'hello';
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

constructor TCliWorld.Create;
begin
  inherited;
  ExitCode := -1;
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
        W.Builder.OnUnhandled(
          function(const E: Exception; const Services: IServiceProvider): Integer
          begin
            W.PolicySaw := E.Message;
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
        W.Builder.OnUnhandled(ExitPolicy.Rethrow());
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
