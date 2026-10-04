unit WebVariant.Feat;

interface

uses
  System.SysUtils,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Hosting,
  Daf.Cli,
  Daf.Web.Application,
  Web.Doubles;

type
  // En la seccion de interfaz por obligacion: el contenedor resuelve por RTTI.
  IShared = interface(IInvokable)
    ['{667A3A79-281D-4414-BE9F-AD09B6D0EA75}']
    function Name: string;
  end;

  TShared = class(TInterfacedObject, IShared)
  public
    function Name: string;
  end;

  /// Un paquete de terceros que aporta registros de arranque. No sabe --ni puede saber-- que
  /// variante lo va a consumir: solo ve IServiceCollection.
  TSharedModule = class
  public
    class procedure AddTo(const Services: IServiceCollection);
  end;

  /// El estado que el Given deja montado y el When usa. Sostenerlo aqui es lo que permite que
  /// el When haga LA ACCION: con un World de solo texto, el Given tiene que ejecutarla para
  /// poder dejar escrito su resultado, y entonces el escenario ya no comprueba comportamiento
  /// sino la cadena que el propio Given acaba de componer.
  TWebVariantWorld = class
  public
    CliBuilder: ICliApplicationBuilder;
    WebBuilder: IWebApplicationBuilder;
    WebBuilderWithArg: IWebApplicationBuilder;
    CliServices: IServiceProvider;
    WebServices: IServiceProvider;
    App: IWebApplication;
    Report: string;
    PolicySaw: string;
    PolicyCalls: Integer;
    /// <summary>
    ///   Empieza en -1 y no en 0: "termino con 0" es justo lo que tendria un campo que el When no
    ///   llego a escribir.
    /// </summary>
    ExitCode: Integer;
    CliExitCode: Integer;
    ConfigFile: string;
    ServerStarted: Boolean;
    Held: THeldPort;
    constructor Create;
    destructor Destroy; override;
    procedure OnServerStarted(Sender: TObject);
    /// <summary>Una aplicacion web en un puerto libre, con el servidor y su parada al arrancar.</summary>
    function WebOnFreePort: IWebApplicationBuilder;
    /// <summary>Ejecuta con el canal de arranque apartado, y deja el Report de lo que salio.</summary>
    procedure RunQuietly;
  end;

  /// Falla al pararse: es lo que pone a prueba la liberacion de una aplicacion sin parar.
  TRefusingStop = class(TInterfacedObject, IHostedService)
  public
    procedure Start;
    procedure Stop;
  end;

  /// Falla al arrancar.
  TRefusingStart = class(TInterfacedObject, IHostedService)
  public
    procedure Start;
    procedure Stop;
  end;

  /// Pide la parada en cuanto arranca: es lo que deja volver al Run, que espera la parada.
  TStopAtStart = class(TInterfacedObject, IHostedService)
  private
    FLifetime: IHostApplicationLifetime;
  public
    constructor Create(const Lifetime: IHostApplicationLifetime);
    procedure Start;
    procedure Stop;
  end;

  /// Anota en la bitacora cuando arranca y cuando para, y si el gancho del RTL esta puesto.
  TNoteService = class(TInterfacedObject, IHostedService)
  public
    procedure Start;
    procedure Stop;
  end;


implementation

uses
  System.Classes,
  System.IOUtils,
  System.StrUtils,
  Daf.MiniSpec,
  Daf.CommandLine,
  Daf.Extensions.Logging,
  Daf.Extensions.Configuration,
  Daf.Configuration.Json,
  Daf.Extensions.HttpServer,
  Daf.Web.Router;

var
  Log: string;
  /// El gancho del RTL tal como lo vio el servicio alojado al arrancar.
  HookAtStart: TMethod;

/// El gancho del RTL como par de punteros, para comparar si es el mismo.
function CurrentHook: TMethod;
begin
  Result := TMethod(ApplicationHandleException);
end;

function SameHook(const A, B: TMethod): Boolean;
begin
  Result := (A.Code = B.Code) and (A.Data = B.Data);
end;

constructor TWebVariantWorld.Create;
begin
  inherited;
  ExitCode := -1;
  CliExitCode := -1;
end;

procedure TWebVariantWorld.OnServerStarted(Sender: TObject);
begin
  ServerStarted := True;
end;

function TWebVariantWorld.WebOnFreePort: IWebApplicationBuilder;
begin
  Result := TWebApplication.CreateBuilder(['--Web:Port=' + FindFreePort.ToString]);
  WebApp.AddHttpServer(Result.Services);
  Hosting.AddHostedService(Result.Services, TStopAtStart);
end;

procedure TWebVariantWorld.RunQuietly;
begin
  // El host informa por el canal de arranque lo que falla al arrancar o al parar; se aparta para
  // que no se confunda con la salida de las specs.
  var Saved := BootstrapLog.Sinks;
  BootstrapLog.ClearSinks;
  try
    try
      ExitCode := App.Run;
      Report := '<no lanzo>';
    except
      on E: Exception do
        Report := E.ClassName + ': ' + E.Message;
    end;
  finally
    for var Sink in Saved do
      BootstrapLog.AddSink(Sink);
  end;
end;

destructor TWebVariantWorld.Destroy;
begin
  Held.Free;
  // Un proveedor construido a mano retiene sus singletons hasta que se le cierra. Sin esto el
  // runner acaba con una fuga que no es del codigo medido sino del montaje del escenario, y una
  // fuga en el informe se lee igual la ponga quien la ponga.
  if CliServices <> nil then
    CliServices.ShutDown;
  if WebServices <> nil then
    WebServices.ShutDown;
  inherited;
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

constructor TStopAtStart.Create(const Lifetime: IHostApplicationLifetime);
begin
  inherited Create;
  FLifetime := Lifetime;
end;

procedure TStopAtStart.Start;
begin
  FLifetime.StopApplication;
end;

procedure TStopAtStart.Stop;
begin
end;

procedure TNoteService.Start;
begin
  HookAtStart := CurrentHook;
  Log := Log + 'start;';
end;

procedure TNoteService.Stop;
begin
  Log := Log + 'stop;';
end;

function TShared.Name: string;
begin
  Result := 'shared';
end;

class procedure TSharedModule.AddTo(const Services: IServiceCollection);
begin
  Services.AddSingleton<IShared, TShared>;
end;

// --- Feature definition ---

initialization

Feature('''
Feature La variante Web de la familia @web @application-family

  Como quien escribe una aplicacion web
  Quiero que el arranque se exprese igual que el de una CLI
  Para que solo difieran donde las variantes difieren de verdad
''')

.UseWorld<TWebVariantWorld>

.Rule('Dos variantes se arrancan igual hasta donde pueden')

  .Scenario('El registro, la configuracion y el entorno se expresan igual en las dos')
    .Given('el arranque de una CLI y el de una web, escritos con las mismas llamadas',
      procedure(W: TWebVariantWorld)
      begin
        W.CliBuilder := TCliApplication.CreateBuilder;
        W.CliBuilder.Services.AddSingleton<IShared, TShared>;
        W.CliBuilder.Configuration['App:Name'] := 'cli';

        W.WebBuilder := TWebApplication.CreateBuilder;
        W.WebBuilder.Services.AddSingleton<IShared, TShared>;
        W.WebBuilder.Configuration['App:Name'] := 'web';
      end)
    .When('se le pregunta a cada uno por lo que se le escribio',
      procedure(W: TWebVariantWorld)
      begin
        W.Report := Format('%s/%s env=%s',
          [W.CliBuilder.Configuration['App:Name'], W.WebBuilder.Configuration['App:Name'],
           BoolToStr(W.CliBuilder.Environment.EnvironmentName =
                     W.WebBuilder.Environment.EnvironmentName, True)]);
      end)
    .&Then('las tres cosas se escriben igual y responden igual',
      procedure(W: TWebVariantWorld)
      begin
        Expect(W.Report).ToEqual('cli/web env=True');
      end)

  // La razon de que el contrato comun exista: sin el, un paquete tendria que ofrecer una via
  // por variante, o el consumidor elegir la suya.
  .Scenario('Un paquete que aporta registros de arranque sirve a cualquier variante')
    .Given('el arranque de dos variantes distintas',
      procedure(W: TWebVariantWorld)
      begin
        W.CliBuilder := TCliApplication.CreateBuilder;
        W.WebBuilder := TWebApplication.CreateBuilder;
      end)
    .When('un paquete que solo conoce IServiceCollection registra en los dos',
      procedure(W: TWebVariantWorld)
      begin
        // Por el contrato comun y sin preguntar cual es cual, que es lo que hace de las dos
        // variantes una familia.
        for var Builder in TArray<IHostApplicationBuilder>.Create(W.CliBuilder, W.WebBuilder) do
          TSharedModule.AddTo(Builder.Services);

        W.CliServices := W.CliBuilder.Services.BuildServiceProvider;
        W.WebServices := W.WebBuilder.Services.BuildServiceProvider;
      end)
    .&Then('lo que registro se resuelve en las dos',
      procedure(W: TWebVariantWorld)
      begin
        Expect(W.CliServices.GetRequiredService<IShared>.Name + '/' +
               W.WebServices.GetRequiredService<IShared>.Name).ToEqual('shared/shared');
      end)

.Rule('Lo que la variante Web aporta')

  // El puerto se SIEMBRA en el manager antes de entregarlo, asi que un argumento posterior lo
  // pisa. Si la variante lo anadiera despues, llegaria tarde.
  .Scenario('El puerto por defecto se puede pisar por linea de comandos')
    .Given('dos arranques web, uno tal cual y otro con el puerto por argumento',
      procedure(W: TWebVariantWorld)
      begin
        W.WebBuilder := TWebApplication.CreateBuilder;
        W.WebBuilderWithArg := TWebApplication.CreateBuilder(['--Web:Port=9000']);
      end)
    .When('se lee el puerto de la configuracion de cada uno',
      procedure(W: TWebVariantWorld)
      begin
        W.Report := Format('defecto=%s dado=%s',
          [W.WebBuilder.Configuration[TWebDefaults.PortKey],
           W.WebBuilderWithArg.Configuration[TWebDefaults.PortKey]]);
      end)
    .&Then('el argumento gana al valor sembrado',
      procedure(W: TWebVariantWorld)
      begin
        Expect(W.Report).ToEqual('defecto=8080 dado=9000');
      end)

  // La fachada la aporta Daf.Web, y el componedor generico no la nombra: es lo que evita que
  // el grafo de paquetes se invierta.
  .Scenario('El servidor se aporta por fachada, no por miembro del componedor generico')
    .Given('un arranque web con el servidor http anadido por la fachada',
      procedure(W: TWebVariantWorld)
      begin
        W.WebBuilder := TWebApplication.CreateBuilder;
        WebApp.AddHttpServer(W.WebBuilder.Services);
      end)
    .When('se construye la aplicacion y se mapean los controladores',
      procedure(W: TWebVariantWorld)
      begin
        W.App := W.WebBuilder.Build;
        W.App.MapControllers;
      end)
    .&Then('el servidor y el enrutado quedan resueltos por el contenedor',
      procedure(W: TWebVariantWorld)
      begin
        // Assigned pide una variable, no el resultado de una llamada: sobre la llamada da E2036
        // y el error sale en la linea siguiente, lejos de la causa.
        var Server: IDafHttpServer := W.App.Server;
        var Router: IWebRouter := W.App.Services.GetService<IWebRouter>;
        Expect(Format('server=%s router=%s',
          [BoolToStr(Assigned(Server), True), BoolToStr(Assigned(Router), True)]))
          .ToEqual('server=True router=True');
      end)

  // src/web no tiene un solo .dpr fuera de sus specs: este escenario es lo unico que se daria
  // cuenta si su arranque dejara de arrancar.
  .Scenario('El arranque web llega hasta un servidor escuchando')
    .Given('una aplicacion web compuesta de punta a punta',
      procedure(W: TWebVariantWorld)
      begin
        W.WebBuilder := TWebApplication.CreateBuilder(['--Web:Port=8099']);
        WebApp.AddHttpServer(W.WebBuilder.Services);
        W.App := W.WebBuilder.Build;
        W.App.MapControllers;
      end)
    .When('se arranca el servidor, se mira, y se para',
      procedure(W: TWebVariantWorld)
      begin
        W.App.Server.Start;
        try
          W.Report := Format('port=%d active=%s',
            [W.App.Server.Port, BoolToStr(W.App.Server.Active, True)]);
        finally
          W.App.Server.Stop;
        end;
      end)
    .&Then('escucho en el puerto que dijo la configuracion',
      procedure(W: TWebVariantWorld)
      begin
        Expect(W.Report).ToEqual('port=8099 active=True');
      end)

.Rule('Todo lo que se escapa de la variante Web lo decide su politica')

  .Scenario('El servidor HTTP no puede arrancar')
    .Given('una aplicacion web con una politica de excepciones declarada, y su puerto ocupado',
      procedure(W: TWebVariantWorld)
      begin
        Log := '';
        W.Held := THeldPort.Create(FindFreePort);
        W.WebBuilder := TWebApplication.CreateBuilder(['--Web:Port=' + W.Held.Port.ToString]);
        W.WebBuilder.OnException(
          function(const Failure: TAppFailure): Integer
          begin
            W.PolicySaw := Failure.Error.Message + IfThen(Failure.Services = nil, ' sin', ' con') +
              ' proveedor' + IfThen(Failure.Reported, ' informado', ' sin informar');
            Result := 42;
          end);
        WebApp.AddHttpServer(W.WebBuilder.Services);
        Hosting.AddHostedService(W.WebBuilder.Services, TNoteService);
        W.App := W.WebBuilder.Build;
      end)
    .When('se ejecuta',
      procedure(W: TWebVariantWorld)
      begin
        W.RunQuietly;
      end)
    .&Then('el Run no lanza, la politica la recibe con proveedor, y el proceso termina con su codigo',
      procedure(W: TWebVariantWorld)
      begin
        Expect(Format('%s exit=%d %s', [W.Report, W.ExitCode, W.PolicySaw])).ToEqual(
          Format('<no lanzo> exit=42 Cannot start: port %d is in use con proveedor sin informar',
          [W.Held.Port]));
      end)
    .&And('el host se para',
      procedure(W: TWebVariantWorld)
      begin
        Expect(Log).ToEqual('start;stop;');
      end)

  .Scenario('Un servicio alojado falla al arrancar')
    .Given('una aplicacion web con una politica de excepciones declarada, y un servicio alojado que falla al arrancar',
      procedure(W: TWebVariantWorld)
      begin
        W.WebBuilder := W.WebOnFreePort;
        W.WebBuilder.OnException(
          function(const Failure: TAppFailure): Integer
          begin
            W.PolicySaw := Failure.Error.Message + IfThen(Failure.Reported, ' informado', ' sin informar');
            Result := 42;
          end);
        Hosting.AddHostedService(W.WebBuilder.Services, TRefusingStart);
        W.App := W.WebBuilder.Build;
        W.App.Server.OnStarted := W.OnServerStarted;
      end)
    .When('se ejecuta',
      procedure(W: TWebVariantWorld)
      begin
        W.RunQuietly;
      end)
    .&Then('la politica la recibe ya informada, el servidor no arranca, y el proceso termina con su codigo',
      procedure(W: TWebVariantWorld)
      begin
        Expect(Format('%s exit=%d %s servidor=%s', [W.Report, W.ExitCode, W.PolicySaw,
          BoolToStr(W.ServerStarted, True)]))
          .ToEqual('<no lanzo> exit=42 el arranque fallo informado servidor=False');
      end)

  .Scenario('Una aplicacion web que termina bien')
    .Given('una aplicacion web sin fallos, con una politica de excepciones declarada',
      procedure(W: TWebVariantWorld)
      begin
        W.WebBuilder := W.WebOnFreePort;
        W.WebBuilder.OnException(
          function(const Failure: TAppFailure): Integer
          begin
            W.PolicySaw := Failure.Error.Message;
            Result := 42;
          end);
        W.App := W.WebBuilder.Build;
        W.App.Server.OnStarted := W.OnServerStarted;
      end)
    .When('se ejecuta y se le pide que pare',
      procedure(W: TWebVariantWorld)
      begin
        W.RunQuietly;
      end)
    .&Then('el servidor arranco, y el proceso termina con el codigo de exito sin pasar por la politica',
      procedure(W: TWebVariantWorld)
      begin
        Expect(Format('%s exit=%d servidor=%s politica=%s', [W.Report, W.ExitCode,
          BoolToStr(W.ServerStarted, True), W.PolicySaw]))
          .ToEqual(Format('<no lanzo> exit=%d servidor=True politica=', [TAppExit.Ok]));
      end)

  .Scenario('Un fallo de la parada lo decide la politica')
    .Given('una aplicacion web con una politica de excepciones declarada, y un servicio alojado que falla al parar',
      procedure(W: TWebVariantWorld)
      begin
        W.WebBuilder := W.WebOnFreePort;
        W.WebBuilder.OnException(
          function(const Failure: TAppFailure): Integer
          begin
            W.PolicySaw := Failure.Error.Message + IfThen(Failure.Services = nil, ' sin', ' con') +
              ' proveedor' + IfThen(Failure.Reported, ' informado', ' sin informar');
            Result := 42;
          end);
        Hosting.AddHostedService(W.WebBuilder.Services, TRefusingStop);
        W.App := W.WebBuilder.Build;
      end)
    .When('se ejecuta y se le pide que pare',
      procedure(W: TWebVariantWorld)
      begin
        W.RunQuietly;
      end)
    .&Then('la politica recibe la excepcion de la parada sin proveedor y ya informada, y el proceso termina con su codigo',
      procedure(W: TWebVariantWorld)
      begin
        Expect(Format('%s exit=%d %s', [W.Report, W.ExitCode, W.PolicySaw]))
          .ToEqual('<no lanzo> exit=42 la parada fallo sin proveedor informado');
      end)

  .Scenario('Lo que falla al componer una aplicacion web lo decide la misma politica')
    .Given('una aplicacion web con una politica de excepciones declarada, y un fichero de configuracion que no existe',
      procedure(W: TWebVariantWorld)
      begin
        W.ConfigFile := TPath.Combine(TPath.GetTempPath, 'daf-web-missing-' + TGuid.NewGuid.ToString + '.json');
        W.WebBuilder := TWebApplication.CreateBuilder;
        W.WebBuilder.OnException(
          function(const Failure: TAppFailure): Integer
          begin
            W.PolicySaw := IfThen(Failure.Services = nil, 'sin', 'con') + ' proveedor';
            W.Report := Failure.Error.Message;
            Result := 42;
          end);
      end)
    .When('la composicion lo anade como obligatorio, y el except del .dpr lo entrega al builder',
      procedure(W: TWebVariantWorld)
      begin
        var Saved := BootstrapLog.Sinks;
        BootstrapLog.ClearSinks;
        try
          try
            JsonConfig.AddFile(W.WebBuilder.Configuration, W.ConfigFile);
            W.App := W.WebBuilder.Build;
            W.ExitCode := W.App.Run;
          except
            on E: Exception do
              W.ExitCode := W.WebBuilder.HandleException(E);
          end;
        finally
          for var Sink in Saved do
            BootstrapLog.AddSink(Sink);
        end;
      end)
    .&Then('la politica la recibe sin proveedor, y el proceso termina con su codigo',
      procedure(W: TWebVariantWorld)
      begin
        Expect(Format('exit=%d %s nombra=%s', [W.ExitCode, W.PolicySaw,
          BoolToStr(W.Report.Contains(W.ConfigFile), True)]))
          .ToEqual('exit=42 sin proveedor nombra=True');
      end)

  .Scenario('Una aplicacion web no toca el gancho del RTL')
    // El gancho lo pone el manejador de peticiones de WebBroker, y es suyo: atiende con el lo que
    // falla dentro de una peticion.
    .Given('una aplicacion web cuyo servicio alojado mira el gancho del RTL al arrancar',
      procedure(W: TWebVariantWorld)
      begin
        Log := '';
        W.WebBuilder := W.WebOnFreePort;
        Hosting.AddHostedService(W.WebBuilder.Services, TNoteService);
        W.App := W.WebBuilder.Build;
      end)
    .When('se ejecuta',
      procedure(W: TWebVariantWorld)
      begin
        var Before := CurrentHook;
        W.ExitCode := W.App.Run;
        W.Report := Format('%s durante=%s despues=%s', [Log,
          BoolToStr(SameHook(HookAtStart, Before), True),
          BoolToStr(SameHook(CurrentHook, Before), True)]);
      end)
    .&Then('el gancho sigue siendo el que habia, tambien mientras corre',
      procedure(W: TWebVariantWorld)
      begin
        Expect(W.Report).ToEqual('start;stop; durante=True despues=True');
      end)

.Rule('La politica es de la familia')

  .Scenario('Una politica escrita una vez sirve a CLI y a Web')
    .Given('una politica de excepciones escrita una vez, y una aplicacion CLI y otra web que fallan',
      procedure(W: TWebVariantWorld)
      begin
        var Policy: TAppExceptionPolicy :=
          function(const Failure: TAppFailure): Integer
          begin
            Inc(W.PolicyCalls);
            Result := 42;
          end;
        W.CliBuilder := TCliApplication.CreateBuilder;
        W.CliBuilder.OnException(Policy);
        W.CliBuilder.SetAction(
          function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
          begin
            raise Exception.Create('el trabajo fallo');
          end);
        W.WebBuilder := W.WebOnFreePort;
        W.WebBuilder.OnException(Policy);
        Hosting.AddHostedService(W.WebBuilder.Services, TRefusingStart);
        W.App := W.WebBuilder.Build;
      end)
    .When('se ejecutan las dos',
      procedure(W: TWebVariantWorld)
      begin
        W.CliExitCode := W.CliBuilder.Build.Run;
        W.RunQuietly;
      end)
    .&Then('las dos la aceptan tal cual, y las dos terminan con el codigo que ella devuelve',
      procedure(W: TWebVariantWorld)
      begin
        Expect(Format('cli=%d web=%d llamadas=%d', [W.CliExitCode, W.ExitCode, W.PolicyCalls]))
          .ToEqual('cli=42 web=42 llamadas=2');
      end)

.Rule('Lo que falla al parar no sale del destructor')

  .Scenario('Soltar una aplicacion web sin ejecutarla, cuyo servicio alojado falla al parar')
    .Given('una aplicacion web construida, con un servicio alojado que falla al parar',
      procedure(W: TWebVariantWorld)
      begin
        W.WebBuilder := TWebApplication.CreateBuilder;
        Hosting.AddHostedService(W.WebBuilder.Services, TRefusingStop);
        W.App := W.WebBuilder.Build;
      end)
    .When('se suelta sin haberla ejecutado',
      procedure(W: TWebVariantWorld)
      begin
        // La parada informa su fallo por el canal de arranque; se aparta para que no se confunda
        // con la salida de las specs.
        var Saved := BootstrapLog.Sinks;
        BootstrapLog.ClearSinks;
        try
          try
            W.App := nil;
            W.WebBuilder := nil;
            W.Report := '<sin excepcion>';
          except
            on E: Exception do
              W.Report := E.ClassName + ': ' + E.Message;
          end;
        finally
          for var Sink in Saved do
            BootstrapLog.AddSink(Sink);
        end;
      end)
    .&Then('la liberacion termina sin error',
      procedure(W: TWebVariantWorld)
      begin
        Expect(W.Report).ToEqual('<sin excepcion>');
      end);

end.
