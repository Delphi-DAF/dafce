unit Startup.Feat;

interface

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.Extensions.Hosting,
  Daf.Extensions.DependencyInjection,
  Daf.Application.Builder,
  Doubles.Feat;

type
  /// Una aplicacion propia, que es como se usa la capa: CreateHostBuilder toma la clase del
  /// Self que lo llama, asi que es TSpecApp --y no TDafApplication-- lo que Build instancia.
  TSpecApp = class(TDafApplication)
  strict private
    class var FExecuted: Integer;
  protected
    procedure Execute; override;
  public
    class property Executed: Integer read FExecuted write FExecuted;
  end;

  TStartupWorld = class
  public
    App: IDafApplication;
    Greeting: string;
    ExecArgReceived: Boolean;
    IsHost: Boolean;
    HostAssigned: Boolean;
    ImplicitTrace: string;
    ExplicitFalseTrace: string;
    ExplicitTrueTrace: string;
    destructor Destroy; override;
  end;

procedure TSpecApp.Execute;
begin
  Inc(FExecuted);
end;

/// Un host construido y no parado se queda vivo: THost solo suelta su contenedor en Stop. Los
/// escenarios que no llegan a Run lo dejan aqui, y despues de Run no hay nada que parar porque
/// TDafApplication.Stop se deshace de su host.
destructor TStartupWorld.Destroy;
begin
  if Assigned(App) and Assigned(App.Host) then
    App.Stop;
  App := nil;
  inherited;
end;

/// Construye por el punto de entrada de verdad. Se devuelve como IDafApplication porque es lo
/// que Build devuelve, y ese es justo el hecho que la guia describia mal: Build da la fachada
/// de aplicacion, no un IHost.
function BuildApp(const AppClass: TDafApplicationClass): IDafApplication;
begin
  var Builder := AppClass.CreateHostBuilder;
  // Estilo inmediato: el registro ocurre donde esta escrito, sin callback que alguien dispare
  // mas tarde.
  Builder.Services.AddSingleton<IGreeter, TGreeter>;
  Result := Builder.Build;
end;

/// Monta una aplicacion sobre un host espia y devuelve lo que el host recibio. Es la medida del
/// valor por defecto de WaitForShutdown: si el defecto fuese True, con el host de verdad esto se
/// colgaria esperando un Ctrl+C, y una spec colgada no es una spec roja.
function TraceOfRun(const Explicit, WaitForShutdown: Boolean): string;
begin
  var Provider := SpecServiceProvider;
  var Spy := TSpyHost.Create(Provider);
  var Host: IHost := Spy;
  var App: IDafApplication := TDafApplication.Create(Host);
  if Explicit then
    App.Run(WaitForShutdown)
  else
    App.Run;
  Result := Spy.Trace;
  // El contenedor sostiene el IHostEnvironment que se le registro, y THostEnvironment es un
  // TSingleton que en Debug afirma ser el unico vivo: sin soltar el contenedor, la llamada
  // siguiente muere en ese Assert en vez de medir nada.
  Provider.ShutDown;
end;

initialization

Feature('''
Feature Arrancar una aplicacion DAF @application @startup

  Como quien escribe el .dpr de una aplicacion
  Quiero un punto de entrada que construya, resuelva y ejecute
  Para no tener que averiguarlo comparando dos samples
''')

.UseWorld<TStartupWorld>

.Rule('El punto de entrada construye una aplicacion utilizable')

  .Scenario('Build devuelve la aplicacion, con sus servicios resueltos')
    .When('se construye por CreateHostBuilder y se resuelve lo registrado',
      procedure(W: TStartupWorld)
      begin
        W.App := BuildApp(TDafApplication);
        W.Greeting := W.App.Services.GetRequiredService<IGreeter>.Greet;
      end)
    .&Then('la aplicacion existe y entrega el servicio',
      procedure(W: TStartupWorld)
      begin
        Expect(Assigned(W.App)).ToBeTrue;
        Expect(W.Greeting).ToEqual('hola');
      end)

  .Scenario('Run sin Exec ejecuta el Execute de la aplicacion')
    .Given('una aplicacion que cuenta sus ejecuciones',
      procedure(W: TStartupWorld)
      begin
        TSpecApp.Executed := 0;
        W.App := BuildApp(TSpecApp);
      end)
    .When('se llama a Run sin nada que ejecutar',
      procedure(W: TStartupWorld)
      begin
        W.App.Run;
      end)
    .&Then('Execute corrio una vez',
      procedure(W: TStartupWorld)
      begin
        Expect(TSpecApp.Executed).ToEqual(1);
      end)

  .Scenario('Run con Exec recibe el proveedor de servicios')
    .Given('una aplicacion construida',
      procedure(W: TStartupWorld)
      begin
        W.App := BuildApp(TDafApplication);
      end)
    .When('se llama a Run pasandole que hacer',
      procedure(W: TStartupWorld)
      begin
        W.App.Run(
          procedure(Services: IServiceProvider)
          begin
            // IServiceProvider es un record, no una interfaz: Assigned no aplica y da E2008.
            W.ExecArgReceived := Services <> nil;
            W.Greeting := Services.GetRequiredService<IGreeter>.Greet;
          end);
      end)
    .&Then('el proc recibio un proveedor del que resolver',
      procedure(W: TStartupWorld)
      begin
        Expect(W.ExecArgReceived).ToBeTrue;
        Expect(W.Greeting).ToEqual('hola');
      end)

.Rule('La fachada de aplicacion no es el host: lo tiene')

  // Fija lo que la guia afirmaba al reves durante meses: que IDafApplication extendia IHost.
  // Redeclara su superficie, que no es lo mismo, y por eso no se puede pasar una aplicacion
  // donde se espera un host.
  .Scenario('Una aplicacion no se puede usar como si fuera un host')
    .Given('una aplicacion construida',
      procedure(W: TStartupWorld)
      begin
        W.App := BuildApp(TDafApplication);
      end)
    .When('se le pregunta si es un IHost',
      procedure(W: TStartupWorld)
      begin
        W.IsHost := Supports(W.App, IHost);
        W.HostAssigned := Assigned(W.App.Host);
      end)
    .&Then('no lo es, pero expone el suyo',
      procedure(W: TStartupWorld)
      begin
        Expect(W.IsHost).ToBeFalse;
        Expect(W.HostAssigned).ToBeTrue;
      end)

.Rule('Run no espera el apagado si no se le pide')

  // La guia documentaba WaitForShutdown con defecto True. Es False, y la diferencia la sufre
  // quien escribe .Run esperando que su aplicacion se quede viva y se le termina sola.
  .Scenario('El valor por defecto de WaitForShutdown es False')
    .When('se ejecuta Run sin argumentos, con False y con True',
      procedure(W: TStartupWorld)
      begin
        W.ImplicitTrace := TraceOfRun(False, False);
        W.ExplicitFalseTrace := TraceOfRun(True, False);
        W.ExplicitTrueTrace := TraceOfRun(True, True);
      end)
    .&Then('sin argumentos hace lo mismo que con False',
      procedure(W: TStartupWorld)
      begin
        Expect(W.ImplicitTrace).ToEqual('start,stop');
        Expect(W.ExplicitFalseTrace).ToEqual(W.ImplicitTrace);
      end)
    .&And('y solo con True se espera el apagado',
      procedure(W: TStartupWorld)
      begin
        Expect(W.ExplicitTrueTrace).ToEqual('start,waitforshutdown,stop');
      end);

end.
