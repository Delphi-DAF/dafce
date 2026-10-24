unit ShutdownRequest.Feat;

interface

uses
  Daf.Extensions.Hosting;

type
  /// Pide la parada mientras el host arranca, antes de que nadie la atienda.
  TStopRequestingTask = class(TInterfacedObject, IStartupTask)
  public
    procedure Run;
  end;

  /// El host vive aqui y no dentro de una funcion porque el escenario tiene que poder mirarlo
  /// entre fases: con el host encerrado en un callback, tanto la consulta como la peticion de
  /// parada tenian que ocurrir ahi dentro, que es donde estaba el Given.
  TShutdownWorld = class
  public
    Host: IHost;
    Lifetime: IHostApplicationLifetime;
    Report: string;
    procedure BuildHost;
    procedure BuildHostRequestingStopAtStartup;
    procedure StartHost;
    procedure StopHost;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.Threading,
  Daf.Extensions.DependencyInjection,
  Daf.Hosting;

{ TStopRequestingTask }

procedure TStopRequestingTask.Run;
begin
  TShutdownHook.RequestShutdown;
end;

{ TShutdownWorld }

procedure TShutdownWorld.BuildHost;
begin
  var Builder := THost.CreateBuilder;
  Host := Builder.Build;
end;

procedure TShutdownWorld.BuildHostRequestingStopAtStartup;
begin
  var Builder := THost.CreateBuilder;
  Builder.Services.AddStartupTask<TStopRequestingTask>;
  Host := Builder.Build;
end;

procedure TShutdownWorld.StopHost;
begin
  Host.Stop;
  Pointer(Host) := nil;
end;

procedure TShutdownWorld.StartHost;
begin
  Host.Start;
end;

destructor TShutdownWorld.Destroy;
begin
  // Stop destruye el host --THost es TNoRefCountObject--, asi que StopHost suelta la referencia
  // sin pasar por _Release: el finalizador del campo lo llamaria sobre memoria ya devuelta.
  if Host <> nil then
    StopHost;
  inherited;
end;

initialization

Feature('''
Feature Pedir la parada de la aplicacion @hosting @lifetime

  Como quien escribe el .dpr de un servicio que se para solo
  Quiero que pedir la parada por codigo valga lo mismo que un Ctrl+C
  Para no descubrir en produccion que StopApplication no paraba nada
''')

.UseWorld<TShutdownWorld>

.Rule('El camino de señal y el de codigo son el mismo')

  // En el linaje hay UNA sola señal de apagado: el manejador de consola llama a StopApplication,
  // y quien espera la parada observa ApplicationStopping. Aqui el manejador invoca
  // TShutdownHook.OnShutdownRequested, asi que ese TProc es el punto donde las dos mitades se
  // unen -- o no se unen.
  .Scenario('Un host arrancado reclama el camino de señal')
    .Given('un host generico construido y todavia sin arrancar',
      procedure(W: TShutdownWorld)
      begin
        W.BuildHost;
      end)
    .When('se arranca',
      procedure(W: TShutdownWorld)
      begin
        W.StartHost;
        W.Report := BoolToStr(Assigned(TShutdownHook.OnShutdownRequested), True);
      end)
    .&Then('hay alguien atendiendo la peticion de parada',
      procedure(W: TShutdownWorld)
      begin
        Expect(W.Report).ToEqual('True');
      end)

  .Scenario('Pedir la parada por ese camino cancela ApplicationStopping')
    .Given('un host arrancado, con su lifetime y su camino de señal en su sitio',
      procedure(W: TShutdownWorld)
      begin
        W.BuildHost;
        W.StartHost;
        W.Lifetime := W.Host.Services.GetService<IHostApplicationLifetime>;
        // Lo que el escenario supone del montaje, afirmado aqui y no dentro del evento: sin
        // esto, el When llamaria a un TProc sin asignar y la violacion de acceso no diria cual
        // era el hueco.
        Expect(Assigned(W.Lifetime)).ToBeTrue;
        Expect(Assigned(TShutdownHook.OnShutdownRequested)).ToBeTrue;
        Expect(W.Lifetime.ApplicationStopping.IsCancellationRequested).ToBeFalse;
      end)
    .When('se pide la parada por ese camino, como lo haria un Ctrl+C',
      procedure(W: TShutdownWorld)
      begin
        // Los parentesis no son adorno: un TProc a pelo es el valor, no la llamada,
        // y da E2014 con la cascada dos lambdas mas abajo.
        TShutdownHook.OnShutdownRequested();
      end)
    .&Then('el token de parada esta cancelado',
      procedure(W: TShutdownWorld)
      begin
        Expect(W.Lifetime.ApplicationStopping.IsCancellationRequested).ToBeTrue;
      end)

.Rule('Una peticion de parada durante el arranque no se pierde')

  .Scenario('Una peticion durante las tareas de arranque para la aplicacion')
    .Given('un host con una tarea de arranque que pide la parada, y ningun host atendiendo la señal',
      procedure(W: TShutdownWorld)
      begin
        W.BuildHostRequestingStopAtStartup;
        Expect(Assigned(TShutdownHook.OnShutdownRequested)).ToBeFalse;
      end)
    .When('el host arranca',
      procedure(W: TShutdownWorld)
      begin
        W.StartHost;
        W.Lifetime := W.Host.Services.GetService<IHostApplicationLifetime>;
        W.Report := BoolToStr(W.Lifetime.ApplicationStopping.IsCancellationRequested, True);
      end)
    .&Then('al terminar de arrancar, ApplicationStopping esta cancelado',
      procedure(W: TShutdownWorld)
      begin
        Expect(W.Report).ToEqual('True');
      end)

  .Scenario('Una peticion pendiente la recibe un solo host')
    .Given('una peticion de parada que ya entrego un host al arrancar',
      procedure(W: TShutdownWorld)
      begin
        W.BuildHostRequestingStopAtStartup;
        W.StartHost;
        W.StopHost;
        Expect(Assigned(TShutdownHook.OnShutdownRequested)).ToBeFalse;
      end)
    .When('arranca despues otro host',
      procedure(W: TShutdownWorld)
      begin
        W.BuildHost;
        W.StartHost;
        W.Lifetime := W.Host.Services.GetService<IHostApplicationLifetime>;
        W.Report := BoolToStr(W.Lifetime.ApplicationStopping.IsCancellationRequested, True);
      end)
    .&Then('su ApplicationStopping no esta cancelado',
      procedure(W: TShutdownWorld)
      begin
        Expect(W.Report).ToEqual('False');
      end);

end.
