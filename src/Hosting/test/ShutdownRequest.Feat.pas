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

  /// Entrega un Ctrl+C al manejador de consola mientras el host arranca, y anota si lo dio por
  /// atendido.
  TInterruptingTask = class(TInterfacedObject, IStartupTask)
  public
    class var Handled: string;
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
    /// El escenario asigno a mano un oyente de la peticion de parada, y el World lo suelta.
    OwnsListener: Boolean;
    procedure BuildHost;
    procedure BuildHostInterruptingAtStartup;
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

{ TInterruptingTask }

procedure TInterruptingTask.Run;
begin
  Handled := BoolToStr(TShutdownHook.HandleConsoleSignal(csInterrupt), True);
end;

{ TShutdownWorld }

procedure TShutdownWorld.BuildHost;
begin
  var Builder := THost.CreateBuilder;
  Host := Builder.Build;
end;

procedure TShutdownWorld.BuildHostInterruptingAtStartup;
begin
  TInterruptingTask.Handled := '';
  var Builder := THost.CreateBuilder;
  Builder.Services.AddStartupTask<TInterruptingTask>;
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
  if OwnsListener then
    TShutdownHook.OnShutdownRequested := nil;
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
      end)

  .Scenario('Una peticion desde codigo sin host queda pendiente')
    .Given('ningun host atendiendo la señal, y una peticion hecha con TShutdownHook.RequestShutdown',
      procedure(W: TShutdownWorld)
      begin
        Expect(Assigned(TShutdownHook.OnShutdownRequested)).ToBeFalse;
        W.BuildHost;
        TShutdownHook.RequestShutdown;
      end)
    .When('arranca un host',
      procedure(W: TShutdownWorld)
      begin
        W.StartHost;
        W.Lifetime := W.Host.Services.GetService<IHostApplicationLifetime>;
        W.Report := BoolToStr(W.Lifetime.ApplicationStopping.IsCancellationRequested, True);
      end)
    .&Then('al terminar de arrancar, su ApplicationStopping esta cancelado',
      procedure(W: TShutdownWorld)
      begin
        Expect(W.Report).ToEqual('True');
      end)

.Rule('Toda interrupcion de consola mientras hay un host pide la parada')

  .Scenario('Una interrupcion durante el arranque')
    .Given('un host con una tarea de arranque que entrega un Ctrl+C al manejador de consola',
      procedure(W: TShutdownWorld)
      begin
        W.BuildHostInterruptingAtStartup;
      end)
    .When('se arranca',
      procedure(W: TShutdownWorld)
      begin
        W.StartHost;
        W.Lifetime := W.Host.Services.GetService<IHostApplicationLifetime>;
      end)
    .&Then('el manejador lo dio por atendido',
      procedure(W: TShutdownWorld)
      begin
        Expect(TInterruptingTask.Handled).ToEqual('True');
      end)
    .&And('al terminar de arrancar, ApplicationStopping esta cancelado',
      procedure(W: TShutdownWorld)
      begin
        Expect(BoolToStr(W.Lifetime.ApplicationStopping.IsCancellationRequested, True)).ToEqual('True');
      end)

  .Scenario('Ctrl+Break pide la parada')
    .Given('un host arrancado',
      procedure(W: TShutdownWorld)
      begin
        W.BuildHost;
        W.StartHost;
        W.Lifetime := W.Host.Services.GetService<IHostApplicationLifetime>;
      end)
    .When('el manejador de consola recibe un Ctrl+Break',
      procedure(W: TShutdownWorld)
      begin
        W.Report := BoolToStr(TShutdownHook.HandleConsoleSignal(csBreak), True);
      end)
    .&Then('la da por atendida',
      procedure(W: TShutdownWorld)
      begin
        Expect(W.Report).ToEqual('True');
      end)
    .&And('ApplicationStopping esta cancelado',
      procedure(W: TShutdownWorld)
      begin
        Expect(BoolToStr(W.Lifetime.ApplicationStopping.IsCancellationRequested, True)).ToEqual('True');
      end)

  .Scenario('Un segundo Ctrl+C tambien se atiende')
    .Given('un host arrancado que ya recibio un Ctrl+C',
      procedure(W: TShutdownWorld)
      begin
        W.BuildHost;
        W.StartHost;
        TShutdownHook.HandleConsoleSignal(csInterrupt);
      end)
    .When('el manejador de consola recibe otro Ctrl+C',
      procedure(W: TShutdownWorld)
      begin
        W.Report := BoolToStr(TShutdownHook.HandleConsoleSignal(csInterrupt), True);
      end)
    .&Then('tambien lo da por atendido',
      procedure(W: TShutdownWorld)
      begin
        Expect(W.Report).ToEqual('True');
      end)

.Rule('Sin un host que atienda la señal, la interrupcion se deja al sistema')

  .Scenario('Un Ctrl+C sin host')
    .Given('ningun host atendiendo la señal',
      procedure(W: TShutdownWorld)
      begin
        Expect(Assigned(TShutdownHook.OnShutdownRequested)).ToBeFalse;
      end)
    .When('el manejador de consola recibe un Ctrl+C',
      procedure(W: TShutdownWorld)
      begin
        W.Report := 'atendida=' + BoolToStr(TShutdownHook.HandleConsoleSignal(csInterrupt), True);
      end)
    .&Then('no lo da por atendido, y no queda ninguna peticion de parada pendiente',
      procedure(W: TShutdownWorld)
      begin
        // Una peticion pendiente solo se ve entregandola: el siguiente oyente que se asigna la recibe.
        var Delivered := False;
        TShutdownHook.OnShutdownRequested :=
          procedure
          begin
            Delivered := True;
          end;
        TShutdownHook.OnShutdownRequested := nil;
        Expect(W.Report + ' pendiente=' + BoolToStr(Delivered, True)).ToEqual('atendida=False pendiente=False');
      end)

.Rule('El cierre de la consola y el de sesion se dejan al sistema')

  .Scenario('Cerrar la consola con un host arrancado')
    .Given('un host arrancado',
      procedure(W: TShutdownWorld)
      begin
        W.BuildHost;
        W.StartHost;
        W.Lifetime := W.Host.Services.GetService<IHostApplicationLifetime>;
      end)
    .When('el manejador de consola recibe el cierre de la consola',
      procedure(W: TShutdownWorld)
      begin
        W.Report := BoolToStr(TShutdownHook.HandleConsoleSignal(csClose), True);
      end)
    .&Then('no lo da por atendido, y ApplicationStopping no esta cancelado',
      procedure(W: TShutdownWorld)
      begin
        Expect(W.Report + ' stopping=' + BoolToStr(W.Lifetime.ApplicationStopping.IsCancellationRequested, True))
          .ToEqual('False stopping=False');
      end)

.Rule('Lo que falla al atender una señal no sale del manejador de consola')

  .Scenario('Un oyente que lanza')
    .Given('un oyente de la peticion de parada asignado a mano, que lanza',
      procedure(W: TShutdownWorld)
      begin
        W.OwnsListener := True;
        TShutdownHook.OnShutdownRequested :=
          procedure
          begin
            raise Exception.Create('el oyente fallo');
          end;
      end)
    .When('el manejador de consola recibe un Ctrl+C',
      procedure(W: TShutdownWorld)
      begin
        try
          W.Report := 'atendida=' + BoolToStr(TShutdownHook.HandleConsoleSignal(csInterrupt), True);
        except
          on E: Exception do
            W.Report := E.ClassName + ': ' + E.Message;
        end;
      end)
    .&Then('el manejador vuelve sin lanzar y da la señal por atendida',
      procedure(W: TShutdownWorld)
      begin
        Expect(W.Report).ToEqual('atendida=True');
      end);

end.
