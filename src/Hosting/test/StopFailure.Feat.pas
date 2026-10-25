unit StopFailure.Feat;

interface

uses
  System.SysUtils,
  Daf.Extensions.Hosting;

type
  EStopRefused = class(Exception);
  EStartRefused = class(Exception);

  // En la seccion interface: el contenedor construye los servicios alojados por RTTI, y un tipo
  // declarado solo en implementation no esta ahi para encontrarlo.

  /// Cuenta sus instancias vivas y deja traza de su arranque y de su parada, asi que el escenario ve
  /// que se paro y que murio sin esperar al informe de fugas del runner. Falla al arrancar o al
  /// pararse si su nombre esta entre los que se le piden al montar el host.
  TStoppingService = class(TInterfacedObject, IHostedService)
  protected
    class function Tag: string; virtual; abstract;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Start;
    procedure Stop;
  end;

  /// Se registra el primero, asi que arranca el primero y se para el ultimo.
  TFirstStopping = class(TStoppingService)
  protected
    class function Tag: string; override;
  end;

  TSecondStopping = class(TStoppingService)
  protected
    class function Tag: string; override;
  end;

  TThirdStopping = class(TStoppingService)
  protected
    class function Tag: string; override;
  end;

implementation

uses
  System.StrUtils,
  System.Threading,
  Daf.MiniSpec,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Logging,
  Daf.Hosting,
  StartupTasks.Feat;

var
  AliveCount: Integer;
  StartTrace: string;
  StopTrace: string;
  RefusingStart: TArray<string>;
  RefusingStop: TArray<string>;

{ TStoppingService }

procedure TStoppingService.AfterConstruction;
begin
  inherited;
  Inc(AliveCount);
end;

procedure TStoppingService.BeforeDestruction;
begin
  Dec(AliveCount);
  inherited;
end;

procedure TStoppingService.Start;
begin
  if MatchStr(Tag, RefusingStart) then
    raise EStartRefused.CreateFmt('%s no pudo arrancar', [Tag]);
  if StartTrace <> '' then
    StartTrace := StartTrace + ',';
  StartTrace := StartTrace + Tag;
end;

procedure TStoppingService.Stop;
begin
  if StopTrace <> '' then
    StopTrace := StopTrace + ',';
  StopTrace := StopTrace + Tag;
  if MatchStr(Tag, RefusingStop) then
    raise EStopRefused.CreateFmt('%s no pudo pararse', [Tag]);
end;

class function TFirstStopping.Tag: string;
begin
  Result := 'primero';
end;

class function TSecondStopping.Tag: string;
begin
  Result := 'segundo';
end;

class function TThirdStopping.Tag: string;
begin
  Result := 'tercero';
end;

/// La clase y el mensaje, y los de dentro si es una agregada: en un solo valor, una excepcion
/// envuelta y la original no se confunden.
function Describe(const E: Exception): string;
begin
  Result := E.ClassName + ': ' + E.Message;
  if E is EAggregateException then
    for var Idx := 0 to EAggregateException(E).Count - 1 do
      Result := Result + ' | ' + Describe(EAggregateException(E)[Idx]);
end;

type
  TStopWorld = class
  strict private
    // Los fallos de la parada se informan por el canal de arranque, y eso sale por la salida de
    // error del arnes. Es el comportamiento que se quiere; aqui solo se aparta para que no se
    // confunda con la salida de las specs.
    FSavedSinks: TArray<IBootstrapSink>;
  public
    Host: IHost;
    Lifetime: IHostApplicationLifetime;
    AliveBefore: Integer;
    Outcome: string;
    constructor Create;
    destructor Destroy; override;
    procedure BuildHost(const Configure: TProc<IServiceCollection>);
    procedure StartHost(const Refusing: TArray<string>);
    /// Stop y Free liberan el host tambien cuando lanzan, asi que la referencia se suelta sin pasar
    /// por _Release: el finalizador del campo lo llamaria sobre memoria ya devuelta.
    procedure Release(const Act: TProc);
    function Alive: Integer;
  end;

constructor TStopWorld.Create;
begin
  inherited Create;
  RefusingStart := nil;
  RefusingStop := nil;
  FSavedSinks := BootstrapLog.Sinks;
  BootstrapLog.ClearSinks;
end;

destructor TStopWorld.Destroy;
begin
  // Solo queda un host aqui si el escenario no llego a soltarlo.
  if Host <> nil then
    Release(
      procedure
      begin
        Host.Stop;
      end);
  Lifetime := nil;
  BootstrapLog.ClearSinks;
  for var Sink in FSavedSinks do
    BootstrapLog.AddSink(Sink);
  inherited;
end;

procedure TStopWorld.BuildHost(const Configure: TProc<IServiceCollection>);
begin
  StartTrace := '';
  StopTrace := '';
  AliveBefore := AliveCount;
  var Builder := THost.CreateBuilder;
  Configure(Builder.Services);
  Host := Builder.Build;
  Lifetime := Host.Services.GetRequiredService<IHostApplicationLifetime>;
end;

procedure TStopWorld.StartHost(const Refusing: TArray<string>);
begin
  RefusingStop := Refusing;
  BuildHost(
    procedure(Services: IServiceCollection)
    begin
      Services.AddHostedService<TFirstStopping>;
      Services.AddHostedService<TSecondStopping>;
    end);
  Host.Start;
end;

procedure TStopWorld.Release(const Act: TProc);
begin
  Outcome := '<nada>';
  try
    Act();
  except
    on E: Exception do
      Outcome := Describe(E);
  end;
  Pointer(Host) := nil;
end;

function TStopWorld.Alive: Integer;
begin
  Result := AliveCount - AliveBefore;
end;

initialization

Feature('''
Feature Parar el host lo para entero aunque algo falle @hosting @lifetime @shutdown

  Como quien escribe un servicio alojado que puede fallar al pararse
  Quiero que su fallo no deje sin parar a los demas ni el host a medias
  Para saber que fallo sin que el proceso se quede con el contenedor entero
''')

.UseWorld<TStopWorld>

.Rule('La parada llega hasta el final aunque algo falle')

  .Scenario('Ningun paso falla')
    .Given('un host con dos servicios alojados que paran bien',
      procedure(W: TStopWorld)
      begin
        W.StartHost([]);
      end)
    .When('se para',
      procedure(W: TStopWorld)
      begin
        W.Release(
          procedure
          begin
            W.Host.Stop;
          end);
      end)
    .&Then('los dos se paran, en el orden inverso al de arranque',
      procedure(W: TStopWorld)
      begin
        Expect(StopTrace).ToEqual('segundo,primero');
      end)
    .&And('no queda vivo ninguno de los dos, y la parada no lanza nada',
      procedure(W: TStopWorld)
      begin
        Expect(Format('vivos=%d %s', [W.Alive, W.Outcome])).ToEqual('vivos=0 <nada>');
      end)

  .Scenario('Un servicio falla al parar')
    .Given('un host con dos servicios alojados, y el que para primero falla al parar',
      procedure(W: TStopWorld)
      begin
        W.StartHost(['segundo']);
      end)
    .When('se para',
      procedure(W: TStopWorld)
      begin
        W.Release(
          procedure
          begin
            W.Host.Stop;
          end);
      end)
    .&Then('el otro servicio tambien se para',
      procedure(W: TStopWorld)
      begin
        Expect(StopTrace).ToEqual('segundo,primero');
      end)
    .&And('no queda vivo ninguno de los dos',
      procedure(W: TStopWorld)
      begin
        Expect(W.Alive).ToEqual(0);
      end)
    .&And('la parada lanza la excepcion de ese servicio, la misma y no una que la envuelva',
      procedure(W: TStopWorld)
      begin
        Expect(W.Outcome).ToEqual('EStopRefused: segundo no pudo pararse');
      end)

  .Scenario('Dos servicios fallan al parar')
    .Given('un host con dos servicios alojados que fallan los dos al parar',
      procedure(W: TStopWorld)
      begin
        W.StartHost(['primero', 'segundo']);
      end)
    .When('se para',
      procedure(W: TStopWorld)
      begin
        W.Release(
          procedure
          begin
            W.Host.Stop;
          end);
      end)
    .&Then('no queda vivo ninguno de los dos',
      procedure(W: TStopWorld)
      begin
        Expect(W.Alive).ToEqual(0);
      end)
    .&And('la parada lanza una excepcion agregada con las dos dentro, cuyo mensaje dice que fallaron dos',
      procedure(W: TStopWorld)
      begin
        Expect(W.Outcome).ToEqual('EAggregateException: the application could not stop cleanly: ' +
          '2 steps failed | EStopRefused: segundo no pudo pararse | EStopRefused: primero no pudo pararse');
      end)

.Rule('Liberar un host sin pararlo lo destruye una vez')

  .Scenario('Liberar un host arrancado sin pararlo')
    .Given('un host arrancado con dos servicios alojados',
      procedure(W: TStopWorld)
      begin
        W.StartHost([]);
      end)
    .When('se libera sin haberlo parado',
      procedure(W: TStopWorld)
      begin
        W.Release(
          procedure
          begin
            (W.Host as TObject).Free;
          end);
      end)
    .&Then('los dos servicios se paran',
      procedure(W: TStopWorld)
      begin
        Expect(StopTrace).ToEqual('segundo,primero');
      end)
    .&And('la liberacion termina sin error y no queda vivo ninguno de los dos',
      procedure(W: TStopWorld)
      begin
        Expect(Format('vivos=%d %s', [W.Alive, W.Outcome])).ToEqual('vivos=0 <nada>');
      end)

.Rule('Parar un host que empezo a arrancar para todos sus servicios alojados')

  .Scenario('Un arranque que falla a medias')
    .Given('un host con tres servicios alojados, y el segundo lanza al arrancar',
      procedure(W: TStopWorld)
      begin
        RefusingStart := ['segundo'];
        W.BuildHost(
          procedure(Services: IServiceCollection)
          begin
            Services.AddHostedService<TFirstStopping>;
            Services.AddHostedService<TSecondStopping>;
            Services.AddHostedService<TThirdStopping>;
          end);
      end)
    .When('se arranca, y despues se para',
      procedure(W: TStopWorld)
      begin
        try
          W.Host.Start;
        except
          on EStartRefused do ;
        end;
        W.Release(
          procedure
          begin
            W.Host.Stop;
          end);
      end)
    .&Then('los tres reciben Stop, en el orden inverso al de registro',
      procedure(W: TStopWorld)
      begin
        Expect(Format('arrancados=<%s> parados=<%s>', [StartTrace, StopTrace]))
          .ToEqual('arrancados=<primero> parados=<tercero,segundo,primero>');
      end)
    .&And('la parada no lanza',
      procedure(W: TStopWorld)
      begin
        Expect(W.Outcome).ToEqual('<nada>');
      end)

  .Scenario('Una tarea de arranque que falla')
    .Given('un host con una tarea de arranque que lanza y un servicio alojado',
      procedure(W: TStopWorld)
      begin
        W.BuildHost(
          procedure(Services: IServiceCollection)
          begin
            Services.AddStartupTask<TFailingTask>;
            Services.AddHostedService<TFirstStopping>;
          end);
      end)
    .When('se arranca, y despues se para',
      procedure(W: TStopWorld)
      begin
        try
          W.Host.Start;
        except
          on Exception do ;
        end;
        W.Release(
          procedure
          begin
            W.Host.Stop;
          end);
      end)
    .&Then('el servicio alojado recibe Stop aunque no arranco',
      procedure(W: TStopWorld)
      begin
        Expect(Format('arrancados=<%s> parados=<%s>', [StartTrace, StopTrace]))
          .ToEqual('arrancados=<> parados=<primero>');
      end)

.Rule('Parar un host que no llego a arrancar no para sus servicios alojados')

  .Scenario('Liberar un host construido y nunca arrancado')
    .Given('un host construido con un servicio alojado que anota su Stop, sin arrancar',
      procedure(W: TStopWorld)
      begin
        W.BuildHost(
          procedure(Services: IServiceCollection)
          begin
            Services.AddHostedService<TFirstStopping>;
          end);
      end)
    .When('se libera',
      procedure(W: TStopWorld)
      begin
        W.Release(
          procedure
          begin
            (W.Host as TObject).Free;
          end);
      end)
    .&Then('el servicio alojado no recibio Stop',
      procedure(W: TStopWorld)
      begin
        Expect(Format('parados=<%s> %s', [StopTrace, W.Outcome])).ToEqual('parados=<> <nada>');
      end)
    .&And('ApplicationStopping y ApplicationStopped estan cancelados',
      procedure(W: TStopWorld)
      begin
        Expect(Format('stopping=%s stopped=%s', [
          BoolToStr(W.Lifetime.ApplicationStopping.IsCancellationRequested, True),
          BoolToStr(W.Lifetime.ApplicationStopped.IsCancellationRequested, True)]))
          .ToEqual('stopping=True stopped=True');
      end);

end.
