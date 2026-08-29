unit StartupTasks.Feat;

interface

uses
  System.SysUtils,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Hosting,
  DAF.Extensions.Logging;

type
  /// Deja constancia del orden en que ocurren las cosas al arrancar, que es lo que estas
  /// specs miden: una tarea de arranque prepara estado del que un servicio alojado depende.
  TTrace = class
  public
    class var Steps: string;
    class procedure Reset;
    class procedure Add(const Step: string);
    class function Count(const Step: string): Integer;
  end;

  ISeed = interface(IInvokable)
    ['{DB7C5BC0-BCD5-46C7-A328-B975241916D7}']
    function Value: string;
  end;

  TSeed = class(TInterfacedObject, ISeed)
  public
    function Value: string;
  end;

  /// No tiene nada que parar, asi que no finge ser un IHostedService con un Stop vacio.
  TSeedingTask = class(TInterfacedObject, IStartupTask)
  public
    procedure Run;
  end;

  /// Recibe su dependencia por constructor, como cualquier otro servicio.
  TDependentTask = class(TInterfacedObject, IStartupTask)
  strict private
    FSeed: ISeed;
  public
    constructor Create(const Seed: ISeed);
    procedure Run;
  end;

  TFailingTask = class(TInterfacedObject, IStartupTask)
  public
    procedure Run;
  end;

  TConsumerService = class(TInterfacedObject, IHostedService)
  public
    procedure Start;
    procedure Stop;
  end;

  TStartupWorld = class
  strict private
    // Hay escenarios que fallan a proposito, y ese fallo sale por la salida de error del arnes.
    // Es el comportamiento que se quiere; aqui solo se aparta para que no se confunda con la
    // salida de las specs.
    FSavedSinks: TArray<IBootstrapSink>;
  public
    Host: IHost;
    Failure: string;
    constructor Create;
    destructor Destroy; override;
    procedure BuildHost(const Configure: TProc<IServiceCollection>);
    procedure StartHost;
    function StartedNotified: Boolean;
  end;

/// Un paquete que trae su propia inicializacion: quien lo usa llama a esta funcion, y no
/// tiene por que saber que detras hay una tarea de arranque ni como se llama.
function AddGreeting(const Services: IServiceCollection): IServiceCollection;

implementation

uses
  System.StrUtils,
  Daf.MiniSpec,
  Daf.Hosting;

type
  TGreetingTask = class(TInterfacedObject, IStartupTask)
  public
    procedure Run;
  end;

function AddGreeting(const Services: IServiceCollection): IServiceCollection;
begin
  Result := Services;
  Services.AddStartupTask<TGreetingTask>;
end;

{ TTrace }

class procedure TTrace.Reset;
begin
  Steps := '';
end;

class procedure TTrace.Add(const Step: string);
begin
  if Steps <> '' then
    Steps := Steps + ',';
  Steps := Steps + Step;
end;

class function TTrace.Count(const Step: string): Integer;
begin
  Result := 0;
  for var Each in SplitString(Steps, ',') do
    if Each = Step then
      Inc(Result);
end;

{ TSeed }

function TSeed.Value: string;
begin
  Result := 'seed';
end;

{ TSeedingTask }

procedure TSeedingTask.Run;
begin
  TTrace.Add('task');
end;

{ TDependentTask }

constructor TDependentTask.Create(const Seed: ISeed);
begin
  inherited Create;
  FSeed := Seed;
end;

procedure TDependentTask.Run;
begin
  TTrace.Add(FSeed.Value);
end;

{ TFailingTask }

procedure TFailingTask.Run;
begin
  TTrace.Add('failing');
  raise Exception.Create('startup task refused to initialize');
end;

{ TConsumerService }

procedure TConsumerService.Start;
begin
  TTrace.Add('service');
end;

procedure TConsumerService.Stop;
begin
end;

{ TGreetingTask }

procedure TGreetingTask.Run;
begin
  TTrace.Add('greeting');
end;

{ TStartupWorld }

constructor TStartupWorld.Create;
begin
  inherited Create;
  FSavedSinks := BootstrapLog.Sinks;
  BootstrapLog.ClearSinks;
end;

destructor TStartupWorld.Destroy;
begin
  if Host <> nil then
    Host.Stop;
  BootstrapLog.ClearSinks;
  for var Sink in FSavedSinks do
    BootstrapLog.AddSink(Sink);
  inherited;
end;

procedure TStartupWorld.BuildHost(const Configure: TProc<IServiceCollection>);
begin
  var Builder := THostBuilder.Create;
  var Owner: IHostApplicationBuilder := Builder;
  Configure(Builder.Services);
  Host := Builder.Build;
end;

procedure TStartupWorld.StartHost;
begin
  Failure := '';
  try
    Host.Start;
  except
    on E: Exception do Failure := E.Message;
  end;
end;

function TStartupWorld.StartedNotified: Boolean;
begin
  Result := Host.Services.GetRequiredService<IHostApplicationLifetime>.ApplicationStarted.IsCancellationRequested;
end;

initialization

Feature('''
Feature Startup tasks @hosting @startup

  As an application author
  I want a contract for code that runs once at startup, with the provider already built
  So I do not have to pretend an initializer is a background service with an empty Stop
''')

.UseWorld<TStartupWorld>

.Rule('Startup initialization has a contract of its own')

  .Scenario('A startup task runs once, and has no Stop')
    .When('a startup task is registered and the host starts',
      procedure(W: TStartupWorld)
      begin
        TTrace.Reset;
        W.BuildHost(
          procedure(Services: IServiceCollection)
          begin
            Services.AddStartupTask<TSeedingTask>;
          end);
        W.StartHost;
      end)
    .&Then('its Run ran exactly once',
      procedure(W: TStartupWorld)
      begin
        Expect(W.Failure).ToBeEmpty;
        Expect(TTrace.Count('task')).ToEqual(1);
      end)

  .Scenario('It receives its dependencies through the constructor')
    .When('a startup task declaring dependencies in its constructor is registered',
      procedure(W: TStartupWorld)
      begin
        TTrace.Reset;
        W.BuildHost(
          procedure(Services: IServiceCollection)
          begin
            Services.AddSingleton<ISeed, TSeed>;
            Services.AddStartupTask<TDependentTask>;
          end);
        W.StartHost;
      end)
    .&Then('they are resolved from the provider like for any other service',
      procedure(W: TStartupWorld)
      begin
        Expect(W.Failure).ToBeEmpty;
        Expect(TTrace.Steps).ToEqual('seed');
      end)

.Rule('Startup tasks run before hosted services')

  .Scenario('The state is ready when a hosted service starts')
    .Given('a startup task that prepares state and a hosted service that consumes it',
      procedure(W: TStartupWorld)
      begin
        TTrace.Reset;
        W.BuildHost(
          procedure(Services: IServiceCollection)
          begin
            Services.AddHostedService<TConsumerService>;
            Services.AddStartupTask<TSeedingTask>;
          end);
      end)
    .When('the host starts',
      procedure(W: TStartupWorld)
      begin
        W.StartHost;
      end)
    .&Then('the hosted service saw the state already prepared',
      procedure(W: TStartupWorld)
      begin
        Expect(W.Failure).ToBeEmpty;
        Expect(TTrace.Steps).ToEqual('task,service');
      end)

  .Scenario('A failure while initializing aborts the startup')
    .Given('a startup task that raises, and a hosted service registered after it',
      procedure(W: TStartupWorld)
      begin
        TTrace.Reset;
        W.BuildHost(
          procedure(Services: IServiceCollection)
          begin
            Services.AddStartupTask<TFailingTask>;
            Services.AddHostedService<TConsumerService>;
          end);
      end)
    .When('the host starts',
      procedure(W: TStartupWorld)
      begin
        W.StartHost;
      end)
    .&Then('the startup fails and the exception propagates',
      procedure(W: TStartupWorld)
      begin
        Expect(W.Failure).ToContain('refused to initialize');
      end)
    .&And('no hosted service was started',
      procedure(W: TStartupWorld)
      begin
        Expect(TTrace.Count('service')).ToEqual(0);
      end)
    .&And('ApplicationStarted was not notified',
      procedure(W: TStartupWorld)
      begin
        Expect(W.StartedNotified).ToBeFalse;
      end)

.Rule('A package can bring its own initialization')

  .Scenario('The executable does not mention the initialization of the package')
    .When('an executable calls the AddXxx of a package that registers a startup task inside',
      procedure(W: TStartupWorld)
      begin
        TTrace.Reset;
        W.BuildHost(
          procedure(Services: IServiceCollection)
          begin
            AddGreeting(Services);
          end);
        W.StartHost;
      end)
    .&Then('at startup the initialization of the package happens',
      procedure(W: TStartupWorld)
      begin
        Expect(W.Failure).ToBeEmpty;
        Expect(TTrace.Count('greeting')).ToEqual(1);
      end);

end.
