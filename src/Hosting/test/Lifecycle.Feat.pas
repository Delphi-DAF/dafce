unit Lifecycle.Feat;

interface

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.Threading,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Hosting,
  Daf.Extensions.Logging,
  Daf.Hosting,
  StartupTasks.Feat;

type
  TLifecycleWorld = class
  public
    Trace: string;
  end;

  /// Un host sin proveedor de logging, asi que lo que informa sale por el canal de arranque; el World
  /// aparta los destinos del canal y pone uno que lo recoge en Reported.
  TLifetimeFailureWorld = class
  strict private
    FSavedSinks: TArray<IBootstrapSink>;
  public
    Host: IHost;
    Lifetime: IHostApplicationLifetime;
    Reported: string;
    Ran: string;
    Outcome: string;
    constructor Create;
    destructor Destroy; override;
    procedure BuildHost(WithService: Boolean);
    procedure Attempt(const Act: TProc);
    /// Stop libera el host tambien cuando lanza, asi que la referencia se suelta sin pasar por
    /// _Release: el finalizador del campo lo llamaria sobre memoria ya devuelta.
    procedure Release(const Act: TProc);
  end;

  TCollectingSink = class(TInterfacedObject, IBootstrapSink)
  strict private
    FWorld: TLifetimeFailureWorld;
  public
    constructor Create(World: TLifetimeFailureWorld);
    function IsAvailable: Boolean;
    procedure Write(const Line: string);
  end;

{ TCollectingSink }

constructor TCollectingSink.Create(World: TLifetimeFailureWorld);
begin
  inherited Create;
  FWorld := World;
end;

function TCollectingSink.IsAvailable: Boolean;
begin
  Result := True;
end;

procedure TCollectingSink.Write(const Line: string);
begin
  FWorld.Reported := FWorld.Reported + Line + sLineBreak;
end;

{ TLifetimeFailureWorld }

constructor TLifetimeFailureWorld.Create;
begin
  inherited Create;
  FSavedSinks := BootstrapLog.Sinks;
  BootstrapLog.ClearSinks;
  BootstrapLog.AddSink(TCollectingSink.Create(Self));
end;

destructor TLifetimeFailureWorld.Destroy;
begin
  if Host <> nil then
    Release(
      procedure
      begin
        Host.Stop;
      end);
  BootstrapLog.ClearSinks;
  for var Sink in FSavedSinks do
    BootstrapLog.AddSink(Sink);
  inherited;
end;

procedure TLifetimeFailureWorld.BuildHost(WithService: Boolean);
begin
  TTrace.Reset;
  var Builder := THostBuilder.Create;
  var Owner: IHostApplicationBuilder := Builder;
  if WithService then
    Builder.Services.AddHostedService<TFirstService>;
  Host := Builder.Build;
  Lifetime := Host.Services.GetRequiredService<IHostApplicationLifetime>;
end;

procedure TLifetimeFailureWorld.Attempt(const Act: TProc);
begin
  Outcome := '<sin ejecutar>';
  try
    Act();
    Outcome := 'no exception';
  except
    on E: Exception do
      Outcome := E.ClassName + ': ' + E.Message;
  end;
end;

procedure TLifetimeFailureWorld.Release(const Act: TProc);
begin
  Lifetime := nil;
  Attempt(Act);
  Pointer(Host) := nil;
end;

initialization

Feature('''
Feature A host stops in the reverse order it started @hosting @lifetime

  As a developer whose service releases what another one is still using
  I want the hosted services stopped in the reverse order of their registration
  So that stopping never leaves a running service without what it depends on
''')

.UseWorld<TLifecycleWorld>

.Rule('What starts first stops last')

  .Scenario('Two hosted services leave a trace of their start and of their stop')
    .When('a host with two hosted services is started and then stopped',
      procedure(W: TLifecycleWorld)
      begin
        TTrace.Reset;
        var Builder := THostBuilder.Create;
        var Owner: IHostApplicationBuilder := Builder;
        Builder.Services.AddHostedService<TFirstService>;
        Builder.Services.AddHostedService<TSecondService>;
        var Host := Builder.Build;
        Host.Start;
        Host.Stop;
        W.Trace := TTrace.Steps;
      end)
    .&Then('stopping mirrors starting, which is the order the container releases in',
      procedure(W: TLifecycleWorld)
      begin
        // The whole statement in one value on purpose: asserting the halves apart would let a
        // stop order that is merely different, rather than reversed, pass.
        Expect(W.Trace).ToEqual('up:1,up:2,down:2,down:1');
      end);

Feature('''
Feature A lifetime callback that fails is reported @hosting @lifetime

  As a developer who hangs work on the application lifetime
  I want a callback that fails reported, without interrupting the host
  So that one listener cannot stop the start or the shutdown of the rest
''')

.UseWorld<TLifetimeFailureWorld>

.Rule('A failing lifetime callback is reported and does not interrupt the host')

  .Scenario('A stopping callback that fails does not interrupt the stop')
    .Given('a started host with a hosted service and two stopping callbacks, the one that runs first raising',
      procedure(W: TLifetimeFailureWorld)
      begin
        W.BuildHost(True);
        W.Host.Start;
        // Run last registered first: the one that raises is registered second so that it runs first.
        W.Lifetime.ApplicationStopping.Register(
          procedure
          begin
            W.Ran := W.Ran + 'other;';
          end);
        W.Lifetime.ApplicationStopping.Register(
          procedure
          begin
            raise Exception.Create('stopping callback failed');
          end);
      end)
    .When('it is stopped',
      procedure(W: TLifetimeFailureWorld)
      begin
        W.Release(
          procedure
          begin
            W.Host.Stop;
          end);
      end)
    .&Then('the other callback ran and the service stopped',
      procedure(W: TLifetimeFailureWorld)
      begin
        Expect(W.Ran + ' | ' + TTrace.Steps).ToEqual('other; | up:1,down:1');
      end)
    .&And('the stop does not raise, and the callback failure was reported',
      procedure(W: TLifetimeFailureWorld)
      begin
        Expect(W.Outcome).ToEqual('no exception');
        Expect(W.Reported).ToContain('stopping callback failed');
      end)

  .Scenario('Requesting the stop with a failing callback does not raise')
    .Given('a started host with a stopping callback that raises',
      procedure(W: TLifetimeFailureWorld)
      begin
        W.BuildHost(False);
        W.Host.Start;
        W.Lifetime.ApplicationStopping.Register(
          procedure
          begin
            raise Exception.Create('stopping callback failed');
          end);
        Expect(Assigned(TShutdownHook.OnShutdownRequested)).ToBeTrue;
      end)
    .When('the stop is requested the way the console signal does',
      procedure(W: TLifetimeFailureWorld)
      begin
        W.Attempt(
          procedure
          begin
            TShutdownHook.OnShutdownRequested();
          end);
      end)
    .&Then('the request returns without raising, and ApplicationStopping is cancelled',
      procedure(W: TLifetimeFailureWorld)
      begin
        Expect(W.Outcome + ' | stopping=' + BoolToStr(W.Lifetime.ApplicationStopping.IsCancellationRequested, True))
          .ToEqual('no exception | stopping=True');
      end)
    .&And('the callback failure was reported',
      procedure(W: TLifetimeFailureWorld)
      begin
        Expect(W.Reported).ToContain('stopping callback failed');
      end)

  .Scenario('A started callback that fails does not abort the start')
    .Given('a host with a started callback that raises',
      procedure(W: TLifetimeFailureWorld)
      begin
        W.BuildHost(False);
        W.Lifetime.ApplicationStarted.Register(
          procedure
          begin
            raise Exception.Create('started callback failed');
          end);
      end)
    .When('it is started',
      procedure(W: TLifetimeFailureWorld)
      begin
        W.Attempt(
          procedure
          begin
            W.Host.Start;
          end);
      end)
    .&Then('the start does not raise, and ApplicationStarted is cancelled',
      procedure(W: TLifetimeFailureWorld)
      begin
        Expect(W.Outcome + ' | started=' + BoolToStr(W.Lifetime.ApplicationStarted.IsCancellationRequested, True))
          .ToEqual('no exception | started=True');
      end)
    .&And('the callback failure was reported',
      procedure(W: TLifetimeFailureWorld)
      begin
        Expect(W.Reported).ToContain('started callback failed');
      end);

end.
