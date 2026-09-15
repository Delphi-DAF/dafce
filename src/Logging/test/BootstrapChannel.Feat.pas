unit BootstrapChannel.Feat;

interface

uses
  Daf.Extensions.Logging;

type
  /// Un destino que se queda con lo que le llega. Es la unica forma de mirar dentro del canal sin
  /// depender de donde escriben de verdad los destinos que trae DAF.
  IRecordingSink = interface(IBootstrapSink)
    ['{16FD9BA9-EF45-4767-91DD-29B14EB94EB0}']
    function Text: string;
    function Count: Integer;
    function Line(const Index: Integer): string;
  end;

  /// Disco lleno, fichero bloqueado, permisos: da igual cual sea el motivo, no puede salir de aqui.
  IThrowingSink = interface(IBootstrapSink)
    ['{2D7D062F-9DB4-4587-80B8-E40F0816AC85}']
    function Attempts: Integer;
  end;

  /// Cada escenario deja el registro de destinos como lo encontro: el canal es global, y uno que se
  /// lleve por delante los destinos por defecto se los quita a los que vengan detras.
  TChannelWorld = class
  strict private
    FSaved: TArray<IBootstrapSink>;
  public
    Recorder: IRecordingSink;
    Second: IRecordingSink;
    Unavailable: IRecordingSink;
    Thrower: IThrowingSink;
    constructor Create;
    destructor Destroy; override;
    /// Los destinos por defecto, tal y como estaban antes de que este escenario tocara nada.
    function DefaultSinks: TArray<IBootstrapSink>;
    procedure OnlySinks(const Sinks: array of IBootstrapSink);
  end;

function NewRecordingSink(const Available: Boolean = True): IRecordingSink;
function NewThrowingSink: IThrowingSink;

implementation

uses
  System.SysUtils,
  Daf.MiniSpec;

type
  TRecordingSink = class(TInterfacedObject, IBootstrapSink, IRecordingSink)
  strict private
    FLines: TArray<string>;
    FAvailable: Boolean;
  public
    constructor Create(const Available: Boolean);
    function IsAvailable: Boolean;
    procedure Write(const ALine: string);
    function Text: string;
    function Count: Integer;
    function Line(const Index: Integer): string;
  end;

  TThrowingSink = class(TInterfacedObject, IBootstrapSink, IThrowingSink)
  strict private
    FAttempts: Integer;
  public
    function IsAvailable: Boolean;
    procedure Write(const ALine: string);
    function Attempts: Integer;
  end;

function NewRecordingSink(const Available: Boolean): IRecordingSink;
begin
  Result := TRecordingSink.Create(Available);
end;

function NewThrowingSink: IThrowingSink;
begin
  Result := TThrowingSink.Create;
end;

{ TRecordingSink }

constructor TRecordingSink.Create(const Available: Boolean);
begin
  inherited Create;
  FAvailable := Available;
end;

function TRecordingSink.IsAvailable: Boolean;
begin
  Result := FAvailable;
end;

procedure TRecordingSink.Write(const ALine: string);
begin
  FLines := FLines + [ALine];
end;

function TRecordingSink.Text: string;
begin
  Result := string.Join(sLineBreak, FLines);
end;

function TRecordingSink.Count: Integer;
begin
  Result := Length(FLines);
end;

function TRecordingSink.Line(const Index: Integer): string;
begin
  if (Index < 0) or (Index > High(FLines)) then
    Exit('');
  Result := FLines[Index];
end;

{ TThrowingSink }

function TThrowingSink.IsAvailable: Boolean;
begin
  Result := True;
end;

procedure TThrowingSink.Write(const ALine: string);
begin
  Inc(FAttempts);
  raise Exception.Create('this sink cannot write today');
end;

function TThrowingSink.Attempts: Integer;
begin
  Result := FAttempts;
end;

{ TChannelWorld }

constructor TChannelWorld.Create;
begin
  inherited Create;
  FSaved := BootstrapLog.Sinks;
  Recorder := NewRecordingSink;
  Second := NewRecordingSink;
  Unavailable := NewRecordingSink(False);
  Thrower := NewThrowingSink;
end;

destructor TChannelWorld.Destroy;
begin
  BootstrapLog.ClearSinks;
  for var Sink in FSaved do
    BootstrapLog.AddSink(Sink);
  inherited;
end;

function TChannelWorld.DefaultSinks: TArray<IBootstrapSink>;
begin
  Result := FSaved;
end;

procedure TChannelWorld.OnlySinks(const Sinks: array of IBootstrapSink);
begin
  BootstrapLog.ClearSinks;
  for var Sink in Sinks do
    BootstrapLog.AddSink(Sink);
end;

initialization

Feature('''
Feature Startup reports reach someone @logging @bootstrap

  As an application author
  I want failures that happen before the service provider exists to reach a destination
  So that a process dying while it reads its configuration does not take the reason with it
''')

.UseWorld<TChannelWorld>

.Rule('The channel is usable before any service exists')

  .Scenario('A failure reported before any provider is built reaches its destination')
    .Given('a channel with a destination registered, and no service provider built',
      procedure(W: TChannelWorld)
      begin
        W.OnlySinks([W.Recorder]);
      end)
    .When('the framework reports a startup failure',
      procedure(W: TChannelWorld)
      begin
        BootstrapLog.LoggerFor('DAF').LogError('cannot parse json in file "appsettings.json"');
      end)
    .&Then('the report names the cause',
      procedure(W: TChannelWorld)
      begin
        Expect(W.Recorder.Count).ToEqual(1);
        Expect(W.Recorder.Text).ToContain('appsettings.json');
      end)

  .Scenario('The channel is there without building anything to use it')
    .&Then('the startup logger exists from the moment the process starts',
      procedure(W: TChannelWorld)
      begin
        Expect(Assigned(BootstrapLog.LoggerFor('DAF'))).ToBeTrue;
        Expect(Length(W.DefaultSinks)).ToBeGreaterThan(0);
      end)

.Rule('A destination that fails takes down neither the startup nor the others')

  .Scenario('A destination that raises does not become a second failure')
    .Given('a destination that raises when written to, registered before a working one',
      procedure(W: TChannelWorld)
      begin
        W.OnlySinks([W.Thrower, W.Recorder]);
      end)
    .When('a startup failure is reported',
      procedure(W: TChannelWorld)
      begin
        ExpectException(
          procedure
          begin
            BootstrapLog.LoggerFor('DAF').LogError('configuration is unreadable');
          end).ToBeNone;
      end)
    .&Then('the destination that could write got the report anyway',
      procedure(W: TChannelWorld)
      begin
        Expect(W.Thrower.Attempts).ToEqual(1);
        Expect(W.Recorder.Text).ToContain('configuration is unreadable');
      end)
    .&And('the one that failed is not tried again',
      procedure(W: TChannelWorld)
      begin
        BootstrapLog.LoggerFor('DAF').LogError('and again');
        Expect(W.Thrower.Attempts).ToEqual(1);
        Expect(W.Recorder.Count).ToEqual(2);
      end)

.Rule('The application can report its own startup')

  .Scenario('A report from the application is not discarded')
    .Given('a channel with a destination registered',
      procedure(W: TChannelWorld)
      begin
        W.OnlySinks([W.Recorder]);
      end)
    .When('the application reports a failure of its own startup, under its own category',
      procedure(W: TChannelWorld)
      begin
        BootstrapLog.LoggerFor('Acme.Payroll').LogError('data folder is not writable');
      end)
    .&Then('the report comes out of the channel',
      procedure(W: TChannelWorld)
      begin
        Expect(W.Recorder.Count).ToEqual(1);
        Expect(W.Recorder.Text).ToContain('data folder is not writable');
      end)
    .&And('it keeps the category of the application',
      procedure(W: TChannelWorld)
      begin
        Expect(W.Recorder.Text).ToContain('] Acme.Payroll |');
      end)

  .Scenario('Framework and application are told apart')
    .Given('a channel with a destination registered',
      procedure(W: TChannelWorld)
      begin
        W.OnlySinks([W.Recorder]);
      end)
    .When('both report during the same startup',
      procedure(W: TChannelWorld)
      begin
        BootstrapLog.LoggerFor('DAF').LogError('host could not be built');
        BootstrapLog.LoggerFor('Acme.Payroll').LogError('data folder is not writable');
      end)
    .&Then('each report says who emitted it',
      procedure(W: TChannelWorld)
      begin
        Expect(W.Recorder.Count).ToEqual(2);
        Expect(W.Recorder.Line(0)).ToContain('] DAF |');
        Expect(W.Recorder.Line(0)).ToContain('host could not be built');
        Expect(W.Recorder.Line(1)).ToContain('] Acme.Payroll |');
        Expect(W.Recorder.Line(1)).ToContain('data folder is not writable');
      end)

  .Scenario('The threshold keeps startup from turning into a debug channel')
    .Given('a channel with a destination registered',
      procedure(W: TChannelWorld)
      begin
        W.OnlySinks([W.Recorder]);
      end)
    .When('reports are made at every level',
      procedure(W: TChannelWorld)
      begin
        BootstrapLog.LoggerFor('DAF').LogTrace('a trace line');
        BootstrapLog.LoggerFor('DAF').LogDebug('a debug line');
        BootstrapLog.LoggerFor('DAF').LogInformation('an information line');
        BootstrapLog.LoggerFor('DAF').LogWarning('a warning line');
        BootstrapLog.LoggerFor('DAF').LogError('an error line');
        BootstrapLog.LoggerFor('DAF').LogCritical('a critical line');
      end)
    .&Then('only Warning and above come out, under the category of whoever reported',
      procedure(W: TChannelWorld)
      begin
        Expect(W.Recorder.Count).ToEqual(3);
        Expect(W.Recorder.Text).ToContain('a warning line');
        Expect(W.Recorder.Text).ToContain('an error line');
        Expect(W.Recorder.Text).ToContain('a critical line');
        Expect(W.Recorder.Text).ToContain('] DAF |');
      end)
    .&And('nothing below Warning is reported',
      procedure(W: TChannelWorld)
      begin
        Expect(W.Recorder.Text.Contains('a trace line')).ToBeFalse;
        Expect(W.Recorder.Text.Contains('a debug line')).ToBeFalse;
        Expect(W.Recorder.Text.Contains('an information line')).ToBeFalse;
      end)

.Rule('A destination that does not write does not pretend it did')

  .Scenario('A destination not implemented on this platform declares itself unavailable')
    .Given('a destination that is not available, registered next to one that is',
      procedure(W: TChannelWorld)
      begin
        W.OnlySinks([W.Unavailable, W.Recorder]);
      end)
    .When('a startup failure is reported',
      procedure(W: TChannelWorld)
      begin
        BootstrapLog.LoggerFor('DAF').LogError('host could not be built');
      end)
    .&Then('the unavailable destination did not take the report',
      procedure(W: TChannelWorld)
      begin
        Expect(W.Unavailable.Count).ToEqual(0);
      end)
    .&And('the ones that are available keep reporting',
      procedure(W: TChannelWorld)
      begin
        Expect(W.Recorder.Text).ToContain('host could not be built');
      end)

  .Scenario('Having no destination available at all can be seen')
    .Given('a channel where no destination is available',
      procedure(W: TChannelWorld)
      begin
        W.OnlySinks([W.Unavailable]);
      end)
    .&Then('the channel says so instead of looking like it reported',
      procedure(W: TChannelWorld)
      begin
        Expect(BootstrapLog.HasAvailableSink).ToBeFalse;
        Expect(Length(BootstrapLog.AvailableSinks)).ToEqual(0);
      end)
    .&And('with one available destination it says so too',
      procedure(W: TChannelWorld)
      begin
        BootstrapLog.AddSink(W.Recorder);
        Expect(BootstrapLog.HasAvailableSink).ToBeTrue;
        Expect(Length(BootstrapLog.AvailableSinks)).ToEqual(1);
      end);

end.
