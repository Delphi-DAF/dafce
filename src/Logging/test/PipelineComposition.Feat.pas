unit PipelineComposition.Feat;

interface

uses
  System.SysUtils,
  Daf.Extensions.Logging,
  Daf.Logging,
  Daf.Extensions.DependencyInjection,
  Daf.DependencyInjection,
  DAF.Logging.Builder,
  BootstrapChannel.Feat;

type
  /// Un proveedor que se queda con lo que le llega. Es la unica forma de comprobar que un pipeline
  /// con destino sigue escribiendo donde escribia: mirar donde escriben los proveedores de verdad
  /// seria probar el proveedor, no la composicion.
  IProbeProvider = interface(ILoggerProvider)
    ['{2198CB3B-5174-45AC-83BB-0E4708EC4CCF}']
    function Count: Integer;
    function Text: string;
  end;

  /// El canal de arranque es global: cada escenario lo deja como lo encontro. Y hace falta mirarlo,
  /// porque una de las cosas que hay que comprobar es que el camino normal NO escribe ahi.
  TCompositionWorld = class
  strict private
    FSaved: TArray<IBootstrapSink>;
    FProvider: IServiceProvider;
  public
    Services: IServiceCollection;
    Probe: IProbeProvider;
    Channel: IRecordingSink;
    Failure: string;
    FailureClass: string;
    constructor Create;
    destructor Destroy; override;
    /// Deja el canal con un solo destino, el que graba, para poder afirmar que no llego nada.
    procedure WatchStartupChannel;
    procedure Compose(const Add: TProc<ILoggingBuilder>);
    function ComposedFactory: Boolean;
    /// Un logger sacado del contenedor, como lo saca la aplicacion.
    function Logger: ILogger;
    procedure WriteSome(const Times: Integer);
  end;

function NewProbeProvider: IProbeProvider;

implementation

uses
  Daf.MiniSpec;

type
  TProbeProvider = class(TInterfacedObject, ILoggerProvider, IProbeProvider)
  strict private
    FLines: TArray<string>;
  public
    function CreateLogger(const Category: string): ILogger;
    function Count: Integer;
    function Text: string;
    procedure Took(const Line: string);
  end;

  TProbeLogger = class(TLogger)
  strict private
    FOwner: TProbeProvider;
  public
    constructor Create(const Category: string; const Owner: TProbeProvider);
    procedure Log(const Entry: TLogEntry); override;
  end;

{ TProbeLogger }

constructor TProbeLogger.Create(const Category: string; const Owner: TProbeProvider);
begin
  inherited Create(Category);
  FOwner := Owner;
end;

procedure TProbeLogger.Log(const Entry: TLogEntry);
begin
  FOwner.Took(Entry.Message);
end;

{ TProbeProvider }

function TProbeProvider.CreateLogger(const Category: string): ILogger;
begin
  Result := TProbeLogger.Create(Category, Self);
end;

procedure TProbeProvider.Took(const Line: string);
begin
  FLines := FLines + [Line];
end;

function TProbeProvider.Count: Integer;
begin
  Result := Length(FLines);
end;

function TProbeProvider.Text: string;
begin
  Result := string.Join(sLineBreak, FLines);
end;

function NewProbeProvider: IProbeProvider;
begin
  Result := TProbeProvider.Create;
end;

{ TCompositionWorld }

constructor TCompositionWorld.Create;
begin
  inherited Create;
  FSaved := BootstrapLog.Sinks;
  Services := TServiceCollection.Create;
  Probe := NewProbeProvider;
  Channel := NewRecordingSink;
end;

destructor TCompositionWorld.Destroy;
begin
  if FProvider <> nil then
    FProvider.ShutDown;
  BootstrapLog.ClearSinks;
  for var Sink in FSaved do
    BootstrapLog.AddSink(Sink);
  inherited;
end;

procedure TCompositionWorld.WatchStartupChannel;
begin
  BootstrapLog.ClearSinks;
  BootstrapLog.AddSink(Channel);
end;

procedure TCompositionWorld.Compose(const Add: TProc<ILoggingBuilder>);
begin
  Failure := '';
  FailureClass := '';
  try
    AddLogging(Services, Add);
  except
    on E: Exception do
    begin
      Failure := E.Message;
      FailureClass := E.ClassName;
    end;
  end;
end;

function TCompositionWorld.ComposedFactory: Boolean;
begin
  Result := Services.Contains<ILoggerFactory>;
end;

function TCompositionWorld.Logger: ILogger;
begin
  if FProvider = nil then
    FProvider := Services.BuildServiceProvider;
  Result := FProvider.GetService<ILoggerFactory>.CreateLogger('Acme.Payroll');
end;

procedure TCompositionWorld.WriteSome(const Times: Integer);
begin
  var Log := Logger;
  for var I := 1 to Times do
    Log.LogError('the payment failed');
end;

initialization

Feature('''
Feature A pipeline with no destination is not a pipeline @logging @pipeline @composition

  As someone who composes an application
  I want composing logging with no destination to fail where I compose it
  So that nobody debugs an incident reading records that were never written
''')

.UseWorld<TCompositionWorld>

.Rule('Forgetting the destination is a composition error, not a running mode')

  .Scenario('Composing with no provider hands out no logger at all')
    .When('an application composes the pipeline without registering a single provider',
      procedure(W: TCompositionWorld)
      begin
        W.Compose(
          procedure(Builder: ILoggingBuilder)
          begin
            // El descuido tal cual se comete: el AddProvider que no esta.
          end);
      end)
    .&Then('the composition fails, and it fails right there',
      procedure(W: TCompositionWorld)
      begin
        Expect(W.FailureClass).ToEqual('ELoggingCompositionError');
      end)
    .&And('nothing was left registered that could accept an entry and drop it',
      procedure(W: TCompositionWorld)
      begin
        Expect(W.ComposedFactory).ToBeFalse;
      end)
    .&And('the reason says how to ask for the silence on purpose',
      procedure(W: TCompositionWorld)
      begin
        Expect(W.Failure).ToContain('AddNullProvider');
      end)

.Rule('Declaring the silence is not the same as forgetting the destination')

  .Scenario('Silence asked for out loud composes, and stays silent')
    .When('the application declares that it wants the entries dropped',
      procedure(W: TCompositionWorld)
      begin
        W.WatchStartupChannel;
        W.Compose(
          procedure(Builder: ILoggingBuilder)
          begin
            Builder.AddNullProvider;
          end);
      end)
    .&Then('the composition goes through',
      procedure(W: TCompositionWorld)
      begin
        Expect(W.Failure).ToBeEmpty;
        Expect(W.ComposedFactory).ToBeTrue;
      end)
    .&And('writing through it neither writes anywhere nor fails, however many entries go in',
      procedure(W: TCompositionWorld)
      begin
        // Aqui se ve que la comprobacion es de composicion y no de escritura: por muchas entradas
        // que pasen, ninguna falla y ninguna se informa.
        ExpectException(
          procedure
          begin
            W.WriteSome(50);
          end).ToBeNone;
        Expect(W.Channel.Count).ToEqual(0);
      end)

.Rule('With a destination, nothing changes')

  .Scenario('The entries reach the provider, as before')
    .Given('a provider that keeps what it is given',
      procedure(W: TCompositionWorld)
      begin
        W.Compose(
          procedure(Builder: ILoggingBuilder)
          begin
            Builder.AddProvider(W.Probe);
          end);
        Expect(W.Failure).ToBeEmpty;
      end)
    .When('the application writes through the pipeline',
      procedure(W: TCompositionWorld)
      begin
        W.WriteSome(2);
      end)
    .&Then('both entries arrived at the provider',
      procedure(W: TCompositionWorld)
      begin
        Expect(W.Probe.Count).ToEqual(2);
        Expect(W.Probe.Text).ToContain('the payment failed');
      end)

  .Scenario('And the startup channel is not used to say anything about it')
    .Given('the startup channel watched, with nothing else writing to it',
      procedure(W: TCompositionWorld)
      begin
        W.WatchStartupChannel;
      end)
    .When('the application composes with a provider and writes',
      procedure(W: TCompositionWorld)
      begin
        W.Compose(
          procedure(Builder: ILoggingBuilder)
          begin
            Builder.AddProvider(W.Probe);
          end);
        W.WriteSome(1);
      end)
    .&Then('not a line came out through the startup channel',
      procedure(W: TCompositionWorld)
      begin
        // El camino normal no paga ni un aviso: un aviso que sale siempre es el que se aprende a
        // ignorar, y entonces ya no avisa de nada.
        Expect(W.Failure).ToBeEmpty;
        Expect(W.Channel.Count).ToEqual(0);
      end);

end.
