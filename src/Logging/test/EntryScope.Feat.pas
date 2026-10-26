unit EntryScope.Feat;

interface

uses
  System.SysUtils,
  Daf.Extensions.Logging,
  Daf.Extensions.DependencyInjection;

type
  /// Un proveedor que se queda con el mensaje y el ambito de la ultima entrada, cada uno por su lado:
  /// es lo que deja ver si el ambito viaja dentro del mensaje o aparte.
  IScopeProbe = interface(ILoggerProvider)
    ['{A1F90316-F0ED-4726-ABA4-D95E3963CDCE}']
    function Message: string;
    function Scope: string;
  end;

  TEntryScopeWorld = class
  strict private
    FServices: IServiceCollection;
    FProvider: IServiceProvider;
    FLogger: ILogger;
    FScopes: TArray<ILogScopeVoid>;
  public
    Probe: IScopeProbe;
    constructor Create;
    destructor Destroy; override;
    procedure Compose;
    procedure OpenScope(const Text: string);
    procedure Write(const Line: string);
  end;

implementation

uses
  Daf.MiniSpec,
  Daf.Logging,
  Daf.Logging.Builder,
  Daf.DependencyInjection;

type
  TScopeProbe = class(TInterfacedObject, ILoggerProvider, IScopeProbe)
  strict private
    FMessage: string;
    FScope: string;
  public
    function CreateLogger(const Category: string): ILogger;
    procedure Took(const Entry: TLogEntry);
    function Message: string;
    function Scope: string;
  end;

  TScopeProbeLogger = class(TLogger)
  strict private
    FOwner: TScopeProbe;
  public
    constructor Create(const Category: string; const Owner: TScopeProbe);
    procedure Log(const Entry: TLogEntry); override;
  end;

{ TScopeProbeLogger }

constructor TScopeProbeLogger.Create(const Category: string; const Owner: TScopeProbe);
begin
  inherited Create(Category);
  FOwner := Owner;
end;

procedure TScopeProbeLogger.Log(const Entry: TLogEntry);
begin
  FOwner.Took(Entry);
end;

{ TScopeProbe }

function TScopeProbe.CreateLogger(const Category: string): ILogger;
begin
  Result := TScopeProbeLogger.Create(Category, Self);
end;

procedure TScopeProbe.Took(const Entry: TLogEntry);
begin
  FMessage := Entry.Message;
  FScope := Entry.Scope;
end;

function TScopeProbe.Message: string;
begin
  Result := FMessage;
end;

function TScopeProbe.Scope: string;
begin
  Result := FScope;
end;

{ TEntryScopeWorld }

constructor TEntryScopeWorld.Create;
begin
  inherited Create;
  FServices := TServiceCollection.Create;
  Probe := TScopeProbe.Create;
end;

destructor TEntryScopeWorld.Destroy;
begin
  // Del interno al externo: un ambito se cierra al soltarlo, y la pila exige que se cierre el de
  // arriba. Y antes que el logger, que es quien guarda la pila.
  for var I := High(FScopes) downto 0 do
    FScopes[I] := nil;
  FLogger := nil;
  if FProvider <> nil then
    FProvider.ShutDown;
  inherited;
end;

procedure TEntryScopeWorld.Compose;
begin
  AddLogging(FServices,
    procedure(Builder: ILoggingBuilder)
    begin
      Builder.AddProvider(Probe);
    end);
  FProvider := FServices.BuildServiceProvider;
  FLogger := FProvider.GetService<ILoggerFactory>.CreateLogger('Acme.Payroll');
end;

procedure TEntryScopeWorld.OpenScope(const Text: string);
begin
  FScopes := FScopes + [FLogger.BeginScope(Text)];
end;

procedure TEntryScopeWorld.Write(const Line: string);
begin
  FLogger.LogInformation(Line);
end;

initialization

Feature('''
Feature The scope of an entry travels apart from its message @logging @scope

  As someone who writes a logging provider
  I want the message of an entry to be its message and nothing else, with the scope apart
  So that I decide whether the scope is written and where, and no line carries it twice
''')

.UseWorld<TEntryScopeWorld>

.Rule('The message of an entry does not carry the scope')

  .Scenario('An entry inside a scope')
    .Given('a logger with a scope open',
      procedure(W: TEntryScopeWorld)
      begin
        W.Compose;
        W.OpenScope('order 12');
      end)
    .When('it writes a line',
      procedure(W: TEntryScopeWorld)
      begin
        W.Write('the payment failed');
      end)
    .&Then('the message of the entry is the text of the line, without the scope',
      procedure(W: TEntryScopeWorld)
      begin
        Expect(W.Probe.Message).ToEqual('the payment failed');
      end)
    .&And('the scope of the entry is the text of the scope, after the separator',
      procedure(W: TEntryScopeWorld)
      begin
        Expect(W.Probe.Scope).ToEqual(' | order 12');
      end)

  .Scenario('Nested scopes')
    .Given('a logger with a scope open inside another',
      procedure(W: TEntryScopeWorld)
      begin
        W.Compose;
        W.OpenScope('batch 7');
        W.OpenScope('order 12');
      end)
    .When('it writes a line',
      procedure(W: TEntryScopeWorld)
      begin
        W.Write('the payment failed');
      end)
    .&Then('the scope of the entry carries the inner one first and the outer one after, each after the separator',
      procedure(W: TEntryScopeWorld)
      begin
        Expect(W.Probe.Scope).ToEqual(' | order 12 | batch 7');
      end)

  .Scenario('An entry outside any scope')
    .Given('a logger with no scope open',
      procedure(W: TEntryScopeWorld)
      begin
        W.Compose;
      end)
    .When('it writes a line',
      procedure(W: TEntryScopeWorld)
      begin
        W.Write('the payment failed');
      end)
    .&Then('the scope of the entry is empty, and the message is the text of the line',
      procedure(W: TEntryScopeWorld)
      begin
        Expect(W.Probe.Message).ToEqual('the payment failed');
        Expect(W.Probe.Scope).ToEqual('');
      end);

end.
