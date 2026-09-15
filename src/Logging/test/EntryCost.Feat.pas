unit EntryCost.Feat;

{
  El coste de una linea de log: lo que se puede preguntar antes de escribirla, y lo que no se paga
  cuando la respuesta es que no se va a escribir. Dos afirmaciones, cada una con su control:
    * un destino que anota lo que recibe NO recibe la linea descartada -- y SI recibe la aceptada;
    * un destino que no sabe que niveles acepta no descarta nada -- y uno que si sabe contesta que no.
}

interface

uses
  System.SysUtils,
  System.Rtti,
  Daf.Extensions.Logging,
  Daf.Extensions.DependencyInjection,
  Daf.Logging.Builder;

type
  /// Un proveedor de prueba cuyo logger (a) contesta IsEnabled desde un suelo de nivel configurable
  /// --o que si a todo, si se le dice que "no sabe"-- y (b) anota CADA entrada que se le entrega.
  /// Anotar lo entregado, y no filtrar dentro de Log, es lo unico que deja distinguir "la puerta la
  /// descarto antes de componer" de "el destino la filtro despues": lo segundo tambien dejaria Count
  /// en cero.
  ILevelProbe = interface(ILoggerProvider)
    ['{B7A3C1D2-4E5F-4071-8293-A4B5C6D7E8F9}']
    function Count: Integer;
    function Last: string;
  end;

  TEntryCostWorld = class
  strict private
    FProvider: IServiceProvider;
  public
    Services: IServiceCollection;
    Probe: ILevelProbe;
    Answer: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Compose(const Add: TProc<ILoggingBuilder>);
    function Logger: ILogger;
    procedure Ask(const Level: TLogLevel);
  end;

/// Un destino que acepta de este nivel hacia arriba.
function ProbeAcceptingFrom(const Floor: TLogLevel): ILevelProbe;
/// Un destino que no puede decir que niveles acepta: su IsEnabled contesta que si a todo.
function ProbeThatCannotSay: ILevelProbe;

implementation

uses
  Daf.MiniSpec,
  Daf.Logging,
  Daf.DependencyInjection;

type
  TLevelProbe = class(TInterfacedObject, ILoggerProvider, ILevelProbe)
  strict private
    FFloor: TLogLevel;
    FCannotSay: Boolean;
    FLines: TArray<string>;
  public
    constructor Create(const Floor: TLogLevel; const CannotSay: Boolean);
    function CreateLogger(const Category: string): ILogger;
    function Accepts(const Level: TLogLevel): Boolean;
    procedure Took(const Line: string);
    function Count: Integer;
    function Last: string;
  end;

  TLevelProbeLogger = class(TLogger)
  strict private
    FOwner: TLevelProbe;
  public
    constructor Create(const Category: string; const Owner: TLevelProbe);
    function IsEnabled(const Level: TLogLevel): Boolean; override;
    procedure Log(const Entry: TLogEntry); override;
  end;

{ TLevelProbe }

constructor TLevelProbe.Create(const Floor: TLogLevel; const CannotSay: Boolean);
begin
  inherited Create;
  FFloor := Floor;
  FCannotSay := CannotSay;
end;

function TLevelProbe.Accepts(const Level: TLogLevel): Boolean;
begin
  // "No sabe" contesta que si: perder una linea por no saber es peor que componer una de mas.
  Result := FCannotSay or (Level >= FFloor);
end;

function TLevelProbe.CreateLogger(const Category: string): ILogger;
begin
  Result := TLevelProbeLogger.Create(Category, Self);
end;

procedure TLevelProbe.Took(const Line: string);
begin
  FLines := FLines + [Line];
end;

function TLevelProbe.Count: Integer;
begin
  Result := Length(FLines);
end;

function TLevelProbe.Last: string;
begin
  if Length(FLines) = 0 then
    Exit('');
  Result := FLines[High(FLines)];
end;

{ TLevelProbeLogger }

constructor TLevelProbeLogger.Create(const Category: string; const Owner: TLevelProbe);
begin
  inherited Create(Category);
  FOwner := Owner;
end;

function TLevelProbeLogger.IsEnabled(const Level: TLogLevel): Boolean;
begin
  Result := FOwner.Accepts(Level);
end;

procedure TLevelProbeLogger.Log(const Entry: TLogEntry);
begin
  // Categoria, nivel y mensaje formateado: leer Entry.Message es lo que fuerza la composicion, que
  // es justo el trabajo que la puerta evita en la linea descartada.
  FOwner.Took(Entry.Category + '|' + Entry.Level.ToString + '|' + Entry.Message);
end;

function ProbeAcceptingFrom(const Floor: TLogLevel): ILevelProbe;
begin
  Result := TLevelProbe.Create(Floor, False);
end;

function ProbeThatCannotSay: ILevelProbe;
begin
  Result := TLevelProbe.Create(TLogLevel.Trace, True);
end;

{ TEntryCostWorld }

constructor TEntryCostWorld.Create;
begin
  inherited Create;
  Services := TServiceCollection.Create;
  Answer := False;
end;

destructor TEntryCostWorld.Destroy;
begin
  if FProvider <> nil then
    FProvider.ShutDown;
  inherited;
end;

procedure TEntryCostWorld.Compose(const Add: TProc<ILoggingBuilder>);
begin
  AddLogging(Services, Add);
end;

function TEntryCostWorld.Logger: ILogger;
begin
  if FProvider = nil then
    FProvider := Services.BuildServiceProvider;
  Result := FProvider.GetService<ILoggerFactory>.CreateLogger('Acme.Payroll');
end;

procedure TEntryCostWorld.Ask(const Level: TLogLevel);
begin
  Answer := Logger.IsEnabled(Level);
end;

initialization

Feature('''
Feature The line that is not written is not paid for @logging @entry-cost

  As someone writing a service
  I want a line no destination would accept to cost nothing, and to be able to ask before I build
    its arguments
  So that a hot-loop LogDebug with logging off does not pay for what gets thrown away
''')

.UseWorld<TEntryCostWorld>

.Rule('A consumer can ask whether the line is worth preparing')

  .Scenario('A level no destination accepts is answered with a no')
    .Given('a pipeline whose only destination accepts Warning and above',
      procedure(W: TEntryCostWorld)
      begin
        var P := ProbeAcceptingFrom(TLogLevel.Warning);
        W.Probe := P;
        W.Compose(
          procedure(Builder: ILoggingBuilder)
          begin
            Builder.AddProvider(P);
          end);
      end)
    .When('the consumer asks whether a Debug line would be written',
      procedure(W: TEntryCostWorld)
      begin
        W.Ask(TLogLevel.Debug);
      end)
    .&Then('the answer is no, so it can skip building its arguments',
      procedure(W: TEntryCostWorld)
      begin
        Expect(W.Answer).ToBeFalse;
      end)

  .Scenario('A level some destination accepts is answered with a yes')
    .Given('a pipeline whose only destination accepts Warning and above',
      procedure(W: TEntryCostWorld)
      begin
        var P := ProbeAcceptingFrom(TLogLevel.Warning);
        W.Probe := P;
        W.Compose(
          procedure(Builder: ILoggingBuilder)
          begin
            Builder.AddProvider(P);
          end);
      end)
    .When('the consumer asks whether an Error line would be written',
      procedure(W: TEntryCostWorld)
      begin
        W.Ask(TLogLevel.Error);
      end)
    .&Then('the answer is yes',
      procedure(W: TEntryCostWorld)
      begin
        Expect(W.Answer).ToBeTrue;
      end)

  .Scenario('A destination that cannot say what it accepts does not drop the line')
    .Given('a pipeline whose destination cannot say which levels it accepts',
      procedure(W: TEntryCostWorld)
      begin
        var P := ProbeThatCannotSay;
        W.Probe := P;
        W.Compose(
          procedure(Builder: ILoggingBuilder)
          begin
            Builder.AddProvider(P);
          end);
      end)
    .When('the consumer asks about a level a floor would have rejected',
      procedure(W: TEntryCostWorld)
      begin
        W.Ask(TLogLevel.Debug);
      end)
    .&Then('the answer is yes, in place of dropping by not knowing',
      procedure(W: TEntryCostWorld)
      begin
        Expect(W.Answer).ToBeTrue;
      end)

.Rule('An entry no destination would accept is never composed')

  .Scenario('The destination does not receive the discarded entry')
    .Given('a destination that records every entry it is handed, accepting Warning and above',
      procedure(W: TEntryCostWorld)
      begin
        var P := ProbeAcceptingFrom(TLogLevel.Warning);
        W.Probe := P;
        W.Compose(
          procedure(Builder: ILoggingBuilder)
          begin
            Builder.AddProvider(P);
          end);
      end)
    .When('a Debug line -- one that destination does not accept -- is written',
      procedure(W: TEntryCostWorld)
      begin
        W.Logger.LogDebug('a chatty debug line');
      end)
    .&Then('the destination recorded nothing',
      procedure(W: TEntryCostWorld)
      begin
        // Se afirma sobre lo que el destino ANOTO, no sobre que no se lanzara: no lanzar lo hace
        // tambien un pipeline que se trago la linea por otro motivo.
        Expect(W.Probe.Count).ToEqual(0);
      end)

  .Scenario('The accepted line arrives just as before')
    .Given('a destination that records every entry it is handed, accepting Warning and above',
      procedure(W: TEntryCostWorld)
      begin
        var P := ProbeAcceptingFrom(TLogLevel.Warning);
        W.Probe := P;
        W.Compose(
          procedure(Builder: ILoggingBuilder)
          begin
            Builder.AddProvider(P);
          end);
      end)
    .When('an Error line with a template argument is written',
      procedure(W: TEntryCostWorld)
      begin
        W.Logger.LogError('the payment for {Customer} failed', [TValue.From<string>('Acme')]);
      end)
    .&Then('the destination received exactly one entry',
      procedure(W: TEntryCostWorld)
      begin
        Expect(W.Probe.Count).ToEqual(1);
      end)
    .&And('with its category, its level and its formatted message',
      procedure(W: TEntryCostWorld)
      begin
        Expect(W.Probe.Last).ToContain('Acme.Payroll');
        Expect(W.Probe.Last).ToContain('ERROR');
        Expect(W.Probe.Last).ToContain('the payment for Acme failed');
      end)

.Rule('Silence asked for on purpose costs nothing')

  .Scenario('The declared silence stops composing')
    .Given('a pipeline told to discard through the null provider',
      procedure(W: TEntryCostWorld)
      begin
        W.Compose(
          procedure(Builder: ILoggingBuilder)
          begin
            Builder.AddNullProvider;
          end);
      end)
    .When('the consumer asks whether an Error line would be written',
      procedure(W: TEntryCostWorld)
      begin
        W.Ask(TLogLevel.Error);
      end)
    .&Then('the answer is no, so nothing gets composed (D6)',
      procedure(W: TEntryCostWorld)
      begin
        Expect(W.Answer).ToBeFalse;
      end)
    .&And('and writing through it however many entries neither writes nor fails',
      procedure(W: TEntryCostWorld)
      begin
        ExpectException(
          procedure
          begin
            for var I := 1 to 50 do
              W.Logger.LogError('the payment failed');
          end).ToBeNone;
      end);

end.
