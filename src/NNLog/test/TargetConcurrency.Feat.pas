unit TargetConcurrency.Feat;

// Un destino lo comparten TODOS los loggers: la regla guarda las referencias a sus destinos y les
// despacha cada entrada. Asi que el destino es el recurso compartido del pipeline, y es donde tiene
// que estar el candado.
//
// Lo que estos escenarios NO hacen es provocar la corrupcion. En el log real que motivo esto
// aparecio 2 veces en 126.185 lineas, de modo que un escenario que la busque da verde cuando tuvo
// suerte: su verde coincide con su modo de fallo. Lo determinista es la propiedad que el candado da
// y que su ausencia rompe siempre: que dos escrituras no se solapen.
//
// Y el contador de escrituras no es adorno. "Cero solapes" lo cumple igual un destino que no
// recibio nada, asi que cada escenario afirma ademas CUANTAS llegaron.

interface

uses
  System.Classes,
  System.SysUtils,
  Daf.Extensions.Logging,
  Daf.NNLog;

type
  // Anota el solape sin medir tiempos: lleva la cuenta de cuantos hilos hay dentro a la vez. Si al
  // entrar ya habia alguno, eso ES un solape, tarde lo que tarde cada uno.
  TOverlapLedger = class
  strict private
    FInside: Integer;
    FOverlaps: Integer;
    FWrites: Integer;
  public
    procedure Arrive;
    procedure Depart;
    function Overlaps: Integer;
    function Writes: Integer;
  end;

  // Tarda dentro a proposito: sin candado, varios hilos coinciden con probabilidad practica de uno.
  TSlowTarget = class(TTarget)
  strict private
    FLedger: TOverlapLedger;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Ledger: TOverlapLedger read FLedger;
  protected
    procedure DoWrite(const Entry: TLogEntry); override;
  end;

  // Para "dos destinos no se esperan": al entrar avisa de que llego y espera a que el otro avise.
  // Si los dos candados fueran el mismo, el segundo no podria entrar y la espera vence. Distingue
  // candado por instancia de candado compartido sin cronometrar nada.
  TRendezvousTarget = class(TTarget)
  strict private
    FArrived: TObject;
    FPartner: TRendezvousTarget;
    FMet: Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure PairWith(const Other: TRendezvousTarget);
    property Met: Boolean read FMet;
  protected
    procedure DoWrite(const Entry: TLogEntry); override;
  end;

  TConcurrencyWorld = class
  strict private
    FTarget: TSlowTarget;
    FLeft: TRendezvousTarget;
    FRight: TRendezvousTarget;
    procedure RunThreads(const Bodies: TArray<TProc>);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ManyThreadsWriteToOneTarget(const ThreadCount, LinesEach: Integer);
    procedure TwoThreadsWriteToTwoTargets;
    function Overlaps: Integer;
    function Writes: Integer;
    function BothMet: Boolean;
  end;

implementation

uses
  System.SyncObjs,
  Daf.MiniSpec;

const
  // Suficiente para que el solape sea seguro sin candado, y despreciable en la suite con el.
  DwellMs = 3;
  // Generoso a proposito: con candado por instancia el encuentro es inmediato, y solo vence si el
  // segundo destino no puede entrar. Un numero mayor no cambia el veredicto, solo lo que se espera.
  RendezvousMs = 2000;

function EntryFor(const Category: string): TLogEntry;
begin
  Result := Default(TLogEntry);
  Result.Level := TLogLevel.Information;
  Result.Category := Category;
  Result.State := TLogState.Create('linea');
end;

{ TOverlapLedger }

procedure TOverlapLedger.Arrive;
begin
  if TInterlocked.Increment(FInside) > 1 then
    TInterlocked.Increment(FOverlaps);
  TInterlocked.Increment(FWrites);
end;

procedure TOverlapLedger.Depart;
begin
  TInterlocked.Decrement(FInside);
end;

function TOverlapLedger.Overlaps: Integer;
begin
  Result := TInterlocked.CompareExchange(FOverlaps, 0, 0);
end;

function TOverlapLedger.Writes: Integer;
begin
  Result := TInterlocked.CompareExchange(FWrites, 0, 0);
end;

{ TSlowTarget }

constructor TSlowTarget.Create;
begin
  inherited Create;
  FLedger := TOverlapLedger.Create;
end;

destructor TSlowTarget.Destroy;
begin
  FLedger.Free;
  inherited;
end;

procedure TSlowTarget.DoWrite(const Entry: TLogEntry);
begin
  FLedger.Arrive;
  try
    TThread.Sleep(DwellMs);
  finally
    FLedger.Depart;
  end;
end;

{ TRendezvousTarget }

constructor TRendezvousTarget.Create;
begin
  inherited Create;
  // Manual: una vez avisado se queda avisado, para que el orden de llegada no decida el resultado.
  FArrived := TEvent.Create(nil, True, False, EmptyStr);
end;

destructor TRendezvousTarget.Destroy;
begin
  FArrived.Free;
  inherited;
end;

procedure TRendezvousTarget.PairWith(const Other: TRendezvousTarget);
begin
  FPartner := Other;
end;

procedure TRendezvousTarget.DoWrite(const Entry: TLogEntry);
begin
  TEvent(FArrived).SetEvent;
  FMet := TEvent(FPartner.FArrived).WaitFor(RendezvousMs) = wrSignaled;
end;

{ TConcurrencyWorld }

constructor TConcurrencyWorld.Create;
begin
  inherited Create;
  FTarget := TSlowTarget.Create;
  FLeft := TRendezvousTarget.Create;
  FRight := TRendezvousTarget.Create;
  FLeft.PairWith(FRight);
  FRight.PairWith(FLeft);
end;

destructor TConcurrencyWorld.Destroy;
begin
  FTarget.Free;
  FLeft.Free;
  FRight.Free;
  inherited;
end;

procedure TConcurrencyWorld.RunThreads(const Bodies: TArray<TProc>);
var
  Threads: TArray<TThread>;
begin
  SetLength(Threads, Length(Bodies));
  for var I := 0 to High(Bodies) do
  begin
    Threads[I] := TThread.CreateAnonymousThread(Bodies[I]);
    Threads[I].FreeOnTerminate := False;
  end;
  try
    for var T in Threads do
      T.Start;
    for var T in Threads do
      T.WaitFor;
  finally
    for var T in Threads do
      T.Free;
  end;
end;

procedure TConcurrencyWorld.ManyThreadsWriteToOneTarget(const ThreadCount, LinesEach: Integer);
var
  Bodies: TArray<TProc>;
begin
  SetLength(Bodies, ThreadCount);
  for var I := 0 to ThreadCount - 1 do
  begin
    var Mine := I;
    Bodies[I] :=
      procedure
      begin
        for var J := 1 to LinesEach do
          FTarget.Write(EntryFor('Hilo' + Mine.ToString));
      end;
  end;
  RunThreads(Bodies);
end;

procedure TConcurrencyWorld.TwoThreadsWriteToTwoTargets;
begin
  RunThreads([
    procedure
    begin
      FLeft.Write(EntryFor('Izquierda'));
    end,
    procedure
    begin
      FRight.Write(EntryFor('Derecha'));
    end]);
end;

function TConcurrencyWorld.Overlaps: Integer;
begin
  Result := FTarget.Ledger.Overlaps;
end;

function TConcurrencyWorld.Writes: Integer;
begin
  Result := FTarget.Ledger.Writes;
end;

function TConcurrencyWorld.BothMet: Boolean;
begin
  Result := FLeft.Met and FRight.Met;
end;

initialization

Feature('''
Feature Un destino de NNLog atiende a varios hilos sin corromperse @nnlog @targets @concurrency

  Como quien registra desde una aplicacion multihilo
  Quiero que el destino serialice sus propias escrituras
  Para que dos hilos que registran a la vez no entrelacen sus lineas ni deriven un recurso que el
  otro acaba de liberar
''')

.UseWorld<TConcurrencyWorld>

.Rule('Un destino serializa sus propias escrituras')

  .Scenario('Dos hilos que escriben por el mismo destino no se solapan')
    .Given('un destino que anota cuando entra y cuando sale de cada escritura, y tarda dentro',
      procedure(W: TConcurrencyWorld)
      begin
        Expect(W.Writes).ToEqual(0);
      end)
    .When('cuatro hilos registran diez lineas cada uno por ese mismo destino',
      procedure(W: TConcurrencyWorld)
      begin
        W.ManyThreadsWriteToOneTarget(4, 10);
      end)
    .&Then('ninguna escritura empezo antes de que terminara la anterior',
      procedure(W: TConcurrencyWorld)
      begin
        Expect(W.Overlaps).ToEqual(0);
      end)
    .&And('y llegaron las cuarenta, que es lo que hace que el cero de arriba signifique algo',
      procedure(W: TConcurrencyWorld)
      begin
        Expect(W.Writes).ToEqual(40);
      end)

  .Scenario('Dos destinos distintos no se esperan')
    .Given('dos destinos, cada uno con su instancia, emparejados para avisarse',
      procedure(W: TConcurrencyWorld)
      begin
        Expect(W.BothMet).ToBeFalse;
      end)
    .When('un hilo entra en la escritura de cada uno',
      procedure(W: TConcurrencyWorld)
      begin
        W.TwoThreadsWriteToTwoTargets;
      end)
    .&Then('los dos se encuentran dentro, asi que el candado de uno no cierra al otro',
      procedure(W: TConcurrencyWorld)
      begin
        Expect(W.BothMet).ToBeTrue;
      end);

end.
