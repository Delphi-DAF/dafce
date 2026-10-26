unit Cancellation.Feat;

interface

implementation

uses
  Winapi.Windows,
  Winapi.TlHelp32,
  System.SysUtils,
  System.Classes,
  System.Diagnostics,
  System.SyncObjs,
  System.Threading,
  System.Generics.Collections,
  System.Generics.Defaults,
  Daf.MiniSpec,
  Daf.Threading;

const
  RaceRounds = 1000;

type
  TCancellationWorld = class
  public
    Source: ICancellationTokenSource;
    Notes: string;
    Count: Integer;
    Report: string;
    ElapsedMS: Int64;
    ThreadsBefore: TArray<Cardinal>;
    /// Un callback que anota Name en Notes.
    function Noting(const Name: string): TProc;
    /// Un callback que lanza una excepcion con Name de mensaje.
    function Failing(const Name: string): TProc;
  end;

function TCancellationWorld.Noting(const Name: string): TProc;
begin
  Result :=
    procedure
    begin
      if Notes <> '' then
        Notes := Notes + ',';
      Notes := Notes + Name;
    end;
end;

function TCancellationWorld.Failing(const Name: string): TProc;
begin
  Result :=
    procedure
    begin
      raise Exception.Create(Name);
    end;
end;

/// La clase de lo que lanza Cancel y, si es agregada, los mensajes de dentro en orden alfabetico.
function CancelOutcome(const Source: ICancellationTokenSource): string;
begin
  try
    Source.Cancel;
    Result := 'sin excepcion';
  except
    on E: EAggregateException do
    begin
      var Messages := TList<string>.Create;
      try
        for var I := 0 to E.Count - 1 do
          Messages.Add(E.InnerExceptions[I].Message);
        Messages.Sort;
        Result := 'EAggregateException[' + string.Join(',', Messages.ToArray) + ']';
      finally
        Messages.Free;
      end;
    end;
    on E: Exception do
      Result := E.ClassName + ': ' + E.Message;
  end;
end;

/// Los identificadores de los hilos vivos del proceso.
function ProcessThreads: TArray<Cardinal>;
var
  Entry: TThreadEntry32;
begin
  Result := nil;
  var Snapshot := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
  if Snapshot = INVALID_HANDLE_VALUE then
    Exit;
  try
    Entry.dwSize := SizeOf(Entry);
    if Thread32First(Snapshot, Entry) then
      repeat
        if Entry.th32OwnerProcessID = GetCurrentProcessId then
          Result := Result + [Entry.th32ThreadID];
      until not Thread32Next(Snapshot, Entry);
  finally
    CloseHandle(Snapshot);
  end;
end;

/// Una vuelta: un hilo cancela una fuente nueva y otro registra en ella, a la vez. Cuantas veces corrio
/// el callback.
function RaceOnce: Integer;
begin
  var Source := CreateCancellationTokenSource;
  var Go := TEvent.Create(nil, True, False, '');
  var Runs := 0;
  try
    var Canceller := TThread.CreateAnonymousThread(
      procedure
      begin
        Go.WaitFor(INFINITE);
        Source.Cancel;
      end);
    var Registrar := TThread.CreateAnonymousThread(
      procedure
      begin
        Go.WaitFor(INFINITE);
        Source.Token.Register(
          procedure
          begin
            TInterlocked.Increment(Runs);
          end);
      end);
    Canceller.FreeOnTerminate := False;
    Registrar.FreeOnTerminate := False;
    Canceller.Start;
    Registrar.Start;
    Go.SetEvent;
    Canceller.WaitFor;
    Registrar.WaitFor;
    Canceller.Free;
    Registrar.Free;
  finally
    Go.Free;
  end;
  Result := Runs;
end;

initialization

Feature('''
Feature Cancelar una fuente @commons @cancellation

  Como quien registra callbacks en un token de cancelacion
  Quiero que cancelar los ejecute todos, una sola vez y en orden inverso, aunque alguno falle
  Para que un callback que falla no deje a los demas sin ejecutar, como en .NET
''')

.UseWorld<TCancellationWorld>

.Rule('Cancelar ejecuta todos los callbacks aunque alguno falle')

  .Scenario('Un callback que falla no impide los demas')
    .Given('una fuente con tres callbacks registrados, y el segundo lanza',
      procedure(W: TCancellationWorld)
      begin
        W.Source := CreateCancellationTokenSource;
        W.Source.Token.Register(W.Noting('uno'));
        W.Source.Token.Register(W.Failing('dos'));
        W.Source.Token.Register(W.Noting('tres'));
      end)
    .When('se cancela',
      procedure(W: TCancellationWorld)
      begin
        W.Report := CancelOutcome(W.Source);
      end)
    .&Then('el primero y el tercero se ejecutaron',
      procedure(W: TCancellationWorld)
      begin
        Expect(Format('uno=%s tres=%s', [BoolToStr(W.Notes.Contains('uno'), True),
          BoolToStr(W.Notes.Contains('tres'), True)])).ToEqual('uno=True tres=True');
      end)
    .&And('la cancelacion lanza una excepcion agregada que contiene la del segundo',
      procedure(W: TCancellationWorld)
      begin
        Expect(W.Report).ToEqual('EAggregateException[dos]');
      end)

  .Scenario('Dos callbacks que fallan')
    .Given('una fuente con dos callbacks que lanzan los dos',
      procedure(W: TCancellationWorld)
      begin
        W.Source := CreateCancellationTokenSource;
        W.Source.Token.Register(W.Failing('a'));
        W.Source.Token.Register(W.Failing('b'));
      end)
    .When('se cancela',
      procedure(W: TCancellationWorld)
      begin
        W.Report := CancelOutcome(W.Source);
      end)
    .&Then('la cancelacion lanza una excepcion agregada con las dos dentro',
      procedure(W: TCancellationWorld)
      begin
        Expect(W.Report).ToEqual('EAggregateException[a,b]');
      end)

  .Scenario('Sin fallos no lanza nada')
    .Given('una fuente con dos callbacks que no lanzan',
      procedure(W: TCancellationWorld)
      begin
        W.Source := CreateCancellationTokenSource;
        W.Source.Token.Register(W.Noting('uno'));
        W.Source.Token.Register(W.Noting('dos'));
      end)
    .When('se cancela',
      procedure(W: TCancellationWorld)
      begin
        W.Report := CancelOutcome(W.Source);
      end)
    .&Then('los dos se ejecutaron y la cancelacion no lanza',
      procedure(W: TCancellationWorld)
      begin
        Expect(Format('%s anotados=%d', [W.Report, Length(W.Notes.Split([',']))])).ToEqual('sin excepcion anotados=2');
      end)

.Rule('Los callbacks se ejecutan en orden inverso al de registro')

  .Scenario('Tres callbacks')
    .Given('una fuente con tres callbacks registrados en orden, cada uno anota su nombre',
      procedure(W: TCancellationWorld)
      begin
        W.Source := CreateCancellationTokenSource;
        W.Source.Token.Register(W.Noting('A'));
        W.Source.Token.Register(W.Noting('B'));
        W.Source.Token.Register(W.Noting('C'));
      end)
    .When('se cancela',
      procedure(W: TCancellationWorld)
      begin
        W.Report := CancelOutcome(W.Source);
      end)
    .&Then('las anotaciones salen en el orden inverso al de registro',
      procedure(W: TCancellationWorld)
      begin
        Expect(W.Notes).ToEqual('C,B,A');
      end)

.Rule('Cada callback se ejecuta una sola vez')

  .Scenario('Cancelar otra vez despues de un fallo')
    .Given('una fuente con un callback que cuenta sus ejecuciones, registrado antes que otro que lanza',
      procedure(W: TCancellationWorld)
      begin
        W.Source := CreateCancellationTokenSource;
        W.Source.Token.Register(
          procedure
          begin
            Inc(W.Count);
          end);
        W.Source.Token.Register(W.Failing('falla'));
      end)
    .When('se cancela dos veces, recogiendo lo que lanza cada una',
      procedure(W: TCancellationWorld)
      begin
        CancelOutcome(W.Source);
        CancelOutcome(W.Source);
      end)
    .&Then('el callback que cuenta se ejecuto una vez',
      procedure(W: TCancellationWorld)
      begin
        Expect(W.Count).ToEqual(1);
      end)

.Rule('Un callback registrado mientras se cancela no se pierde')

  .Scenario('Registrar y cancelar a la vez desde dos hilos')
    .Given('muchas vueltas, cada una con una fuente nueva, un hilo que la cancela y otro que registra en ella un callback que cuenta, a la vez',
      procedure(W: TCancellationWorld)
      begin
        W.Count := 0;
      end)
    .When('terminan las vueltas',
      procedure(W: TCancellationWorld)
      begin
        var Wrong := 0;
        for var Round := 1 to RaceRounds do
          if RaceOnce <> 1 then
            Inc(Wrong);
        W.Report := Format('%d de %d vueltas sin exactamente una ejecucion', [Wrong, RaceRounds]);
      end)
    .&Then('en cada vuelta el callback se ejecuto exactamente una vez',
      procedure(W: TCancellationWorld)
      begin
        Expect(W.Report).ToEqual(Format('0 de %d vueltas sin exactamente una ejecucion', [RaceRounds]));
      end)

.Rule('Liberar una fuente con plazo antes de que venza detiene su temporizador')

  .Scenario('Una fuente con un plazo largo se libera enseguida')
    .Given('una fuente con un plazo de un minuto',
      procedure(W: TCancellationWorld)
      begin
        W.ThreadsBefore := ProcessThreads;
        W.Source := CreateCancellationTokenSourceWithTimeout(60000);
      end)
    .When('se libera antes de que venza',
      procedure(W: TCancellationWorld)
      begin
        var Watch := TStopwatch.StartNew;
        W.Source := nil;
        W.ElapsedMS := Watch.ElapsedMilliseconds;
      end)
    .&Then('la liberacion vuelve enseguida',
      procedure(W: TCancellationWorld)
      begin
        Expect(Format('enseguida=%s', [BoolToStr(W.ElapsedMS < 1000, True)])).ToEqual('enseguida=True');
      end)
    .&And('no queda vivo ningun hilo de su temporizador',
      procedure(W: TCancellationWorld)
      begin
        var Before := TList<Cardinal>.Create(W.ThreadsBefore);
        try
          var Remaining := 0;
          for var Id in ProcessThreads do
            if not Before.Contains(Id) then
              Inc(Remaining);
          Expect(Format('hilos nuevos=%d', [Remaining])).ToEqual('hilos nuevos=0');
        finally
          Before.Free;
        end;
      end);

end.
