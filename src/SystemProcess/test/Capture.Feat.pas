unit Capture.Feat;

interface

implementation

uses
  System.SysUtils,
  System.Classes,
  System.Diagnostics,
  System.Threading,
  Daf.MiniSpec,
  Daf.Threading,
  Daf.SystemProcess;

const
  // Un hijo que deja en segundo plano un nieto que hereda su salida estandar, vive unos cinco
  // segundos y escribe adios al terminar; el hijo escribe hola y termina en el acto.
  WithGrandchild = 'cmd /c start "" /b cmd /c "ping -n 6 127.0.0.1 >nul & echo adios" & echo hola';
  // Lo que tarda el nieto, con margen: una ejecucion que vuelve antes no lo ha esperado.
  GrandchildMS = 4000;

type
  TCaptureWorld = class
  public
    Proc: ISystemProcess;
    Result: TProcessResult;
    Future: IFuture<TProcessResult>;
    Source: ICancellationTokenSource;
    Captured: string;
    ElapsedMS: Int64;
    destructor Destroy; override;
    function Capture: TProcessOutputProc;
    function WithGrandchildProc(TimeoutMS: Cardinal): ISystemProcess;
  end;

function StatusText(Status: TProcessResult.TStatus): string;
begin
  case Status of
    TProcessResult.TStatus.Failed:    Result := 'Failed';
    TProcessResult.TStatus.Timeout:   Result := 'Timeout';
    TProcessResult.TStatus.Running:   Result := 'Running';
    TProcessResult.TStatus.Completed: Result := 'Completed';
    TProcessResult.TStatus.Canceled:  Result := 'Canceled';
    TProcessResult.TStatus.Killed:    Result := 'Killed';
  else
    Result := '?';
  end;
end;

destructor TCaptureWorld.Destroy;
begin
  // La tarea de un escenario que falla a medias se espera, para que su nieto no llegue al siguiente.
  if Assigned(Future) then
    try
      Future.Wait(30000);
    except
      on Exception do;
    end;
  inherited;
end;

function TCaptureWorld.Capture: TProcessOutputProc;
begin
  Result :=
    procedure(Text: string)
    begin
      TMonitor.Enter(Self);
      try
        Captured := Captured + Text;
      finally
        TMonitor.Exit(Self);
      end;
    end;
end;

function TCaptureWorld.WithGrandchildProc(TimeoutMS: Cardinal): ISystemProcess;
begin
  Result := TSystemProcess.Builder
    .Command(WithGrandchild)
    .StandardOutput(TProcessOutput.Capture(Capture()))
    .StandardError(TProcessOutput.Discard)
    .HideWindow(True)
    .Timeout(TimeoutMS)
    .Build;
end;

initialization

Feature('''
Feature Cuando termina la espera de lo capturado @systemprocess @capture

  Como quien lanza un proceso hijo y captura lo que escribe
  Quiero que la espera de la captura siga las reglas de .NET
  Para que un descendiente que retiene la tuberia no deje la ejecucion sin limite cuando hay plazo
''')

.UseWorld<TCaptureWorld>

.Rule('Sin plazo ni token, la ejecucion espera al fin de lo capturado')

  .Scenario('Un nieto que hereda la salida capturada la entrega entera')
    .Given('un hijo que deja en segundo plano un nieto que escribe adios al terminar, escribe hola y termina, con la salida estandar capturada y sin plazo',
      procedure(W: TCaptureWorld)
      begin
        W.Proc := W.WithGrandchildProc(INFINITE);
      end)
    .When('se ejecuta',
      procedure(W: TCaptureWorld)
      begin
        var Watch := TStopwatch.StartNew;
        W.Result := W.Proc.Execute;
        W.ElapsedMS := Watch.ElapsedMilliseconds;
      end)
    .&Then('Execute vuelve despues de que termine el nieto, con Completed',
      procedure(W: TCaptureWorld)
      begin
        Expect(Format('%s espero-al-nieto=%s', [StatusText(W.Result.Status),
          BoolToStr(W.ElapsedMS >= GrandchildMS, True)])).ToEqual('Completed espero-al-nieto=True');
      end)
    .&And('el manejador recibio hola y adios',
      procedure(W: TCaptureWorld)
      begin
        Expect(W.Captured).ToContain('hola');
        Expect(W.Captured).ToContain('adios');
      end)

.Rule('El plazo y la cancelacion acotan tambien la lectura de lo capturado')

  .Scenario('El plazo acota la ejecucion aunque un nieto retenga la tuberia')
    .Given('un hijo que escribe hola y deja en segundo plano un nieto que retiene su salida estandar capturada, con un plazo de un segundo',
      procedure(W: TCaptureWorld)
      begin
        W.Proc := W.WithGrandchildProc(1000);
      end)
    .When('se ejecuta',
      procedure(W: TCaptureWorld)
      begin
        var Watch := TStopwatch.StartNew;
        W.Result := W.Proc.Execute;
        W.ElapsedMS := Watch.ElapsedMilliseconds;
      end)
    .&Then('Execute vuelve antes de que termine el nieto, con Timeout',
      procedure(W: TCaptureWorld)
      begin
        Expect(Format('%s antes-del-nieto=%s', [StatusText(W.Result.Status),
          BoolToStr(W.ElapsedMS < GrandchildMS, True)])).ToEqual('Timeout antes-del-nieto=True');
      end)
    .&And('el manejador recibio hola',
      procedure(W: TCaptureWorld)
      begin
        Expect(W.Captured).ToContain('hola');
      end)

  .Scenario('La cancelacion acota la ejecucion aunque un nieto retenga la tuberia')
    .Given('un hijo que escribe hola y deja en segundo plano un nieto que retiene su salida estandar capturada, lanzado con ExecuteAsync y un token',
      procedure(W: TCaptureWorld)
      begin
        W.Source := CreateCancellationTokenSource;
        W.Proc := W.WithGrandchildProc(INFINITE);
        W.Future := W.Proc.ExecuteAsync(W.Source.Token);
      end)
    .When('se cancela el token despues de que el hijo termine',
      procedure(W: TCaptureWorld)
      begin
        var Watch := TStopwatch.StartNew;
        while W.Proc.IsRunning do
          Sleep(20);
        W.Source.Cancel;
        W.Result := W.Future.Value;
        W.ElapsedMS := Watch.ElapsedMilliseconds;
      end)
    .&Then('la tarea termina antes que el nieto, con Canceled',
      procedure(W: TCaptureWorld)
      begin
        Expect(Format('%s antes-del-nieto=%s', [StatusText(W.Result.Status),
          BoolToStr(W.ElapsedMS < GrandchildMS, True)])).ToEqual('Canceled antes-del-nieto=True');
      end)
    .&And('el manejador recibio hola',
      procedure(W: TCaptureWorld)
      begin
        Expect(W.Captured).ToContain('hola');
      end);

end.
