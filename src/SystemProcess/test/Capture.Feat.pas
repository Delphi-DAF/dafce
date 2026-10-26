unit Capture.Feat;

interface

implementation

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
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
  /// La excepcion que lanza el manejador del escenario: ninguna otra pieza la produce.
  ECaptureSpecError = class(Exception);

  TCaptureWorld = class
  public
    Proc: ISystemProcess;
    Result: TProcessResult;
    Future: IFuture<TProcessResult>;
    Source: ICancellationTokenSource;
    Captured: string;
    /// Lo capturado cuando llego la notificacion del final.
    Notified: string;
    /// Las notificaciones del final que corrieron.
    EndNotes: string;
    Calls: Integer;
    Report: string;
    BigFile: string;
    ElapsedMS: Int64;
    destructor Destroy; override;
    function Capture: TProcessOutputProc;
    /// Un manejador que tarda DelayMS en anotar cada lectura.
    function SlowCapture(DelayMS: Cardinal): TProcessOutputProc;
    /// Una notificacion que anota lo capturado hasta ese momento.
    function NoteCaptured: TProcessNotifyProc;
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
  if BigFile <> '' then
    try
      TFile.Delete(BigFile);
    except
      on Exception do;
    end;
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

function TCaptureWorld.SlowCapture(DelayMS: Cardinal): TProcessOutputProc;
begin
  var Inner := Capture();
  Result :=
    procedure(Text: string)
    begin
      Sleep(DelayMS);
      Inner(Text);
    end;
end;

function TCaptureWorld.NoteCaptured: TProcessNotifyProc;
begin
  Result :=
    procedure(R: TProcessResult)
    begin
      TMonitor.Enter(Self);
      try
        Notified := Captured.Trim;
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
      end)

.Rule('Las notificaciones del final llegan despues de la salida capturada')

  .Scenario('OnCompleted ve toda la salida capturada')
    .Given('un hijo que escribe hola y termina, con la salida capturada por un manejador lento, y un OnCompleted que anota lo capturado',
      procedure(W: TCaptureWorld)
      begin
        W.Proc := TSystemProcess.Builder
          .Command('cmd /c echo hola')
          .StandardOutput(TProcessOutput.Capture(W.SlowCapture(1000)))
          .StandardError(TProcessOutput.Discard)
          .HideWindow(True)
          .OnCompleted(W.NoteCaptured())
          .Build;
      end)
    .When('se ejecuta',
      procedure(W: TCaptureWorld)
      begin
        W.Result := W.Proc.Execute;
      end)
    .&Then('lo que anoto OnCompleted es hola',
      procedure(W: TCaptureWorld)
      begin
        Expect(Format('%s anotado=%s', [StatusText(W.Result.Status), W.Notified])).ToEqual('Completed anotado=hola');
      end)

  .Scenario('OnKilled ve toda la salida capturada')
    .Given('un hijo que escribe hola y sigue vivo, con la salida capturada por un manejador mas lento que el plazo, un plazo corto y un OnKilled que anota lo capturado',
      procedure(W: TCaptureWorld)
      begin
        // El manejador tarda mas que el plazo.
        W.Proc := TSystemProcess.Builder
          .Command('cmd /c echo hola & ping -n 10 127.0.0.1 >nul')
          .StandardOutput(TProcessOutput.Capture(W.SlowCapture(4000)))
          .StandardError(TProcessOutput.Discard)
          .HideWindow(True)
          .Timeout(3000)
          .OnKilled(W.NoteCaptured())
          .Build;
      end)
    .When('se ejecuta',
      procedure(W: TCaptureWorld)
      begin
        W.Result := W.Proc.Execute;
      end)
    .&Then('el resultado es Timeout, y lo que anoto OnKilled es hola',
      procedure(W: TCaptureWorld)
      begin
        Expect(Format('%s anotado=%s capturado=%s', [StatusText(W.Result.Status), W.Notified, W.Captured.Trim])).ToEqual('Timeout anotado=hola capturado=hola');
      end)

.Rule('Un manejador de captura que lanza aborta la ejecucion')

  .Scenario('Un manejador que falla aborta la ejecucion sin matar al hijo')
    .Given('un hijo que escribe un megabyte, con la salida capturada por un manejador que lanza en su primera llamada, y un plazo de diez segundos',
      procedure(W: TCaptureWorld)
      begin
        W.BigFile := TPath.Combine(TPath.GetTempPath, 'daf-capture-' + TGUID.NewGuid.ToString.Trim(['{', '}']) + '.txt');
        TFile.WriteAllText(W.BigFile, StringOfChar('x', 1024 * 1024), TEncoding.ANSI);
        var Note :=
          function(const Name: string): TProcessNotifyProc
          begin
            Result :=
              procedure(R: TProcessResult)
              begin
                W.EndNotes := W.EndNotes + Name + ' ';
              end;
          end;
        W.Proc := TSystemProcess.Builder
          .Command('cmd /c type "' + W.BigFile + '"')
          .StandardOutput(TProcessOutput.Capture(
            procedure(Text: string)
            begin
              Inc(W.Calls);
              raise ECaptureSpecError.Create('el manejador falla');
            end))
          .StandardError(TProcessOutput.Discard)
          .HideWindow(True)
          .Timeout(10000)
          .OnCompleted(Note('OnCompleted'))
          .OnKilled(Note('OnKilled'))
          .OnCancelled(Note('OnCancelled'))
          .OnFailed(Note('OnFailed'))
          .Build;
      end)
    .When('se ejecuta',
      procedure(W: TCaptureWorld)
      begin
        var Watch := TStopwatch.StartNew;
        try
          W.Proc.Execute;
          W.Report := 'sin excepcion';
        except
          on E: Exception do
            W.Report := E.ClassName + ': ' + E.Message;
        end;
        W.ElapsedMS := Watch.ElapsedMilliseconds;
      end)
    .&Then('Execute lanza la excepcion del manejador antes de que venza el plazo',
      procedure(W: TCaptureWorld)
      begin
        Expect(Format('%s antes-del-plazo=%s', [W.Report, BoolToStr(W.ElapsedMS < 10000, True)]))
          .ToEqual('ECaptureSpecError: el manejador falla antes-del-plazo=True');
      end)
    .&And('el manejador recibio una sola llamada, y ninguna notificacion del final ha corrido',
      procedure(W: TCaptureWorld)
      begin
        Expect(Format('llamadas=%d avisos=%s', [W.Calls, W.EndNotes.Trim])).ToEqual('llamadas=1 avisos=');
      end)
    .&And('el hijo termina por si mismo, con un codigo de salida distinto de $FFFFFFFF',
      procedure(W: TCaptureWorld)
      begin
        var H := OpenProcess($1000 or SYNCHRONIZE, False, W.Proc.Id);
        var Code: DWORD := 0;
        var Ended := (H <> 0) and (WaitForSingleObject(H, 10000) = WAIT_OBJECT_0);
        if H <> 0 then
        begin
          GetExitCodeProcess(H, Code);
          CloseHandle(H);
        end;
        Expect(Format('termina=%s muerto=%s', [BoolToStr(Ended, True), BoolToStr(Code = $FFFFFFFF, True)]))
          .ToEqual('termina=True muerto=False');
      end);

end.
