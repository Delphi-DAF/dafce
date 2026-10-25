unit Process.Feat;

interface

implementation

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Diagnostics,
  System.Threading,
  Daf.MiniSpec,
  Daf.Threading,
  Daf.SystemProcess;

const
  PROCESS_QUERY_LIMITED_INFORMATION = $1000;

type
  /// La excepcion que lanza el OnIdle del escenario: ninguna otra pieza la produce.
  EIdleSpecError = class(Exception);

  TProcessWorld = class
  public
    Proc: ISystemProcess;
    Result: TProcessResult;
    Future: IFuture<TProcessResult>;
    Source: ICancellationTokenSource;
    /// Un handle del hijo que abre el escenario, para que su proceso siga existiendo al terminar.
    ChildHandle: THandle;
    Report: string;
    Captured: string;
    /// Las notificaciones del final que corrieron.
    EndNotes: string;
    ElapsedMS: Int64;
    destructor Destroy; override;
    /// Un hijo que dura mas que cualquier plazo corto: sirve para que quien corte la ejecucion
    /// sea la causa que el escenario monta y no el propio hijo terminando.
    function Largo(TimeoutMS: Cardinal): ISystemProcess;
    /// Un hijo que dura alrededor de un segundo y sale con el codigo 3.
    function Breve(TimeoutMS: Cardinal; const OnIdle: TProcessNotifyProc): ISystemProcess;
    /// Abre un handle del hijo por su Id, que dura hasta el destructor.
    procedure HoldChild;
    /// Lanza un hijo breve con ExecuteAsync, abre su handle y espera a que termine.
    procedure RunHeldToEnd;
  end;

destructor TProcessWorld.Destroy;
begin
  // Un escenario que falla a medias puede dejar un hijo vivo: se mata y se espera a su tarea.
  if Assigned(Future) and not (Future.Status in [TTaskStatus.Completed, TTaskStatus.Canceled,
    TTaskStatus.Exception]) then
  begin
    try
      if Proc.IsRunning then
        Proc.Kill;
      Future.Wait(30000);
    except
      on Exception do;
    end;
  end;
  try
    if Assigned(Proc) and Proc.IsRunning then
      Proc.Kill;
  except
    on Exception do;
  end;
  if ChildHandle <> 0 then
    CloseHandle(ChildHandle);
  inherited;
end;

function TProcessWorld.Breve(TimeoutMS: Cardinal; const OnIdle: TProcessNotifyProc): ISystemProcess;
begin
  var B := TSystemProcess.Builder
    .Command('cmd /c ping -n 2 127.0.0.1 >nul & exit 3')
    .StandardOutput(TProcessOutput.Discard)
    .StandardError(TProcessOutput.Discard)
    .HideWindow(True)
    .Timeout(TimeoutMS);
  if Assigned(OnIdle) then
    B.OnIdle(OnIdle);
  Result := B.Build;
end;

procedure TProcessWorld.HoldChild;
begin
  ChildHandle := OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION or SYNCHRONIZE, False, Proc.Id);
end;

procedure TProcessWorld.RunHeldToEnd;
begin
  Proc := Breve(INFINITE, nil);
  Future := Proc.ExecuteAsync;
  HoldChild;
  Future.Wait;
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

/// El estado y el codigo de salida de un resultado.
function Outcome(const R: TProcessResult): string;
begin
  Result := Format('%s %d', [StatusText(R.Status), Integer(R.ExitCode)]);
end;

/// El codigo de salida de un proceso por su handle, en hexadecimal.
function ExitCodeOf(Handle: THandle): string;
var
  Code: DWORD;
begin
  if GetExitCodeProcess(Handle, Code) then
    Result := Format('$%.8x', [Code])
  else
    Result := 'sin codigo: ' + SysErrorMessage(GetLastError);
end;

function TProcessWorld.Largo(TimeoutMS: Cardinal): ISystemProcess;
begin
  Result := TSystemProcess.Builder
    .Command('ping')
    .CmdArgs(['-n', '30', '127.0.0.1'])
    .StandardOutput(TProcessOutput.Discard)
    .StandardError(TProcessOutput.Discard)
    .HideWindow(True)
    .Timeout(TimeoutMS)
    .Build;
end;

initialization

Feature('''
Feature El resultado dice por que se corto la ejecucion @systemprocess @process

  Como quien lanza un proceso hijo desde una aplicacion DAF
  Quiero distinguir si acabo solo, si vencio su plazo o si lo mate yo
  Para poder reaccionar distinto a cada una de las tres
''')

.UseWorld<TProcessWorld>

.Rule('Las tres formas de acabar tienen valor propio')

  .Scenario('Vencer el plazo se informa como Timeout')
    .Given('un proceso largo con un plazo corto',
      procedure(W: TProcessWorld)
      begin
        W.Proc := W.Largo(400);
      end)
    .When('se ejecuta',
      procedure(W: TProcessWorld)
      begin
        W.Result := W.Proc.Execute;
      end)
    .&Then('el resultado dice que vencio el plazo',
      procedure(W: TProcessWorld)
      begin
        Expect(W.Result.Status = TProcessResult.TStatus.Timeout).ToBeTrue;
      end)

  .Scenario('Un Kill explicito se informa como Killed, no como Timeout')
    .Given('un proceso largo con plazo de sobra y un hilo que lo matara',
      procedure(W: TProcessWorld)
      begin
        // El plazo es amplio a proposito: si el resultado dijera Timeout seria porque la causa
        // se informa mal, no porque el plazo venciera.
        W.Proc := W.Largo(60000);
        var P := W.Proc;
        TThread.CreateAnonymousThread(
          procedure
          begin
            // Se mata con el hijo ya lanzado: un Kill sin hijo lanza.
            while not P.IsRunning do
              Sleep(10);
            Sleep(400);
            P.Kill;
          end).Start;
      end)
    .When('se ejecuta',
      procedure(W: TProcessWorld)
      begin
        W.Result := W.Proc.Execute;
      end)
    .&Then('el resultado dice que lo mataron',
      procedure(W: TProcessWorld)
      begin
        Expect(W.Result.Status = TProcessResult.TStatus.Killed).ToBeTrue;
      end)

  .Scenario('Un hijo que termina solo se informa como Completed')
    .Given('un proceso corto sin plazo',
      procedure(W: TProcessWorld)
      begin
        // El control de los dos anteriores: sin el, una implementacion que informara siempre lo
        // mismo pasaria igual de verde.
        W.Proc := TSystemProcess.Builder
          .Command('ping')
          .CmdArgs(['-n', '1', '127.0.0.1'])
          .StandardOutput(TProcessOutput.Discard)
          .StandardError(TProcessOutput.Discard)
          .HideWindow(True)
          .Build;
      end)
    .When('se ejecuta',
      procedure(W: TProcessWorld)
      begin
        W.Result := W.Proc.Execute;
      end)
    .&Then('el resultado dice que completo',
      procedure(W: TProcessWorld)
      begin
        Expect(W.Result.Status = TProcessResult.TStatus.Completed).ToBeTrue;
      end)

.Rule('Un hijo que ya termino no se mata')

  .Scenario('Una cancelacion que llega con el hijo ya terminado')
    .Given('un hijo que termina solo con el codigo 3, y un token que se cancela despues de que el hijo termine y antes de que la ejecucion lo note',
      procedure(W: TProcessWorld)
      begin
        W.Source := CreateCancellationTokenSource;
        var Armed := True;
        W.Proc := W.Breve(INFINITE,
          procedure(R: TProcessResult)
          begin
            if not Armed then
              Exit;
            Armed := False;
            W.HoldChild;
            WaitForSingleObject(W.ChildHandle, 10000);
            W.Source.Cancel;
          end);
      end)
    .When('se ejecuta con ese token',
      procedure(W: TProcessWorld)
      begin
        try
          W.Report := Outcome(W.Proc.Execute(W.Source.Token));
        except
          on E: Exception do
            W.Report := E.ClassName;
        end;
      end)
    .&Then('el resultado es Completed, con el codigo de salida 3',
      procedure(W: TProcessWorld)
      begin
        Expect(W.Report).ToEqual('Completed 3');
      end)

  .Scenario('Un plazo que vence con el hijo ya terminado')
    .Given('un hijo que termina solo con el codigo 3, y un plazo que vence despues de que el hijo termine y antes de que la ejecucion lo note',
      procedure(W: TProcessWorld)
      begin
        // 600 ms: mas que la primera espera de 250 ms, y menos que el segundo que dura el hijo.
        var Armed := True;
        W.Proc := W.Breve(600,
          procedure(R: TProcessResult)
          begin
            if not Armed then
              Exit;
            Armed := False;
            W.HoldChild;
            WaitForSingleObject(W.ChildHandle, 10000);
          end);
      end)
    .When('se ejecuta',
      procedure(W: TProcessWorld)
      begin
        try
          W.Report := Outcome(W.Proc.Execute);
        except
          on E: Exception do
            W.Report := E.ClassName;
        end;
      end)
    .&Then('el resultado es Completed, con el codigo de salida 3',
      procedure(W: TProcessWorld)
      begin
        Expect(W.Report).ToEqual('Completed 3');
      end)

.Rule('KillById sobre un proceso que ya termino no lanza')

  .Scenario('Matar por identificador a un proceso terminado')
    .Given('un hijo que ya termino, cuyo proceso sigue existiendo porque el escenario mantiene abierto un handle suyo',
      procedure(W: TProcessWorld)
      begin
        W.RunHeldToEnd;
      end)
    .When('se llama a TProcess.KillById con su identificador',
      procedure(W: TProcessWorld)
      begin
        try
          W.Report := 'devuelve ' + BoolToStr(TProcess.KillById(W.Proc.Id), True);
        except
          on E: Exception do
            W.Report := E.ClassName;
        end;
      end)
    .&Then('devuelve False, y no lanza',
      procedure(W: TProcessWorld)
      begin
        Expect(W.Report).ToEqual('devuelve False');
      end)

.Rule('Kill actua sobre el hijo que lanzo la instancia')

  .Scenario('Kill sin un hijo lanzado lanza')
    .Given('un proceso construido que no se ha ejecutado',
      procedure(W: TProcessWorld)
      begin
        W.Proc := W.Largo(60000);
      end)
    .When('se llama a Kill',
      procedure(W: TProcessWorld)
      begin
        try
          W.Proc.Kill;
          W.Report := 'sin excepcion';
        except
          on E: Exception do
            W.Report := E.ClassName + ': ' + E.Message;
        end;
      end)
    .&Then('se lanza EInvalidOpException con el mensaje de .NET',
      procedure(W: TProcessWorld)
      begin
        Expect(W.Report).ToEqual('EInvalidOpException: No process is associated with this object.');
      end)

  .Scenario('Un Kill rechazado no corta la ejecucion')
    .Given('un proceso construido sobre el que Kill lanzo por no tener hijo',
      procedure(W: TProcessWorld)
      begin
        W.Proc := TSystemProcess.Builder
          .Command('ping')
          .CmdArgs(['-n', '1', '127.0.0.1'])
          .StandardOutput(TProcessOutput.Discard)
          .StandardError(TProcessOutput.Discard)
          .HideWindow(True)
          .Build;
        try
          W.Proc.Kill;
        except
          on Exception do;
        end;
      end)
    .When('se ejecuta',
      procedure(W: TProcessWorld)
      begin
        W.Report := StatusText(W.Proc.Execute.Status);
      end)
    .&Then('el resultado es Completed',
      procedure(W: TProcessWorld)
      begin
        Expect(W.Report).ToEqual('Completed');
      end)

  .Scenario('Un Kill no corta la ejecucion siguiente')
    .Given('un proceso cuya primera ejecucion termino como Killed por un Kill',
      procedure(W: TProcessWorld)
      begin
        W.Proc := TSystemProcess.Builder
          .Command('ping')
          .CmdArgs(['-n', '3', '127.0.0.1'])
          .StandardOutput(TProcessOutput.Discard)
          .StandardError(TProcessOutput.Discard)
          .HideWindow(True)
          .Timeout(60000)
          .Build;
        W.Future := W.Proc.ExecuteAsync;
        W.Proc.Kill;
        W.Report := 'primera=' + StatusText(W.Future.Value.Status);
      end)
    .When('se ejecuta otra vez',
      procedure(W: TProcessWorld)
      begin
        W.Report := W.Report + ' segunda=' + StatusText(W.Proc.Execute.Status);
      end)
    .&Then('el resultado es Completed',
      procedure(W: TProcessWorld)
      begin
        Expect(W.Report).ToEqual('primera=Killed segunda=Completed');
      end)

  .Scenario('Un Kill despues de que el hijo termine no lanza')
    .Given('un proceso cuyo hijo ya termino, y cuyo proceso sigue existiendo porque el escenario mantiene abierto un handle suyo',
      procedure(W: TProcessWorld)
      begin
        W.RunHeldToEnd;
      end)
    .When('se llama a Kill',
      procedure(W: TProcessWorld)
      begin
        try
          W.Proc.Kill;
          W.Report := 'sin excepcion';
        except
          on E: Exception do
            W.Report := E.ClassName;
        end;
      end)
    .&Then('no lanza',
      procedure(W: TProcessWorld)
      begin
        Expect(W.Report).ToEqual('sin excepcion');
      end)

.Rule('La instancia retiene a su hijo hasta el siguiente lanzamiento')

  .Scenario('El proceso del hijo sigue abierto despues de la ejecucion')
    .Given('un proceso cuyo hijo ya termino',
      procedure(W: TProcessWorld)
      begin
        W.Proc := W.Breve(INFINITE, nil);
        W.Result := W.Proc.Execute;
      end)
    .When('se abre el proceso por el Id del hijo, sin otro handle abierto que el de la instancia',
      procedure(W: TProcessWorld)
      begin
        W.ChildHandle := OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, False, W.Proc.Id);
        if W.ChildHandle = 0 then
          W.Report := 'no se abre: ' + SysErrorMessage(GetLastError)
        else
          W.Report := Format('codigo %s, resultado $%.8x', [ExitCodeOf(W.ChildHandle), W.Result.ExitCode]);
      end)
    .&Then('se abre, y su codigo de salida es el que dio el resultado',
      procedure(W: TProcessWorld)
      begin
        Expect(W.Report).ToEqual('codigo $00000003, resultado $00000003');
      end)

.Rule('Matar deja al hijo con el codigo de salida -1')

  .Scenario('Un hijo muerto por Kill sale con -1')
    .Given('un hijo largo lanzado con ExecuteAsync, y un handle suyo abierto por el escenario',
      procedure(W: TProcessWorld)
      begin
        W.Proc := W.Largo(60000);
        W.Future := W.Proc.ExecuteAsync;
        W.HoldChild;
      end)
    .When('se llama a Kill y se espera a la tarea',
      procedure(W: TProcessWorld)
      begin
        W.Proc.Kill;
        W.Future.Wait;
        W.Report := ExitCodeOf(W.ChildHandle);
      end)
    .&Then('el codigo de salida del proceso del hijo es $FFFFFFFF',
      procedure(W: TProcessWorld)
      begin
        Expect(W.Report).ToEqual('$FFFFFFFF');
      end)

.Rule('El resultado lleva el codigo de salida real del hijo')

  .Scenario('Un hijo muerto por el plazo da -1 en el resultado')
    .Given('un hijo largo con un plazo corto',
      procedure(W: TProcessWorld)
      begin
        W.Proc := W.Largo(400);
      end)
    .When('se ejecuta',
      procedure(W: TProcessWorld)
      begin
        W.Result := W.Proc.Execute;
        W.Report := Format('%s $%.8x', [StatusText(W.Result.Status), W.Result.ExitCode]);
      end)
    .&Then('el resultado es Timeout, con el codigo de salida $FFFFFFFF',
      procedure(W: TProcessWorld)
      begin
        Expect(W.Report).ToEqual('Timeout $FFFFFFFF');
      end)

  .Scenario('Un hijo muerto por Kill da -1 en el resultado')
    .Given('un hijo largo lanzado con ExecuteAsync',
      procedure(W: TProcessWorld)
      begin
        W.Proc := W.Largo(60000);
        W.Future := W.Proc.ExecuteAsync;
      end)
    .When('se llama a Kill y se espera a la tarea',
      procedure(W: TProcessWorld)
      begin
        W.Proc.Kill;
        W.Result := W.Future.Value;
        W.Report := Format('%s $%.8x', [StatusText(W.Result.Status), W.Result.ExitCode]);
      end)
    .&Then('el resultado es Killed, con el codigo de salida $FFFFFFFF',
      procedure(W: TProcessWorld)
      begin
        Expect(W.Report).ToEqual('Killed $FFFFFFFF');
      end)

.Rule('Un OnIdle que lanza no retiene la ejecucion')

  .Scenario('Un OnIdle que falla no espera al hijo')
    .Given('un hijo que escribe hola y vive varios segundos, con la salida estandar capturada y un OnIdle que lanza en su primera llamada',
      procedure(W: TProcessWorld)
      begin
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
          .Command('cmd /c echo hola & ping -n 10 127.0.0.1 >nul')
          .StandardOutput(TProcessOutput.Capture(
            procedure(Text: string)
            begin
              W.Captured := W.Captured + Text;
            end))
          .StandardError(TProcessOutput.Discard)
          .HideWindow(True)
          .Timeout(60000)
          .OnIdle(
            procedure(R: TProcessResult)
            begin
              raise EIdleSpecError.Create('OnIdle falla');
            end)
          .OnCompleted(Note('OnCompleted'))
          .OnKilled(Note('OnKilled'))
          .OnCancelled(Note('OnCancelled'))
          .OnFailed(Note('OnFailed'))
          .Build;
      end)
    .When('se ejecuta',
      procedure(W: TProcessWorld)
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
    .&Then('Execute lanza la excepcion de OnIdle antes de que el hijo termine',
      procedure(W: TProcessWorld)
      begin
        Expect(Format('%s antes-del-hijo=%s', [W.Report, BoolToStr(W.ElapsedMS < 5000, True)]))
          .ToEqual('EIdleSpecError: OnIdle falla antes-del-hijo=True');
      end)
    .&And('ninguna notificacion del final ha corrido, e IsRunning es cierto',
      procedure(W: TProcessWorld)
      begin
        Expect(Format('avisos=%s corre=%s', [W.EndNotes.Trim, BoolToStr(W.Proc.IsRunning, True)]))
          .ToEqual('avisos= corre=True');
      end);

end.
