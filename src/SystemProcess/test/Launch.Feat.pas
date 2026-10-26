unit Launch.Feat;

interface

implementation

uses
  System.SysUtils,
  System.Classes,
  System.Threading,
  System.SyncObjs,
  Daf.MiniSpec,
  Daf.Threading,
  Daf.SystemProcess;

const
  MissingCommand = 'daf-comando-que-no-existe.exe';

type
  TLaunchWorld = class
  public
    Proc: ISystemProcess;
    Builder: TBuilder;
    Done: TEvent;
    Result: TProcessResult;
    Future: IFuture<TProcessResult>;
    Source: ICancellationTokenSource;
    Captured: string;
    Report: string;
    Threads: string;
    Pid: Cardinal;
    Notified: string;
    CallerThread: TThreadID;
    FailedThread: TThreadID;
    destructor Destroy; override;
    /// Un hijo que tarda Seconds segundos y no escribe nada.
    function Slow(Seconds: Integer): ISystemProcess;
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

/// El Id como texto, o la excepcion que lanza al leerlo.
function IdOf(const Proc: ISystemProcess): string;
begin
  try
    Result := Proc.Id.ToString;
  except
    on E: Exception do
      Result := E.ClassName + ': ' + E.Message;
  end;
end;

destructor TLaunchWorld.Destroy;
begin
  // Un escenario que falla a medias puede dejar un hijo vivo: se mata y se espera a su tarea, para que
  // no llegue al siguiente.
  if Assigned(Future) and not (Future.Status in [TTaskStatus.Completed, TTaskStatus.Canceled,
    TTaskStatus.Exception]) then
  begin
    try
      if Assigned(Proc) and Proc.IsRunning then
        Proc.Kill;
      Future.Wait(30000);
    except
      on Exception do;
    end;
  end;
  Done.Free;
  inherited;
end;

/// Un builder con un hijo que tarda Seconds segundos, y un OnCompleted que anota el estado en Report
/// y avisa por Done.
function Notifying(W: TLaunchWorld; Seconds: Integer): TBuilder;
begin
  W.Done := TEvent.Create(nil, True, False, '');
  Result := TSystemProcess.Builder
    .Command('ping')
    .CmdArgs(['-n', IntToStr(Seconds + 1), '127.0.0.1'])
    .StandardOutput(TProcessOutput.Discard)
    .StandardError(TProcessOutput.Discard)
    .HideWindow(True)
    .Timeout(60000)
    .OnCompleted(
      procedure(R: TProcessResult)
      begin
        W.Report := StatusText(R.Status);
        W.Done.SetEvent;
      end);
end;

/// Lanza con ExecuteAsync un hijo que tarda un momento y devuelve solo la tarea: la referencia al
/// proceso muere al volver. OnCompleted anota el estado en Notified y avisa por Done.
function LaunchUnheld(W: TLaunchWorld): IFuture<TProcessResult>;
begin
  W.Done := TEvent.Create(nil, True, False, '');
  Result := TSystemProcess.Builder
    .Command('ping')
    .CmdArgs(['-n', '2', '127.0.0.1'])
    .StandardOutput(TProcessOutput.Discard)
    .StandardError(TProcessOutput.Discard)
    .HideWindow(True)
    .Timeout(60000)
    .OnCompleted(
      procedure(R: TProcessResult)
      begin
        W.Notified := StatusText(R.Status);
        W.Done.SetEvent;
      end)
    .Build
    .ExecuteAsync;
end;

function TLaunchWorld.Slow(Seconds: Integer): ISystemProcess;
begin
  Result := TSystemProcess.Builder
    .Command('ping')
    .CmdArgs(['-n', IntToStr(Seconds + 1), '127.0.0.1'])
    .StandardOutput(TProcessOutput.Discard)
    .StandardError(TProcessOutput.Discard)
    .HideWindow(True)
    .Timeout(60000)
    .Build;
end;

initialization

Feature('''
Feature Lo que se sabe de un hijo desde que se lanza @systemprocess @launch

  Como quien lanza un proceso hijo desde una aplicacion DAF
  Quiero saber su identificador en cuanto queda lanzado
  Para apuntarlo, encontrar lo que escribe con el en el nombre o matarlo desde otro sitio
''')

.UseWorld<TLaunchWorld>

.Rule('El proceso expone el identificador del hijo')

  .Scenario('El identificador es el del hijo')
    .Given('un hijo que escribe su propio identificador de proceso',
      procedure(W: TLaunchWorld)
      begin
        W.Proc := TSystemProcess.Builder
          .Command('powershell -NoProfile -Command $PID')
          .StandardOutput(TProcessOutput.Capture(
            procedure(Text: string)
            begin
              W.Captured := W.Captured + Text;
            end))
          .StandardError(TProcessOutput.Discard)
          .HideWindow(True)
          .Timeout(60000)
          .Build;
      end)
    .When('se ejecuta y termina',
      procedure(W: TLaunchWorld)
      begin
        W.Result := W.Proc.Execute;
        W.Report := Format('%s %s', [StatusText(W.Result.Status), IdOf(W.Proc)]);
      end)
    .&Then('Id vale lo que escribio el hijo',
      procedure(W: TLaunchWorld)
      begin
        Expect(W.Report).ToEqual('Completed ' + W.Captured.Trim);
      end)

  .Scenario('Sin ejecutar no hay identificador')
    .Given('un proceso construido que no se ha ejecutado',
      procedure(W: TLaunchWorld)
      begin
        W.Proc := W.Slow(1);
      end)
    .When('se lee Id',
      procedure(W: TLaunchWorld)
      begin
        W.Report := IdOf(W.Proc);
      end)
    .&Then('se lanza EInvalidOpException',
      procedure(W: TLaunchWorld)
      begin
        Expect(W.Report).ToEqual('EInvalidOpException: No process is associated with this object.');
      end)

  .Scenario('Un lanzamiento que falla no deja identificador')
    .Given('un proceso con un comando que no existe',
      procedure(W: TLaunchWorld)
      begin
        W.Proc := TSystemProcess.Builder
          .Command(MissingCommand)
          .HideWindow(True)
          .Build;
      end)
    .When('se ejecuta',
      procedure(W: TLaunchWorld)
      begin
        W.Result := W.Proc.Execute;
        W.Report := Format('%s %s', [StatusText(W.Result.Status), IdOf(W.Proc)]);
      end)
    .&Then('el resultado es Failed, y leer Id lanza EInvalidOpException',
      procedure(W: TLaunchWorld)
      begin
        Expect(W.Report).ToEqual('Failed EInvalidOpException: No process is associated with this object.');
      end)

.Rule('ExecuteAsync vuelve con el hijo ya lanzado')

  .Scenario('Al volver, el hijo corre')
    .Given('un hijo que tarda varios segundos en terminar',
      procedure(W: TLaunchWorld)
      begin
        W.Proc := W.Slow(2);
      end)
    .When('ExecuteAsync vuelve',
      procedure(W: TLaunchWorld)
      begin
        W.Future := W.Proc.ExecuteAsync;
        var IdText := IdOf(W.Proc);
        var Pid := StrToIntDef(IdText, 0);
        W.Report := Format('responde=%s vivo=%s corre=%s', [BoolToStr(Pid <> 0, True),
          BoolToStr((Pid <> 0) and not TProcess.HasExited(Cardinal(Pid)), True),
          BoolToStr(W.Proc.IsRunning, True)]);
      end)
    .&Then('Id responde con el identificador del hijo, e IsRunning es cierto',
      procedure(W: TLaunchWorld)
      begin
        Expect(W.Report).ToEqual('responde=True vivo=True corre=True');
      end)
    .&And('la tarea termina con Completed cuando termina el hijo',
      procedure(W: TLaunchWorld)
      begin
        Expect(StatusText(W.Future.Value.Status)).ToEqual('Completed');
      end)

  .Scenario('Un lanzamiento que falla se resuelve sin esperar')
    .Given('un proceso con un comando que no existe, y un OnFailed que anota en que hilo corre',
      procedure(W: TLaunchWorld)
      begin
        W.Proc := TSystemProcess.Builder
          .Command(MissingCommand)
          .HideWindow(True)
          .OnFailed(
            procedure(R: TProcessResult)
            begin
              W.FailedThread := TThread.CurrentThread.ThreadID;
            end)
          .Build;
      end)
    .When('se llama a ExecuteAsync',
      procedure(W: TLaunchWorld)
      begin
        W.CallerThread := TThread.CurrentThread.ThreadID;
        W.Future := W.Proc.ExecuteAsync;
        // El estado antes que el valor: leer Value espera a que la tarea termine.
        var Settled := W.Future.Status = TTaskStatus.Completed;
        W.Report := Format('resuelta=%s %s', [BoolToStr(Settled, True), StatusText(W.Future.Value.Status)]);
        W.Threads := Format('mismo-hilo=%s', [BoolToStr(W.FailedThread = W.CallerThread, True)]);
      end)
    .&Then('la tarea que devuelve ya esta resuelta, con Failed',
      procedure(W: TLaunchWorld)
      begin
        Expect(W.Report).ToEqual('resuelta=True Failed');
      end)
    .&And('OnFailed corrio en el hilo de quien llamo',
      procedure(W: TLaunchWorld)
      begin
        Expect(W.Threads).ToEqual('mismo-hilo=True');
      end)

  .Scenario('Cancelar despues de volver sigue matando al hijo')
    .Given('un hijo que tarda varios segundos, lanzado con ExecuteAsync y un token de cancelacion',
      procedure(W: TLaunchWorld)
      begin
        W.Source := CreateCancellationTokenSource;
        W.Proc := W.Slow(30);
        W.Future := W.Proc.ExecuteAsync(W.Source.Token);
        W.Pid := W.Proc.Id;
      end)
    .When('se cancela el token despues de que ExecuteAsync vuelva',
      procedure(W: TLaunchWorld)
      begin
        W.Source.Cancel;
        W.Report := Format('%s corre=%s', [StatusText(W.Future.Value.Status),
          BoolToStr(not TProcess.HasExited(W.Pid), True)]);
      end)
    .&Then('la tarea termina con Canceled, y el hijo ya no corre',
      procedure(W: TLaunchWorld)
      begin
        Expect(W.Report).ToEqual('Canceled corre=False');
      end)

.Rule('ExecuteAsync sostiene la instancia hasta que el hijo termina')

  .Scenario('Un proceso que nadie sostiene termina igual')
    .Given('un hijo que tarda un momento en terminar, y un OnCompleted que anota el resultado',
      procedure(W: TLaunchWorld)
      begin
        W.Notified := '';
      end)
    .When('se lanza con ExecuteAsync, se suelta el ISystemProcess y se espera a la tarea',
      procedure(W: TLaunchWorld)
      begin
        W.Future := LaunchUnheld(W);
        try
          W.Report := StatusText(W.Future.Value.Status);
        except
          on E: Exception do
            W.Report := E.ClassName + ': ' + E.Message;
        end;
        if W.Done.WaitFor(5000) <> wrSignaled then
          W.Notified := 'sin OnCompleted';
      end)
    .&Then('la tarea termina con Completed, y OnCompleted recibe Completed',
      procedure(W: TLaunchWorld)
      begin
        Expect(Format('tarea=%s aviso=%s', [W.Report, W.Notified])).ToEqual('tarea=Completed aviso=Completed');
      end)

.Rule('Lanzar y liberar en segundo plano espera al hijo')

  // Que el runner no informe de fugas es la otra mitad de los dos escenarios que siguen: liberar la
  // instancia y el builder no se ve desde dentro, y el informe lo dice al apagarse.
  .Scenario('El hijo termina y la instancia se libera despues')
    .Given('un builder con un hijo que tarda un momento y un OnCompleted que anota el resultado',
      procedure(W: TLaunchWorld)
      begin
        W.Builder := Notifying(W, 1);
      end)
    .When('se lanza con ExecuteAsyncAndFree y se espera a que termine',
      procedure(W: TLaunchWorld)
      begin
        W.Builder.ExecuteAsyncAndFree;
        W.Builder := nil;
        if W.Done.WaitFor(15000) <> wrSignaled then
          W.Report := 'sin OnCompleted';
      end)
    .&Then('OnCompleted recibe Completed',
      procedure(W: TLaunchWorld)
      begin
        Expect(W.Report).ToEqual('Completed');
      end)

.Rule('Ejecutar y liberar no deja el builder vivo')

  .Scenario('Ejecutar y liberar no pierde el builder')
    .Given('un builder con un hijo corto y un OnCompleted que anota el resultado',
      procedure(W: TLaunchWorld)
      begin
        W.Builder := Notifying(W, 0);
      end)
    .When('se lanza con ExecuteAndFree',
      procedure(W: TLaunchWorld)
      begin
        W.Builder.ExecuteAndFree;
        W.Builder := nil;
      end)
    .&Then('OnCompleted recibe Completed',
      procedure(W: TLaunchWorld)
      begin
        Expect(W.Report).ToEqual('Completed');
      end);

end.
