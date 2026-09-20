unit Process.Feat;

interface

implementation

uses
  System.SysUtils,
  System.Classes,
  Daf.MiniSpec,
  Daf.SystemProcess;

type
  TProcessWorld = class
  public
    Proc: ISystemProcess;
    Result: TProcessResult;
    /// Un hijo que dura mas que cualquier plazo corto: sirve para que quien corte la ejecucion
    /// sea la causa que el escenario monta y no el propio hijo terminando.
    function Largo(TimeoutMS: Cardinal): ISystemProcess;
  end;

function TProcessWorld.Largo(TimeoutMS: Cardinal): ISystemProcess;
begin
  Result := TSystemProcess.Builder
    .Command('ping')
    .CmdArgs(['-n', '30', '127.0.0.1'])
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
      end);

end.
