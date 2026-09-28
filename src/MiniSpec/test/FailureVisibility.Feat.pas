unit FailureVisibility.Feat;

// Lo que fijan estas specs es que un fallo anterior al primer step de un escenario se vea: que se
// le atribuya a ese escenario, que cuente y que salga en el informe. La ejecucion anidada es lo que
// las hace escribibles, porque un escenario que falla a proposito tiene que fallar SIN poner roja
// la suite que lo esta comprobando.

interface

implementation

uses
  System.SysUtils,
  System.Classes,
  Daf.MiniSpec,
  Daf.MiniSpec.Types,
  Daf.MiniSpec.Runner,
  Daf.MiniSpec.Injection;

type
  /// Nadie lo registra nunca: es lo que hace insatisfacible la propiedad de abajo.
  TNeverRegistered = class end;

  TNeedsMissing = class
  strict private
    FMissing: TNeverRegistered;
  public
    [MiniInject]
    property Missing: TNeverRegistered read FMissing write FMissing;
  end;

  /// Se niega a construirse la primera vez y se deja construir despues. Es lo que separa «el
  /// escenario roto falla» de «el escenario roto se lleva por delante a los que vienen detras»:
  /// con un World que falla siempre, los dos escenarios saldrian rojos y no se sabria cual es cual.
  TFlakyWorld = class
  public
    class var Attempts: Integer;
    constructor Create;
  end;

  TPlainWorld = class end;

  /// Cuenta lo que se ejecuta detras de un Background: sus propias pasadas, y los steps de los
  /// escenarios que prepara. Es de clase porque cada escenario tiene su World.
  TBackgroundWorld = class
  public
    // N antes de los class var: un campo declarado detras de un class var tambien es de clase.
    N: Integer;
    class var Runs: Integer;
    class var ScenarioSteps: Integer;
  end;

  /// Anota lo que el informe de una ejecucion emite. Es el unico modo de afirmar que algo «sale en
  /// el informe»: los recuentos dicen cuantos, no quienes ni con que mensaje.
  TRecordingListener = class(TCustomListener)
  strict private
    FLines: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure OnItem(const Context: IRunContext; const Item: ISpecItem); override;
    function Text: string;
  end;

  TVisibilityWorld = class
  strict private
    FRun: TSpecRun;
    FRecorder: TRecordingListener;
  public
    Report: string;
    DiscardedShown: string;
    StepsRan: string;
    BackgroundRest: string;
    constructor Create;
    destructor Destroy; override;
    property Run: TSpecRun read FRun;
    /// El listener lo mantiene vivo la ejecucion, que guarda su interfaz. Esta referencia es para
    /// leerlo, no para poseerlo: liberarla aqui seria una doble liberacion.
    property Recorder: TRecordingListener read FRecorder;
  end;

constructor TFlakyWorld.Create;
begin
  inherited;
  Inc(Attempts);
  if Attempts = 1 then
    raise Exception.Create('this World refuses to be built the first time');
end;

constructor TRecordingListener.Create;
begin
  inherited Create;
  FLines := TStringList.Create;
end;

destructor TRecordingListener.Destroy;
begin
  FLines.Free;
  inherited;
end;

procedure TRecordingListener.OnItem(const Context: IRunContext; const Item: ISpecItem);
begin
  FLines.Add(Format('%s: %s [%s]', [Item.KeyWord, Item.Description, Item.RunInfo.ErrMsg]));
end;

function TRecordingListener.Text: string;
begin
  Result := FLines.Text;
end;

constructor TVisibilityWorld.Create;
begin
  inherited;
  FRun := TSpecRun.Create('Nested');
  FRecorder := TRecordingListener.Create;
  FRun.AddListener(FRecorder);
end;

destructor TVisibilityWorld.Destroy;
begin
  FRun.Free;
  inherited;
end;

initialization

Feature('''
Failure visibility @failure-visibility

  As whoever reads the result of a suite
  I want a failure before the first step of a scenario to be seen
  So a feature that blew up cannot fall out of the report leaving the run green
''')

.UseWorld<TVisibilityWorld>

.Rule('A failure while preparing a scenario is a failure of that scenario')

  .Scenario('The World of a scenario cannot be built')
    .Given('a run whose only scenario declares a dependency nothing satisfies', procedure(W: TVisibilityWorld)
      begin
        Feature('Needs something nobody registered', W.Run.Suite)
          .UseWorld<TNeedsMissing>
          .Scenario('It never gets to run')
            .&Then('this step is never reached', procedure(N: TNeedsMissing)
              begin
                Expect('the World was built').ToEqual('it should not have been');
              end);
      end)
    .When('the suite is executed', procedure(W: TVisibilityWorld)
      begin
        W.Run.Execute;
        W.Report := W.Recorder.Text;
      end)
    .&Then('that scenario is in the report marked as failed, with the message of the error', procedure(W: TVisibilityWorld)
      begin
        Expect(W.Report).ToContain('It never gets to run');
        Expect(W.Report).ToContain('Missing');
        Expect(W.Report).ToContain('TNeverRegistered');
      end)
    .&And('it counts as one failed spec', procedure(W: TVisibilityWorld)
      begin
        Expect(Integer(W.Run.FailCount)).ToEqual(1);
      end)
    .&And('the exit code of that run is 1', procedure(W: TVisibilityWorld)
      begin
        Expect(W.Run.ExitCode).ToEqual(1);
      end)

  .Scenario('One broken scenario and one sound one in the same rule')
    .Given('a rule with two scenarios where the first cannot build its World', procedure(W: TVisibilityWorld)
      begin
        TFlakyWorld.Attempts := 0;
        Feature('Two scenarios, one of them broken', W.Run.Suite)
          .UseWorld<TFlakyWorld>
          .Rule('Both of them')
            .Scenario('The first one, whose World refuses')
              .&Then('this step is never reached', procedure(F: TFlakyWorld)
                begin
                  Expect('the World was built').ToEqual('it should not have been');
                end)
            .Scenario('The second one, which is sound')
              .&Then('it runs and passes', procedure(F: TFlakyWorld)
                begin
                  Expect(1).ToEqual(1);
                end);
      end)
    .When('the suite is executed', procedure(W: TVisibilityWorld)
      begin
        W.Run.Execute;
      end)
    .&Then('the first one counts as failed', procedure(W: TVisibilityWorld)
      begin
        Expect(Integer(W.Run.FailCount)).ToEqual(1);
      end)
    .&And('the second one runs and its result is its own', procedure(W: TVisibilityWorld)
      begin
        Expect(Integer(W.Run.PassCount)).ToEqual(1);
      end)

  .Scenario('A step of the Background fails')
    .Given('a run whose only scenario is prepared by a Background that fails', procedure(W: TVisibilityWorld)
      begin
        TBackgroundWorld.ScenarioSteps := 0;
        TBackgroundWorld.Runs := 0;
        Feature('Its Background fails', W.Run.Suite)
          .UseWorld<TBackgroundWorld>
          .Background
            .Given('the environment is ready', procedure(B: TBackgroundWorld)
              begin
                Expect('broken').ToEqual('ready');
              end)
            .&And('the rest of the environment is set up on top', procedure(B: TBackgroundWorld)
              begin
                Inc(TBackgroundWorld.Runs);
              end)
          .Scenario('It depends on the environment')
            .When('it uses the environment', procedure(B: TBackgroundWorld)
              begin
                Inc(TBackgroundWorld.ScenarioSteps);
              end)
            .&Then('it checks what it got', procedure(B: TBackgroundWorld)
              begin
                Inc(TBackgroundWorld.ScenarioSteps);
              end);
      end)
    .When('the suite is executed', procedure(W: TVisibilityWorld)
      begin
        W.Run.Execute;
        W.Report := W.Recorder.Text;
        W.StepsRan := Format('%d steps of the scenario ran', [TBackgroundWorld.ScenarioSteps]);
        W.BackgroundRest := Format('%d steps of the Background ran after the failure', [TBackgroundWorld.Runs]);
      end)
    .&Then('that scenario counts as failed, and none as passed', procedure(W: TVisibilityWorld)
      begin
        Expect(Format('%d failed, %d passed', [W.Run.FailCount, W.Run.PassCount])).ToEqual('1 failed, 0 passed');
      end)
    .&And('its own steps are not run', procedure(W: TVisibilityWorld)
      begin
        Expect(W.StepsRan).ToEqual('0 steps of the scenario ran');
      end)
    .&And('nor the steps of the Background after the one that failed', procedure(W: TVisibilityWorld)
      begin
        Expect(W.BackgroundRest).ToEqual('0 steps of the Background ran after the failure');
      end)
    .&And('the report says which step of the Background failed and why', procedure(W: TVisibilityWorld)
      begin
        Expect(W.Report).ToContain('Background: Given the environment is ready: Expected ready but got broken');
      end)
    .&And('the exit code of that run is 1', procedure(W: TVisibilityWorld)
      begin
        Expect(W.Run.ExitCode).ToEqual(1);
      end)

  .Scenario('A Background that fails for one scenario fails only that one')
    // Cada escenario ejecuta su Background con su World: un fallo no se hereda al siguiente.
    .Given('a rule with two scenarios whose Background fails only the first time', procedure(W: TVisibilityWorld)
      begin
        TBackgroundWorld.Runs := 0;
        Feature('Its Background fails once', W.Run.Suite)
          .UseWorld<TBackgroundWorld>
          .Background
            .Given('the environment is ready from the second time on', procedure(B: TBackgroundWorld)
              begin
                Inc(TBackgroundWorld.Runs);
                if TBackgroundWorld.Runs = 1 then
                  Expect('broken').ToEqual('ready');
              end)
          .Scenario('The first one')
            .&Then('it passes by itself', procedure(B: TBackgroundWorld) begin end)
          .Scenario('The second one')
            .&Then('it passes by itself', procedure(B: TBackgroundWorld) begin end);
      end)
    .When('the suite is executed', procedure(W: TVisibilityWorld)
      begin
        W.Run.Execute;
      end)
    .&Then('the first one fails and the second one passes', procedure(W: TVisibilityWorld)
      begin
        Expect(Format('%d failed, %d passed', [W.Run.FailCount, W.Run.PassCount])).ToEqual('1 failed, 1 passed');
      end)

  .Scenario('A Background that fails before the rows of an outline')
    .Given('an outline of two rows prepared by a Background that fails', procedure(W: TVisibilityWorld)
      begin
        TBackgroundWorld.ScenarioSteps := 0;
        Feature('Its Background fails under an outline', W.Run.Suite)
          .UseWorld<TBackgroundWorld>
          .Background
            .Given('the environment is ready', procedure(B: TBackgroundWorld)
              begin
                Expect('broken').ToEqual('ready');
              end)
          .ScenarioOutline('Row <N>')
            .&Then('it runs', procedure(B: TBackgroundWorld)
              begin
                Inc(TBackgroundWorld.ScenarioSteps);
              end)
            .Examples([['N'], [1], [2]]);
      end)
    .When('the suite is executed', procedure(W: TVisibilityWorld)
      begin
        W.Run.Execute;
        W.StepsRan := Format('%d steps of the scenario ran', [TBackgroundWorld.ScenarioSteps]);
      end)
    .&Then('each row counts as failed', procedure(W: TVisibilityWorld)
      begin
        Expect(Format('%d failed, %d passed', [W.Run.FailCount, W.Run.PassCount])).ToEqual('2 failed, 0 passed');
      end)
    .&And('no row runs its steps', procedure(W: TVisibilityWorld)
      begin
        Expect(W.StepsRan).ToEqual('0 steps of the scenario ran');
      end)

  .Scenario('The same outline with a Background that passes')
    // El control del anterior: sin el fallo del Background, las dos filas se ejecutan y pasan.
    .Given('the same outline prepared by a Background that passes', procedure(W: TVisibilityWorld)
      begin
        TBackgroundWorld.ScenarioSteps := 0;
        Feature('Its Background passes under an outline', W.Run.Suite)
          .UseWorld<TBackgroundWorld>
          .Background
            .Given('the environment is ready', procedure(B: TBackgroundWorld) begin end)
          .ScenarioOutline('Row <N>')
            .&Then('it runs', procedure(B: TBackgroundWorld)
              begin
                Inc(TBackgroundWorld.ScenarioSteps);
              end)
            .Examples([['N'], [1], [2]]);
      end)
    .When('the suite is executed', procedure(W: TVisibilityWorld)
      begin
        W.Run.Execute;
        W.StepsRan := Format('%d steps of the scenario ran', [TBackgroundWorld.ScenarioSteps]);
      end)
    .&Then('both rows run and pass', procedure(W: TVisibilityWorld)
      begin
        Expect(Format('%d failed, %d passed, %s', [W.Run.FailCount, W.Run.PassCount, W.StepsRan]))
          .ToEqual('0 failed, 2 passed, 2 steps of the scenario ran');
      end)

.Rule('A feature that fails outside its scenarios is in the report')

  .Scenario('A Before hook of a feature that throws')
    .Given('a run with a feature whose Before hook throws', procedure(W: TVisibilityWorld)
      begin
        Feature('Its Before hook throws', W.Run.Suite)
          .UseWorld<TPlainWorld>
          .Before('prepare something that is not there',
            procedure
            begin
              raise Exception.Create('the Before hook of the feature blew up');
            end)
          .Scenario('It never gets to run')
            .&Then('this step is never reached', procedure(P: TPlainWorld)
              begin
                Expect('the scenario ran').ToEqual('it should not have');
              end);
      end)
    .When('the suite is executed', procedure(W: TVisibilityWorld)
      begin
        W.Run.Execute;
        W.Report := W.Recorder.Text;
      end)
    .&Then('the feature is in the report with the message of that exception', procedure(W: TVisibilityWorld)
      begin
        Expect(W.Report).ToContain('the Before hook of the feature blew up');
      end)
    .&And('the total of reported features includes it', procedure(W: TVisibilityWorld)
      begin
        Expect(W.Run.FeatureCount).ToEqual(1);
      end)
    .&And('the exit code of that run is 1', procedure(W: TVisibilityWorld)
      begin
        Expect(W.Run.ExitCode).ToEqual(1);
      end)

.Rule('A feature the filter discards stays out of the report')

  // Es lo que separa las dos formas de acabar sin escenarios ejecutados. Sin este escenario, hacer
  // que se imprima TODA feature sin escenarios terminados pasaria los de arriba y este no.
  .Scenario('The filter matches none of its scenarios')
    .Given('a run with two features and a filter that matches only one', procedure(W: TVisibilityWorld)
      begin
        Feature('The one that is kept', W.Run.Suite)
          .UseWorld<TPlainWorld>
          .Scenario('keep this one')
            .&Then('it runs', procedure(P: TPlainWorld)
              begin
                Expect(1).ToEqual(1);
              end);
        Feature('The one the filter discards', W.Run.Suite)
          .UseWorld<TPlainWorld>
          .Scenario('drop this one')
            .&Then('it never runs', procedure(P: TPlainWorld)
              begin
                Expect(1).ToEqual(1);
              end);
        W.Run.Options.SpecMatcher :=
          function(const Scenario: IScenario): Boolean
          begin
            Result := Scenario.Description.Contains('keep');
          end;
      end)
    .When('the run finishes', procedure(W: TVisibilityWorld)
      begin
        W.Run.Execute;
        W.Report := W.Recorder.Text;
        if W.Report.Contains('The one the filter discards') then
          W.DiscardedShown := 'shown'
        else
          W.DiscardedShown := 'not shown';
      end)
    .&Then('that feature is not in the report', procedure(W: TVisibilityWorld)
      begin
        Expect(W.DiscardedShown).ToEqual('not shown');
        Expect(W.Run.FeatureCount).ToEqual(1);
      end)
    .&And('it does not count as a failure', procedure(W: TVisibilityWorld)
      begin
        Expect(Integer(W.Run.FailCount)).ToEqual(0);
      end);

end.
