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
    [Inject]
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
