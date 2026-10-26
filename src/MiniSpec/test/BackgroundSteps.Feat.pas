unit BackgroundSteps.Feat;

// Un Background prepara cada escenario con los mismos objetos step, y el resultado de una pasada no
// llega a la siguiente. Las ejecuciones son anidadas porque sus escenarios fallan, quedan pendientes
// o undefined a proposito, sin tocar la suite de fuera.

interface

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.MiniSpec.Types,
  Daf.MiniSpec.Runner;

type
  /// Cuenta lo que se ejecuta en la ejecucion anidada. Es de clase porque cada escenario tiene su
  /// World.
  TPreparedWorld = class
  public
    // Trace antes de los class var: un campo declarado detras de un class var tambien es de clase.
    Trace: string;
    class var FeatureRuns: Integer;
    class var Seen: string;
    class var FirstAttempts: Integer;
    class var SecondRuns: Integer;
    class var RestRuns: Integer;
    class var ScenarioSteps: Integer;
    class procedure Reset; static;
  end;

  TBackgroundStepsWorld = class
  strict private
    FRun: TSpecRun;
  public
    Ran: string;
    constructor Create;
    destructor Destroy; override;
    property Run: TSpecRun read FRun;
  end;

class procedure TPreparedWorld.Reset;
begin
  FeatureRuns := 0;
  Seen := '';
  FirstAttempts := 0;
  SecondRuns := 0;
  RestRuns := 0;
  ScenarioSteps := 0;
end;

constructor TBackgroundStepsWorld.Create;
begin
  inherited;
  FRun := TSpecRun.Create('Nested');
end;

destructor TBackgroundStepsWorld.Destroy;
begin
  FRun.Free;
  inherited;
end;

initialization

Feature('''
Background steps @background-steps

  As whoever reads the result of a suite
  I want every step of a Background to run for each scenario it prepares, and to decide it
  So a Background that is pending, undefined or failed once cannot leave a scenario passing
''')

.UseWorld<TBackgroundStepsWorld>

.Rule('A step of the Background runs in every scenario it prepares')

  .Scenario('A step skipped in one scenario runs in the next')
    .Given('a run with two scenarios prepared by a Background of two steps, whose first step fails only the first time',
      procedure(W: TBackgroundStepsWorld)
      begin
        TPreparedWorld.Reset;
        Feature('Its Background fails once', W.Run.Suite)
          .UseWorld<TPreparedWorld>
          .Background
            .Given('the environment is ready, except the first time', procedure(B: TPreparedWorld)
              begin
                Inc(TPreparedWorld.FirstAttempts);
                if TPreparedWorld.FirstAttempts = 1 then
                  Expect('broken').ToEqual('ready');
              end)
            .&And('the rest of the environment is set up on top', procedure(B: TPreparedWorld)
              begin
                Inc(TPreparedWorld.SecondRuns);
              end)
          .Scenario('The first one')
            .When('it uses the environment', procedure(B: TPreparedWorld)
              begin
              end)
          .Scenario('The second one')
            .When('it uses the environment', procedure(B: TPreparedWorld)
              begin
              end);
      end)
    .When('the suite is executed', procedure(W: TBackgroundStepsWorld)
      begin
        W.Run.Execute;
        W.Ran := Format('the second step of the Background ran %d time(s)', [TPreparedWorld.SecondRuns]);
      end)
    .&Then('the second step of the Background runs once, in the second scenario', procedure(W: TBackgroundStepsWorld)
      begin
        Expect(W.Ran).ToEqual('the second step of the Background ran 1 time(s)');
      end)
    .&And('the first scenario counts as failed and the second as passed', procedure(W: TBackgroundStepsWorld)
      begin
        Expect(Format('%d failed, %d passed', [W.Run.FailCount, W.Run.PassCount])).ToEqual('1 failed, 1 passed');
      end)

.Rule('Every Background runs once per scenario')

  .Scenario('A Background of the feature runs once per scenario')
    .Given('a run with two scenarios prepared by a Background of the feature with one step',
      procedure(W: TBackgroundStepsWorld)
      begin
        TPreparedWorld.Reset;
        Feature('Its Background counts its runs', W.Run.Suite)
          .UseWorld<TPreparedWorld>
          .Background
            .Given('the environment is set up', procedure(B: TPreparedWorld)
              begin
                Inc(TPreparedWorld.FeatureRuns);
              end)
          .Scenario('The first one')
            .When('it uses the environment', procedure(B: TPreparedWorld)
              begin
              end)
          .Scenario('The second one')
            .When('it uses the environment', procedure(B: TPreparedWorld)
              begin
              end);
      end)
    .When('the suite is executed', procedure(W: TBackgroundStepsWorld)
      begin
        W.Run.Execute;
        W.Ran := Format('the step of the Background ran %d time(s)', [TPreparedWorld.FeatureRuns]);
      end)
    .&Then('that step runs twice', procedure(W: TBackgroundStepsWorld)
      begin
        Expect(W.Ran).ToEqual('the step of the Background ran 2 time(s)');
      end)

  .Scenario('The Background of a rule adds to the one of the feature')
    .Given('a feature with a Background, a scenario outside any rule, and a rule with its own Background and a scenario',
      procedure(W: TBackgroundStepsWorld)
      begin
        TPreparedWorld.Reset;
        Feature('Its rule has a Background too', W.Run.Suite)
          .UseWorld<TPreparedWorld>
          .Background
            .Given('the feature sets up its part', procedure(B: TPreparedWorld)
              begin
                B.Trace := B.Trace + 'F';
              end)
          .Scenario('Outside')
            .When('it records how it was prepared', procedure(B: TPreparedWorld)
              begin
                TPreparedWorld.Seen := TPreparedWorld.Seen + 'outside:' + B.Trace + ' ';
              end)
          .Rule('A rule')
            .Background
              .Given('the rule sets up its part', procedure(B: TPreparedWorld)
                begin
                  B.Trace := B.Trace + 'R';
                end)
            .Scenario('Inside')
              .When('it records how it was prepared', procedure(B: TPreparedWorld)
                begin
                  TPreparedWorld.Seen := TPreparedWorld.Seen + 'inside:' + B.Trace;
                end);
      end)
    .When('the suite is executed', procedure(W: TBackgroundStepsWorld)
      begin
        W.Run.Execute;
        W.Ran := TPreparedWorld.Seen;
      end)
    .&Then('the scenario outside is prepared by the Background of the feature alone, and the one inside by that and then its own',
      procedure(W: TBackgroundStepsWorld)
      begin
        Expect(W.Ran).ToEqual('outside:F inside:FR');
      end)

.Rule('A Pending or Undefined step of the Background decides the scenario')

  .Scenario('A Pending step in the Background')
    .Given('a run with one scenario prepared by a Background whose first step is declared Pending',
      procedure(W: TBackgroundStepsWorld)
      begin
        TPreparedWorld.Reset;
        Feature('Its Background is pending', W.Run.Suite)
          .UseWorld<TPreparedWorld>
          .Background
            .Given('the environment is not written yet', procedure(B: TPreparedWorld)
              begin
                Inc(TPreparedWorld.RestRuns);
              end).Pending
            .&And('the rest of the environment is set up on top', procedure(B: TPreparedWorld)
              begin
                Inc(TPreparedWorld.RestRuns);
              end)
          .Scenario('It depends on the environment')
            .When('it uses the environment', procedure(B: TPreparedWorld)
              begin
                Inc(TPreparedWorld.ScenarioSteps);
              end)
            .&Then('it checks what it got', procedure(B: TPreparedWorld)
              begin
                Inc(TPreparedWorld.ScenarioSteps);
              end);
      end)
    .When('the suite is executed', procedure(W: TBackgroundStepsWorld)
      begin
        W.Run.Execute;
        W.Ran := Format('%d of the Background, %d of the scenario',
          [TPreparedWorld.RestRuns, TPreparedWorld.ScenarioSteps]);
      end)
    .&Then('the scenario counts as pending, and none as passed', procedure(W: TBackgroundStepsWorld)
      begin
        Expect(Format('%d pending, %d passed', [W.Run.PendingCount, W.Run.PassCount])).ToEqual('1 pending, 0 passed');
      end)
    .&And('neither the rest of the Background nor the steps of the scenario run', procedure(W: TBackgroundStepsWorld)
      begin
        Expect(W.Ran).ToEqual('0 of the Background, 0 of the scenario');
      end)
    .&And('the exit code of that run is 0', procedure(W: TBackgroundStepsWorld)
      begin
        Expect(W.Run.ExitCode).ToEqual(0);
      end)

  .Scenario('An Undefined step in the Background')
    .Given('a run with one scenario prepared by a Background that has a step with no binding',
      procedure(W: TBackgroundStepsWorld)
      begin
        TPreparedWorld.Reset;
        Feature('Its Background is undefined', W.Run.Suite)
          .UseWorld<TPreparedWorld>
          .Background
            .Given('an environment that no binding of BackgroundSteps.Feat answers')
          .Scenario('It depends on the environment')
            .When('it uses the environment', procedure(B: TPreparedWorld)
              begin
                Inc(TPreparedWorld.ScenarioSteps);
              end)
            .&Then('it checks what it got', procedure(B: TPreparedWorld)
              begin
                Inc(TPreparedWorld.ScenarioSteps);
              end);
      end)
    .When('the suite is executed', procedure(W: TBackgroundStepsWorld)
      begin
        W.Run.Execute;
        W.Ran := Format('%d of the scenario', [TPreparedWorld.ScenarioSteps]);
      end)
    .&Then('the scenario counts as undefined, and none as passed', procedure(W: TBackgroundStepsWorld)
      begin
        Expect(Format('%d undefined, %d passed', [W.Run.UndefinedCount, W.Run.PassCount])).ToEqual('1 undefined, 0 passed');
      end)
    .&And('the steps of the scenario do not run', procedure(W: TBackgroundStepsWorld)
      begin
        Expect(W.Ran).ToEqual('0 of the scenario');
      end)
    .&And('the exit code of that run is 1', procedure(W: TBackgroundStepsWorld)
      begin
        Expect(W.Run.ExitCode).ToEqual(1);
      end);

end.
