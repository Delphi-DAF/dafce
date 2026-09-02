unit NestedRun.Feat;

// Estas specs son las primeras que ejercitan al runner ejecutandolo: montan un arbol de features en
// una suite propia, lo corren y afirman sobre sus recuentos. Lo que hace falta comprobar en cada una
// es que la ejecucion de fuera --la que esta ejecutando esta spec-- no se entera.

interface

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.MiniSpec.Types,
  Daf.MiniSpec.Runner;

type
  TToyWorld = class
  public
    Touched: Boolean;
  end;

  /// Contexto de suite que se niega a construirse. Es la unica forma de que una excepcion escape de
  /// TSpecRun.Execute: lo que revienta dentro de un escenario lo atrapa el propio runner.
  TExplodingCtx = class
  public
    constructor Create;
  end;

  TNestedWorld = class
  strict private
    FRun: TSpecRun;
  public
    OuterFailsBefore: Integer;
    OuterFailsAfter: Integer;
    OuterFeaturesBefore: Integer;
    OuterFeaturesAfter: Integer;
    OuterStep: string;
    OuterScenario: string;
    AfterStep: string;
    AfterScenario: string;
    ProcessFeaturesBefore: Integer;
    ProcessFeaturesAfter: Integer;
    Report: string;
    NestedExitCode: Integer;
    ProcessExitCode: Integer;
    OwnFeatureFound: Boolean;
    constructor Create;
    destructor Destroy; override;
    property Run: TSpecRun read FRun;
  end;

constructor TExplodingCtx.Create;
begin
  inherited;
  raise Exception.Create('this suite context refuses to be built');
end;

constructor TNestedWorld.Create;
begin
  inherited;
  FRun := TSpecRun.Create('Nested');
end;

destructor TNestedWorld.Destroy;
begin
  FRun.Free;
  inherited;
end;

/// Un escenario que falla, en la suite que se le indique. Falla por una expectativa y no por una
/// excepcion suelta: es el fallo corriente, el que cualquier spec produce cuando esta en rojo.
procedure AddFailingFeature(const Run: TSpecRun);
begin
  Feature('A toy feature that fails', Run.Suite)
    .UseWorld<TToyWorld>
    .Scenario('It fails on purpose')
      .&Then('one is not two', procedure(W: TToyWorld)
        begin
          Expect(1).ToEqual(2);
        end);
end;

procedure AddPassingFeature(const Run: TSpecRun);
begin
  Feature('A toy feature that passes', Run.Suite)
    .UseWorld<TToyWorld>
    .Scenario('It passes')
      .&Then('one is one', procedure(W: TToyWorld)
        begin
          Expect(1).ToEqual(1);
        end);
end;

initialization

Feature('''
Nested run @nested

  As whoever writes specs for the runner itself
  I want to run a suite inside another one without either of them noticing
  So the rules of the runner can be checked with specs instead of by hand
''')

.UseWorld<TNestedWorld>

.Rule('A nested run does not alter the counts of the run that contains it')

  .Scenario('A scenario that fails inside the nested one does not dirty the outer one')
    .Given('a nested run whose only scenario fails', procedure(W: TNestedWorld)
      begin
        AddFailingFeature(W.Run);
        W.OuterFailsBefore := MiniSpec.Suite.RunInfo.FailCount;
      end)
    .When('it is executed', procedure(W: TNestedWorld)
      begin
        W.Run.Execute;
        W.OuterFailsAfter := MiniSpec.Suite.RunInfo.FailCount;
      end)
    .&Then('the nested one reports one failure of its own', procedure(W: TNestedWorld)
      begin
        Expect(Integer(W.Run.FailCount)).ToEqual(1);
      end)
    .&And('the counts of the outer run did not move', procedure(W: TNestedWorld)
      begin
        Expect(W.OuterFailsAfter).ToEqual(W.OuterFailsBefore);
      end)

  .Scenario('The counts of the nested one are readable by whoever launched it')
    .Given('a nested run with one scenario that passes and one that fails', procedure(W: TNestedWorld)
      begin
        AddPassingFeature(W.Run);
        AddFailingFeature(W.Run);
      end)
    .When('it is executed', procedure(W: TNestedWorld)
      begin
        W.Run.Execute;
      end)
    .&Then('how many passed, failed, were pending or undefined can be read', procedure(W: TNestedWorld)
      begin
        Expect(Integer(W.Run.PassCount)).ToEqual(1);
        Expect(Integer(W.Run.FailCount)).ToEqual(1);
        Expect(Integer(W.Run.PendingCount)).ToEqual(0);
        Expect(Integer(W.Run.UndefinedCount)).ToEqual(0);
        Expect(Integer(W.Run.TotalCount)).ToEqual(2);
      end)

.Rule('A nested run does not write into the reports of the outer one')

  // El recuento de features informadas del runner de fuera es lo que separa las dos cosas: solo
  // sube cuando ESE runner informa. Si el entorno anidado usara el runner del proceso, subiria aqui.
  .Scenario('With no destination declared it prints nothing')
    .Given('a nested run with no listener of its own', procedure(W: TNestedWorld)
      begin
        AddPassingFeature(W.Run);
        W.OuterFeaturesBefore := MiniSpec.Runner.FeatureCount;
      end)
    .When('it is executed', procedure(W: TNestedWorld)
      begin
        W.Run.Execute;
        W.OuterFeaturesAfter := MiniSpec.Runner.FeatureCount;
      end)
    .&Then('the runner of the outer run reported nothing', procedure(W: TNestedWorld)
      begin
        Expect(W.OuterFeaturesBefore).ToEqual(0);
        Expect(W.OuterFeaturesAfter).ToEqual(0);
      end)
    .&And('the nested one did report its own feature', procedure(W: TNestedWorld)
      begin
        Expect(W.Run.FeatureCount).ToEqual(1);
      end)

.Rule('The execution context is restored when the nested one ends')

  .Scenario('The outer scenario still sees itself afterwards')
    .Given('a nested run ready to go', procedure(W: TNestedWorld)
      begin
        AddPassingFeature(W.Run);
        W.OuterScenario := SpecContext.Scenario.Description;
      end)
    .When('a step of the outer run launches it halfway through its steps', procedure(W: TNestedWorld)
      begin
        W.OuterStep := SpecContext.Step.Description;
        W.Run.Execute;
        W.AfterStep := SpecContext.Step.Description;
        W.AfterScenario := SpecContext.Scenario.Description;
      end)
    .&Then('its next steps read its own scenario and its own step', procedure(W: TNestedWorld)
      begin
        Expect(W.AfterStep).ToEqual(W.OuterStep);
        Expect(W.AfterScenario).ToEqual(W.OuterScenario);
        Expect(SpecContext.Scenario.Description).ToEqual(W.OuterScenario);
      end)

  .Scenario('A nested run that ends with an exception restores it too')
    .Given('a nested run whose suite context refuses to be built', procedure(W: TNestedWorld)
      begin
        W.Run.UseSuiteContext<TExplodingCtx>;
        AddPassingFeature(W.Run);
        W.OuterScenario := SpecContext.Scenario.Description;
      end)
    .When('it is executed and the exception escapes it', procedure(W: TNestedWorld)
      begin
        W.OuterStep := SpecContext.Step.Description;
        try
          W.Run.Execute;
          W.Report := 'it did not blow up';
        except
          on E: Exception do
            W.Report := E.Message;
        end;
        W.AfterStep := SpecContext.Step.Description;
        W.AfterScenario := SpecContext.Scenario.Description;
      end)
    .&Then('the context of the outer run is as it was before launching it', procedure(W: TNestedWorld)
      begin
        Expect(W.Report).ToContain('refuses to be built');
        Expect(W.AfterStep).ToEqual(W.OuterStep);
        Expect(W.AfterScenario).ToEqual(W.OuterScenario);
      end)

.Rule('A feature declares which suite it registers in')

  .Scenario('A feature created with a suite of its own does not appear in the process one')
    .Given('the process suite is counted before anything is created', procedure(W: TNestedWorld)
      begin
        W.ProcessFeaturesBefore := MiniSpec.Suite.Features.Count;
      end)
    .When('a feature is created naming the suite of a nested run', procedure(W: TNestedWorld)
      begin
        AddPassingFeature(W.Run);
        W.ProcessFeaturesAfter := MiniSpec.Suite.Features.Count;
      end)
    .&Then('that feature belongs to that suite', procedure(W: TNestedWorld)
      begin
        Expect(W.Run.Suite.Features.Count).ToEqual(1);
      end)
    .&And('the process suite does not contain it', procedure(W: TNestedWorld)
      begin
        Expect(W.ProcessFeaturesAfter).ToEqual(W.ProcessFeaturesBefore);
      end)

  // No se crea una feature nueva para comprobarlo: hacerlo mientras la suite del proceso se recorre
  // modificaria la lista que se esta enumerando. La que corre esta spec se declaro sin suite, asi
  // que ella misma es el caso.
  .Scenario('A feature created without naming a suite registers in the process one')
    .When('the process suite is searched for the feature declared by this file', procedure(W: TNestedWorld)
      begin
        for var F in MiniSpec.Suite.Features do
          if F.Title = SpecContext.Feature.Title then
            W.OwnFeatureFound := True;
      end)
    .&Then('it is registered there, just as before this change', procedure(W: TNestedWorld)
      begin
        Expect(W.OwnFeatureFound).ToBeTrue;
      end)

.Rule('Only the root run sets the exit code of the process')

  .Scenario('A nested one with failures does not bring the process down')
    .Given('a nested run whose only scenario fails', procedure(W: TNestedWorld)
      begin
        AddFailingFeature(W.Run);
      end)
    .When('it is executed', procedure(W: TNestedWorld)
      begin
        W.Run.Execute;
        W.NestedExitCode := W.Run.ExitCode;
        W.ProcessExitCode := ExitCode;
      end)
    .&Then('its result says there were failures', procedure(W: TNestedWorld)
      begin
        Expect(W.NestedExitCode).ToEqual(1);
      end)
    .&And('the exit code of the process is still the root run to decide', procedure(W: TNestedWorld)
      begin
        Expect(W.ProcessExitCode).ToEqual(0);
      end);

end.
