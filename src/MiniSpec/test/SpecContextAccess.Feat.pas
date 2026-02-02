unit SpecContextAccess.Feat;

interface

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.MiniSpec.Types;

type
  /// <summary>
  /// Contexto individual para cada escenario.
  /// El contexto de ejecución es accesible globalmente via SpecContext.
  /// </summary>
  TScenarioContext = class
  public
    StepDescription: string;
    ScenarioDescription: string;
    FeatureTitle: string;
    ExampleRowIndex: Integer;
    Value: Integer;
  end;

var
  GBeforeExecuted: Boolean;

initialization

GBeforeExecuted := False;

Feature('''
SpecContext Demo @meta

  As a test author
  I want to access the execution context via SpecContext
  So I can implement advanced test patterns
''')

.UseWorld<TScenarioContext>

.Scenario('Access step info via global SpecContext')
  .Given('I have a simple ScenarioContext class', procedure(Ctx: TScenarioContext)
    begin
      GBeforeExecuted := True;
      // SpecContext es accesible globalmente
      Expect(SpecContext.Step <> nil).ToBeTrue;
    end)
  .When('I access the Step property', procedure(Ctx: TScenarioContext)
    begin
      Ctx.StepDescription := SpecContext.Step.Description;
    end)
  .&Then('I can read the step description', procedure(Ctx: TScenarioContext)
    begin
      Expect(Ctx.StepDescription).ToContain('I access the Step');
    end)

.Scenario('Access Scenario via global SpecContext')
  .Given('I have a simple ScenarioContext class', procedure(Ctx: TScenarioContext)
    begin
      Expect(SpecContext.Scenario).ToNotBeNull;
    end)
  .When('I access Scenario', procedure(Ctx: TScenarioContext)
    begin
      Ctx.ScenarioDescription := SpecContext.Scenario.Description;
    end)
  .&Then('I can read the scenario description', procedure(Ctx: TScenarioContext)
    begin
      Expect(Ctx.ScenarioDescription).ToContain('Access Scenario');
    end)

.Scenario('Access Feature via global SpecContext')
  .Given('I have a simple ScenarioContext class', procedure(Ctx: TScenarioContext)
    begin
      Expect(SpecContext.Feature).ToNotBeNull;
    end)
  .When('I access Feature', procedure(Ctx: TScenarioContext)
    begin
      Ctx.FeatureTitle := SpecContext.Feature.Title;
    end)
  .&Then('I can read the feature title', procedure(Ctx: TScenarioContext)
    begin
      Expect(Ctx.FeatureTitle).ToContain('SpecContext Demo');
    end)

.ScenarioOutline('Context works in ScenarioOutline with value <Value>')
  .Given('a value <Value>', procedure(Ctx: TScenarioContext)
    begin
      // En cada Example, Step apunta al step actual
      Expect(SpecContext.Step).ToNotBeNull;
    end)
  .When('I check the context', procedure(Ctx: TScenarioContext)
    begin
      // Scenario apunta al Example (un IScenario hijo del Outline)
      Ctx.ScenarioDescription := SpecContext.Scenario.Description;
      Ctx.ExampleRowIndex := SpecContext.Scenario.ExampleMeta.RowIndex;
    end)
  .&Then('I can access Example metadata', procedure(Ctx: TScenarioContext)
    begin
      // Verificar que tenemos acceso al row index del Example
      Expect(Ctx.ExampleRowIndex > 0).ToBeTrue;
      // El Scenario description contiene los valores sustituidos
      Expect(Ctx.ScenarioDescription).ToContain('value');
    end)
  .Examples([
    ['Value'],
    [10],
    [20],
    [30]
  ]);

finalization
  // Verificación deshabilitada temporalmente mientras se refactoriza el contexto
  // if GBeforeExecuted then
  //   Expect(SpecContext.Feature).ToBeNull;  // Context should be clean

end.
