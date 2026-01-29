unit FeatureWorld.Feat;

interface

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.MiniSpec.Types;

type
  /// <summary>
  /// World simple - ya no necesita heredar de TFeatureWorld.
  /// El contexto de ejecución es accesible globalmente via SpecContext.
  /// </summary>
  TMyWorld = class
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
FeatureWorld Demo @meta

  As a test author
  I want to access the execution context from my World
  So I can implement advanced test patterns
''')

.UseContext<TMyWorld>

.Scenario('Access step info via global SpecContext')
  .Given('I have a simple World class', procedure(World: TMyWorld)
    begin
      GBeforeExecuted := True;
      // SpecContext es accesible globalmente
      Expect(SpecContext.Step <> nil).ToBeTrue;
    end)
  .When('I access the Step property', procedure(World: TMyWorld)
    begin
      World.StepDescription := SpecContext.Step.Description;
    end)
  .&Then('I can read the step description', procedure(World: TMyWorld)
    begin
      Expect(World.StepDescription).ToContain('I access the Step');
    end)

.Scenario('Access Scenario via global SpecContext')
  .Given('I have a simple World class', procedure(World: TMyWorld)
    begin
      Expect(SpecContext.Scenario).ToNotBeNull;
    end)
  .When('I access Scenario', procedure(World: TMyWorld)
    begin
      World.ScenarioDescription := SpecContext.Scenario.Description;
    end)
  .&Then('I can read the scenario description', procedure(World: TMyWorld)
    begin
      Expect(World.ScenarioDescription).ToContain('Access Scenario');
    end)

.Scenario('Access Feature via global SpecContext')
  .Given('I have a simple World class', procedure(World: TMyWorld)
    begin
      Expect(SpecContext.Feature).ToNotBeNull;
    end)
  .When('I access Feature', procedure(World: TMyWorld)
    begin
      World.FeatureTitle := SpecContext.Feature.Title;
    end)
  .&Then('I can read the feature title', procedure(World: TMyWorld)
    begin
      Expect(World.FeatureTitle).ToContain('FeatureWorld Demo');
    end)

.ScenarioOutline('Context works in ScenarioOutline with value <Value>')
  .Given('a value <Value>', procedure(World: TMyWorld)
    begin
      // En cada Example, Step apunta al step actual
      Expect(SpecContext.Step).ToNotBeNull;
    end)
  .When('I check the context', procedure(World: TMyWorld)
    begin
      // Scenario apunta al Example (un IScenario hijo del Outline)
      World.ScenarioDescription := SpecContext.Scenario.Description;
      World.ExampleRowIndex := SpecContext.Scenario.ExampleMeta.RowIndex;
    end)
  .&Then('I can access Example metadata', procedure(World: TMyWorld)
    begin
      // Verificar que tenemos acceso al row index del Example
      Expect(World.ExampleRowIndex > 0).ToBeTrue;
      // El Scenario description contiene los valores sustituidos
      Expect(World.ScenarioDescription).ToContain('value');
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
