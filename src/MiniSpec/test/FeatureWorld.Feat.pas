unit FeatureWorld.Feat;

interface

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.MiniSpec.Types;

type
  /// <summary>
  /// World que hereda de TFeatureWorld para acceder al contexto de ejecución.
  /// El contexto es accesible via ISpecContext.
  /// </summary>
  TMyWorld = class(TFeatureWorld)
  public
    StepDescription: string;
    ScenarioDescription: string;
    FeatureTitle: string;
    ExampleRowIndex: Integer;
    Value: Integer;
    function Context: ISpecContext;
  end;

function TMyWorld.Context: ISpecContext;
begin
  Result := Self as ISpecContext;
end;

initialization

Feature('''
FeatureWorld Demo @meta

  As a test author
  I want to access the execution context from my World
  So I can implement advanced test patterns
''')

.UseWorld<TMyWorld>

.Scenario('Access step info via ISpecContext')
  .Given('I have a TFeatureWorld-based World', procedure(World: TMyWorld)
    begin
      // CurrentStep is accessible via ISpecContext
      Expect(World.Context.CurrentStep <> nil).ToBeTrue;
    end)
  .When('I access the CurrentStep property', procedure(World: TMyWorld)
    begin
      World.StepDescription := World.Context.CurrentStep.Description;
    end)
  .&Then('I can read the step description', procedure(World: TMyWorld)
    begin
      Expect(World.StepDescription).ToContain('I access the CurrentStep');
    end)

.Scenario('Access CurrentScenario directly')
  .Given('I have a TFeatureWorld-based World', procedure(World: TMyWorld)
    begin
      Expect(World.Context.CurrentScenario).ToNotBeNull;
    end)
  .When('I access CurrentScenario', procedure(World: TMyWorld)
    begin
      World.ScenarioDescription := World.Context.CurrentScenario.Description;
    end)
  .&Then('I can read the scenario description', procedure(World: TMyWorld)
    begin
      Expect(World.ScenarioDescription).ToContain('Access CurrentScenario');
    end)

.Scenario('Access CurrentFeature directly')
  .Given('I have a TFeatureWorld-based World', procedure(World: TMyWorld)
    begin
      Expect(World.Context.CurrentFeature).ToNotBeNull;
    end)
  .When('I access CurrentFeature', procedure(World: TMyWorld)
    begin
      World.FeatureTitle := World.Context.CurrentFeature.Title;
    end)
  .&Then('I can read the feature title', procedure(World: TMyWorld)
    begin
      Expect(World.FeatureTitle).ToContain('FeatureWorld Demo');
    end)

.ScenarioOutline('Context works in ScenarioOutline with value <Value>')
  .Given('a value <Value>', procedure(World: TMyWorld)
    begin
      // En cada Example, CurrentStep apunta al step actual
      Expect(World.Context.CurrentStep).ToNotBeNull;
    end)
  .When('I check the context', procedure(World: TMyWorld)
    begin
      // CurrentScenario apunta al Example (un IScenario hijo del Outline)
      World.ScenarioDescription := World.Context.CurrentScenario.Description;
      World.ExampleRowIndex := World.Context.CurrentScenario.ExampleMeta.RowIndex;
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

end.
