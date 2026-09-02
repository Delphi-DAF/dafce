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
    SeenRule: string;
  end;

var
  GBeforeExecuted: Boolean;
  /// Que Rule vio el hook de feature, que corre fuera de todo escenario. Global porque un hook no
  /// recibe World: es el mismo motivo por el que GBeforeExecuted lo es.
  GRuleInFeatureHook: string;

/// Que Rule ve el contexto ahora mismo, en una sola cadena. ToBeNull no imprime el valor real, y
/// cuando esto falla lo que hay que ver es CUAL contesto, no que hubiera alguna.
function RuleReport: string;
begin
  if SpecContext.Rule = nil then
    Exit('none');
  if SpecContext.Rule.Kind = sikRule then
    Result := 'rule:<' + SpecContext.Rule.Description + '>'
  else
    Result := 'implicit:<' + SpecContext.Rule.Description + '>';
end;

initialization

GBeforeExecuted := False;
// Centinela: si el hook no llegara a correr, el escenario que lo comprueba falla diciendolo en vez
// de comparar contra la cadena vacia, que es lo que dejaria una variable sin tocar.
GRuleInFeatureHook := '<hook did not run>';

Feature('''
SpecContext Demo @meta

  As a test author
  I want to access the execution context via SpecContext
  So I can implement advanced test patterns
''')

.UseWorld<TScenarioContext>

.Before('read the Rule outside any scenario',
  procedure
  begin
    GRuleInFeatureHook := RuleReport;
  end)

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
  ])

// La Rule implicita no es un detalle interno del arbol: es lo que SpecContext.Rule devuelve cuando
// nadie declaro una, y por eso Rule no es nil dentro de un escenario. Lo que la distingue de una
// explicita es su Description vacia, no su ausencia.
.Scenario('A scenario declared without a Rule still has one')
  .When('it asks the context for its Rule', procedure(Ctx: TScenarioContext)
    begin
      Ctx.SeenRule := RuleReport;
    end)
  .&Then('it is the implicit Rule, and its description is empty', procedure(Ctx: TScenarioContext)
    begin
      Expect(Ctx.SeenRule).ToEqual('implicit:<>');
    end)

// El control del anterior: sin este, un SpecContext.Rule que nunca fuera nil pasaria aquel igual.
.Scenario('Outside every scenario there is no Rule at all')
  .&Then('the feature hook found none', procedure(Ctx: TScenarioContext)
    begin
      Expect(GRuleInFeatureHook).ToEqual('none');
    end)

.Rule('An explicit rule')

  .Scenario('A scenario inside an explicit Rule reads that one')
    .When('it asks the context for its Rule', procedure(Ctx: TScenarioContext)
      begin
        Ctx.SeenRule := RuleReport;
      end)
    .&Then('it is that Rule, named by its description', procedure(Ctx: TScenarioContext)
      begin
        Expect(Ctx.SeenRule).ToEqual('rule:<An explicit rule>');
      end);

finalization
  // Verificación deshabilitada temporalmente mientras se refactoriza el contexto
  // if GBeforeExecuted then
  //   Expect(SpecContext.Feature).ToBeNull;  // Context should be clean

end.
