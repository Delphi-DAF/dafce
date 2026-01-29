unit FeatureContext.Feat;

interface

var
  /// Contador para verificar ciclo de vida del FeatureContext
  FeatureContextCreateCount: Integer;
  FeatureContextDestroyCount: Integer;

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.MiniSpec.Builders,
  Daf.MiniSpec.Types;

type
  /// <summary>
  /// FeatureContext compartido entre todos los Scenarios de la Feature.
  /// </summary>
  TSharedFeatureContext = class
  public
    Counter: Integer;
    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  /// Contexto individual para cada Scenario.
  /// </summary>
  TScenarioContext = class
  public
    LocalValue: Integer;
  end;

{ TSharedFeatureContext }

constructor TSharedFeatureContext.Create;
begin
  inherited;
  Inc(FeatureContextCreateCount);
  Counter := 100;
end;

destructor TSharedFeatureContext.Destroy;
begin
  Inc(FeatureContextDestroyCount);
  inherited;
end;

initialization

FeatureContextCreateCount := 0;
FeatureContextDestroyCount := 0;

Feature('''
FeatureContext Demo @featurecontext

  As a test author
  I want to share a context across all scenarios in a feature
  So I can maintain expensive resources without recreating them
''')
.ShareContext<TSharedFeatureContext>
.UseContext<TScenarioContext>

.Before('Initialize shared counter',
  procedure
  begin
    // FeatureContext ya existe aquí
    var FCtx := SpecContext.FeatureContext as TSharedFeatureContext;
    FCtx.Counter := 200;
  end)

.Scenario('First scenario accesses shared context')
  .Given('I access the FeatureContext', procedure(Ctx: TScenarioContext)
    begin
      var FCtx := SpecContext.FeatureContext as TSharedFeatureContext;
      Ctx.LocalValue := FCtx.Counter;
    end)
  .When('I modify the shared counter', procedure(Ctx: TScenarioContext)
    begin
      var FCtx := SpecContext.FeatureContext as TSharedFeatureContext;
      FCtx.Counter := FCtx.Counter + 1;  // 200 -> 201
    end)
  .&Then('The local value was captured correctly', procedure(Ctx: TScenarioContext)
    begin
      Expect(Ctx.LocalValue).ToEqual(200);
    end)

.Scenario('Second scenario sees previous changes')
  .Given('I access the FeatureContext again', procedure(Ctx: TScenarioContext)
    begin
      var FCtx := SpecContext.FeatureContext as TSharedFeatureContext;
      Ctx.LocalValue := FCtx.Counter;
    end)
  .When('I check the counter value', procedure(Ctx: TScenarioContext)
    begin
      // El contador fue incrementado por el scenario anterior
    end)
  .&Then('The counter reflects the previous scenario change', procedure(Ctx: TScenarioContext)
    begin
      Expect(Ctx.LocalValue).ToEqual(201);  // 200 + 1 del scenario anterior
    end)

.Scenario('FeatureContext was created only once')
  .Given('all scenarios have run', procedure(Ctx: TScenarioContext)
    begin
      // Verificamos que solo se creó una instancia
    end)
  .&Then('FeatureContext was created exactly once', procedure(Ctx: TScenarioContext)
    begin
      Expect(FeatureContextCreateCount).ToEqual(1);
    end);

end.
