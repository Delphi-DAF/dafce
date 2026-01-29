unit PropertyInjection.Feat;

interface

var
  /// Contador para verificar que el FeatureContext se inyectó
  InjectionCount: Integer;

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.MiniSpec.Builders,
  Daf.MiniSpec.Types,
  Daf.MiniSpec.Injection;

type
  /// <summary>
  /// FeatureContext compartido entre todos los Scenarios.
  /// </summary>
  TSharedContext = class
  public
    Value: Integer;
    constructor Create;
  end;

  /// <summary>
  /// ScenarioContext con propiedad [Inject] para FeatureContext.
  /// </summary>
  TScenarioContext = class
  private
    FShared: TSharedContext;
  public
    /// <summary>
    /// Esta propiedad será inyectada automáticamente por el runner.
    /// </summary>
    [Inject]
    property Shared: TSharedContext read FShared write FShared;
  end;

{ TSharedContext }

constructor TSharedContext.Create;
begin
  inherited;
  Value := 42;
end;

initialization

InjectionCount := 0;

Feature('''
Property Injection Demo @injection

  As a test author
  I want FeatureContext to be auto-injected into ScenarioContext
  So I can access shared state without manual casting
''')
.ShareContext<TSharedContext>
.UseContext<TScenarioContext>

.Scenario('FeatureContext is auto-injected via [Inject] property')
  .Given('I have a ScenarioContext with [Inject] property', procedure(Ctx: TScenarioContext)
    begin
      // No hacemos nada, solo verificamos que Ctx.Shared fue inyectado
    end)
  .When('the scenario starts', procedure(Ctx: TScenarioContext)
    begin
      // El runner ya inyectó Shared antes de llegar aquí
    end)
  .&Then('the Shared property is not nil', procedure(Ctx: TScenarioContext)
    begin
      Assert(Ctx.Shared <> nil, 'Shared should have been injected');
    end)

.Scenario('Injected property has correct value')
  .Given('FeatureContext was created with Value=42', procedure(Ctx: TScenarioContext)
    begin
      // TSharedContext.Create sets Value := 42
    end)
  .When('I access the injected Shared property', procedure(Ctx: TScenarioContext)
    begin
      Inc(InjectionCount);
    end)
  .&Then('I can read the correct value', procedure(Ctx: TScenarioContext)
    begin
      Expect(Ctx.Shared.Value).ToEqual(42);
    end)

.Scenario('Injected property reflects modifications')
  .Given('I modify the shared Value', procedure(Ctx: TScenarioContext)
    begin
      Ctx.Shared.Value := 100;
    end)
  .When('another scenario accesses it', procedure(Ctx: TScenarioContext)
    begin
      // This is a new ScenarioContext but same FeatureContext
    end)
  .&Then('the modification is visible', procedure(Ctx: TScenarioContext)
    begin
      Expect(Ctx.Shared.Value).ToEqual(100);
    end);

end.
