unit FeatureContext.Feat;

interface

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.MiniSpec.Types;

type
  /// <summary>
  /// Shared context that persists across all scenarios in the feature.
  /// The class must have a constructor with no parameters.
  /// </summary>
  TSharedFeatureContext = class
  public
    Counter: Integer;
    InitializedBy: string;
    constructor Create;
  end;

  TScenarioWorld = class
  public
    LocalValue: Integer;
  end;

var
  GCreateCount: Integer;

{ TSharedFeatureContext }

constructor TSharedFeatureContext.Create;
begin
  inherited Create;
  Inc(GCreateCount);
  Counter := 0;
  InitializedBy := '';
end;

initialization

GCreateCount := 0;

Feature('''
Feature FeatureContext @meta @context

  As a test author
  I want shared state across all scenarios in a feature
  So I can avoid expensive setup for each scenario
''')

.UseContext<TScenarioWorld>
.UseFeatureContext(TSharedFeatureContext)

.Scenario('FeatureContext is accessible')
  .Given('the feature uses FeatureContext', procedure(World: TScenarioWorld)
    begin
      Expect(SpecContext.FeatureContext <> nil).ToBeTrue;
    end)
  .When('I check the create count', procedure(World: TScenarioWorld)
    begin
      // Nothing to do
    end)
  .&Then('it was created exactly once', procedure(World: TScenarioWorld)
    begin
      Expect(GCreateCount).ToEqual(1);
    end)

;

end.
