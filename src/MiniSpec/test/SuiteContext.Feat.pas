unit SuiteContext.Feat;

interface
uses
  System.SysUtils,
  Daf.MiniSpec;

type
  /// <summary>
  /// Test context shared across all features in a suite
  /// </summary>
  TSharedSuiteContext = class
  public
    CreateCount: Integer;
    constructor Create;
  end;

  /// <summary>
  /// Test world for accessing suite context
  /// </summary>
  TTestWorld = class
  end;

implementation

constructor TSharedSuiteContext.Create;
begin
  CreateCount := 1;
end;

// Feature test
initialization

Feature('''
Feature SuiteContext @meta @context

  As a test author
  I want shared state across all features in a suite
  So I can set up expensive resources once
''')

.UseContext<TTestWorld>

.Scenario('SuiteContext is accessible')
  .Given('the suite uses SuiteContext', procedure(World: TTestWorld)
    begin
      // Context will be created before features execute
    end)
  .When('I check the suite context', procedure(World: TTestWorld)
    begin
      var Ctx := SpecContext.SuiteContext as TSharedSuiteContext;
      Expect(Ctx).IsNotNil;
      if Ctx <> nil then
        Expect(Ctx.CreateCount).Equals(1);
    end)
  .Then('it was created exactly once', procedure(World: TTestWorld)
    begin
      var Ctx := SpecContext.SuiteContext as TSharedSuiteContext;
      Expect(Ctx).IsNotNil;
    end);

end.

