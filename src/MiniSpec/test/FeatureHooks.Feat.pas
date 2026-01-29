unit FeatureHooks.Feat;

interface

implementation

uses
  System.SysUtils,
  Daf.MiniSpec;

var
  // Variables globales para verificar ejecución de hooks
  GBeforeExecuted: Boolean;
  GAfterExecuted: Boolean;
  GBeforeCount: Integer;
  GAfterCount: Integer;
  GExecutionLog: string;

type
  THooksWorld = class
  public
    Value: Integer;
  end;

initialization

// Reset state before running
GBeforeExecuted := False;
GAfterExecuted := False;
GBeforeCount := 0;
GAfterCount := 0;
GExecutionLog := '';

Feature('''
Feature Hooks @meta @hooks

  As a test author
  I want to run setup code once before all scenarios
  And teardown code once after all scenarios
  So I can manage expensive resources efficiently
''')

.UseWorld<THooksWorld>

.Before('Initialize test environment', procedure
  begin
    GBeforeExecuted := True;
    Inc(GBeforeCount);
    GExecutionLog := GExecutionLog + '[Before]';
  end)

.After('Cleanup test environment', procedure
  begin
    GAfterExecuted := True;
    Inc(GAfterCount);
    GExecutionLog := GExecutionLog + '[After]';
  end)

.Scenario('Before hook runs before first scenario')
  .Given('the feature has a Before hook', procedure(World: THooksWorld)
    begin
      // Before should have already executed
      GExecutionLog := GExecutionLog + '[Scenario1:Given]';
    end)
  .When('this scenario executes', procedure(World: THooksWorld)
    begin
      GExecutionLog := GExecutionLog + '[Scenario1:When]';
    end)
  .&Then('Before was already called', procedure(World: THooksWorld)
    begin
      Expect(GBeforeExecuted).ToBeTrue;
      Expect(GBeforeCount).ToEqual(1);
    end)

.Scenario('Before hook runs only once')
  .Given('we are in a second scenario', procedure(World: THooksWorld)
    begin
      GExecutionLog := GExecutionLog + '[Scenario2:Given]';
    end)
  .When('I check the Before count', procedure(World: THooksWorld)
    begin
      GExecutionLog := GExecutionLog + '[Scenario2:When]';
    end)
  .&Then('Before was still called only once', procedure(World: THooksWorld)
    begin
      // Before should NOT run again for second scenario
      Expect(GBeforeCount).ToEqual(1);
    end)

.Scenario('After hook runs after last scenario')
  .Given('we are in the last scenario', procedure(World: THooksWorld)
    begin
      GExecutionLog := GExecutionLog + '[Scenario3:Given]';
    end)
  .When('I check if After has run', procedure(World: THooksWorld)
    begin
      GExecutionLog := GExecutionLog + '[Scenario3:When]';
    end)
  .&Then('After has NOT run yet', procedure(World: THooksWorld)
    begin
      // After should run AFTER all scenarios complete
      Expect(GAfterExecuted).ToBeFalse;
    end)

;

finalization
  // Verify After was called when feature finalized
  // (This runs after the feature completes)
  // Solo verificar si el Before hook se ejecutó (indicando que el feature corrió)
  if GBeforeExecuted then
  begin
    if not GAfterExecuted then
      raise Exception.Create('After hook was never executed!');
    if GAfterCount <> 1 then
      raise Exception.CreateFmt('After hook ran %d times instead of 1!', [GAfterCount]);

    // Log the execution order for debugging
    // Expected: [Before][Scenario1:Given][Scenario1:When]...[Scenario3:When][After]
    WriteLn('=== Hook Execution Log ===');
    WriteLn(GExecutionLog);
  end;

end.
