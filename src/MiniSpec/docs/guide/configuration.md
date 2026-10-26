# Global Configuration

**🌍 Language: English | [Español](configuration.es.md)**

[← Back to Guide](../GUIDE.md)

---

## MiniSpec: Global Suite Configuration

The `MiniSpec` function returns the global test suite instance. It allows configuring global options and suite-level hooks (before/after all features):

```pascal
program MySpecs;

{$APPTYPE CONSOLE}

uses
  Daf.MiniSpec,
  // ... features ...

begin
  MiniSpec
    .Category('My Test Suite')  // Suite title
    
    .Before('Initialize environment', procedure
      begin
        // Runs ONCE, before all features
        DatabaseTestServer.Start;
      end)
    
    .After('Cleanup environment', procedure
      begin
        // Runs ONCE, after all features
        DatabaseTestServer.Stop;
      end);
  
  MiniSpec.Run;
end.
```

### UseSuiteContext: Global State

Similar to `UseFeatureContext` but at suite level. Context is shared between **all features**:

```pascal
type
  TGlobalContext = class
  public
    TestServer: TTestServer;
    SharedConfig: TConfig;
  end;

begin
  MiniSpec
    .UseSuiteContext<TGlobalContext>
    .Before('Start server', procedure
      begin
        // TGlobalContext is already created and available via injection
      end);
  
  MiniSpec.Run;
end.
```

World classes from each feature can inject this context with `[MiniInject]`.

### Execution Options

```pascal
MiniSpec
  .DryRun(True)     // List scenarios without executing
  .Pause(True)      // Wait for key at end
  .Reporter('live:port=9000');  // Configure reporter programmatically
```

---

## SpecContext: Execution Context Access

For advanced cases where you need access to execution context (current step, scenario, feature), MiniSpec provides the global `SpecContext` function.

```pascal
type
  TMyWorld = class  // Simple class, no special inheritance required
  public
    Value: Integer;
  end;

// In steps, access context via SpecContext:
.When('I execute something', procedure(World: TMyWorld)
  begin
    // Access current step
    WriteLn('Step: ', SpecContext.Step.Description);

    // Direct navigation to parents
    WriteLn('Scenario: ', SpecContext.Scenario.Description);
    WriteLn('Feature: ', SpecContext.Feature.Title);

    // Inside a scenario Rule is always assigned: one declared without an explicit Rule belongs
    // to an implicit Rule, whose Description is empty. It is nil only outside a scenario — in a
    // suite or feature hook.
    if SpecContext.Rule.Description <> '' then
      WriteLn('Rule: ', SpecContext.Rule.Description);
    
    // Current step's DataTable (nil if none)
    if Assigned(SpecContext.DataTable) then
      WriteLn('Rows: ', SpecContext.DataTable.RowCount);
  end)
```

### ISpecContext provides

| Property | Description |
|----------|-------------|
| `Suite` | The containing Suite |
| `Feature` | The containing Feature |
| `Rule` | The containing Rule. Always assigned inside a scenario — implicit, with an empty `Description`, when none was declared; nil outside a scenario |
| `Scenario` | The current scenario (or Example) |
| `Step` | The step being executed |
| `DataTable` | The step's data table (nil if none) |
| `SuiteContext` | The suite context, or nil if none was declared |
| `FeatureContext` | The feature context, or nil if none was declared |
| `ScenarioContext` | The World of the running scenario |
| `Injector` | The injector of this run, for resolving a context by type |

---

## FeatureContext: Shared State Between Scenarios

Unlike the **World** (which is created fresh for each scenario), the **FeatureContext** allows sharing state between all scenarios in a Feature. Useful for expensive-to-create resources:

```pascal
type
  TSharedContext = class
  public
    Connection: TDbConnection;
    Cache: TDictionary<string, TObject>;
  end;

  TScenarioWorld = class
  private
    [MiniInject] FShared: TSharedContext;  // Automatically injected
  public
    LocalData: string;
    property Shared: TSharedContext read FShared;
  end;

Feature('Database operations')
  .UseFeatureContext<TSharedContext>  // Creates ONE instance for entire Feature
  .UseWorld<TScenarioWorld>    // Each scenario gets its own World
  
  .Scenario('First query')
    .When('query data', procedure(W: TScenarioWorld)
      begin
        // W.Shared points to same TSharedContext
        W.Shared.Cache.Add('key', SomeObject);
      end)
  
  .Scenario('Second query')
    .When('use cached data', procedure(W: TScenarioWorld)
      begin
        // Access data created in previous scenario
        var Obj := W.Shared.Cache['key'];
      end)
```

### Lifecycle

- FeatureContext is created at Feature start
- Destroyed when Feature ends
- Each ScenarioWorld receives injection of same FeatureContext

---

[← Hooks](hooks.md) | [Next: Dependency Injection →](injection.md)
