# Before / After Hooks

**🌍 Language: English | [Español](hooks.es.md)**

[← Back to Guide](../GUIDE.md)

---

`Before` and `After` hooks execute code **once** per Feature, unlike `Background` which executes per scenario:

```pascal
Feature('Database Tests')
  .UseWorld<TDbWorld>
  
  .Before('Start test database', procedure
    begin
      GTestDb := TTestDatabase.Create;
      GTestDb.Start;
    end)
  
  .After('Stop test database', procedure
    begin
      GTestDb.Free;
    end)
  
  .Background
    .Given('a fresh transaction', procedure(W: TDbWorld)
      begin
        W.Tx := GTestDb.BeginTransaction;  // Executes per scenario
      end)
  
  .Scenario('Insert record')
    // ...
```

## Comparison

| Element | Executes | Typical use |
|---------|----------|-------------|
| `Before` | Once before all scenarios | Start server, create DB |
| `After` | Once after all scenarios | Free expensive resources |
| `Background` | Before each scenario | Prepare test data |

**Note**: Hooks don't receive World because they execute before any instance exists.

## Hooks of the suite

`Before` and `After` declared on `MiniSpec`, in the program that runs the suite, execute once before
the first feature and once after the last one, also when a feature fails:

```pascal
MiniSpec
  .Before('Start the shared server', procedure
    begin
      // ...
    end)
  .After('Stop the shared server', procedure
    begin
      // ...
    end)
  .Run;
```

## When a hook raises

In a feature, a `Before` that raises stops the `Before` hooks after it, and the scenarios of the
feature do not run. The feature counts as a failure, and the report shows the message under its
heading. Its `After` hooks run all the same.

In the suite, every `Before` runs even when one raises, and then no feature runs. Every `After` runs
too. Each hook that raises counts as one failure, its message appears at the start of the console
report, and the run exits with 1.

---

[← Pending and NoAction](pending-noaction.md) | [Next: Configuration →](configuration.md)
