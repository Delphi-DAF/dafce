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

---

[← Pending and NoAction](pending-noaction.md) | [Next: Configuration →](configuration.md)
