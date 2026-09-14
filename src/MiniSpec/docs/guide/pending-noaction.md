# Pending and NoAction

**🌍 Language: English | [Español](pending-noaction.es.md)**

[← Back to Guide](../GUIDE.md)

---

MiniSpec provides two ways to mark steps that have no executable code:

| Method | Semantics | Result | Typical use |
|--------|-----------|--------|-------------|
| `.Pending` | "Pending implementation" | Skip + stops scenario | Work in progress |
| `.NoAction` | "No action required" | Pass + continues scenario | Purely descriptive steps |

## Pending

Indicates a step is pending implementation. The scenario is marked as Skip and **following steps are not executed**:

```pascal
.Scenario('Feature in development')
  .Given('a precondition', procedure(Ctx: TMyWorld) begin end)
  .When('something pending implementation')
    .Pending                          // Scenario Skip, doesn't continue
  .&Then('never executes', procedure(Ctx: TMyWorld)
    begin
      // This code does not execute
    end)
```

## NoAction

Indicates a step is purely descriptive and requires no code. The step passes automatically and **following steps DO execute**:

```pascal
.Scenario('With descriptive steps')
  .Given('the system is configured')  // Descriptive, system is already ready
    .NoAction
  .When('user logs in', procedure(Ctx: TMyWorld)
    begin
      Ctx.DoLogin;  // This code DOES execute
    end)
  .&Then('accesses dashboard', procedure(Ctx: TMyWorld)
    begin
      Expect(Ctx.IsLoggedIn).ToBeTrue;  // Also executes
    end)
```

## Where it lands

`.NoAction` replaces the body of the **last declared step**, whatever its kind:

```pascal
.Given('descriptive context').NoAction
.When('implicit action').NoAction
```

⚠️ **Nothing stops you from putting it on a `Then`, and nothing warns you.** The step becomes a
body that does nothing and passes, so the scenario keeps a `Then` that verifies nothing and stays
green — which is worse than a failure, because it looks like coverage.

```pascal
.&Then('verification').NoAction  // compiles, runs, verifies nothing
```

---

[← Gherkin Vocabulary](gherkin-vocabulary.md) | [Next: Hooks →](hooks.md)
