# The World: Sharing State

**🌍 Language: English | [Español](world.es.md)**

[← Back to Guide](../GUIDE.md)

---

The **World** is the glue between scenario steps. Each step receives the same instance, allowing:

- **Given** prepares initial state
- **When** modifies that state
- **Then** verifies the resulting state

MiniSpec **creates and destroys the World automatically** for each scenario. That's why your World class must have a **parameterless constructor** (the default `TObject` constructor is sufficient). If you need to free resources, override `Destroy`:

```pascal
type
  TCalculatorWorld = class
  public
    Calculator: TCalculator;  // The object under test
    destructor Destroy; override;
  end;

destructor TCalculatorWorld.Destroy;
begin
  Calculator.Free;
  inherited;
end;
```

**Each scenario receives a new World**. This ensures scenarios are isolated from each other: what one scenario does doesn't affect others.

```pascal
Feature('...')
.UseWorld<TCalculatorWorld>  // MiniSpec creates one instance per scenario

.Scenario('First test')      // World #1
  // ...

.Scenario('Second test')     // World #2 (independent from previous)
  // ...
```

---

[← Getting Started](getting-started.md) | [Next: Scenario Outline →](scenario-outline.md)
