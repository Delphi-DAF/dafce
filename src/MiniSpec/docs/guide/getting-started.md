# Getting Started with MiniSpec

**🌍 Language: English | [Español](getting-started.es.md)**

[← Back to Guide](../GUIDE.md)

---

## Why MiniSpec?

Imagine you could write your application **requirements** in a way that:

1. **Anyone can read them** — developers, testers, analysts, clients
2. **They run automatically** — verifying the code meets the specification
3. **They never get outdated** — because they ARE the test

This idea has a name: **Specification by Example**. Instead of requirement documents that nobody reads and become stale after the first change, you write **concrete examples** of expected behavior. These examples become **executable specifications**: documentation that validates itself automatically.

This practice is the heart of **[BDD (Behavior-Driven Development)](https://cucumber.io/docs/bdd/)**, a widely adopted methodology for capturing requirements collaboratively. The vocabulary BDD uses is called **[Gherkin](https://cucumber.io/docs/gherkin/)** and you'll recognize it by its keywords: *Feature*, *Scenario*, *Given*, *When*, *Then*. Tools like [Cucumber](https://cucumber.io/), [SpecFlow](https://specflow.org/), and [Behave](https://behave.readthedocs.io/) have been demonstrating the value of this approach for years.

MiniSpec brings BDD to Delphi, using Gherkin vocabulary natively:

- **Feature**: The functionality you're specifying
- **Scenario**: A concrete example of how it should behave
- **Given-When-Then**: The pattern that structures each example

The result is code that **documents, specifies, and verifies** at the same time.

---

## Your First Specification

Imagine you're developing a calculator. Before writing code, you describe **how it should behave**:

> *"When I add 2 and 3, the result should be 5"*

This simple sentence is an **executable specification**. With MiniSpec, you write it almost literally:

```pascal
unit Calculator.Add.Feat.pas;

interface

implementation

uses
  Daf.MiniSpec,
  Calculator.Engine;  // Here's TCalculator, the system we're specifying

type
  TCalculatorWorld = class
  public
    Calculator: TCalculator;  // The SUT (System Under Test)
    Result: Integer;
    destructor Destroy; override;
  end;

destructor TCalculatorWorld.Destroy;
begin
  Calculator.Free;
  inherited;
end;

initialization

Feature('Calculator - Addition')

.UseWorld<TCalculatorWorld>

.Background
  .Given('I have a calculator', procedure(Ctx: TCalculatorWorld)
    begin
      Ctx.Calculator := TCalculator.Create;
    end)

.Scenario('Add two positive numbers')
  .When('I add 2 and 3', procedure(Ctx: TCalculatorWorld)
    begin
      Ctx.Result := Ctx.Calculator.Add(2, 3);
    end)
  .&Then('the result is 5', procedure(Ctx: TCalculatorWorld)
    begin
      Expect(Ctx.Result).ToEqual(5);
    end)

end.
```

The specification defines the behavior of `TCalculator` — the **SUT** (*System Under Test*). We don't care *how* it's implemented, only *what* it should do. The implementation is in `Calculator.Engine.pas`:

```pascal
unit Calculator.Engine;

interface

type
  TCalculator = class
  public
    function Add(A, B: Integer): Integer;
    // ... more operations
  end;

implementation
  // The implementation that makes the specification pass
end.
```

**What's happening here?**

1. **Feature** declares what functionality we're specifying
2. **UseWorld** defines a class containing the SUT and test data
3. **Background** prepares common context for all scenarios (creates the calculator)
4. **Scenario** describes a concrete use case
5. **When** executes the action we want to test (uses the SUT)
6. **Then** verifies the result is as expected

> 💡 Steps follow the **Given-When-Then** pattern: *"Given that... When... Then..."*. This simple but powerful pattern forces you to think about preconditions, actions, and results.

---

## Running the Specification

To make the specification executable, you need a minimal program:

```pascal
program CalculatorSpecs;

{$APPTYPE CONSOLE}

uses
  Daf.MiniSpec,
  Calculator.Add.Feat in 'Calculator.Add.Feat.pas';

begin
  MiniSpec.Run;
end.
```

When running, you'll see in the console:

```
Feature: Calculator - Addition
  Background:
    ✓ Given I have a calculator (0 ms)
  Scenario: Add two positive numbers
    ✓ When I add 2 and 3 (0 ms)
    ✓ Then the result is 5 (0 ms)

Pass: 1 | Fail: 0 | Skip: 0 | Total: 1 Specs in 1 Features | 0 ms | at 2026-01-30T14:57:07
```

**The cycle is simple**: describe expected behavior → run → MiniSpec tells you if the code meets the specification.

---

[← Back to Guide](../GUIDE.md) | [Next: The World →](world.md)
