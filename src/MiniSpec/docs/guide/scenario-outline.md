# Multiple Examples with Scenario Outline

**🌍 Language: English | [Español](scenario-outline.es.md)**

[← Back to Guide](../GUIDE.md)

---

A single example is rarely enough. What about negative numbers? What about zero? You could write several almost identical scenarios... or use **Scenario Outline**:

```pascal
.ScenarioOutline('Adding <A> and <B> should give <Result>')
  .When('I add <A> and <B>', procedure(Ctx: TCalculatorWorld)
    begin
      Ctx.Result := Ctx.Calculator.Add(Ctx.A, Ctx.B);
    end)
  .&Then('the result is <Result>', procedure(Ctx: TCalculatorWorld)
    begin
      Expect(Ctx.Result).ToEqual(Ctx.Result);
    end)
  .Examples([
    ['A',   'B',   'Result'],  // Headers = World field names
    [ 2,     3,     5],        // Example 1
    [ 0,     0,     0],        // Example 2
    [-1,     1,     0],        // Example 3
    [100,  -50,    50]         // Example 4
  ])
```

**What's happening here?**

1. The **placeholders** `<A>`, `<B>`, `<Result>` in descriptions are replaced with concrete values
2. MiniSpec **reads values from the Examples table** and **injects them into corresponding World properties** (that's why World must have `A`, `B`, and `Result` properties)
3. The scenario runs **once per row** in the Examples table, **each with a new World** (like any normal scenario)

The output shows the outline with its examples table:

```
Feature: Calculator - Addition
  Background:
    ✓ Given I have a calculator (0 ms)
  Scenario Outline: Adding <A> and <B> should give <Result>
    When I add <A> and <B>
    Then the result is <Result>
    Examples:
        | A   | B   | Result |
      ✓ | 2   | 3   | 5      | (0 ms)
      ✓ | 0   | 0   | 0      | (0 ms)
      ✓ | -1  | 1   | 0      | (0 ms)
      ✓ | 100 | -50 | 50     | (0 ms)

Passed: 4 | Failed: 0 | Pending: 0 | Undefined: 0 | Skipped: 0 | Total: 4 Specs in 1 Features | 0 ms | at 2026-01-30T14:57:07
```

> 💡 **A normal Scenario is simply an Outline with a single implicit example.** There's no conceptual difference: both specify behavior with concrete examples. The Outline just makes the data table explicit.

## Display-only steps

A step defined without a lambda is display-only: it appears in the report but runs as a no-op. This is useful for the `Given` row in outlines where the data already comes from the Examples table via RTTI injection — no code needed:

```pascal
.ScenarioOutline('Adding <A> and <B> should give <Result>')
  .Given('the numbers <A> and <B>')   // display-only — values injected from Examples
  .When('they are added', procedure(Ctx: TCalculatorWorld)
    begin
      Ctx.Result := Ctx.A + Ctx.B;
    end)
  ...
```

The `Given` line appears in the output as context but does not execute any code. If a step binding matching the substituted description is registered, it runs instead.

---

[← The World](world.md) | [Next: DataTables →](datatables.md)
