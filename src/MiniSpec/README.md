# MiniSpec — BDD Testing Framework for Delphi

**🌍 Language: English | [Español](README.es.md)**

**Behavior-Driven Development (BDD) framework for Delphi** — Write executable specifications with fluent Gherkin-style syntax (Given/When/Then). A modern alternative to DUnit and DUnitX for test-driven development.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

> **Keywords**: Delphi testing, BDD Delphi, Gherkin Delphi, Cucumber for Delphi, unit testing, TDD, test framework, Object Pascal testing, RAD Studio testing

---

## Why MiniSpec?

- 🎯 **Native Gherkin syntax** — Given/When/Then directly in Delphi code
- 🔄 **Fluent API** — Natural chaining without external `.feature` files
- 🧪 **Type-safe** — Autocomplete and compile-time verification
- 📊 **Multiple reporters** — Console, JSON, JUnit (CI/CD), Live Dashboard
- 🏷️ **Powerful filtering** — By tags, features, scenarios, categories
- 💉 **Dependency injection** — Lightweight built-in system

---

## Quick Start

```pascal
unit Calculator.Add.Feat;

interface
implementation
uses Daf.MiniSpec, Calculator.Engine;

type
  TWorld = class
    Calculator: TCalculator;  // System Under Test
    A, B, Result: Integer;    // Parameters from Examples
  end;

initialization

Feature('Calculator Addition @arithmetic')
.UseWorld<TWorld>

.Background
  .Given('I have a calculator', procedure(W: TWorld)
    begin
      W.Calculator := TCalculator.Create;
    end)

.ScenarioOutline('Adding <A> and <B> should be <Result>')
  .Given('the numbers <A> and <B>')  // Auto-bound from Examples
  .When('they are added', procedure(W: TWorld)
    begin
      W.Calculator.Add(W.A, W.B);
    end)
  .&Then('the result is <Result>', procedure(W: TWorld)
    begin
      Expect(W.Calculator.Result).ToEqual(W.Result);
    end)
  .Examples(
    [['A', 'B', 'Result'],
     [1, 1, 2],
     [10, 20, 30],
     [5, -2, 3]])

end.
```

**Run:**

```bash
CalculatorSpecs.exe                    # Run all tests
CalculatorSpecs.exe -f "@arithmetic"   # Only tests tagged @arithmetic
CalculatorSpecs.exe -f "Feat:Calculator" # Filter by feature
CalculatorSpecs.exe -r live            # Real-time dashboard
CalculatorSpecs.exe -r junit:output=results.xml  # For CI/CD
```

---

## Key Features

| Feature | Description |
|---------|-------------|
| **Gherkin Vocabulary** | Feature, Scenario, Given, When, Then, And, But, Background, Rule |
| **Scenario Outline** | Data-driven tests with Examples table |
| **DataTables** | Inline structured data in steps |
| **Step Bindings** | Reusable steps with regex attributes |
| **Before/After** | Feature-level hooks |
| **Tags & Filters** | `@tag`, `Feat:`, `Scen:`, `Rule:`, `Cat:` |
| **Assertions** | Full `Expect()` API with matchers |
| **Reporters** | Console, JSON, JUnit, Gherkin, Live Dashboard |

---

## Documentation

| Resource | Description |
|----------|-------------|
| [**User Guide**](docs/GUIDE.md) | Complete documentation of all features |
| [**Testing Patterns**](docs/TESTING-PATTERNS.md) | BDD for unit, integration, and E2E tests |
| [**Samples**](../../samples/CalculatorSpecs/) | Working examples |
| [**Changelog**](../../CHANGELOG.md) | Version history |

---

## Reporters

### Console (default)
```
Feature: Calculator Addition @arithmetic
  ScenarioOutline: Adding <A> and <B> should be <Result>
    Examples:
        | A   | B   | Result |
      ✓ | 1   | 1   | 2      | (0 ms)
      ✓ | 10  | 20  | 30     | (0 ms)
      ✓ | 5   | -2  | 3      | (0 ms)

Pass: 3 | Fail: 0 | Skip: 0 | Total: 3 Specs in 1 Features | 0 ms | at 2026-01-30T14:57:07
```

### Live Dashboard
```bash
MySpecs.exe -r live:port=8080
```
Interactive real-time dashboard via Server-Sent Events.

### JUnit (CI/CD)
```bash
MySpecs.exe -r junit:output=test-results.xml
```
Compatible with GitHub Actions, GitLab CI, Jenkins, Azure DevOps.

---

## Requirements

- **Delphi 12 Athens** or later (requires multi-line strings `'''`)
- Windows (32/64 bit)

---

## Comparison with Other Frameworks

| Feature | MiniSpec | DUnitX | DUnit |
|---------|:--------:|:------:|:-----:|
| BDD / Gherkin syntax | ✅ | ❌ | ❌ |
| Given/When/Then | ✅ | ❌ | ❌ |
| Scenario Outline | ✅ | ❌ | ❌ |
| Fluent API | ✅ | Partial | ❌ |
| JUnit XML output | ✅ | ✅ | ❌ |
| Live Dashboard | ✅ | ❌ | ❌ |
| Tag filtering | ✅ | ✅ | ❌ |

---

## License

[MIT License](../../legal/LICENSE.md)

---

## Contributing

Found a bug? Have an idea? Open an [issue](https://github.com/delphi-daf/dafce/issues) or submit a PR.

---

<p align="center">
  <sub>Built with ❤️ for the Delphi community</sub><br>
  <sub><a href="https://github.com/Delphi-DAF/dafce">DAF Project</a> — Delphi Application Framework</sub>
</p>
