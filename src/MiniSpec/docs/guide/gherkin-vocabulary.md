# Gherkin Vocabulary

**🌍 Language: English | [Español](gherkin-vocabulary.es.md)**

[← Back to Guide](../GUIDE.md)

---

Complete reference of Gherkin vocabulary in MiniSpec:

| Keyword | MiniSpec | Description |
|---------|----------|-------------|
| Feature | `Feature('...')` | Functionality being specified |
| Rule | `.Rule('...')` | Groups scenarios under a business rule |
| Background | `.Background` | Steps common to all scenarios; if one fails, the scenario fails without running its own steps |
| Scenario | `.Scenario('...')` | A concrete example of behavior |
| Scenario Outline | `.ScenarioOutline('...')` | Parameterized example with table |
| Examples | `.Examples([...])` | Data table for the Outline |
| DataTable | After step description | Structured data table for a step |
| Given | `.Given('...', proc)` | Establishes initial context |
| When | `.When('...', proc)` | The action being tested |
| Then | `.&Then('...', proc)` | The expected result (`&` required) |
| And | `.&And('...', proc)` | Additional step (`&` required) |
| But | `.But('...', proc)` | Exception or negation |
| @tag | In description | For filtering scenarios |

> 💡 The `&` before `Then` and `And` is required because they are reserved words in Delphi.

## What each phase claims

A scenario states the behaviour of **one event**. The three phases are that claim, split up — so
each one has a job, and a piece that does not belong to it:

| Phase | Claims | Does **not** do |
|---|---|---|
| `Given` | the state the event needs to make sense | trigger the event |
| `When` | the event, one and named | verify anything |
| `Then` | what can be observed afterwards | trigger anything |

**The test, scenario by scenario: name the event.** Which action's behaviour am I claiming here? If
you can name it, it belongs in the `When`. If you cannot, the scenario has no subject — and that is
not fixed by moving the code to another phase.

⚠️ **A scenario whose event sits outside the `When` still passes and fails**, so nothing turns red to
tell you. What you lose instead is this:

- **Event in the `Given`** — a failure of the event is reported as a setup failure, sending the
  reader to the wrong place. And the `Then` can only compare what the `Given` left behind, so the
  assertion measures the setup rather than the subject.
- **Event in the `Then`** — the failure is reported as a failed assertion. With several `Then`
  steps, either each one repeats the event — making them different events rather than one observed
  twice — or only the first runs it.

See [Pending and NoAction](pending-noaction.md) for what it means to leave a phase empty.

## Features with Narrative

A Feature can include a narrative that explains the **why**:

```pascal
Feature('''
Calculator - Division

  As a calculator user
  I need to divide numbers
  To solve mathematical problems

  @math @division
''')
```

The first line is the title. MiniSpec takes the rest as narrative. Tags can appear anywhere in the description but are recommended at the end.

## Category for Filtering

Assign categories to filter with `Cat:text`. You can use a string or a class as marker:

```pascal
// With string
Feature('Login')
  .Category('auth')
  .UseWorld<TLoginWorld>

// With marker class
type
  TIntegrationTest = class end;  // Empty class as marker

Feature('Database operations')
  .Category(TIntegrationTest)  // Uses QualifiedClassName: 'Unit1.TIntegrationTest'
  .UseWorld<TDbWorld>
```

Filter by category:

```bash
MyApp.exe -f "Cat:auth"              # Features with category 'auth'
MyApp.exe -f "Cat:Integration"       # Matches 'TIntegrationTest'
```

> 💡 Marker classes are useful for categories used across multiple files: the compiler catches typos.

---

[← Rules](rules.md) | [Next: Pending and NoAction →](pending-noaction.md)
