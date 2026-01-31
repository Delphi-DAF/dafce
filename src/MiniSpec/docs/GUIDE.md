# MiniSpec - User Guide

**🌍 Language: English | [Español](GUIDE.es.md)**

Welcome to the MiniSpec guide, the BDD framework for Delphi.

> 📖 **See also**: [Testing Patterns](TESTING-PATTERNS.md) — How to use BDD for unit, integration, and E2E tests.

---

## Table of Contents

### Getting Started
- [**Getting Started**](guide/getting-started.md) — Why MiniSpec? Your first specification. Running specs.

### Fundamental Concepts
- [**The World**](guide/world.md) — Sharing state between scenario steps
- [**Scenario Outline**](guide/scenario-outline.md) — Multiple examples with data tables
- [**DataTables**](guide/datatables.md) — Structured data for complex steps

### Organization and Reuse
- [**Step Bindings**](guide/step-bindings.md) — Reusable steps with regex patterns
- [**Tags and Filtering**](guide/tags-filtering.md) — Categorize and filter scenarios
- [**Rules**](guide/rules.md) — Group scenarios by business rule

### Verifications
- [**Assertions (Expect)**](guide/assertions.md) — Fluent API for verifications and exceptions

### Vocabulary and Markers
- [**Gherkin Vocabulary**](guide/gherkin-vocabulary.md) — Complete keyword reference
- [**Pending and NoAction**](guide/pending-noaction.md) — Unimplemented or descriptive steps

### Advanced Configuration
- [**Hooks (Before/After)**](guide/hooks.md) — Feature-level setup/teardown code
- [**Global Configuration**](guide/configuration.md) — MiniSpec, SpecContext, FeatureContext
- [**Dependency Injection**](guide/injection.md) — Built-in [Inject] system

### Execution and Output
- [**Reporters**](guide/reporters.md) — Console, JSON, JUnit, Gherkin, Live Dashboard
- [**Command Line**](guide/cli.md) — Execution options

---

## Quick Reference

| Concept | Usage |
|---------|-------|
| Feature | `Feature('...')` — Functionality to specify |
| Scenario | `.Scenario('...')` — Concrete example |
| Given/When/Then | `.Given()`, `.When()`, `.&Then()` — Steps |
| ScenarioOutline | `.ScenarioOutline('...').Examples([...])` — Data-driven |
| Background | `.Background` — Common setup |
| Rule | `.Rule('...')` — Group by rule |
| Tags | `@tag` in description — Filtering |
| World | `.UseWorld<T>` — Scenario state |

---

<p align="center">
  <sub><a href="../README.md">← Back to README</a></sub>
</p>

