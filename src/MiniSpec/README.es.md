# MiniSpec — Framework BDD para Delphi

**🌍 Idioma: [English](README.md) | Español**

**Framework de Behavior-Driven Development (BDD) para Delphi** — Escribe especificaciones ejecutables con sintaxis fluida estilo Gherkin (Given/When/Then). Una alternativa moderna a DUnit y DUnitX.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

---

## ¿Por qué MiniSpec?

- 🎯 **Sintaxis Gherkin nativa** — Given/When/Then directamente en código Delphi
- 🔄 **API fluida** — Encadenamiento natural sin archivos `.feature` externos
- 🧪 **Type-safe** — Autocompletado y verificación en tiempo de compilación
- 📊 **Múltiples reporters** — Consola, JSON, JUnit (CI/CD), Live Dashboard
- 🏷️ **Filtrado potente** — Por tags, features, scenarios, categorías
- 💉 **Inyección de dependencias** — Sistema ligero integrado

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
    A, B, Result: Integer;    // Parámetros de Examples
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
  .Given('the numbers <A> and <B>')  // Auto-binding desde Examples
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

**Ejecutar:**

```bash
CalculatorSpecs.exe                    # Ejecutar todos los tests
CalculatorSpecs.exe -f "@arithmetic"   # Solo tests con tag @arithmetic
CalculatorSpecs.exe -f "Feat:Calculator" # Filtrar por feature
CalculatorSpecs.exe -r live            # Dashboard en tiempo real
CalculatorSpecs.exe -r junit:output=results.xml  # Para CI/CD
```

---

## Características Principales

| Característica | Descripción |
|----------------|-------------|
| **Vocabulario Gherkin** | Feature, Scenario, Given, When, Then, And, But, Background, Rule |
| **Scenario Outline** | Tests data-driven con tabla de Examples |
| **DataTables** | Datos estructurados inline en steps |
| **Step Bindings** | Pasos reutilizables con atributos regex |
| **Before/After** | Hooks a nivel de Feature |
| **Tags & Filtros** | `@tag`, `Feat:`, `Scen:`, `Rule:`, `Cat:` |
| **Assertions** | API `Expect()` completa con matchers |
| **Reporters** | Console, JSON, JUnit, Gherkin, Live Dashboard |

---

## Documentación

| Recurso | Descripción |
|---------|-------------|
| [**Guía de Usuario**](docs/GUIDE.es.md) | Documentación completa de todas las características |
| [**Patrones de Testing**](docs/TESTING-PATTERNS.es.md) | BDD para tests unitarios, integración y E2E |
| [**Samples**](../../samples/CalculatorSpecs/) | Ejemplos funcionales |
| [**Changelog**](../../CHANGELOG.md) | Historial de cambios |

---

## Reporters

### Consola (por defecto)
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
MisSpecs.exe -r live:port=8080
```
Dashboard interactivo en tiempo real via Server-Sent Events.

### JUnit (CI/CD)
```bash
MisSpecs.exe -r junit:output=test-results.xml
```
Compatible con GitHub Actions, GitLab CI, Jenkins, Azure DevOps.

---

## Requisitos

- **Delphi 12 Athens** o superior (requiere multi-line strings `'''`)
- Windows (32/64 bit)

---

## Comparación con otros frameworks

| Característica | MiniSpec | DUnitX | DUnit |
|----------------|:--------:|:------:|:-----:|
| Sintaxis BDD / Gherkin | ✅ | ❌ | ❌ |
| Given/When/Then | ✅ | ❌ | ❌ |
| Scenario Outline | ✅ | ❌ | ❌ |
| API Fluida | ✅ | Parcial | ❌ |
| Salida JUnit XML | ✅ | ✅ | ❌ |
| Live Dashboard | ✅ | ❌ | ❌ |
| Filtrado por tags | ✅ | ✅ | ❌ |

---

## Licencia

[MIT License](../../legal/LICENSE.md)

---

## Contribuir

¿Encontraste un bug? ¿Tienes una idea? Abre un [issue](https://github.com/delphi-daf/dafce/issues) o envía un PR.

---

<p align="center">
  <sub>Hecho con ❤️ para la comunidad Delphi</sub><br>
  <sub><a href="https://github.com/Delphi-DAF/dafce">DAF Project</a> — Delphi Application Framework</sub>
</p>
