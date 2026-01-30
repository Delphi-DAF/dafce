# MiniSpec

**Framework BDD para Delphi** — Escribe especificaciones ejecutables con sintaxis fluida inspirada en Gherkin/Cucumber.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](../../legal/LICENSE.md)

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
uses Daf.MiniSpec;

type
  TWorld = class
    A, B, Result: Integer;
  end;

initialization

Feature('Calculator Addition')
.UseWorld<TWorld>

.Scenario('Add two numbers')
  .Given('I have entered 50 into the calculator', procedure(W: TWorld)
    begin
      W.A := 50;
    end)
  .&And('I have entered 70 into the calculator', procedure(W: TWorld)
    begin
      W.B := 70;
    end)
  .When('I press add', procedure(W: TWorld)
    begin
      W.Result := W.A + W.B;
    end)
  .&Then('the result should be 120', procedure(W: TWorld)
    begin
      Expect(W.Result).ToEqual(120);
    end)

end.
```

**Ejecutar:**

```bash
CalculatorSpecs.exe                    # Ejecutar todos los tests
CalculatorSpecs.exe -f "@unit"         # Solo tests con tag @unit
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

## Instalación

### Boss (recomendado)

```bash
boss install delphi-daf/dafce
```

### Manual

1. Clona el repositorio
2. Añade `src/MiniSpec` al Library Path de Delphi
3. Usa `Daf.MiniSpec` en tus units

---

## Estructura de Proyecto Recomendada

```
MyProject/
├── src/                    # Código de producción
├── specs/
│   ├── MySpecs.dpr        # Proyecto de specs
│   ├── Calculator.Add.Feat.pas
│   ├── Calculator.Mult.Feat.pas
│   └── Calculator.SpecHelpers.pas
```

---

## Documentación

| Recurso | Descripción |
|---------|-------------|
| [**Guía de Usuario**](GUIDE.md) | Documentación completa de todas las características |
| [**Samples**](../../samples/CalculatorSpecs/) | Ejemplos funcionales |
| [**Changelog**](../../CHANGELOG.md) | Historial de cambios |

---

## Reporters

### Consola (por defecto)
```
Feature: Calculator Addition
  Scenario: Add two numbers
    ✓ Given I have entered 50 into the calculator
    ✓ And I have entered 70 into the calculator
    ✓ When I press add
    ✓ Then the result should be 120

Pass: 4 | Fail: 0 | Skip: 0 | Total: 4 Steps
```

### Live Dashboard
```bash
MySpecs.exe -r live:port=8080
```
Dashboard interactivo en tiempo real via Server-Sent Events.

### JUnit (CI/CD)
```bash
MySpecs.exe -r junit:output=test-results.xml
```
Compatible con GitHub Actions, GitLab CI, Jenkins, Azure DevOps.

---

## Requisitos

- **Delphi 12 Athens** o superior (requiere multi-line strings `'''`)
- Windows (32/64 bit)

---

## Licencia

[Apache License 2.0](../../legal/LICENSE.md)

---

## Contribuir

¿Encontraste un bug? ¿Tienes una idea? Abre un [issue](https://github.com/delphi-daf/dafce/issues) o envía un PR.
