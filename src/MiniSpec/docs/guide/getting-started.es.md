# Primeros Pasos con MiniSpec

**🌍 Idioma: [English](getting-started.md) | Español**

[← Volver a la Guía](../GUIDE.es.md)

---

## ¿Por qué MiniSpec?

Imagina que pudieras escribir los **requisitos** de tu aplicación de forma que:

1. **Cualquiera pueda leerlos** — desarrolladores, testers, analistas, clientes
2. **Se ejecuten automáticamente** — verificando que el código cumple lo especificado
3. **Nunca queden desactualizados** — porque son el propio test

Esta idea tiene un nombre: **Especificación por Ejemplos** (*Specification by Example*). En lugar de documentos de requisitos que nadie lee y que se desactualizan al primer cambio, escribes **ejemplos concretos** del comportamiento esperado. Estos ejemplos se convierten en **especificaciones ejecutables**: documentación que se valida automáticamente.

Esta práctica es el corazón de **[BDD (Behavior-Driven Development)](https://cucumber.io/docs/bdd/)**, una metodología ampliamente adoptada en la industria para capturar requisitos de forma colaborativa. El vocabulario que usa BDD se llama **[Gherkin](https://cucumber.io/docs/gherkin/)** y lo reconocerás por sus palabras clave: *Feature*, *Scenario*, *Given*, *When*, *Then*. Herramientas como [Cucumber](https://cucumber.io/), [SpecFlow](https://specflow.org/) y [Behave](https://behave.readthedocs.io/) llevan años demostrando el valor de este enfoque.

MiniSpec trae BDD a Delphi, usando el vocabulario Gherkin de forma nativa:

- **Feature**: La funcionalidad que estás especificando
- **Scenario**: Un ejemplo concreto de cómo debe comportarse
- **Given-When-Then**: El patrón que estructura cada ejemplo

El resultado es código que **documenta, especifica y verifica** al mismo tiempo.

---

## Tu Primera Especificación

Imagina que estás desarrollando una calculadora. Antes de escribir código, describes **cómo debería comportarse**:

> *"Cuando sumo 2 y 3, el resultado debería ser 5"*

Esta frase simple es una **especificación ejecutable**. Con MiniSpec, la escribes casi literalmente (el código del sample está en inglés):

<!-- quickstart: src/MiniSpec/samples/FirstSpec/Addition.Feat.pas -->

```pascal
unit Addition.Feat;

interface

implementation

uses
  Daf.MiniSpec,
  Calculator.Engine;  // Here's TCalculator, the system we're specifying

type
  TCalculatorWorld = class
  public
    Calculator: TCalculator;  // The SUT (System Under Test)
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
      Ctx.Calculator.Add(2, 3);
    end)
  .&Then('the result is 5', procedure(Ctx: TCalculatorWorld)
    begin
      Expect(Ctx.Calculator.Result).ToEqual(5);
    end)

end.
```

La especificación define el comportamiento de `TCalculator` — el **SUT** (*System Under Test*). No nos importa *cómo* está implementado, solo *qué* debe hacer. La implementación es `Calculator.Engine.pas`, del sample `samples/CalculatorSpecs/`:

<!-- quickstart: src/MiniSpec/samples/CalculatorSpecs/Calculator.Engine.pas -->

```pascal
unit Calculator.Engine;

interface

uses
  System.SysUtils;

type
  TCalculator = class
  strict private
    FResult: Integer;
  public
    procedure Add(A, B: Integer);
    procedure Subtract(A, B: Integer);
    procedure Mult(A, B: Integer);
    procedure Divide(A, B: Integer);
    property Result: Integer read FResult;
  end;

implementation

procedure TCalculator.Add(A, B: Integer);
begin
  FResult := A + B;
end;

procedure TCalculator.Subtract(A, B: Integer);
begin
  FResult := A - B;
end;

procedure TCalculator.Mult(A, B: Integer);
begin
  FResult := A * B;
end;

procedure TCalculator.Divide(A, B: Integer);
begin
  if B = 0 then
    raise EDivByZero.Create('Division by zero');
  FResult := A div B;
end;

end.
```

**¿Qué está pasando aquí?**

1. **Feature** declara qué funcionalidad estamos especificando
2. **UseWorld** define una clase que contiene el SUT y los datos del test
3. **Background** prepara el contexto común a todos los escenarios (crea la calculadora)
4. **Scenario** describe un caso concreto de uso
5. **When** ejecuta la acción que queremos probar (usa el SUT)
6. **Then** verifica que el resultado sea el esperado

> 💡 Los pasos siguen el patrón **Given-When-Then**: *"Dado que... Cuando... Entonces..."*. Este patrón simple pero poderoso te obliga a pensar en precondiciones, acciones y resultados.

---

## Ejecutando la Especificación

Para que la especificación sea ejecutable, necesitas un programa mínimo. Junto con la feature de arriba, es el sample `samples/FirstSpec/`:

<!-- quickstart: src/MiniSpec/samples/FirstSpec/FirstSpec.dpr -->

```pascal
program FirstSpec;

{$APPTYPE CONSOLE}

uses
  Daf.MiniSpec,
  Calculator.Engine in '..\CalculatorSpecs\Calculator.Engine.pas',
  Addition.Feat in 'Addition.Feat.pas';

begin
  MiniSpec.Run;
end.
```

Al ejecutar, verás en consola:

```
Feature: Calculator - Addition
  Background: 
    ✓ Given I have a calculator (0 ms)
  Scenario: Add two positive numbers
      ✓ When I add 2 and 3 (0 ms)
      ✓ Then the result is 5 (2 ms)

Passed: 1 | Failed: 0 | Pending: 0 | Undefined: 0 | Skipped: 0 | Total: 1 Specs in 1 Features | 0 ms | at 2026-09-30T11:15:11
```

**El ciclo es simple**: describes el comportamiento esperado → ejecutas → MiniSpec te dice si el código cumple la especificación.

---

[← Volver a la Guía](../GUIDE.es.md) | [Siguiente: El World →](world.es.md)
