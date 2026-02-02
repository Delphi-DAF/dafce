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

Esta frase simple es una **especificación ejecutable**. Con MiniSpec, la escribes casi literalmente:

```pascal
unit Calculator.Add.Feat.pas;

interface

implementation

uses
  Daf.MiniSpec,
  Calculator.Engine;  // Aquí está TCalculator, el sistema que especificamos

type
  TCalculatorWorld = class
  public
    Calculator: TCalculator;  // El SUT (System Under Test)
    Result: Integer;
    destructor Destroy; override;
  end;

destructor TCalculatorWorld.Destroy;
begin
  Calculator.Free;
  inherited;
end;

initialization

Feature('Calculadora - Suma')

.UseWorld<TCalculatorWorld>

.Background
  .Given('tengo una calculadora', procedure(Ctx: TCalculatorWorld)
    begin
      Ctx.Calculator := TCalculator.Create;
    end)

.Scenario('Sumar dos números positivos')
  .When('sumo 2 y 3', procedure(Ctx: TCalculatorWorld)
    begin
      Ctx.Result := Ctx.Calculator.Add(2, 3);
    end)
  .&Then('el resultado es 5', procedure(Ctx: TCalculatorWorld)
    begin
      Expect(Ctx.Result).ToEqual(5);
    end)

end.
```

La especificación define el comportamiento de `TCalculator` — el **SUT** (*System Under Test*). No nos importa *cómo* está implementado, solo *qué* debe hacer. La implementación está en `Calculator.Engine.pas`:

```pascal
unit Calculator.Engine;

interface

type
  TCalculator = class
  public
    function Add(A, B: Integer): Integer;
    // ... más operaciones
  end;

implementation
  // La implementación que hace pasar la especificación
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

Para que la especificación sea ejecutable, necesitas un programa mínimo:

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

Al ejecutar, verás en consola:

```
Feature: Calculadora - Suma
  Background:
    ✓ Given una calculadora (0 ms)
  Scenario: Sumar dos números positivos
    ✓ When sumo 2 y 3 (0 ms)
    ✓ Then el resultado es 5 (0 ms)

Pass: 1 | Fail: 0 | Skip: 0 | Total: 1 Specs in 1 Features | 0 ms | at 2026-01-30T14:57:07
```

**El ciclo es simple**: describes el comportamiento esperado → ejecutas → MiniSpec te dice si el código cumple la especificación.

---

[← Volver a la Guía](../GUIDE.es.md) | [Siguiente: El World →](world.es.md)
