# Múltiples Ejemplos con Scenario Outline

**🌍 Idioma: [English](scenario-outline.md) | Español**

[← Volver a la Guía](../GUIDE.es.md)

---

Un solo ejemplo rara vez es suficiente. ¿Qué pasa con números negativos? ¿Con cero? Podrías escribir varios escenarios casi idénticos... o usar **Scenario Outline**:

```pascal
.ScenarioOutline('Sumar <A> y <B> debería dar <Resultado>')
  .When('sumo <A> y <B>', procedure(Ctx: TCalculatorWorld)
    begin
      Ctx.Result := Ctx.Calculator.Add(Ctx.A, Ctx.B);
    end)
  .&Then('el resultado es <Resultado>', procedure(Ctx: TCalculatorWorld)
    begin
      Expect(Ctx.Result).ToEqual(Ctx.Resultado);
    end)
  .Examples([
    ['A',   'B',   'Resultado'],  // Headers = nombres de campos del World
    [ 2,     3,     5],           // Ejemplo 1
    [ 0,     0,     0],           // Ejemplo 2
    [-1,     1,     0],           // Ejemplo 3
    [100,  -50,    50]            // Ejemplo 4
  ])
```

**¿Qué ocurre aquí?**

1. Los **placeholders** `<A>`, `<B>`, `<Resultado>` en las descripciones se reemplazan por los valores concretos
2. MiniSpec **lee los valores de la tabla Examples** y los **inyecta en las propiedades correspondientes del World** (por eso el World debe tener propiedades `A`, `B` y `Resultado`)
3. El escenario se ejecuta **una vez por cada fila** de la tabla Examples, **cada uno con un World nuevo** (como cualquier escenario normal)

El resultado muestra el outline con su tabla de ejemplos:

```
Feature: Calculadora - Suma
  Background:
    ✓ Given tengo una calculadora (0 ms)
  ✓ Scenario Outline: Sumar <A> y <B> debería dar <Resultado> (0 ms)
    When sumo <A> y <B>
    Then el resultado es <Resultado>
    Examples:
        | A   | B   | Resultado |
      ✓ | 2   | 3   | 5         | (0 ms)
      ✓ | 0   | 0   | 0         | (0 ms)
      ✓ | -1  | 1   | 0         | (0 ms)
      ✓ | 100 | -50 | 50        | (0 ms)

Pass: 4 | Fail: 0 | Skip: 0 | Total: 4 Specs in 1 Features | 0 ms | at 2026-01-30T14:57:07
```

> 💡 **Un Scenario normal es simplemente un Outline con un solo ejemplo implícito.** No hay diferencia conceptual: ambos especifican comportamiento con ejemplos concretos. El Outline solo hace explícita la tabla de datos.

---

[← El World](world.es.md) | [Siguiente: DataTables →](datatables.es.md)
