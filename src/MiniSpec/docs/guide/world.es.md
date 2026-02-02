# El World: Compartiendo Estado

**🌍 Idioma: [English](world.md) | Español**

[← Volver a la Guía](../GUIDE.es.md)

---

El **World** es el pegamento entre los pasos de un escenario. Cada paso recibe la misma instancia, permitiendo:

- **Given** prepara el estado inicial
- **When** modifica ese estado
- **Then** verifica el estado resultante

MiniSpec **crea y destruye el World automáticamente** para cada escenario. Por eso tu clase World debe tener un **constructor sin parámetros** (el constructor por defecto de `TObject` es suficiente). Si necesitas liberar recursos, sobrescribe `Destroy`:

```pascal
type
  TCalculatorWorld = class
  public
    Calculator: TCalculator;  // El objeto bajo prueba
    Result: Integer;          // El resultado a verificar
    destructor Destroy; override;
  end;

destructor TCalculatorWorld.Destroy;
begin
  Calculator.Free;
  inherited;
end;
```

**Cada escenario recibe un World nuevo**. Esto garantiza que los escenarios estén aislados entre sí: lo que hace un escenario no afecta a otros.

```pascal
Feature('...')
.UseWorld<TCalculatorWorld>  // MiniSpec crea una instancia por escenario

.Scenario('Primer test')     // World #1
  // ...

.Scenario('Segundo test')    // World #2 (independiente del anterior)
  // ...
```

---

[← Primeros Pasos](getting-started.es.md) | [Siguiente: Scenario Outline →](scenario-outline.es.md)
