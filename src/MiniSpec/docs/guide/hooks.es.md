# Before / After Hooks

**🌍 Idioma: [English](hooks.md) | Español**

[← Volver a la Guía](../GUIDE.es.md)

---

Los hooks `Before` y `After` ejecutan código **una sola vez** por Feature, a diferencia de `Background` que ejecuta por cada scenario:

```pascal
Feature('Database Tests')
  .UseWorld<TDbWorld>
  
  .Before('Start test database', procedure
    begin
      GTestDb := TTestDatabase.Create;
      GTestDb.Start;
    end)
  
  .After('Stop test database', procedure
    begin
      GTestDb.Free;
    end)
  
  .Background
    .Given('a fresh transaction', procedure(W: TDbWorld)
      begin
        W.Tx := GTestDb.BeginTransaction;  // Se ejecuta por cada scenario
      end)
  
  .Scenario('Insert record')
    // ...
```

## Comparación

| Elemento | Ejecuta | Uso típico |
|----------|---------|------------|
| `Before` | Una vez antes de todos los scenarios | Iniciar servidor, crear BD |
| `After` | Una vez después de todos los scenarios | Liberar recursos costosos |
| `Background` | Antes de cada scenario | Preparar datos del test |

**Nota**: Los hooks no reciben World porque se ejecutan antes de que exista cualquier instancia.

---

[← Pending y NoAction](pending-noaction.es.md) | [Siguiente: Configuración →](configuration.es.md)
