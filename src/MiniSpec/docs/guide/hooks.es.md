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

## Hooks de la suite

`Before` y `After` declarados sobre `MiniSpec`, en el programa que ejecuta la suite, se ejecutan una
vez antes de la primera feature y una vez después de la última, también cuando alguna feature falla:

```pascal
MiniSpec
  .Before('Arrancar el servidor compartido', procedure
    begin
      // ...
    end)
  .After('Parar el servidor compartido', procedure
    begin
      // ...
    end)
  .Run;
```

## Cuando un hook lanza

En una feature, un `Before` que lanza detiene los `Before` que le siguen, y los escenarios de la
feature no se ejecutan. La feature cuenta como un fallo, y el informe muestra el mensaje bajo su
cabecera. Sus `After` se ejecutan igual.

En la suite, todos los `Before` se ejecutan aunque uno lance, y entonces no se ejecuta ninguna
feature. Todos los `After` se ejecutan también. Cada hook que lanza cuenta como un fallo, su mensaje
sale al principio del informe de consola, y la ejecución sale con 1.

---

[← Pending y NoAction](pending-noaction.es.md) | [Siguiente: Configuración →](configuration.es.md)
