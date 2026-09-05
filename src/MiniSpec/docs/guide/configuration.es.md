# Configuración Global

**🌍 Idioma: [English](configuration.md) | Español**

[← Volver a la Guía](../GUIDE.es.md)

---

## MiniSpec: Configuración Global de la Suite

La función `MiniSpec` devuelve la instancia global de la suite de tests. Permite configurar opciones globales y hooks a nivel de suite (antes/después de todas las features):

```pascal
program MySpecs;

{$APPTYPE CONSOLE}

uses
  Daf.MiniSpec,
  // ... features ...

begin
  MiniSpec
    .Category('Mi Suite de Tests')  // Título de la suite
    
    .Before('Inicializar entorno', procedure
      begin
        // Se ejecuta UNA vez, antes de todas las features
        DatabaseTestServer.Start;
      end)
    
    .After('Limpiar entorno', procedure
      begin
        // Se ejecuta UNA vez, después de todas las features
        DatabaseTestServer.Stop;
      end);
  
  MiniSpec.Run;
end.
```

### UseSuiteContext: Estado Global

Similar a `UseFeatureContext` pero a nivel de toda la suite. El contexto se comparte entre **todas las features**:

```pascal
type
  TGlobalContext = class
  public
    TestServer: TTestServer;
    SharedConfig: TConfig;
  end;

begin
  MiniSpec
    .UseSuiteContext<TGlobalContext>
    .Before('Start server', procedure
      begin
        // TGlobalContext ya está creado y disponible via inyección
      end);
  
  MiniSpec.Run;
end.
```

Los World de cada feature pueden inyectar este contexto con `[MiniInject]`.

### Opciones de Ejecución

```pascal
MiniSpec
  .DryRun(True)     // Lista escenarios sin ejecutarlos
  .Pause(True)      // Espera tecla al finalizar
  .Reporter('live:port=9000');  // Configura reporter programáticamente
```

---

## SpecContext: Acceso al Contexto de Ejecución

Para casos avanzados donde necesitas acceder al contexto de ejecución (step actual, scenario, feature), MiniSpec proporciona la función global `SpecContext`.

```pascal
type
  TMyWorld = class  // Clase simple, sin herencia especial requerida
  public
    Value: Integer;
  end;

// En los steps, acceder al contexto via SpecContext:
.When('ejecuto algo', procedure(World: TMyWorld)
  begin
    // Acceso al step actual
    WriteLn('Step: ', SpecContext.Step.Description);

    // Navegación directa a padres
    WriteLn('Scenario: ', SpecContext.Scenario.Description);
    WriteLn('Feature: ', SpecContext.Feature.Title);

    // Dentro de un escenario Rule siempre está asignada: uno declarado sin Rule explícita
    // pertenece a una Rule implícita, cuya Description está vacía. Solo es nil fuera de un
    // escenario — en un hook de suite o de feature.
    if SpecContext.Rule.Description <> '' then
      WriteLn('Rule: ', SpecContext.Rule.Description);
    
    // DataTable del step actual (nil si no tiene)
    if Assigned(SpecContext.DataTable) then
      WriteLn('Rows: ', SpecContext.DataTable.RowCount);
  end)
```

### ISpecContext proporciona

| Propiedad | Descripción |
|-----------|-------------|
| `Suite` | La Suite contenedora |
| `Feature` | La Feature contenedora |
| `Rule` | La Rule contenedora. Siempre asignada dentro de un escenario —implícita, con `Description` vacía, si no se declaró ninguna—; nil fuera de un escenario |
| `Scenario` | El scenario (o Example) actual |
| `Step` | El step que se está ejecutando |
| `DataTable` | La tabla de datos del step (nil si no tiene) |
| `SuiteContext` | El contexto de suite, o nil si no se declaró ninguno |
| `FeatureContext` | El contexto de feature, o nil si no se declaró ninguno |
| `ScenarioContext` | El World del escenario en curso |
| `Injector` | El inyector de esta ejecución, para resolver un contexto por tipo |

---

## FeatureContext: Estado Compartido entre Escenarios

A diferencia del **World** (que se crea nuevo para cada escenario), el **FeatureContext** permite compartir estado entre todos los escenarios de una Feature. Es útil para recursos costosos de crear:

```pascal
type
  TSharedContext = class
  public
    Connection: TDbConnection;
    Cache: TDictionary<string, TObject>;
  end;

  TScenarioWorld = class
  private
    [MiniInject] FShared: TSharedContext;  // Inyectado automáticamente
  public
    LocalData: string;
    property Shared: TSharedContext read FShared;
  end;

Feature('Database operations')
  .UseFeatureContext<TSharedContext>  // Crea UNA instancia para toda la Feature
  .UseWorld<TScenarioWorld>    // Cada escenario recibe su propio World
  
  .Scenario('First query')
    .When('query data', procedure(W: TScenarioWorld)
      begin
        // W.Shared apunta al mismo TSharedContext
        W.Shared.Cache.Add('key', SomeObject);
      end)
  
  .Scenario('Second query')
    .When('use cached data', procedure(W: TScenarioWorld)
      begin
        // Accede a datos creados en el escenario anterior
        var Obj := W.Shared.Cache['key'];
      end)
```

### Ciclo de Vida

- El FeatureContext se crea al inicio de la Feature
- Se destruye al finalizar la Feature
- Cada ScenarioWorld recibe la inyección del mismo FeatureContext

---

[← Hooks](hooks.es.md) | [Siguiente: Inyección de Dependencias →](injection.es.md)
