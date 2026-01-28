# MiniSpec v1.2.0

Framework BDD (Behavior-Driven Development) para Delphi, inspirado en Gherkin/Cucumber.

## Novedades v1.2.0

- **Archivo de configuración `MiniSpec.ini`**: Persistencia automática de opciones
- **Live Reporter**: Dashboard en tiempo real via SSE (Server-Sent Events)
- **Gherkin Reporter**: Exporta features a formato `.feature` estándar
- **Dry-run mode**: Lista escenarios sin ejecutarlos
- **Pause mode**: Espera tecla al finalizar
- **Filtros extendidos**: Además de `@tag`, soporta `Feat:texto`, `Scen:texto`, `Rule:texto`, `Cat:texto`
- **Category**: Permite agrupar features por categoría (ej: usando el nombre de la unit)
- **EndRule**: Permite volver de una Rule a la Feature para añadir más Rules o scenarios
- **Reporters modulares**: Cada reporter en su propia unit para mejor mantenibilidad
- **ISpecRunner**: Nueva interfaz para el runner con gestión de listeners

---

## Escribiendo Features

### Nomenclatura Recomendada

| Elemento | Convención | Ejemplo |
|----------|------------|---------|
| Proyecto | `<Dominio>Specs.dpr` | `CalculatorSpecs.dpr`, `EvaluationSpecs.dpr` |
| Unit de Feature | `<Feature>.Feat.pas` | `Calculator.Add.Feat.pas`, `Login.Feat.pas` |
| Unit de Helpers | `<Dominio>.SpecHelpers.pas` | `Calculator.SpecHelpers.pas` |

### Estructura Básica

```pascal
unit MiFeature.Feat.pas;

interface

implementation

uses
  Daf.MiniSpec;

type
  TMyWorld = class
  public
    Resultado: Integer;
    Input: string;
  end;

initialization

Feature('''
Mi Feature                         // <-- Primera línea = Título

  Como usuario                       // <-- Resto = Narrativa (opcional)
  Necesito hacer algo
  Para lograr un objetivo

  @unit @mi-tag                       // <-- Tags al final, en línea propia
''')

.UseWorld<TMyWorld>

.Background
  .Given('una precondición común', procedure(Ctx: TMyWorld)
    begin
      // Setup compartido por todos los escenarios
    end)

.Scenario('Mi primer escenario')
  .Given('una condición inicial', procedure(Ctx: TMyWorld)
    begin
      Ctx.Input := 'valor';
    end)
  .When('ejecuto la acción', procedure(Ctx: TMyWorld)
    begin
      Ctx.Resultado := 42;
    end)
  .&Then('obtengo el resultado esperado', procedure(Ctx: TMyWorld)
    begin
      Expect(Ctx.Resultado).ToEqual(42);
    end)

end.
```

### El World

El **World** es una clase que actúa como contexto compartido entre los pasos de un escenario. Cada escenario recibe una instancia nueva del World, permitiendo:

- **Compartir estado** entre Given, When y Then del mismo escenario
- **Aislar escenarios** entre sí (cada uno tiene su propia instancia)
- **Tipar el contexto** para autocompletado y verificación en tiempo de compilación

```pascal
type
  TCalculatorWorld = class
  public
    Calculator: TCalculator;
    Result: Integer;
    Error: string;
    destructor Destroy; override;  // Liberar recursos si es necesario
  end;

Feature('...')
.UseWorld<TCalculatorWorld>  // Cada escenario recibe un TCalculatorWorld nuevo
```

**Reutilización**: Un mismo World puede usarse en varias features relacionadas. Sin embargo, si las features son muy distintas, es mejor definir Worlds separados para mantener cada contexto limpio y enfocado:

```pascal
// Calculator.Add.Feat.pas
Feature('Suma').UseWorld<TCalculatorWorld>

// Login.Feat.pas
Feature('Login').UseWorld<TLoginWorld>

// Report.Feat.pas
Feature('Reportes').UseWorld<TReportWorld>
```

### TFeatureWorld: Acceso al Contexto de Ejecución

Para casos avanzados donde el World necesita acceder al contexto de ejecución (step actual, scenario, feature), MiniSpec proporciona `TFeatureWorld` como clase base opcional.

**Diseño**: El contexto está completamente oculto en la clase. Solo es accesible mediante cast explícito a `ISpecContext`. Esto mantiene la API del World limpia y evita exponer detalles internos del framework.

```pascal
type
  TMyWorld = class(TFeatureWorld)
  public
    // Tus campos y métodos normales
    Value: Integer;
  end;

// En los steps, acceder al contexto via ISpecContext:
.When('ejecuto algo', procedure(World: TMyWorld)
  var
    Ctx: ISpecContext;
  begin
    Ctx := World as ISpecContext;  // Cast explícito requerido

    // Acceso al step actual
    WriteLn('Step: ', Ctx.CurrentStep.Description);

    // Navegación directa a padres
    WriteLn('Scenario: ', Ctx.CurrentScenario.Description);
    WriteLn('Feature: ', Ctx.CurrentFeature.Title);

    // CurrentRule puede ser nil si no hay Rule explícita
    if Assigned(Ctx.CurrentRule) then
      WriteLn('Rule: ', Ctx.CurrentRule.Description);
  end)
```

**ISpecContext proporciona**:

| Propiedad/Método | Descripción |
|------------------|-------------|
| `CurrentStep` | El step que se está ejecutando |
| `CurrentScenario` | El scenario (o Example) actual |
| `CurrentRule` | La Rule contenedora (nil si no hay) |
| `CurrentFeature` | La Feature contenedora |

**Nota técnica**: `TFeatureWorld` hereda de `System.TNoRefCountObject` para evitar problemas con ARC (Automatic Reference Counting). Esto significa que las instancias del World se gestionan manualmente por el framework.

### Vocabulario Gherkin

| Gherkin | MiniSpec | Nota |
|---------|----------|------|
| Feature | `Feature('...')` | Descripción multilínea con `'''` |
| Rule | `.Rule('...')` | Agrupa escenarios relacionados |
| Background | `.Background` | Pasos comunes a escenarios del Feature/Rule |
| Scenario | `.Scenario('...')` | Un caso de prueba |
| Scenario Outline | `.ScenarioOutline('...')` | Con tabla de ejemplos |
| Examples | `.Examples([...])` | Tabla de datos para Outline |
| Given | `.Given('...', proc)` | Precondición |
| When | `.When('...', proc)` | Acción |
| Then | `.&Then('...', proc)` | Verificación (`&` requerido) |
| And | `.&And('...', proc)` | Paso adicional (`&` requerido) |
| But | `.But('...', proc)` | Paso negativo |
| @tags | `@tag` en descripción | Filtrado de tests |
| Doc Strings | `'''..'''` | Sintaxis nativa Delphi 12+ |

### Category para Agrupar Features

Las features pueden asignarse a una categoría para facilitar el filtrado con `Cat:texto`. Una forma conveniente es usar un tipo marker para extraer automáticamente el nombre de la unit:

```pascal
unit Calculator.Add.Feat;

interface

type
  TUnitMarker = class end;  // Tipo marker para identificar la unit

implementation
uses Daf.MiniSpec;

initialization

Feature('Calculator Addition')
  .Category(TUnitMarker)  // Extrae "Calculator.Add.Feat" del QualifiedClassName
  .UseWorld<TCalculatorWorld>
  // ...
```

Ahora puedes filtrar por categoría:

```bash
MiApp.exe -f "Cat:Calculator"     # Features cuya categoría contenga "Calculator"
MiApp.exe -f "Cat:Add.Feat"       # Features cuya categoría contenga "Add.Feat"
```

### EndRule para Volver a Feature

Cuando necesitas añadir Rules hermanas o volver a la Feature después de una Rule:

```pascal
Feature('...')
  .UseWorld<TWorld>
  
  .Rule('Primera regla')
    .Scenario('Test 1')
      // ...
    
  .EndRule  // Vuelve a Feature
  
  .Rule('Segunda regla')
    .Scenario('Test 2')
      // ...
```

### And / But

Usa `&And` y `But` para añadir pasos al grupo anterior (Given, When o Then):

```pascal
.Scenario('Con pasos adicionales')
  .Given('una condición', procedure(Ctx: TMyWorld) begin end)
  .&And('otra condición', procedure(Ctx: TMyWorld) begin end)   // Se añade a Given
  .When('ejecuto algo', procedure(Ctx: TMyWorld) begin end)
  .&Then('pasa esto', procedure(Ctx: TMyWorld) begin end)
  .&And('también esto', procedure(Ctx: TMyWorld) begin end)     // Se añade a Then
  .But('no pasa esto otro', procedure(Ctx: TMyWorld) begin end) // Se añade a Then
```

### Scenario Outline con Examples

```pascal
.ScenarioOutline('Sumar dos números')
  .Given('tengo el número <A>', procedure(Ctx: TMyWorld) begin end)
  .When('sumo <B>', procedure(Ctx: TMyWorld)
    begin
      Ctx.Resultado := Ctx.A + Ctx.B;
    end)
  .&Then('obtengo <Resultado>', procedure(Ctx: TMyWorld)
    begin
      Expect(Ctx.Resultado).ToEqual(Ctx.Esperado);
    end)
  .Examples([
    [ 'A',  'B',  'Esperado'],  // Headers (nombres de campos del World)
    [ 1,    2,    3],
    [ 5,    5,    10],
    [-1,    1,    0]
  ])
```

### Tags

Los tags pueden aparecer en cualquier punto del texto, pero **se recomienda añadirlos al final**, preferiblemente en línea propia, para mayor claridad:

```pascal
// En Feature: tags al final de la descripción
Feature('''
Mi Feature

  Como usuario necesito X para Y

  @unit @fast
''')

// En Scenario: tags al final de la descripción, en línea propia
.Scenario('''
  Escenario lento
  @slow @integration
''')

// También válido en línea propia dentro del Scenario Outline
.ScenarioOutline('''
  Sumar números
  @math @unit
''')
```

---

## Assertions (Expect)

### Igualdad

| Método | Descripción |
|--------|-------------|
| `Expect(valor).ToEqual(esperado)` | Verifica igualdad |
| `Expect(valor).ToNotEqual(otro)` | Verifica desigualdad |

### Nulos

| Método | Descripción |
|--------|-------------|
| `Expect(obj).ToBeNull` | Verifica que sea nil |
| `Expect(obj).ToNotBeNull` | Verifica que no sea nil |

### Comparaciones Numéricas

| Método | Descripción |
|--------|-------------|
| `Expect(num).ToBeGreaterThan(5)` | Mayor que |
| `Expect(num).ToBeGreaterOrEqual(5)` | Mayor o igual |
| `Expect(num).ToBeLessThan(10)` | Menor que |
| `Expect(num).ToBeLessOrEqual(10)` | Menor o igual |
| `Expect(num).ToBeBetween(1, 10)` | Entre dos valores (inclusivo) |

### Aproximaciones

| Método | Descripción |
|--------|-------------|
| `Expect(3.14159).ToBeCloseTo(3.14, 2)` | Igual con N decimales de precisión |
| `Expect(105).ToBeWithinPercent(100, 10)` | Dentro de ±N% del valor |

### Strings

| Método | Descripción |
|--------|-------------|
| `Expect(str).ToStartWith('Hello')` | Empieza con |
| `Expect(str).ToEndWith('World')` | Termina con |
| `Expect(str).ToContain('text')` | Contiene substring |
| `Expect(str).ToMatch('^\d+$')` | Coincide con regex |

### Booleanos

| Método | Descripción |
|--------|-------------|
| `Expect(cond).ToBeTrue` | Es verdadero |
| `Expect(cond).ToBeFalse` | Es falso |

### Excepciones

La acción se ejecuta en el step When y la excepción se captura automáticamente. En el Then se verifica con `Expect(Raised)`:

```pascal
.Scenario('División por cero')
  .Given('los números 10 y 0', procedure(World: TWorld)
    begin
      World.A := 10;
      World.B := 0;
    end)
  .When('se divide', procedure(World: TWorld)
    begin
      World.Calculator.Divide(World.A, World.B);  // Excepción capturada
    end)
  .&Then('lanza EDivByZero', procedure(World: TWorld)
    begin
      Expect(Raised).ToBe(EDivByZero);
    end);
```

| Método | Descripción |
|--------|-------------|
| `Expect(Raised).ToBeAny` | Se lanzó cualquier excepción |
| `Expect(Raised).ToBe(EMyException)` | Se lanzó tipo específico |
| `Expect(Raised).ToBe<EMyException>` | Se lanzó tipo específico (genérico) |
| `Expect(Raised).ToHaveMessage('texto')` | Mensaje contiene substring |
| `Expect(Raised).ToMatchMessage('patron')` | Mensaje coincide con regex |
| `Expect(Raised).ToBeNone` | No se lanzó excepción |

---

## Línea de Comandos

### Opciones Generales

| Opción | Descripción |
|--------|-------------|
| `-h, --help` | Muestra ayuda |
| `-f, --filter <expr>` | Filtra tests por expresión de tags |
| `-t, --tags` | Lista todos los tags con conteos |
| `-q, --query <expr>` | Muestra escenarios que coinciden (sin ejecutar) |
| `-r, --reporter <spec>` | Reporter con opciones (ver abajo) |
| `--pause` | Espera tecla al finalizar |
| `--dry-run` | Lista escenarios sin ejecutarlos |
| `--stacktrace` | Muestra stack trace completo en errores |

> **Nota**: `--stacktrace` requiere una librería de stack traces (JclDebug, MadExcept, EurekaLog)
> para mostrar información útil. Sin ella, `Exception.StackTrace` estará vacío.

### Expresiones de Filtro

```
@tag                    # Escenarios con el tag
~@tag                   # Escenarios SIN el tag
Feat:texto              # Feature title contiene texto
Scen:texto              # Scenario description contiene texto
Rule:texto              # Rule description contiene texto
Cat:texto               # Category contiene texto
@a and @b               # Ambos tags
@a or @b                # Cualquiera de los dos
(Feat:Login or @auth) and ~@slow  # Expresiones complejas
```

Ejemplos:

```bash
MiApp.exe -f "@unit"
MiApp.exe -f "@unit and ~@slow"
MiApp.exe -f "Feat:Calculator"
MiApp.exe -f "Cat:Login.Feat"
MiApp.exe -f "Scen:division and @arithmetic"
MiApp.exe -f "Rule:Division"
```

### Reporters

Sintaxis: `-r <nombre>:<opcion1>=<valor>,<opcion2>=<valor>,...`

| Reporter | Opciones | Ejemplo |
|----------|----------|---------|
| `console` | *(ninguna)* | `-r console` |
| `json` | `output=<file>` | `-r json:output=report.json` |
| `gherkin` | `output=<dir>` | `-r gherkin:output=features/` |
| `live` | `port=<num>`, `wait=<ms>` | `-r live:port=8080,wait=5000` |

**Live Reporter**: Por defecto espera 3 segundos para conexión del navegador. Usa `wait=0` para deshabilitar.

---

## Archivo de Configuración

MiniSpec crea automáticamente `MiniSpec.ini` en el directorio del ejecutable:

```ini
[minispec]
reporter=live
filter=@unit
pause=true

[reporter.live]
port=8080
wait=3000
```

Las opciones de línea de comandos tienen prioridad sobre el archivo.

---

## Archivos del Framework

| Archivo | Descripción |
|---------|-------------|
| `Daf.MiniSpec.pas` | API principal y runner |
| `Daf.MiniSpec.Types.pas` | Tipos e interfaces (ISpecItem, TSpecMatcher, etc.) |
| `Daf.MiniSpec.Builders.pas` | Builders fluent |
| `Daf.MiniSpec.Expects.pas` | Assertions |
| `Daf.MiniSpec.Reporter.pas` | Base de reporters (ISpecRunner, ISpecListener, TSpecRunner) |
| `Daf.MiniSpec.Reporter.Console.pas` | Reporter de consola |
| `Daf.MiniSpec.Reporter.Json.pas` | Reporter JSON |
| `Daf.MiniSpec.Reporter.Gherkin.pas` | Reporter Gherkin (.feature) |
| `Daf.MiniSpec.Reporter.Live.pas` | Reporter Live Dashboard (SSE) |
| `Daf.MiniSpec.LiveDashboard.pas` | HTML template del Live Dashboard |
| `Daf.MiniSpec.Filter.pas` | Parser de expresiones de filtro |
| `Daf.MiniSpec.TagFilter.pas` | Parser legacy de tags (deprecated) |
| `Daf.MiniSpec.Utils.pas` | Utilidades |
