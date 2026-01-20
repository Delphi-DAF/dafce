# MiniSpec v1.0.0

Framework BDD (Behavior-Driven Development) para Delphi, inspirado en Gherkin/Cucumber.

## Guía Rápida

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
  Mi Feature

    Como usuario
    Necesito hacer algo
    Para lograr un objetivo

    @unit @mi-tag
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

  .Scenario('Otro escenario')
    // ...

end.
```

### Vocabulario Gherkin Soportado

| Gherkin | MiniSpec | Nota |
|---------|----------|------|
| Feature | `Feature('...')` | Descripción multilínea con `'''` |
| Background | `.Background` | Pasos comunes a todos los escenarios |
| Scenario | `.Scenario('...')` | Un caso de prueba |
| Scenario Outline | `.ScenarioOutline('...')` | Con tabla de ejemplos |
| Examples | `.Examples([...])` | Tabla de datos |
| Given | `.Given('...', proc)` | Precondición |
| When | `.When('...', proc)` | Acción |
| Then | `.&Then('...', proc)` | Verificación (`&` requerido) |
| And | `.&And('...', proc)` | Paso adicional (`&` requerido) |
| But | `.But('...', proc)` | Paso negativo |
| @tags | `@tag` en descripción | Filtrado de tests |
| Doc Strings | `'''..'''` | Sintaxis nativa Delphi 12+ |

### Assertions (Expect)

```pascal
// Igualdad
Expect(valor).ToEqual(esperado);
Expect(valor).ToNotEqual(otro);

// Nulos
Expect(obj).ToBeNull;
Expect(obj).ToNotBeNull;

// Comparaciones numéricas
Expect(num).ToBeGreaterThan(5);
Expect(num).ToBeGreaterOrEqual(5);
Expect(num).ToBeLessThan(10);
Expect(num).ToBeLessOrEqual(10);
Expect(num).ToBeBetween(1, 10);

// Aproximaciones
Expect(3.14159).ToBeCloseTo(3.14, 2);      // 2 decimales
Expect(105).ToBeWithinPercent(100, 10);    // ±10%

// Strings
Expect(str).ToStartWith('Hello');
Expect(str).ToEndWith('World');
Expect(str).ToMatch('^\d+$');  // Regex

// Booleanos
Expect(cond).ToBeTrue;
Expect(cond).ToBeFalse;

// Excepciones
ExpectProc(procedure begin ... end).ToRaise;
ExpectProc(procedure begin ... end).ToRaiseType(EMyException);
ExpectProc(procedure begin ... end).ToRaiseWithMessage('texto');
ExpectProc(procedure begin ... end).ToNotRaise;
```

### Scenario Outline con Examples

```pascal
.ScenarioOutline('Sumar dos números')
  .Given('tengo el número <A>', procedure(Ctx: TMyWorld)
    begin
      // Ctx.A ya está asignado desde Examples
    end)
  .When('sumo <B>', procedure(Ctx: TMyWorld)
    begin
      Ctx.Resultado := Ctx.A + Ctx.B;
    end)
  .&Then('obtengo <Resultado>', procedure(Ctx: TMyWorld)
    begin
      Expect(Ctx.Resultado).ToEqual(Ctx.Esperado);
    end)
  .Examples([
  //  A,    B,    Esperado
    [ 'A',  'B',  'Esperado'],  // Headers (nombres de campos)
    [ 1,    2,    3],
    [ 5,    5,    10],
    [-1,    1,    0]
  ])
```

### Tags y Filtrado

Añade tags en la descripción del Feature o Scenario:

```pascal
Feature('''
  Mi Feature
    @unit @fast
''')

.Scenario('@slow Escenario lento')
```

Ejecutar con filtro:

```bash
# Filtrar por tags
MiApp.exe -f "@unit"
MiApp.exe -f "@unit and ~@slow"
MiApp.exe -f "(@fast or @critical) and ~@skip"

# Listar tags disponibles
MiApp.exe -t

# Consultar escenarios que coinciden
MiApp.exe -q "@integration"

# Ayuda
MiApp.exe -h
```

### Reporters

```bash
# Consola (default)
MiApp.exe

# HTML
MiApp.exe -r html -o report.html

# JSON
MiApp.exe -r json -o report.json
```

### And / But

Usa `&And` y `But` para añadir pasos al grupo anterior:

```pascal
.Scenario('Ejemplo con And y But')
  .Given('una condición', procedure(Ctx: TMyWorld)
    begin
      // ...
    end)
  .&And('otra condición adicional', procedure(Ctx: TMyWorld)
    begin
      // Se añade a Given
    end)
  .When('ejecuto algo', procedure(Ctx: TMyWorld)
    begin
      // ...
    end)
  .&Then('pasa esto', procedure(Ctx: TMyWorld)
    begin
      // ...
    end)
  .&And('también esto', procedure(Ctx: TMyWorld)
    begin
      // Se añade a Then
    end)
  .But('no pasa esto otro', procedure(Ctx: TMyWorld)
    begin
      // Se añade a Then
    end)
```

### Opciones de Línea de Comandos

| Opción | Descripción |
|--------|-------------|
| `-h, --help` | Muestra ayuda |
| `-f, --filter <expr>` | Filtra tests por expresión de tags |
| `-t, --tags` | Lista todos los tags con conteos |
| `-q, --query <expr>` | Muestra escenarios que coinciden (sin ejecutar) |
| `-r, --reporter <name>` | Formato: `html`, `json` (default: consola) |
| `-o, --output <file>` | Archivo de salida para el reporte |

### Expresiones de Tags

```
@tag                    # Escenarios con el tag
~@tag                   # Escenarios SIN el tag
@a and @b               # Ambos tags
@a or @b                # Cualquiera de los dos
(@a or @b) and ~@c      # Expresiones complejas
```

## Archivos

- `Daf.MiniSpec.pas` - API principal y runner
- `Daf.MiniSpec.Types.pas` - Tipos e interfaces
- `Daf.MiniSpec.Builders.pas` - Builders fluent
- `Daf.MiniSpec.Expects.pas` - Assertions
- `Daf.MiniSpec.Reporter.pas` - Reporters (console, HTML, JSON)
- `Daf.MiniSpec.TagFilter.pas` - Parser de expresiones de tags
- `Daf.MiniSpec.Utils.pas` - Utilidades
