
# MiniSpec - Guía de Usuario

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

## Tabla de Contenidos

- [Tu Primera Especificación](#tu-primera-especificación)
- [Ejecutando la Especificación](#ejecutando-la-especificación)
- [El World: Compartiendo Estado](#el-world-compartiendo-estado)
- [Múltiples Ejemplos con Scenario Outline](#múltiples-ejemplos-con-scenario-outline)
- [DataTables: Datos Estructurados](#datatables-datos-estructurados)
- [Step Bindings: Organizando Pasos Complejos](#step-bindings-organizando-pasos-complejos)
- [Tags y Filtrado](#tags-y-filtrado)
- [Assertions (Expect)](#assertions-expect)
- [Rules: Agrupando por Regla de Negocio](#rules-agrupando-por-regla-de-negocio)
- [Vocabulario Gherkin](#vocabulario-gherkin)
- [Pending y NoAction](#pending-y-noaction)
- [Before / After Hooks](#before--after-hooks)
- [MiniSpec: Configuración Global](#minispec-configuración-global-de-la-suite)
- [SpecContext: Acceso al Contexto](#speccontext-acceso-al-contexto-de-ejecución)
- [FeatureContext: Estado Compartido entre Escenarios](#featurecontext-estado-compartido-entre-escenarios)
- [Inyección de Dependencias](#inyección-de-dependencias-con-inject)
- [Reporters](#reporters)
- [Línea de Comandos](#línea-de-comandos)

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

Pass: 1 | Fail: 0 | Skip: 0 | Total: 1 Specs in 1 Features | 0 ms
```

**El ciclo es simple**: describes el comportamiento esperado → ejecutas → MiniSpec te dice si el código cumple la especificación.

---

## El World: Compartiendo Estado

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

## Múltiples Ejemplos con Scenario Outline

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

Pass: 4 | Fail: 0 | Skip: 0 | Total: 4 Specs in 1 Features | 0 ms
```

> 💡 **Un Scenario normal es simplemente un Outline con un solo ejemplo implícito.** No hay diferencia conceptual: ambos especifican comportamiento con ejemplos concretos. El Outline solo hace explícita la tabla de datos.

---

## DataTables: Datos Estructurados

A veces un paso necesita datos más complejos que simples valores. Las **DataTables** permiten pasar estructuras tabulares:

```pascal
.Scenario('Crear múltiples usuarios')
  .Given('los siguientes usuarios:', [
    ['nombre', 'email',            'rol'],
    ['Alice',  'alice@test.com',   'admin'],
    ['Bob',    'bob@test.com',     'user'],
    ['Carol',  'carol@test.com',   'user']
  ], procedure(World: TMyWorld)
    begin
      var Table := SpecContext.DataTable;
      
      for var I := 0 to Table.RowCount - 1 do
      begin
        var Row := Table.AsMap(I);  // Fila como diccionario
        World.Users.Add(
          Row['nombre'].AsString,
          Row['email'].AsString,
          Row['rol'].AsString
        );
      end;
    end)
  .When('cuento los usuarios', procedure(World: TMyWorld)
    begin
      World.Count := World.Users.Count;
    end)
  .&Then('tengo 3 usuarios', procedure(World: TMyWorld)
    begin
      Expect(World.Count).ToEqual(3);
    end)
```

**API de DataTable**:

| Propiedad/Método | Descripción |
|------------------|-------------|
| `RowCount` | Número de filas (sin contar headers) |
| `ColCount` | Número de columnas |
| `Headers` | Array con nombres de columnas |
| `Cell[row, col]` | Celda por índices |
| `Cell[row, 'name']` | Celda por nombre de columna |
| `AsMap(row)` | Fila como `TDictionary<string, TValue>` |
| `AsList` | Todas las filas como array de diccionarios |
| `Transpose` | Nueva tabla con filas/columnas intercambiadas |

---

## Step Bindings: Organizando Pasos Complejos

Cuando tus especificaciones crecen, notarás que muchos pasos se repiten:

```pascal
// En Feature A:
.Given('un usuario autenticado', procedure(W: TWorldA) begin ... end)

// En Feature B:
.Given('un usuario autenticado', procedure(W: TWorldB) begin ... end)  // ¡Duplicado!
```

Los **Step Bindings** resuelven esto: defines pasos como métodos de una clase, usando patrones regex:

```pascal
unit Auth.Steps.pas;

interface

uses
  Daf.MiniSpec,
  Daf.MiniSpec.Binding;

type
  TAuthBindings = class
  public
    [Given('un usuario "([^"]+)" con password "([^"]+)"')]
    procedure SetupUser(World: TObject; Username, Password: string);
    
    [When('hace login')]
    procedure DoLogin(World: TObject);
    
    [ThenAttribute('el login es exitoso')]
    procedure VerifyLoginSuccess(World: TObject);
    
    [ThenAttribute('el login falla con "([^"]+)"')]
    procedure VerifyLoginError(World: TObject; ExpectedError: string);
  end;

implementation

procedure TAuthBindings.SetupUser(World: TObject; Username, Password: string);
begin
  var W := World as TAuthWorld;
  W.Username := Username;
  W.Password := Password;
end;

// ... resto de implementaciones
```

**Registrar y usar**:

```pascal
initialization
  Bindings.RegisterSteps<TAuthBindings>;
  
  Feature('Autenticación')
  .UseWorld<TAuthWorld>
  
  .Scenario('Login válido')
    .Given('un usuario "admin" con password "secret123"')  // Usa el binding
    .When('hace login')
    .&Then('el login es exitoso')
    
  .Scenario('Password incorrecto')
    .Given('un usuario "admin" con password "wrong"')
    .When('hace login')
    .&Then('el login falla con "Credenciales inválidas"')
```

**Características de los Bindings**:

| Aspecto | Descripción |
|---------|-------------|
| Patrones | Regex con grupos de captura para parámetros |
| Tipos | `Integer`, `Int64`, `Float`, `string`, `Boolean` |
| Primer parámetro | Siempre el World (usa `TObject` y haz cast) |
| Prioridad | Lambda inline > Binding registrado |
| Atributos | `[GivenAttribute]`, `[WhenAttribute]`, `[ThenAttribute]` |

> 💡 Los bindings son ideales para pasos comunes (autenticación, setup de datos, etc.) que se usan en múltiples features.

---

## Tags y Filtrado

Los **tags** permiten categorizar y filtrar escenarios. Añádelos en la descripción, preferiblemente al final:

```pascal
Feature('''
Calculadora

  @math @core
''')

.Scenario('''
  División por cero
  @error @edge-case
''')
```

### Ejecutando con Filtros

```bash
# Solo escenarios con @unit
MiApp.exe -f "@unit"

# Escenarios @unit pero NO @slow
MiApp.exe -f "@unit and ~@slow"

# Por título de feature
MiApp.exe -f "Feat:Calculator"

# Por categoría
MiApp.exe -f "Cat:Login"

# Expresiones complejas
MiApp.exe -f "(Feat:Login or @auth) and ~@slow"
```

**Sintaxis de filtros**:

| Expresión | Significado |
|-----------|-------------|
| `@tag` | Tiene el tag |
| `~@tag` | NO tiene el tag |
| `Feat:texto` | Título de feature contiene texto |
| `Scen:texto` | Descripción de scenario contiene texto |
| `Rule:texto` | Descripción de rule contiene texto |
| `Cat:texto` | Categoría contiene texto |
| `and`, `or` | Operadores lógicos |
| `()` | Agrupación |

> ⚠️ Los tags **deben** comenzar con `@`. Escribir `--filter unit` dará error; usa `--filter @unit`.

---

## Assertions (Expect)

MiniSpec proporciona una API fluida para verificaciones. Los métodos son autoexplicativos:

```pascal
// Valores
Expect(resultado).ToEqual(5);
Expect(valor).ToNotEqual(0);
Expect(numero).ToBeBetween(1, 10);

// Strings
Expect(mensaje).ToContain('error');
Expect(email).ToMatch('^[\w.-]+@[\w.-]+\.\w+$');

// Booleanos y nulos
Expect(activo).ToBeTrue;
Expect(objeto).ToNotBeNull;
```

### Verificando Excepciones

Un caso especial importante: verificar que el código lanza una excepción. MiniSpec **captura automáticamente** las excepciones en el step When, permitiendo verificarlas en el Then:

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
| `Expect(Raised).ToHaveMessage('texto')` | Mensaje contiene substring |
| `Expect(Raised).ToBeNone` | No se lanzó excepción |

> 💡 Consulta el código fuente de `Daf.MiniSpec.Expect.pas` para ver todos los métodos disponibles.

---

## Rules: Agrupando por Regla de Negocio

Cuando una Feature tiene múltiples reglas de negocio, las **Rules** ayudan a organizar los escenarios que ilustran cada regla:

```pascal
Feature('Sistema de Descuentos')
.UseWorld<TDiscountWorld>

.Rule('Los clientes VIP tienen 20% de descuento')
  .Scenario('Compra normal de cliente VIP')
    .Given('un cliente VIP', procedure(W: TDiscountWorld)
      begin
        W.Customer := TCustomer.Create(TCustomerType.VIP);
      end)
    .When('compra un producto de $100', procedure(W: TDiscountWorld)
      begin
        W.Purchase := W.Customer.Buy(100);
      end)
    .&Then('el total es $80', procedure(W: TDiscountWorld)
      begin
        Expect(W.Purchase.Total).ToEqual(80);
      end)

.Rule('Las compras mayores a $100 tienen envío gratis')
  .Scenario('Compra de $150')
    // ...
  .Scenario('Compra de $50')
    // ...
```

**Comportamiento de Rules**:

| Aspecto | Descripción |
|---------|-------------|
| Agrupación | Los escenarios bajo una Rule comparten esa regla de negocio |
| No anidables | Cada `.Rule()` cierra la anterior automáticamente |
| Background | El Background de la Feature aplica a todos los escenarios, incluidos los de Rules |
| Filtrado | Usa `Rule:texto` para filtrar por descripción de rule |

### EndRule: Escenarios sin Rule

`.EndRule` solo es necesario cuando quieres añadir escenarios **sin Rule** después de una Rule:

```pascal
Feature('...')
  .Rule('Una regla específica')
    .Scenario('Test dentro de la Rule')
    
  .EndRule  // Necesario para salir de la Rule
  
  .Scenario('Escenario general sin Rule')
```

---

## Vocabulario Gherkin

Ahora que conoces todos los conceptos clave de BDD en MiniSpec, aquí tienes una referencia completa del vocabulario:

| Palabra clave | MiniSpec | Descripción |
|---------------|----------|-------------|
| Feature | `Feature('...')` | Funcionalidad que se especifica |
| Rule | `.Rule('...')` | Agrupa escenarios bajo una regla de negocio |
| Background | `.Background` | Pasos comunes a todos los escenarios |
| Scenario | `.Scenario('...')` | Un ejemplo concreto de comportamiento |
| Scenario Outline | `.ScenarioOutline('...')` | Ejemplo parametrizado con tabla |
| Examples | `.Examples([...])` | Tabla de datos para el Outline |
| DataTable | Tras la descripción del step | Tabla de datos estructurados para un step |
| Given | `.Given('...', proc)` | Establece el contexto inicial |
| When | `.When('...', proc)` | La acción que se prueba |
| Then | `.&Then('...', proc)` | El resultado esperado (`&` requerido) |
| And | `.&And('...', proc)` | Paso adicional (`&` requerido) |
| But | `.But('...', proc)` | Excepción o negación |
| @tag | En descripción | Para filtrar escenarios |

> 💡 El `&` antes de `Then` y `And` es necesario porque son palabras reservadas en Delphi.

### Features con Narrativa

Una Feature puede incluir una narrativa que explica el **por qué**:

```pascal
Feature('''
Calculadora - División

  Como usuario de la calculadora
  Necesito poder dividir números
  Para resolver problemas matemáticos

  @math @division
''')
```

La primera línea es el título. Miispec toma el resto como narrativa. Los tags pueden aparecer en cualquier sitio de la descripción pero se recomienda ponerlos al final.

### Category para Filtrado

Asigna categorías para filtrar con `Cat:texto`. Puedes usar un string o una clase como marcador:

```pascal
// Con string
Feature('Login')
  .Category('auth')
  .UseWorld<TLoginWorld>

// Con clase marcadora
type
  TIntegrationTest = class end;  // Clase vacía como marcador

Feature('Database operations')
  .Category(TIntegrationTest)  // Usa QualifiedClassName: 'Unit1.TIntegrationTest'
  .UseWorld<TDbWorld>
```

Filtrar por categoría:

```bash
MiApp.exe -f "Cat:auth"              # Features con categoría 'auth'
MiApp.exe -f "Cat:Integration"       # Coincide con 'TIntegrationTest'
```

> 💡 Las clases marcadoras son útiles para categorías que se usan en múltiples archivos: el compilador detecta errores tipográficos.

---

## Pending y NoAction

MiniSpec proporciona dos formas de marcar steps que no tienen código ejecutable:

| Método | Semántica | Resultado | Uso típico |
|--------|-----------|-----------|------------|
| `.Pending` | "Pendiente de implementar" | Skip + detiene escenario | Work in progress |
| `.NoAction` | "Sin acción requerida" | Pass + continúa escenario | Steps puramente descriptivos |

**Pending**: Indica que un step está pendiente de implementar. El escenario se marca como Skip y **no se ejecutan los steps siguientes**:

```pascal
.Scenario('Feature en desarrollo')
  .Given('una precondición', procedure(Ctx: TMyWorld) begin end)
  .When('algo pendiente de implementar')
    .Pending                          // Escenario Skip, no sigue
  .&Then('nunca se ejecuta', procedure(Ctx: TMyWorld)
    begin
      // Este código no se ejecuta
    end)
```

**NoAction**: Indica que un step es puramente descriptivo y no requiere código. El step pasa automáticamente y **los steps siguientes sí se ejecutan**:

```pascal
.Scenario('Con steps descriptivos')
  .Given('el sistema está configurado')  // Descriptivo, el sistema ya está listo
    .NoAction
  .When('el usuario hace login', procedure(Ctx: TMyWorld)
    begin
      Ctx.DoLogin;  // Este código SÍ se ejecuta
    end)
  .&Then('accede al dashboard', procedure(Ctx: TMyWorld)
    begin
      Expect(Ctx.IsLoggedIn).ToBeTrue;  // También se ejecuta
    end)
```

**Restricción**: `.NoAction` solo es válido para steps Given y When. Los steps Then siempre deben contener verificaciones (assertions), por lo que usar `.NoAction` en Then lanzará una excepción.

```pascal
// ✓ Válido
.Given('contexto descriptivo').NoAction
.When('acción implícita').NoAction

// ✗ Error en tiempo de ejecución
.&Then('verificación').NoAction  // Lanza excepción
```

---

## Before / After Hooks

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

| Elemento | Ejecuta | Uso típico |
|----------|---------|------------|
| `Before` | Una vez antes de todos los scenarios | Iniciar servidor, crear BD |
| `After` | Una vez después de todos los scenarios | Liberar recursos costosos |
| `Background` | Antes de cada scenario | Preparar datos del test |

**Nota**: Los hooks no reciben World porque se ejecutan antes de que exista cualquier instancia.

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

Los World de cada feature pueden inyectar este contexto con `[Inject]`.

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

    // Rule puede ser nil si no hay Rule explícita
    if Assigned(SpecContext.Rule) then
      WriteLn('Rule: ', SpecContext.Rule.Description);
    
    // DataTable del step actual (nil si no tiene)
    if Assigned(SpecContext.DataTable) then
      WriteLn('Rows: ', SpecContext.DataTable.RowCount);
  end)
```

**ISpecContext proporciona**:

| Propiedad | Descripción |
|-----------|-------------|
| `Suite` | La Suite contenedora |
| `Feature` | La Feature contenedora |
| `Rule` | La Rule contenedora (nil si no hay) |
| `Scenario` | El scenario (o Example) actual |
| `Step` | El step que se está ejecutando |
| `DataTable` | La tabla de datos del step (nil si no tiene) |

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
    [Inject] FShared: TSharedContext;  // Inyectado automáticamente
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

**Ciclo de vida**:
- El FeatureContext se crea al inicio de la Feature
- Se destruye al finalizar la Feature
- Cada ScenarioWorld recibe la inyección del mismo FeatureContext

---

## Inyección de Dependencias con [Inject]

MiniSpec incluye un sistema ligero de inyección de dependencias para propiedades marcadas con `[Inject]`.

**Uso básico** (inyección automática del FeatureContext):

```pascal
uses
  Daf.MiniSpec,
  Daf.MiniSpec.Injection;  // Para el atributo [Inject]

type
  TFeatureContext = class
  public
    SharedValue: Integer;
  end;

  TWorld = class
  private
    [Inject] FCtx: TFeatureContext;  // Inyectado automáticamente
  public
    property Ctx: TFeatureContext read FCtx;
  end;

Feature('...')
  .UseFeatureContext<TFeatureContext>  // Registra TFeatureContext en el Injector
  .UseWorld<TWorld>             // Al crear World, inyecta FCtx
```

**Servicios personalizados a nivel de Suite**:

```pascal
MiniSpec
  .Before('Setup services', procedure
    begin
      TInjectorService.Register(TDatabaseMock.Create);
      TInjectorService.Register(THttpClientMock.Create);
    end)
  .After('Cleanup', procedure
    begin
      TInjectorService.Clear;  // Libera todos los servicios
    end);
```

**API del TInjectorService**:

| Método | Descripción |
|--------|-------------|
| `Register(Instance)` | Registra un servicio (la clase del objeto es la clave) |
| `Unregister(Instance)` | Elimina el registro de un servicio |
| `Resolve(AClass)` | Obtiene el servicio registrado para esa clase |
| `Resolve<T>` | Versión genérica de Resolve |
| `InjectInto(Target)` | Inyecta servicios en propiedades marcadas con `[Inject]` |
| `Clear` | Libera y elimina todos los servicios registrados |

**Errores de inyección**:

Si una propiedad marcada con `[Inject]` no puede ser inyectada, se lanza `EInjectionError`:

- Propiedad no es de tipo clase
- Propiedad no tiene setter
- No hay servicio registrado compatible con el tipo

---

## Reporters

Sintaxis: `-r <nombre>:<opcion1>=<valor>,<opcion2>=<valor>,...`

| Reporter | Opciones | Ejemplo |
|----------|----------|---------|
| `console` | *(ninguna)* | `-r console` |
| `json` | `output=<file>` | `-r json:output=report.json` |
| `junit` | `output=<file>` | `-r junit:output=results.xml` |
| `gherkin` | `output=<dir>` | `-r gherkin:output=features/` |
| `live` | `port=<num>`, `wait=<ms>` | `-r live:port=8080,wait=5000` |

### Múltiples Reporters

Puedes usar varios reporters en la misma ejecución repitiendo la opción `-r`:

```bash
# Consola + JUnit para CI + JSON para análisis
MiApp.exe -r console -r junit:output=results.xml -r json:output=report.json
```

Todos los reporters reciben los mismos eventos y generan su salida simultáneamente.

**JUnit Reporter**: Genera XML en formato JUnit para integración CI/CD. Compatible con GitHub Actions, GitLab CI, Jenkins, Azure DevOps.

**Live Reporter**: Por defecto espera 3 segundos para conexión del navegador. Usa `wait=0` para deshabilitar.

### Archivo de Configuración

MiniSpec crea `MiniSpec.ini` en el directorio del ejecutable si no existe:

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

## Línea de Comandos

| Opción | Descripción |
|--------|-------------|
| `-h, --help` | Muestra ayuda |
| `-f, --filter <expr>` | Filtra escenarios (ver [Tags y Filtrado](#tags-y-filtrado)) |
| `-t, --tags` | Lista todos los tags con conteos |
| `-q, --query <expr>` | Muestra escenarios que coinciden (sin ejecutar) |
| `-r, --reporter <spec>` | Reporter con opciones (ver [Reporters](#reporters)) |
| `--pause` | Espera tecla al finalizar |
| `--dry-run` | Lista escenarios sin ejecutarlos |
| `--stacktrace` | Muestra stack trace completo en errores |

> 💡 `--stacktrace` requiere una librería de stack traces (JclDebug, MadExcept, EurekaLog).
