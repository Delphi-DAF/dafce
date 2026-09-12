# Patrones de Testing con MiniSpec

**🌍 Idioma: [English](TESTING-PATTERNS.md) | Español**

Esta guía complementa la [Guía de Usuario](GUIDE.es.md) mostrando cómo adaptar BDD a diferentes niveles de testing. Aunque BDD nació para capturar requisitos de negocio, su vocabulario expresivo es útil en cualquier nivel de la pirámide de tests.

⚠️ **Los nombres de fichero de esta guía son una sugerencia para tu proyecto, no la convención del propio DAF.** Aquí el sufijo lleva el nivel del test: `.Spec.pas` para una clase, `.Feat.pas` para una feature. El árbol de DAF llama `[Feature].Feat.pas` a todas sus specs y `[Feature].Steps.pas` a sus ficheros de pasos, sea cual sea el nivel.

---

## Tabla de Contenidos

- [La Pirámide de Tests](#la-pirámide-de-tests)
- [Tests E2E: Especificando el Sistema](#tests-e2e-especificando-el-sistema)
- [Tests Unitarios: Especificando Clases](#tests-unitarios-especificando-clases)
- [Tests de Integración: Especificando APIs](#tests-de-integración-especificando-apis)
- [Organizando por Tipo de Test](#organizando-por-tipo-de-test)

---

## La Pirámide de Tests

```
        /\
       /  \      E2E Tests
      /    \     (Sistema completo, requisitos de negocio)
     /------\
    /        \   Integration Tests
   /          \  (APIs, servicios, componentes conectados)
  /------------\
 /              \ Unit Tests
/________________\(Clases individuales, funciones puras)
```

| Nivel | SUT | Velocidad | Fragilidad | Enfoque |
|-------|-----|-----------|------------|---------|
| **E2E** | Sistema completo | Lento | Alta | Requisitos de negocio |
| **Integración** | API / Servicio | Medio | Media | Contratos entre componentes |
| **Unitario** | Clase / Función | Rápido | Baja | Comportamiento aislado |

MiniSpec puede usarse en los tres niveles. La clave está en **qué describes** y **cómo estructuras** tus especificaciones.

---

## Tests E2E: Especificando el Sistema

Este es el uso "clásico" de BDD, cubierto en detalle en la [Guía de Usuario](GUIDE.es.md). El SUT es el **sistema completo** y describes **requisitos de negocio**:

```pascal
Feature('''
Proceso de Checkout

  Como cliente
  Necesito completar mi compra
  Para recibir los productos en mi domicilio

  @e2e @checkout
''')

.UseWorld<TCheckoutWorld>

.Scenario('Compra exitosa con tarjeta de crédito')
  .Given('tengo productos en el carrito', procedure(W: TCheckoutWorld)
    begin
      W.Cart.Add(TProduct.Create('Laptop', 999.99));
    end)
  .When('completo el pago con tarjeta válida', procedure(W: TCheckoutWorld)
    begin
      W.Checkout.Pay(TCreditCard.Create('4111111111111111'));
    end)
  .&Then('recibo confirmación de pedido', procedure(W: TCheckoutWorld)
    begin
      Expect(W.Checkout.OrderConfirmed).ToBeTrue;
      Expect(W.Checkout.OrderNumber).ToMatch('^\d{8}$');
    end)
```

**Características de tests E2E:**

- Vocabulario de **negocio**, no técnico
- El World orquesta múltiples componentes
- Pueden ser lentos (base de datos real, servicios externos)
- Ideales para **criterios de aceptación**

---

## Tests Unitarios: Especificando Clases

Cuando el SUT es una **clase individual**, BDD sigue siendo útil. Piensa en frameworks como RSpec (Ruby) o Jest (JavaScript) que usan `describe()` para agrupar comportamientos.

### El Patrón: Feature = Clase, Scenario = Método/Comportamiento

```pascal
unit TStringBuilder.Spec;

interface

implementation

uses
  System.SysUtils,
  Daf.MiniSpec;

type
  TStringBuilderWorld = class
  public
    SUT: TStringBuilder;  // System Under Test
    Result: string;
    destructor Destroy; override;
  end;

destructor TStringBuilderWorld.Destroy;
begin
  SUT.Free;
  inherited;
end;

initialization

Feature('''
TStringBuilder @unit

  Clase para construir strings eficientemente
  mediante concatenación incremental.
''')

.UseWorld<TStringBuilderWorld>

.Background
  .Given('un StringBuilder vacío', procedure(W: TStringBuilderWorld)
    begin
      W.SUT := TStringBuilder.Create;
    end)

.Rule('Append: añade texto al final')

  .Scenario('Append de un string')
    .When('añado "Hello"', procedure(W: TStringBuilderWorld)
      begin
        W.SUT.Append('Hello');
      end)
    .&Then('el contenido es "Hello"', procedure(W: TStringBuilderWorld)
      begin
        Expect(W.SUT.ToString).ToEqual('Hello');
      end)

  .Scenario('Append encadenado')
    .When('añado "Hello" y luego " World"', procedure(W: TStringBuilderWorld)
      begin
        W.SUT.Append('Hello').Append(' World');
      end)
    .&Then('el contenido es "Hello World"', procedure(W: TStringBuilderWorld)
      begin
        Expect(W.SUT.ToString).ToEqual('Hello World');
      end)

.Rule('Clear: vacía el contenido')

  .Scenario('Clear después de añadir')
    .Given('contenido existente', procedure(W: TStringBuilderWorld)
      begin
        W.SUT.Append('Existing content');
      end)
    .When('llamo Clear', procedure(W: TStringBuilderWorld)
      begin
        W.SUT.Clear;
      end)
    .&Then('el contenido está vacío', procedure(W: TStringBuilderWorld)
      begin
        Expect(W.SUT.ToString).ToEqual('');
        Expect(W.SUT.Length).ToEqual(0);
      end)

.Rule('Length: devuelve la longitud actual')

  .Scenario('Length inicial es cero')
    .&Then('Length es 0', procedure(W: TStringBuilderWorld)
      begin
        Expect(W.SUT.Length).ToEqual(0);
      end)

  .ScenarioOutline('Length después de Append')
    .When('añado <texto>', procedure(W: TStringBuilderWorld)
      begin
        W.SUT.Append(W.Texto);
      end)
    .&Then('Length es <longitud>', procedure(W: TStringBuilderWorld)
      begin
        Expect(W.SUT.Length).ToEqual(W.Longitud);
      end)
    .Examples([
      ['texto',   'longitud'],
      ['',        0],
      ['a',       1],
      ['Hello',   5],
      ['こんにちは', 5]  // Unicode
    ])

end.
```

### Convenciones para Tests Unitarios

| Elemento | Convención | Ejemplo |
|----------|------------|---------|
| Feature | Nombre de la clase | `TStringBuilder`, `TCalculator` |
| Rule | Método o grupo de comportamiento | `Append: añade texto`, `Validación de entrada` |
| Scenario | Caso específico del comportamiento | `Append de string vacío` |
| Tag | `@unit` para filtrado | `TStringBuilder @unit` |
| World | Contiene solo el SUT y datos del test | `TStringBuilderWorld` |

### Comparación con RSpec

Si vienes de RSpec, esta es la equivalencia:

```ruby
# RSpec (Ruby)
describe TStringBuilder do
  describe '#append' do
    it 'adds text to the end' do
      builder = TStringBuilder.new
      builder.append('Hello')
      expect(builder.to_s).to eq('Hello')
    end
  end
end
```

```pascal
// MiniSpec (Delphi)
Feature('TStringBuilder @unit')
.UseWorld<TWorld>

.Rule('Append: añade texto al final')
  .Scenario('Append de un string')
    .Given('un StringBuilder vacío', ...)
    .When('añado "Hello"', ...)
    .&Then('el contenido es "Hello"', ...)
```

La estructura es similar:
- `describe Class` → `Feature('Class')`
- `describe '#method'` → `.Rule('Method: descripción')`
- `it 'behavior'` → `.Scenario('behavior')`

---

## Tests de Integración: Especificando APIs

Cuando el SUT es un **API** (REST, GraphQL, gRPC...), cada **endpoint** o **operación** puede ser una Feature o Rule.

### El Patrón: Feature = Recurso/Endpoint, Scenario = Operación

```pascal
unit API.Users.Spec;

interface

implementation

uses
  System.SysUtils,
  System.JSON,
  System.Net.HttpClient,
  Daf.MiniSpec;

type
  TApiWorld = class
  public
    Client: THTTPClient;
    Response: IHTTPResponse;
    ResponseJson: TJSONObject;
    UserId: string;
    destructor Destroy; override;
  end;

destructor TApiWorld.Destroy;
begin
  ResponseJson.Free;
  Client.Free;
  inherited;
end;

initialization

Feature('''
API: /users @integration @api

  Gestión de usuarios via REST API.
  Base URL: http://localhost:3000/api/v1
''')

.UseWorld<TApiWorld>

.Background
  .Given('un cliente HTTP configurado', procedure(W: TApiWorld)
    begin
      W.Client := THTTPClient.Create;
      W.Client.ContentType := 'application/json';
    end)

.Rule('GET /users - Listar usuarios')

  .Scenario('Lista vacía cuando no hay usuarios')
    .When('GET /users', procedure(W: TApiWorld)
      begin
        W.Response := W.Client.Get('http://localhost:3000/api/v1/users');
      end)
    .&Then('responde 200 OK', procedure(W: TApiWorld)
      begin
        Expect(W.Response.StatusCode).ToEqual(200);
      end)
    .&And('devuelve array vacío', procedure(W: TApiWorld)
      begin
        var Json := TJSONObject.ParseJSONValue(W.Response.ContentAsString);
        Expect(Json is TJSONArray).ToBeTrue;
        Expect((Json as TJSONArray).Count).ToEqual(0);
        Json.Free;
      end)

.Rule('POST /users - Crear usuario')

  .Scenario('Crear usuario con datos válidos')
    .When('POST /users con nombre y email', procedure(W: TApiWorld)
      begin
        var Body := TJSONObject.Create;
        Body.AddPair('name', 'John Doe');
        Body.AddPair('email', 'john@example.com');
        W.Response := W.Client.Post(
          'http://localhost:3000/api/v1/users',
          TStringStream.Create(Body.ToString)
        );
        Body.Free;
      end)
    .&Then('responde 201 Created', procedure(W: TApiWorld)
      begin
        Expect(W.Response.StatusCode).ToEqual(201);
      end)
    .&And('devuelve el usuario con ID', procedure(W: TApiWorld)
      begin
        W.ResponseJson := TJSONObject.ParseJSONValue(
          W.Response.ContentAsString) as TJSONObject;
        Expect(W.ResponseJson.GetValue('id')).ToNotBeNull;
        Expect(W.ResponseJson.GetValue<string>('name')).ToEqual('John Doe');
        W.UserId := W.ResponseJson.GetValue<string>('id');
      end)

  .Scenario('Error con email inválido')
    .When('POST /users con email malformado', procedure(W: TApiWorld)
      begin
        var Body := TJSONObject.Create;
        Body.AddPair('name', 'John');
        Body.AddPair('email', 'not-an-email');
        W.Response := W.Client.Post(
          'http://localhost:3000/api/v1/users',
          TStringStream.Create(Body.ToString)
        );
        Body.Free;
      end)
    .&Then('responde 400 Bad Request', procedure(W: TApiWorld)
      begin
        Expect(W.Response.StatusCode).ToEqual(400);
      end)
    .&And('incluye mensaje de error', procedure(W: TApiWorld)
      begin
        W.ResponseJson := TJSONObject.ParseJSONValue(
          W.Response.ContentAsString) as TJSONObject;
        Expect(W.ResponseJson.GetValue<string>('error')).ToContain('email');
      end)

.Rule('GET /users/{id} - Obtener usuario')

  .Scenario('Usuario existente')
    .Given('un usuario existente', procedure(W: TApiWorld)
      begin
        // Crear usuario primero
        var Body := TJSONObject.Create;
        Body.AddPair('name', 'Jane');
        Body.AddPair('email', 'jane@example.com');
        var Resp := W.Client.Post(
          'http://localhost:3000/api/v1/users',
          TStringStream.Create(Body.ToString)
        );
        var Json := TJSONObject.ParseJSONValue(Resp.ContentAsString) as TJSONObject;
        W.UserId := Json.GetValue<string>('id');
        Json.Free;
        Body.Free;
      end)
    .When('GET /users/{id}', procedure(W: TApiWorld)
      begin
        W.Response := W.Client.Get(
          'http://localhost:3000/api/v1/users/' + W.UserId);
      end)
    .&Then('responde 200 OK', procedure(W: TApiWorld)
      begin
        Expect(W.Response.StatusCode).ToEqual(200);
      end)
    .&And('devuelve los datos del usuario', procedure(W: TApiWorld)
      begin
        W.ResponseJson := TJSONObject.ParseJSONValue(
          W.Response.ContentAsString) as TJSONObject;
        Expect(W.ResponseJson.GetValue<string>('name')).ToEqual('Jane');
      end)

  .Scenario('Usuario no existente')
    .When('GET /users/999999', procedure(W: TApiWorld)
      begin
        W.Response := W.Client.Get(
          'http://localhost:3000/api/v1/users/999999');
      end)
    .&Then('responde 404 Not Found', procedure(W: TApiWorld)
      begin
        Expect(W.Response.StatusCode).ToEqual(404);
      end)

end.
```

### Usando FeatureContext para Estado Compartido

En tests de API, a menudo necesitas datos creados en un escenario para usarlos en otro:

```pascal
type
  TApiContext = class
  public
    BaseUrl: string;
    AuthToken: string;
    CreatedIds: TList<string>;
    constructor Create;
    destructor Destroy; override;
  end;

  TApiWorld = class
  private
    [MiniInject] FCtx: TApiContext;
  public
    Client: THTTPClient;
    Response: IHTTPResponse;
    property Ctx: TApiContext read FCtx;
  end;

Feature('API: /orders @integration')
  .UseFeatureContext<TApiContext>
  .UseWorld<TApiWorld>
  
  .Before('Iniciar servidor de test', procedure
    begin
      TestServer.Start;
    end)
  
  .After('Limpiar datos de test', procedure
    begin
      TestServer.CleanupTestData;
      TestServer.Stop;
    end)
```

### Tabla de Códigos HTTP Comunes

Para verificar respuestas HTTP:

```pascal
// Helpers reutilizables
procedure ExpectSuccess(Response: IHTTPResponse);
begin
  Expect(Response.StatusCode).ToBeBetween(200, 299);
end;

procedure ExpectClientError(Response: IHTTPResponse);
begin
  Expect(Response.StatusCode).ToBeBetween(400, 499);
end;

procedure ExpectServerError(Response: IHTTPResponse);
begin
  Expect(Response.StatusCode).ToBeBetween(500, 599);
end;
```

---

## Organizando por Tipo de Test

Hay dos estrategias principales: **un solo ejecutable con tags** o **ejecutables separados por nivel**.

### Opción A: Un Solo Ejecutable con Tags

Todos los tests en un único proyecto, filtrados por tags en tiempo de ejecución.

```
MyProject/
├── src/                        # Código de producción
├── specs/
│   ├── AllSpecs.dpr           # Ejecutable único
│   ├── AllSpecs.groupproj
│   │
│   ├── unit/                  # Tests unitarios
│   │   ├── TCalculator.Spec.pas
│   │   ├── TStringBuilder.Spec.pas
│   │   └── TValidator.Spec.pas
│   │
│   ├── integration/           # Tests de integración
│   │   ├── API.Users.Spec.pas
│   │   ├── API.Orders.Spec.pas
│   │   └── Database.Spec.pas
│   │
│   └── e2e/                   # Tests end-to-end
│       ├── Checkout.Feat.pas
│       ├── Registration.Feat.pas
│       └── Search.Feat.pas
```

**Marcar cada test con su nivel:**

```pascal
// Unit test
Feature('TCalculator @unit @math')

// Integration test  
Feature('API: /users @integration @api')

// E2E test
Feature('Proceso de Checkout @e2e @checkout')
```

**Ejecutar por tipo:**

```bash
# Solo tests unitarios (rápidos)
AllSpecs.exe -f "@unit"

# Solo integración
AllSpecs.exe -f "@integration"

# Solo E2E
AllSpecs.exe -f "@e2e"

# Excluir tests lentos
AllSpecs.exe -f "~@slow"

# Unit + Integration, sin E2E
AllSpecs.exe -f "(@unit or @integration) and ~@e2e"
```

**Ventajas:** Simple, un solo proyecto, filtrado flexible.  
**Desventajas:** El ejecutable incluye todas las dependencias (incluso las de E2E).

---

### Opción B: Ejecutables Separados por Nivel

Cada nivel de testing tiene su propio proyecto `.dpr`. Ideal cuando los niveles tienen dependencias muy distintas o para pipelines CI/CD que ejecutan en paralelo.

```
MyProject/
├── src/                        # Código de producción
├── specs/
│   ├── Specs.groupproj        # Agrupa todos los proyectos de specs
│   │
│   ├── unit/
│   │   ├── UnitSpecs.dpr      # Ejecutable: solo tests unitarios
│   │   ├── UnitSpecs.dproj
│   │   ├── TCalculator.Spec.pas
│   │   ├── TStringBuilder.Spec.pas
│   │   └── TValidator.Spec.pas
│   │
│   ├── integration/
│   │   ├── IntegrationSpecs.dpr   # Ejecutable: solo integración
│   │   ├── IntegrationSpecs.dproj
│   │   ├── API.Users.Spec.pas
│   │   ├── API.Orders.Spec.pas
│   │   └── Database.Spec.pas
│   │
│   └── e2e/
│       ├── E2ESpecs.dpr       # Ejecutable: solo E2E
│       ├── E2ESpecs.dproj
│       ├── Checkout.Feat.pas
│       ├── Registration.Feat.pas
│       └── Search.Feat.pas
```

**Cada ejecutable configura su nivel:**

```pascal
// UnitSpecs.dpr - Rápido, sin dependencias externas
program UnitSpecs;
{$APPTYPE CONSOLE}
uses
  Daf.MiniSpec,
  TCalculator.Spec in 'TCalculator.Spec.pas',
  TStringBuilder.Spec in 'TStringBuilder.Spec.pas',
  TValidator.Spec in 'TValidator.Spec.pas';
begin
  MiniSpec
    .Category('Unit Tests')
    .Run;
end.

// IntegrationSpecs.dpr - Requiere servicios externos
program IntegrationSpecs;
{$APPTYPE CONSOLE}
uses
  Daf.MiniSpec,
  TestServer in '..\common\TestServer.pas',
  API.Users.Spec in 'API.Users.Spec.pas',
  API.Orders.Spec in 'API.Orders.Spec.pas';
begin
  MiniSpec
    .Category('Integration Tests')
    .Before('Start test server', procedure
      begin
        TestServer.Start;
      end)
    .After('Stop test server', procedure
      begin
        TestServer.Stop;
      end)
    .Run;
end.

// E2ESpecs.dpr - Sistema completo
program E2ESpecs;
{$APPTYPE CONSOLE}
uses
  Daf.MiniSpec,
  Checkout.Feat in 'Checkout.Feat.pas',
  Registration.Feat in 'Registration.Feat.pas';
begin
  MiniSpec
    .Category('E2E Tests')
    .Before('Initialize full system', procedure
      begin
        TestEnvironment.Setup;
      end)
    .After('Cleanup', procedure
      begin
        TestEnvironment.Teardown;
      end)
    .Run;
end.
```

**Ventajas:** Ejecutables pequeños, dependencias separadas, fácil paralelizar en CI.  
**Desventajas:** Más proyectos que mantener.

### Resumen: Cuándo Usar Cada Nivel

| Pregunta | Unit | Integration | E2E |
|----------|------|-------------|-----|
| ¿El SUT es una clase aislada? | ✓ | | |
| ¿Pruebas un API o servicio? | | ✓ | |
| ¿Describes requisitos de negocio? | | | ✓ |
| ¿Necesitas base de datos real? | | A veces | ✓ |
| ¿El test debe ser muy rápido? | ✓ | | |
| ¿Stakeholders no técnicos lo leerán? | | | ✓ |

---

## Conclusión

BDD no es exclusivo de tests E2E. El vocabulario de MiniSpec — Feature, Rule, Scenario, Given/When/Then — proporciona una estructura clara para **cualquier nivel de testing**:

- **Unit tests**: Feature = Clase, Rule = Método
- **Integration tests**: Feature = API/Recurso, Rule = Endpoint
- **E2E tests**: Feature = Funcionalidad de negocio, Scenario = Criterio de aceptación

La clave está en adaptar el **vocabulario** y la **granularidad** al nivel que estás probando, manteniendo siempre el principio fundamental de BDD: **especificar comportamiento con ejemplos concretos**.
