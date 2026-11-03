# MiniSpec Doubles — Test Doubles para Delphi

**🌍 Idioma: [English](doubles.md) | Español**

Una API fluida y minimalista para test doubles que mantiene la elegancia de MiniSpec. Crea stubs, mocks y spies con una sintaxis encadenable e intuitiva.

---

## Contenido

- [Inicio Rápido](#inicio-rápido)
- [Stubs](#stubs)
- [Mocks](#mocks)
- [Spies](#spies)
- [Matchers de Argumentos](#matchers-de-argumentos)
- [Buenas Prácticas](#buenas-prácticas)

---

## Inicio Rápido

```pascal
uses
  Daf.MiniSpec,
  Daf.MiniSpec.Doubles;

type
  IUserRepository = interface
    function FindById(Id: Integer): IUser;
    procedure Save(User: IUser);
  end;

initialization

Feature('Servicio de Usuario')
.UseWorld<TUserWorld>

.Scenario('Actualizar email de usuario')
  .Given('un repositorio de usuarios', procedure(W: TUserWorld)
    begin
      W.Repo := Stub<IUserRepository>
        .Setup('FindById').WithArgs([42]).Returns(TUser.Create('John'))
        .Instance;
    end)
  .&And('un servicio de usuarios', procedure(W: TUserWorld)
    begin
      W.Service := TUserService.Create(W.Repo);  // montar el SUT es preparación, no el evento
    end)
  .When('actualizo el email', procedure(W: TUserWorld)
    begin
      W.Service.UpdateEmail(42, 'john@new.com');
    end)
  .&Then('el usuario es guardado', procedure(W: TUserWorld)
    begin
      Expect(W.Repo.WasCalled('Save')).ToBeTrue;
    end)
```

---

## Stubs

Los stubs proporcionan respuestas predefinidas a llamadas de métodos. Úsalos cuando necesites controlar lo que devuelven los colaboradores.

### Stub Básico

```pascal
var Calculator := Stub<ICalculator>
  .Setup('Add').Returns(42)
  .Instance;

// Calculator.Add(cualquier cosa) devuelve 42
```

### Retornos Condicionales con Argumentos

```pascal
var Service := Stub<IUserService>
  .Setup('FindById').WithArgs([1]).Returns(User1)
  .Setup('FindById').WithArgs([2]).Returns(User2)
  .Setup('FindById').Raises(Exception.Create('FindById sin Setup para estos argumentos'))  // Por defecto: que falle
  .Instance;

// Service.FindById(1) → User1
// Service.FindById(2) → User2  
// Service.FindById(999) → lanza: el escenario no dijo qué devolver
```

⚠️ **Un Stub sin `Setup` que case devuelve el cero del tipo** (`False`, `nil`, `0`, `''`), sin avisar. Ese cero puede ser justo un valor «bueno» y dar un **falso verde**: la prueba pasa sin que el escenario haya dicho nunca qué debía devolver el doble. Por eso el ejemplo cierra con un `Setup` por defecto que **lanza una excepción** en vez de `.Returns(nil)`: un doble al que el escenario no ha dicho qué devolver tiene que fallar. Haz lo mismo en tus stubs cuando el cero del tipo pudiera pasar por un resultado válido, o marca el doble como [estricto](#modo-estricto).

### Modo Estricto

`.Strict` hace que un `Stub` o un `Mock` lance `EUnconfiguredCall` cuando se llama a una **función** y ningún `Setup` casa, en vez de devolver el cero del tipo. El mensaje nombra la interfaz, el método y los argumentos:

```pascal
var Service := Stub<IUserService>.New.Strict
  .Setup('FindByName').WithArgs([Arg.NotNull]).Returns(7)
  .Instance;

// Service.FindByName('ana') devuelve 7
// Service.FindByName('') lanza EUnconfiguredCall: no Setup matches IUserService.FindByName('')
```

- Cuenta cualquier `Setup` que case, configure lo que configure (`Returns`, `Raises`, `Executes`). Un `Setup` sin `WithArgs` acepta todas las llamadas.
- Los procedimientos no se ven afectados: la llamada a un procedimiento sin `Setup` se registra y no lanza.
- La llamada que lanza se registra antes, así que `CallsTo` y `WasCalled` la cuentan.
- `EUnconfiguredCall` desciende de `EDoubleError`.
- Llama a `.Strict` justo después de `New`, en el `Given`. Solo afecta a las llamadas posteriores.

⚠️ Si el código bajo prueba captura todas las excepciones, también se traga `EUnconfiguredCall` y el escenario puede pasar igualmente. Sin `.Strict`, un doble devuelve el cero del tipo.

### Lanzar Excepciones

```pascal
var Service := Stub<IPaymentService>
  .Setup('Charge').WithArgs([0]).Raises(EInvalidAmount.Create('El monto debe ser positivo'))
  .Setup('Charge').Returns(True)
  .Instance;

// Service.Charge(0) lanza EInvalidAmount
// Service.Charge(100) devuelve True
```

### Comportamiento Personalizado

```pascal
var Counter := 0;
var Service := Stub<ICounter>
  .Setup('Increment').Executes(
    procedure(Args: TArray<TValue>)
    begin
      Inc(Counter);
    end)
  .Instance;
```

### Verificar Llamadas

```pascal
var Stub := Stub<ILogger>
  .Setup('Log').Returns(nil)
  .Instance;

// ... usar el stub ...

if Stub.WasCalled('Log') then
  WriteLn('Log fue llamado ', Stub.CallsTo('Log'), ' veces');

// Obtener detalles de la última llamada
var LastCall := Stub.LastCallTo('Log');
WriteLn('Último mensaje: ', LastCall.Args[0].AsString);
```

---

## Mocks

Los mocks verifican interacciones. Úsalos cuando el comportamiento que estás probando trata sobre *cómo* tu código interactúa con sus colaboradores.

### Expectativas Básicas

```pascal
var Logger := Mock<ILogger>
  .Expects('Log').Once
  .Expects('Flush').AtLeastOnce
  .Instance;

// ... usar logger en tu código ...

Logger.Verify;  // Lanza EExpectationFailed si las expectativas no se cumplen
```

### Tipos de Expectativas

```pascal
.Expects('Method').Never        // No debe ser llamado
.Expects('Method').Once         // Exactamente una vez
.Expects('Method').Twice        // Exactamente dos veces
.Expects('Method').AtLeastOnce  // Una o más veces
.Expects('Method').AtMostOnce   // Cero o una vez
.Expects('Method').Exactly(5)   // Exactamente N veces
```

### Combinar Setup y Expectativas

```pascal
var PaymentGateway := Mock<IPaymentGateway>
  // Configurar valores de retorno
  .Setup('Authorize').Returns(True)
  .Setup('Capture').Returns('TXN-123')
  // Definir expectativas
  .Expects('Authorize').Once
  .Expects('Capture').Once
  .Expects('Refund').Never
  .Instance;

// ... procesar pago ...

PaymentGateway.Verify;
```

### Expectativas por Argumento

```pascal
var Notifier := Mock<INotifier>
  .Expects('Send').WithArgs(['admin@example.com']).Once
  .Expects('Send').WithArgs(['user@example.com']).Once
  .Instance;
```

### Mocks Estrictos

`Mock<T>.New.Strict` se comporta como un stub estricto. En un mock, `Expects` **no** cuenta como configuración: una función con `Expects` y sin `Setup` que case lanza `EUnconfiguredCall`, porque `Expects` dice cuántas veces se llama, no qué devuelve. Añade el `Setup`:

```pascal
var Calc := Mock<ICalculator>.New.Strict
  .Setup('Add').Returns(100)
  .Expects('Add').Once
  .Instance;
```

---

## Spies

Los spies envuelven instancias reales, rastreando llamadas mientras preservan el comportamiento original.

```pascal
var RealCalc := TCalculator.Create as ICalculator;

var Spy := SpyOn<ICalculator>(RealCalc)
  .TrackAll
  .Instance;

// Usar spy - las llamadas van a la implementación real
var Result := Spy.Add(2, 3);  // Devuelve 5 (comportamiento real)

// Verificar qué pasó
Expect(Spy.CallsTo('Add')).ToEqual(1);
Expect(Spy.LastCallTo('Add').Args[0].AsInteger).ToEqual(2);
```

### Rastreo Selectivo

```pascal
var Spy := SpyOn<IService>(RealService)
  .Track('MetodoImportante')
  .Track('OtroMetodo')
  .Instance;
```

---

## Matchers de Argumentos

Para coincidencia flexible de argumentos en stubs y mocks.

### Matchers Incluidos

```pascal
uses Daf.MiniSpec.Doubles;

// Coincidir cualquier valor
.Setup('Process').WithArgs([Arg.Any]).Returns(True)

// Coincidir no-nulo/no-vacío
.Setup('Save').WithArgs([Arg.NotNull]).Returns(True)

// Coincidir string que contiene substring
.Setup('Log').WithArgs([Arg.Contains('error')]).Returns(nil)

// Coincidir lo que acepte un predicado
.Setup('Listen').WithArgs([Arg.Matching<Integer>(
  function(Port: Integer): Boolean
  begin
    Result := (Port >= 1) and (Port <= 65535);
  end)]).Returns(True)
```

`Arg.Matching<T>` recibe la condición misma. El valor llega al predicado ya convertido a
`T`, así que un rango es uno de estos y no un matcher aparte. Un argumento de otro tipo no
casa, y al predicado no se le pregunta.

### Ejemplo con Matchers

Los setups se resuelven **del último declarado hacia atrás**, y uno con matchers gana a uno
sin ellos. Así que el más estrecho va **el último**: declararlo primero no lo protege de uno
más amplio declarado después.

```pascal
var EmailService := Stub<IEmailService>
  .Setup('Send')
    .WithArgs([Arg.NotNull, Arg.NotNull])
    .Returns(True)
  .Setup('Send')
    .WithArgs([Arg.Contains('@admin'), Arg.Any])
    .Raises(ESecurityException.Create('No se puede enviar email a admins'))
  .Instance;

// EmailService.Send('admin@co.com', 'Hola') → lanza excepción
// EmailService.Send('user@co.com', 'Hola') → True
// EmailService.Send('', 'Hola') → False: no casó ninguno, así que vuelve el cero del tipo
// (ojo: un False silencioso puede ser un falso verde; añade un Setup por defecto que lance)
```

---

## Buenas Prácticas

### 1. Preferir Stubs sobre Mocks

Usa stubs cuando solo necesites proporcionar datos de prueba. Reserva los mocks para cuando la interacción en sí es lo que estás probando.

```pascal
// ✅ Bien: Stub para datos
var Repo := Stub<IRepository>
  .Setup('Find').Returns(TestData)
  .Instance;

// ✅ Bien: Mock cuando la interacción importa
var Notifier := Mock<INotifier>
  .Expects('SendAlert').Once
  .Instance;
```

### 2. Mantener los Doubles Simples

No sobre-configurar. Configura solo lo que la prueba necesita.

```pascal
// ❌ Demasiada configuración
var Service := Stub<IService>
  .Setup('Method1').Returns(...)
  .Setup('Method2').Returns(...)
  .Setup('Method3').Returns(...)
  .Setup('Method4').Returns(...)
  .Instance;

// ✅ Solo lo que esta prueba necesita
var Service := Stub<IService>
  .Setup('GetUser').Returns(TestUser)
  .Instance;
```

### 3. Un Mock Por Test

Evita múltiples mocks en un solo escenario. Si necesitas muchos, considera si tu diseño necesita refactorización.

### 4. Verificar al Final

Llama a `Verify` en tu paso Then, después de que todas las interacciones hayan ocurrido.

```pascal
.&Then('las notificaciones fueron enviadas', procedure(W: TWorld)
  begin
    W.NotifierMock.Verify;
  end)
```

### 5. Usar Fallos Descriptivos

Los fallos de verificación son descriptivos:

```
Mock verification failed:
"SendEmail" expected once, but called 0 times
"Log" expected never, but called 3 times
```

---

## Integración con MiniSpec

Los doubles funcionan perfectamente con el patrón World de MiniSpec:

```pascal
type
  TOrderWorld = class
    Repo: IStubSetup<IOrderRepository>;
    Notifier: IMockExpect<INotifier>;
    Service: TOrderService;
    destructor Destroy; override;
  end;

destructor TOrderWorld.Destroy;
begin
  Service.Free;
  inherited;
end;

initialization

Feature('Procesamiento de Pedidos')
.UseWorld<TOrderWorld>

.Background
  .Given('un repositorio stub', procedure(W: TOrderWorld)
    begin
      W.Repo := Stub<IOrderRepository>
        .Setup('FindById').Returns(TOrder.Create(1, 'Pendiente'));
    end)
  .&And('un notificador mock', procedure(W: TOrderWorld)
    begin
      W.Notifier := Mock<INotifier>
        .Setup('Notify').Returns(nil);
    end)

.Scenario('Completar un pedido envía notificación')
  .Given('el servicio', procedure(W: TOrderWorld)
    begin
      W.Notifier.Expects('Notify').Once;
      W.Service := TOrderService.Create(W.Repo.Instance, W.Notifier.Instance);
    end)
  .When('completo el pedido #1', procedure(W: TOrderWorld)
    begin
      W.Service.Complete(1);
    end)
  .&Then('el cliente es notificado', procedure(W: TOrderWorld)
    begin
      W.Notifier.Verify;
    end)
```

---

## Requisitos

- Delphi 12 Athens o posterior
- Interfaces con RTTI habilitado (`{$M+}` o `{$TYPEINFO ON}`)

---

<p align="center">
  <sub>Parte de <a href="../../README.md">MiniSpec</a> — Framework BDD para Delphi</sub>
</p>
