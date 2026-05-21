# MediatR

**🌍 Idioma: [English](README.md) | Español**

Mensajería en proceso para Delphi — comandos, consultas y notificaciones despachados a través de una única interfaz `IMediator`. Inspirado en la biblioteca .NET MediatR.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![Licencia](https://img.shields.io/badge/licencia-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Conceptos

| Tipo | Dirección | Devuelve | Caso de uso |
|------|-----------|----------|-------------|
| `IRequest` | 1 → 1 handler | — | Comando (fire-and-forget) |
| `IRequest<TResponse>` | 1 → 1 handler | `TResponse` | Consulta |
| `INotification` | 1 → N handlers | — | Evento de dominio |

---

## Inicio rápido — Comando

```pascal
// 1. Definir comando
type
  TCreateOrderCommand = class(TRequest)
    OrderId: Integer;
  end;

// 2. Implementar handler
type
  TCreateOrderHandler = class(TRequestHandler<TCreateOrderCommand>)
    procedure Handle(Request: TCreateOrderCommand); override;
  end;

// 3. Registrar handler
Services.AddTransient<IRequestHandler<TCreateOrderCommand>, TCreateOrderHandler>;

// 4. Despachar
Mediator.Send<TCreateOrderCommand>(Cmd);
```

---

## Inicio rápido — Consulta

```pascal
type
  TGetOrderQuery    = class(TRequest<TOrderDTO>);
  TGetOrderHandler  = class(TResponseHandler<TOrderDTO, TGetOrderQuery>)
    function Handle(Request: TGetOrderQuery): TOrderDTO; override;
  end;

var DTO := Mediator.Send<TOrderDTO, TGetOrderQuery>(Query);
```

---

## Inicio rápido — Notificación

```pascal
type
  TOrderCreatedEvent    = class(TNotification);
  TEmailNotifyHandler   = class(TNotificacionHandler<TOrderCreatedEvent>)
    procedure Handle(Notification: TOrderCreatedEvent); override;
  end;

// Registrar tantos handlers como sea necesario
Services.AddTransient<INotificationHandler<TOrderCreatedEvent>, TEmailNotifyHandler>;
Services.AddTransient<INotificationHandler<TOrderCreatedEvent>, TAuditLogHandler>;

// Publicar — se llama a TODOS los handlers
Mediator.Publish<TOrderCreatedEvent>(Evt);
```

---

## Registro en DI

```pascal
uses Daf.MediatR.DependencyInjection;

Services.AddMediatR;   // registra el singleton IMediator + escanea handlers
```

O manualmente por handler:

```pascal
Services.AddTransient<IRequestHandler<TMyCommand>, TMyCommandHandler>;
```

---

## Behaviors de pipeline

Los behaviors envuelven cada llamada `Send` con lógica transversal (logging, validación, caché, etc.).

### Behaviors globales

Se ejecutan para **todas** las requests. Extiende `TPipelineBehaviorBase` y sobreescribe `Invoke`:

```pascal
type
  TLoggingBehavior = class(TPipelineBehaviorBase)
  public
    function Invoke(Request: TObject; Next: TFunc<TValue>): TValue; override;
  end;

function TLoggingBehavior.Invoke(Request: TObject; Next: TFunc<TValue>): TValue;
begin
  Log('Antes de ' + Request.ClassName);
  Result := Next();
  Log('Después de ' + Request.ClassName);
end;
```

### Behaviors tipados

Se ejecutan **solo** para un tipo de request específico (y sus subclases). Extiende `TPipelineBehavior<TResponse, TRequest>`:

```pascal
type
  TPingBehavior = class(TPipelineBehavior<string, TPing>)
  public
    function Handle(Request: TPing; Next: TFunc<TValue>): string; override;
  end;

function TPingBehavior.Handle(Request: TPing; Next: TFunc<TValue>): string;
begin
  Result := Next().AsType<string>;
end;
```

### Registro

```pascal
// Auto-descubrimiento
Services.AddMediatRBehaviors(_T.PackageOf<TMyClass>);

// Manual
Services.AddTransient(TypeInfo(IPipelineBehaviorInvoker), TLoggingBehavior);
Services.AddTransient(TypeInfo(IPipelineBehaviorInvoker), TPingBehavior);
```

Usa `[MediatorAbstract]` en behaviors que solo registres manualmente para excluirlos del auto-descubrimiento.

### Orden de ejecución

Los behaviors se ejecutan en orden de registro — el primero registrado es el envoltorio más externo.

### Cortocircuito

No llamar a `Next` aborta el pipeline sin invocar el handler:

```pascal
function TAuthBehavior.Invoke(Request: TObject; Next: TFunc<TValue>): TValue;
begin
  if not Authenticated then
    Result := Default(TValue)
  else
    Result := Next();
end;
```

---

## Documentación

- 📖 [Guía de uso](docs/GUIDE.es.md) — IMediatorHelper, respuestas ARC, pipeline, [MediatorAbstract], escaneo de DI
