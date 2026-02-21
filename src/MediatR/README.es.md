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

## Documentación

- 📖 [Guía de uso](docs/GUIDE.es.md) — IMediatorHelper, respuestas ARC, pipeline, [MediatorAbstract], escaneo de DI
