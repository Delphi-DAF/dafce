# MediatR

**🌍 Language: English | [Español](README.es.md)**

In-process messaging for Delphi — commands, queries, and notifications dispatched through a single `IMediator` interface. Inspired by the .NET MediatR library.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Concepts

| Type | Direction | Returns | Use case |
|------|-----------|---------|----------|
| `IRequest` | 1 → 1 handler | — | Command (fire-and-forget) |
| `IRequest<TResponse>` | 1 → 1 handler | `TResponse` | Query |
| `INotification` | 1 → N handlers | — | Domain event |

---

## Quick Start — Command

```pascal
// 1. Define command
type
  TCreateOrderCommand = class(TRequest)
    OrderId: Integer;
  end;

// 2. Implement handler
type
  TCreateOrderHandler = class(TRequestHandler<TCreateOrderCommand>)
    procedure Handle(Request: TCreateOrderCommand); override;
  end;

// 3. Register handler
Services.AddTransient<IRequestHandler<TCreateOrderCommand>, TCreateOrderHandler>;

// 4. Dispatch
Mediator.Send<TCreateOrderCommand>(Cmd);
```

---

## Quick Start — Query

```pascal
type
  TGetOrderQuery    = class(TRequest<TOrderDTO>);
  TGetOrderHandler  = class(TResponseHandler<TOrderDTO, TGetOrderQuery>)
    function Handle(Request: TGetOrderQuery): TOrderDTO; override;
  end;

var DTO := Mediator.Send<TOrderDTO, TGetOrderQuery>(Query);
```

---

## Quick Start — Notification

```pascal
type
  TOrderCreatedEvent    = class(TNotification);
  TEmailNotifyHandler   = class(TNotificacionHandler<TOrderCreatedEvent>)
    procedure Handle(Notification: TOrderCreatedEvent); override;
  end;

// Register as many handlers as needed
Services.AddTransient<INotificationHandler<TOrderCreatedEvent>, TEmailNotifyHandler>;
Services.AddTransient<INotificationHandler<TOrderCreatedEvent>, TAuditLogHandler>;

// Publish — ALL handlers are called
Mediator.Publish<TOrderCreatedEvent>(Evt);
```

---

## DI Registration

```pascal
uses Daf.MediatR.DependencyInjection;

Services.AddMediatR;   // registers IMediator singleton + scans for handlers
```

Or manually per handler:

```pascal
Services.AddTransient<IRequestHandler<TMyCommand>, TMyCommandHandler>;
```

---

## Documentation

- 📖 [Usage Guide](docs/GUIDE.md) — IMediatorHelper, ARC responses, pipeline, [MediatorAbstract], DI scanning
