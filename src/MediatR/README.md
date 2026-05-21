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


---

## Pipeline Behaviors

Behaviors wrap every `Send` call with cross-cutting logic (logging, validation, caching, etc.).

### Global behaviors

Run for **every request**, regardless of type. Extend `TPipelineBehaviorBase` and override `Invoke`:

```pascal
type
  TLoggingBehavior = class(TPipelineBehaviorBase)
  public
    function Invoke(Request: TObject; Next: TFunc<TValue>): TValue; override;
  end;

function TLoggingBehavior.Invoke(Request: TObject; Next: TFunc<TValue>): TValue;
begin
  Log('Before ' + Request.ClassName);
  Result := Next();
  Log('After ' + Request.ClassName);
end;
```

### Typed behaviors

Run **only** for a specific request type (and its descendants). Extend `TPipelineBehavior<TResponse, TRequest>` for response requests, or `TPipelineBehavior<TRequest>` for void requests:

```pascal
// Response behavior (IRequest<string>)
type
  TPingBehavior = class(TPipelineBehavior<string, TPing>)
  public
    function Handle(Request: TPing; Next: TFunc<TValue>): string; override;
  end;

function TPingBehavior.Handle(Request: TPing; Next: TFunc<TValue>): string;
begin
  // Call Next to invoke the rest of the pipeline and get the response
  Result := Next().AsType<string>;
end;

// Void behavior (IRequest)
type
  TJingBehavior = class(TPipelineBehavior<TJing>)
  public
    procedure Handle(Request: TJing; Next: TProc); override;
  end;

procedure TJingBehavior.Handle(Request: TJing; Next: TProc);
begin
  Next();  // continue pipeline
end;
```

The framework detects the concrete parameter type of `Handle` via RTTI and skips the behavior for non-matching request types automatically. Typed behaviors also apply to subclasses of the declared request type.

### Registration

```pascal
// Auto-discovery (scans package for IBasePipelineBehavior implementations)
Services.AddMediatRBehaviors(_T.PackageOf<TMyClass>);

// Manual registration
Services.AddTransient(TypeInfo(IPipelineBehavior), TLoggingBehavior);
Services.AddTransient(TypeInfo(IPipelineBehavior), TPingBehavior);
```

Mark behaviors with `[MediatorAbstract]` to exclude them from auto-discovery (useful for behaviors you register manually only in specific scenarios).

### Execution order

Behaviors execute in **registration order** — first registered is the outermost wrapper:

```
Invoke: Behavior1 → Invoke: Behavior2 → Handler → return to Behavior2 → return to Behavior1
```

### Short-circuiting

Don't call `Next` (or `Next()`) to abort the pipeline. The handler and inner behaviors are never invoked, but outer behaviors still complete normally:

```pascal
function TAuthBehavior.Invoke(Request: TObject; Next: TFunc<TValue>): TValue;
begin
  if not IsAuthenticated then
    Result := Default(TValue)  // short-circuit: handler is never called
  else
    Result := Next();
end;
```

### Exception handling

Wrap `Next()` in a `try/except` to catch exceptions from the handler or inner behaviors:

```pascal
function TErrorHandlerBehavior.Invoke(Request: TObject; Next: TFunc<TValue>): TValue;
begin
  try
    Result := Next();
  except on E: Exception do
  begin
    Log('Error: ' + E.Message);
    Result := Default(TValue);
  end;
  end;
end;
```

### Dependency injection

Behaviors are resolved from the DI container. Constructor parameters are injected automatically:

```pascal
type
  TLoggingBehavior = class(TPipelineBehaviorBase)
  private
    FLogger: ILogger;
  public
    constructor Create(const Logger: ILogger);
    function Invoke(Request: TObject; Next: TFunc<TValue>): TValue; override;
  end;
```

### Backward compatibility

Pipeline behaviors are opt-in. If no behaviors are registered, `Send` invokes the handler directly with no additional overhead.

---

## Documentation
