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

There are three behavior flavors — all use the same `Next.Call` pattern:

| Base class | Applies to | `Next` type | Override |
|------------|-----------|-------------|----------|
| `TPipelineBehavior` | every request | `TNextValue` | `function Handle(Request: TObject; Next: TNextValue): TValue` |
| `TPipelineBehavior<TReq>` | one void request type | `TNext` | `procedure Handle(Request: TReq; Next: TNext)` |
| `TPipelineBehavior<TRes, TReq>` | one response request type | `TNext<TRes>` | `function Handle(Request: TReq; Next: TNext<TRes>): TRes` |

### Global behavior (all requests)

```pascal
type
  TLoggingBehavior = class(TPipelineBehavior)
  public
    function Handle(Request: TObject; Next: TNextValue): TValue; override;
  end;

function TLoggingBehavior.Handle(Request: TObject; Next: TNextValue): TValue;
begin
  Log('Before ' + Request.ClassName);
  Result := Next.Call;
  Log('After ' + Request.ClassName);
end;
```

### Typed void behavior (one `IRequest` type)

```pascal
type
  TValidationBehavior = class(TPipelineBehavior<TCreateOrderCommand>)
  public
    procedure Handle(Request: TCreateOrderCommand; Next: TNext); override;
  end;

procedure TValidationBehavior.Handle(Request: TCreateOrderCommand; Next: TNext);
begin
  if Request.Name.IsEmpty then
    Exit;  // short-circuit — handler never called
  Next.Call;
end;
```

### Typed response behavior (one `IRequest<TResponse>` type)

```pascal
type
  TQueryResultBehavior = class(TPipelineBehavior<TOrderList, TGetOrdersQuery>)
  public
    function Handle(Request: TGetOrdersQuery; Next: TNext<TOrderList>): TOrderList; override;
  end;

function TQueryResultBehavior.Handle(Request: TGetOrdersQuery; Next: TNext<TOrderList>): TOrderList;
begin
  Result := Next.Call;
  Log(Format('%d order(s) returned', [Result.Count]));
end;
```

The framework uses RTTI to detect the concrete request type of `Handle` and skips the behavior for non-matching types automatically. Typed behaviors also apply to subclasses.

### Registration

```pascal
// Auto-discovery (scans package, skips [MediatorAbstract])
MediatR.AddTo(ServiceCollection, _T.PackageOf<TMyClass>);

// Manual
MediatR.AddBehavior(ServiceCollection, TLoggingBehavior);
MediatR.AddBehavior(ServiceCollection, TValidationBehavior);
```

### Execution order

Behaviors execute in **registration order** — first registered is the outermost wrapper:

```
Behavior1 → Behavior2 → Handler → return to Behavior2 → return to Behavior1
```

### Short-circuiting

Don't call `Next.Call` to abort the pipeline. The handler and inner behaviors are never invoked:

```pascal
procedure TValidationBehavior.Handle(Request: TCreateOrderCommand; Next: TNext);
begin
  if not IsValid(Request) then
    Exit;  // handler is never called
  Next.Call;
end;
```

### Exception handling

Wrap `Next.Call` in a `try/except` to catch exceptions from the handler or inner behaviors:

```pascal
function TErrorBehavior.Handle(Request: TObject; Next: TNextValue): TValue;
begin
  try
    Result := Next.Call;
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
  TLoggingBehavior = class(TPipelineBehavior)
  private
    FLogger: ILogger;
  public
    constructor Create(const Logger: ILogger);
    function Handle(Request: TObject; Next: TNextValue): TValue; override;
  end;
```

### Backward compatibility

Pipeline behaviors are opt-in. If no behaviors are registered, `Send` invokes the handler directly with no additional overhead.

---

## Documentation
