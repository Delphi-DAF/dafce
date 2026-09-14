# MediatR — Usage Guide

**🌍 Language: English | [Español](GUIDE.es.md)**

---

## Table of Contents

1. [Message types](#message-types)
2. [Defining messages](#defining-messages)
3. [Implementing handlers](#implementing-handlers)
4. [IMediator API](#imediator-api)
5. [IMediatorHelper — ARC responses](#imediatorhelper--arc-responses)
6. [Registering handlers](#registering-handlers)
7. [Registration and auto-scan](#registration-and-auto-scan)
8. [MediatorAbstract attribute](#mediatorabstract-attribute)
9. [Pipeline Behaviors](#pipeline-behaviors)
10. [Base class reference](#base-class-reference)

---

## Message types

| Interface | Base class | Handler interface | Base handler class |
|-----------|-----------|-------------------|--------------------|
| `IRequest` | `TRequest` | `IRequestHandler<T>` | `TRequestHandler<T>` |
| `IRequest<TResponse>` | `TRequest<TResponse>` | `IRequestHandler<TRes,TReq>` | `TResponseHandler<TRes,TReq>` |
| `INotification` | `TNotification` | `INotificationHandler<T>` | `TNotificacionHandler<T>` |

---

## Defining messages

### Command (no return value)

```pascal
uses Daf.MediatR.Contracts;

type
  TDeleteUserCommand = class(TRequest)
    UserId: Integer;
  end;
```

### Query (with return value)

```pascal
type
  TGetUserQuery = class(TRequest<TUserDTO>)
    UserId: Integer;
  end;
```

### Notification (event)

```pascal
type
  TUserDeletedEvent = class(TNotification)
    UserId: Integer;
    DeletedAt: TDateTime;
  end;
```

---

## Implementing handlers

### Command handler

```pascal
type
  TDeleteUserHandler = class(TRequestHandler<TDeleteUserCommand>)
  private
    FRepo: IUserRepository;
  public
    constructor Create(Repo: IUserRepository);
    procedure Handle(Request: TDeleteUserCommand); override;
  end;

procedure TDeleteUserHandler.Handle(Request: TDeleteUserCommand);
begin
  FRepo.Delete(Request.UserId);
end;
```

### Query handler

```pascal
type
  TGetUserHandler = class(TResponseHandler<TUserDTO, TGetUserQuery>)
  private
    FRepo: IUserRepository;
  public
    constructor Create(Repo: IUserRepository);
    procedure Handle(Request: TGetUserQuery; out Result: TUserDTO); override;
  end;

procedure TGetUserHandler.Handle(Request: TGetUserQuery; out Result: TUserDTO);
begin
  Result := FRepo.FindById(Request.UserId);
end;
```

The response leaves through an `out` parameter, not as the function result — the mediator reads it
back from there.

### Notification handler

```pascal
type
  TEmailOnUserDeleted = class(TNotificacionHandler<TUserDeletedEvent>)
    procedure Handle(Notification: TUserDeletedEvent); override;
  end;

procedure TEmailOnUserDeleted.Handle(Notification: TUserDeletedEvent);
begin
  // send farewell email
end;
```

---

## IMediator API

`IMediator` is a **record** wrapping the registered `IMediatorImpl`. Resolve the implementation and
assign it: the implicit conversion hands you the record.

```pascal
IMediator = record
  // Command — no return value
  procedure Send<TRequest: class, IRequest>(Request: TRequest); overload;

  // Query — with return value
  function Send<TResponse; TRequest: class, IRequest<TResponse>>(
    Request: TRequest): TResponse; overload;

  // Notification — fan-out to all registered handlers
  procedure Publish<TNotification: class, INotification>(
    Notification: TNotification);
end;
```

Usage:

```pascal
var Mediator: IMediator := Provider.GetRequiredService<IMediatorImpl>;

// Command
var Cmd := TDeleteUserCommand.Create;
Cmd.UserId := 42;
Mediator.Send<TDeleteUserCommand>(Cmd);

// Query
var Q := TGetUserQuery.Create;
Q.UserId := 42;
var DTO := Mediator.Send<TUserDTO, TGetUserQuery>(Q);

// Notification
var Evt := TUserDeletedEvent.Create;
Evt.UserId := 42;
Mediator.Publish<TUserDeletedEvent>(Evt);
```

---

## IMediatorHelper — ARC responses

`IMediatorHelper` is a **record helper** for `IMediator`, so `SendARC` sits on the mediator itself and there is nothing to resolve. It wraps a **class** response in `ARC<TResponse>`, released when it goes out of scope:

```pascal
IMediatorHelper = record helper for IMediator
  function SendARC<TResponse: class; TRequest: class, IRequest<TResponse>>(
    const Request: TRequest): ARC<TResponse>;
end;
```

```pascal
var Q := TGetOrdersQuery.Create;
var Orders := Mediator.SendARC<TOrderList, TGetOrdersQuery>(Q);
// Orders is ARC<TOrderList> — the list is released when Orders goes out of scope
```

---

## Registering handlers

Each handler is registered as a transient service in DI:

```pascal
// Command handler
Services.AddTransient<IRequestHandler<TDeleteUserCommand>, TDeleteUserHandler>;

// Query handler
Services.AddTransient<IRequestHandler<TUserDTO, TGetUserQuery>, TGetUserHandler>;

// Notification handler (multiple handlers supported)
Services.AddTransient<INotificationHandler<TUserDeletedEvent>, TEmailOnUserDeleted>;
Services.AddTransient<INotificationHandler<TUserDeletedEvent>, TAuditLogHandler>;
```

---

## Registration and auto-scan

```pascal
uses Daf.Rtti, Daf.MediatR.DependencyInjection;

Services.AddMediatR;                                     // the mediator itself
Services.AddMediatRClasses(_T.PackageOf<TMyHandler>);    // handlers in that package
Services.AddMediatRBehaviors(_T.PackageOf<TMyHandler>);  // and its pipeline behaviors
```

`AddMediatR` registers **`IMediatorImpl` as transient**, and only if nothing registered it before. It scans nothing: discovery is `AddMediatRClasses(Package)`, which walks that package's RTTI for handler interfaces and **registers what it finds** — the handlers do not have to be in the container first.

⚠️ The three are `record helper` methods on `IServiceCollection`, and only one helper per type is in scope. Where another `uses` shadows it, the `MediatR` class covers the first two — `MediatR.AddTo(Services)` and `MediatR.AddTo(Services, Package)` — and behaviors go one at a time through `MediatR.AddBehavior(Services, TMyBehavior)`. There is no class-side equivalent of `AddMediatRBehaviors`.

---

## MediatorAbstract attribute

Mark abstract base handler classes with `[MediatorAbstract]` so the DI scan skips them:
`AddMediatRClasses` and `AddMediatRBehaviors` never register a class carrying it, which is what
keeps the container from being asked to instantiate an abstract class. Registering by hand
ignores the attribute — only the scans read it.

```pascal
[MediatorAbstract]
TBaseOrderHandler<T: TRequest> = class(TRequestHandler<T>)
  // shared logic
end;

TCreateOrderHandler = class(TBaseOrderHandler<TCreateOrderCommand>)
  // concrete — NOT marked MediatorAbstract
end;
```

---

## Pipeline Behaviors

Behaviors intercept every `Send` call and let you add cross-cutting logic (logging, validation, caching, error handling) without touching your handlers.

### Three behavior flavors

All use the same `Next.Call` pattern; the type of `Next` varies by flavor:

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
  TValidatePingBehavior = class(TPipelineBehavior<string, TPing>)
  public
    function Handle(Request: TPing; Next: TNext<string>): string; override;
  end;

function TValidatePingBehavior.Handle(Request: TPing; Next: TNext<string>): string;
begin
  if Request.Target.IsEmpty then
    raise EArgumentException.Create('Target required');
  Result := Next.Call;
end;
```

The framework uses RTTI to determine whether a behavior applies to the current request type. Typed behaviors also match subclasses.

### Short-circuiting

Don't call `Next.Call` to abort the pipeline:

```pascal
procedure TAuthBehavior.Handle(Request: TCreateOrderCommand; Next: TNext);
begin
  if not Authenticated then
    Exit;   // handler is never called
  Next.Call;
end;
```

For global behaviors return `Default(TValue)` instead of calling `Next.Call`:

```pascal
function TAuthBehavior.Handle(Request: TObject; Next: TNextValue): TValue;
begin
  if not Authenticated then
    Result := Default(TValue)
  else
    Result := Next.Call;
end;
```

### Exception handling

Wrap `Next.Call` to catch errors from inner behaviors and the handler:

```pascal
function TErrorBehavior.Handle(Request: TObject; Next: TNextValue): TValue;
begin
  try
    Result := Next.Call;
  except on E: Exception do
    Result := Default(TValue);
  end;
end;
```

### Dependency injection

Constructor parameters are resolved from the container automatically:

```pascal
constructor TLoggingBehavior.Create(const Logger: ILogger);
begin
  inherited Create;
  FLogger := Logger;
end;
```

### Registration

```pascal
// Auto-discovery (skips classes marked [MediatorAbstract])
ServiceCollection.AddMediatRBehaviors(_T.PackageOf<TMyClass>);

// Manual
MediatR.AddBehavior(ServiceCollection, TLoggingBehavior);
MediatR.AddBehavior(ServiceCollection, TValidatePingBehavior);
```

⚠️ `AddMediatRBehaviors` is the behavior scan, and `AddMediatRClasses` is not: the latter discovers
handlers only, so a package registered through it alone contributes no behavior — with no error to
show for it, because a request with no behaviors registered goes straight to its handler.

Behaviors execute in registration order — first registered is outermost. For behaviors you only register manually, mark them `[MediatorAbstract]` to exclude them from auto-discovery.

---

## Base class reference

| Base class | Override | Must call inherited? |
|------------|----------|---------------------|
| `TRequestHandler<TReq>` | `procedure Handle(Request: TReq)` | No |
| `TResponseHandler<TRes, TReq>` | `procedure Handle(Request: TReq; out Result: TRes)` | No |
| `TNotificacionHandler<TNot>` | `procedure Handle(Notification: TNot)` | No |
| `TRequest` | — | Inherits `IRequest` |
| `TRequest<TResponse>` | — | Inherits `IRequest<TResponse>` |
| `TNotification` | — | Inherits `INotification` |
| `TPipelineBehavior` | `function Handle(Request: TObject; Next: TNextValue): TValue` | No |
| `TPipelineBehavior<TReq>` | `procedure Handle(Request: TReq; Next: TNext)` | No |
| `TPipelineBehavior<TRes, TReq>` | `function Handle(Request: TReq; Next: TNext<TRes>): TRes` | No |
