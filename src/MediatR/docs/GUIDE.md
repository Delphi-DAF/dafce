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
7. [Auto-scan with AddMediatR](#auto-scan-with-addmediatr)
8. [MediatorAbstract attribute](#mediatorabstract-attribute)
9. [Pipeline Behaviors](#pipeline-behaviors)
10. [Base class reference](#base-class-reference)

---

## Message types

| Interface | Base class | Handler interface | Base handler class |
|-----------|-----------|-------------------|--------------------|
| `IRequest` | `TRequest` | `IRequestHandler<T>` | `TRequestHandler<T>` |
| `IRequest<TResponse>` | `TRequest<TResponse>` | `IResponseHandler<TRes,TReq>` | `TResponseHandler<TRes,TReq>` |
| `INotification` | `TNotification` | `INotificationHandler<T>` | `TNotificacionHandler<T>` |

---

## Defining messages

### Command (no return value)

```pascal
uses Daf.MediatR.Abstractions;

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
    function Handle(Request: TGetUserQuery): TUserDTO; override;
  end;

function TGetUserHandler.Handle(Request: TGetUserQuery): TUserDTO;
begin
  Result := FRepo.FindById(Request.UserId);
end;
```

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

```pascal
IMediator = interface
  // Command — no return value
  procedure Send<TRequest: IRequest>(Request: TRequest); overload;

  // Query — with return value
  function Send<TResponse; TRequest: IRequest<TResponse>>(
    Request: TRequest): TResponse; overload;

  // Notification — fan-out to all registered handlers
  procedure Publish<TNotification: INotification>(
    Notification: TNotification);
end;
```

Usage:

```pascal
var Mediator := Provider.GetRequiredService<IMediator>;

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

When the response is a reference-counted object, use `IMediatorHelper.SendARC` to receive it wrapped in `ARC<TResponse>`:

```pascal
IMediatorHelper = interface(IMediator)
  function SendARC<TResponse: IInterface; TRequest: IRequest<TResponse>>(
    Request: TRequest): ARC<TResponse>;
end;
```

```pascal
var Helper := Provider.GetRequiredService<IMediatorHelper>;
var ArcResult := Helper.SendARC<IOrderList, TGetOrdersQuery>(Q);
// ArcResult is ARC<IOrderList> — auto-released when it goes out of scope
```

---

## Registering handlers

Each handler is registered as a transient service in DI:

```pascal
// Command handler
Services.AddTransient<IRequestHandler<TDeleteUserCommand>, TDeleteUserHandler>;

// Query handler
Services.AddTransient<IResponseHandler<TUserDTO, TGetUserQuery>, TGetUserHandler>;

// Notification handler (multiple handlers supported)
Services.AddTransient<INotificationHandler<TUserDeletedEvent>, TEmailOnUserDeleted>;
Services.AddTransient<INotificationHandler<TUserDeletedEvent>, TAuditLogHandler>;
```

---

## Auto-scan with AddMediatR

```pascal
uses Daf.MediatR.DependencyInjection;

Services.AddMediatR;
```

`AddMediatR` registers `IMediator` and `IMediatorHelper` as singletons and scans all registered types for handler interfaces, registering them automatically. Handlers must still be in the DI container for the scan to pick them up.

---

## MediatorAbstract attribute

Mark abstract base handler classes with `[MediatorAbstract]` so the mediator does not attempt to resolve them:

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
MediatR.AddTo(ServiceCollection, _T.PackageOf<TMyClass>);

// Manual
MediatR.AddBehavior(ServiceCollection, TLoggingBehavior);
MediatR.AddBehavior(ServiceCollection, TValidatePingBehavior);
```

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
