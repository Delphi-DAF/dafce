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
9. [Base class reference](#base-class-reference)

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

## Base class reference

| Base class | Override | Must call inherited? |
|------------|----------|---------------------|
| `TRequestHandler<TReq>` | `procedure Handle(Request: TReq)` | No |
| `TResponseHandler<TRes, TReq>` | `function Handle(Request: TReq): TRes` | No |
| `TNotificacionHandler<TNot>` | `procedure Handle(Notification: TNot)` | No |
| `TRequest` | — | Inherits `IRequest` |
| `TRequest<TResponse>` | — | Inherits `IRequest<TResponse>` |
| `TNotification` | — | Inherits `INotification` |
