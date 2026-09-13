# MediatR.Abstractions

**🌍 Language: English | [Español](README.es.md)**

Core interfaces and base classes for the DAFce MediatR in-process messaging. Reference these from application code and handler implementations without taking a dependency on the concrete mediator.

All types live in `Daf.MediatR.Contracts` — the package is `Daf.MediatR.Abstractions`, which is not a unit and cannot be used in a `uses` clause.

---

## Interfaces

### IRequest

Fire-and-forget command — dispatched to exactly one handler, no return value.

```pascal
IRequest = interface
end;
```

### IRequest\<TResponse\>

Query — dispatched to exactly one handler, returns `TResponse`.

```pascal
IRequest<TResponse> = interface(IRequest)
end;
```

### INotification

Event — published to **all** registered handlers.

```pascal
INotification = interface
end;
```

### IMediator

```pascal
IMediator = record            // a record, not an interface: it wraps an IMediatorImpl
  procedure Send<TRequest: class, IRequest>(Request: TRequest); overload;
  function  Send<TResponse; TRequest: class, IRequest<TResponse>>(
              Request: TRequest): TResponse; overload;
  procedure Publish<TNotification: class, INotification>(
              Notification: TNotification);
end;
```

### IMediatorHelper

A **record helper** for `IMediator`, so `SendARC` sits on the mediator itself. Wraps a **class** response in `ARC<TResponse>`, released when it goes out of scope.

```pascal
IMediatorHelper = record helper for IMediator
  function SendARC<TResponse: class; TRequest: class, IRequest<TResponse>>(
             const Request: TRequest): ARC<TResponse>;
end;
```

### IRequestHandler\<TRequest\>

```pascal
IRequestHandler<TRequest: IRequest> = interface
  procedure Handle(Request: TRequest);
end;
```

### IResponseHandler\<TResponse, TRequest\>

```pascal
IResponseHandler<TResponse; TRequest: IRequest<TResponse>> = interface
  function Handle(Request: TRequest): TResponse;
end;
```

### INotificationHandler\<TNotification\>

```pascal
INotificationHandler<TNotification: INotification> = interface
  procedure Handle(Notification: TNotification);
end;
```

---

## Base classes

| Class | Use |
|-------|-----|
| `TRequest` | Inherit to define a command |
| `TRequest<TResponse>` | Inherit to define a query |
| `TNotification` | Inherit to define an event |
| `TRequestHandler<TReq>` | Inherit to handle a command — override `Handle` |
| `TResponseHandler<TRes,TReq>` | Inherit to handle a query — override `Handle` |
| `TNotificacionHandler<TNot>` | Inherit to handle an event — override `Handle` |

---

## Attributes

### [MediatorAbstract]

Apply to abstract handler base classes to prevent the mediator from resolving them directly:

```pascal
[MediatorAbstract]
TBaseOrderHandler<T: TRequest> = class(TRequestHandler<T>);
```

---

## Related

- [MediatR](../MediatR/README.md) — concrete mediator, DI registration helpers
- [MediatR — Usage Guide](../MediatR/docs/GUIDE.md)
