# MediatR.Abstractions

**🌍 Language: English | [Español](README.es.md)**

Core interfaces and base classes for the DAFce MediatR in-process messaging. Reference these from application code and handler implementations without taking a dependency on the concrete mediator.

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
IMediator = interface
  procedure Send<TRequest: IRequest>(Request: TRequest); overload;
  function  Send<TResponse; TRequest: IRequest<TResponse>>(
              Request: TRequest): TResponse; overload;
  procedure Publish<TNotification: INotification>(
              Notification: TNotification);
end;
```

### IMediatorHelper

Extends `IMediator` with `SendARC` — returns the response wrapped in `ARC<TResponse>` for safe reference-counted ownership.

```pascal
IMediatorHelper = interface(IMediator)
  function SendARC<TResponse: IInterface; TRequest: IRequest<TResponse>>(
             Request: TRequest): ARC<TResponse>;
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
TBaseHandler<T: TRequest> = class(TRequestHandler<T>);
```

---

## Related

- [MediatR](../MediatR/README.md) — concrete mediator, DI registration helpers
- [MediatR — Usage Guide](../MediatR/docs/GUIDE.md)
