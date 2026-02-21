# MediatR.Abstractions

**🌍 Idioma: [English](README.md) | Español**

Interfaces y clases base fundamentales para la mensajería en proceso MediatR de DAFce. Referencia estas desde el código de aplicación y las implementaciones de handlers sin depender del mediador concreto.

---

## Interfaces

### IRequest

Comando fire-and-forget — despachado a exactamente un handler, sin valor de retorno.

```pascal
IRequest = interface
end;
```

### IRequest\<TResponse\>

Consulta — despachada a exactamente un handler, devuelve `TResponse`.

```pascal
IRequest<TResponse> = interface(IRequest)
end;
```

### INotification

Evento — publicado a **todos** los handlers registrados.

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

Extiende `IMediator` con `SendARC` — devuelve la respuesta envuelta en `ARC<TResponse>` para una propiedad con conteo de referencias segura.

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

## Clases base

| Clase | Uso |
|-------|-----|
| `TRequest` | Hereda para definir un comando |
| `TRequest<TResponse>` | Hereda para definir una consulta |
| `TNotification` | Hereda para definir un evento |
| `TRequestHandler<TReq>` | Hereda para manejar un comando — sobreescribe `Handle` |
| `TResponseHandler<TRes,TReq>` | Hereda para manejar una consulta — sobreescribe `Handle` |
| `TNotificacionHandler<TNot>` | Hereda para manejar un evento — sobreescribe `Handle` |

---

## Atributos

### [MediatorAbstract]

Aplicar a clases base abstractas de handlers para evitar que el mediador las resuelva directamente:

```pascal
[MediatorAbstract]
TBaseHandler<T: TRequest> = class(TRequestHandler<T>);
```

---

## Relacionado

- [MediatR](../MediatR/README.es.md) — mediador concreto, helpers de registro en DI
- [MediatR — Guía de uso](../MediatR/docs/GUIDE.es.md)
