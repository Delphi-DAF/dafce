# MediatR.Abstractions

**🌍 Idioma: [English](README.md) | Español**

Interfaces y clases base fundamentales para la mensajería en proceso MediatR de DAFce. Referencia estas desde el código de aplicación y las implementaciones de handlers sin depender del mediador concreto.

Todos los tipos se encuentran en `Daf.MediatR.Contracts` — el paquete es `Daf.MediatR.Abstractions`, que no es una unit y no vale en un `uses`.

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
IMediator = record            // un record, no una interfaz: envuelve un IMediatorImpl
  procedure Send<TRequest: class, IRequest>(Request: TRequest); overload;
  function  Send<TResponse; TRequest: class, IRequest<TResponse>>(
              Request: TRequest): TResponse; overload;
  procedure Publish<TNotification: class, INotification>(
              Notification: TNotification);
end;
```

### IMediatorHelper

Un **record helper** de `IMediator`, así que `SendARC` está sobre el propio mediador. Envuelve una respuesta de **clase** en `ARC<TResponse>`, que se libera al salir del scope.

```pascal
IMediatorHelper = record helper for IMediator
  function SendARC<TResponse: class; TRequest: class, IRequest<TResponse>>(
             const Request: TRequest): ARC<TResponse>;
end;
```

### IRequestHandler\<TRequest\>

```pascal
IRequestHandler<TRequest: class, IRequest> = interface
  procedure Handle(Request: TRequest);
end;
```

### IRequestHandler\<TResponse, TRequest\>

El mismo nombre con dos parámetros genéricos: Delphi distingue las dos sobrecargas por aridad. La
respuesta vuelve por un parámetro `out`, no como resultado de la función.

```pascal
IRequestHandler<TResponse; TRequest: class, IRequest<TResponse>> = interface
  procedure Handle(Request: TRequest; out Result: TResponse);
end;
```

### INotificationHandler\<TNotification\>

```pascal
INotificationHandler<TNotification: class, INotification> = interface
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
| `TNotificationHandler<TNot>` | Hereda para manejar un evento — sobreescribe `Handle` |

---

## Atributos

### [MediatorAbstract]

Aplicar a clases base abstractas de handlers para que el escaneo de DI las salte:
`AddMediatRClasses` y `AddMediatRBehaviors` no registran nunca una clase que lo lleve, que es
lo que evita pedirle al contenedor una clase abstracta. Sin escaneo, el atributo no hace nada.

```pascal
[MediatorAbstract]
TBaseOrderHandler<T: TRequest> = class(TRequestHandler<T>);
```

---

## Relacionado

- [MediatR](../MediatR/README.es.md) — mediador concreto, helpers de registro en DI
- [MediatR — Guía de uso](../MediatR/docs/GUIDE.es.md)
