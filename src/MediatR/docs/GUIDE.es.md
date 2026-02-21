# MediatR — Guía de uso

**🌍 Idioma: [English](GUIDE.md) | Español**

---

## Tabla de contenidos

1. [Tipos de mensaje](#tipos-de-mensaje)
2. [Definir mensajes](#definir-mensajes)
3. [Implementar handlers](#implementar-handlers)
4. [API de IMediator](#api-de-imediator)
5. [IMediatorHelper — respuestas ARC](#imediatorhelper--respuestas-arc)
6. [Registrar handlers](#registrar-handlers)
7. [Auto-escaneo con AddMediatR](#auto-escaneo-con-addmediatr)
8. [Atributo MediatorAbstract](#atributo-mediatorabstract)
9. [Referencia de clases base](#referencia-de-clases-base)

---

## Tipos de mensaje

| Interfaz | Clase base | Interfaz handler | Clase base handler |
|----------|-----------|------------------|--------------------|
| `IRequest` | `TRequest` | `IRequestHandler<T>` | `TRequestHandler<T>` |
| `IRequest<TResponse>` | `TRequest<TResponse>` | `IResponseHandler<TRes,TReq>` | `TResponseHandler<TRes,TReq>` |
| `INotification` | `TNotification` | `INotificationHandler<T>` | `TNotificacionHandler<T>` |

---

## Definir mensajes

### Comando (sin valor de retorno)

```pascal
uses Daf.MediatR.Abstractions;

type
  TDeleteUserCommand = class(TRequest)
    UserId: Integer;
  end;
```

### Consulta (con valor de retorno)

```pascal
type
  TGetUserQuery = class(TRequest<TUserDTO>)
    UserId: Integer;
  end;
```

### Notificación (evento)

```pascal
type
  TUserDeletedEvent = class(TNotification)
    UserId: Integer;
    DeletedAt: TDateTime;
  end;
```

---

## Implementar handlers

### Handler de comando

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

### Handler de consulta

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

### Handler de notificación

```pascal
type
  TEmailOnUserDeleted = class(TNotificacionHandler<TUserDeletedEvent>)
    procedure Handle(Notification: TUserDeletedEvent); override;
  end;

procedure TEmailOnUserDeleted.Handle(Notification: TUserDeletedEvent);
begin
  // enviar email de despedida
end;
```

---

## API de IMediator

```pascal
IMediator = interface
  // Comando — sin valor de retorno
  procedure Send<TRequest: IRequest>(Request: TRequest); overload;

  // Consulta — con valor de retorno
  function Send<TResponse; TRequest: IRequest<TResponse>>(
    Request: TRequest): TResponse; overload;

  // Notificación — fan-out a todos los handlers registrados
  procedure Publish<TNotification: INotification>(
    Notification: TNotification);
end;
```

Uso:

```pascal
var Mediator := Provider.GetRequiredService<IMediator>;

// Comando
var Cmd := TDeleteUserCommand.Create;
Cmd.UserId := 42;
Mediator.Send<TDeleteUserCommand>(Cmd);

// Consulta
var Q := TGetUserQuery.Create;
Q.UserId := 42;
var DTO := Mediator.Send<TUserDTO, TGetUserQuery>(Q);

// Notificación
var Evt := TUserDeletedEvent.Create;
Evt.UserId := 42;
Mediator.Publish<TUserDeletedEvent>(Evt);
```

---

## IMediatorHelper — respuestas ARC

Cuando la respuesta es un objeto con conteo de referencias, usa `IMediatorHelper.SendARC` para recibirla envuelta en `ARC<TResponse>`:

```pascal
IMediatorHelper = interface(IMediator)
  function SendARC<TResponse: IInterface; TRequest: IRequest<TResponse>>(
    Request: TRequest): ARC<TResponse>;
end;
```

```pascal
var Helper := Provider.GetRequiredService<IMediatorHelper>;
var ArcResult := Helper.SendARC<IOrderList, TGetOrdersQuery>(Q);
// ArcResult es ARC<IOrderList> — liberado automáticamente al salir del scope
```

---

## Registrar handlers

Cada handler se registra como servicio transitorio en DI:

```pascal
// Handler de comando
Services.AddTransient<IRequestHandler<TDeleteUserCommand>, TDeleteUserHandler>;

// Handler de consulta
Services.AddTransient<IResponseHandler<TUserDTO, TGetUserQuery>, TGetUserHandler>;

// Handler de notificación (se soportan múltiples handlers)
Services.AddTransient<INotificationHandler<TUserDeletedEvent>, TEmailOnUserDeleted>;
Services.AddTransient<INotificationHandler<TUserDeletedEvent>, TAuditLogHandler>;
```

---

## Auto-escaneo con AddMediatR

```pascal
uses Daf.MediatR.DependencyInjection;

Services.AddMediatR;
```

`AddMediatR` registra `IMediator` e `IMediatorHelper` como singletons y escanea todos los tipos registrados en busca de interfaces handler, registrándolos automáticamente. Los handlers deben estar ya en el contenedor DI para que el escaneo los detecte.

---

## Atributo MediatorAbstract

Marca las clases base de handler abstractas con `[MediatorAbstract]` para que el mediador no intente resolverlas:

```pascal
[MediatorAbstract]
TBaseOrderHandler<T: TRequest> = class(TRequestHandler<T>)
  // lógica compartida
end;

TCreateOrderHandler = class(TBaseOrderHandler<TCreateOrderCommand>)
  // concreto — NO marcado con MediatorAbstract
end;
```

---

## Referencia de clases base

| Clase base | Sobreescribe | ¿Llamar inherited? |
|------------|--------------|---------------------|
| `TRequestHandler<TReq>` | `procedure Handle(Request: TReq)` | No |
| `TResponseHandler<TRes, TReq>` | `function Handle(Request: TReq): TRes` | No |
| `TNotificacionHandler<TNot>` | `procedure Handle(Notification: TNot)` | No |
| `TRequest` | — | Hereda `IRequest` |
| `TRequest<TResponse>` | — | Hereda `IRequest<TResponse>` |
| `TNotification` | — | Hereda `INotification` |
