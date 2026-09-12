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
7. [Registro y auto-escaneo](#registro-y-auto-escaneo)
8. [Atributo MediatorAbstract](#atributo-mediatorabstract)
9. [Behaviors de pipeline](#behaviors-de-pipeline)
10. [Referencia de clases base](#referencia-de-clases-base)

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
uses Daf.MediatR.Contracts;

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

`IMediator` es un **record** que envuelve al `IMediatorImpl` registrado. Resuelve la implementación y
asígnala: la conversión implícita te entrega el record.

```pascal
IMediator = record
  // Comando — sin valor de retorno
  procedure Send<TRequest: class, IRequest>(Request: TRequest); overload;

  // Consulta — con valor de retorno
  function Send<TResponse; TRequest: class, IRequest<TResponse>>(
    Request: TRequest): TResponse; overload;

  // Notificación — fan-out a todos los handlers registrados
  procedure Publish<TNotification: class, INotification>(
    Notification: TNotification);
end;
```

Uso:

```pascal
var Mediator: IMediator := Provider.GetRequiredService<IMediatorImpl>;

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

`IMediatorHelper` es un **record helper** de `IMediator`, así que `SendARC` está sobre el propio mediador y no hay nada que resolver. Envuelve una respuesta de **clase** en `ARC<TResponse>`, que se libera al salir del scope:

```pascal
IMediatorHelper = record helper for IMediator
  function SendARC<TResponse: class; TRequest: class, IRequest<TResponse>>(
    const Request: TRequest): ARC<TResponse>;
end;
```

```pascal
var Q := TGetOrdersQuery.Create;
var Orders := Mediator.SendARC<TOrderList, TGetOrdersQuery>(Q);
// Orders es ARC<TOrderList> — la lista se libera al salir Orders del scope
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

## Registro y auto-escaneo

```pascal
uses Daf.Rtti, Daf.MediatR.DependencyInjection;

Services.AddMediatR;                                     // el mediador
Services.AddMediatRClasses(_T.PackageOf<TMyHandler>);    // los handlers de ese paquete
Services.AddMediatRBehaviors(_T.PackageOf<TMyHandler>);  // y sus behaviors de pipeline
```

`AddMediatR` registra **`IMediatorImpl` como transient**, y solo si no lo había registrado nadie antes. No escanea nada: el descubrimiento es `AddMediatRClasses(Package)`, que recorre el RTTI de ese paquete buscando interfaces handler y **registra lo que encuentra** — los handlers no tienen que estar antes en el contenedor.

⚠️ Los tres son métodos de un `record helper` sobre `IServiceCollection`, y solo un helper por tipo está en alcance. Si otro `uses` lo ensombrece, las mismas llamadas viven en la clase `MediatR`: `MediatR.AddTo(Services)` y `MediatR.AddTo(Services, Package)`.

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

## Behaviors de pipeline

Los behaviors interceptan cada llamada a `Send` y permiten añadir lógica transversal (logging, validación, caché, gestión de errores) sin modificar los handlers.

### Tres variantes de behavior

Todas usan el mismo patrón `Next.Call`; el tipo de `Next` varía según la variante:

| Clase base | Se aplica a | Tipo de `Next` | Sobreescribe |
|------------|------------|----------------|--------------|
| `TPipelineBehavior` | toda request | `TNextValue` | `function Handle(Request: TObject; Next: TNextValue): TValue` |
| `TPipelineBehavior<TReq>` | un tipo de request void | `TNext` | `procedure Handle(Request: TReq; Next: TNext)` |
| `TPipelineBehavior<TRes, TReq>` | un tipo de request con respuesta | `TNext<TRes>` | `function Handle(Request: TReq; Next: TNext<TRes>): TRes` |

### Behavior global (todas las requests)

```pascal
type
  TLoggingBehavior = class(TPipelineBehavior)
  public
    function Handle(Request: TObject; Next: TNextValue): TValue; override;
  end;

function TLoggingBehavior.Handle(Request: TObject; Next: TNextValue): TValue;
begin
  Log('Antes de ' + Request.ClassName);
  Result := Next.Call;
  Log('Después de ' + Request.ClassName);
end;
```

### Behavior tipado void (un tipo `IRequest`)

```pascal
type
  TValidationBehavior = class(TPipelineBehavior<TCreateOrderCommand>)
  public
    procedure Handle(Request: TCreateOrderCommand; Next: TNext); override;
  end;

procedure TValidationBehavior.Handle(Request: TCreateOrderCommand; Next: TNext);
begin
  if Request.Name.IsEmpty then
    Exit;  // cortocircuito — el handler nunca se llama
  Next.Call;
end;
```

### Behavior tipado con respuesta (un tipo `IRequest<TResponse>`)

```pascal
type
  TValidatePingBehavior = class(TPipelineBehavior<string, TPing>)
  public
    function Handle(Request: TPing; Next: TNext<string>): string; override;
  end;

function TValidatePingBehavior.Handle(Request: TPing; Next: TNext<string>): string;
begin
  if Request.Target.IsEmpty then
    raise EArgumentException.Create('Target requerido');
  Result := Next.Call;
end;
```

El framework usa RTTI para determinar si un behavior aplica al tipo de request actual. Los behaviors tipados también se aplican a subclases.

### Cortocircuito

No llamar a `Next.Call` aborta el pipeline:

```pascal
procedure TValidationBehavior.Handle(Request: TCreateOrderCommand; Next: TNext);
begin
  if not IsValid(Request) then
    Exit;   // el handler nunca se llama
  Next.Call;
end;
```

Para behaviors globales devuelve `Default(TValue)` en lugar de llamar a `Next.Call`:

```pascal
function TAuthBehavior.Handle(Request: TObject; Next: TNextValue): TValue;
begin
  if not Authenticated then
    Result := Default(TValue)
  else
    Result := Next.Call;
end;
```

### Gestión de excepciones

Envuelve `Next.Call` para capturar errores de behaviors internos y del handler:

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

### Inyección de dependencias

Los parámetros del constructor se resuelven del contenedor automáticamente:

```pascal
constructor TLoggingBehavior.Create(const Logger: ILogger);
begin
  inherited Create;
  FLogger := Logger;
end;
```

### Registro

```pascal
// Auto-descubrimiento (omite clases marcadas [MediatorAbstract])
MediatR.AddTo(ServiceCollection, _T.PackageOf<TMyClass>);

// Manual
MediatR.AddBehavior(ServiceCollection, TLoggingBehavior);
MediatR.AddBehavior(ServiceCollection, TValidatePingBehavior);
```

Los behaviors se ejecutan en orden de registro — el primero registrado es el más externo. Para behaviors que solo registras manualmente, márcalos con `[MediatorAbstract]` para excluirlos del auto-descubrimiento.

---

## Referencia de clases base

| Clase base | Sobreescribe | ¿Llamar inherited? |
|------------|--------------|---------------------|
| `TRequestHandler<TReq>` | `procedure Handle(Request: TReq)` | No |
| `TResponseHandler<TRes, TReq>` | `procedure Handle(Request: TReq; out Result: TRes)` | No |
| `TNotificacionHandler<TNot>` | `procedure Handle(Notification: TNot)` | No |
| `TRequest` | — | Hereda `IRequest` |
| `TRequest<TResponse>` | — | Hereda `IRequest<TResponse>` |
| `TNotification` | — | Hereda `INotification` |
| `TPipelineBehavior` | `function Handle(Request: TObject; Next: TNextValue): TValue` | No |
| `TPipelineBehavior<TReq>` | `procedure Handle(Request: TReq; Next: TNext)` | No |
| `TPipelineBehavior<TRes, TReq>` | `function Handle(Request: TReq; Next: TNext<TRes>): TRes` | No |
