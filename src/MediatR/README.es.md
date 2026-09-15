# MediatR

**🌍 Idioma: [English](README.md) | Español**

Mensajería en proceso para Delphi — comandos, consultas y notificaciones despachados a través de una única interfaz `IMediator`. Inspirado en la biblioteca .NET MediatR.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![Licencia](https://img.shields.io/badge/licencia-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Conceptos

| Tipo | Dirección | Devuelve | Caso de uso |
|------|-----------|----------|-------------|
| `IRequest` | 1 → 1 handler | — | Comando (fire-and-forget) |
| `IRequest<TResponse>` | 1 → 1 handler | `TResponse` | Consulta |
| `INotification` | 1 → N handlers | — | Evento de dominio |

---

## Inicio rápido — Comando

```pascal
// 1. Definir comando
type
  TCreateOrderCommand = class(TRequest)
    OrderId: Integer;
  end;

// 2. Implementar handler
type
  TCreateOrderHandler = class(TRequestHandler<TCreateOrderCommand>)
    procedure Handle(Request: TCreateOrderCommand); override;
  end;

// 3. Registrar handler
Services.AddTransient<IRequestHandler<TCreateOrderCommand>, TCreateOrderHandler>;

// 4. Despachar
Mediator.Send<TCreateOrderCommand>(Cmd);
```

---

## Inicio rápido — Consulta

```pascal
type
  TGetOrderQuery    = class(TRequest<TOrderDTO>);
  TGetOrderHandler  = class(TResponseHandler<TOrderDTO, TGetOrderQuery>)
    procedure Handle(Request: TGetOrderQuery; out Result: TOrderDTO); override;
  end;

var DTO := Mediator.Send<TOrderDTO, TGetOrderQuery>(Query);
```

---

## Inicio rápido — Notificación

```pascal
type
  TOrderCreatedEvent    = class(TNotification);
  TEmailNotifyHandler   = class(TNotificationHandler<TOrderCreatedEvent>)
    procedure Handle(Notification: TOrderCreatedEvent); override;
  end;

// Registrar tantos handlers como sea necesario
Services.AddTransient<INotificationHandler<TOrderCreatedEvent>, TEmailNotifyHandler>;
Services.AddTransient<INotificationHandler<TOrderCreatedEvent>, TAuditLogHandler>;

// Publicar — se llama a TODOS los handlers
Mediator.Publish<TOrderCreatedEvent>(Evt);
```

---

## Registro en DI

```pascal
uses Daf.Rtti, Daf.MediatR.DependencyInjection;

Services.AddMediatR;                                   // registra IMediatorImpl como transient
Services.AddMediatRClasses(_T.PackageOf<TMyHandler>);  // descubre y registra los handlers
```

O manualmente por handler:

```pascal
Services.AddTransient<IRequestHandler<TMyCommand>, TMyCommandHandler>;
```

---

## Behaviors de pipeline

Los behaviors envuelven cada llamada `Send` con lógica transversal (logging, validación, caché, etc.).

Hay tres variantes — todas usan el mismo patrón `Next.Call`:

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
  TQueryResultBehavior = class(TPipelineBehavior<TOrderList, TGetOrdersQuery>)
  public
    function Handle(Request: TGetOrdersQuery; Next: TNext<TOrderList>): TOrderList; override;
  end;

function TQueryResultBehavior.Handle(Request: TGetOrdersQuery; Next: TNext<TOrderList>): TOrderList;
begin
  Result := Next.Call;
  Log(Format('%d pedido(s) devueltos', [Result.Count]));
end;
```

El framework usa RTTI para detectar el tipo concreto de request en `Handle` y omite el behavior para tipos no coincidentes. Los behaviors tipados también se aplican a subclases.

### Registro

```pascal
// Auto-descubrimiento (omite [MediatorAbstract])
ServiceCollection.AddMediatRBehaviors(_T.PackageOf<TMyClass>);

// Manual
MediatR.AddBehavior(ServiceCollection, TLoggingBehavior);
MediatR.AddBehavior(ServiceCollection, TValidationBehavior);
```

Los behaviors tienen su propio escaneo: `AddMediatRClasses` descubre handlers y nada más, así que un
paquete registrado solo con él no aporta ningún behavior — y nadie lo avisa, porque una request sin
behaviors registrados va directa a su handler.

Usa `[MediatorAbstract]` en behaviors que solo registres manualmente para excluirlos del auto-descubrimiento.

### Orden de ejecución

Los behaviors se ejecutan en orden de registro — el primero registrado es el envoltorio más externo.

### Cortocircuito

No llamar a `Next.Call` aborta el pipeline sin invocar el handler:

```pascal
procedure TValidationBehavior.Handle(Request: TCreateOrderCommand; Next: TNext);
begin
  if not IsValid(Request) then
    Exit;  // el handler nunca se llama
  Next.Call;
end;
```

### Manejo de excepciones

Envuelve `Next.Call` en un `try/except` para capturar las excepciones del handler y de los behaviors
internos:

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

### Inyección de dependencias

Los behaviors se resuelven del contenedor de DI. Los parámetros del constructor se inyectan
automáticamente:

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

### Compatibilidad hacia atrás

Los behaviors de pipeline son opcionales. Si no hay ninguno registrado, `Send` invoca el handler
directamente y sin coste adicional.

---

## Documentación

- 📖 [Guía de uso](docs/GUIDE.es.md) — IMediatorHelper, respuestas ARC, pipeline, [MediatorAbstract], escaneo de DI
- 📁 [MediatRSample](samples/MediatRSample/README.md) — comandos, consultas y notificaciones en un solo programa
