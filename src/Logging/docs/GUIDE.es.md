# Logging — Guía de uso

**🌍 Idioma: [English](GUIDE.md) | Español**

---

## Tabla de contenidos

1. [Arquitectura](#arquitectura)
2. [API de ILogger](#api-de-ilogger)
3. [Niveles de log](#niveles-de-log)
4. [Logging estructurado](#logging-estructurado)
5. [Scopes](#scopes)
6. [ILoggerFactory](#iloggerfactory)
7. [Proveedores](#proveedores)
8. [Integración con DI](#integración-con-di)
9. [Crear un proveedor personalizado](#crear-un-proveedor-personalizado)
10. [Referencia de TLogEntry](#referencia-de-tlogentry)

---

## Arquitectura

```
Tu código
   │
   ▼
ILogger           ← fachada por categoría
   │
   ▼
ILoggerFactory    ← TMultiProviderLoggerFactory
   │  (fan-out)
   ├─► ILoggerProvider A  → ILogger A
   ├─► ILoggerProvider B  → ILogger B
   └─► ILoggerProvider C  → ILogger C
```

Un único `ILogger` delega cada escritura a **todos los proveedores registrados**. Cada proveedor puede escribir a ficheros, consola, sinks remotos, etc., de forma independiente.

---

## API de ILogger

```pascal
ILogger = interface(IInvokable)
  // Escritura principal
  procedure Log(const Level: TLogLevel; const Msg: string; const Args: TArray<TValue> = nil); overload;
  procedure Log(const Level: TLogLevel; const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;

  // Métodos de conveniencia
  procedure LogTrace      (const Msg: string; const Args: TArray<TValue> = nil); overload;
  procedure LogDebug      (const Msg: string; const Args: TArray<TValue> = nil); overload;
  procedure LogInformation(const Msg: string; const Args: TArray<TValue> = nil); overload;
  procedure LogWarning    (const Msg: string; const Args: TArray<TValue> = nil); overload;
  procedure LogError      (const Msg: string; const Args: TArray<TValue> = nil); overload;
  procedure LogError      (const Ex: Exception; const Msg: string = ''; const Args: TArray<TValue> = nil); overload;
  procedure LogCritical   (const Msg: string; const Args: TArray<TValue> = nil); overload;
  procedure LogCritical   (const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;

  // Scopes
  function BeginScope(const Msg: string; const Args: TArray<TValue>;
                      const ScopedProc: TProc = nil): ILogScopeVoid; overload;
end;
```

---

## Niveles de log

```pascal
TLogLevel = (Trace, Debug, Information, Warning, Error, Critical, None);
```

| Valor | Constante | Cuándo usarlo |
|------:|-----------|---------------|
| 0 | `Trace` | Muy detallado — desactivado en producción |
| 1 | `Debug` | Diagnóstico durante el desarrollo |
| 2 | `Information` | Hitos del flujo normal de la aplicación |
| 3 | `Warning` | Inesperado pero recuperable |
| 4 | `Error` | La operación actual ha fallado |
| 5 | `Critical` | Requiere atención inmediata |
| 6 | `None` | Desactiva el logging completamente |

---

## Logging estructurado

Los marcadores nombrados en la plantilla de mensaje se emparejan posicionalmente con `Args`:

```pascal
Logger.LogInformation('Usuario {UserId} autenticado desde {IP}', [UserId, RemoteIP]);
Logger.LogWarning    ('Reintento {Attempt} de {Max} de {Op}', [3, 5, 'SendEmail']);
```

Internamente, `TLogState` contiene la plantilla y el array de argumentos. Los proveedores reciben el record `TLogState` y pueden formatearlo de forma independiente (cadena plana, JSON, etc.).

### Record TLogState

```pascal
TLogState = record
  property Template: string;                             // '{UserId} autenticado desde {IP}'
  property Arguments: TArray<TValue>;                    // en el orden en que se pasaron
  property KeyValuePairs: TArray<TPair<string, TValue>>; // ('UserId', ...), ('IP', ...)
  property Formatter: TLogFormatter;                     // lo que renderiza la línea final
  function TryGetValue(const Key: string; out Value: TValue): Boolean;
end;
```

---

## Scopes

Un scope adjunta contexto ambiental a todas las entradas de log emitidas dentro de un bloque:

```pascal
Logger.BeginScope('Procesando pedido {OrderId}', [OrderId],
  procedure
  begin
    Logger.LogInformation('Comprobando stock', []);
    Logger.LogInformation('Pago autorizado', []);
    // Las dos entradas llevan el scope, aparte de su mensaje
  end);
```

Los scopes están basados en pila — los datos de scopes anidados se acumulan. Al salir, el scope se extrae automáticamente.

El scope llega a la entrada como `TLogEntry.Scope`, aparte de su mensaje: `TLogEntry.Message` es el
mensaje formateado y nada más. `Scope` lleva los scopes abiertos del más interno al más externo,
cada uno precedido de ` | `, y va vacío fuera de todo scope. Cada proveedor decide si lo escribe y
dónde. En NNLog el token es `${scope}`: `${message}${scope}` pone los scopes justo detrás del
mensaje, y un layout sin `${scope}` no los escribe.

---

## ILoggerFactory

```pascal
ILoggerFactory = interface(IInvokable)
  function CreateLogger(const Category: string): ILogger; overload;
  function CreateLogger(const AClass: TClass): ILogger; overload;
  function CreateLogger(const AType: PTypeInfo): ILogger; overload;
end;
```

### Ayuda genérica

Está en la factoría **concreta**, no en el contrato: una interfaz de Delphi no admite un método
parametrizado. Por `ILoggerFactory` el mismo resultado es `CreateLogger(TMyService)`.

```pascal
var Factory := TMultiProviderLoggerFactory.Create;
var Logger  := Factory.CreateLogger<TMyService>;
```

### Instanciación directa (sin DI)

```pascal
var Factory := TMultiProviderLoggerFactory.Create;
Factory.AddProvider(MiProveedor);
```

---

## Proveedores

Un proveedor convierte records `TLogEntry` en salida:

```pascal
ILoggerProvider = interface
  function CreateLogger(const Category: string): ILogger;
end;
```

### Proveedores incluidos

| Proveedor | Unidad | Descripción |
|-----------|--------|-------------|
| `TNNLogProvider` | `Daf.NNLog` | Targets basados en reglas (fichero, consola, …) |
| `TNullLoggerProvider` | `Daf.Extensions.Logging` | Descarta toda entrada — así se pide silencio a propósito |

**No hay `Daf.Logging.Console` ni `Daf.Logging.Debug`.** NNLog trae `Console`, `Debug`, `File_` y
`UDP`, con reglas de filtrado y layouts. Y un fallo de arranque llega a alguien sin nada de eso: el
canal de arranque escribe a la salida de error y a la de depuración del sistema sin configurar nada.

### Añadir un proveedor

```pascal
Factory.AddProvider(TNNLogProvider.Create(Targets, Rules));
```

---

## Integración con DI

```pascal
uses Daf.Extensions.Logging, Daf.Logging.Builder, Daf.NNLog;

// Al componer el arranque, sobre los Services del builder:
AddLogging(Builder.Services,
  procedure(Log: ILoggingBuilder)
  begin
    Log.AddProvider(TNNLogProvider.Create(Targets, Rules));
  end);
```

`AddLogging` registra:
- `ILoggerFactory` como singleton
- Llama internamente a `Builder.Build` para conectar los proveedores antes de arrancar el host

### Componer sin destino es un error

Un pipeline sin proveedores acepta todas las entradas y las descarta, que es indistinguible de uno que
funciona — hasta que alguien depura un incidente leyendo unos registros que nunca se escribieron. Así
que `AddLogging` no lo acepta:

```pascal
AddLogging(Services,
  procedure(Builder: ILoggingBuilder)
  begin
    // el AddProvider que no está
  end);
// ELoggingCompositionError
```

La comprobación corre **una sola vez, donde se compone el pipeline** — dentro del `Build` del builder
del host, antes de que exista un solo servicio hospedado y antes de la primera entrada, y nunca al
escribir.

El silencio a propósito sigue estando bien. Solo hay que decirlo:

```pascal
AddLogging(Services,
  procedure(Builder: ILoggingBuilder)
  begin
    Builder.AddNullProvider;
  end);
```

| Lo que escribes | Lo que obtienes |
|-----------------|-----------------|
| No llamas a `AddLogging` | No hay `ILoggerFactory` en el contenedor. Los informes de arranque siguen saliendo por el canal de arranque |
| `AddLogging` con `AddNullProvider` | Una factoría que descarta toda entrada — silencio declarado |
| `AddLogging` sin proveedores | `ELoggingCompositionError` al construir el host |

La fila del medio es la que quieres cuando tu propio código resuelve `ILoggerFactory` o recibe un
`ILogger` inyectado: sin `AddLogging` no hay factoría que inyectar. Si tu aplicación simplemente no
loguea, no llamar a `AddLogging` es la ruta más barata y no hay nada que declarar.

Resolver en servicios:

```pascal
constructor TMyService.Create(Factory: ILoggerFactory);
begin
  FLogger := Factory.CreateLogger<TMyService>;
end;
```

---

## Crear un proveedor personalizado

1. Implementa `ILoggerProvider`:

```pascal
type
  TMyProvider = class(TInterfacedObject, ILoggerProvider)
    function CreateLogger(const Category: string): ILogger;
  end;

function TMyProvider.CreateLogger(const Category: string): ILogger;
begin
  Result := TMyLogger.Create(Category);
end;
```

2. Implementa `ILogger`:

```pascal
type
  TMyLogger = class(TInterfacedObject, ILogger)
    procedure Log(const Level: TLogLevel; const Msg: string;
                  const Args: TArray<TValue> = nil); overload;
    // … el resto de sobrecargas delegan a esta
  end;
```

3. Registra:

```pascal
Factory.AddProvider(TMyProvider.Create);
// o a través del Builder dentro de AddLogging(...)
```

---

## Referencia de TLogEntry

```pascal
TLogEntry = record
  Level     : TLogLevel;
  Category  : string;
  EventId   : TEventId;
  Exception : Exception;      // nil si no hay excepción
  Scope     : string;         // cadena del scope renderizado
  State     : TLogState;      // plantilla + valores
  Formatter : TLogFormatter;  // lo que convierte State en la línea
end;
```

Todos los campos se rellenan antes de que se llame a `Write(Entry)` en un `TTarget`. `Message` no es
uno de ellos: es una propiedad de solo lectura, y la primera lectura pasa `Formatter` sobre `State` y
guarda el resultado — así que un target que renderiza su propio layout desde `State` no lo paga
nunca. `Message` no incluye `Scope`: un proveedor que quiera los scopes los escribe desde `Scope`.
