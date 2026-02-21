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
ILogger = interface
  // Escritura principal
  procedure Log(Level: TLogLevel; const Msg: string; const Args: array of const); overload;
  procedure Log(Level: TLogLevel; E: Exception; const Msg: string; const Args: array of const); overload;

  // Métodos de conveniencia
  procedure LogTrace      (const Msg: string; const Args: array of const); overload;
  procedure LogDebug      (const Msg: string; const Args: array of const); overload;
  procedure LogInformation(const Msg: string; const Args: array of const); overload;
  procedure LogWarning    (const Msg: string; const Args: array of const); overload;
  procedure LogError      (const Msg: string; const Args: array of const); overload;
  procedure LogError      (E: Exception; const Msg: string; const Args: array of const); overload;
  procedure LogCritical   (const Msg: string; const Args: array of const); overload;
  procedure LogCritical   (E: Exception; const Msg: string; const Args: array of const); overload;

  // Scopes
  function BeginScope(const Msg: string; const Args: array of const;
                      ScopedProc: TProc): ILogScopeVoid; overload;
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
  Template: string;          // '{UserId} autenticado desde {IP}'
  Args: TArray<TVarRec>;     // valores posicionales
  function Format: string;   // renderiza la cadena final
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
    // Ambas líneas llevan los datos del scope
  end);
```

Los scopes están basados en pila — los datos de scopes anidados se acumulan. Al salir, el scope se extrae automáticamente.

---

## ILoggerFactory

```pascal
ILoggerFactory = interface
  function  CreateLogger(const Category: string): ILogger; overload;
  function  CreateLogger(AClass: TClass): ILogger; overload;
  function  CreateLogger(TypeInfo: PTypeInfo): ILogger; overload;
  procedure AddProvider(Provider: ILoggerProvider);
end;
```

### Ayuda genérica

```pascal
// Devuelve un logger categorizado por nombre de clase
var Logger := Factory.CreateLogger<TMyService>;
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
| `TNNLogProvider` | `DAF.NNLog` | Targets basados en reglas (fichero, consola, …) |

### Añadir un proveedor

```pascal
Factory.AddProvider(TNNLogProvider.Create(Targets, Rules));
```

---

## Integración con DI

```pascal
uses DAF.Logging.Builder, DAF.NNLog;

// En ConfigureServices:
AddLogging(Services,
  procedure(Builder: ILoggingBuilder)
  begin
    Builder.AddProvider(TNNLogProvider.Create(Targets, Rules));
  end);
```

`AddLogging` registra:
- `ILoggerFactory` como singleton
- Llama internamente a `Builder.Build` para conectar los proveedores antes de arrancar el host

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

2. Implementa `ILogger` (generalmente derivando de `TLoggerBase`):

```pascal
type
  TMyLogger = class(TInterfacedObject, ILogger)
    procedure Log(Level: TLogLevel; const Msg: string;
                  const Args: array of const); overload;
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
  EventId   : Integer;
  Exception : Exception;      // nil si no hay excepción
  Scope     : string;         // cadena del scope renderizado
  State     : TLogState;      // plantilla + argumentos
  Message   : string;         // cadena pre-renderizada
end;
```

Todos los campos se rellenan antes de que se llame a `Write(Entry)` en un `TTarget`.
