# Logging.Abstractions

**🌍 Idioma: [English](README.md) | Español**

Contratos fundamentales de la infraestructura de logging de DAFce. Referencia estas interfaces al construir componentes que emiten entradas de log o al implementar proveedores personalizados — sin depender de ninguna implementación concreta de logging.

---

## Interfaces

### ILogger

Fachada por categoría que emite entradas de log.

```pascal
ILogger = interface
  procedure Log(Level: TLogLevel; const Msg: string; const Args: array of const); overload;
  procedure Log(Level: TLogLevel; E: Exception; const Msg: string; const Args: array of const); overload;

  procedure LogTrace      (const Msg: string; const Args: array of const); overload;
  procedure LogDebug      (const Msg: string; const Args: array of const); overload;
  procedure LogInformation(const Msg: string; const Args: array of const); overload;
  procedure LogWarning    (const Msg: string; const Args: array of const); overload;
  procedure LogError      (const Msg: string; const Args: array of const); overload;
  procedure LogError      (E: Exception; const Msg: string; const Args: array of const); overload;
  procedure LogCritical   (const Msg: string; const Args: array of const); overload;
  procedure LogCritical   (E: Exception; const Msg: string; const Args: array of const); overload;

  function BeginScope(const Msg: string; const Args: array of const;
                      ScopedProc: TProc): ILogScopeVoid; overload;
end;
```

---

### ILoggerFactory

Crea instancias de `ILogger` y distribuye las escrituras a todos los proveedores registrados.

```pascal
ILoggerFactory = interface
  function  CreateLogger(const Category: string): ILogger; overload;
  function  CreateLogger(AClass: TClass): ILogger; overload;
  function  CreateLogger(TypeInfo: PTypeInfo): ILogger; overload;
  procedure AddProvider(Provider: ILoggerProvider);
end;
```

Ayuda genérica: `Factory.CreateLogger<TMyClass>` — usa el nombre de la clase como categoría.

---

### ILoggerProvider

Produce instancias de `ILogger` para un destino de salida específico.

```pascal
ILoggerProvider = interface
  function CreateLogger(const Category: string): ILogger;
end;
```

---

### ILoggingBuilder

Interfaz fluida para configurar proveedores antes de que el host arranque (se usa dentro de `AddLogging`).

```pascal
ILoggingBuilder = interface
  procedure AddProvider(Provider: ILoggerProvider);
  function  Build: ILoggerFactory;
end;
```

---

### ILogScope / ILogScopeVoid

Representan un scope de logging activo. `ILogScopeVoid` es la versión devuelta por `BeginScope` cuando el procedimiento de scope no devuelve ningún valor.

---

## Tipos de valor

### TLogLevel

```pascal
TLogLevel = (Trace, Debug, Information, Warning, Error, Critical, None);
```

### TLogState

Contiene un mensaje de log estructurado — plantilla de cadena más argumentos posicionales.

```pascal
TLogState = record
  Template: string;
  Args: TArray<TVarRec>;
  function Format: string;
end;
```

### TLogEntry

Entrada de log individual entregada al método `Write` de un proveedor.

```pascal
TLogEntry = record
  Level     : TLogLevel;
  Category  : string;
  EventId   : Integer;
  Exception : Exception;
  Scope     : string;
  State     : TLogState;
  Message   : string;
end;
```

---

## Relacionado

- [Logging](../Logging/README.es.md) — implementación concreta y registro en DI
- [Logging — Guía de uso](../Logging/docs/GUIDE.es.md)
- [NNLog](../NNLog/README.es.md) — proveedor basado en reglas
