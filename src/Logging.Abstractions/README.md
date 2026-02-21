# Logging.Abstractions

**🌍 Language: English | [Español](README.es.md)**

Core contracts for the DAFce logging infrastructure. Reference these interfaces when building components that emit log entries or when implementing custom log providers — without taking a dependency on any concrete logging implementation.

---

## Interfaces

### ILogger

Per-category façade that emits log entries.

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

Creates `ILogger` instances and fans writes out to all registered providers.

```pascal
ILoggerFactory = interface
  function  CreateLogger(const Category: string): ILogger; overload;
  function  CreateLogger(AClass: TClass): ILogger; overload;
  function  CreateLogger(TypeInfo: PTypeInfo): ILogger; overload;
  procedure AddProvider(Provider: ILoggerProvider);
end;
```

Generic helper: `Factory.CreateLogger<TMyClass>` — uses the class name as category.

---

### ILoggerProvider

Produce `ILogger` instances for a specific output destination.

```pascal
ILoggerProvider = interface
  function CreateLogger(const Category: string): ILogger;
end;
```

---

### ILoggingBuilder

Fluent interface for configuring providers before the host starts (used inside `AddLogging`).

```pascal
ILoggingBuilder = interface
  procedure AddProvider(Provider: ILoggerProvider);
  function  Build: ILoggerFactory;
end;
```

---

### ILogScope / ILogScopeVoid

Represent an active logging scope. `ILogScopeVoid` is the version returned by `BeginScope` when the scope proc does not return a value.

---

## Value types

### TLogLevel

```pascal
TLogLevel = (Trace, Debug, Information, Warning, Error, Critical, None);
```

### TLogState

Holds a structured log message — template string plus positional args.

```pascal
TLogState = record
  Template: string;
  Args: TArray<TVarRec>;
  function Format: string;
end;
```

### TLogEntry

Single log entry delivered to a provider's `Write` method.

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

## Related

- [Logging](../Logging/README.md) — concrete implementation and DI registration
- [Logging — Usage Guide](../Logging/docs/GUIDE.md)
- [NNLog](../NNLog/README.md) — rule-based provider
