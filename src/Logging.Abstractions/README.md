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
  procedure AddNullProvider;
  function  Build: ILoggerFactory;
end;
```

`AddNullProvider` is how an application says it wants its entries dropped. Composing with no provider
at all is not the same thing — that raises `ELoggingCompositionError`, because a pipeline with no
destination is indistinguishable from one that works. See
[Logging — Usage Guide](../Logging/docs/GUIDE.md#composing-with-no-destination-is-an-error).

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

## What lives here, and why

This is the contracts package, and yet it also holds concrete classes: `TNullLogger`,
`TNullLoggerFactory`, `TNullLoggerProvider`, and the bootstrap channel destinations
(`TDebugOutputBootstrapSink`, `TStdErrBootstrapSink`, `TFileBootstrapSink`). That looks like a
contradiction, so here is the rule that decides where each piece of logging goes:

> **A package boundary is earned by isolating a dependency the others do not need.**

The rule answers in both directions, and the two cases already settled are the examples:

| Piece | Dependency it drags in | Where it lives |
|-------|------------------------|----------------|
| `TStdErrBootstrapSink`, `TDebugOutputBootstrapSink`, `TFileBootstrapSink` | nothing beyond the RTL | here, with the contract |
| `TPlatformLogBootstrapSink` | FireMonkey | `Daf.FMX` — a package of its own |
| `TNullLogger`, `TNullLoggerFactory`, `TNullLoggerProvider` | nothing | here, with the contract |

A destination that needs nothing but the standard library earns no boundary; one that leans on a
graphical framework does, which is why the FireMonkey sink has a package of its own. .NET puts
`NullLogger` in `Microsoft.Extensions.Logging.Abstractions` for the same reason: a default with no
dependencies that everyone needs.

There is a second reason that applies only to the bootstrap destinations: **moving them to another
package would deregister them by default.** An application that today gets the error output and the
system debug output without asking for anything would end up with no destination at all unless it
added a reference — which is exactly the worst case `BootstrapLog.HasAvailableSink` exists to make
visible.

---

## Related

- [Logging](../Logging/README.md) — concrete implementation and DI registration
- [Logging — Usage Guide](../Logging/docs/GUIDE.md)
- [NNLog](../NNLog/README.md) — rule-based provider
