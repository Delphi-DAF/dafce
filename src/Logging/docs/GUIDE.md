# Logging — Usage Guide

**🌍 Language: English | [Español](GUIDE.es.md)**

---

## Table of Contents

1. [Architecture](#architecture)
2. [ILogger API](#ilogger-api)
3. [Log Levels](#log-levels)
4. [Structured Logging](#structured-logging)
5. [Scopes](#scopes)
6. [ILoggerFactory](#iloggerfactory)
7. [Providers](#providers)
8. [Dependency Injection Integration](#dependency-injection-integration)
9. [Creating a Custom Provider](#creating-a-custom-provider)
10. [TLogEntry Reference](#tlogentry-reference)

---

## Architecture

```
Your code
   │
   ▼
ILogger           ← per-category façade
   │
   ▼
ILoggerFactory    ← TMultiProviderLoggerFactory
   │  (fan-out)
   ├─► ILoggerProvider A  → ILogger A
   ├─► ILoggerProvider B  → ILogger B
   └─► ILoggerProvider C  → ILogger C
```

A single `ILogger` delegates every write to **all registered providers**. Each provider can write to files, consoles, sinks, or remote endpoints independently.

---

## ILogger API

```pascal
ILogger = interface
  // Core write
  procedure Log(Level: TLogLevel; const Msg: string; const Args: array of const); overload;
  procedure Log(Level: TLogLevel; E: Exception; const Msg: string; const Args: array of const); overload;

  // Convenience wrappers
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

## Log Levels

```pascal
TLogLevel = (Trace, Debug, Information, Warning, Error, Critical, None);
```

| Value | Constant | Typical use |
|------:|----------|-------------|
| 0 | `Trace` | Very fine-grained, off in production |
| 1 | `Debug` | Diagnostic during development |
| 2 | `Information` | Normal operational milestones |
| 3 | `Warning` | Unexpected but recoverable |
| 4 | `Error` | Current operation failed |
| 5 | `Critical` | Requires immediate attention |
| 6 | `None` | Disable logging entirely |

---

## Structured Logging

Named placeholders in the message template are matched positionally to `Args`:

```pascal
Logger.LogInformation('User {UserId} logged in from {IP}', [UserId, RemoteIP]);
Logger.LogWarning('Retry {Attempt} of {Max} for {Op}', [3, 5, 'SendEmail']);
```

Internally, `TLogState` holds the template string and the argument array. Providers receive the `TLogState` record and may format them independently (flat string, JSON, etc.).

### TLogState record

```pascal
TLogState = record
  Template: string;          // '{UserId} logged in from {IP}'
  Args: TArray<TVarRec>;     // positional values
  function Format: string;   // renders the final string
end;
```

---

## Scopes

A scope attaches ambient context to all log entries emitted inside a block:

```pascal
Logger.BeginScope('Processing order {OrderId}', [OrderId],
  procedure
  begin
    Logger.LogInformation('Checking stock', []);
    Logger.LogInformation('Payment authorised', []);
    // Both lines carry the scope data
  end);
```

Scopes are stack-based — nested scope data accumulates. On exit the scope is popped automatically.

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

### Generic helper

```pascal
// Returns a logger categorised by class name
var Logger := Factory.CreateLogger<TMyService>;
```

### Direct instantiation (no DI)

```pascal
var Factory := TMultiProviderLoggerFactory.Create;
Factory.AddProvider(MyProvider);
```

---

## Providers

A provider converts `TLogEntry` records into output:

```pascal
ILoggerProvider = interface
  function CreateLogger(const Category: string): ILogger;
end;
```

### Built-in providers

| Provider | Unit | Description |
|----------|------|-------------|
| `TNNLogProvider` | `DAF.NNLog` | Rule-based targets (file, console, …) |

### Adding a provider

```pascal
Factory.AddProvider(TNNLogProvider.Create(Targets, Rules));
```

---

## Dependency Injection Integration

```pascal
uses DAF.Logging.Builder, DAF.NNLog;

// In ConfigureServices:
AddLogging(Services,
  procedure(Builder: ILoggingBuilder)
  begin
    Builder.AddProvider(TNNLogProvider.Create(Targets, Rules));
  end);
```

`AddLogging` registers:
- `ILoggerFactory` as singleton
- Internally calls `Builder.Build` to wire providers before the host starts

Resolve in services:

```pascal
constructor TMyService.Create(Factory: ILoggerFactory);
begin
  FLogger := Factory.CreateLogger<TMyService>;
end;
```

---

## Creating a Custom Provider

1. Implement `ILoggerProvider`:

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

2. Implement `ILogger` (usually via `TLoggerBase`):

```pascal
type
  TMyLogger = class(TInterfacedObject, ILogger)
    procedure Log(Level: TLogLevel; const Msg: string;
                  const Args: array of const); overload;
    // … other overloads delegate to this one
  end;
```

3. Register:

```pascal
Factory.AddProvider(TMyProvider.Create);
// or via Builder inside AddLogging(...)
```

---

## TLogEntry Reference

```pascal
TLogEntry = record
  Level     : TLogLevel;
  Category  : string;
  EventId   : Integer;
  Exception : Exception;      // nil when no exception
  Scope     : string;         // rendered scope chain
  State     : TLogState;      // template + args
  Message   : string;         // pre-rendered string
end;
```

All fields are populated before `Write(Entry)` is called on a `TTarget`.
