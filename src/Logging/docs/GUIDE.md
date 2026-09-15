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
ILogger = interface(IInvokable)
  // Core write
  procedure Log(const Level: TLogLevel; const Msg: string; const Args: TArray<TValue> = nil); overload;
  procedure Log(const Level: TLogLevel; const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;

  // Convenience wrappers
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
  property Template: string;                             // '{UserId} logged in from {IP}'
  property Arguments: TArray<TValue>;                    // in the order they were passed
  property KeyValuePairs: TArray<TPair<string, TValue>>; // ('UserId', ...), ('IP', ...)
  property Formatter: TLogFormatter;                     // what renders the final line
  function TryGetValue(const Key: string; out Value: TValue): Boolean;
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

The scope text reaches the entry as `TLogEntry.Scope`, and a provider decides how to show it. With
NNLog the token is `${scope}`: adding it to a target's layout puts the open scopes on every line
written inside them.

---

## ILoggerFactory

```pascal
ILoggerFactory = interface(IInvokable)
  function CreateLogger(const Category: string): ILogger; overload;
  function CreateLogger(const AClass: TClass): ILogger; overload;
  function CreateLogger(const AType: PTypeInfo): ILogger; overload;
end;
```

### Generic helper

On the **concrete** factory, not on the contract: a Delphi interface cannot declare a parameterized
method. Through `ILoggerFactory` the same result is `CreateLogger(TMyService)`.

```pascal
var Factory := TMultiProviderLoggerFactory.Create;
var Logger  := Factory.CreateLogger<TMyService>;
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
| `TNNLogProvider` | `Daf.NNLog` | Rule-based targets (file, console, …) |
| `TNullLoggerProvider` | `Daf.Extensions.Logging` | Drops every entry — how to ask for silence on purpose |

**There is no `Daf.Logging.Console` and no `Daf.Logging.Debug`.** NNLog brings `Console`, `Debug`,
`File_` and `UDP`, with filtering rules and layouts. And a startup failure reaches somebody without
any of that: the bootstrap channel writes to the error output and to the system debug output with
nothing configured.

### Adding a provider

```pascal
Factory.AddProvider(TNNLogProvider.Create(Targets, Rules));
```

---

## Dependency Injection Integration

```pascal
uses Daf.Extensions.Logging, Daf.Logging.Builder, Daf.NNLog;

// While composing startup, on the builder's Services:
AddLogging(Builder.Services,
  procedure(Log: ILoggingBuilder)
  begin
    Log.AddProvider(TNNLogProvider.Create(Targets, Rules));
  end);
```

`AddLogging` registers:
- `ILoggerFactory` as singleton
- Internally calls `Builder.Build` to wire providers before the host starts

### Composing with no destination is an error

A pipeline with no provider accepts every entry and drops it, which is indistinguishable from one that
works — until someone debugs an incident reading records that were never written. So `AddLogging`
refuses it:

```pascal
AddLogging(Services,
  procedure(Builder: ILoggingBuilder)
  begin
    // the AddProvider that is not there
  end);
// ELoggingCompositionError
```

The check runs **once, where the pipeline is composed** — inside the host builder's `Build`, before a
single hosted service exists and before the first entry, and never on the write path.

Silence on purpose is still fine. It just has to be said:

```pascal
AddLogging(Services,
  procedure(Builder: ILoggingBuilder)
  begin
    Builder.AddNullProvider;
  end);
```

| What you write | What you get |
|----------------|--------------|
| No `AddLogging` at all | No `ILoggerFactory` in the container. Startup reports still come out through the bootstrap channel |
| `AddLogging` with `AddNullProvider` | A factory that drops every entry — declared silence |
| `AddLogging` with no provider | `ELoggingCompositionError` while the host is being built |

The middle row is the one you want when your own code resolves `ILoggerFactory` or takes an injected
`ILogger`: without `AddLogging` there is no factory to inject. If your application simply does not log,
not calling `AddLogging` is the cheaper route and there is nothing to declare.

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

2. Implement `ILogger`:

```pascal
type
  TMyLogger = class(TInterfacedObject, ILogger)
    procedure Log(const Level: TLogLevel; const Msg: string;
                  const Args: TArray<TValue> = nil); overload;
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
  EventId   : TEventId;
  Exception : Exception;      // nil when no exception
  Scope     : string;         // rendered scope chain
  State     : TLogState;      // template + values
  Formatter : TLogFormatter;  // what turns State into the line
end;
```

Every field is populated before `Write(Entry)` is called on a `TTarget`. `Message` is not among them:
it is a read-only property, and the first read runs `Formatter` over `State`, appends `Scope` and
keeps the result — so a target that renders its own layout out of `State` never pays for it.
