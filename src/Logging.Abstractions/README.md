# Logging.Abstractions

**🌍 Language: English | [Español](README.es.md)**

Core contracts for the DAFce logging infrastructure. Reference these interfaces when building components that emit log entries or when implementing custom log providers — without taking a dependency on any concrete logging implementation.

All types live in `Daf.Extensions.Logging`.

---

## Interfaces

### ILogger

Per-category façade that emits log entries.

```pascal
ILogger = interface(IInvokable)
  function IsEnabled(const Level: TLogLevel): Boolean;

  procedure Log(const Level: TLogLevel; const Msg: string; const Args: TArray<TValue> = nil); overload;
  procedure Log(const Level: TLogLevel; const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;

  procedure LogTrace      (const Msg: string; const Args: TArray<TValue> = nil); overload;
  procedure LogDebug      (const Msg: string; const Args: TArray<TValue> = nil); overload;
  procedure LogInformation(const Msg: string; const Args: TArray<TValue> = nil); overload;
  procedure LogWarning    (const Msg: string; const Args: TArray<TValue> = nil); overload;
  procedure LogError      (const Msg: string; const Args: TArray<TValue> = nil); overload;
  procedure LogError      (const Ex: Exception; const Msg: string = ''; const Args: TArray<TValue> = nil); overload;
  procedure LogCritical   (const Msg: string; const Args: TArray<TValue> = nil); overload;
  procedure LogCritical   (const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;

  function BeginScope(const Msg: string; const Args: TArray<TValue>;
                      const ScopedProc: TProc = nil): ILogScopeVoid; overload;
end;
```

The arguments travel as `TArray<TValue>`, so `['a', 1, True]` keeps each value typed instead of
flattening into a `TVarRec`. Every one of these has a further overload taking a `TEventId` first.

`IsEnabled(Level)` answers whether an entry of that level would reach any destination. Ask it before
building arguments that cost more than the line itself — a state dump, a query, a walk — because in
Delphi the arguments are evaluated at the call, so `LogDebug(Dump)` pays for `Dump` even with Debug
off:

```pascal
if Log.IsEnabled(TLogLevel.Debug) then
  Log.LogDebug('state = {State}', [ExpensiveDump]);
```

A composite answers yes if **any** destination would write; a destination that cannot say answers yes,
because dropping a line by not knowing is worse than composing one too many. This is
`Microsoft.Extensions.Logging`'s `ILogger.IsEnabled`.

---

### ILoggerFactory

Creates `ILogger` instances; the logger it hands back writes to every registered provider.

```pascal
ILoggerFactory = interface(IInvokable)
  function CreateLogger(const Category: string): ILogger; overload;
  function CreateLogger(const AClass: TClass): ILogger; overload;
  function CreateLogger(const AType: PTypeInfo): ILogger; overload;
end;
```

`CreateLogger(TMyClass)` takes the category from the class — its unit and its name. Providers are
added through `ILoggingBuilder`, not here.

---

### ILogger&lt;T&gt;

The logger a service asks for by naming itself. It adds no methods to `ILogger`; what it adds is
that its **category is already the category of whoever asked for it**:

```pascal
type
  TPayrollService = class(TInterfacedObject, IPayroll)
  strict private
    FLog: ILogger<TPayrollService>;
  public
    constructor Create(const Log: ILogger<TPayrollService>);
  end;

procedure TPayrollService.Run;
begin
  FLog.LogInformation('the payroll ran');   // comes out under Payroll.TPayrollService
end;
```

Nothing is registered for that service, and nothing is registered for the next one either.
`AddLogging` puts in **one** registration, with its type argument left open, that serves every
closure — the same shape .NET writes as `services.AddSingleton(typeof(ILogger<>), typeof(Logger<>))`.
See [registering a generic contract once](../DependencyInjection.Abstractions/README.md#registering-a-generic-contract-once)
for what that is and what it costs.

What it buys is that the category cannot drift. Written by hand it appears **twice** — the name of
the class, and the string that names it — with nothing checking that the two agree; and when the
class is renamed, the string stays behind. Here it is derived from the class, so renaming carries
it along and the composition is not touched.

The lifetime is per closed type: `ILogger<TPayroll>` and `ILogger<TInvoicing>` are two loggers, and
asking twice for either one gives the same one back.

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
{$SCOPEDENUMS ON}
TLogLevel = (Trace, Debug, Information, Warning, Error, Critical, None);
```

Scoped, so it is written `TLogLevel.Debug`. Text goes in and comes out through a helper, which is
what a level read from a configuration file needs:

```pascal
S      := Level.ToString;
Level  := TLogLevel.Parse('Debug');                     // raises: Invalid log level: <text>
Level  := TLogLevel.Parse(Raw, TLogLevel.Information);  // or a fallback, and no exception
Levels := TLogLevel.ParseList('Debug,Error');           // comma separated
if TLogLevel.TryParse(Raw, Level) then ...
```

⚠️ `ParseList` **drops silently** what it cannot read: a list of three with one typo comes back with
two levels and no error.

### TLogState

The message an entry carries: the template as written, its arguments, and — when the template names
them — the pairs it yields.

| Member | What it gives |
|---|---|
| `Template` | the template as written, placeholders included |
| `Arguments` | `TArray<TValue>`, in the order they were passed |
| `KeyValuePairs` | `TArray<TPair<string, TValue>>`; empty when the template names nothing |
| `TryGetValue(Key, out Value)` | the value of one named placeholder |
| `Formatter` | the `TLogFormatter` that turns the state into a line |

`LogInformation('user {User} ordered {Count}', [Name, N])` yields two pairs, and `TryGetValue` is how
a layout resolves `{User}` — it is what NNLog's renderer calls. A template that names nothing yields
no pairs, and `Arguments` still holds them in order, which is what a `'%s'` template uses.

### TLogEntry

Single log entry delivered to a provider's `Write` method.

```pascal
TLogEntry = record
  Level     : TLogLevel;
  Category  : string;
  EventId   : TEventId;
  Exception : Exception;
  Scope     : string;
  State     : TLogState;
  Formatter : TLogFormatter;
end;
```

`Message` is a **read-only property**, not a field: the first read runs `Formatter` over `State` and
keeps the result. A provider that writes its own layout out of `State` never pays for it.

`Message` does not carry the scope. `Scope` holds the open scopes from the innermost to the
outermost, each prefixed with ` | `, and is empty outside any scope; a provider that wants them
writes them from there.

---

## Bootstrap channel

An `ILogger` available from `initialization`, with no dependency injection and no configuration. It
covers the stretch before `BuildServiceProvider`, where nothing that was registered can report a
failure yet — and only that stretch: no per-category levels, no configurable layouts, no
asynchronous writing. It is **not** configured from a file, deliberately: reading configuration to
find out where to report that reading the configuration failed is the same chicken and the same
egg.

When to reach for it, what each deployment shape gets, and the handover to the pipeline are in
[Startup composition](../DependencyInjection/docs/STARTUP-COMPOSITION.md).

### IBootstrapSink

A destination of the channel. It receives a **line already formed** — never a `TLogEntry` — so it
cannot invent its own format nor filter: it sees nothing that lets it.

```pascal
IBootstrapSink = interface(IInvokable)
  function IsAvailable: Boolean;
  procedure Write(const Line: string);
end;
```

A destination that cannot write **declares itself unavailable**; it never takes a line it will
lose in silence. `IsAvailable` is answered here and now, not once at registration: the error
handle of a process without a console, or a path that stopped being writable, change the answer.

### BootstrapLog

The channel itself: the registry of destinations and the dispatch. **It never raises** — a channel
whose reason to exist is reporting a failure cannot cause a second one.

```pascal
BootstrapLog = record
  class procedure AddSink(const Sink: IBootstrapSink); static;
  class procedure RemoveSink(const Sink: IBootstrapSink); static;
  class procedure ClearSinks; static;
  class function Sinks: TArray<IBootstrapSink>; static;
  class function AvailableSinks: TArray<IBootstrapSink>; static;
  class function HasAvailableSink: Boolean; static;
  class procedure Write(const Line: string); static;
  class function FormatLine(const Entry: TLogEntry): string; static;
  class function LoggerFor(const Category: string): ILogger; static;
end;
```

| Member | Answers |
|--------|---------|
| `AddSink` `RemoveSink` `ClearSinks` | Destinations are added and removed from `initialization` or from the main, before the host is built. Adding is additive: DAF's own reports keep coming out through theirs |
| `Sinks` | Every registered destination |
| `AvailableSinks` `HasAvailableSink` | The ones that can write **right now**. Empty is the worst case — nobody is going to learn anything — and it has to be distinguishable from having reported |
| `Write` | Dispatches a line already formed to the available destinations |
| `FormatLine` | The layout of the channel, applied before dispatching so every destination writes the same thing |
| `LoggerFor` | A startup `ILogger` under the **caller's** category: an application reports its own startup without disguising itself as DAF. It is the only way in, and DAF's own reports go through it too |

The channel keeps a `Warning`-and-above threshold.

### The destinations that live here

| Class | Writes to | Registered by default |
|-------|-----------|-----------------------|
| `TDebugOutputBootstrapSink` | the system debug output — on Windows, `OutputDebugString`, which belongs to the system and not to the debugger, so it is captured with none attached | yes |
| `TStdErrBootstrapSink` | the process's error output. An application with no console has no error handle, and the destination declares itself unavailable there; it does not raise on write | yes |
| `TFileBootstrapSink` | the file named in its constructor, readable back from its `FileName` property | **no** — a durable path is known by the application, not by the framework |

`TPlatformLogBootstrapSink`, over `IFMXLoggingService`, lives in the `Daf.FMX` package so that
FireMonkey reaches only whoever references it — see [Daf.FMX](../FMX/README.md).

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
