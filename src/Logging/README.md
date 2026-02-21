# Logging

**🌍 Language: English | [Español](README.es.md)**

Structured logging infrastructure for Delphi inspired by .NET's `Microsoft.Extensions.Logging`. Write log entries via a unified `ILogger` interface — swappable providers (NNLog, console, custom) registered at startup.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Why use this?

- 🎚️ **Six log levels** — Trace, Debug, Information, Warning, Error, Critical
- 📝 **Structured logging** — Named `{Key}` placeholders in message templates
- 🔌 **Provider model** — Register one or more `ILoggerProvider` implementations; all receive each entry
- 🏷️ **Categories** — Loggers are tagged by class or string name for filtering
- 🔗 **Scopes** — Attach contextual data to a block of log entries with `BeginScope`
- 🛠️ **DI-ready** — `AddLogging(Services, ...)` registers `ILoggerFactory` in the container

---

## Quick Start

```pascal
uses
  Daf.Extensions.Logging,
  Daf.Logging,
  DAF.Logging.Builder,
  DAF.NNLog; // any provider

// Standalone (no DI)
var Factory := TMultiProviderLoggerFactory.Create;
Factory.AddProvider(TNNLogProvider.Create(Targets, Rules));
var Logger := Factory.CreateLogger('MyApp');

Logger.LogInformation('Server started on port {Port}', [8080]);
Logger.LogError(Ex, 'Unhandled exception in {Operation}', ['ProcessOrder']);
```

---

## Log Levels

| Level | Value | When to use |
|-------|------:|-------------|
| `Trace` | 0 | Extremely detailed — disabled in production |
| `Debug` | 1 | Development diagnostics |
| `Information` | 2 | Normal app flow milestones |
| `Warning` | 3 | Unexpected but recoverable situations |
| `Error` | 4 | Failures in the current operation |
| `Critical` | 5 | Requires immediate attention |
| `None` | 6 | Disables logging |

---

## Structured Logging

Use named placeholders in message templates:

```pascal
Logger.LogInformation('User {UserId} logged in from {IP}', [UserId, RemoteIP]);
Logger.LogWarning('Retry {Attempt} of {Max} for {Operation}', [3, 5, 'SendEmail']);
```

The template and arguments are kept together in `TLogState`, letting providers format or index them independently.

---

## Integration with DI and Hosting

```pascal
uses DAF.Logging.Builder, DAF.NNLog;

// In ConfigureServices:
AddLogging(Services, procedure(Builder: ILoggingBuilder)
begin
  Builder.AddProvider(TNNLogProvider.Create(Targets, Rules));
end);

// Resolve in a service:
var Factory := Provider.GetRequiredService<ILoggerFactory>;
var Logger  := Factory.CreateLogger<TMyService>;
```

---

## Documentation

- 📖 [Usage Guide](docs/GUIDE.md) — scopes, structured logging, providers, DI, creating custom providers
