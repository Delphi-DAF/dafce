# NNLog

**🌍 Language: English | [Español](README.es.md)**

Rule-based logging provider for DAFce. Connects to the `ILoggerFactory` pipeline via `ILoggerProvider` and routes log entries to one or more **targets** (console, file, …) according to configurable **rules**.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Quick Start

```pascal
uses
  DAF.NNLog,
  DAF.NNLog.Targets.Console,
  DAF.Logging.Builder;

// 1. Define a target
var ConsoleTarget := TNNLogConsoleTarget.Create;
ConsoleTarget.Name   := 'console';
ConsoleTarget.Layout := '${timestamp} [${level}] ${category} | ${message}';

// 2. Define rules
var Rule := TLogRule.Create;
Rule.Name    := '*';             // match all categories
Rule.MinLevel := TLogLevel.Debug;
Rule.WriteTo  := [ConsoleTarget];

// 3. Create the provider and wire it to the factory
var Provider := TNNLogProvider.Create([ConsoleTarget], [Rule]);
Factory.AddProvider(Provider);
```

---

## Layout tokens

| Token | Description |
|-------|-------------|
| `${timestamp}` | ISO-8601 timestamp |
| `${pid}` | Process ID |
| `${thread}` | Thread ID |
| `${level}` | Log level name |
| `${category}` | Logger category |
| `${message}` | Rendered message |
| `${exception}` | Exception class + message (empty if none) |
| `${scope}` | Active scope chain |

Default layout: `${timestamp} #${pid}:${thread} [${level}] ${category} | ${message} ${exception}`

---

## Integration with DI / Hosting

```pascal
uses DAF.Logging.Builder, DAF.NNLog, DAF.NNLog.Targets.Console;

AddLogging(Services,
  procedure(Builder: ILoggingBuilder)
  var
    Target : TNNLogConsoleTarget;
    Rule   : TLogRule;
  begin
    Target := TNNLogConsoleTarget.Create;
    Target.Name := 'console';

    Rule := TLogRule.Create;
    Rule.Name     := '*';
    Rule.MinLevel := TLogLevel.Information;
    Rule.WriteTo  := [Target];

    Builder.AddProvider(TNNLogProvider.Create([Target], [Rule]));
  end);
```

---

## Documentation

- 📖 [Usage Guide](docs/GUIDE.md) — rules, targets, layout, multiple targets, custom targets
