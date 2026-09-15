# NNLog

**🌍 Language: English | [Español](README.es.md)**

Rule-based logging provider — a port of NLog ("NNLog is Not NLog"). It connects to the
`ILoggerFactory` pipeline through `ILoggerProvider` and routes log entries to one or more
**targets** (console, file, UDP, debug output) according to configurable **rules**.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Quick Start

Targets and rules come from a configuration section:

```json
{
  "Logging": {
    "NLog": {
      "targets": {
        "console": {
          "targetType": "Console",
          "layout": "${timestamp} [${level}] ${category} | ${message}",
          "UseColors": "true"
        }
      },
      "rules": [
        { "name": "*", "writeTo": "console", "minLevel": "Debug" }
      ]
    }
  }
}
```

`TNNLogLoader` reads the section and builds the provider:

```pascal
uses
  Daf.Cli,
  Daf.Configuration.CommandLine,
  Daf.Extensions.Configuration,
  Daf.Extensions.Logging,
  Daf.NNLog.Loader;

var B := TCliApplication.CreateBuilder(CommandLineConfig.ProcessArgs);
B.Logging.AddProvider(TNNLogLoader.BuildProvider(B.Environment, B.Configuration.GetSection('Logging:NLog')));
```

`BuildProvider` also takes a `TNNLogConfiguration` assembled in code, for settings that do not come
from a file.

---

## Layout tokens

| Token | Description |
|-------|-------------|
| `${timestamp}` | render time, as `yyyy-MM-dd HH:mm:ss.zzz` |
| `${date:format}` | render time, formatted |
| `${pid}` | process id |
| `${thread}` | thread id |
| `${level}` | level name: `TRACE`, `DEBUG`, `INFO`, `WARN`, `ERROR`, `CRIT` |
| `${category}` | logger category |
| `${scope}` | the scopes open around the entry |
| `${message}` | rendered message |
| `${exception}` | exception class + message, empty when there is none |
| `${env:NAME}` | environment variable |
| `${event-properties:key}` | a value from the entry's state |
| `${binPath}`, `${contentRootPath}`, `${environment}`, `${ApplicationName}` | from `IHostEnvironment` |

Default layout: `${timestamp} #${pid}:${thread} [${level}] ${category} | ${message} ${exception}`

The full table, with the arguments each token takes, is in the [guide](docs/GUIDE.md#layout-tokens).

---

## Rules

A rule selects which loggers it applies to by **category**, through its `name`:

- `*` — and an **absent or empty** `name` — matches **every** category.
- Any other value is a **regular expression**, anchored to the **whole** category: the pattern must
  match the category in full, so `Daf` names one category, not every category that contains it.
- The pattern decides case-sensitivity itself, with an inline `(?i)`; by default it is case-sensitive.

This is a **divergence from NLog**, where `name` is a glob of two wildcards (`*` and `?`). `Daf.*`
keeps working, because `.*` and `*` coincide there; a `?` wildcard is written as `.`, and a literal
dot as `\.`.

A `name` that is not a valid regular expression is a **composition error**: the configuration fails
to load, naming the rule and the pattern.

A rule is **active unless its configuration says otherwise**: a rule that does not declare `enabled`
is enabled. `enabled` takes the literals `true` and `false`, in any case — the values NLog
documents; any other value disables the rule.

Which levels a rule accepts is the conjunction of `minLevel`, `maxLevel` and `levels`. Absent, they
default to `Trace`, `Critical` and no list, so a rule with a `name` and a `writeTo` accepts
everything.

---

## Built-in targets

| `targetType` | Class | Output |
|--------------|-------|--------|
| `Console` | `TConsoleTarget` | standard output, optionally coloured by level |
| `File` | `TFileTarget` | a log file, with rotation by date, age, month or size |
| `Debug` | `TDebugTarget` | the system debug output, on Windows |
| `UDP` | `TUDPTarget` | a UDP endpoint |

`targetType` resolves by RTTI to `Daf.NNLog.Targets.<Type>.T<Type>Target`, which is also how a
custom target is reached from configuration. Every other key in a target's section is set on the
target as a writable property of that name.

A custom target overrides `DoWrite`, not `Write`: `TTarget.Write` takes a lock held by that target
instance and then calls it, so writes from several threads through one target never overlap. The
guarantee is within one process — two processes writing to the same log file are not covered.

The type has to survive smart linking for the lookup to find it: `{$STRONGLINKTYPES ON}` in the
`.dpr`, and each target unit it configures named in its `uses`. A `targetType` whose unit is not
linked reports `Unknown target type`.

---

## Documentation

- 📖 [Usage Guide](docs/GUIDE.md) — composition, configuration, rules, targets, layout, custom targets
