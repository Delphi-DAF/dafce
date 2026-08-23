# Configuration

**🌍 Language: English | [Español](README.es.md)**

Layered configuration system for Delphi inspired by .NET's `Microsoft.Extensions.Configuration`. Read settings from JSON files, INI files, environment variables, in-memory dictionaries or chained sources — all unified behind a single `IConfiguration` interface.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Why use this?

- 📄 **Multiple sources** — JSON, INI, environment variables, in-memory, or custom
- 🔗 **Layered & overridable** — sources are chained; later sources override earlier ones
- 🗂️ **Hierarchical keys** — use `:` as separator (`Database:ConnectionString`)
- 🔄 **Reload support** — `IConfigurationRoot.Reload` re-reads all providers
- 🎯 **Binder** — map a configuration section directly to a Delphi class

---

## Quick Start

```pascal
uses
  Daf.Configuration.Builder,
  Daf.Configuration.Json,
  Daf.Configuration.Env,
  Daf.Extensions.Configuration;

var Config := TConfigurationBuilder.Create
  .AddJsonFile('appsettings.json')
  .AddEnvironmentVariables
  .Build;

// Read a value
var ConnStr := Config['Database:ConnectionString'];

// Read a section
var DbSection := Config.GetSection('Database');
var Host := DbSection['Host'];
```

---

## Supported Providers

| Provider | Unit | Description |
|----------|------|-------------|
| JSON file | `Daf.Configuration.Json` | `appsettings.json`, optional/required |
| INI file | `Daf.Configuration.Ini` | Classic `[Section]\nKey=Value` format |
| Environment variables | `Daf.Configuration.Env` | OS env vars, optionally filtered by prefix |
| In-memory | `Daf.Configuration.Memory` | Dictionary of `string → string` pairs |
| Command line | `Daf.Configuration.CommandLine` | Process arguments, with .NET's syntax and short aliases |
| Chained | `Daf.Configuration.Chained` | Wrap an existing `IConfiguration` as a source |

The first four are the .NET set: `Microsoft.Extensions.Configuration`'s `Json`, `Ini`,
`EnvironmentVariables` and `CommandLine`. **Precedence is the order in which you add them — the
last source wins.** No provider claims a precedence of its own.

---

## The command line

The highest-precedence source, and the one that needs no file: it lets you override any setting when
invoking the executable.

```pascal
uses
  Daf.Configuration.Builder,
  Daf.Configuration.Json,
  Daf.Configuration.CommandLine,
  Daf.Extensions.Configuration;

var Builder: IConfigurationBuilder := TConfigurationBuilder.Create;

JsonConfig.AddFile(Builder, 'appsettings.json');
CommandLineConfig.AddArgs(Builder);   // added last, so it overrides the file

var Config := Builder.Build;
```

`AddArgs` with no arguments reads the ones this process was invoked with. The overload taking an
explicit array is what makes the source testable — a process cannot fake its own `ParamStr`:

```pascal
CommandLineConfig.AddArgs(Builder, ['--Logging:LogLevel:Default=Debug']);
```

### Accepted forms

| Form | Example |
|---|---|
| `key=value` | `Logging:LogLevel:Default=Debug` |
| `--key=value` | `--Logging:LogLevel:Default=Debug` |
| `--key value` | `--Logging:LogLevel:Default Debug` |
| `/key=value` | `/Logging:LogLevel:Default=Debug` |
| `/key value` | `/Logging:LogLevel:Default Debug` |
| `-alias value` | only when the alias is declared |

Keys are case-insensitive, a repeated key keeps its last value, and `key=` with nothing after the
equals sign is a key whose value is the empty string. Nested keys use the same separator as every
other source, so a value set here and a value read from `appsettings.json` land on the same key.

### Short aliases

An alias maps a short switch onto a long key. It must start with one or two dashes, and declaring
the same alias twice is a composition error — reported when the source is assembled, not when the
process runs.

```pascal
CommandLineConfig.AddArgs(Builder, ['-v', 'Debug'],
  [TSwitchMapping.Create('-v', 'Logging:LogLevel:Default')]);
```

⚠️ **One deliberate difference from .NET.** There, a single-dash switch with no alias declared and an
equals sign — `-x=1` — raises a `FormatException`. Here it is ignored, like every other argument that
names no key: reading the command line should not be able to stop an application from starting
because of one extra dash.

### Not to be confused with `Daf.CmdLn`

Writing a command-line *application* — commands, subcommands, typed options, generated help — is a
different job, and it belongs to [`Daf.CmdLn`](../CmdLn/README.md). The two share no engine, the same
way `Microsoft.Extensions.Configuration.CommandLine` does not use `System.CommandLine`.

---

## Binding to Objects

Use `TConfigurationBinder` to populate a Delphi object from a configuration section:

```pascal
uses Daf.Configuration.Binder;

type
  TDatabaseOptions = class
  public
    Host: string;
    Port: Integer;
    Name: string;
  end;

var Opts := TDatabaseOptions.Create;
TConfigurationBinder.Bind(Config.GetSection('Database'), Opts);
// Opts.Host, Opts.Port, Opts.Name are now populated
```

---

## Integration with Hosting

When using `Hosting`, configure sources in `ConfigureAppConfiguration`:

```pascal
THostBuilder.Create
  .ConfigureAppConfiguration(procedure(Ctx: IHostBuilderContext;
                                       Builder: IConfigurationBuilder)
  begin
    Builder
      .AddJsonFile('appsettings.json')
      .AddJsonFile('appsettings.' + string(Ctx.Environment.EnvironmentName) + '.json', True)
      .AddEnvironmentVariables;
  end)
  .ConfigureServices(procedure(Ctx: IHostBuilderContext; Services: IServiceCollection)
  begin
    // Ctx.Configuration is fully built here
    Services.AddSingleton<IMyOptions>(TMyOptions.Create(Ctx.Configuration));
  end)
  .Build.Run;
```

---

## Documentation

- 📖 [Usage Guide](docs/GUIDE.md) — providers in depth, hierarchical keys, binder, custom sources
