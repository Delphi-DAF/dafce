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

var Builder: IConfigurationBuilder := TConfigurationBuilder.Create;

JsonConfig.AddFile(Builder, 'appsettings.json');
Builder.Add(TEnvironmentVariablesConfigurationSource.Create);

var Config := Builder.Build;

// Read a value
var ConnStr := Config['Database:ConnectionString'];

// Read a section
var DbSection := Config.GetSection('Database');
var Host := DbSection['Host'];

// Tell an absent key from one whose value is empty -- the indexer gives '' for both
if Config.Exists('Database:Password') then ;          // presence, and nothing else
var Read := Config.Find('Database:Password');         // presence AND value, one lookup
```

---

## Absent is not empty

Two members that .NET does not have, because **there the distinction is carried by nullability**:
the indexer is `string? this[key]`, `null` for absent and `''` for present-but-empty. A Delphi
string cannot be nil, so what the `?` does there needs a member of its own here — `Exists` for the
presence, `Find` for presence and value in a single lookup.

The indexer returns a `string`, so `C['absent'] = ''` and `C['empty'] = ''` are both `True`.
[The guide](docs/GUIDE.md#telling-an-absent-key-from-an-empty-one) has the section semantics of
`Exists`, and when the distinction bites hardest.


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
names no key.

### Not to be confused with `Daf.CommandLine`

Writing a command-line *application* — commands, subcommands, typed options, generated help — is a
different job, and it belongs to [`Daf.CommandLine`](../CommandLine/README.md). The two share no
engine, the same way `Microsoft.Extensions.Configuration.CommandLine` does not use
`System.CommandLine`.

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

// Builds the object and binds it in one call. `boFields` because this class exposes public
// fields; the default, `boProperties`, reads published properties instead.
var Opts := TConfigurationBinder.Bind<TDatabaseOptions>(Config, 'Database', boFields);
// Opts.Host, Opts.Port, Opts.Name are now populated
```

A key that no source mentions is not written, so a default set in the constructor survives the bind.

---

## Settings with an owner

`TConfigurationBinder` builds the object and hands it over: the caller owns it, and the caller frees
it. That is still true, and it is the right thing for a utility, a script, or a startup step running
before there is a container.

Inside a container it is not. `ConfigOptions.Bind` declares that some settings are filled from a
section, and from then on the container builds them, owns them and frees them:

```pascal
uses Daf.Options.Configuration;

ConfigOptions.Bind<TAppSettings>(Services, Config);                       // the root
ConfigOptions.Bind<TDbSettings>(Services, Config, 'Database', boFields);  // a section

Services.AddSingleton<IGreeter, TGreeter>;
```

```pascal
constructor TGreeter.Create(const Options: IOptions<TAppSettings>);
begin
  inherited Create;
  FTitle := Options.Value.Title;
end;
```

The service **never names its section**: it asks for its settings by constructor and startup decides
where they come from. Changing the section is one line at the composition site and no change at all
in the service.

The contract of `IOptions<T>` — ownership, the dangling-pointer trap, lazy construction, validation,
and what happens with settings nobody registered — is in
[`DependencyInjection.Abstractions`](../DependencyInjection.Abstractions/README.md).

### A photograph, not a mirror

The section is read **once**, when the value is built, and changing the configuration afterwards
does **not** change it. The providers report no change, so what a service holds is the state of the
section at the moment its settings were built.

---

## Integration with Hosting

When using `Hosting`, the builder already loads the `appsettings` files, the application's own
settings files and the environment variables; which ones and in what order is in
[Hosting's guide](../Hosting/docs/GUIDE.md). Add
your own sources to the builder's `Configuration`:

```pascal
var Builder := THost.CreateBuilder;

JsonConfig.AddFile(Builder.Configuration,
  TPath.Combine(Builder.Environment.ContentRootPath, 'features.json'), [csoOptional]);

// The manager reads as well as builds, so the registration consumes it on the spot. The cast
// is because the manager is reader and builder at once, and Delphi allows one parent interface.
Builder.Services.AddSingleton<IMyOptions>(
  TMyOptions.Create(Builder.Configuration as IConfiguration));

var Host := Builder.Build;
Host.Start;
```

---

## Documentation

- 📖 [Usage Guide](docs/GUIDE.md) — providers in depth, hierarchical keys, binder, custom sources
- 📁 [ConfigSample](samples/ConfigSample/README.md) — the provider chain and its precedence, running
