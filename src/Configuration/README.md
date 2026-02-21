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
| Chained | `Daf.Configuration.Chained` | Wrap an existing `IConfiguration` as a source |

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
