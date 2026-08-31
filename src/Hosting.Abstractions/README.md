# Hosting.Abstractions

**🌍 Language: English | [Español](README.es.md)**

Core contracts for the DAF hosting system. This module defines **interfaces and types only** — no implementation. Reference it when writing hosted services, environment-aware components, or generic host extensions without depending on the concrete host.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

---

## What's in this module

All types live in `Daf.Extensions.Hosting`.

### Core interfaces

| Interface | Role |
|-----------|------|
| `IHostApplicationBuilder` | The shared contract of the application family: `Services`, `Configuration`, `Environment`, `Logging`, `Properties` |
| `IStartupTask` | Runs once at startup with the provider already built: `Run`, and nothing to stop |
| `IHost` | The running host: `Services`, `Start`, `Stop`, `WaitForShutdown` |
| `IHostedService` | Background service contract: `Start` / `Stop` |
| `IHostEnvironment` | Runtime environment: name, app name, paths, `IsDevelopment`, ... |
| `IHostBuilderContext` | Context passed to configuration callbacks: `Environment` + `Configuration` |
| `IHostApplicationLifetime` | Lifetime signals: `ApplicationStarted`, `ApplicationStopping`, `ApplicationStopped`, `StopApplication` |

### Delegate types

```pascal
TConfigureServicesAction   = TProc<IHostBuilderContext, IServiceCollection>;
```

### Environment constants

```pascal
TEnvironments.Development   // 'Development'
TEnvironments.Staging       // 'Staging'
TEnvironments.Production    // 'Production'
TEnvironments.Testing       // 'Testing'
```

### DAF environment variables

```pascal
TDafEnvVars.APP_ENV       // 'DAF_APP_ENV'
TDafEnvVars.APP_NAME      // 'DAF_APP_NAME'
TDafEnvVars.CONTENT_ROOT  // 'DAF_CONTENT_ROOT'
```

### Helper

`IServiceCollectionHelper` adds `AddHostedService<T>` directly on `IServiceCollection`:

```pascal
Services.AddHostedService<TMyWorker>;
// equivalent to Services.AddSingleton<IHostedService, TMyWorker>
```

---

## Dependencies

- `Daf.Extensions.DependencyInjection` (DependencyInjection.Abstractions)
- `Daf.Extensions.Configuration` (Configuration.Abstractions)
- Delphi RTL only

The implementation is in [`Hosting`](../Hosting/README.md).
