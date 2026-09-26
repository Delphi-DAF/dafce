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
| `IHostApplicationBuilder` | The shared contract of the application family: `Services`, `Configuration`, `Environment`, `Logging`, `Properties`, `ServiceProviderOptions` |
| `IStartupTask` | Runs once at startup with the provider already built: `Run`, and nothing to stop |
| `IHost` | The running host: `Services`, `Start`, `Stop`, `WaitForShutdown` |
| `IHostedService` | Background service contract: `Start` / `Stop` |
| `IHostEnvironment` | Runtime environment: name, app name, paths, `IsDevelopment`, ... |
| `IHostBuilderContext` | Startup context, registered in the container: `Environment`, `Configuration`, `Properties` |
| `IHostApplicationLifetime` | Lifetime signals: `ApplicationStarted`, `ApplicationStopping`, `ApplicationStopped`, `StopApplication` |

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

`IServiceCollectionHelper` adds `AddHostedService<T>` and `AddStartupTask<T>` directly on
`IServiceCollection`:

```pascal
Services.AddHostedService<TMyWorker>;   // = AddSingleton<IHostedService, TMyWorker>
Services.AddStartupTask<TMySeeder>;     // = AddSingleton<IStartupTask, TMySeeder>
```

Only one `record helper` per type is in scope in Delphi, so a unit bringing a second one for
`IServiceCollection` hides this one. The `Hosting` class facade is the way through, and it takes the
implementation as a metaclass: `Hosting.AddHostedService(Services, TMyWorker)`.

---

## Dependencies

- `Daf.Extensions.DependencyInjection` (DependencyInjection.Abstractions)
- `Daf.Extensions.Configuration` (Configuration.Abstractions)
- `Daf.Extensions.Logging` (Logging.Abstractions) — for the `ILoggingBuilder` the family contract
  exposes as a property
- `Daf.Commons`

The implementation is in [`Hosting`](../Hosting/README.md).
