# Hosting

**🌍 Language: English | [Español](README.es.md)**

Generic host for Delphi applications inspired by .NET's `Microsoft.Extensions.Hosting`. Composes services, configuration, hosted services (background workers) and the application lifetime into a single, managed runtime.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Why use this?

- 🏗️ **Unified startup** — One place for DI, configuration, logging and background services
- 🌍 **Environment-aware** — Built-in `Development` / `Staging` / `Production` / `Testing` modes
- ⚙️ **Layered configuration** — Host config, then app config, with environment variable overrides
- 🔄 **Hosted services** — Register `IHostedService` implementations that start and stop with the host
- 🛑 **Graceful shutdown** — `IHostApplicationLifetime` signals `Started`, `Stopping`, `Stopped`
- 🔌 **DI integration** — `IServiceProvider` is the backbone; all services registered via `IServiceCollection`

---

## Quick Start

```pascal
uses
  Daf.Hosting,
  Daf.Extensions.Hosting,
  Daf.Extensions.DependencyInjection;

var Host := THostBuilder.Create
  .ConfigureServices(procedure(Context: IHostBuilderContext;
                               Services: IServiceCollection)
  begin
    Services.AddSingleton<IMyService, TMyService>;
    Services.AddHostedService<TMyWorker>; // IHostedService
  end)
  .Build;

Host.Start;
Host.WaitForShutdown;
```

---

## IHostedService

Any class registered as a hosted service must implement `IHostedService`:

```pascal
type
  TMyWorker = class(TInterfacedObject, IHostedService)
  public
    procedure Start;
    procedure Stop;
  end;

procedure TMyWorker.Start;
begin
  // launched when Host.Start is called
end;

procedure TMyWorker.Stop;
begin
  // called on graceful shutdown
end;
```

Register it:
```pascal
Services.AddHostedService<TMyWorker>;
// equivalent to:
Services.AddSingleton<IHostedService, TMyWorker>;
```

---

## Environment

The environment is read from the `DAF_APP_ENV` variable (or defaults to `Production`):

| Value | IsDevelopment | IsStaging | IsProduction | IsTesting |
|-------|:---:|:---:|:---:|:---:|
| `Development` / `deve` | ✅ | | | |
| `Staging` / `stag` | | ✅ | | |
| `Production` / `prod` | | | ✅ | |
| `Testing` / `test` | | | | ✅ |

```pascal
var Env := Host.Services.GetRequiredService<IHostEnvironment>;
if Env.IsDevelopment then
  WriteLn('Running in development mode');
WriteLn(Env.ContentRootPath);
```

Environment variables read at startup:

| Variable | Default | Purpose |
|----------|---------|---------|
| `DAF_APP_ENV` | `Production` | Environment name |
| `DAF_APP_NAME` | Executable name | Application name |
| `DAF_CONTENT_ROOT` | App binary path | Content root directory |

---

## Application Lifetime

```pascal
var Lifetime := Host.Services.GetRequiredService<IHostApplicationLifetime>;

Lifetime.ApplicationStarted.Register(procedure begin
  WriteLn('App started');
end);

Lifetime.ApplicationStopping.Register(procedure begin
  WriteLn('Stopping...');
end);

// Request a graceful stop from anywhere
Lifetime.StopApplication;
```

---

## Using TDafApplication (Application module)

For console apps, the `Application` module provides a higher-level wrapper:

```pascal
uses DAF.Application.Builder;

TDafApplication.CreateHostBuilder
  .ConfigureServices(procedure(Context: IHostBuilderContext;
                               Services: IServiceCollection)
  begin
    Services.AddSingleton<IMyService, TMyService>;
    Services.AddHostedService<TMyWorker>;
  end)
  .Build
  .Run;
```

See [Application](../Application/README.md) for the full Application module docs.

---

## Documentation

- 📖 [Usage Guide](docs/GUIDE.md) — configuration pipeline, scoped services, hosted services, lifetime hooks
