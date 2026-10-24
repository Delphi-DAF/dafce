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

var Builder := THost.CreateBuilder;

Builder.Services.AddSingleton<IMyService, TMyService>;
Builder.Services.AddHostedService<TMyWorker>; // IHostedService

var Host := Builder.Build;
Host.Start;
try
  Host.WaitForShutdown;
finally
  StopAndNil(Host);
end;
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

If a `Stop` raises, the rest of the shutdown still runs: the other services stop and the container
is shut down. `Host.Stop` raises what failed at the end — the exception itself, or an
`EAggregateException` holding every failure when several steps failed.

---

## Environment

The environment is read from the `environment` host setting — `--environment Staging` on the
command line, or the `DAF_ENVIRONMENT` variable — and defaults to `Production`:

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

Host settings read at startup, as an argument or as a variable — the argument wins:

| Key | Variable | Default | Purpose |
|-----|----------|---------|---------|
| `environment` | `DAF_ENVIRONMENT` | `Production` | Environment name |
| `applicationName` | `DAF_APPLICATIONNAME` | Executable name | Application name |
| `contentRoot` | `DAF_CONTENTROOT` | Current directory | Content root directory |

When the current directory is the system one, as for a Windows service, the content root is the
folder of the executable instead. `Env.BinPath` is always that folder.

---

## Container validation

In the `Development` environment the host builds the container with `ValidateOnBuild` and
`ValidateScopes`: a registration whose dependencies cannot be satisfied, or a Singleton that
captures a Scoped, stops `Build` with every finding listed. In any other environment the container
is built without validating.

`ServiceProviderOptions` on the builder reads the options `Build` will use, and assigning it fixes
them whatever the environment:

```pascal
Builder.ServiceProviderOptions := TServiceProviderOptions.None;               // never validate
Builder.ServiceProviderOptions := TServiceProviderOptions.Create(True, True); // always validate
```

The validation reaches the registrations made by class; a factory registration is left out. With
`ValidateScopes`, a Scoped service asked of the root provider is also rejected when it is resolved.
Hosted services and startup tasks are Singletons, so what they need from a Scoped they resolve from
a scope of their own, through an `IServiceScopeFactory` taken by constructor.

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

## Using the application family

For a program with a main line, each variant of the application family — `Daf.Cli`, `Daf.Web`,
`Daf.VCL` or `Daf.FMX` — wraps this pipeline and adds `Run`. The builder is usable the moment it is
created: `Services`,
`Configuration` and `Environment` answer as soon as they are read, so registrations happen where they
are written.
`Build` returns the application, and its `Run` gives the exit code.

```pascal
uses Daf.Cli, Daf.CommandLine, Daf.Configuration.CommandLine;

var B := TCliApplication.CreateBuilder(CommandLineConfig.ProcessArgs);

B.Services.AddSingleton<IMyService, TMyService>;
B.Services.AddHostedService<TMyWorker>;

ExitCode := B.Build.Run;
```

See [The application family](docs/APPLICATION-FAMILY.md) for which entry point is yours and what the layer adds over the host.

---

## Documentation

- 📖 [Usage Guide](docs/GUIDE.md) — configuration pipeline, scoped services, hosted services, lifetime hooks
- 📖 [The application family](docs/APPLICATION-FAMILY.md) — which entry point is yours, what the layer adds over the host, and what it is in .NET terms
- 📖 [Startup composition](../DependencyInjection/docs/STARTUP-COMPOSITION.md) — `IStartupTask` vs `IHostedService`, and the two phases of startup
- 📁 [HostQuickStart](samples/HostQuickStart/README.md) — compose, build, start and stop, with no application variant around it
- 📁 [StartupReport](samples/StartupReport/README.md) — where the startup record goes, and what a startup that does not complete shows
- 📁 [HostLifetime](samples/HostLifetime/README.md) — the three lifetime notifications, and a copy of the `IHost` released in `ApplicationStopped`
- 📁 [ConsoleSample](../Cli/samples/ConsoleSample/README.md) — configuration and the options pattern in one console program
- 📁 [HostedService](../samples/HostedService/) — background workers with `AddHostedService`
- 📁 [PackageInit](../Cli/samples/PackageInit/README.md) — an `IStartupTask` contributed from a package, without the `.dpr` naming it
