# Hosting — Usage Guide

**🌍 Language: English | [Español](GUIDE.es.md)**

---

## Contents

1. [Architecture Overview](#architecture-overview)
2. [Building a Host](#building-a-host)
3. [Configuration Pipeline](#configuration-pipeline)
4. [Sharing data between startup pieces](#sharing-data-between-startup-pieces)
5. [Registering Services](#registering-services)
6. [Hosted Services (Background Workers)](#hosted-services-background-workers)
7. [Startup Tasks](#startup-tasks)
8. [Environment](#environment)
9. [Application Lifetime](#application-lifetime)
10. [Accessing Services After Build](#accessing-services-after-build)
11. [Graceful Shutdown](#graceful-shutdown)
12. [AppModule Pattern with Hosting](#appmodule-pattern-with-hosting)

---

## Architecture Overview

```
THost.CreateBuilder ──► IGenericHostBuilder
  ├── Configuration: IConfigurationManager   ← sources, and reading them back
  ├── Environment: IHostEnvironment          ← already resolved, derived from host config
  ├── Services: IServiceCollection           ← DI registrations
  ├── Logging: ILoggingBuilder               ← only composed if you touch it
  ├── Properties                             ← data the startup pieces share
  └── Build ──► IHost
                 ├── Services: IServiceProvider
                 ├── IHostEnvironment
                 ├── IHostApplicationLifetime
                 └── [IHostedService, ...]
```

The host is built once, started once, and shut down once. All services are managed by the DI container.

---

## Building a Host

```pascal
uses Daf.Hosting, Daf.Extensions.Hosting, Daf.Extensions.DependencyInjection;

var Builder := THost.CreateBuilder;

Builder.Services.AddSingleton<IMyService, TMyService>;

var Host := Builder.Build;
Host.Start;
Host.WaitForShutdown; // blocks until shutdown signal
```

`THost.CreateBuilder` hands back an `IGenericHostBuilder`, so a single variable owns the builder
and reaches `Build`. Each builder is independent: several builders and several hosts can be alive
at once in one process.  
`Build` can only be called **once** per builder; it freezes configuration and creates the DI container.

---

## Configuration Pipeline

There is **one** configuration, and it is already populated when the builder hands itself back.
`Builder.Configuration` is an `IConfigurationManager` — a builder and a reader at the same time —
so a source can be chosen from what is already configured.

### 1. What the builder seeds

Before returning, the builder loads the sources `Host.CreateApplicationBuilder` loads, in this
order, each one winning over the ones before it:

1. the process environment variables prefixed `DAF_`, with the prefix stripped — `DAF_ENVIRONMENT`
   sets `environment` — and, if it was given arguments, the command line;
2. `appsettings.json`, `appsettings.{Environment}.json`, `{ApplicationName}.settings.json` and
   `{ApplicationName}.settings.{Environment}.json` from the content root, all optional; a path
   separator in the application name becomes `_`;
3. every environment variable, with `__` as the section separator: `Db__Server` is `Db:Server`;
4. the command line, again.

```pascal
var Builder := THost.CreateBuilder(CommandLineConfig.ProcessArgs);
```

Those first sources are where the environment comes from, through the keys .NET uses and
`THostDefaults` names: `environment`, `applicationName` and `contentRoot`. So `--environment Staging`
and `DAF_ENVIRONMENT=Staging` say the same thing, and the argument wins.

The content root is the current directory, as in .NET, unless `contentRoot` names another one; a
relative one is resolved against the folder of the executable. The exception is the system
directory, which is where a Windows service starts: there the content root is the folder of the
executable. `Environment.BinPath` is always that folder, whatever the working directory.

The files are read once, when the builder is created: a change to them is not picked up while the
process runs.

### 2. The environment, already resolved

`Builder.Environment` is derived from that seed inside the constructor. That is what lets the next
step depend on it: there is no phase to wait for.

### 3. Your own sources

Add them where you stand, between the builder and `Build`. They come after the defaults, so they
win over them, the command line included, as in .NET:

```pascal
JsonConfig.AddFile(Builder.Configuration,
  TPath.Combine(Builder.Environment.ContentRootPath, 'features.json'), [csoOptional]);
```

And because the manager reads as well as builds, a registration consumes configuration on the spot:

```pascal
var Db := Builder.Configuration['Database:ConnectionString'];
Builder.Services.AddSingleton<IDatabase>(TDatabase.Create(Db));
```

That is what the deferred callbacks existed for, and why there are none: a configuration that can
be read while it is being composed leaves nothing to defer.

`Build` freezes it. From there on the same configuration is what `IHostBuilderContext.Configuration`
hands out.

---

## Sharing data between startup pieces

`Properties` is a `TDictionary<string, TValue>` the builder carries, for the data that the pieces
composing startup need to pass to each other. It is on `IHostApplicationBuilder`, and the same
dictionary is read from the other side through `IHostBuilderContext.Properties`, which is
registered in the container.

```pascal
Builder.Properties.AddOrSetValue('tenant', 'acme');

// ...anywhere holding the context:
var Tenant := Context.Properties['tenant'].AsString;
```

---

## Registering Services

The full `IServiceCollection` API is available on `Builder.Services`:

```pascal
var Services := Builder.Services;

// Lifetimes
Services.AddSingleton<ICache, TMemoryCache>;
Services.AddScoped<IUnitOfWork, TDbUnitOfWork>;
Services.AddTransient<IEmailSender, TSmtpSender>;

// Hosted service (started/stopped with the host)
Services.AddHostedService<TBackgroundIndexer>;
```

See [DependencyInjection](../../DependencyInjection/docs/GUIDE.md) for the full registration reference.

---

## Hosted Services (Background Workers)

Implement `IHostedService` for any work that should run alongside the host:

```pascal
type
  TOrderProcessor = class(TInterfacedObject, IHostedService)
  private
    FTimer: TTimer; // or use TThread, etc.
  public
    procedure Start;
    procedure Stop;
  end;

procedure TOrderProcessor.Start;
begin
  // set up periodic work, open connections, etc.
end;

procedure TOrderProcessor.Stop;
begin
  // flush, close, clean up
end;
```

Register:
```pascal
Services.AddHostedService<TOrderProcessor>;
```

- `Start` is called for each hosted service when `Host.Start` runs, **in registration order**.
- `Stop` is called in the **reverse** order during shutdown — the order the container releases in —,
  so a service stops before the ones registered ahead of it, which it may be using.
- `AddHostedService<T>` registers `T` under `IHostedService` and nothing else. To reach the same
  instance as `IOrderProcessor`, register that too and have the hosted-service registration
  resolve it.

---

## Startup Tasks

An `IStartupTask` runs **once** at startup, with the provider already built. It has a `Run` and
nothing to stop: it is initialization, not a service with a lifetime.

```pascal
type
  TWarmUpCache = class(TInterfacedObject, IStartupTask)
  public
    procedure Run;
  end;

procedure TWarmUpCache.Run;
begin
  // read what the application needs before it serves anything
end;
```

Register it on the collection:

```pascal
Services.AddStartupTask<TWarmUpCache>;
```

`THost.Start` runs the tasks **in registration order, before every `IHostedService` and before
`ApplicationStarted`** — a hosted service may depend on the state a task prepares, so the order is
the point and not an accident.

**A task that raises aborts the startup.** The exception is reported and re-raised, so no hosted
service starts and the application is never declared started. That is what makes a startup task the
place for a precondition: a cache that cannot be filled, a folder that is not writable, a schema
that is not the expected one.

### When the helper is shadowed

Only one `record helper` per type is visible in Delphi, so a unit that brings a second helper for
`IServiceCollection` hides this one. The class facade is the way through, and it takes the
implementation as a metaclass:

```pascal
Hosting.AddStartupTask(Services, TWarmUpCache);
```

It is the same registration: a package contributes its own initialization with it without the
`.dpr` naming the task — see [PackageInit](../../Cli/samples/PackageInit/README.md).

Choosing between an `IStartupTask` and an `IHostedService` is decided in
[Startup composition](../../DependencyInjection/docs/STARTUP-COMPOSITION.md).

---

## Environment

The `IHostEnvironment` is pre-registered as a Singleton. Access it anywhere via the DI container:

```pascal
var Env := Host.Services.GetRequiredService<IHostEnvironment>;

Env.ApplicationName   // e.g. 'MyApp'
Env.EnvironmentName   // e.g. 'Development'
Env.ContentRootPath   // e.g. 'C:\MyApp\'
Env.BinPath           // always the binary directory

Env.IsDevelopment   // True in Development
Env.IsProduction    // True in Production
Env.IsTesting       // True in Testing (useful for test runners)
```

Set environment via OS variable before launching:
```
DAF_ENVIRONMENT=Development
```

Accepted shorthands: `deve`, `stag`, `prod`, `test` (case-insensitive).

In `Development` the host also validates the container when it builds it: a registration whose
dependencies cannot be satisfied, or a Singleton that captures a Scoped, stops `Build` with every
finding listed, and a Scoped asked of the root provider is rejected when it is resolved. `ServiceProviderOptions` on the builder fixes that choice whatever the environment —
see *Container validation* in the module README.

You can also read arbitrary environment variables:
```pascal
var DbHost := Env['MY_DB_HOST'];
```

---

## Application Lifetime

`IHostApplicationLifetime` publishes three cancellation tokens you can react to:

```pascal
var Lifetime := Services.GetRequiredService<IHostApplicationLifetime>;

// Fired after all hosted services have started
Lifetime.ApplicationStarted.Register(procedure begin
  Logger.LogInformation('Application ready');
end);

// Fired when a shutdown has been requested (before services stop)
Lifetime.ApplicationStopping.Register(procedure begin
  Logger.LogInformation('Shutdown initiated');
end);

// Fired after all hosted services have stopped
Lifetime.ApplicationStopped.Register(procedure begin
  Logger.LogInformation('Application stopped');
end);

// Request a graceful shutdown programmatically
Lifetime.StopApplication;
```

`StopApplication` triggers the same shutdown sequence as `Ctrl+C` / `SIGTERM`.

---

## Accessing Services After Build

```pascal
var Host := Builder.Build;
Host.Start;

// Access services from outside the DI graph
var Svc := Host.Services.GetRequiredService<IMyService>;
Svc.Execute;

Host.WaitForShutdown;
```

For scoped operations (e.g., a background job that needs a unit of work):

```pascal
var Scope := Host.Services.CreateScope;
try
  var UoW := Scope.ServiceProvider.GetRequiredService<IUnitOfWork>;
  UoW.DoWork;
  UoW.Commit;
finally
  Scope := nil;
end;
```

---

## Graceful Shutdown

`Host.WaitForShutdown` blocks until the process receives a shutdown signal (`Ctrl+C`, `SIGTERM`, or `Lifetime.StopApplication`). The shutdown sequence is:

1. `IHostApplicationLifetime.ApplicationStopping` callbacks fire
2. `IHostedService.Stop` called for each service, in reverse registration order
3. The DI container releases each Singleton once nothing but itself holds it, so a service goes
   after everything that depends on it
4. `IHostApplicationLifetime.ApplicationStopped` callbacks fire

`WaitForShutdown` integrates with the OS signal handler via `TShutdownHook`.

---

## AppModule Pattern with Hosting

Split registrations across modules for large apps:

```pascal
uses Daf.AppModule, Daf.Extensions.DependencyInjection;

type
  TInfraModule = class(TInterfacedObject, IAppModule)
  public
    procedure AddServices(const Services: IServiceCollection);
  end;

procedure TInfraModule.AddServices(const Services: IServiceCollection);
begin
  Services.AddSingleton<IDatabase, TPostgresDatabase>;
  Services.AddHostedService<TBackgroundWorker>;
end;
```

Load the modules on `Builder.Services`, one `AddModule<T>` per module:

```pascal
Builder.Services
  .AddModule<TInfraModule>
  .AddModule<TDomainModule>;
```

`AddModule<T>` instantiates the module, calls its `AddServices` on the spot, and owns its
lifetime. Do not call `TInfraModule.Create.AddServices(Services)` by hand: the module is an
`TInterfacedObject` created with a reference count of zero, so nobody frees it.

When a module needs configuration, construct it yourself and pass the instance:
`Services.AddModule(TInfraModule.Create(...))`. Handing it over as an `IAppModule` is what
gives it an owner, and it is the only way to pass a module any configuration.

**There is no automatic discovery.** `AddModules(Package)`, the RTTI scan for `IAppModule`, was
removed: it hid the declaration in a `uses` clause, left startup silently blind to a trimmed
RTTI, and put the registration order in the hands of reflection. No variant of the application
family registers a module on its own either — its modules are declared wherever the services are
composed, exactly as above. See [Startup composition](../../DependencyInjection/docs/STARTUP-COMPOSITION.md) for
the two phases and the ordering rules.
