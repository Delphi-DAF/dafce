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
7. [Environment](#environment)
8. [Application Lifetime](#application-lifetime)
9. [Accessing Services After Build](#accessing-services-after-build)
10. [Graceful Shutdown](#graceful-shutdown)
11. [AppModule Pattern with Hosting](#appmodule-pattern-with-hosting)

---

## Architecture Overview

```
THostBuilder
  ├── ConfigureHostConfiguration   ← low-level host config (env vars, etc.)
  ├── ConfigureAppConfiguration    ← app-level config (JSON, INI, env)
  ├── ConfigureServices            ← DI registrations
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

var Host := THostBuilder.Create
  .ConfigureServices(procedure(Context: IHostBuilderContext;
                               Services: IServiceCollection)
  begin
    Services.AddSingleton<IMyService, TMyService>;
  end)
  .Build;

Host.Start;
Host.WaitForShutdown; // blocks until shutdown signal
```

Each `THostBuilder` is independent: several builders and several hosts can be alive at once in
one process.  
`Build` can only be called **once** per builder; it freezes configuration and creates the DI container.

---

## Configuration Pipeline

The host runs configuration in three ordered phases:

### 1. Host configuration

Bootstrap-level settings — applied before the environment is known:

```pascal
.ConfigureHostConfiguration(procedure(Builder: IConfigurationBuilder)
begin
  Builder.Add(TEnvironmentVariablesConfigurationSource.Create); // reads DAF_APP_ENV, DAF_APP_NAME, etc.
end)
```

### 2. App configuration

Full app settings — runs after the environment is resolved:

```pascal
.ConfigureAppConfiguration(procedure(Context: IHostBuilderContext;
                                     Builder: IConfigurationBuilder)
begin
  JsonConfig.AddFile(Builder, 'appsettings.json');
  JsonConfig.AddFile(Builder,
    'appsettings.' + string(Context.Environment.EnvironmentName) + '.json', [csoOptional]);
end)
```

### 3. Services

DI registrations — `Context.Configuration` is fully populated here:

```pascal
.ConfigureServices(procedure(Context: IHostBuilderContext;
                              Services: IServiceCollection)
begin
  var Db := Context.Configuration['Database:ConnectionString'];
  Services.AddSingleton<IDatabase>(TDatabase.Create(Db));
end)
```

You can call `ConfigureServices` multiple times — all callbacks are applied in registration order.

---

## Sharing data between startup pieces

`Properties` is a `TDictionary<string, TValue>` the builder carries, for the data that the pieces
composing startup need to pass to each other. It is on `IHostApplicationBuilder`, and the same
dictionary is read from the other side through `IHostBuilderContext.Properties` — which is what a
configuration or logging callback receives.

`THostBuilder` adds `UseProperty` for writing to it fluently:

```pascal
Builder.UseProperty('tenant', 'acme');

// ...anywhere holding the context:
var Tenant := Context.Properties['tenant'].AsString;
```

`UseProperty` is a method of the concrete `THostBuilder`, not of the interface: hold the builder in
a variable of its own type to chain it. Reading works through the interface everywhere.

---

## Registering Services

Inside `ConfigureServices` the full `IServiceCollection` API is available:

```pascal
.ConfigureServices(procedure(Context: IHostBuilderContext;
                              Services: IServiceCollection)
begin
  // Lifetimes
  Services.AddSingleton<ICache, TMemoryCache>;
  Services.AddScoped<IUnitOfWork, TDbUnitOfWork>;
  Services.AddTransient<IEmailSender, TSmtpSender>;

  // Hosted service (started/stopped with the host)
  Services.AddHostedService<TBackgroundIndexer>;
end)
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
- `Stop` is called in **reverse registration order** during shutdown.
- Hosted services are Singletons — you can inject them as `IOrderProcessor` too if needed.

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
DAF_APP_ENV=Development
```

Accepted shorthands: `deve`, `stag`, `prod`, `test` (case-insensitive).

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
  Logger.LogInfo('Application ready');
end);

// Fired when a shutdown has been requested (before services stop)
Lifetime.ApplicationStopping.Register(procedure begin
  Logger.LogInfo('Shutdown initiated');
end);

// Fired after all hosted services have stopped
Lifetime.ApplicationStopped.Register(procedure begin
  Logger.LogInfo('Application stopped');
end);

// Request a graceful shutdown programmatically
Lifetime.StopApplication;
```

`StopApplication` triggers the same shutdown sequence as `Ctrl+C` / `SIGTERM`.

---

## Accessing Services After Build

```pascal
var Host := HostBuilder.Build;
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
2. `IHostedService.Stop` called for each service (reverse order)
3. DI container disposes Singleton instances (reverse creation order)
4. `IHostApplicationLifetime.ApplicationStopped` callbacks fire

Always prefer `WaitForShutdown` over busy-waiting — it integrates with the OS signal handler via `TShutdownHook`.

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

Load the modules in `ConfigureServices`, one `AddModule<T>` per module:

```pascal
Builder.ConfigureServices(
  procedure(Context: IHostBuilderContext; Services: IServiceCollection)
  begin
    Services
      .AddModule<TInfraModule>
      .AddModule<TDomainModule>;
  end);
```

`AddModule<T>` instantiates the module, calls its `AddServices` on the spot, and owns its
lifetime. Do not call `TInfraModule.Create.AddServices(Services)` by hand: the module is an
`TInterfacedObject` created with a reference count of zero, so nobody frees it.

When a module needs configuration, construct it yourself and pass the instance:
`Services.AddModule(TInfraModule.Create(...))`. Handing it over as an `IAppModule` is what
gives it an owner, and it is the only way to pass a module any configuration.

**There is no automatic discovery.** `AddModules(Package)`, the RTTI scan for `IAppModule`, was
removed: it hid the declaration in a `uses` clause, left startup silently blind to a trimmed
RTTI, and put the registration order in the hands of reflection. A `TDafApplication` registers no
module on its own either — its modules are declared wherever the services are composed, exactly
as above. See [Startup composition](../../DependencyInjection/docs/STARTUP-COMPOSITION.md) for
the two phases and the ordering rules.
