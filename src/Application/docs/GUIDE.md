# Application — Usage Guide

**🌍 Language: English | [Español](GUIDE.es.md)**

---

## Table of Contents

1. [Overview](#overview)
2. [TDafApplicationBuilder](#tdafapplicationbuilder)
3. [IDafApplication](#idafapplication)
4. [TDafApplication class property](#tdafapplication-class-property)
5. [Lifecycle methods](#lifecycle-methods)
6. [Version metadata](#version-metadata)
7. [Services at runtime](#services-at-runtime)
8. [Subclass pattern](#subclass-pattern)
9. [Hosting integration](#hosting-integration)

---

## Overview

The Application module sits on top of the Hosting pipeline:

```
TDafApplication.CreateHostBuilder
        │
        ▼
TDafApplicationBuilder   (extends THostBuilder)
        │  .ConfigureServices / .ConfigureAppConfiguration / …
        ▼
      .Build
        │
        ▼
IDafApplication          (extends IHost)
        │  .Run / .RunAsync / .Start / .Stop
```

Everything available on `THostBuilder` and `IHost` is also available here.

---

## TDafApplicationBuilder

`TDafApplication.CreateHostBuilder(ExePath)` returns a `TDafApplicationBuilder`. It inherits all fluent methods from `THostBuilder`:

```pascal
TDafApplication
  .CreateHostBuilder(ParamStr(0))
  .UseEnvironment('Production')
  .ConfigureAppConfiguration(
    procedure(Ctx: IHostBuilderContext; Cfg: IConfigurationBuilder)
    begin
      Cfg.AddJsonFile('appsettings.json');
    end)
  .ConfigureServices(
    procedure(Ctx: IHostBuilderContext; Services: IServiceCollection)
    begin
      Services.AddHostedService<TWorker>;
      Services.AddTransient<IMyService, TMyService>;
    end)
  .Build
  .Run;
```

---

## IDafApplication

```pascal
IDafApplication = interface(IHost)
  // Lifecycle
  procedure Run(WaitForShutdown: Boolean = True); overload;
  procedure Run(Exec: TProc; WaitForShutdown: Boolean = True); overload;
  procedure RunAsync;
  procedure Start;
  procedure Stop;
  procedure WaitForShutdown;

  // Services
  function Services    : IServiceProvider;
  function Environment : IHostEnvironment;
  function Configuration: IConfiguration;
  function Host        : IHost;

  // Version
  function VersionInfo : TVersionInfo;
end;
```

---

## TDafApplication class property

After calling `.Build`, the application instance is accessible globally:

```pascal
var App := TDafApplication.App; // class property
```

Useful in units that are not wired via DI and need to resolve services:

```pascal
var Svc := TDafApplication.App.Services.GetService<IMyService>;
```

---

## Lifecycle methods

| Method | Description |
|--------|-------------|
| `Run` | Start host, block until shutdown signal (Ctrl+C, `IHostApplicationLifetime.StopApplication`) |
| `Run(Exec)` | Execute `Exec` proc then stop cleanly |
| `RunAsync` | Start host on a background thread |
| `Start` | Start host without blocking |
| `Stop` | Signal graceful shutdown |
| `WaitForShutdown` | Block calling thread until shutdown completes |

---

## Version metadata

`TVersionInfo` is a record populated from `VERSION.txt` at compile time:

```pascal
TVersionInfo = record
  Major     : Integer;
  Minor     : Integer;
  Patch     : Integer;
  PreRelease: string;    // 'alpha.1', 'rc.2', …  empty if stable
  Metadata  : string;    // build metadata (git SHA, etc.)
  Tag       : string;    // 'Major.Minor.Patch' or 'Major.Minor.Patch-PreRelease'
  ArchBits  : Integer;   // 32 or 64
  Platform  : string;    // 'Win32', 'Win64', 'Linux64', …
  Debug     : Boolean;

  function VersionTag(WithCompiledMeta: Boolean = False): string;
end;
```

### Example

```pascal
var V := TDafApplication.App.VersionInfo;
WriteLn(V.VersionTag);                // '1.2.3'
WriteLn(V.VersionTag(True));          // '1.2.3+abc1234-Win64-64bit-Debug'
WriteLn(Format('%d-bit %s', [V.ArchBits, V.Platform]));
```

---

## Services at runtime

```pascal
var App := TDafApplication.App;

// Resolve from root container
var Logger := App.Services.GetRequiredService<ILoggerFactory>
                         .CreateLogger('Main');

// Access configuration key
var Port := App.Configuration['server:port'];

// Check environment
if App.Environment.IsEnvironment('Development') then
  Logger.LogDebug('Dev mode', []);
```

---

## Subclass pattern

Inherit from `TDafApplication` and override `Execute` for main-line application logic:

```pascal
type
  TMyApp = class(TDafApplication)
  private
    FWorkerService: IWorkerService;
  protected
    procedure Execute; override;
  end;

procedure TMyApp.Execute;
begin
  FWorkerService := Services.GetRequiredService<IWorkerService>;
  Logger.LogInformation('Starting {AppName} v{Version}',
                        [AppName, VersionInfo.Tag]);
  FWorkerService.DoWork;
end;
```

Register and run:

```pascal
TDafApplication
  .CreateHostBuilder(ParamStr(0))
  .ConfigureServices(procedure(S: IServiceCollection)
    begin
      S.AddTransient<IWorkerService, TWorkerService>;
    end)
  .Build                  // creates TMyApp instance, sets App class prop
  .Run;
```

---

## Hosting integration

`TDafApplicationBuilder` fully extends `THostBuilder`, so all Hosting features work identically:

- Hosted services → `Services.AddHostedService<TWorker>`
- Cancellation tokens → `IHostApplicationLifetime`
- Graceful shutdown timeout → `UseShutdownTimeout`
- Scoped services → `IServiceScope` created per request or per worker tick

See the [Hosting Guide](../../Hosting/docs/GUIDE.md) for details.
