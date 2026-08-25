# Application — Usage Guide

**🌍 Language: English | [Español](GUIDE.es.md)**

---

## Table of Contents

1. [Two ways to start, and which one to pick](#two-ways-to-start-and-which-one-to-pick)
2. [What this layer is, in .NET terms](#what-this-layer-is-in-net-terms)
3. [Quick start](#quick-start)
4. [TDafApplicationBuilder](#tdafapplicationbuilder)
5. [IDafApplication](#idafapplication)
6. [TDafApplication.App](#tdafapplicationapp)
7. [Lifecycle methods](#lifecycle-methods)
8. [Version metadata](#version-metadata)
9. [Services at runtime](#services-at-runtime)
10. [Subclass pattern](#subclass-pattern)
11. [Hosting integration](#hosting-integration)

---

## Two ways to start, and which one to pick

There are two entry points that work, and they are not rivals — they sit at different levels:

| | `THostBuilder` (Hosting) | `TDafApplication.CreateHostBuilder` (Application) |
|---|---|---|
| Lives in | `Daf.Hosting` | `Daf.Application.Builder` |
| `Build` returns | `IHost` | `IDafApplication` |
| You get | services, configuration, hosted services, startup tasks | all of that, plus what the next rows add |
| Run the main line | you write `Start` / `WaitForShutdown` / `Stop` yourself | `Run`, `Run(Exec)`, or an overridden `Execute` |
| Version metadata | — | `VersionInfo`, from a `[Version]` attribute on your class |
| Reachable from anywhere | — | `TDafApplication.App` |
| A class of your own to hang startup logic on | — | subclass `TDafApplication`, override `Execute` |

**Start with the application layer.** It is the one that has a place to put your startup code, and
it gives you the host anyway — `App.Host`. Drop to `THostBuilder` when you want the pipeline
without a main line around it: a package's own composition, a test harness, a plugin hosted inside
somebody else's program. `Hosting/samples/ConsoleSample` is the host on its own;
`samples/QuickStart` and `samples/HostedService` are the application layer.

What the application layer is **not**: `IDafApplication` re-declares the surface of `IHost`
(`Start`, `Stop`, `WaitForShutdown`, `Services`) but it does not descend from it. An application
**has** a host, it is not one, so it cannot be passed where an `IHost` is expected — use
`App.Host` there. A spec in `src/Application/test/` pins exactly this, because the previous
version of this guide claimed the opposite.

> **This split is being redesigned.** A family of application types — console, service, desktop,
> web — is planned, and it may retire `IHostBuilder` and deprecate `TDafApplication`. What is
> written here describes what exists today; it is not a verdict on which of the two survives, so
> do not read the recommendation above as permanent.

---

## What this layer is, in .NET terms

Most DAF modules port something from `Microsoft.Extensions.*`, and this one seems to port nothing
— there is no `Microsoft.Extensions.Application`. The equivalent does exist, just elsewhere in the
same ecosystem: it is the **application-builder family built on top of the generic host** —
`HostApplicationBuilder`, `WebApplicationBuilder`, `MauiAppBuilder`. Each pairs a builder with a
built application object (`WebApplication`, `MauiApp`) that owns the host rather than being one.
`TDafApplicationBuilder` / `IDafApplication` is that pair, with a single member so far.

Where it **diverges**, and it matters more than the resemblance:

- **Delphi has no extension methods, and in .NET the whole composition surface is extension
  methods.** `AddJsonFile`, `AddHostedService`, `UseSerilog`, `ConfigureKestrel` — all of them are
  extensions contributed by the package that owns the feature, which is how .NET lets a builder
  grow without the builder knowing about anything. Delphi has class helpers, but only **one** is
  in scope per type and none apply to interfaces, so the trick does not carry over. DAF composes
  through explicit facades instead — `JsonConfig.AddFile(Builder, …)` rather than
  `Builder.AddJsonFile(…)` — and through `IServiceCollection` being a record with real methods.
- **There is one application type, not a family.** .NET picks its builder by the kind of program
  you are writing; here the kind of program is whatever you put in `Execute`.
- **`RunAsync` is synchronous.** It exists for syntactic familiarity — the source says so — and
  Delphi has no `async`/`await` to make it anything else. It is `Run` under another name.

---

## Quick start

This is `samples/QuickStart/QuickStart.dpr`, verbatim; `eng/check-quickstart-snippet.py` compares
the two, so this block is a file that the compiler reads:

<!-- quickstart: src/Application/samples/QuickStart/QuickStart.dpr -->

```pascal
program QuickStart;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  System.SysUtils,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Hosting,
  Daf.Application.Builder,
  QuickStart.Greeter in 'QuickStart.Greeter.pas';

begin
  var Builder := TDafApplication.CreateHostBuilder;

  Builder.ConfigureServices(
    procedure(Context: IHostBuilderContext; Services: IServiceCollection)
    begin
      Services.AddSingleton<IGreeter, TGreeter>;
    end);

  Builder.Build.Run(
    procedure(Services: IServiceProvider)
    begin
      Writeln(Services.GetRequiredService<IGreeter>.Greet);
    end);
end.
```

Two shapes worth naming, because both are easy to get wrong:

- **The builder is held in a variable of its own class.** `CreateHostBuilder` returns a
  `TDafApplicationBuilder`, but every `Configure*` method returns `IHostBuilder`, and
  `IHostBuilder.Build` returns an `IHost`. Chain the whole thing fluently and you get the host,
  not the application. `TDafApplicationBuilder.Build` is the overload that returns
  `IDafApplication`, and it is reachable only through the class.
- **The service is declared in a unit, not in the `.dpr`.** A type in the program file is not in
  the `interface` section of any unit, and the container refuses it at run time with
  `ENonPublicType`.

---

## TDafApplicationBuilder

`TDafApplicationBuilder` extends `THostBuilder`, so it inherits the whole fluent surface —
`ConfigureHostConfiguration`, `ConfigureAppConfiguration`, `ConfigureServices`, `UseProperty`,
`AddHostedService<T>`. What it adds is a `Build` that returns the application:

```pascal
var Builder := TDafApplication.CreateHostBuilder;

Builder.ConfigureAppConfiguration(
  procedure(Context: IHostBuilderContext; Config: IConfigurationBuilder)
  begin
    JsonConfig.AddFile(Config, 'appsettings.json');
  end);

Builder.ConfigureServices(
  procedure(Context: IHostBuilderContext; Services: IServiceCollection)
  begin
    Services.AddHostedService<TWorker>;
    Services.AddTransient<IMyService, TMyService>;
  end);

Builder.Build.Run(True);
```

There is no `UseEnvironment` and no `UseShutdownTimeout`. The environment, the application name and
the content root come from environment variables, read once when the host is built:

| Variable | Sets | Default |
|---|---|---|
| `DAF_APP_ENV` | `IHostEnvironment.EnvironmentName` | `Production` |
| `DAF_APP_NAME` | `IHostEnvironment.ApplicationName` | the executable's own name |
| `DAF_CONTENT_ROOT` | `IHostEnvironment.ContentRootPath` | the executable's directory |

To supply your own builder class, `CreateHostBuilder<T>` takes it as a type argument:

```pascal
var Builder := TDafApplication.CreateHostBuilder<TMyBuilder>;
```

---

## IDafApplication

This is the declaration, not a paraphrase of it:

```pascal
IDafApplication = interface(IInvokable)
  ['{FAE0176F-FED1-42DF-92DF-C2A219853AC4}']
  function GetVersionInfo: TVersionInfo;
  function GetEnvironment: IHostEnvironment;
  function GetHost: IHost;
  function GetServices: IServiceProvider;
  procedure Start;
  procedure Stop;
  procedure WaitForShutdown;
  procedure Run(const WaitForShutdown: Boolean = False); overload;
  procedure Run(const Exec: TProc<IServiceProvider>;
                const WaitForShutdown: Boolean = False); overload;
  procedure RunAsync(const Exec: TProc<IServiceProvider> = nil;
                     const WaitForShutdown: Boolean = False);
  function GetConfiguration: IConfiguration;
  property VersionInfo: TVersionInfo read GetVersionInfo;
  property Host: IHost read GetHost;
  property Environment: IHostEnvironment read GetEnvironment;
  property Configuration: IConfiguration read GetConfiguration;
  property Services: IServiceProvider read GetServices;
end;
```

Note `interface(IInvokable)`, not `interface(IHost)` — see the first section for why that matters.

---

## TDafApplication.App

The constructor records the instance in a class variable, so after `Build` it is reachable from
anywhere:

```pascal
var App := TDafApplication.App;              // the class, not IDafApplication
var Svc := TDafApplication.App.Services.GetService<IMyService>;
```

Useful in units that are not wired through DI. It is a raw class reference and it is not cleared
when the application is released, so do not reach for it after the application is gone.

---

## Lifecycle methods

| Call | What happens |
|---|---|
| `Run` | starts the host, runs `Execute`, stops. Does **not** wait for a shutdown signal |
| `Run(True)` | the same, then blocks on the shutdown signal (Ctrl+C) before stopping |
| `Run(Exec)` | starts the host, calls `Exec(Services)` instead of `Execute`, stops |
| `Run(Exec, True)` | the same, blocking on the shutdown signal before stopping |
| `RunAsync(Exec, Wait)` | what both `Run` overloads call. Synchronous despite the name |
| `Start` | starts the host: startup tasks, then hosted services. Does not block |
| `Stop` | stops the hosted services, releases the container, and drops the host |
| `WaitForShutdown` | blocks the calling thread until Ctrl+C |

`WaitForShutdown` defaults to **`False`** in every overload. If `Exec` or `Execute` raises, the
application stops and the exception is re-raised.

After `Stop` the application no longer has a host: `App.Host` is `nil` and it cannot be restarted.

---

## Version metadata

`VersionInfo` is **not** read from a file. It comes from a `[Version]` attribute on the application
class, found by RTTI — so an application without that attribute has an empty `TVersionInfo`, which
is the usual reason for wondering where the version went:

```pascal
type
  [Version('1.2.3-rc.2', 'MyApp', 'My Company')]
  TMyApp = class(TDafApplication)
  end;
```

```pascal
TVersionInfo = record
  property Tag            : string;   // '1.2.3' or '1.2.3-rc.2', as given
  property ApplicationName: string;
  property Company        : string;
  property Major          : Integer;
  property Minor          : Integer;
  property Patch          : Integer;
  property PreRelease     : string;   // 'alpha.1', 'rc.2', … empty when stable
  property Metadata       : string;   // build metadata
  property ArchBits       : Integer;  // 32 or 64, from the compilation
  property Platform       : string;   // 'Win32', 'Win64', …
  property Debug          : Boolean;

  function VersionTag(const WithCompiledMeta: Boolean = False): string;
end;
```

`TDafApplication.VersionRequested` is a class function that answers whether the first command-line
argument is `--version`, for printing it and exiting before anything is built.

---

## Services at runtime

```pascal
var App := TDafApplication.App;

var Logger := App.Services.GetRequiredService<ILoggerFactory>.CreateLogger('Main');
var Port := App.Configuration['server:port'];

if App.Environment.IsDevelopment then
  Logger.LogDebug('dev mode', []);
```

`IConfiguration` returns the empty string both for a key that is absent and for one whose value is
empty; only a provider's `TryGet` tells the two apart.

---

## Subclass pattern

`CreateHostBuilder` is a class method that uses the class it was called on, and that is what
decides the type `Build` instantiates. Call it on `TDafApplication` and you get a
`TDafApplication`:

```pascal
type
  TMyApp = class(TDafApplication)
  protected
    procedure Execute; override;
  end;

procedure TMyApp.Execute;
begin
  // Services, Configuration, Environment and VersionInfo are this object's own properties.
  // There is no inherited Logger and no AppName: resolve what you need.
  var Logger := Services.GetRequiredService<ILoggerFactory>.CreateLogger('MyApp');
  Logger.LogInformation('running {V}', [VersionInfo.Tag]);
  Services.GetRequiredService<IWorkerService>.DoWork;
end;

// TMyApp here, not TDafApplication.
var Builder := TMyApp.CreateHostBuilder;
Builder.ConfigureServices(
  procedure(Context: IHostBuilderContext; Services: IServiceCollection)
  begin
    Services.AddTransient<IWorkerService, TWorkerService>;
  end);
Builder.Build.Run;
```

`Execute` is what `Run` calls when no `Exec` is given; the two are alternatives, not a sequence.

---

## Hosting integration

`TDafApplicationBuilder` extends `THostBuilder`, so everything the Hosting module offers works
unchanged:

- hosted services → `Services.AddHostedService<TWorker>`
- one-shot initialization → `Services.AddStartupTask<TSeeder>`, which runs before the hosted
  services
- cooperative shutdown → `IHostApplicationLifetime`
- scopes → `IServiceScopeFactory.CreateScope`

See the [Hosting Guide](../../Hosting/docs/GUIDE.md) for the details.
