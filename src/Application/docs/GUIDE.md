# Application — Usage Guide

**🌍 Language: English | [Español](GUIDE.es.md)**

---

## Table of Contents

1. [Which entry point is yours](#which-entry-point-is-yours)
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

## Which entry point is yours

There is **one** per application variant, and picking is a matter of what kind of application you
are writing — not which of two layers to use. A VCL one, whole and with its own specs, is
[TicTacToeApp](../samples/TicTacToeApp/README.md):

| Your application | Entry point | Lives in | `Build` returns |
|---|---|---|---|
| Console / CLI tool | `TCliApplication.CreateBuilder(Args)` | `Daf.Cli` | `ICliApplication` |
| Web | `TWebApplication.CreateBuilder(Args)` | `Daf.Web` | `IWebApplication` |
| Desktop, VCL | `TVclApplication.CreateBuilder` | `Daf.VCL` | `IVclApplication` |
| Desktop, FireMonkey | `TFmxApplication.CreateBuilder` | `Daf.FMX` | `IFmxApplication` |

All four share `Services`, `Configuration`, `Environment`, `Logging` and `Properties`, written the
same way. **That shared contract — `IHostApplicationBuilder` — is what makes them a family rather
than four similar things**, and it is what lets a package contribute startup registrations without
knowing which variant will consume it.

They split in two by **who owns the loop**: in CLI and Web, DAF owns the process and `Run` starts,
works and stops, so the `.dpr` ends when `Run` ends. In VCL and FMX the message loop already exists
and belongs to `Vcl.Forms.Application` / `FMX.Forms.Application`; the variant creates the main form
and delegates. Two lifecycle contracts, not four variations of one.

### What was retired, and where it went

| Was | Is now |
|---|---|
| `THostBuilder.Create.ConfigureServices(...)` | `B.Services.…` on any variant's builder — the deferred style is gone; there is one style, the immediate one |
| `IHostBuilder` | `IHostApplicationBuilder`, the family's shared contract. `THostBuilder` survives whole as **implementation detail** |
| `TDafApplication.CreateHostBuilder` | the `CreateBuilder` of your variant, from the table above |
| `TDafApplication` / `TDafApplicationBuilder` | **deprecated**, still compiling in 2.0. Their replacement is the variant |
| `TDafApplication.VersionRequested` | `ICommandLineParseResult.VersionRequested` — the grammar answers `--version`, and the hand-written check is gone rather than moved |
| `TDafApplication.App` | `DafApp`, **only in VCL and FMX**. CLI and Web have no global on purpose |
| `Daf.Application.Version` | moved to `Daf.Commons`, unchanged — all four variants reach it without the deprecated package |

### What the application layer adds over the host

The host gives you the container, the configuration, the hosted services and the startup tasks. On
top of that, the variants add **the edge around `Run`** — which is the part every `.dpr` used to
write by hand:

| What | Where | Which variants |
|---|---|---|
| Deciding whether the process starts at all | `SingleInstance(Id)` | CLI |
| The policy for what escapes, and the exit code | `OnUnhandled(Policy)` | CLI |
| The work, and its exit code | `OnExecute` / a registered `ICliHandler` | CLI |
| `--version`, `--help`, invalid invocation | `RootCommand`, answered by the parse | CLI |
| The HTTP server, wired to DI and the host | `Web.AddHttpServer` + `MapControllers` | Web |
| Creating the main form and handing over the loop | `Run(TFormClass)` | VCL, FMX |
| A global for the code the platform builds itself | `DafApp` | VCL, FMX |

Two members of the .NET family are **not** ported, and they are declared rather than discovered:
`Metrics`, because DAF has no equivalent piece to point at, and `ConfigureContainer<T>`, because a
Delphi interface admits no generic methods — it is not rejected, it is unrepresentable.

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
  Daf.CommandLine,
  Daf.Configuration.CommandLine,
  Daf.Cli,
  QuickStart.Greeter in 'QuickStart.Greeter.pas';

// Lo mas corto que se puede escribir con la familia: una variante, sus registros donde estan
// escritos, y el codigo de salida que devuelve el trabajo.
begin
  var B := TCliApplication.CreateBuilder(CommandLineConfig.ProcessArgs);

  B.Services.AddSingleton<IGreeter, TGreeter>;

  B.OnExecute(
    function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
    begin
      Writeln(Services.GetRequiredService<IGreeter>.Greet);
      Result := TCliExit.Ok;
    end);

  ExitCode := B.Build.Run;
end.
```

Two shapes worth naming, because both are easy to get wrong:

- **The builder answers before it is built.** `CreateBuilder` returns the family variant's own
  builder, whose `Services`, `Configuration` and `Environment` are alive on return, so each
  registration takes effect where it is written. `Build` returns the application, and its `Run`
  gives the exit code.
- **The service is declared in a unit, not in the `.dpr`.** A type in the program file is not in
  the `interface` section of any unit, and the container refuses it at run time with
  `ENonPublicType`.

---

> **From here on, this guide documents the deprecated route.** `TDafApplication` and
> `TDafApplicationBuilder` still compile in 2.0, so what follows is still true — but it is not the
> recommendation. Your entry point is in the table above.

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
