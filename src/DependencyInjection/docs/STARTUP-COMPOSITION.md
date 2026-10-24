# Startup composition

**🌍 Language: English | [Español](STARTUP-COMPOSITION.es.md)**

---

## Contents

1. [Two phases, not one](#two-phases-not-one)
2. [Grouping registrations](#grouping-registrations)
3. [Governing someone else's registration](#governing-someone-elses-registration)
4. [Decorating](#decorating)
5. [Startup tasks](#startup-tasks)
6. [Reporting a startup failure](#reporting-a-startup-failure)
7. [Validating the graph](#validating-the-graph)
8. [.NET ↔ DAF](#net--daf)

---

## Two phases, not one

Almost every doubt about startup comes from mixing up two things that happen at different
moments:

```
  ┌───────────────────────────────────────┐        ┌──────────────────────────────────────┐
  │ PHASE 1 · COMPOSING                   │        │ PHASE 2 · STARTING                   │
  │ the IServiceProvider does NOT exist   │        │ the IServiceProvider already exists   │
  ├───────────────────────────────────────┤        ├──────────────────────────────────────┤
  │ WHAT gets registered, in what order?  │        │ WHAT runs, in what order?            │
  │                                       │        │                                      │
  │   AddModule<T>                        │        │   IStartupTask.Run                   │
  │   AddXxx functions                    │        │   IHostedService.Start               │
  │   TryAdd / Replace / Decorate         │        │   ApplicationStarted                 │
  └───────────────────────────────────────┘        └──────────────────────────────────────┘
                          └──── BuildServiceProvider ────┘
```

`IAppModule` belongs to phase 1 and has **one** phase: `AddServices`, and nothing else. If
something needs the provider already built, it is a phase-2 concern, and its contract is
`IStartupTask` — not a second phase of the module. A composition object dies when
composition ends; keeping it alive until startup, without it being a service, is the road
ASP.NET Core walked with the `Startup` class before turning back.

The two phases are not rivals: a package usually needs both — it declares its registrations
in phase 1 and leaves a phase-2 task behind, so the executable writes one line. See
`src/Cli/samples/PackageInit`.

## Grouping registrations

Two ways, and the criterion fits in one sentence: **if the group needs a type — so the
application can inherit it and override a registration, so it can have variants by subtype,
or so it can carry configuration — it is a module; if it is just a handful of registrations,
a function is enough.**

### A module

```pascal
type
  TEntornoModule = class(TAppModule)
  public
    procedure AddServices(const ServiceCollection: IServiceCollection); override;
  end;
```

Applied by name, in the order written, chaining with the `Add*` that were already there:

```pascal
Services
  .AddSingleton<IClock, TClock>
  .AddModule<TDomainDaosModule>
  .AddModule<TEntornoModule>
  .AddModule<TEvaluationModule>;
```

`AddModule<T>` instantiates the module and calls its `AddServices` **on the spot**, not
deferred: composition is a single sequence, and whatever is written after a module runs after
it. That is what makes a later `TryAdd` leave the module's registration alone.

A module that carries configuration cannot get it from dependency injection — it runs before
the provider exists — so it comes from the caller, through the instance overload:

```pascal
var Module: IAppModule := TEntornoModule.Create(EnvironmentName);
Services.AddModule(Module);
```

The generic overload carries a `constructor` constraint, so **the module's own constructor
runs** and no RTTI is involved.

`AddModule<T>` is a method of `IServiceCollection` itself, not a `record helper`: it is
always available, it does not depend on the order of the `uses`, and any package can
contribute its module without colliding with another.

**Lineage.** The module is not a DAF invention: it is `Autofac.Module`, `IWindsorInstaller`
of Castle Windsor, `Registry` of StructureMap — grouping registrations in a class is a
classic addition of the full containers. What `Microsoft.Extensions` does not have is the
concept, because there that job is done by extension methods, which Delphi does not have;
that is why here the module hangs off `IServiceCollection` itself.

### A function

```pascal
function AddEntorno(const Services: IServiceCollection): IServiceCollection;
```

This is the extension method of the generic host, and it is the canonical form there. In
Delphi there is a catch worth stating plainly: **it does not chain.** `Services.AddEntorno`
is method syntax, and a free function is called `AddEntorno(Services)`. A startup written
with functions reads as a sequence, not as a chain:

```pascal
Services.AddSingleton<IClock, TClock>;
AddDomainDaos(Services);
AddEntorno(Services);
AddEvaluation(Services);
```

It is still in one place and in order, which is what matters. Anyone arriving from C#
expecting `services.AddEntorno()` will not get it.

### What both do

Both make the `uses` with the `// force-link:` comment unnecessary. Calling the function
links its unit; naming the type in `AddModule<T>` links its own.

## Governing someone else's registration

`Add*` always adds. To write a reusable `AddXxx` you need the semantics .NET takes for
granted: **the library sets the default, the application decides.**

| DAF | .NET |
|---|---|
| `TryAddSingleton` / `TryAddScoped` / `TryAddTransient` | `ServiceCollectionDescriptorExtensions.TryAdd*` |
| `Replace(LifeTime, ServiceType, …)` | `ServiceCollectionDescriptorExtensions.Replace` |
| `RemoveAll(ServiceType)` | `ServiceCollectionDescriptorExtensions.RemoveAll` |
| `Descriptors(ServiceType)` | indexing `IServiceCollection`, which **is** an `IList<ServiceDescriptor>` |

`TryAdd*` registers **only if there is no registration** for that service type, and never
replaces the existing one:

```pascal
// In the package: this is its default
Services.TryAddSingleton<IClock, TSystemClock>;

// In the startup, before or after: this is what wins
Services.AddSingleton<IClock, TFrozenClock>;
```

`Replace` removes the first registration of the type, if there is one, and adds the new one at
the end, as in .NET: with two registrations before, the second stays and the new one comes after
it. `RemoveAll` removes them all without adding anything — which is what a spec harness needs to
take a production registration apart.

**A deliberate difference from .NET.** There, the collection *is* a mutable list, and
`TryAdd`, `Replace` and Scrutor are all extension methods on top of it. In DAF the
descriptors are exposed **read-only**, as a copy: the live list is owned by the internal
dictionary, and handing that reference out would let anyone mutate the collection without
going through its guards. The capability — read in order to compose — is kept; the raw
mutability, which is harmless there because there is no owned list involved, is not.

That read access is what makes the rest writable, inside DAF and outside it. A third party
can write its own Scrutor without touching the framework:

```pascal
function AddShouting(const Services: IServiceCollection): IServiceCollection;
begin
  Result := Services;
  var Current := Services.Descriptors<IFoo>;
  if Length(Current) = 0 then Exit;

  var Inner := Current[High(Current)].Factory;
  Services.Replace(Current[High(Current)].LifeTime, TypeInfo(IFoo),
    function(Provider: IServiceProvider): IInterface
    begin
      var Original: IFoo;
      Supports(Inner(Provider), IFoo, Original);
      Result := TShoutingFoo.Create(Original) as IFoo;
    end);
end;
```

**Everything here stops when the collection does.** `BuildServiceProvider` freezes the
collection; after that, `TryAdd*`, `Replace`, `RemoveAll` and `Decorate` raise, exactly like
`Add`. A `TryAdd` that does not add is still an attempted change: on a frozen collection it
is the caller's mistake, not a silent no-op.

## Decorating

```pascal
Services.Decorate<IRepository>(
  function(const Inner: IRepository; const Provider: IServiceProvider): IRepository
  begin
    Result := TLoggingRepository.Create(Inner, Provider.GetRequiredService<ILogger>);
  end);
```

Every descriptor of the service is replaced by one that builds the original with the old
factory and hands it to the decorator. **The lifetime is preserved**, and two `Decorate`
calls nest in registration order: the second wraps the first, which wraps the original.

This is not in the .NET framework: it is **Scrutor** (`services.Decorate<IFoo, Bar>()`), and
the full containers bring it too (Autofac's `RegisterDecorator`).

**Scope.** Decoration is for wrapping behaviour — logging, caching, retries, metrics. It is
**not** the place to hang startup side effects: that is what `IStartupTask` is for. A
decorator that returns `Inner` without wrapping anything is an abuse of the mechanism.

## Startup tasks

```pascal
IStartupTask = interface(IInvokable)
  procedure Run;   // once, at startup, with the provider built. There is no Stop.
end;

Services.AddStartupTask<TSeedEngineTask>;
```

`THost.Start` runs the startup tasks **in registration order**, **before** the
`IHostedService` and before notifying `ApplicationStarted`. The order matters: a startup task
prepares state a hosted service may depend on. If one raises, startup fails and the
exception propagates: no hosted service is started, and the application is never declared
started. An initialization that cannot complete must not leave the app half up.

**This is a deliberate divergence from `Microsoft.Extensions.*`**, which does not have the
contract. The known consequence there is that `IHostedService` gets used as an initializer,
with an empty `StopAsync` that lies about what the object is. DAF clones .NET to inherit its
good decisions, not its gaps.

**The rule, in one sentence: if it has something to stop, it is `IHostedService`; if it just
runs once, it is `IStartupTask`.**

`IHostApplicationLifetime.ApplicationStarted` does not cover this case: it runs *after* the
hosted services and after the application declares itself started, so it cannot prepare
state those services depend on.

## Reporting a startup failure

The same boundary that splits the two phases splits **who reports a failure**, and it is worth
knowing which side you are on: reporting on the wrong side means either nobody hears it, or your
own logging configuration gets skipped.

```
  THostBuilder.Build                          ── bootstrap channel ──▶  system debug output
  ├── InitializeHostConfiguration                                       error output, if the
  ├── InitializeHostingEnvironment                                      process has one
  ├── InitializeDefaultAppConfiguration  ← appsettings.json is read here
  ├── InitializeHostBuilderContext                                      whatever you registered
  ├── InitializeAppConfiguration
  └── InitializeServiceProvider
  ════════════════════ BuildServiceProvider ════════════════════
  THost.Start / THost.Stop                    ── ILogger pipeline ──▶  your providers, your
                                                                       configuration
```

Before `BuildServiceProvider` there is no `IServiceProvider`, so there is no `ILoggerFactory` and
nothing that was registered can report anything. What covers that stretch is the **bootstrap
channel**: an `ILogger` available from the moment the process starts, with no dependency injection
and no configuration. It cannot read configuration on purpose — failing to read the configuration
is one of the things it is there to report, and reading a file to find out where to say that the
file could not be read is the same chicken and the same egg.

Once the provider exists the pipeline takes over, with the providers and the levels you configured.
The bootstrap channel stays as the **backstop**: if `ILoggerFactory` cannot be resolved, the report
comes out through it instead of being lost. `THostBuilder.Build` stays on the channel, because that
is exactly the stretch where the provider does not exist yet.

**The bootstrap channel is not the application logger.** Keeping it after `BuildServiceProvider`
means skipping the configuration you wrote. This two-stage handover is a pattern with a name in
.NET: Serilog's `CreateBootstrapLogger`.

### The failure always propagates, and that is half the report

`THostBuilder.Build` re-raises after reporting, and that is not an implementation detail. Every
platform already has its way of showing that a process did not start — a dialog on a desktop
application, the error output and a non-zero exit code on a console one — and on the desktop that
is the *only* way that reaches whoever has the program in front of them. A `.dpr` that swallows
the exception takes that away.

### Destinations

DAF only brings destinations **whose place it does not have to guess**:

| Destination | Where it writes | Available when |
|---|---|---|
| `TDebugOutputBootstrapSink` | system debug output (`OutputDebugString`) | on Windows; a debugger is **not** required |
| `TStdErrBootstrapSink` | the process's error output | the process has an error handle |
| `TPlatformLogBootstrapSink` | the platform's log service (`IFMXLoggingService`) | the platform registers that service |
| `TFileBootstrapSink` | the file you name | **not registered by default** |

The first two are registered for you. `TPlatformLogBootstrapSink` lives in the `Daf.FMX` package,
in `Daf.FMX.BootstrapSink`, so that FireMonkey reaches only whoever references it; it covers
macOS, iOS and Android, and declares itself unavailable on Linux, where there is no FireMonkey
to ask.

A durable destination needs a path, and that path is known by the application — where it keeps its
data — not by the framework. So DAF ships the mechanism and does not choose the path:

```pascal
BootstrapLog.AddSink(TFileBootstrapSink.Create(TPath.Combine(MyDataFolder, 'startup.log')));
```

This is .NET's split too: it writes no startup file of its own and invents no path.

A destination that cannot write **declares itself unavailable** instead of taking the report and
losing it, and a destination that raises is dropped without taking the startup down with it. If
you need to know that nothing at all is listening — the worst case, and the one that used to look
just like a successful report — `BootstrapLog.HasAvailableSink` answers it.

### Reporting your own startup

An application has as much startup of its own as the framework does: the path of its configuration
file, its sources, its composition. It reports under **its own category**:

```pascal
BootstrapLog.LoggerFor('Acme.Payroll').LogError('data folder "%s" is not writable', [Folder]);
```

The channel keeps a `Warning`-and-above threshold: startup is no place for debug chatter, and a
channel that writes always ends up ignored.

**Coming from `DAFInternalLogger`?** That global is gone. It was the only way in while the channel's
category was pinned to `DAF`; once the pin came off it became a second door that labels as DAF
whoever is not. Replace it with `BootstrapLog.LoggerFor(<your category>)` — which is exactly the
call DAF's own reports go through now, so the framework does not preach an API it does not use.

### What each deployment shape gets

| Shape | What shows the failure |
|---|---|
| Desktop (VCL or FMX, no console) | the propagation: the platform shows its own dialog. Register a file destination if you want a record that survives the "OK" |
| Console | the error output, plus a non-zero exit code |
| Unattended (wrapper, task scheduler) | the error output, which the wrapper or the scheduler captures; a file destination for the durable record |

The `.dpr` is where this is decided, before the host is built. This is the `StartupReport`
sample, verbatim -- the file the compiler reads, and a check compares the two:

<!-- quickstart: src/Hosting/samples/StartupReport/StartupReport.dpr -->

```pascal
program StartupReport;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  System.IOUtils,
  System.SysUtils,
  Daf.Extensions.Logging,
  Daf.Extensions.Hosting,
  Daf.Hosting,
  StartupReport.Payroll in 'StartupReport.Payroll.pas';

// Lo que el .dpr decide antes de que exista el host: donde queda el informe de arranque, y que se
// ve cuando el arranque falla. La consola se lleva el error y un codigo de salida distinto de
// cero; el fichero se lleva el registro que sobrevive.
//
//   StartupReport         -> arranca el servicio, y sale con Ctrl+C
//   StartupReport --fail  -> el arranque aborta: rc=1, y el motivo en startup.log
begin
  // La aplicacion sabe donde viven sus datos; DAF no.
  BootstrapLog.AddSink(TFileBootstrapSink.Create(TPath.Combine(DataFolder, 'startup.log')));
  var Host: IHost;
  try
    var Builder := THost.CreateBuilder;

    AddPayroll(Builder.Services);

    Host := Builder.Build;
    Host.Start;
  except
    on E: Exception do
    begin
      BootstrapLog.LoggerFor('Acme.Payroll').LogCritical(E, 'el arranque no pudo completarse');
      ExitCode := 1;   // consola y desatendido: el codigo de salida es parte del informe
      raise;           // escritorio: esto es lo que hace que la plataforma lo muestre
    end;
  end;
  try
    Host.WaitForShutdown;
  finally
    StopAndNil(Host);
  end;
end.
```

## Validating the graph

```pascal
var Provider := Services.BuildServiceProvider(TServiceProviderOptions.Create(True, True));
//                                                                          │     └ ValidateScopes
//                                                                          └────── ValidateOnBuild
```

- `ValidateOnBuild` checks, for every descriptor registered **by class**, that the chosen
  constructor can be satisfied **and that every marked property can be filled**. It reports
  **all** the failures at once, not the first one: a miswired startup usually has several, and
  finding them one at a time costs a compile cycle each.
- `ValidateScopes` rejects a Singleton depending on a Scoped (*captured dependency*), naming
  both services. It reads the same two routes: the dependency captured through a marked
  property is caught like the one taken by the constructor. With it on, resolving a Scoped from the
  root provider — directly, or through a service that requires it — also fails, when it is resolved:
  a Scoped resolved there lives until the container shuts down. Resolve it from a scope. A Scoped
  resolved through the provider a Singleton received — from its factory, an `ILazy` or a provider it
  kept — fails the same way.

Both are calqued from `ServiceProviderOptions`, and both are **opt-in**: `BuildServiceProvider`
without options validates nothing, and the host of the application family turns both on in the
`Development` environment. In Delphi this is
worth more than in .NET: without comfortable stack traces, the difference is between a
message at startup and an access violation halfway through a long operation.

**The limit: it only reaches descriptors registered by class**, where the constructor is
inspectable through RTTI. Registrations by **factory** are opaque — validating what a lambda
does would require running it, and running it during `Build` would have effects. So a startup
that validates without complaining does not say the whole graph is sound: it says the
inspectable part of it is.

## .NET ↔ DAF

| .NET | DAF | Note |
|---|---|---|
| `IServiceCollection` as `IList<ServiceDescriptor>` | `Descriptors(ServiceType)` | Read-only copy, on purpose |
| `TryAdd*` / `Replace` / `RemoveAll` | same names | Same semantics |
| Scrutor's `Decorate` | `Decorate<S>` | Not in the .NET framework either |
| `ServiceProviderOptions` | `TServiceProviderOptions` | Off by default, on in the family's host in Development; only reaches class registrations |
| extension methods `services.AddXxx()` | `AddXxx(Services)` | Delphi has no extension methods: **it does not chain** |
| Autofac / Windsor / StructureMap modules | `AddModule<T>` | Divergence of layer: on the collection, not on the container |
| — | `IStartupTask` | Deliberate divergence: .NET has no contract for this |
| Serilog's `CreateBootstrapLogger` | `BootstrapLog` | The two-stage handover, by its known name |
| `Microsoft.Extensions.Logging.Debug` | `TDebugOutputBootstrapSink` | Same job: the system debug output, with no configuration |

---

**See also:** [DependencyInjection — Usage Guide](GUIDE.md) ·
`src/Cli/samples/PackageInit`
