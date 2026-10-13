# The application family — Usage Guide

**🌍 Language: English | [Español](APPLICATION-FAMILY.es.md)**

A DAF application is one of four variants built on the generic [host](GUIDE.md). This guide covers
what they share: which one is yours, what the layer adds over the host, what it corresponds to in
.NET, and where the entry points that came before it went.

---

## Table of Contents

1. [Which entry point is yours](#which-entry-point-is-yours)
2. [What was retired, and where it went](#what-was-retired-and-where-it-went)
3. [What the application layer adds over the host](#what-the-application-layer-adds-over-the-host)
4. [What this layer is, in .NET terms](#what-this-layer-is-in-net-terms)

---

## Which entry point is yours

There is **one** per application variant, and picking is a matter of what kind of application you
are writing — not which of two layers to use. A VCL one, whole and with its own specs, is
[TicTacToeApp](../../VCL/samples/TicTacToeApp/README.md):

| Your application | Entry point | Lives in | `Build` returns |
|---|---|---|---|
| Console / CLI tool | `TCliApplication.CreateBuilder(Args)` + `B.ParseArgs(Args)` | `Daf.Cli` | `ICliApplication` |
| Web | `TWebApplication.CreateBuilder(Args)` | `Daf.Web` | `IWebApplication` |
| Desktop, VCL | `TVclApplication.CreateBuilder` | `Daf.VCL` | `IVclApplication` |
| Desktop, FireMonkey | `TFmxApplication.CreateBuilder` | `Daf.FMX` | `IFmxApplication` |

All four share `Services`, `Configuration`, `Environment`, `Logging`, `Properties` and
`ServiceProviderOptions`, written the
same way. **That shared contract — `IHostApplicationBuilder` — is what makes them a family rather
than four similar things**, and it is what lets a package contribute startup registrations without
knowing which variant will consume it.

They split in two by **who owns the loop**: in CLI and Web, DAF owns the process and `Run` starts,
works and stops, so the `.dpr` ends when `Run` ends. In VCL and FMX the message loop already exists
and belongs to `Vcl.Forms.Application` / `FMX.Forms.Application`; the variant creates the main form
and delegates. Two lifecycle contracts, not four variations of one.

---

## What was retired, and where it went

| Was | Is now |
|---|---|
| `THostBuilder.Create.ConfigureServices(...)` | `B.Services.…` on any variant's builder — the deferred style is gone; there is one style, the immediate one |
| `IHostBuilder` | `IHostApplicationBuilder`, the family's shared contract. `THostBuilder` survives whole as **implementation detail** |
| `TDafApplication` / `TDafApplicationBuilder` | **retired in 2.0.** Their replacement is the variant, from the table above |
| `TDafApplication.CreateHostBuilder` | the `CreateBuilder` of your variant |
| `IDafApplication` | the `I…Application` your variant's `Build` returns |
| `TDafApplication.RunAsync` | `Run`. It was the same call under another name, and Delphi has no `async`/`await` to make it anything else |
| `TDafApplication.VersionRequested` | `IParseResult.IsVersionRequested` — the grammar answers `--version`, and the hand-written check is gone rather than moved |
| `TDafApplication.App` | `DafApp`, **only in VCL and FMX**. CLI and Web have no global on purpose |
| `Daf.Application.Version` | `Daf.Commons`, unchanged — all four variants reach `TVersionInfo` and `[Version]` from there |

---

## What the application layer adds over the host

The host gives you the container, the configuration, the hosted services and the startup tasks. On
top of that, the variants add **the edge around `Run`** — which is the part every `.dpr` used to
write by hand:

| What | Where | Which variants |
|---|---|---|
| Deciding whether the process starts at all | `SingleInstance(Id)` | CLI, VCL, FMX |
| The policy for what escapes, and the exit code | `OnException(Policy)` | CLI, Web |
| Handing that policy what escapes before `Run` | `HandleException(E)`, in the `.dpr`'s `except` | CLI, Web |
| The action of the command reached, and its exit code | `SetAction(Action)` / `SetAction(Command, Action)` | CLI |
| `--version`, `--help`, invalid invocation | `RootCommand`, answered by the parse of what `B.ParseArgs(Args)` receives | CLI |
| The HTTP server, wired to DI and the host | `WebApp.AddHttpServer` + `MapControllers` | Web |
| Creating the main form and handing over the loop | `Run(TFormClass)` | VCL, FMX |
| A global for the code the platform builds itself | `DafApp` | VCL, FMX |

Two members of the .NET family are **not** ported, and they are declared rather than discovered:
`Metrics`, because DAF has no equivalent piece to point at, and `ConfigureContainer<T>`, because a
Delphi interface admits no generic methods — it is not rejected, it is unrepresentable. The options
of the default provider, which .NET sets through that member, are the builder's
`ServiceProviderOptions`.

---

## What this layer is, in .NET terms

Most DAF modules port something from `Microsoft.Extensions.*`, and this one seems to port nothing
— there is no `Microsoft.Extensions.Application`. The equivalent does exist, just elsewhere in the
same ecosystem: it is the **application-builder family built on top of the generic host** —
`HostApplicationBuilder`, `WebApplicationBuilder`, `MauiAppBuilder`. Each pairs a builder with a
built application object (`WebApplication`, `MauiApp`) that owns the host rather than being one.
Each variant's `CreateBuilder` and the `I…Application` it builds is that pair.

The family has four members where .NET has three, and the extra one is not an extra concept:
Delphi has two UI stacks, so desktop is VCL and FMX rather than a single entry.

Where it **diverges**, and it matters more than the resemblance:

- **Delphi has no extension methods, and in .NET the whole composition surface is extension
  methods.** `AddJsonFile`, `AddHostedService`, `UseSerilog`, `ConfigureKestrel` — all of them are
  extensions contributed by the package that owns the feature, which is how .NET lets a builder
  grow without the builder knowing about anything. Delphi has class helpers, but only **one** is
  in scope per type and none apply to interfaces, so the trick does not carry over. DAF composes
  through explicit facades instead — `JsonConfig.AddFile(Builder, …)` rather than
  `Builder.AddJsonFile(…)` — and through `IServiceCollection` being a record with real methods.
- **The default sources are those of .NET 10, with three differences.** The builder loads
  `appsettings.json`, `appsettings.{Environment}.json`, `{ApplicationName}.settings.json`,
  `{ApplicationName}.settings.{Environment}.json`, every environment variable and the command line,
  as `Host.CreateApplicationBuilder` does, and derives the environment and the content root the same
  way — the host's own variables carry `DAF_` where .NET's carry `DOTNET_`, so `DAF_ENVIRONMENT` is
  `environment`. What differs: there are no user secrets; the files are not reloaded when they
  change; and an environment variable with a connection-string prefix such as `SQLCONNSTR_` is not
  turned into a `ConnectionStrings:` key.
- **No logging provider by default.** .NET logs to the console, the debug output and the event
  source unless told otherwise; DAF adds none. Whoever uses `Logging` adds a provider, or
  `AddNullProvider` to declare the silence deliberate: with `Logging` used and no provider, `Build`
  fails with `ELoggingCompositionError`.
- **`Properties` is keyed by string**, `TDictionary<string, TValue>`, where .NET keys it by `object`.
- **Startup is synchronous throughout.** .NET's `RunAsync`, `StartAsync` and `StopAsync` have no
  counterpart here; `Run`, `Start` and `Stop` return when the work is done.

---

## See also

- 📖 [Hosting — Usage Guide](GUIDE.md) — the generic host the four variants are built on
- 📖 [Startup composition](../../DependencyInjection/docs/STARTUP-COMPOSITION.md) — the two phases
  and the ordering rules
