# Daf.FMX — What Leans on FireMonkey

**🌍 Language: English | [Español](README.es.md)**

The DAF implementations that need FireMonkey, kept in one package so that stack reaches only whoever asks for it. There are **two**, and they are this package's two reasons to exist:

- `TPlatformLogBootstrapSink`, the bootstrap-channel destination that writes through the platform's own log service — so that **outside Windows the startup report does not get lost in silence**.
- `TFmxApplication`, the **FMX member of the application family**. It lives here and not in a package of its own because the invariant above — no other DAF package links FireMonkey — does not admit a second one that does.

**Neither of them is platform support.** That reading and this one look alike and are not the same thing: a destination that asks the platform whether it can write fixes *knowing* when it cannot, and **an FMX application variant does not make DAF run there either** — the variant creates the form and hands the loop to `FMX.Forms.Application`, which is a *lifecycle* contract, not a target list. The framework's declared targets are **Win32 and Win64**, and no project of it declares another.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

---

## The application variant

```pascal
uses Daf.FMX.Application;

var B := TFmxApplication.CreateBuilder;
B.Services.AddSingleton<IMyService, TMyService>;
var App := B.Build;
App.Run(TMainForm);          // creates the form, then FMX.Forms.Application takes over
```

`Run` here is **not** the CLI's `Run`. In CLI and Web, DAF owns the process: `Run` starts, works and stops, and the `.dpr` ends when it ends. Here the message loop already exists and belongs to `FMX.Forms.Application`; the variant creates the main form and delegates to it. Two lifecycle contracts, not four variations of one.

### The global, and why it exists here and not in CLI

`DafApp` returns the running application. It exists in VCL and FMX **only because the platform already has such a global and the user expects it** — denying it would force a worse mechanism for the form code the platform builds on its own, which nobody can inject into. In CLI and Web there is no such global, .NET has none either, and adding one would be a service locator with nothing to justify it.

Its name is deliberately **not** `Application`: in the same `.dpr` this one and the platform's coexist. **The preferred route is still injection** — `DafApp` is the way out for code the container never sees, not a way around the container.

---

## Registering the destination

The bootstrap channel takes its destinations before the host is built, and this one is not registered for you — referencing the package is how you ask for it:

```pascal
uses DAF.Extensions.Logging, Daf.FMX.BootstrapSink;

begin
  BootstrapLog.AddSink(TPlatformLogBootstrapSink.Create);
  // ... build the host, from here on a failure has somewhere to be reported
end.
```

Availability is answered by the platform, not by an `{$IFDEF}` someone keeps up to date by hand: `IsAvailable` asks `TPlatformServices` for `IFMXLoggingService` and reports what it got back. Where there is no such service the destination **declares itself unavailable**, instead of taking the report and losing it.

---

## Why a package of its own

`Daf.Logging.Abstractions` is the contract package: everyone uses it and its constraint is to depend on nothing, so FireMonkey would be worst placed there. `Daf.Logging` is required by whoever wants logging, not by whoever wants FireMonkey, so putting it there would drag FMX into every VCL application that uses the pipeline.

A boundary earns its keep when an optional capability drags a dependency the rest do not need. This one does, and the rule is checked rather than trusted: `eng/check-packages.py` fails the build if any package other than this one links `FMX.*`, whether by `requires` or by implicit import.

---

## Related

- [Logging.Abstractions](../Logging.Abstractions/README.md) — the `IBootstrapSink` contract and the bootstrap channel
- [Startup composition](../DependencyInjection/docs/STARTUP-COMPOSITION.md) — the destinations table and which phase reports what
