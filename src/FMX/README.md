# Daf.FMX — What Leans on FireMonkey

**🌍 Language: English | [Español](README.es.md)**

The DAF implementations that need FireMonkey, kept in one package so that stack reaches only whoever asks for it. Today there is one: `TPlatformLogBootstrapSink`, the bootstrap-channel destination that writes through the platform's own log service.

**What this package is for: so that outside Windows the startup report does not get lost in silence.** It is **not** platform support. The two read alike and are not the same thing: a destination that asks the platform whether it can write fixes *knowing* when it cannot — it does not make DAF run outside Windows. The framework's declared targets are **Win32 and Win64**, and no project of it declares another.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

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
