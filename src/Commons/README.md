# Commons

**🌍 Language: English | [Español](README.es.md)**

Cross-cutting utilities for DAFce applications — smart pointers, cancellation tokens, async futures, RTTI helpers, enumerable abstractions, arrays and more. This is the floor every other DAF package stands on.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Modules at a glance

| Unit | Highlights |
|------|-----------|
| `Daf.MemUtils` | `ARC<T>` smart pointer (`reference to function: T`), `TFinalizer`, `TPurgatory` |
| `Daf.Threading` | `ICancellationToken`, `ICancellationTokenSource` (factory: `CreateCancellationTokenSource`), `TShutdownHook` |
| `Daf.Enumerable` | `ToIEnumerable<T>` adapter, `IInterfaceList<T>`, `TInterfaceList<T>`, `TDeferredEnumerable<T>` / `TGeneratorEnumerator<T>` (the deferred sequence) |
| `Daf.Nullable` | `TNullable<T>` — an absent value told apart from a present default |
| `Daf.Comparers` | `TValueComparers`, `TNullableComparers` — NULL-aware ordering and equality |
| `Daf.Application.Version` | `TVersionInfo`, `VersionAttribute` — the application's SemVer version, and the platform it was compiled for |
| `Daf.Rtti` | RTTI helpers — `_T.Extends`, type predicates |
| `Daf.Activator` | Create class instances via RTTI |
| `Daf.Arrays` | `TArrayHelper` (`Map`, `Trim`), `TSmartArray<T>` (LINQ-style: `Where`, `Select`, `Contains`, `Sort`, …) |
| `Daf.FilterSpec` | `TFilterSpec<T>` — closure predicate, composable with `and` / `or` / `not` |
| `Daf.Types` | Common base types |

---

## Quick examples

### ARC smart pointer

```pascal
uses Daf.MemUtils;

var Ref := ARC.From<TMyObject>(TMyObject.Create);
Ref().DoWork;
// auto-freed when Ref goes out of scope
```

### Cancellation token

```pascal
uses Daf.Threading;

var Cts   := CreateCancellationTokenSource;
var Token := Cts.Token;

TThread.CreateAnonymousThread(procedure
begin
  while not Token.IsCancellationRequested do
    DoWork;
end).Start;

Sleep(5000);
Cts.Cancel;
```

---

## What belongs in Commons

One question decides it: **does this extend the runtime library, or is it an application utility?**

`Daf.MemUtils`, `Daf.Types`, `Daf.Threading`, `Daf.Enumerable`, `Daf.Rtti`, `Daf.Arrays`,
`Daf.Comparers` and `Daf.Nullable` are things Delphi arguably should ship and does not. Every other
package stands on this one, so anything that is *used by* an application rather than *missing from*
the language belongs elsewhere — otherwise whoever wants a smart pointer ends up linking a process
launcher too.

Two units moved out under that rule. `Daf.SystemProcess` kept its unit and type names, so adding a
reference to the new package is enough for existing `uses` clauses to keep compiling.
`Daf.CmdLn.Parser` went out too and has since been **withdrawn**: the command line is
[`Daf.CommandLine`](../CommandLine/README.md), a port of .NET's `System.CommandLine`.

| Moved to | What it does |
|---|---|
| [`Daf.SystemProcess`](../SystemProcess/README.md) | `TSystemProcess` — async external process runner |

---

## Documentation

- 📖 [Usage Guide](docs/GUIDE.md) — full API reference for all units

