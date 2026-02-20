# Commons

**🌍 Language: English | [Español](README.es.md)**

Cross-cutting utilities for DAFce applications — smart pointers, cancellation tokens, async futures, process execution, RTTI helpers, enumerable abstractions, command-line parsing, and more.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Modules at a glance

| Unit | Highlights |
|------|-----------|
| `Daf.MemUtils` | `ARC<T>` auto-ref-count smart pointer, `WRef<T>` weak reference |
| `Daf.Threading` | `ICancellationToken`, `ICancellationTokenSource`, `IFuture<T>`, `TShutdownHook` |
| `Daf.Enumerable` | `IEnumerable<T>`, `IInterfaceList<T>`, `TOrderedDictionary` |
| `Daf.SystemProcess` | `TSystemProcess` — async external process runner with events |
| `Daf.CmdLn.Parser` | Command-line argument parser |
| `Daf.Rtti` | RTTI helpers — `_T.Extends`, type predicates |
| `Daf.Activator` | Create class instances via RTTI |
| `Daf.Arrays` | `TArray` extension helpers (IndexOf, etc.) |
| `Daf.Expression` | Simple expression evaluator |
| `Daf.Types` | Common base types |

---

## Quick examples

### ARC smart pointer

```pascal
uses Daf.MemUtils;

var Ref := ARC<TMyObject>.Create(TMyObject.Create);
Ref.Value.DoWork;
// auto-freed when Ref goes out of scope
```

### Cancellation token

```pascal
uses Daf.Threading;

var Cts := TCancellationTokenSource.Create;
var Token := Cts.Token;

TThread.CreateAnonymousThread(procedure
begin
  while not Token.IsCancellationRequested do
    DoWork;
end).Start;

Sleep(5000);
Cts.Cancel;
```

### External process (async)

```pascal
uses Daf.SystemProcess;

var Process := TSystemProcess.Builder
  .Command('git')
  .CmdArgs(['--version'])
  .OnStdOut(procedure(Line: string) begin WriteLn(Line); end)
  .OnCompleted(procedure(R: TProcessResult) begin WriteLn('Done, exit=', R.ExitCode); end)
  .Build;

Process.ExecuteAsync;
```

### Future

```pascal
uses Daf.Threading;

var Future: IFuture<Integer> := TFuture<Integer>.Run(
  function: Integer begin Result := HeavyComputation; end);

WriteLn('Result: ', Future.Value);
```

---

## Documentation

- 📖 [Usage Guide](docs/GUIDE.md) — full API reference for all units

