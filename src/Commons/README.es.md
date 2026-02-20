# Commons

**🌍 Idioma: [English](README.md) | Español**

Utilidades transversales para aplicaciones DAFce — punteros inteligentes, tokens de cancelación, futuros asíncronos, ejecución de procesos, helpers de RTTI, abstracciones enumerables, parsing de línea de comandos y más.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![Licencia](https://img.shields.io/badge/licencia-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Módulos de un vistazo

| Unidad | Aspectos destacados |
|--------|-------------------|
| `Daf.MemUtils` | `ARC<T>` smart pointer (`reference to function: T`), `TFinalizer`, `TPurgatory` |
| `Daf.Threading` | `ICancellationToken`, `ICancellationTokenSource` (fábrica: `CreateCancellationTokenSource`), `TShutdownHook` |
| `Daf.Enumerable` | Adaptador `ToIEnumerable<T>`, `IInterfaceList<T>`, `TInterfaceList<T>` |
| `Daf.SystemProcess` | `TSystemProcess` — runner de procesos externos asíncrono con eventos |
| `Daf.CmdLn.Parser` | Parser de argumentos de línea de comandos |
| `Daf.Rtti` | Helpers de RTTI — `_T.Extends`, predicados de tipo |
| `Daf.Activator` | Crear instancias de clases mediante RTTI |
| `Daf.Arrays` | `TArrayHelper` (`Map`, `Trim`), `TSmartArray<T>` (estilo LINQ: `Where`, `Select`, `Contains`, `Sort`, …) |
| `Daf.Expression` | Evaluador de expresiones simple |
| `Daf.Types` | Tipos base comunes |

---

## Ejemplos rápidos

### Puntero inteligente ARC

```pascal
uses Daf.MemUtils;

var Ref := ARC.From<TMyObject>(TMyObject.Create);
Ref().DoWork;
// liberado automáticamente cuando Ref sale del scope
```

### Token de cancelación

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

### Proceso externo (asíncrono)

```pascal
uses Daf.SystemProcess;

var Process := TSystemProcess.Builder
  .Command('git')
  .CmdArgs(['--version'])
  .OnStdOut(procedure(Line: string) begin WriteLn(Line); end)
  .OnCompleted(procedure(R: TProcessResult) begin WriteLn('Hecho, exit=', R.ExitCode); end)
  .Build;

Process.ExecuteAsync;
```

---

## Documentación

- 📖 [Guía de uso](docs/GUIDE.es.md) — referencia de API completa para todas las unidades
