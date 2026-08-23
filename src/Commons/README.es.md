# Commons

**🌍 Idioma: [English](README.md) | Español**

Utilidades transversales para aplicaciones DAFce — punteros inteligentes, tokens de cancelación, futuros asíncronos, helpers de RTTI, abstracciones enumerables, arrays y más. Es el suelo que pisan todos los demás paquetes de DAF.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![Licencia](https://img.shields.io/badge/licencia-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Módulos de un vistazo

| Unidad | Aspectos destacados |
|--------|-------------------|
| `Daf.MemUtils` | `ARC<T>` smart pointer (`reference to function: T`), `TFinalizer`, `TPurgatory` |
| `Daf.Threading` | `ICancellationToken`, `ICancellationTokenSource` (fábrica: `CreateCancellationTokenSource`), `TShutdownHook` |
| `Daf.Enumerable` | Adaptador `ToIEnumerable<T>`, `IInterfaceList<T>`, `TInterfaceList<T>` |
| `Daf.Rtti` | Helpers de RTTI — `_T.Extends`, predicados de tipo |
| `Daf.Activator` | Crear instancias de clases mediante RTTI |
| `Daf.Arrays` | `TArrayHelper` (`Map`, `Trim`), `TSmartArray<T>` (estilo LINQ: `Where`, `Select`, `Contains`, `Sort`, …) |
| `Daf.FilterSpec` | `TFilterSpec<T>` — predicado por clausuras, componible con `and` / `or` / `not` |
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

---

## Qué se gana el sitio en Commons

Lo decide una pregunta: **¿esto extiende la biblioteca estándar, o es una utilidad de aplicación?**

`Daf.MemUtils`, `Daf.Types`, `Daf.Threading`, `Daf.Enumerable`, `Daf.Rtti`, `Daf.Arrays`,
`Daf.Comparers` y `Daf.Nullable` son cosas que Delphi debería traer y no trae. Todos los demás
paquetes pisan éste, así que lo que una aplicación *usa* —en vez de lo que al lenguaje le *falta*—
va en otro sitio; si no, quien quiere un puntero inteligente acaba enlazando un lanzador de procesos.

Dos units salieron por esa regla. Ni sus nombres ni los de sus tipos han cambiado: basta añadir una
referencia al paquete nuevo y las cláusulas `uses` existentes siguen compilando.

| Se mudó a | Qué hace |
|---|---|
| [`Daf.SystemProcess`](../SystemProcess/README.es.md) | `TSystemProcess` — runner de procesos externos asíncrono |
| [`Daf.CmdLn`](../CmdLn/README.es.md) | `Daf.CmdLn.Parser` — gramática de línea de comandos |

---

## Documentación

- 📖 [Guía de uso](docs/GUIDE.es.md) — referencia de API completa para todas las unidades
