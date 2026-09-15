# Configuration.Abstractions

**🌍 Language: English | [Español](README.es.md)**

Core contracts for the DAF configuration system. This module defines **interfaces and types only** — no implementation. Reference it in libraries that need to read configuration without being bound to a specific provider.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

---

## What's in this module

**Two units, and which one declares a type is what goes in your `uses`:**

| Unit | What it declares |
|---|---|
| `Daf.Extensions.Configuration` | the contracts below, `TConfigurationPath` and `TConfigurationSourceOption` |
| `Daf.Configuration.Binder` | `TConfigurationBinder` and `TBindOption` |

### Core interfaces

| Interface | Role |
|-----------|------|
| `IConfiguration` | Read/write key-value store with section navigation; `Exists` and `Find` answer presence |
| `IConfigurationSection` | A named subtree of `IConfiguration` — adds `Key`, `Path`, `Value`, `HasChildren` |
| `IConfigurationRoot` | Root of the tree — adds `Reload` and `Providers` |
| `IConfigurationBuilder` | Fluent builder: add sources, then call `Build` |
| `IConfigurationProvider` | Single source implementation: `TryGet`, `Set`, `Load`, `GetChildKeys` |
| `IConfigurationSource` | Factory for a provider: `Build(Builder): IConfigurationProvider` |
| `IConfigurationManager` | Builder **and** reader on one object: it is read while it is still being built |

### Key access

```pascal
Config['Database:Host']               // direct key
Config.GetSection('Database')['Host'] // via section
```

Key separator is `:`. Sections expose `GetChildren` to iterate sub-keys.

### Binding a subtree onto an object

```pascal
uses Daf.Configuration.Binder;

var Settings := TConfigurationBinder.Bind<TAppSettings>(Config, 'AppSettings');
```

`Bind<T>` constructs the instance and returns it, and **the caller owns it**. The overload taking an
`Instance: TObject` fills one that already exists. `TBindOption` says what gets filled:
`boProperties`, the default, or `boFields`.

Binding needs this package and nothing else: the binder works over any `IConfiguration` and knows
nothing about JSON, INI or the environment. What it does with nested objects, collections and a
member it cannot convert is in the [`Configuration` guide](../Configuration/docs/GUIDE.md).

### `TConfigurationPath`

Utility class for path manipulation:

```pascal
TConfigurationPath.Combine(['Database', 'Host']) // 'Database:Host'
TConfigurationPath.GetSectionKey('Database:Host') // 'Host'
TConfigurationPath.GetParentPath('Database:Host') // 'Database'
```

### `TConfigurationSourceOption`

```pascal
type
  TConfigurationSourceOption  = (csoOptional, csoReloadOnChange);
  TConfigurationSourceOptions = set of TConfigurationSourceOption;
```

What a source takes is the **set**, and `csoOptional` is the value that changes what happens: without
it, a file that is not there aborts the load with `File "<name>" not Found`; with it, the source
warns and contributes nothing. No provider reads `csoReloadOnChange`.

---

## Dependencies

`Daf.Commons` only, for the `TNullable<T>` that `Find` returns and the RTTI the binder walks types with.

The implementation is in [`Configuration`](../Configuration/README.md).
