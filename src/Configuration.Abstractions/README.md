# Configuration.Abstractions

**🌍 Language: English | [Español](README.es.md)**

Core contracts for the DAF configuration system. This module defines **interfaces and types only** — no implementation. Reference it in libraries that need to read configuration without being bound to a specific provider.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

---

## What's in this module

All types live in `Daf.Extensions.Configuration`.

### Core interfaces

| Interface | Role |
|-----------|------|
| `IConfiguration` | Read/write key-value store with section navigation |
| `IConfigurationSection` | A named subtree of `IConfiguration` — adds `Key`, `Path`, `Value`, `HasChildren` |
| `IConfigurationRoot` | Root of the tree — adds `Reload` and `Providers` |
| `IConfigurationBuilder` | Fluent builder: add sources, then call `Build` |
| `IConfigurationProvider` | Single source implementation: `TryGet`, `Set`, `Load`, `GetChildKeys` |
| `IConfigurationSource` | Factory for a provider: `Build(Builder): IConfigurationProvider` |

### Key access

```pascal
Config['Database:Host']               // direct key
Config.GetSection('Database')['Host'] // via section
```

Key separator is `:`. Sections expose `GetChildren` to iterate sub-keys.

### `TConfigurationPath`

Utility class for path manipulation:

```pascal
TConfigurationPath.Combine('Database', 'Host')  // 'Database:Host'
TConfigurationPath.GetSectionKey('Database:Host') // 'Host'
TConfigurationPath.GetParentPath('Database:Host') // 'Database'
```

### `TConfigurationSourceOption`

```pascal
type TConfigurationSourceOption = (csoOptional, csoReloadOnChange);
```

---

## Dependencies

No dependencies on other DAF modules. Delphi RTL only.

The implementation is in [`Configuration`](../Configuration/README.md).
