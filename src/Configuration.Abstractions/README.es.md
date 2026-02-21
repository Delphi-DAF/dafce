# Configuration.Abstractions

**🌍 Idioma: [English](README.md) | Español**

Contratos principales del sistema de configuración de DAF. Este módulo define **solo interfaces y tipos** — sin implementación. Referencíalo en librerías que necesiten leer configuración sin depender de un proveedor concreto.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![Licencia](https://img.shields.io/badge/licencia-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Qué hay en este módulo

Todos los tipos se encuentran en `Daf.Extensions.Configuration`.

### Interfaces principales

| Interfaz | Rol |
|----------|-----|
| `IConfiguration` | Almacén clave-valor con navegación por secciones |
| `IConfigurationSection` | Subárbol nombrado de `IConfiguration` — añade `Key`, `Path`, `Value`, `HasChildren` |
| `IConfigurationRoot` | Raíz del árbol — añade `Reload` y `Providers` |
| `IConfigurationBuilder` | Constructor fluido: añade fuentes y llama a `Build` |
| `IConfigurationProvider` | Implementación de una fuente: `TryGet`, `Set`, `Load`, `GetChildKeys` |
| `IConfigurationSource` | Factoría de un proveedor: `Build(Builder): IConfigurationProvider` |

### Acceso a claves

```pascal
Config['Database:Host']                // clave directa
Config.GetSection('Database')['Host']  // via sección
```

El separador de claves es `:`. Las secciones exponen `GetChildren` para iterar las sub-claves.

### `TConfigurationPath`

Clase de utilidades para manipulación de rutas:

```pascal
TConfigurationPath.Combine('Database', 'Host')    // 'Database:Host'
TConfigurationPath.GetSectionKey('Database:Host') // 'Host'
TConfigurationPath.GetParentPath('Database:Host') // 'Database'
```

### `TConfigurationSourceOption`

```pascal
type TConfigurationSourceOption = (csoOptional, csoReloadOnChange);
```

---

## Dependencias

Sin dependencias de otros módulos DAF. Solo RTL de Delphi.

La implementación está en [`Configuration`](../Configuration/README.es.md).
