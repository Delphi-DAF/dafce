# Configuration.Abstractions

**🌍 Idioma: [English](README.md) | Español**

Contratos principales del sistema de configuración de DAF. Este módulo define **solo interfaces y tipos** — sin implementación. Referencíalo en librerías que necesiten leer configuración sin depender de un proveedor concreto.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![Licencia](https://img.shields.io/badge/licencia-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Qué hay en este módulo

**Dos units, y cuál declara cada tipo es lo que va en tu `uses`:**

| Unit | Qué declara |
|---|---|
| `Daf.Extensions.Configuration` | los contratos de abajo, `TConfigurationPath`, `TConfigurationSourceOption` y `ConfigurationKeyName` |
| `Daf.Configuration.Binder` | `TConfigurationBinder` y `TBindOption` |

### Interfaces principales

| Interfaz | Rol |
|----------|-----|
| `IConfiguration` | Almacén clave-valor con navegación por secciones; `Exists` y `Find` contestan la presencia |
| `IConfigurationSection` | Subárbol nombrado de `IConfiguration` — añade `Key`, `Path`, `Value`, `HasChildren` |
| `IConfigurationRoot` | Raíz del árbol — añade `Reload` y `Providers` |
| `IConfigurationBuilder` | Constructor fluido: añade fuentes y llama a `Build` |
| `IConfigurationProvider` | Implementación de una fuente: `TryGet`, `Set`, `Load`, `GetChildKeys` |
| `IConfigurationSource` | Factoría de un proveedor: `Build(Builder): IConfigurationProvider` |
| `IConfigurationManager` | Constructor **y** lectura sobre un solo objeto: se lee mientras se está construyendo |

### Acceso a claves

```pascal
Config['Database:Host']                // clave directa
Config.GetSection('Database')['Host']  // via sección
```

El separador de claves es `:`. Las secciones exponen `GetChildren` para iterar las sub-claves.

### Enlazar un subárbol sobre un objeto

```pascal
uses Daf.Configuration.Binder;

var Settings := TConfigurationBinder.Bind<TAppSettings>(Config, 'AppSettings');
```

`Bind<T>` construye la instancia y la devuelve, y **el dueño es quien llama**. La sobrecarga que
recibe un `Instance: TObject` rellena uno que ya existe. `TBindOption` dice qué se rellena:
`boProperties`, que es el valor por defecto, o `boFields`.

Para enlazar basta este paquete: el enlazador trabaja sobre cualquier `IConfiguration` y no sabe
nada de JSON, INI ni del entorno. Qué hace con una clave ausente, objetos anidados, colecciones y
un miembro que no puede convertir está en la [guía de `Configuration`](../Configuration/docs/GUIDE.es.md).

### `TConfigurationPath`

Clase de utilidades para manipulación de rutas:

```pascal
TConfigurationPath.Combine(['Database', 'Host'])  // 'Database:Host'
TConfigurationPath.GetSectionKey('Database:Host') // 'Host'
TConfigurationPath.GetParentPath('Database:Host') // 'Database'
```

### `TConfigurationSourceOption`

```pascal
type
  TConfigurationSourceOption  = (csoOptional);
  TConfigurationSourceOptions = set of TConfigurationSourceOption;
```

Lo que recibe una fuente es el **conjunto**, y `csoOptional` es el valor que cambia lo que pasa: sin
él, un fichero que no está aborta la carga con `File "<nombre>" not Found`; con él, la fuente avisa y
no aporta nada.

### `ConfigurationKeyName`

```pascal
type
  TDbSettings = class
  private
    FConnectionString: string;
  public
    [ConfigurationKeyName('connection-string')]
    property ConnectionString: string read FConnectionString write FConnectionString;
  end;
```

El atributo nombra la clave por la que se lee un miembro, así que una clave que el fichero escribe
como `connection-string` llega a una propiedad que Delphi no puede llamar así. **Sustituye** al
nombre del miembro: puesto el atributo, un valor bajo `ConnectionString` ya no responde por ese
miembro. Sobre un miembro de tipo objeto renombra la sección entera. Vale igual con `boProperties`
que con `boFields`, donde .NET lo declara solo para propiedades.

---

## Dependencias

Solo `Daf.Commons`, por el `TNullable<T>` que devuelve `Find` y el RTTI con el que el enlazador recorre los tipos.

La implementación está en [`Configuration`](../Configuration/README.es.md).
