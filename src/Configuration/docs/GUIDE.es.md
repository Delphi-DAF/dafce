# Configuration — Guía de uso

**🌍 Idioma: [English](GUIDE.md) | Español**

---

## Contenido

1. [Construir una configuración](#construir-una-configuración)
2. [Leer valores](#leer-valores)
3. [Claves jerárquicas y secciones](#claves-jerárquicas-y-secciones)
4. [Proveedor JSON](#proveedor-json)
5. [Proveedor INI](#proveedor-ini)
6. [Proveedor de variables de entorno](#proveedor-de-variables-de-entorno)
7. [Proveedor en memoria](#proveedor-en-memoria)
8. [Proveedor encadenado](#proveedor-encadenado)
9. [Orden de capas y sobreescritura](#orden-de-capas-y-sobreescritura)
10. [Binding a objetos](#binding-a-objetos)
11. [Proveedores personalizados](#proveedores-personalizados)

---

## Construir una configuración

```pascal
uses
  Daf.Configuration.Builder,
  Daf.Extensions.Configuration;

var Config: IConfiguration := TConfigurationBuilder.Create
  .AddJsonFile('appsettings.json')
  .AddEnvironmentVariables
  .Build;
```

`TConfigurationBuilder.Build` devuelve un `IConfigurationRoot`, que también implementa `IConfiguration`.

---

## Leer valores

```pascal
// Acceso directo por clave (devuelve '' si no se encuentra)
var Value := Config['MiClave'];

// Clave jerárquica
var Conn := Config['Database:ConnectionString'];

// Sección y luego clave
var Port := Config.GetSection('Database')['Port'];

// Iterar hijos de una sección
for var Child in Config.GetSection('Logging').GetChildren do
  WriteLn(Child.Key, ' = ', Child.Value);
```

Las rutas de clave usan `:` como separador de jerarquía. Los arrays usan sufijos numéricos desde cero: `Plugins:0`, `Plugins:1`, etc.

---

## Claves jerárquicas y secciones

Dado este JSON:

```json
{
  "Database": {
    "Host": "localhost",
    "Port": 5432,
    "Name": "mydb"
  },
  "Logging": {
    "Level": "Information"
  }
}
```

```pascal
Config['Database:Host']                // 'localhost'
Config['Database:Port']                // '5432'
Config.GetSection('Database')['Name']  // 'mydb'

var Db := Config.GetSection('Database');
Db['Host']      // 'localhost'
Db.HasChildren  // True
Db.Key          // 'Database'
Db.Path         // 'Database'
```

---

## Proveedor JSON

```pascal
uses Daf.Configuration.Json;

Builder
  .AddJsonFile('appsettings.json')                              // requerido
  .AddJsonFile('appsettings.Development.json', {optional:} True) // opcional
```

Soporta objetos anidados y arrays. Las claves se aplanan con `:`.

---

## Proveedor INI

```pascal
uses Daf.Configuration.Ini;

Builder.AddIniFile('config.ini');
```

Formato INI:
```ini
[Database]
Host=localhost
Port=5432
```

Se accede como `Config['Database:Host']`.

---

## Proveedor de variables de entorno

```pascal
uses Daf.Configuration.Env;

Builder.AddEnvironmentVariables;               // todas las variables
Builder.AddEnvironmentVariables('MYAPP_');     // filtradas por prefijo (el prefijo se elimina)
```

Con el prefijo `MYAPP_`, la variable `MYAPP_DATABASE__HOST` se convierte en `Database:Host` (doble `__` → `:`).

---

## Proveedor en memoria

Útil para tests o bootstrap:

```pascal
uses Daf.Configuration.Memory;

Builder.AddInMemoryCollection([
  TPair<string,string>.Create('Database:Host', 'localhost'),
  TPair<string,string>.Create('Database:Port', '5432')
]);
```

---

## Proveedor encadenado

Envuelve un `IConfiguration` existente como fuente en un nuevo builder:

```pascal
uses Daf.Configuration.Chained;

var HostConfig: IConfiguration := ...; // de ConfigureHostConfiguration
var AppBuilder := TConfigurationBuilder.Create
  .AddConfiguration(HostConfig)   // encadenar config previa
  .AddJsonFile('appsettings.json')
  .Build;
```

Así es como `THostBuilder` comparte los ajustes del host con el pipeline de configuración de la app.

---

## Orden de capas y sobreescritura

Los proveedores se aplican **en orden de registro**. Un valor de un proveedor posterior sobreescribe la misma clave de uno anterior:

```pascal
Builder
  .AddJsonFile('appsettings.json')                         // valores base
  .AddJsonFile('appsettings.Development.json', True)       // sobreescrituras de entorno
  .AddEnvironmentVariables;                                // el SO sobreescribe todo
```

Para recargar todos los proveedores (p. ej., tras un cambio de fichero):

```pascal
(Config as IConfigurationRoot).Reload;
```

---

## Binding a objetos

`TConfigurationBinder` mapea las claves de una sección a los campos públicos de una clase mediante RTTI:

```pascal
uses Daf.Configuration.Binder;

type
  TDatabaseOptions = class
  public
    Host: string;
    Port: Integer;
    Name: string;
    MaxPoolSize: Integer;
  end;

var Opts := TDatabaseOptions.Create;
TConfigurationBinder.Bind(Config.GetSection('Database'), Opts);

WriteLn(Opts.Host);         // 'localhost'
WriteLn(Opts.Port);         // 5432
WriteLn(Opts.MaxPoolSize);  // 0 si no está en la config
```

La conversión de tipos desde `string` se gestiona automáticamente para `Integer`, `Boolean`, `Double` y `string`.

---

## Proveedores personalizados

Implementa `IConfigurationSource` e `IConfigurationProvider`:

```pascal
type
  TVaultSource = class(TInterfacedObject, IConfigurationSource)
  public
    function Build(const Builder: IConfigurationBuilder): IConfigurationProvider;
  end;

  TVaultProvider = class(TInterfacedObject, IConfigurationProvider)
  public
    function TryGet(const Key: string; out Value: string): Boolean;
    procedure &Set(const Key: string; const Value: string);
    procedure Load;
    function GetChildKeys(const EarlierKeys: IEnumerable<string>;
      const ParentPath: string): IEnumerable<string>;
  end;

function TVaultSource.Build(const Builder: IConfigurationBuilder): IConfigurationProvider;
begin
  Result := TVaultProvider.Create;
end;

// Registrar
Builder.Add(TVaultSource.Create);
```
