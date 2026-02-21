# Configuration — Usage Guide

**🌍 Language: English | [Español](GUIDE.es.md)**

---

## Contents

1. [Building a Configuration](#building-a-configuration)
2. [Reading Values](#reading-values)
3. [Hierarchical Keys and Sections](#hierarchical-keys-and-sections)
4. [JSON Provider](#json-provider)
5. [INI Provider](#ini-provider)
6. [Environment Variables Provider](#environment-variables-provider)
7. [In-Memory Provider](#in-memory-provider)
8. [Chained Provider](#chained-provider)
9. [Layering and Override Order](#layering-and-override-order)
10. [Binding to Objects](#binding-to-objects)
11. [Custom Providers](#custom-providers)

---

## Building a Configuration

```pascal
uses
  Daf.Configuration.Builder,
  Daf.Extensions.Configuration;

var Config: IConfiguration := TConfigurationBuilder.Create
  .AddJsonFile('appsettings.json')
  .AddEnvironmentVariables
  .Build;
```

`TConfigurationBuilder.Build` returns an `IConfigurationRoot`, which is also an `IConfiguration`.

---

## Reading Values

```pascal
// Direct key access (returns '' if not found)
var Value := Config['MyKey'];

// Hierarchical key
var Conn := Config['Database:ConnectionString'];

// Section then key
var Port := Config.GetSection('Database')['Port'];

// Iterate children of a section
for var Child in Config.GetSection('Logging').GetChildren do
  WriteLn(Child.Key, ' = ', Child.Value);
```

Key paths use `:` as the hierarchy separator. Arrays use zero-based numeric suffixes: `Plugins:0`, `Plugins:1`, etc.

---

## Hierarchical Keys and Sections

Given this JSON:

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
Config['Database:Host']              // 'localhost'
Config['Database:Port']              // '5432'
Config.GetSection('Database')['Name'] // 'mydb'

var Db := Config.GetSection('Database');
Db['Host']   // 'localhost'
Db.HasChildren  // True
Db.Key          // 'Database'
Db.Path         // 'Database'
```

---

## JSON Provider

```pascal
uses Daf.Configuration.Json;

Builder
  .AddJsonFile('appsettings.json')                         // required
  .AddJsonFile('appsettings.Development.json', {optional:} True) // optional
```

Supports nested objects and arrays. Keys are flattened with `:`.

---

## INI Provider

```pascal
uses Daf.Configuration.Ini;

Builder.AddIniFile('config.ini');
```

INI format:
```ini
[Database]
Host=localhost
Port=5432
```

Accessed as `Config['Database:Host']`.

---

## Environment Variables Provider

```pascal
uses Daf.Configuration.Env;

Builder.AddEnvironmentVariables;                      // all variables
Builder.AddEnvironmentVariables('MYAPP_');            // filtered by prefix (prefix stripped)
```

With prefix `MYAPP_`, the variable `MYAPP_DATABASE__HOST` becomes `Database:Host` (double `__` → `:`).

---

## In-Memory Provider

Useful for tests or bootstrapping:

```pascal
uses Daf.Configuration.Memory;

Builder.AddInMemoryCollection([
  TPair<string,string>.Create('Database:Host', 'localhost'),
  TPair<string,string>.Create('Database:Port', '5432')
]);
```

---

## Chained Provider

Wrap an existing `IConfiguration` as a source in a new builder:

```pascal
uses Daf.Configuration.Chained;

var HostConfig: IConfiguration := ...; // from ConfigureHostConfiguration
var AppBuilder := TConfigurationBuilder.Create
  .AddConfiguration(HostConfig)   // chain previous config
  .AddJsonFile('appsettings.json')
  .Build;
```

This is how `THostBuilder` shares host-level settings into the app configuration pipeline.

---

## Layering and Override Order

Providers are applied **in registration order**. A value from a later provider overrides the same key from an earlier one:

```pascal
Builder
  .AddJsonFile('appsettings.json')           // base values
  .AddJsonFile('appsettings.Development.json', True)  // environment overrides
  .AddEnvironmentVariables;                  // OS overrides everything
```

To reload all providers (e.g., after a file change):

```pascal
(Config as IConfigurationRoot).Reload;
```

---

## Binding to Objects

`TConfigurationBinder` maps a section's keys to a class's published/public fields using RTTI:

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
WriteLn(Opts.MaxPoolSize);  // 0 if not present in config
```

Type conversion from `string` is handled automatically for `Integer`, `Boolean`, `Double`, and `string`.

---

## Custom Providers

Implement `IConfigurationSource` and `IConfigurationProvider`:

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

// Register
Builder.Add(TVaultSource.Create);
```
