# Configuration — Usage Guide

**🌍 Language: English | [Español](GUIDE.es.md)**

---

## Contents

1. [Building a Configuration](#building-a-configuration)
2. [Reading Values](#reading-values)
3. [Telling an absent key from an empty one](#telling-an-absent-key-from-an-empty-one)
4. [Hierarchical Keys and Sections](#hierarchical-keys-and-sections)
5. [JSON Provider](#json-provider)
6. [INI Provider](#ini-provider)
7. [Environment Variables Provider](#environment-variables-provider)
8. [In-Memory Provider](#in-memory-provider)
9. [Chained Provider](#chained-provider)
10. [Layering and Override Order](#layering-and-override-order)
11. [Binding to Objects](#binding-to-objects)
12. [Custom Providers](#custom-providers)

---

## Building a Configuration

```pascal
uses
  Daf.Configuration.Builder,
  Daf.Configuration.Json,
  Daf.Configuration.Env,
  Daf.Extensions.Configuration;

var Builder: IConfigurationBuilder := TConfigurationBuilder.Create;

JsonConfig.AddFile(Builder, 'appsettings.json');
Builder.Add(TEnvironmentVariablesConfigurationSource.Create);

var Config: IConfiguration := Builder.Build;
```

⚠️ **Hold the builder in an `IConfigurationBuilder` variable.** `TConfigurationBuilder` is reference
counted, so chaining straight off `TConfigurationBuilder.Create` leaves nobody owning it and the
builder leaks.

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

## Telling an absent key from an empty one

`IConfiguration` answers presence with two members that .NET does not have, because **there the
distinction is carried by nullability** — the indexer is `string? this[key]`,
with `null` for absent and `''` for present-but-empty. A Delphi string cannot be nil, so what the
`?` does there needs a member of its own here.

```pascal
if Config.Exists('Features:Beta') then ;              // presence, and nothing else
var Read := Config.Find('Features:Beta');             // presence AND value, one lookup
if Read.HasValue and (Read.Value = 'on') then ;
var Value := Config['Features:Beta'];                 // '' when absent AND when empty
```

`Exists` is literal parity with .NET's `IConfigurationSection.Exists()`, **section semantics
included**: a section exists when it has a value of its own **or** has keys underneath. A section
that only groups — the one holding `Logging:Level` and its siblings — has no value of its own, and
answering that it is not there while its children are being read is a false answer about the shape
of the document.

**The indexer returns a `string`**, so `C['absent'] = ''` and `C['empty'] = ''` are both `True`.
Having to name `Find` is what closes the hole — the presence can only be read by asking the type
for it.

This matters most while a configuration is being **composed**, because a key read before the source
that carries it was added looks exactly like a key whose value is empty — it compiles, it runs, it
raises nothing, and the feature never turns on.

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

JsonConfig.AddFile(Builder, 'appsettings.json');                          // required
JsonConfig.AddFile(Builder, 'appsettings.Development.json', [csoOptional]);
```

Supports nested objects and arrays. Keys are flattened with `:`.

---

## INI Provider

```pascal
uses Daf.Configuration.Ini;

IniConfig.AddFile(Builder, 'config.ini');
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

Builder.Add(TEnvironmentVariablesConfigurationSource.Create);           // all variables
Builder.Add(TEnvironmentVariablesConfigurationSource.Create('MYAPP_'));  // by prefix (prefix stripped)
```

`Daf.Configuration.Env` also declares `AddEnvironmentVariables` as a class helper on
`TConfigurationBuilder`. It only applies when the builder is held as the **class**, not as
`IConfigurationBuilder` — and holding it as the class is what leaks it, so prefer the source form
above.

With prefix `MYAPP_`, the variable `MYAPP_DATABASE__HOST` becomes `Database:Host` (double `__` → `:`).

---

## In-Memory Provider

Useful for tests or bootstrapping:

```pascal
uses Daf.Configuration.Memory;

var Data := TDictionary<string, string>.Create;   // the source takes ownership
Data.Add('Database:Host', 'localhost');
Data.Add('Database:Port', '5432');

MemoryConfig.AddCollection(Builder, Data);
```

---

## Chained Provider

Wrap an `IConfiguration` **that already exists** as a source of a new builder. It is the only way
to seed a builder from a configuration you were handed: the in-memory source takes a dictionary of
strings, not an `IConfiguration`.

```pascal
uses Daf.Extensions.Configuration, Daf.Configuration.Builder, Daf.Configuration.Chained,
     Daf.Configuration.Json;

// A configuration somebody else resolved and handed to you -- a plugin host, a test harness,
// a library that composes its own defaults.
var Given: IConfiguration := HostedBy.Configuration;

var Builder: IConfigurationBuilder := TConfigurationBuilder.Create;
AddChainedConfiguration(Builder, Given);              // what you were given, first
JsonConfig.AddFile(Builder, 'appsettings.json');      // your own, on top

var Config := Builder.Build;
```

**Precedence is decided by whoever composes**, not by the chained source: it claims none of its
own, so it wins or loses purely by where it was added. And chaining an unassigned configuration
fails on the spot rather than in silence — it is a decision of the composer, not an input.

A key whose value is the **empty string** survives the chain: the source reports presence by asking
the wrapped configuration, not by inferring it from the value not being empty. That distinction is
the one the [presence query](#telling-an-absent-key-from-an-empty-one) restored.

> This provider no longer has a caller inside the framework, and that is a change of role, not of
> behaviour. It used to be the seam joining the host configuration to the application one; with a
> single builder style there is a single configuration and nothing to join. It stays because it has
> no substitute, and .NET keeps it public for the same reason.

## Layering and Override Order

Providers are applied **in registration order**. A value from a later provider overrides the same key from an earlier one:

```pascal
JsonConfig.AddFile(Builder, 'appsettings.json');                            // base values
JsonConfig.AddFile(Builder, 'appsettings.Development.json', [csoOptional]); // environment overrides
Builder.Add(TEnvironmentVariablesConfigurationSource.Create);               // OS overrides those
CommandLineConfig.AddArgs(Builder);                                         // and the command line wins
```

To reload all providers (e.g., after a file change):

```pascal
(Config as IConfigurationRoot).Reload;
```

---

## Binding to Objects

`TConfigurationBinder` maps a section's keys onto a class through RTTI. Which members it writes
is the `TBindOption` argument: `boProperties`, the default, reads published properties;
`boFields`, public fields — which is what this example has:

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

// The generic overload builds the object and hands it over; the caller frees it.
var Opts := TConfigurationBinder.Bind<TDatabaseOptions>(Config, 'Database', boFields);

// To fill an instance you already have, the other overload takes all four arguments:
//   TConfigurationBinder.Bind(Config, 'Database', Opts, boFields);

WriteLn(Opts.Host);         // 'localhost'
WriteLn(Opts.Port);         // 5432
WriteLn(Opts.MaxPoolSize);  // 0 if not present in config
```

Type conversion from `string` is handled automatically for `Integer`, `Boolean`, `Double`, and `string`.

### The settings class frees its object members

A member of object type is **built** by the binder, recursively — that is what makes it useful to
nest a section inside another — and whoever builds it does not keep it. So the rule is: **the
settings class frees what is its own.**

```pascal
type
  TDbSection = class
  public
    Server: string;
    Port: Integer;
  end;

  TAppConfig = class
  public
    Name: string;
    Db: TDbSection;
    destructor Destroy; override;   // <- and this is not optional
  end;

destructor TAppConfig.Destroy;
begin
  Db.Free;
  inherited;
end;
```

Without that destructor the nested object leaks, silently and whoever owns the root.

Two more things about nested binding, worth knowing before being surprised by them:

- binding the same section separately gives an **independent instance**, not the one nested inside
  the parent — the same as in .NET
- the binder **reuses** a member object that the class already built in its constructor, rather than
  replacing it, so building it there and freeing it in the destructor is the shape that works

### Handing the section over as settings with an owner

Inside a container, the caller does not have to own the object at all: `ConfigOptions.Bind` gives
the container the job of building it, owning it and freeing it, and the service asks for its
settings by constructor. See [the module README](../README.md).

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
