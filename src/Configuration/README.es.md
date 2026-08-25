# Configuration

**🌍 Idioma: [English](README.md) | Español**

Sistema de configuración por capas para Delphi inspirado en `Microsoft.Extensions.Configuration` de .NET. Lee ajustes desde ficheros JSON, INI, variables de entorno, diccionarios en memoria o fuentes encadenadas — todo unificado detrás de una única interfaz `IConfiguration`.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![Licencia](https://img.shields.io/badge/licencia-MIT-blue.svg)](../../legal/LICENSE.md)

---

## ¿Por qué usarlo?

- 📄 **Múltiples fuentes** — JSON, INI, variables de entorno, en memoria o personalizadas
- 🔗 **Con capas y sobreescribibles** — las fuentes se encadenan; las últimas sobreescriben a las anteriores
- 🗂️ **Claves jerárquicas** — usa `:` como separador (`Database:ConnectionString`)
- 🔄 **Soporte de recarga** — `IConfigurationRoot.Reload` relee todos los proveedores
- 🎯 **Binder** — mapea una sección de configuración directamente a una clase Delphi

---

## Inicio rápido

```pascal
uses
  Daf.Configuration.Builder,
  Daf.Configuration.Json,
  Daf.Configuration.Env,
  Daf.Extensions.Configuration;

var Builder: IConfigurationBuilder := TConfigurationBuilder.Create;

JsonConfig.AddFile(Builder, 'appsettings.json');
Builder.Add(TEnvironmentVariablesConfigurationSource.Create);

var Config := Builder.Build;

// Leer un valor
var ConnStr := Config['Database:ConnectionString'];

// Leer una sección
var DbSection := Config.GetSection('Database');
var Host := DbSection['Host'];
```

---

## Proveedores disponibles

| Proveedor | Unidad | Descripción |
|-----------|--------|-------------|
| Fichero JSON | `Daf.Configuration.Json` | `appsettings.json`, opcional/requerido |
| Fichero INI | `Daf.Configuration.Ini` | Formato clásico `[Sección]\nClave=Valor` |
| Variables de entorno | `Daf.Configuration.Env` | Variables del SO, opcionalmente filtradas por prefijo |
| En memoria | `Daf.Configuration.Memory` | Diccionario de pares `string → string` |
| Línea de comandos | `Daf.Configuration.CommandLine` | Argumentos del proceso, con la sintaxis de .NET y alias cortos |
| Encadenado | `Daf.Configuration.Chained` | Envuelve un `IConfiguration` existente como fuente |

Los cuatro primeros son el juego de .NET: `Json`, `Ini`, `EnvironmentVariables` y `CommandLine`
de `Microsoft.Extensions.Configuration`. **La precedencia es el orden en que los añades: gana la
última fuente.** Ningún proveedor se arroga una precedencia propia.

---

## La línea de comandos

La fuente de mayor precedencia, y la que no necesita ningún fichero: permite anular cualquier ajuste
al invocar el ejecutable.

```pascal
uses
  Daf.Configuration.Builder,
  Daf.Configuration.Json,
  Daf.Configuration.CommandLine,
  Daf.Extensions.Configuration;

var Builder: IConfigurationBuilder := TConfigurationBuilder.Create;

JsonConfig.AddFile(Builder, 'appsettings.json');
CommandLineConfig.AddArgs(Builder);   // añadida última, así que anula al fichero

var Config := Builder.Build;
```

`AddArgs` sin argumentos lee los que recibió este proceso. La sobrecarga que toma un array explícito
es lo que hace verificable la fuente — un proceso no puede falsear su propio `ParamStr`:

```pascal
CommandLineConfig.AddArgs(Builder, ['--Logging:LogLevel:Default=Debug']);
```

### Formas admitidas

| Forma | Ejemplo |
|---|---|
| `clave=valor` | `Logging:LogLevel:Default=Debug` |
| `--clave=valor` | `--Logging:LogLevel:Default=Debug` |
| `--clave valor` | `--Logging:LogLevel:Default Debug` |
| `/clave=valor` | `/Logging:LogLevel:Default=Debug` |
| `/clave valor` | `/Logging:LogLevel:Default Debug` |
| `-alias valor` | solo si el alias está declarado |

Las claves no distinguen mayúsculas, una clave repetida se queda con su último valor, y `clave=` sin
nada detrás del igual es una clave cuyo valor es la cadena vacía. Las claves anidadas usan el mismo
separador que las demás fuentes, así que un valor puesto aquí y uno leído de `appsettings.json`
aterrizan en la misma clave.

### Alias cortos

Un alias hace corresponder un modificador corto con una clave larga. Debe empezar por uno o dos
guiones, y declarar el mismo alias dos veces es un error de composición — se avisa al montar la
fuente, no al ejecutar el proceso.

```pascal
CommandLineConfig.AddArgs(Builder, ['-v', 'Debug'],
  [TSwitchMapping.Create('-v', 'Logging:LogLevel:Default')]);
```

⚠️ **Una diferencia deliberada con .NET.** Allí, un modificador de un solo guion sin alias declarado
y con signo igual —`-x=1`— lanza una `FormatException`. Aquí se ignora, como cualquier otro argumento
que no nombra una clave: leer la línea de comandos no debería poder impedir que una aplicación
arranque por un guion de más.

### No confundir con `Daf.CommandLine`

Escribir una *aplicación* de línea de comandos —comandos, subcomandos, opciones tipadas, ayuda
generada— es otro trabajo, y le corresponde a [`Daf.CommandLine`](../CommandLine/README.es.md). Los
dos no comparten motor, igual que `Microsoft.Extensions.Configuration.CommandLine` no usa
`System.CommandLine`.

---

## Binding a objetos

Usa `TConfigurationBinder` para poblar un objeto Delphi desde una sección de configuración:

```pascal
uses Daf.Configuration.Binder;

type
  TDatabaseOptions = class
  public
    Host: string;
    Port: Integer;
    Name: string;
  end;

var Opts := TDatabaseOptions.Create;
TConfigurationBinder.Bind(Config.GetSection('Database'), Opts);
// Opts.Host, Opts.Port, Opts.Name quedan poblados
```

---

## Integración con Hosting

Con `Hosting`, configura las fuentes en `ConfigureAppConfiguration`:

```pascal
THostBuilder.Create
  .ConfigureAppConfiguration(procedure(Ctx: IHostBuilderContext;
                                       Builder: IConfigurationBuilder)
  begin
    JsonConfig.AddFile(Builder, 'appsettings.json');
    JsonConfig.AddFile(Builder,
      'appsettings.' + string(Ctx.Environment.EnvironmentName) + '.json', [csoOptional]);
    Builder.Add(TEnvironmentVariablesConfigurationSource.Create);
  end)
  .ConfigureServices(procedure(Ctx: IHostBuilderContext; Services: IServiceCollection)
  begin
    // Ctx.Configuration está completamente construida aquí
    Services.AddSingleton<IMyOptions>(TMyOptions.Create(Ctx.Configuration));
  end)
  .Build.Run;
```

---

## Documentación

- 📖 [Guía de uso](docs/GUIDE.es.md) — proveedores en profundidad, claves jerárquicas, binder, fuentes personalizadas
