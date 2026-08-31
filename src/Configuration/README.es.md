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

// Distinguir una clave ausente de una cuyo valor es vacío -- el indexador da '' para las dos
if Config.Exists('Database:Password') then ;          // la presencia, y nada más
var Read := Config.Find('Database:Password');         // presencia Y valor, una sola lectura
```

---

## Ausente no es vacío

Dos miembros que .NET no tiene, y la razón es que **allí la distinción la lleva la anulabilidad**:
el indexador es `string? this[key]`, `null` para ausente y `''` para presente y vacía. Una cadena
de Delphi no puede ser nula, así que lo que allí hace el `?` aquí necesita un miembro propio —
`Exists` para la presencia, `Find` para presencia y valor en una sola lectura.

El indexador sigue devolviendo `string` a propósito. Un retorno anulable convierte de forma
implícita y esa conversión se aplica en la comparación, así que `C['ausente'] = ''` y
`C['vacía'] = ''` seguirían dando las dos `True` — escondería el defecto tras un tipo que promete
arreglarlo. [La guía](docs/GUIDE.es.md#distinguir-una-clave-ausente-de-una-vacía) tiene la medida.


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

## Ajustes con dueño

`TConfigurationBinder` construye el objeto y lo entrega: es de quien llama, y quien llama lo libera.
Eso sigue siendo así, y es lo correcto para una utilidad, un script o un paso de arranque anterior a
que haya contenedor.

Dentro de un contenedor no lo es. `ConfigOptions.Bind` declara que unos ajustes se llenan desde una
sección, y a partir de ahí el contenedor los construye, es su dueño y los libera:

```pascal
uses Daf.Options.Configuration;

ConfigOptions.Bind<TAppSettings>(Services, Config);                       // la raíz
ConfigOptions.Bind<TDbSettings>(Services, Config, 'Database', boFields);  // una sección

Services.AddSingleton<IGreeter, TGreeter>;
```

```pascal
constructor TGreeter.Create(const Options: IOptions<TAppSettings>);
begin
  inherited Create;
  FTitle := Options.Value.Title;
end;
```

El servicio **no nombra su sección**: pide sus ajustes por constructor y el arranque decide de dónde
salen. Cambiar de sección es una línea en el sitio de composición y ningún cambio en el servicio.

El contrato de `IOptions<T>` —la propiedad del objeto, la trampa del puntero colgando, la
construcción perezosa, la validación y qué pasa con unos ajustes que nadie registró— está en
[`DependencyInjection.Abstractions`](../DependencyInjection.Abstractions/README.es.md).

### Una foto, no un espejo

La sección se lee **una sola vez**, al construirse el valor, y cambiar la configuración después
**no** lo cambia. Se dice en voz alta porque es lo contrario de lo que espera quien ha visto el
patrón en otro sitio: sin notificación de cambio en los proveedores, releer en cada acceso daría a
dos lecturas del mismo objeto dos respuestas distintas y a nadie forma de saber cuándo cambió. La
vista que sigue a la configuración viva es otra pieza, y todavía no existe.

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
- 📁 [ConfigSample](samples/ConfigSample/README.md) — la cadena de proveedores y su precedencia, funcionando
