# NNLog

**🌍 Idioma: [English](README.md) | Español**

Proveedor de logging basado en reglas para DAFce. Se conecta al pipeline `ILoggerFactory` mediante `ILoggerProvider` y enruta las entradas de log a uno o más **targets** (consola, fichero, …) según **reglas** configurables.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![Licencia](https://img.shields.io/badge/licencia-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Inicio rápido

```pascal
uses
  DAF.NNLog,
  DAF.NNLog.Targets.Console,
  DAF.Logging.Builder,
  DAF.Extensions.Logging;

// 1. Definir un target
var ConsoleTarget := TConsoleTarget.Create;
ConsoleTarget.Name   := 'console';
ConsoleTarget.Layout := '${timestamp} [${level}] ${category} | ${message}';

// 2. Definir reglas
var Rule := TLogRule.Create;
Rule.Name    := '*';             // coincide con todas las categorías
Rule.MinLevel := TLogLevel.Debug;
Rule.WriteTo  := [ConsoleTarget];

// 3. Crear el proveedor y conectarlo a la factory
var Provider := TNNLogProvider.Create([ConsoleTarget], [Rule]);
Factory.AddProvider(Provider);
```

---

## Tokens de layout

| Token | Descripción |
|-------|-------------|
| `${timestamp}` | Timestamp ISO-8601 |
| `${pid}` | ID del proceso |
| `${thread}` | ID del hilo |
| `${level}` | Nombre del nivel de log |
| `${category}` | Categoría del logger |
| `${message}` | Mensaje renderizado |
| `${exception}` | Clase + mensaje de excepción (vacío si no hay) |
| `${scope}` | Cadena del scope activo |

Layout por defecto: `${timestamp} #${pid}:${thread} [${level}] ${category} | ${message} ${exception}`

---

## Reglas

Una regla elige a qué loggers se aplica por **categoría**, mediante su `name`:

- `*` —y un `name` **ausente o vacío**— empareja **todas** las categorías.
- Cualquier otro valor es una **expresión regular**, anclada a la categoría **completa**: el patrón
  tiene que emparejar la categoría entera, así que `Daf` nombra una categoría, no todas las que la
  contienen.
- La caja (mayúsculas/minúsculas) la decide el propio patrón, con un `(?i)` en línea; por defecto
  distingue mayúsculas.

Es una **divergencia deliberada de NLog**, donde `name` es un glob de dos comodines (`*` y `?`). Un
regex es estrictamente más expresivo, ya viene en la RTL, y lleva la decisión de la caja al patrón en
vez de esconderla en el código. `Daf.*` sigue funcionando (`.*` y `*` coinciden ahí); un comodín `?`
hay que escribirlo como `.`.

Un `name` que no sea una expresión regular válida es un **error de composición**: la configuración
falla al cargar, nombrando la regla y el patrón, en vez de dejar el fallo para el primer registro.

Una regla está **activa salvo que su configuración diga lo contrario**: `enabled` vale `true` por
defecto, y solo el literal `false` la desactiva — la semántica que documenta NLog.

---

## Integración con DI / Hosting

```pascal
uses DAF.Extensions.Logging, DAF.Logging.Builder, DAF.NNLog, DAF.NNLog.Targets.Console;

AddLogging(Services,
  procedure(Builder: ILoggingBuilder)
  var
    Target : TConsoleTarget;
    Rule   : TLogRule;
  begin
    Target := TConsoleTarget.Create;
    Target.Name := 'console';

    Rule := TLogRule.Create;
    Rule.Name     := '*';
    Rule.MinLevel := TLogLevel.Information;
    Rule.WriteTo  := [Target];

    Builder.AddProvider(TNNLogProvider.Create([Target], [Rule]));
  end);
```

---

## Documentación

- 📖 [Guía de uso](docs/GUIDE.es.md) — reglas, targets, layout, múltiples targets, targets personalizados
