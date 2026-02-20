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
  DAF.Logging.Builder;

// 1. Definir un target
var ConsoleTarget := TNNLogConsoleTarget.Create;
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

## Integración con DI / Hosting

```pascal
uses DAF.Logging.Builder, DAF.NNLog, DAF.NNLog.Targets.Console;

AddLogging(Services,
  procedure(Builder: ILoggingBuilder)
  var
    Target : TNNLogConsoleTarget;
    Rule   : TLogRule;
  begin
    Target := TNNLogConsoleTarget.Create;
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
