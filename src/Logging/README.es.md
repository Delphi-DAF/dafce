# Logging

**🌍 Idioma: [English](README.md) | Español**

Infraestructura de logging estructurado para Delphi inspirada en `Microsoft.Extensions.Logging` de .NET. Escribe entradas de log a través de una interfaz `ILogger` unificada — proveedores intercambiables (NNLog, consola, personalizados) registrados al arrancar.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![Licencia](https://img.shields.io/badge/licencia-MIT-blue.svg)](../../legal/LICENSE.md)

---

## ¿Por qué usarlo?

- 🎚️ **Seis niveles de log** — Trace, Debug, Information, Warning, Error, Critical
- 📝 **Logging estructurado** — Marcadores `{Clave}` nombrados en plantillas de mensaje
- 🔌 **Modelo de proveedores** — Registra uno o más `ILoggerProvider`; todos reciben cada entrada
- 🏷️ **Categorías** — Los loggers se etiquetan por clase o nombre de cadena para filtrado
- 🔗 **Scopes** — Adjunta datos contextuales a un bloque de entradas con `BeginScope`
- 🛠️ **Listo para DI** — `AddLogging(Services, ...)` registra `ILoggerFactory` en el contenedor

---

## Inicio rápido

```pascal
uses
  Daf.Extensions.Logging,
  Daf.Logging,
  DAF.Logging.Builder,
  DAF.NNLog; // cualquier proveedor

// Sin DI
var Factory := TMultiProviderLoggerFactory.Create;
Factory.AddProvider(TNNLogProvider.Create(Targets, Rules));
var Logger := Factory.CreateLogger('MyApp');

Logger.LogInformation('Servidor arrancado en el puerto {Port}', [8080]);
Logger.LogError(Ex, 'Excepción no controlada en {Operation}', ['ProcessOrder']);
```

---

## Niveles de log

| Nivel | Valor | Cuándo usarlo |
|-------|------:|---------------|
| `Trace` | 0 | Extremadamente detallado — desactivado en producción |
| `Debug` | 1 | Diagnóstico de desarrollo |
| `Information` | 2 | Hitos del flujo normal de la app |
| `Warning` | 3 | Situaciones inesperadas pero recuperables |
| `Error` | 4 | Fallos en la operación actual |
| `Critical` | 5 | Requiere atención inmediata |
| `None` | 6 | Desactiva el logging |

---

## Logging estructurado

Usa marcadores nombrados en las plantillas de mensaje:

```pascal
Logger.LogInformation('Usuario {UserId} autenticado desde {IP}', [UserId, RemoteIP]);
Logger.LogWarning('Reintento {Attempt} de {Max} para {Operation}', [3, 5, 'SendEmail']);
```

La plantilla y los argumentos se mantienen juntos en `TLogState`, lo que permite a los proveedores formatearlos o indexarlos de forma independiente.

---

## Integración con DI y Hosting

```pascal
uses DAF.Logging.Builder, DAF.NNLog;

// En ConfigureServices:
AddLogging(Services, procedure(Builder: ILoggingBuilder)
begin
  Builder.AddProvider(TNNLogProvider.Create(Targets, Rules));
end);

// Resolver en un servicio:
var Factory := Provider.GetRequiredService<ILoggerFactory>;
var Logger  := Factory.CreateLogger<TMyService>;
```

---

## Documentación

- 📖 [Guía de uso](docs/GUIDE.es.md) — scopes, logging estructurado, proveedores, DI, crear proveedores personalizados
