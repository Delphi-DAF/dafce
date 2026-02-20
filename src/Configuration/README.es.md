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

var Config := TConfigurationBuilder.Create
  .AddJsonFile('appsettings.json')
  .AddEnvironmentVariables
  .Build;

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
| Encadenado | `Daf.Configuration.Chained` | Envuelve un `IConfiguration` existente como fuente |

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
    Builder
      .AddJsonFile('appsettings.json')
      .AddJsonFile('appsettings.' + string(Ctx.Environment.EnvironmentName) + '.json', True)
      .AddEnvironmentVariables;
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
