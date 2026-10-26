# Hosting.Abstractions

**🌍 Idioma: [English](README.md) | Español**

Contratos principales del sistema de hosting de DAF. Este módulo define **solo interfaces y tipos** — sin implementación. Referencíalo cuando escribas hosted services, componentes conscientes del entorno, o extensiones del host genérico sin depender del host concreto.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![Licencia](https://img.shields.io/badge/licencia-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Qué hay en este módulo

Todos los tipos se encuentran en `Daf.Extensions.Hosting`.

### Interfaces principales

| Interfaz | Rol |
|----------|-----|
| `IHostApplicationBuilder` | El contrato común de la familia de aplicaciones: `Services`, `Configuration`, `Environment`, `Logging`, `Properties`, `ServiceProviderOptions` |
| `IStartupTask` | Se ejecuta una vez al arrancar, con el proveedor ya construido: `Run`, y nada que parar |
| `IHost` | El host en ejecución: `Services`, `Start`, `Stop`, `WaitForShutdown` |
| `IHostedService` | Contrato de servicio en segundo plano: `Start` / `Stop` |
| `IHostEnvironment` | Entorno de ejecución: nombre, nombre de app, rutas, `IsDevelopment`, ... |
| `IHostBuilderContext` | Contexto de arranque, registrado en el contenedor: `Environment`, `Configuration`, `Properties` |
| `IHostApplicationLifetime` | Señales de ciclo de vida: `ApplicationStarted`, `ApplicationStopping`, `ApplicationStopped`, `StopApplication` |

### Constantes de entorno

```pascal
TEnvironments.Development   // 'Development'
TEnvironments.Staging       // 'Staging'
TEnvironments.Production    // 'Production'
TEnvironments.Testing       // 'Testing'
```

### Ajustes de host

Las claves de la configuración de host de la que se deriva `IHostEnvironment`, con los nombres del
`HostDefaults` de .NET. Como variables de entorno llevan el prefijo `DAF_`: `DAF_ENVIRONMENT`.

```pascal
THostDefaults.EnvironmentKey   // 'environment'
THostDefaults.ApplicationKey   // 'applicationName'
THostDefaults.ContentRootKey   // 'contentRoot'
```

### Helper

`IServiceCollectionHelper` añade `AddHostedService<T>` y `AddStartupTask<T>` directamente sobre
`IServiceCollection`:

```pascal
Services.AddHostedService<TMyWorker>;   // = AddSingleton<IHostedService, TMyWorker>
Services.AddStartupTask<TMySeeder>;     // = AddSingleton<IStartupTask, TMySeeder>
```

En Delphi solo hay un `record helper` visible por tipo, así que una unit que traiga otro para
`IServiceCollection` esconde este. La salida es la fachada de clase `Hosting`, que toma la
implementación como metaclase: `Hosting.AddHostedService(Services, TMyWorker)`.

---

## Dependencias

- `Daf.Extensions.DependencyInjection` (DependencyInjection.Abstractions)
- `Daf.Extensions.Configuration` (Configuration.Abstractions)
- `Daf.Extensions.Logging` (Logging.Abstractions) — por el `ILoggingBuilder` que el contrato de la
  familia expone como propiedad
- `Daf.Commons`

La implementación está en [`Hosting`](../Hosting/README.es.md).
