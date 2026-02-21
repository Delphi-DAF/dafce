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
| `IHostBuilder` | Constructor fluido — configura y construye el host |
| `IHost` | El host en ejecución: `Services`, `Start`, `Stop`, `WaitForShutdown` |
| `IHostedService` | Contrato de servicio en segundo plano: `Start` / `Stop` |
| `IHostEnvironment` | Entorno de ejecución: nombre, nombre de app, rutas, `IsDevelopment`, ... |
| `IHostBuilderContext` | Contexto pasado a los callbacks de configuración: `Environment` + `Configuration` |
| `IHostApplicationLifetime` | Señales de ciclo de vida: `ApplicationStarted`, `ApplicationStopping`, `ApplicationStopped`, `StopApplication` |

### Tipos delegado

```pascal
TConfigureHostConfigAction = TProc<IConfigurationBuilder>;
TConfigureAppConfigAction  = TProc<IHostBuilderContext, IConfigurationBuilder>;
TConfigureServicesAction   = TProc<IHostBuilderContext, IServiceCollection>;
```

### Constantes de entorno

```pascal
TEnvironments.Development   // 'Development'
TEnvironments.Staging       // 'Staging'
TEnvironments.Production    // 'Production'
TEnvironments.Testing       // 'Testing'
```

### Variables de entorno DAF

```pascal
TDafEnvVars.APP_ENV       // 'DAF_APP_ENV'
TDafEnvVars.APP_NAME      // 'DAF_APP_NAME'
TDafEnvVars.CONTENT_ROOT  // 'DAF_CONTENT_ROOT'
```

### Helper

`IServiceCollectionHelper` añade `AddHostedService<T>` directamente sobre `IServiceCollection`:

```pascal
Services.AddHostedService<TMyWorker>;
// equivalente a Services.AddSingleton<IHostedService, TMyWorker>
```

---

## Dependencias

- `Daf.Extensions.DependencyInjection` (DependencyInjection.Abstractions)
- `Daf.Extensions.Configuration` (Configuration.Abstractions)
- Solo RTL de Delphi

La implementación está en [`Hosting`](../Hosting/README.es.md).
