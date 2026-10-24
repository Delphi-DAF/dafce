# Hosting

**🌍 Idioma: [English](README.md) | Español**

Host genérico para aplicaciones Delphi inspirado en `Microsoft.Extensions.Hosting` de .NET. Compone servicios, configuración, hosted services (workers en segundo plano) y el ciclo de vida de la aplicación en un runtime único y gestionado.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![Licencia](https://img.shields.io/badge/licencia-MIT-blue.svg)](../../legal/LICENSE.md)

---

## ¿Por qué usarlo?

- 🏗️ **Arranque unificado** — Un único lugar para DI, configuración, logging y servicios en segundo plano
- 🌍 **Consciente del entorno** — Modos `Development` / `Staging` / `Production` / `Testing` integrados
- ⚙️ **Configuración por capas** — Host config, luego app config, con sobreescritura por variables de entorno
- 🔄 **Hosted services** — Registra implementaciones de `IHostedService` que arrancan y paran con el host
- 🛑 **Apagado elegante** — `IHostApplicationLifetime` señaliza `Started`, `Stopping`, `Stopped`
- 🔌 **Integración con DI** — `IServiceProvider` es la columna vertebral; todos los servicios se registran via `IServiceCollection`

---

## Inicio rápido

```pascal
uses
  Daf.Hosting,
  Daf.Extensions.Hosting,
  Daf.Extensions.DependencyInjection;

var Builder := THost.CreateBuilder;

Builder.Services.AddSingleton<IMyService, TMyService>;
Builder.Services.AddHostedService<TMyWorker>; // IHostedService

var Host := Builder.Build;
Host.Start;
try
  Host.WaitForShutdown;
finally
  StopAndNil(Host);
end;
```

---

## IHostedService

Cualquier clase registrada como hosted service debe implementar `IHostedService`:

```pascal
type
  TMyWorker = class(TInterfacedObject, IHostedService)
  public
    procedure Start;
    procedure Stop;
  end;

procedure TMyWorker.Start;
begin
  // se lanza cuando se llama a Host.Start
end;

procedure TMyWorker.Stop;
begin
  // se llama en el apagado elegante
end;
```

Registra el worker:
```pascal
Services.AddHostedService<TMyWorker>;
// equivalente a:
Services.AddSingleton<IHostedService, TMyWorker>;
```

Si un `Stop` lanza, el resto del apagado se ejecuta igual: los demás servicios paran y el contenedor
se apaga. `Host.Stop` lanza lo que falló al final: la misma excepción, o una `EAggregateException`
con todos los fallos dentro si fallaron varios pasos.

---

## Entorno

El entorno se lee del ajuste de host `environment` —`--environment Staging` en la línea de
comandos, o la variable `DAF_ENVIRONMENT`— y por defecto es `Production`:

| Valor | IsDevelopment | IsStaging | IsProduction | IsTesting |
|-------|:---:|:---:|:---:|:---:|
| `Development` / `deve` | ✅ | | | |
| `Staging` / `stag` | | ✅ | | |
| `Production` / `prod` | | | ✅ | |
| `Testing` / `test` | | | | ✅ |

```pascal
var Env := Host.Services.GetRequiredService<IHostEnvironment>;
if Env.IsDevelopment then
  WriteLn('Modo desarrollo activo');
WriteLn(Env.ContentRootPath);
```

Ajustes de host leídos al arrancar, como argumento o como variable —gana el argumento—:

| Clave | Variable | Por defecto | Propósito |
|-------|----------|-------------|-----------|
| `environment` | `DAF_ENVIRONMENT` | `Production` | Nombre del entorno |
| `applicationName` | `DAF_APPLICATIONNAME` | Nombre del ejecutable | Nombre de la aplicación |
| `contentRoot` | `DAF_CONTENTROOT` | Directorio actual | Directorio raíz de contenido |

Cuando el directorio actual es el del sistema, como en un servicio de Windows, el content root es
en su lugar la carpeta del ejecutable. `Env.BinPath` es siempre esa carpeta.

---

## Validación del contenedor

En el entorno `Development` el host construye el contenedor con `ValidateOnBuild` y
`ValidateScopes`: un registro cuyas dependencias no se pueden satisfacer, o un Singleton que captura
un Scoped, detiene `Build` con todos los hallazgos juntos. En cualquier otro entorno el contenedor se
construye sin validar.

`ServiceProviderOptions` en el builder dice las opciones que usará `Build`, y asignarla las fija sea
cual sea el entorno:

```pascal
Builder.ServiceProviderOptions := TServiceProviderOptions.None;               // nunca valida
Builder.ServiceProviderOptions := TServiceProviderOptions.Create(True, True); // valida siempre
```

La validación alcanza a los registros por clase; un registro por factory queda fuera. Con
`ValidateScopes`, un servicio Scoped pedido al proveedor raíz también se rechaza al resolverlo. Los
servicios alojados y las tareas de arranque son Singleton, así que lo que necesiten de un Scoped lo
resuelven desde un ámbito propio, con un `IServiceScopeFactory` recibido por constructor.

---

## Ciclo de vida de la aplicación

```pascal
var Lifetime := Host.Services.GetRequiredService<IHostApplicationLifetime>;

Lifetime.ApplicationStarted.Register(procedure begin
  WriteLn('App arrancada');
end);

Lifetime.ApplicationStopping.Register(procedure begin
  WriteLn('Parando...');
end);

// Solicitar parada elegante desde cualquier lugar
Lifetime.StopApplication;
```

---

## Usar la familia de aplicaciones

Para un programa con línea principal, cada variante de la familia de aplicaciones —`Daf.Cli`,
`Daf.Web`, `Daf.VCL` o `Daf.FMX`— envuelve este pipeline y añade `Run`. El builder es usable en
cuanto se crea: `Services`,
`Configuration` y `Environment` responden en cuanto se leen, así que los registros ocurren donde
están escritos.
`Build` devuelve la aplicación, y su `Run` da el código de salida.

```pascal
uses Daf.Cli, Daf.CommandLine, Daf.Configuration.CommandLine;

var B := TCliApplication.CreateBuilder(CommandLineConfig.ProcessArgs);

B.Services.AddSingleton<IMyService, TMyService>;
B.Services.AddHostedService<TMyWorker>;

ExitCode := B.Build.Run;
```

Consulta [La familia de aplicaciones](docs/APPLICATION-FAMILY.es.md) para saber cuál es tu punto de entrada y qué añade la capa sobre el host.

---

## Documentación

- 📖 [Guía de uso](docs/GUIDE.es.md) — pipeline de configuración, servicios scoped, hosted services, hooks de ciclo de vida
- 📖 [La familia de aplicaciones](docs/APPLICATION-FAMILY.es.md) — cuál es tu punto de entrada, qué añade la capa sobre el host y qué es en términos de .NET
- 📖 [Composición del arranque](../DependencyInjection/docs/STARTUP-COMPOSITION.es.md) — `IStartupTask` frente a `IHostedService`, y las dos fases del arranque
- 📁 [HostQuickStart](samples/HostQuickStart/README.md) — componer, construir, arrancar y parar, sin ninguna variante de aplicación alrededor
- 📁 [StartupReport](samples/StartupReport/README.md) — dónde queda el registro del arranque, y qué enseña un arranque que no llega a completarse
- 📁 [HostLifetime](samples/HostLifetime/README.md) — las tres notificaciones del ciclo de vida, y una copia del `IHost` que se suelta en `ApplicationStopped`
- 📁 [ConsoleSample](../Cli/samples/ConsoleSample/README.md) — configuración y el patrón de opciones en un programa de consola
- 📁 [HostedService](../samples/HostedService/) — workers en segundo plano con `AddHostedService`
- 📁 [PackageInit](../Cli/samples/PackageInit/README.md) — un `IStartupTask` aportado desde un paquete, sin que el `.dpr` lo nombre
