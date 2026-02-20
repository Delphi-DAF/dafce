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

var Host := THostBuilder.Create
  .ConfigureServices(procedure(Context: IHostBuilderContext;
                               Services: IServiceCollection)
  begin
    Services.AddSingleton<IMyService, TMyService>;
    Services.AddHostedService<TMyWorker>; // IHostedService
  end)
  .Build;

Host.Start;
Host.WaitForShutdown;
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

---

## Entorno

El entorno se lee de la variable `DAF_APP_ENV` (por defecto `Production`):

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

Variables de entorno leídas al arrancar:

| Variable | Por defecto | Propósito |
|----------|-------------|-----------|
| `DAF_APP_ENV` | `Production` | Nombre del entorno |
| `DAF_APP_NAME` | Nombre del ejecutable | Nombre de la aplicación |
| `DAF_CONTENT_ROOT` | Ruta del binario | Directorio raíz de contenido |

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

## Usando TDafApplication (módulo Application)

Para apps de consola, el módulo `Application` ofrece un wrapper de más alto nivel:

```pascal
uses DAF.Application.Builder;

TDafApplication.CreateHostBuilder
  .ConfigureServices(procedure(Context: IHostBuilderContext;
                               Services: IServiceCollection)
  begin
    Services.AddSingleton<IMyService, TMyService>;
    Services.AddHostedService<TMyWorker>;
  end)
  .Build
  .Run;
```

Consulta [Application](../Application/README.es.md) para la documentación completa del módulo Application.

---

## Documentación

- 📖 [Guía de uso](docs/GUIDE.es.md) — pipeline de configuración, servicios scoped, hosted services, hooks de ciclo de vida
