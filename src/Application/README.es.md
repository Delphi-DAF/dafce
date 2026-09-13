# Application

**🌍 Idioma: [English](README.md) | Español**

Punto de entrada principal de las aplicaciones DAFce. `TDafApplication` envuelve el pipeline genérico de [Hosting](../Hosting/README.es.md) y añade ayudas de ciclo de vida a nivel de aplicación, metadatos de versión y un acceso singleton global.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![Licencia](https://img.shields.io/badge/licencia-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Inicio rápido

El ejemplo de entrada es de la variante de la familia que estés escribiendo:
[`Cli/samples/QuickStart`](../Cli/samples/QuickStart/) para una herramienta de consola,
[`Cli/samples/ServiceWiring`](../Cli/samples/ServiceWiring/) para lo mismo con un servicio
registrado, y [`Hosting/samples/HostQuickStart`](../Hosting/samples/HostQuickStart/) para el host
genérico sin ninguna variante alrededor.

El builder es usable en cuanto se crea: `Services`, `Configuration` y `Environment` ya responden, así
que los registros ocurren donde están escritos. `Build` devuelve la aplicación, y `Run` convierte el
resultado del trabajo en un código de salida.

---

## Patrón de subclase

Sobreescribe `Execute` para escribir lógica de arranque imperativa. `CreateHostBuilder` toma la
clase sobre la que se llama, así que llamarlo sobre la tuya es lo que hace que `Build` la instancie:

```pascal
type
  TMyApp = class(TDafApplication)
  protected
    procedure Execute; override;
  end;

procedure TMyApp.Execute;
begin
  // Services, Configuration y Environment son propiedades de la propia aplicación.
  // No hay un Logger heredado: se resuelve.
  var Logger := Services.GetRequiredService<ILoggerFactory>.CreateLogger('MyApp');
  Logger.LogInformation('en ejecución, versión {V}', [VersionInfo.Tag]);
end;

// TMyApp, no TDafApplication: eso es lo que elige la clase que Build crea.
var Builder := TMyApp.CreateHostBuilder;
Builder.Build.Run;
```

---

## Información de versión

`VersionInfo` sale de un atributo `[Version]` en la clase de la aplicación, leído por RTTI —
sin ese atributo el record está vacío:

```pascal
var V := TDafApplication.App.VersionInfo;
// V.Major, V.Minor, V.Patch, V.PreRelease, V.Tag
// V.ArchBits (32 / 64), V.Platform, V.Debug (bool)
// V.VersionTag(WithCompiledMeta) → cadena semver completa
```

---

## Singleton global

```pascal
TDafApplication.App  // función de clase — disponible después de Build
```

---

## Servicios en tiempo de ejecución

```pascal
var App := TDafApplication.App;
App.Services       // IServiceProvider
App.Environment    // IHostEnvironment
App.Configuration  // IConfiguration
App.Host           // IHost
```

---

## Documentación

- 📖 [Guía de uso](docs/GUIDE.es.md) — API del builder, ciclo de vida, metadatos de versión, integración con Hosting
