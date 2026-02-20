# Application

**🌍 Idioma: [English](README.md) | Español**

Punto de entrada principal de las aplicaciones DAFce. `TDafApplication` envuelve el pipeline genérico de [Hosting](../Hosting/README.es.md) y añade ayudas de ciclo de vida a nivel de aplicación, metadatos de versión y un acceso singleton global.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![Licencia](https://img.shields.io/badge/licencia-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Inicio rápido

```pascal
program MyApp;

uses
  Daf.Application.Builder;

begin
  TDafApplication
    .CreateHostBuilder(ParamStr(0))
    .ConfigureServices(
      procedure(Services: IServiceCollection)
      begin
        Services.AddTransient<IMyService, TMyService>;
      end)
    .Build
    .Run;
end.
```

---

## Patrón de subclase

Sobreescribe `Execute` para escribir lógica de arranque imperativa:

```pascal
type
  TMyApp = class(TDafApplication)
  protected
    procedure Execute; override;
  end;

procedure TMyApp.Execute;
begin
  Logger.LogInformation('App en ejecución, versión {V}', [VersionInfo.Tag]);
  // … trabajo de larga duración o RunAsync
end;
```

---

## Información de versión

Cada aplicación lleva un record `TVersionInfo` rellenado en tiempo de compilación:

```pascal
var V := TDafApplication.App.VersionInfo;
// V.Major, V.Minor, V.Patch, V.PreRelease, V.Tag
// V.ArchBits (32 / 64), V.Platform, V.Debug (bool)
// V.VersionTag(WithCompiledMeta) → cadena semver completa
```

---

## Singleton global

```pascal
TDafApplication.App  // propiedad de clase — disponible después de Build
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
