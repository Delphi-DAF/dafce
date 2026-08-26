# Application

**🌍 Idioma: [English](README.md) | Español**

Punto de entrada principal de las aplicaciones DAFce. `TDafApplication` envuelve el pipeline genérico de [Hosting](../Hosting/README.es.md) y añade ayudas de ciclo de vida a nivel de aplicación, metadatos de versión y un acceso singleton global.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![Licencia](https://img.shields.io/badge/licencia-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Inicio rápido

Esto es el [sample `QuickStart`](samples/QuickStart/) tal cual. `eng/check-quickstart-snippet.py`
compara los dos, así que lo que copies de aquí es lo que lee el compilador:

<!-- quickstart: src/Application/samples/QuickStart/QuickStart.dpr -->

```pascal
program QuickStart;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  System.SysUtils,
  Daf.Extensions.DependencyInjection,
  Daf.CommandLine,
  Daf.Configuration.CommandLine,
  Daf.Cli,
  QuickStart.Greeter in 'QuickStart.Greeter.pas';

// Lo mas corto que se puede escribir con la familia: una variante, sus registros donde estan
// escritos, y el codigo de salida que devuelve el trabajo.
begin
  var B := TCliApplication.CreateBuilder(CommandLineConfig.ProcessArgs);

  B.Services.AddSingleton<IGreeter, TGreeter>;

  B.OnExecute(
    function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
    begin
      Writeln(Services.GetRequiredService<IGreeter>.Greet);
      Result := TCliExit.Ok;
    end);

  ExitCode := B.Build.Run;
end.
```

El builder se sostiene en una variable **de su propia clase**, no en una cadena: `ConfigureServices`
devuelve un `IHostBuilder`, cuyo `Build` da un `IHost` y no la aplicación.

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
