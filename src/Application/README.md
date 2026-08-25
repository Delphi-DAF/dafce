# Application

**🌍 Language: English | [Español](README.es.md)**

Top-level entry point for DAFce applications. `TDafApplication` wraps the generic [Hosting](../Hosting/README.md) pipeline and adds application-level lifecycle helpers, version metadata, and a global singleton accessor.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Quick Start

This is the [`QuickStart` sample](samples/QuickStart/), verbatim. `eng/check-quickstart-snippet.py`
compares the two, so what you copy from here is what the compiler reads:

<!-- quickstart: src/Application/samples/QuickStart/QuickStart.dpr -->

```pascal
program QuickStart;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  System.SysUtils,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Hosting,
  Daf.Application.Builder,
  QuickStart.Greeter in 'QuickStart.Greeter.pas';

begin
  var Builder := TDafApplication.CreateHostBuilder;

  Builder.ConfigureServices(
    procedure(Context: IHostBuilderContext; Services: IServiceCollection)
    begin
      Services.AddSingleton<IGreeter, TGreeter>;
    end);

  Builder.Build.Run(
    procedure(Services: IServiceProvider)
    begin
      Writeln(Services.GetRequiredService<IGreeter>.Greet);
    end);
end.
```

The builder is held in a variable of its **own class**, not chained: `ConfigureServices` returns an
`IHostBuilder`, whose `Build` gives an `IHost` rather than the application.

---

## Subclass pattern

Override `Execute` to write imperative startup logic. `CreateHostBuilder` takes the class it is
called on, so calling it on your own class is what makes `Build` instantiate it:

```pascal
type
  TMyApp = class(TDafApplication)
  protected
    procedure Execute; override;
  end;

procedure TMyApp.Execute;
begin
  // Services, Configuration and Environment are the application's own properties.
  // There is no inherited Logger: resolve one.
  var Logger := Services.GetRequiredService<ILoggerFactory>.CreateLogger('MyApp');
  Logger.LogInformation('running version {V}', [VersionInfo.Tag]);
end;

// TMyApp, not TDafApplication: that is what picks the class Build creates.
var Builder := TMyApp.CreateHostBuilder;
Builder.Build.Run;
```

---

## Version information

`VersionInfo` comes from a `[Version]` attribute on the application class, read by RTTI —
without that attribute the record is empty:

```pascal
var V := TDafApplication.App.VersionInfo;
// V.Major, V.Minor, V.Patch, V.PreRelease, V.Tag
// V.ArchBits (32 / 64), V.Platform, V.Debug (bool)
// V.VersionTag(WithCompiledMeta) → full semver string
```

---

## Global singleton

```pascal
TDafApplication.App  // class property — available after Build
```

---

## Runtime services

```pascal
var App := TDafApplication.App;
App.Services       // IServiceProvider
App.Environment    // IHostEnvironment
App.Configuration  // IConfiguration
App.Host           // IHost
```

---

## Documentation

- 📖 [Usage Guide](docs/GUIDE.md) — builder API, lifecycle, version metadata, hosting integration
