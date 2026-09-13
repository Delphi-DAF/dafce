# Application

**🌍 Language: English | [Español](README.es.md)**

Top-level entry point for DAFce applications. The application family — `TCliApplication`, `TWebApplication`, `TVclApplication`, `TFmxApplication` — wraps the generic [Hosting](../Hosting/README.md) pipeline and adds the edge around `Run`: exit codes, version metadata and the platform's own lifecycle. `TDafApplication` and `TDafApplicationBuilder` are **deprecated** and still compile; see the [Usage Guide](docs/GUIDE.md) for what replaced what.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Quick Start

The entry example belongs to the family variant you are writing:
[`Cli/samples/QuickStart`](../Cli/samples/QuickStart/) for a console tool,
[`Cli/samples/ServiceWiring`](../Cli/samples/ServiceWiring/) for the same with a registered service,
and [`Hosting/samples/HostQuickStart`](../Hosting/samples/HostQuickStart/) for the generic host with
no variant around it.

The builder is usable the moment it is created: `Services`, `Configuration` and `Environment` already
answer, so registrations happen where they are written. `Build` returns the application, and `Run`
turns the work's outcome into an exit code.

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
TDafApplication.App  // class function — available after Build
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
