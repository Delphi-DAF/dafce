# Application

**🌍 Language: English | [Español](README.es.md)**

Top-level entry point for DAFce applications. `TDafApplication` wraps the generic [Hosting](../Hosting/README.md) pipeline and adds application-level lifecycle helpers, version metadata, and a global singleton accessor.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Quick Start

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

## Subclass pattern

Override `Execute` to write imperative startup logic:

```pascal
type
  TMyApp = class(TDafApplication)
  protected
    procedure Execute; override;
  end;

procedure TMyApp.Execute;
begin
  Logger.LogInformation('App running, version {V}', [VersionInfo.Tag]);
  // … long-running work or RunAsync
end;
```

---

## Version information

Each application carries a `TVersionInfo` record populated at compile time:

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
