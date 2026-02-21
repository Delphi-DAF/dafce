# Application — Guía de uso

**🌍 Idioma: [English](GUIDE.md) | Español**

---

## Tabla de contenidos

1. [Visión general](#visión-general)
2. [TDafApplicationBuilder](#tdafapplicationbuilder)
3. [IDafApplication](#idafapplication)
4. [Propiedad de clase TDafApplication](#propiedad-de-clase-tdafapplication)
5. [Métodos de ciclo de vida](#métodos-de-ciclo-de-vida)
6. [Metadatos de versión](#metadatos-de-versión)
7. [Servicios en tiempo de ejecución](#servicios-en-tiempo-de-ejecución)
8. [Patrón de subclase](#patrón-de-subclase)
9. [Integración con Hosting](#integración-con-hosting)

---

## Visión general

El módulo Application se sitúa sobre el pipeline de Hosting:

```
TDafApplication.CreateHostBuilder
        │
        ▼
TDafApplicationBuilder   (extiende THostBuilder)
        │  .ConfigureServices / .ConfigureAppConfiguration / …
        ▼
      .Build
        │
        ▼
IDafApplication          (extiende IHost)
        │  .Run / .RunAsync / .Start / .Stop
```

Todo lo disponible en `THostBuilder` e `IHost` también está disponible aquí.

---

## TDafApplicationBuilder

`TDafApplication.CreateHostBuilder(ExePath)` devuelve un `TDafApplicationBuilder`. Hereda todos los métodos fluidos de `THostBuilder`:

```pascal
TDafApplication
  .CreateHostBuilder(ParamStr(0))
  .UseEnvironment('Production')
  .ConfigureAppConfiguration(
    procedure(Ctx: IHostBuilderContext; Cfg: IConfigurationBuilder)
    begin
      Cfg.AddJsonFile('appsettings.json');
    end)
  .ConfigureServices(
    procedure(Ctx: IHostBuilderContext; Services: IServiceCollection)
    begin
      Services.AddHostedService<TWorker>;
      Services.AddTransient<IMyService, TMyService>;
    end)
  .Build
  .Run;
```

---

## IDafApplication

```pascal
IDafApplication = interface(IHost)
  // Ciclo de vida
  procedure Run(WaitForShutdown: Boolean = True); overload;
  procedure Run(Exec: TProc; WaitForShutdown: Boolean = True); overload;
  procedure RunAsync;
  procedure Start;
  procedure Stop;
  procedure WaitForShutdown;

  // Servicios
  function Services    : IServiceProvider;
  function Environment : IHostEnvironment;
  function Configuration: IConfiguration;
  function Host        : IHost;

  // Versión
  function VersionInfo : TVersionInfo;
end;
```

---

## Propiedad de clase TDafApplication

Tras llamar a `.Build`, la instancia de la aplicación es accesible globalmente:

```pascal
var App := TDafApplication.App; // propiedad de clase
```

Útil en unidades que no están conectadas mediante DI y necesitan resolver servicios:

```pascal
var Svc := TDafApplication.App.Services.GetService<IMyService>;
```

---

## Métodos de ciclo de vida

| Método | Descripción |
|--------|-------------|
| `Run` | Arranca el host y bloquea hasta la señal de parada (Ctrl+C, `IHostApplicationLifetime.StopApplication`) |
| `Run(Exec)` | Ejecuta el procedimiento `Exec` y luego se detiene limpiamente |
| `RunAsync` | Arranca el host en un hilo en segundo plano |
| `Start` | Arranca el host sin bloquear |
| `Stop` | Señaliza un apagado gracioso |
| `WaitForShutdown` | Bloquea el hilo llamante hasta que el apagado finaliza |

---

## Metadatos de versión

`TVersionInfo` es un record rellenado desde `VERSION.txt` en tiempo de compilación:

```pascal
TVersionInfo = record
  Major     : Integer;
  Minor     : Integer;
  Patch     : Integer;
  PreRelease: string;    // 'alpha.1', 'rc.2', …  vacío si es estable
  Metadata  : string;    // metadatos de build (SHA de git, etc.)
  Tag       : string;    // 'Major.Minor.Patch' o 'Major.Minor.Patch-PreRelease'
  ArchBits  : Integer;   // 32 o 64
  Platform  : string;    // 'Win32', 'Win64', 'Linux64', …
  Debug     : Boolean;

  function VersionTag(WithCompiledMeta: Boolean = False): string;
end;
```

### Ejemplo

```pascal
var V := TDafApplication.App.VersionInfo;
WriteLn(V.VersionTag);                // '1.2.3'
WriteLn(V.VersionTag(True));          // '1.2.3+abc1234-Win64-64bit-Debug'
WriteLn(Format('%d-bit %s', [V.ArchBits, V.Platform]));
```

---

## Servicios en tiempo de ejecución

```pascal
var App := TDafApplication.App;

// Resolver desde el contenedor raíz
var Logger := App.Services.GetRequiredService<ILoggerFactory>
                         .CreateLogger('Main');

// Acceder a una clave de configuración
var Port := App.Configuration['server:port'];

// Comprobar el entorno
if App.Environment.IsEnvironment('Development') then
  Logger.LogDebug('Modo dev', []);
```

---

## Patrón de subclase

Hereda de `TDafApplication` y sobreescribe `Execute` para la lógica principal de la aplicación:

```pascal
type
  TMyApp = class(TDafApplication)
  private
    FWorkerService: IWorkerService;
  protected
    procedure Execute; override;
  end;

procedure TMyApp.Execute;
begin
  FWorkerService := Services.GetRequiredService<IWorkerService>;
  Logger.LogInformation('Iniciando {AppName} v{Version}',
                        [AppName, VersionInfo.Tag]);
  FWorkerService.DoWork;
end;
```

Registrar y ejecutar:

```pascal
TDafApplication
  .CreateHostBuilder(ParamStr(0))
  .ConfigureServices(procedure(S: IServiceCollection)
    begin
      S.AddTransient<IWorkerService, TWorkerService>;
    end)
  .Build
  .Run;
```

---

## Integración con Hosting

`TDafApplicationBuilder` extiende completamente `THostBuilder`, por lo que todas las funcionalidades de Hosting funcionan de forma idéntica:

- Hosted services → `Services.AddHostedService<TWorker>`
- Tokens de cancelación → `IHostApplicationLifetime`
- Timeout de apagado gracioso → `UseShutdownTimeout`
- Servicios con scope → `IServiceScope` creado por petición o por tick del worker

Consulta la [Guía de Hosting](../../Hosting/docs/GUIDE.es.md) para más detalles.
