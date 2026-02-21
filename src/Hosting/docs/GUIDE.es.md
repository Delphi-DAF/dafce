# Hosting — Guía de uso

**🌍 Idioma: [English](GUIDE.md) | Español**

---

## Contenido

1. [Visión general de la arquitectura](#visión-general-de-la-arquitectura)
2. [Construir un host](#construir-un-host)
3. [Pipeline de configuración](#pipeline-de-configuración)
4. [Registrar servicios](#registrar-servicios)
5. [Hosted services (workers en segundo plano)](#hosted-services-workers-en-segundo-plano)
6. [Entorno](#entorno)
7. [Ciclo de vida de la aplicación](#ciclo-de-vida-de-la-aplicación)
8. [Acceder a servicios tras el Build](#acceder-a-servicios-tras-el-build)
9. [Apagado elegante](#apagado-elegante)
10. [Patrón AppModule con Hosting](#patrón-appmodule-con-hosting)

---

## Visión general de la arquitectura

```
THostBuilder
  ├── ConfigureHostConfiguration   ← configuración de bajo nivel (variables de entorno, etc.)
  ├── ConfigureAppConfiguration    ← configuración de app (JSON, INI, env)
  ├── ConfigureServices            ← registros DI
  └── Build ──► IHost
                 ├── Services: IServiceProvider
                 ├── IHostEnvironment
                 ├── IHostApplicationLifetime
                 └── [IHostedService, ...]
```

El host se construye una vez, se arranca una vez y se para una vez. Todos los servicios están gestionados por el contenedor DI.

---

## Construir un host

```pascal
uses Daf.Hosting, Daf.Extensions.Hosting, Daf.Extensions.DependencyInjection;

var Host := THostBuilder.Create
  .ConfigureServices(procedure(Context: IHostBuilderContext;
                               Services: IServiceCollection)
  begin
    Services.AddSingleton<IMyService, TMyService>;
  end)
  .Build;

Host.Start;
Host.WaitForShutdown; // bloquea hasta la señal de parada
```

`THostBuilder` es un **singleton** — una sola instancia por proceso.  
`Build` solo puede llamarse **una vez**; congela la configuración y crea el contenedor DI.

---

## Pipeline de configuración

El host ejecuta la configuración en tres fases ordenadas:

### 1. Configuración del host

Ajustes de bootstrap — se aplican antes de que se conozca el entorno:

```pascal
.ConfigureHostConfiguration(procedure(Builder: IConfigurationBuilder)
begin
  Builder.AddEnvironmentVariables; // lee DAF_APP_ENV, DAF_APP_NAME, etc.
end)
```

### 2. Configuración de la app

Ajustes completos — se ejecuta tras resolver el entorno:

```pascal
.ConfigureAppConfiguration(procedure(Context: IHostBuilderContext;
                                     Builder: IConfigurationBuilder)
begin
  Builder
    .AddJsonFile('appsettings.json')
    .AddJsonFile('appsettings.' + string(Context.Environment.EnvironmentName) + '.json',
      {optional:} True);
end)
```

### 3. Servicios

Registros DI — `Context.Configuration` está completamente poblado aquí:

```pascal
.ConfigureServices(procedure(Context: IHostBuilderContext;
                              Services: IServiceCollection)
begin
  var Db := Context.Configuration['Database:ConnectionString'];
  Services.AddSingleton<IDatabase>(TDatabase.Create(Db));
end)
```

Puedes llamar a `ConfigureServices` varias veces — todos los callbacks se aplican en orden de registro.

---

## Registrar servicios

Dentro de `ConfigureServices` está disponible la API completa de `IServiceCollection`:

```pascal
.ConfigureServices(procedure(Context: IHostBuilderContext;
                              Services: IServiceCollection)
begin
  // Tiempos de vida
  Services.AddSingleton<ICache, TMemoryCache>;
  Services.AddScoped<IUnitOfWork, TDbUnitOfWork>;
  Services.AddTransient<IEmailSender, TSmtpSender>;

  // Hosted service (arranca/para con el host)
  Services.AddHostedService<TBackgroundIndexer>;
end)
```

Consulta [DependencyInjection](../../DependencyInjection/docs/GUIDE.es.md) para la referencia completa de registro.

---

## Hosted services (workers en segundo plano)

Implementa `IHostedService` para cualquier trabajo que deba ejecutarse junto al host:

```pascal
type
  TOrderProcessor = class(TInterfacedObject, IHostedService)
  private
    FTimer: TTimer;
  public
    procedure Start;
    procedure Stop;
  end;

procedure TOrderProcessor.Start;
begin
  // configurar trabajo periódico, abrir conexiones, etc.
end;

procedure TOrderProcessor.Stop;
begin
  // vaciar, cerrar, limpiar
end;
```

Registra el servicio:
```pascal
Services.AddHostedService<TOrderProcessor>;
```

- `Start` se llama para cada hosted service cuando se ejecuta `Host.Start`, **en orden de registro**.
- `Stop` se llama en **orden inverso de registro** durante el apagado.
- Los hosted services son Singletons — también puedes inyectarlos como `IOrderProcessor` si es necesario.

---

## Entorno

`IHostEnvironment` está pre-registrado como Singleton. Accede a él desde cualquier parte del contenedor DI:

```pascal
var Env := Host.Services.GetRequiredService<IHostEnvironment>;

Env.ApplicationName   // p. ej. 'MyApp'
Env.EnvironmentName   // p. ej. 'Development'
Env.ContentRootPath   // p. ej. 'C:\MyApp\'
Env.BinPath           // siempre el directorio del binario

Env.IsDevelopment   // True en Development
Env.IsProduction    // True en Production
Env.IsTesting       // True en Testing (útil para test runners)
```

Establece el entorno mediante variable de OS antes de lanzar:
```
DAF_APP_ENV=Development
```

Abreviaturas aceptadas: `deve`, `stag`, `prod`, `test` (sin distinción de mayúsculas).

También puedes leer variables de entorno arbitrarias:
```pascal
var DbHost := Env['MY_DB_HOST'];
```

---

## Ciclo de vida de la aplicación

`IHostApplicationLifetime` publica tres cancellation tokens a los que puedes reaccionar:

```pascal
var Lifetime := Services.GetRequiredService<IHostApplicationLifetime>;

// Se dispara cuando todos los hosted services han arrancado
Lifetime.ApplicationStarted.Register(procedure begin
  Logger.LogInfo('Aplicación lista');
end);

// Se dispara cuando se solicita un apagado (antes de que paren los servicios)
Lifetime.ApplicationStopping.Register(procedure begin
  Logger.LogInfo('Apagado iniciado');
end);

// Se dispara cuando todos los hosted services han parado
Lifetime.ApplicationStopped.Register(procedure begin
  Logger.LogInfo('Aplicación detenida');
end);

// Solicitar apagado elegante por programa
Lifetime.StopApplication;
```

`StopApplication` desencadena la misma secuencia de apagado que `Ctrl+C` / `SIGTERM`.

---

## Acceder a servicios tras el Build

```pascal
var Host := HostBuilder.Build;
Host.Start;

// Acceder a servicios desde fuera del grafo DI
var Svc := Host.Services.GetRequiredService<IMyService>;
Svc.Execute;

Host.WaitForShutdown;
```

Para operaciones con scope (p. ej. un job en segundo plano que necesita una unidad de trabajo):

```pascal
var Scope := Host.Services.CreateScope;
try
  var UoW := Scope.ServiceProvider.GetRequiredService<IUnitOfWork>;
  UoW.DoWork;
  UoW.Commit;
finally
  Scope := nil;
end;
```

---

## Apagado elegante

`Host.WaitForShutdown` bloquea hasta que el proceso recibe una señal de apagado (`Ctrl+C`, `SIGTERM` o `Lifetime.StopApplication`). La secuencia de apagado es:

1. Se disparan los callbacks de `IHostApplicationLifetime.ApplicationStopping`
2. Se llama a `IHostedService.Stop` para cada servicio (en orden inverso)
3. El contenedor DI libera las instancias Singleton (en orden inverso de creación)
4. Se disparan los callbacks de `IHostApplicationLifetime.ApplicationStopped`

Usa siempre `WaitForShutdown` en lugar de busy-waiting — se integra con el manejador de señales del SO mediante `TShutdownHook`.

---

## Patrón AppModule con Hosting

Divide los registros en módulos para apps grandes:

```pascal
uses Daf.AppModule, Daf.Extensions.DependencyInjection;

type
  TInfraModule = class(TInterfacedObject, IAppModule)
  public
    procedure AddServices(const Services: IServiceCollection);
  end;

procedure TInfraModule.AddServices(const Services: IServiceCollection);
begin
  Services.AddSingleton<IDatabase, TPostgresDatabase>;
  Services.AddHostedService<TBackgroundWorker>;
end;
```

Carga todos los módulos en `ConfigureServices`:

```pascal
.ConfigureServices(procedure(Context: IHostBuilderContext;
                              Services: IServiceCollection)
begin
  TInfraModule.Create.AddServices(Services);
  TDomainModule.Create.AddServices(Services);
end)
```

Con `TDafApplication` (módulo Application), los módulos que implementan `IAppModule` se descubren y cargan automáticamente — consulta [Application](../../Application/docs/GUIDE.es.md).
