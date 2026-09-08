# Hosting — Guía de uso

**🌍 Idioma: [English](GUIDE.md) | Español**

---

## Contenido

1. [Visión general de la arquitectura](#visión-general-de-la-arquitectura)
2. [Construir un host](#construir-un-host)
3. [Pipeline de configuración](#pipeline-de-configuración)
4. [Compartir datos entre piezas del arranque](#compartir-datos-entre-piezas-del-arranque)
5. [Registrar servicios](#registrar-servicios)
6. [Hosted services (workers en segundo plano)](#hosted-services-workers-en-segundo-plano)
7. [Entorno](#entorno)
8. [Ciclo de vida de la aplicación](#ciclo-de-vida-de-la-aplicación)
9. [Acceder a servicios tras el Build](#acceder-a-servicios-tras-el-build)
10. [Apagado elegante](#apagado-elegante)
11. [Patrón AppModule con Hosting](#patrón-appmodule-con-hosting)

---

## Visión general de la arquitectura

```
THost.CreateBuilder ──► IGenericHostBuilder
  ├── Configuration: IConfigurationManager   ← las fuentes, y su lectura
  ├── Environment: IHostEnvironment          ← ya resuelto, derivado de la config de host
  ├── Services: IServiceCollection           ← registros DI
  ├── Logging: ILoggingBuilder               ← solo se compone si lo tocas
  ├── Properties                             ← datos que comparten las piezas del arranque
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

var Builder := THost.CreateBuilder;

Builder.Services.AddSingleton<IMyService, TMyService>;

var Host := Builder.Build;
Host.Start;
Host.WaitForShutdown; // bloquea hasta la señal de parada
```

`THost.CreateBuilder` entrega un `IGenericHostBuilder`, así que una sola variable posee el builder
y llega a `Build`. Cada builder es independiente: puede haber varios builders y varios hosts vivos
a la vez en un mismo proceso.  
`Build` solo puede llamarse **una vez** por builder; congela la configuración y crea el contenedor DI.

---

## Pipeline de configuración

Hay **una** configuración, y ya viene poblada cuando el builder se entrega.
`Builder.Configuration` es un `IConfigurationManager` —constructor y lector a la vez—, así que una
fuente se puede elegir a partir de lo ya configurado.

### 1. Lo que siembra el builder

Antes de entregarse, el builder lee las variables de entorno del proceso con prefijo `DAF_` —de
donde salen `DAF_APP_ENV`, `DAF_APP_NAME` y `DAF_CONTENT_ROOT`— y, si se le dieron argumentos, la
línea de comandos:

```pascal
var Builder := THost.CreateBuilder(CommandLineConfig.ProcessArgs);
```

Los argumentos se añaden después de las variables de entorno, así que ganan. Una variante que
declare gramática propia de línea de comandos **no** los pasa aquí: son dos lecturas del mismo
`argv` con reglas distintas, y discrepan por construcción.

### 2. El entorno, ya resuelto

`Builder.Environment` se deriva de esa siembra dentro del constructor. Eso es lo que permite que el
paso siguiente dependa de él: no hay ninguna fase que esperar.

### 3. Tus propias fuentes

Se añaden donde estás, entre el builder y `Build`:

```pascal
JsonConfig.AddFile(Builder.Configuration, 'appsettings.json');
JsonConfig.AddFile(Builder.Configuration,
  'appsettings.' + string(Builder.Environment.EnvironmentName) + '.json', [csoOptional]);
```

Y como el manager lee además de construir, un registro consume configuración en el sitio:

```pascal
var Db := Builder.Configuration['Database:ConnectionString'];
Builder.Services.AddSingleton<IDatabase>(TDatabase.Create(Db));
```

Para eso existían los callbacks diferidos, y por eso no hay ninguno: con una configuración que se
lee mientras se compone, no queda nada que diferir.

`Build` la congela. A partir de ahí, esa misma configuración es la que entrega
`IHostBuilderContext.Configuration`.

---

## Compartir datos entre piezas del arranque

`Properties` es un `TDictionary<string, TValue>` que lleva el builder, para los datos que las piezas
que componen el arranque necesitan pasarse entre sí. Está en `IHostApplicationBuilder`, y el mismo
diccionario se lee desde el otro lado por `IHostBuilderContext.Properties`, que está registrado en
el contenedor.

```pascal
Builder.Properties.AddOrSetValue('tenant', 'acme');

// ...en cualquier sitio que tenga el contexto:
var Tenant := Context.Properties['tenant'].AsString;
```

---

## Registrar servicios

La API completa de `IServiceCollection` está disponible en `Builder.Services`:

```pascal
var Services := Builder.Services;

// Tiempos de vida
Services.AddSingleton<ICache, TMemoryCache>;
Services.AddScoped<IUnitOfWork, TDbUnitOfWork>;
Services.AddTransient<IEmailSender, TSmtpSender>;

// Hosted service (arranca/para con el host)
Services.AddHostedService<TBackgroundIndexer>;
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
var Host := Builder.Build;
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

`WaitForShutdown` se integra con el manejador de señales del SO mediante `TShutdownHook`.

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

Carga los módulos en `Builder.Services`, un `AddModule<T>` por módulo:

```pascal
Builder.Services
  .AddModule<TInfraModule>
  .AddModule<TDomainModule>;
```

`AddModule<T>` instancia el módulo, llama a su `AddServices` en el sitio y se hace cargo de su
vida. No llames a `TInfraModule.Create.AddServices(Services)` a mano: el módulo es un
`TInterfacedObject` creado con contador de referencias a cero, así que nadie lo libera.

Cuando un módulo necesita configuración, constrúyelo tú y pasa la instancia:
`Services.AddModule(TInfraModule.Create(...))`. Entregarlo como `IAppModule` es lo que le da un
dueño, y es la única vía de pasarle configuración.

**No hay descubrimiento automático.** `AddModules(Package)`, el escaneo por RTTI de los
`IAppModule`, se retiró: escondía la declaración en un `uses`, dejaba el arranque ciego en
silencio ante una RTTI recortada y ponía el orden de registro en manos de la reflexión. Una
`TDafApplication` tampoco registra módulos por su cuenta — los suyos se declaran donde se
compongan los servicios, igual que arriba. Las dos fases y las reglas de orden, en
[Composición del arranque](../../DependencyInjection/docs/STARTUP-COMPOSITION.es.md).
