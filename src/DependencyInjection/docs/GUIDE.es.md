# DependencyInjection — Guía de uso

**🌍 Idioma: [English](GUIDE.md) | Español**

---

## Contenido

1. [Conceptos](#conceptos)
2. [Registro de servicios](#registro-de-servicios)
3. [Construir el contenedor](#construir-el-contenedor)
4. [Resolver servicios](#resolver-servicios)
5. [Tiempos de vida](#tiempos-de-vida)
6. [Scopes](#scopes)
7. [Patrón AppModule](#patrón-appmodule)
8. [Resolución diferida (Lazy)](#resolución-diferida-lazy)
9. [Decorador Duck Typing](#decorador-duck-typing)
10. [Integración con Hosting](#integración-con-hosting)

---

## Conceptos

El contenedor DI se articula en torno a tres tipos:

| Tipo | Rol |
|------|-----|
| `IServiceCollection` | Registro — añade descriptores de servicio antes de construir |
| `IServiceProvider` | Resolutor — resuelve servicios en tiempo de ejecución |
| `IServiceScope` | Frontera de vida — agrupa instancias Scoped |

`TServiceCollection` es la implementación concreta de `IServiceCollection`.  
Llama a `BuildServiceProvider` una sola vez para congelar la colección y obtener el `IServiceProvider`.

---

## Registro de servicios

### Por clase de implementación

```pascal
Services.AddSingleton<IMyService, TMyServiceImpl>;
Services.AddTransient<IMyService, TMyServiceImpl>;
Services.AddScoped<IMyService, TMyServiceImpl>;
```

El contenedor usa RTTI para construir `TMyServiceImpl`. Debe tener un constructor público sin parámetros, o bien utiliza una factoría.

### Por lambda de factoría

Control total sobre la construcción:

```pascal
Services.AddTransient<IMyService>(
  function(P: IServiceProvider): IMyService
  begin
    Result := TMyServiceImpl.Create(
      P.GetRequiredService<IConfig>,
      P.GetRequiredService<ILogger>
    );
  end);
```

### Por instancia existente (solo Singleton)

```pascal
var Repo: IMyRepository := TMyRepository.Create(ConnectionString);
Services.AddSingleton<IMyRepository>(Repo);
```

### Registro de bajo nivel (PTypeInfo)

Cuando los genéricos no están disponibles:

```pascal
Services.Add(TypeInfo(IMyService), TServiceLifeTime.Transient,
  function(P: IServiceProvider): IInterface
  begin
    Result := TMyServiceImpl.Create;
  end);
```

### Varios registros para la misma interfaz

Puedes registrar la misma interfaz varias veces:

```pascal
Services.AddTransient<IPlugin, TPluginA>;
Services.AddTransient<IPlugin, TPluginB>;

// Resolver todos
var Plugins := Provider.GetServices<IPlugin>;
for var P in Plugins do
  P.Execute;
```

---

## Construir el contenedor

```pascal
var Provider := Services.BuildServiceProvider;
```

- `BuildServiceProvider` **congela** la colección — llamadas posteriores a `Add*` lanzan una excepción.
- Solo puede llamarse **una vez** por instancia de `TServiceCollection`.
- El `IServiceProvider` devuelto es el scope raíz (los Singletons viven aquí).

**Siempre llama a `ShutDown` al terminar:**

```pascal
Provider.ShutDown;
```

Esto libera todas las instancias singleton gestionadas en orden inverso de creación.

---

## Resolver servicios

| Método | Comportamiento |
|--------|---------------|
| `GetService<T>` | Devuelve `nil` si no está registrado |
| `GetRequiredService<T>` | Lanza `EServiceProviderError` si no está registrado |
| `TryGet<T>(out Service)` | Devuelve `False` si no está registrado (sin excepción) |
| `GetServices<T>` | Devuelve todos los registros para `T` como `IInterfaceList<T>` |
| `CanResolve(TypeInfo)` | `True` si existe al menos un registro |

```pascal
// Resolución segura
var Svc: IMyService;
if Provider.TryGet<IMyService>(Svc) then
  Svc.DoWork;

// Resolución requerida — excepción si falta
var Svc := Provider.GetRequiredService<IMyService>;

// Resolver todos
for var P in Provider.GetServices<IPlugin> do
  P.Execute;
```

---

## Tiempos de vida

| Tiempo de vida | Se crea | Se destruye |
|----------------|---------|-------------|
| `Singleton` | Una vez (primer resolve) | En `Provider.ShutDown` |
| `Scoped` | Una vez por scope | Al hacer `nil` del scope / `ShutDown` |
| `Transient` | En cada resolve | Cuando se libera la última referencia |

```pascal
// Singleton — siempre la misma instancia
Services.AddSingleton<ICache, TMemoryCache>;

// Scoped — nueva por scope (p. ej. por petición HTTP, por unidad de trabajo)
Services.AddScoped<IUnitOfWork, TEFUnitOfWork>;

// Transient — nueva cada vez
Services.AddTransient<IEmailSender, TSmtpSender>;
```

> ⚠️ **Dependencia cautiva**: nunca inyectes un servicio `Transient` o `Scoped` en un `Singleton` — la instancia de vida más corta quedará capturada durante la vida del Singleton. El contenedor no detecta esto automáticamente.

---

## Scopes

Usa scopes para agrupar servicios Scoped relacionados — por ejemplo, por petición o por unidad de trabajo:

```pascal
var Scope := Provider.CreateScope;
try
  var SP := Scope.ServiceProvider;
  var UoW := SP.GetRequiredService<IUnitOfWork>;
  var Repo := SP.GetRequiredService<IRepository>;
  // UoW y Repo comparten el mismo scope; las deps Scoped se comparten entre ellos
  UoW.Commit;
finally
  Scope := nil; // libera todas las instancias Scoped de este scope
end;
```

`IServiceScope` solo expone `ServiceProvider: IServiceProvider` — úsalo para resolver.

---

## Patrón AppModule

Divide los registros en módulos lógicos usando `IAppModule`:

```pascal
uses Daf.AppModule;

type
  TInfrastructureModule = class(TInterfacedObject, IAppModule)
  public
    procedure AddServices(const Services: IServiceCollection);
  end;

procedure TInfrastructureModule.AddServices(const Services: IServiceCollection);
begin
  Services.AddSingleton<IDatabase, TPostgresDatabase>;
  Services.AddScoped<IUnitOfWork, TDbUnitOfWork>;
end;
```

Registra todos los módulos desde el punto de entrada de la aplicación:

```pascal
var Services: IServiceCollection := TServiceCollection.Create;
TInfrastructureModule.Create.AddServices(Services);
TApplicationModule.Create.AddServices(Services);
var Provider := Services.BuildServiceProvider;
```

Con `Hosting`, los módulos se cargan automáticamente — consulta la documentación del módulo Hosting.

---

## Resolución diferida (Lazy)

Usa `Lazy.From<T>` para diferir la construcción hasta el primer uso. Útil cuando:
- El servicio es costoso de crear y puede que no se necesite
- Hay que romper dependencias circulares

```pascal
uses Daf.Extensions.DependencyInjection;

Services.AddTransient<IExpensiveService>(
  function(P: IServiceProvider): IExpensiveService
  begin
    Result := Lazy.From<IExpensiveService>(P,
      function(P2: IServiceProvider): IExpensiveService
      begin
        Result := TExpensiveService.Create;
      end);
  end);
```

El proxy devuelto implementa `IExpensiveService` mediante `TVirtualInterface`. La instancia real se crea en la primera llamada a un método.

---

## Decorador Duck Typing

`TDuckDecorator<T>` envuelve cualquier objeto e intercepta todas las llamadas a métodos. Úsalo para añadir comportamiento transversal (logging, caché, métricas) sin herencia:

```pascal
type
  TLoggingDecorator = class(TDuckDecorator<IMyService>)
  protected
    procedure BeforeCall(Method: TRttiMethod; const Args: TArray<TValue>;
      var Result: TValue); override;
    procedure AfterCall(Method: TRttiMethod; const Args: TArray<TValue>;
      var Result: TValue); override;
  end;

procedure TLoggingDecorator.BeforeCall(Method: TRttiMethod; ...);
begin
  Logger.Log('Llamando: ' + Method.Name);
end;

// Registrar como decorador
Services.AddTransient<IMyService>(
  function(P: IServiceProvider): IMyService
  begin
    var Inner: IMyService := TMyServiceImpl.Create;
    Result := TLoggingDecorator.Create(Inner);
  end);
```

---

## Integración con Hosting

Con el módulo `Hosting`, el `IServiceCollection` lo proporciona automáticamente el `IHostBuilder`. Nunca llamas a `BuildServiceProvider` directamente:

```pascal
uses Daf.Application.Builder, Daf.Extensions.DependencyInjection;

TApplication.CreateBuilder
  .ConfigureServices(procedure(Services: IServiceCollection)
  begin
    Services.AddSingleton<IMyService, TMyService>;
  end)
  .Build
  .Run;
```

El `IServiceProvider` queda accesible a través de `IHost.Services` o mediante inyección en los servicios alojados.

Consulta [Hosting](../../Hosting/docs/GUIDE.es.md) para la guía completa de integración.
