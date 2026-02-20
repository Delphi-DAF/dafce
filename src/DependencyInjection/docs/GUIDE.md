# DependencyInjection — Usage Guide

**🌍 Language: English | [Español](GUIDE.es.md)**

---

## Contents

1. [Concepts](#concepts)
2. [Service Registration](#service-registration)
3. [Building the Container](#building-the-container)
4. [Resolving Services](#resolving-services)
5. [Service Lifetimes](#service-lifetimes)
6. [Scopes](#scopes)
7. [AppModule Pattern](#appmodule-pattern)
8. [Lazy Resolution](#lazy-resolution)
9. [Duck Typing Decorator](#duck-typing-decorator)
10. [Integration with Hosting](#integration-with-hosting)

---

## Concepts

The DI container is built around three types:

| Type | Role |
|------|------|
| `IServiceCollection` | Registry — add service descriptors before building |
| `IServiceProvider` | Resolver — resolve services at runtime |
| `IServiceScope` | Lifetime boundary — group Scoped instances together |

`TServiceCollection` is the concrete implementation of `IServiceCollection`.  
Call `BuildServiceProvider` once to freeze the collection and get an `IServiceProvider`.

---

## Service Registration

### By implementation class

```pascal
Services.AddSingleton<IMyService, TMyServiceImpl>;
Services.AddTransient<IMyService, TMyServiceImpl>;
Services.AddScoped<IMyService, TMyServiceImpl>;
```

The container uses RTTI to construct `TMyServiceImpl` — it must have a public parameterless constructor, or you can inject via a factory.

### By factory lambda

Full control over construction:

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

### By existing instance (Singleton only)

```pascal
var Repo: IMyRepository := TMyRepository.Create(ConnectionString);
Services.AddSingleton<IMyRepository>(Repo);
```

### Low-level registration (PTypeInfo)

When generics are not available:

```pascal
Services.Add(TypeInfo(IMyService), TServiceLifeTime.Transient,
  function(P: IServiceProvider): IInterface
  begin
    Result := TMyServiceImpl.Create;
  end);
```

### Multiple registrations for the same interface

You can register the same interface more than once:

```pascal
Services.AddTransient<IPlugin, TPluginA>;
Services.AddTransient<IPlugin, TPluginB>;

// Resolve all of them
var Plugins := Provider.GetServices<IPlugin>;
for var P in Plugins do
  P.Execute;
```

---

## Building the Container

```pascal
var Provider := Services.BuildServiceProvider;
```

- `BuildServiceProvider` **freezes** the collection — further calls to `Add*` raise an exception.
- It can only be called **once** per `TServiceCollection` instance.
- The returned `IServiceProvider` is the root scope (Singletons live here).

**Always call `ShutDown` when done:**

```pascal
Provider.ShutDown;
```

This releases all managed singleton instances in reverse creation order.

---

## Resolving Services

| Method | Behaviour |
|--------|-----------|
| `GetService<T>` | Returns `nil` if not registered |
| `GetRequiredService<T>` | Raises `EServiceProviderError` if not registered |
| `TryGet<T>(out Service)` | Returns `False` if not registered (no exception) |
| `GetServices<T>` | Returns all registrations for `T` as `IInterfaceList<T>` |
| `CanResolve(TypeInfo)` | `True` if at least one registration exists |

```pascal
// Safe resolve
var Svc: IMyService;
if Provider.TryGet<IMyService>(Svc) then
  Svc.DoWork;

// Required resolve — exception if missing
var Svc := Provider.GetRequiredService<IMyService>;

// Resolve all
for var P in Provider.GetServices<IPlugin> do
  P.Execute;
```

---

## Service Lifetimes

| Lifetime | Created | Destroyed |
|----------|---------|-----------|
| `Singleton` | Once (first resolve) | On `Provider.ShutDown` |
| `Scoped` | Once per scope | On scope `nil` / `ShutDown` |
| `Transient` | Every resolve | When last reference is released |

```pascal
// Singleton — same instance always
Services.AddSingleton<ICache, TMemoryCache>;

// Scoped — new per scope (e.g., per HTTP request, per unit of work)
Services.AddScoped<IUnitOfWork, TEFUnitOfWork>;

// Transient — new every time
Services.AddTransient<IEmailSender, TSmtpSender>;
```

> ⚠️ **Captive dependency**: never inject a `Transient` or `Scoped` service into a `Singleton` — the shorter-lived instance will be captured for the Singleton's lifetime. The container does not detect this automatically.

---

## Scopes

Use scopes to group related Scoped services — for example, per request or per unit of work:

```pascal
var Scope := Provider.CreateScope;
try
  var SP := Scope.ServiceProvider;
  var UoW := SP.GetRequiredService<IUnitOfWork>;
  var Repo := SP.GetRequiredService<IRepository>;
  // UoW and Repo share the same scope; Scoped deps are shared between them
  UoW.Commit;
finally
  Scope := nil; // disposes all Scoped instances in this scope
end;
```

`IServiceScope` exposes only `ServiceProvider: IServiceProvider` — use that to resolve.

---

## AppModule Pattern

Split registrations across logical modules using `IAppModule`:

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

Register all modules from the app entry point:

```pascal
var Services: IServiceCollection := TServiceCollection.Create;
TInfrastructureModule.Create.AddServices(Services);
TApplicationModule.Create.AddServices(Services);
var Provider := Services.BuildServiceProvider;
```

When using `Hosting`, modules are loaded automatically — see the Hosting module docs.

---

## Lazy Resolution

Use `Lazy.From<T>` to defer construction until first use. Useful when:
- The service is expensive to create and may not be needed
- Circular dependencies need to be broken

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

The returned proxy implements `IExpensiveService` via `TVirtualInterface`. The real instance is created on the first method call.

---

## Duck Typing Decorator

`TDuckDecorator<T>` wraps any object and intercepts all method calls. Use it to add cross-cutting behaviour (logging, caching, metrics) without subclassing:

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
  Logger.Log('Calling: ' + Method.Name);
end;

// Register as decorator
Services.AddTransient<IMyService>(
  function(P: IServiceProvider): IMyService
  begin
    var Inner: IMyService := TMyServiceImpl.Create;
    Result := TLoggingDecorator.Create(Inner);
  end);
```

---

## Integration with Hosting

When using the `Hosting` module, the `IServiceCollection` is provided automatically by `IHostBuilder`. You never call `BuildServiceProvider` directly:

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

The `IServiceProvider` is then accessible through `IHost.Services` or via constructor injection in hosted services.

See [Hosting](../../Hosting/docs/GUIDE.md) for the full integration guide.
