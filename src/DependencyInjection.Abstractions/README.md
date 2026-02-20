# DependencyInjection.Abstractions

**🌍 Language: English | [Español](README.es.md)**

Core contracts (interfaces and types) for the DAF Dependency Injection system. This module contains **no implementation** — it defines the shape that consumers depend on and that `DependencyInjection` implements.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

---

## What's in this module

| Unit | Contents |
|------|----------|
| `Daf.Extensions.DependencyInjection` | `IServiceCollection`, `IServiceProvider`, `IServiceScope`, lifetimes, `Factory`, `TDuckDecorator<T>`, `Lazy` |
| `Daf.AppModule` | `IAppModule` — module registration pattern |
| `Daf.DependencyInjection.ActivatorUtilities` | Helpers to construct objects via RTTI with DI-resolved arguments |

---

## Key types

### `IServiceCollection`

Fluent builder for registering services before the container is built.

```pascal
Services
  .AddSingleton<ICache, TMemoryCache>
  .AddTransient<IRepo, TRepoImpl>
  .AddScoped<IUoW, TDbUoW>;

var Provider := Services.BuildServiceProvider;
```

### `IServiceProvider`

The resolved container. Use it to obtain service instances.

```pascal
var Svc := Provider.GetRequiredService<IMyService>;
var Opt: IMyService;
if Provider.TryGet<IMyService>(Opt) then ...
var All := Provider.GetServices<IPlugin>;
```

### `TServiceLifeTime`

```pascal
type TServiceLifeTime = (Singleton, Scoped, Transient);
```

### `IServiceScope`

A lifetime boundary. Scoped services are shared within a scope and released when the scope ends.

```pascal
var Scope := Provider.CreateScope;
var Svc := Scope.ServiceProvider.GetRequiredService<IScopedService>;
Scope := nil; // releases scoped instances
```

### `IAppModule`

Convention for grouping service registrations:

```pascal
type
  IAppModule = interface(IInvokable)
    procedure AddServices(const Services: IServiceCollection);
  end;
```

### `Factory`

Helper to build `TServiceFactory` from a class or instance:

```pascal
Services.Add(TypeInfo(IMyService), Singleton, Factory.From(TypeInfo(IMyService), TMyImpl));
```

### `TDuckDecorator<T>`

Base class for wrapping a service with pre/post-call logic without inheritance. Override `BeforeCall` / `AfterCall`.

### `Lazy`

```pascal
Result := Lazy.From<IHeavy>(Provider, function(P): IHeavy begin Result := THeavy.Create end);
```

---

## Dependency

This module has **no dependencies** on other DAF modules. It depends only on the Delphi RTL (`System.Rtti`, `System.Generics.Collections`).

The implementation is in [`DependencyInjection`](../DependencyInjection/README.md).
