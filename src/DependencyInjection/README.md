# DependencyInjection

**🌍 Language: English | [Español](README.es.md)**

IoC/DI container for Delphi with a fluent API inspired by .NET's `Microsoft.Extensions.DependencyInjection`. Register services once, resolve them anywhere — with `Singleton`, `Scoped`, and `Transient` lifetimes.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Why use this?

- 🔌 **Familiar API** — Same `AddSingleton` / `AddTransient` / `AddScoped` pattern as .NET
- 🎯 **Generic & type-safe** — Compile-time verification via generics, no string tokens
- ⚡ **Three lifetime modes** — Singleton, Scoped (per scope), Transient (new every time)
- 🏭 **Flexible registration** — By class, by factory lambda, or by existing instance
- 💤 **Lazy resolution** — `Lazy.From<T>()` defers creation until first use
- 🦆 **Duck Typing** — `TDuckDecorator<T>` wraps any object implementing the same interface shape
- 📦 **Zero external deps** — Depends only on `DependencyInjection.Abstractions`

---

## Quick Start

```pascal
uses
  Daf.Extensions.DependencyInjection,
  Daf.DependencyInjection;

var
  Services: IServiceCollection;
  Provider: IServiceProvider;
begin
  Services := TServiceCollection.Create;

  // Register
  Services.AddSingleton<IMyService, TMyService>;

  // Build the container (freezes the collection)
  Provider := Services.BuildServiceProvider;

  // Resolve
  var Svc := Provider.GetRequiredService<IMyService>;
  Svc.DoWork;

  // Always shut down when done
  Provider.ShutDown;
end;
```

---

## Registration overloads

```pascal
// By implementation class
Services.AddSingleton<IMyService, TMyServiceImpl>;

// By factory lambda (full control)
Services.AddTransient<IMyService>(
  function(P: IServiceProvider): IMyService
  begin
    Result := TMyServiceImpl.Create(P.GetRequiredService<IConfig>);
  end);

// By existing instance (Singleton only)
Services.AddSingleton<IMyService>(MyExistingInstance);
```

---

## Scopes

```pascal
var Scope := Provider.CreateScope;
try
  var Svc := Scope.ServiceProvider.GetRequiredService<IScopedService>;
  // Scoped instances are unique within this scope
finally
  Scope := nil; // triggers Shutdown + managed cleanup
end;
```

---

## AppModule pattern

Organise registrations by module:

```pascal
type
  TMyModule = class(TInterfacedObject, IAppModule)
    procedure AddServices(const Services: IServiceCollection);
  end;

procedure TMyModule.AddServices(const Services: IServiceCollection);
begin
  Services.AddSingleton<IMyService, TMyService>;
  Services.AddTransient<IOther, TOtherImpl>;
end;
```

See [DependencyInjection.Abstractions](../DependencyInjection.Abstractions/README.md) for the `IAppModule` interface and all core contracts.

---

## Documentation

- 📖 [Usage Guide](docs/GUIDE.md) — lifetimes, scopes, Lazy, DuckDecorator, integration with Hosting
