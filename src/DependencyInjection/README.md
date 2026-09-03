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

## Property injection

**The constructor is the default, and this does not replace it.** A dependency that enters through
the constructor is guaranteed by the compiler: the object cannot exist without it. One that enters
through a property is not, and that is paid at every read. Two cases justify the price, and only two:

- **Two services that need each other.** A cycle by constructor is unbuildable — neither constructor
  can run before the other finishes. By property it resolves.
- **An instance the container does not build.** A form is built by the platform, so there is no
  constructor for the container to choose.

The class declares that it takes part, and each property declares itself:

```pascal
type
  [DafInjectProperties]
  TReportService = class(TInterfacedObject, IReportService)
  private
    FStorage: IStorage;
  public
    [DafInject]
    property Storage: IStorage read FStorage write FStorage;
  end;
```

The property must be of an interface type and writable; whether a field or a method backs it makes no
difference, and neither does that field being `strict private`. A marked property that cannot be
filled aborts naming the property, its type and the class — a declared dependency left `nil` moves the
failure to the first use, somewhere else.

**The constructor does not see them.** Injection happens once the constructor has returned, so a
marked property is `nil` inside it. A dependency needed *during* construction goes through the
constructor; both routes live together in the same class.

### Wiring an instance the container did not build

```pascal
TActivatorUtilities.InjectInto(Provider, Self);   // in the form's constructor
```

Same marks, same requirements, same errors as a resolution. The container does **not** take ownership
of what it wires: it does not count it, closing does not destroy it, and the services it holds live as
long as it holds them.

### Wiring by code, with nothing marked

```pascal
Services.AddInjector<TReportService>(
  procedure(const Instance: TReportService; const Provider: IServiceProvider)
  begin
    Instance.Storage := Provider.GetRequiredService<IStorage>;
  end);
```

For what cannot be annotated: a class from another library, a unit whose property RTTI is trimmed —
this route uses no RTTI at all, so it is the only one immune — and a service key that is awkward to
write inside an attribute. It is a policy of the type, so it reaches descendants too, and several
wirings run in the order they were registered. It runs after the constructor and after the attribute
injection, and only on what the container builds.

⚠️ **A trimmed property RTTI hides the marks.** `{$RTTI EXPLICIT PROPERTIES([])}` leaves the
properties of a unit out of the RTTI, and then a marked one does not exist for the sweep. A class that
declares taking part and exposes no marked property fails saying so and pointing at the directives —
`ValidateOnBuild` reports it at startup, and the resolution reports it if validation is off.

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

## Threads

Once the provider is built, **resolving from several threads at once is safe**. Each registration is
built **once, on one thread**, and every thread that asks gets that same instance — so a constructor
does not have to be thread-safe.

What that guarantee covers is the container, not what comes out of it: a resolved service is as
thread-safe as its author made it.

**Composing is single-threaded.** Register on one thread, build the provider, and from then on the
collection is only read — registering after building fails saying it is frozen. There is no guarantee
against composing from two threads, and none is planned: it is the one-time setup of an application.

**A scope is a lifetime boundary, not a thread.** A scope can be used from any thread, and from
several at the same time, whoever created it. One scope per unit of work is still the recommendation,
but the reason is **the services**, not the container: a service with mutable state does not become
concurrent by living in a scope. And a scope per thread does **not** isolate singletons — the
singletons scope is one for all scopes, so two threads with their own scopes still share it.

**Closing is safe against resolutions in flight.** A `ShutDown` concurrent with a resolution ends in
one of two ways: the resolution completes and delivers its service, or it fails with
`EContainerShutDownError`. If it had already built its instance when it lost the race, that instance
is destroyed rather than handed out.

⚠️ **A constructor that resolves on another thread and waits for the result deadlocks.** The
construction of one registration is serialised, so the thread that waits is holding what the other
one needs. No container prevents this; resolve the dependency through the constructor instead.

---

## AppModule pattern

Organise registrations by module:

```pascal
type
  TMyModule = class(TAppModule)
  public
    procedure AddServices(const Services: IServiceCollection); override;
  end;

procedure TMyModule.AddServices(const Services: IServiceCollection);
begin
  Services.AddSingleton<IMyService, TMyService>;
  Services.AddTransient<IOther, TOtherImpl>;
end;
```

Applied by name, in the order written — `AddModule<T>` instantiates the module and calls its
`AddServices` on the spot, so nothing calls `AddServices` by hand:

```pascal
Services
  .AddModule<TMyModule>
  .AddModule<TOtherModule>;
```

See [DependencyInjection.Abstractions](../DependencyInjection.Abstractions/README.md) for the `IAppModule` interface and all core contracts.

---

## Documentation

- 📖 [Usage Guide](docs/GUIDE.md) — lifetimes, scopes, Lazy, DuckDecorator, integration with Hosting
- 📖 [Startup composition](docs/STARTUP-COMPOSITION.md) — modules and `AddXxx` functions, `TryAdd`/`Replace`/`Decorate`, `IStartupTask`, graph validation
- 📁 [ModuleComposition](samples/ModuleComposition/README.md) — three modules applied in order, and the one that only the instance overload can carry
