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
- 💤 **Lazy resolution** — the consumer declares `ILazy<T>` in its constructor and the service is built on first use, with its registration untouched
- 🦆 **Duck Typing** — `TDuckDecorator<T>` wraps any object implementing the same interface shape
- 📦 **Zero external deps** — `DependencyInjection.Abstractions` and `Commons`, both DAF's own

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

## Keyed services

Two implementations of the same contract, told apart by a key. Same names as
`Microsoft.Extensions.DependencyInjection`: `AddKeyedSingleton`, `GetRequiredKeyedService`,
`RemoveAllKeyed`.

```pascal
Services.AddKeyedSingleton<IStorage, TBlobStorage>(TValue.From<TStorageKind>(Blob));
Services.AddKeyedSingleton<IStorage, TFileStorage>(TValue.From<TStorageKind>(FileSystem));

var Blobs := Provider.GetRequiredKeyedService<IStorage>(TValue.From<TStorageKind>(Blob));
var Missing := Provider.GetKeyedService<IStorage>('nobody');   // nil
```

`AddKeyedSingleton`, `AddKeyedScoped` and `AddKeyedTransient` come in the same forms as their `Add*`
— by class, by factory and by instance — and each has its `TryAddKeyed*`.

**An enumerated key is the recommended form**, and the reason is the compiler: renaming a member
propagates to every use and a typo is an error. `'bolb'` compiles.

A key can be a **string**, an **integer**, a **boolean**, a value of an **enumerated type**, a
**metaclass** or a **`PTypeInfo`**. Two keys are the same when their **type and value** match, so two
enumerated values of different types that share an ordinal are different keys, and the string `'42'`
is not the integer `42`. Anything else — a record, an object — is rejected where it enters, naming
the type: the container cannot define an equality that those types do not have.

⚠️ **A character is not a key.** A one-character key is a string key: use a string.

### The two spaces are isolated

A registration made **with** a key is not reachable **without** one, and the other way round. A
contract with a key is a different contract:

- `GetServices<T>` does not include the keyed registrations, and `CanResolve` without a key answers
  only for the keyless ones.
- `Replace` and `RemoveAll` reach only the keyless ones. `RemoveAllKeyed` takes the key.
- `TryAdd*` asks about the **pair**, not the type: one key does not block another, and neither
  blocks the keyless registration.
- `Decorate` wraps the keyless registrations and leaves the keyed ones alone. Decorating a type that
  has *only* keyed registrations fails saying nothing is registered for it.

⚠️ **BREAKING** — `RemoveAll` used to remove everything registered for the type, because there were
no keys. It now removes the keyless ones only.

### Asking for a key from a constructor or a property

The same mark serves both positions, and the key travels inside it:

```pascal
type
  TReportService = class(TInterfacedObject, IReportService)
  private
    FStorage: IStorage;
  public
    constructor Create([DafInject('audit')] const Audit: IAuditLog);
    [StorageInject(FileSystem)]
    property Storage: IStorage read FStorage write FStorage;
  end;
```

`[DafInject]` takes a string, an integer, a boolean, a metaclass or a `TypeInfo(...)`. For an
**enumerated** key the application declares its own mark, in one line:

```pascal
type
  StorageInjectAttribute = class(DafInjectAttribute<TStorageKind>);
```

The container recognises it **by descent**, so it never needs to know that enumeration.

⚠️ **That mark goes in a unit of its own**, separate from the classes that use it. Declared in the
same unit, reading the attribute aborts with an access violation.

⚠️ **And the generic is never written inside a `[ ]`.** `[DafInject<TStorageKind>(...)]` *compiles*,
with a `W1074 Unknown custom attribute` almost nobody reads, and at run time the property has no
attribute at all: the injection does not happen and nothing says so.

For a one-off enumerated key without declaring a mark there is an escape,
`[DafInject(TypeInfo(TStorageKind), Ord(FileSystem))]`. The ordinal is checked against the range of
the type, and one that does not belong fails naming the property.

### One registration for any key

A registration under `ServiceKey.Any` serves every key of its service type that has no registration
of its own, and its factory is told which one was asked for:

```pascal
Services.AddKeyedSingleton<IStorage>(ServiceKey.Any,
  function(P: IServiceProvider; const Key: TValue): IStorage
  begin
    Result := TStorage.Create(Key.ToString);
  end);
```

A concrete registration beats it — the wildcard is asked only when the direct lookup fails — and each
key it serves gets its own instance. The sentinel is a value of a type declared inside the DI module,
so nobody outside can build it by accident.

⚠️ **A keyed registration cannot leave its generic arguments unbound.** The two matchings are not
combined, and the registration is rejected on the spot.

---

## Property injection

**The constructor is the default, and this does not replace it.** Two cases need property injection:

- **Two services that need each other.** A cycle by constructor is unbuildable — neither constructor
  can run before the other finishes. By property it resolves.
- **An instance the container does not build.** A form is built by the platform, so there is no
  constructor for the container to choose.

Each property declares itself, and a class takes part by having one — there is nothing
else to declare:

```pascal
type
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
filled aborts naming the property, its type and the class.

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

⚠️ **A trimmed property RTTI switches this off, and nothing says so.** `{$RTTI EXPLICIT
PROPERTIES([])}` leaves the properties of a unit out of the RTTI, and then a marked one does not exist
for the sweep: the class looks like any other that marks nothing, so it resolves with its dependency
left `nil` and the failure appears at the first use. The container cannot detect it. The directive is
written by hand at the top of that same unit, and the way out is wiring by code, which uses no RTTI.

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
