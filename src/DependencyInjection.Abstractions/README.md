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
| `Daf.Extensions.Options` | `IOptions<T>`, `TOptionsValidator<T>`, `EOptionsValidationError` — the options pattern contract |
| `Daf.Options` | `TOptions<T>` and `AppOptions` — settings registered with code |
| `Daf.AppModule` | `IAppModule` — module registration pattern |
| `Daf.DependencyInjection.ActivatorUtilities` | Helpers to construct objects via RTTI with DI-resolved arguments, and `InjectInto` to wire an instance the container did not build |
| `Daf.DependencyInjection.OpenGenerics` | Matching, checking and activation for registrations whose type arguments are left open |

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

### `ServiceKey` and `TServiceSlot`

`ServiceKey` is everything the container knows about a service key: whether a `TValue` can be one,
how two are compared, how one is named in a message, and which one is the wildcard.

```pascal
ServiceKey.IsValid(Key);   // a string, an integer, a boolean, an enumerated value,
                           // a metaclass or a PTypeInfo -- and the empty one, which means "no key"
ServiceKey.Check(Key);     // aborts naming the type when it cannot be one
ServiceKey.Text(Key);      // for a message, and for nothing else
ServiceKey.Any;            // the wildcard key
```

Two keys are the same when their **type and value** match, so two enumerated values of different
types that share an ordinal are different keys. The rejection happens where the key **enters** — at
registration and at resolution — and never when comparing two keys: a dictionary only consults the
comparison for keys that land in the same bucket, so a guard placed there does not run for a type
that collides with nothing, and the lookup answers "not found" in silence.

`TServiceSlot` is the pair `(service type, key)` the collection indexes by. An empty key **is** the
keyless space, not a third state.

### `TServiceKeyedFactory<T>`

```pascal
TServiceKeyedFactory<ServiceType: IInterface> =
  reference to function(Provider: IServiceProvider; const Key: TValue): ServiceType;
```

A factory told **which key** is being asked for. It is what makes a wildcard registration useful: the
only thing a factory serving every key can do is decide by which one it got. A separate type and not
an optional parameter of `TServiceFactory`, for the same reason as `TServiceFactoryEx`.

### `DafInject`

The mark of property injection: it goes on each property the container fills, and a class takes part
by having one. There is nothing to declare on the class.

```pascal
TReportService = class(TInterfacedObject, IReportService)
private
  FStorage: IStorage;
public
  [DafInject]
  property Storage: IStorage read FStorage write FStorage;
end;
```

The property must be of an interface type and writable. A marked property that cannot be filled
raises `EPropertyInjectionError` naming the property, its type and the class.

**One mark and not two**, and the alternative is worth naming because it was the earlier design: a
second attribute on the class survives `{$RTTI EXPLICIT PROPERTIES([])}`, which would let the
container say "this class takes part and I see no marked property". It was dropped because a mark you
must remember to write can be forgotten, and forgetting it leaves the properties unfilled with no
error — the same silence, moved from the rare case to the common one. What is lost is written down:
under a trimmed RTTI the injection is off and nothing says so.

The key travels **inside this mark**, and the same mark serves a **constructor parameter**:

```pascal
constructor Create([DafInject('audit')] const Audit: IAuditLog);
```

`[DafInject]` accepts a string, an integer, a boolean, a metaclass, a `TypeInfo(...)`, or a
`TypeInfo(...)` plus an ordinal. It cannot accept a `TValue`: an attribute argument has to be a
constant, and a `TValue` there gives `E2026` even for a plain string literal — the `TValue` is built
inside the constructor, which the RTTI calls when reading the attribute.

For an **enumerated** key the application declares its own mark over the generic base, in one line:

```pascal
type
  StorageInjectAttribute = class(DafInjectAttribute<TStorageKind>);
```

The sweep recognises it **by descent** (`Attr is DafInjectAttribute`), so the framework never needs to
know that enumeration. ⚠️ **That mark goes in a unit of its own**, separate from the classes that use
it: declared in the same unit, reading the attribute aborts with an access violation. ⚠️ **And the
generic is never written inside a `[ ]`** — `[DafInject<TStorageKind>(...)]` compiles with a `W1074`
and at run time the property has no attribute at all.

The name carries the `Daf` prefix on purpose: `Daf.MiniSpec.Injection` exports an `InjectAttribute` of
its own, and two units exporting the same name in the same `uses` is not an error — the last one wins,
in silence.

### `TServiceInjector<T>` and `AddInjector<T>`

Wiring by code, for what cannot be annotated:

```pascal
Services.AddInjector<TReportService>(
  procedure(const Instance: TReportService; const Provider: IServiceProvider)
  begin
    Instance.Storage := Provider.GetRequiredService<IStorage>;
  end);
```

It is a policy of the type — it reaches descendants too — and it runs after the constructor and after
the attribute injection, on every instance the container builds. It uses no RTTI, so it is the only
route immune to a trimmed property RTTI.

It is called `AddInjector` and not `AddInitializer` —the name the field uses— because it is the third
route of one feature, sharing its root with `DafInject` and `TActivatorUtilities.InjectInto`.
`AddInitializer` stays free for a general purpose post-construction hook, which is a different thing.

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

## Settings a service asks for by constructor

A service declares in its constructor the settings it needs; the container builds them, owns them
and frees them. Nobody writes a `Free`:

```pascal
uses Daf.Options;

AppOptions.Configure<TCacheSettings>(Services,
  procedure(S: TCacheSettings)
  begin
    S.Host := 'localhost';
    S.MaxItems := 500;
  end);

Services.AddSingleton<ICache, TCache>;
```

```pascal
constructor TCache.Create(const Options: IOptions<TCacheSettings>);
begin
  inherited Create;
  FHost := Options.Value.Host;
  FMaxItems := Options.Value.MaxItems;
end;
```

`AppOptions.Configure` fills the settings **with code** — no file, no environment variable, nothing
to read. Values computed at startup, or written by a test, come in through the same door as values
read from a file, and the service that receives them cannot tell where they came from. Filling them
from a configuration section is `ConfigOptions.Bind`, in
[`Configuration`](../Configuration/README.md).

The value is built on **first use** rather than when it is registered, and built **once**: composing
is not starting up, so settings the run never gets to use read nothing and validate nothing; and the
value is a mutable object, so two reads that returned different objects would make writing to it
mean different things depending on who asked.

### The value has an owner, and this is where it bites

`Value` hands back a **raw object**, and its owner is the option. Delphi has no garbage collector,
so a consumer that keeps the object and lets go of the option is left with a dangling pointer — a
hazard that does not exist where this pattern is ported from, which is exactly why it is said here
and not in a footnote.

Two correct ways to consume it, and no third:

```pascal
// hold the option in a field, and read Value whenever it is needed
FOptions := Options;

// or copy what you need in the constructor, which is what most services want
FHost := Options.Value.Host;
```

### Validation runs when the value is built, and names the setting

```pascal
AppOptions.Configure<TCacheSettings>(Services,
  procedure(S: TCacheSettings)
  begin
    S.MaxItems := Cfg.MaxItems;
  end,
  function(const S: TCacheSettings; out Error: string): Boolean
  begin
    Result := S.MaxItems > 0;
    if not Result then
      Error := Format('MaxItems must be greater than zero, and it is %d', [S.MaxItems]);
  end);
```

It runs once, when the value is built, and a failure raises `EOptionsValidationError` **naming the
setting**. The name is the requirement, not decoration: a `MaxItems` at zero that aborted saying
"invalid configuration" would send whoever reads it back through the whole file, which is the work
this pattern exists to remove.

### What nobody registered is not invented

Asking for settings nobody registered **fails naming the type**. This diverges from the framework
being ported, and it is written down as a divergence because whoever comes from there expects the
opposite: in .NET a single open registration serves every settings type at once, so asking for one
nobody configured hands back a freshly built object with everything at zero and says nothing.

The cause of the divergence is that this registration shape does not exist here — Delphi closes
generics at compile time, so a type the compiler never instantiated cannot be built afterwards — but
the outcome is the better one: a missing setting and a setting at zero stop being indistinguishable.

The same reasoning has a corollary worth saying in the same words: **`IOptions<TSection>` is not
derived from a parent settings class that has a property of that type.** .NET does not do it either.
The section is registered against its own path, and if both are registered, the parent's nested
object and the section's are **two distinct instances**.

### Settings with object members free what is theirs

A settings class with members of object type **frees them itself**. Options frees the root object
and nothing else. This is not a new burden — it is the rule the only nested settings class in the
repository already follows — but this is where it is written down. See
[Configuration's guide](../Configuration/docs/GUIDE.md), where the binder builds those members.

---

## Registering a generic contract once

A generic contract can be registered **with its type arguments left open**, and that one
registration serves every closure of it:

```pascal
Services.AddSingleton<IRepo<_>, TRepo<_>>;

// nothing else is registered, and both of these resolve
var Users  := Provider.GetRequiredService<IRepo<TUser>>;
var Orders := Provider.GetRequiredService<IRepo<TOrder>>;
```

`_` is the placeholder declared in `Daf.Rtti`. `IRepo<_>` is an ordinary closed instantiation
with a type of its own, so registering it needs nothing new — what the container learns is to
**match** when it resolves. Three rules follow:

- the lifetime counts **per closed type**: an open singleton is one instance per closure, not one
  in total
- a registration written for a concrete closure always wins over the open one
- name and arity identify the contract, so one of the same name and a different arity does not match

### `_ClosedArgs`, or how the implementation learns its closure

Delphi closes generics at compile time. Inside an implementation registered open, `T` **is** the
placeholder: `TypeInfo(T)` and `T.Create` are of no use there — the placeholder's constructor says
so out loud instead of quietly building one. Everything the implementation needs about the real
type arrives through a field of a fixed name that the container fills **before the constructor
runs**, because that is where a service decides most of what it is:

```pascal
type
  TRepoArgs = record
    T1: TEntityClass;                     // the bound, expressed by the field's type
  end;

  TRepo<T: class> = class(TInterfacedObject, IRepo<T>)
  private
    _ClosedArgs: TRepoArgs;
  public
    constructor Create;
  end;

constructor TRepo<T>.Create;
begin
  inherited Create;
  FTable := _ClosedArgs.T1.ClassName;     // TUser, TOrder, ...
end;
```

**Declaring it is optional.** An implementation that needs nothing from the closure — a null
implementation, a container that only holds — does not declare the field and pays nothing for it.
Not declaring it is exactly the same as declaring one position per type argument and typing every
one of them `TClass`: nothing to fill, and no bound to check. Note what that does *not* say — the
argument does not lose its real class, the closure still names it. What is missing is the
restriction.

Seven things to know about that field, and every one of them is a way to get it wrong:

- **It is read, and never assigned.** The `_` prefix means "somebody else operates this", as in
  `_AddRef` — here with the arrow the other way round: you declare it, the container writes it.
  Reading it is free and is what it is for.
- **Position binds; the name only documents.** Delphi emits nothing about the names of a generic's
  parameters, so the container binds the record's fields to the type arguments **in declaration
  order**. A record with its fields the other way round compiles and does the opposite.
- **The field's type is the bound.** `class of TEntity` says "only entities", and a closure that
  does not satisfy it is refused naming the argument and the bound it fails to meet.
- **`TClass` means no bound — and also no dispatch.** `SomeTClass.Create` calls `TObject.Create`
  and never the class's own constructor, not even a virtual one, because `TClass` is
  `class of TObject` and `TObject.Create` is not virtual. It hands back an object of the right
  class with its fields at zero, which reads like success. Type each field as narrowly as you can;
  where there really is no bound, build with `TActivator.CreateInstance`.
- **The record goes outside the class.** Inside it, `T1` is already the generic parameter:
  `E2004 Identifier redeclared`.
- **Its visibility is yours to choose, and `private` is the one to choose.** The container writes
  it and the class itself reads it; nobody outside has anything to do with it, so putting it on the
  surface of the type widens it for nothing. The default RTTI settings bound **methods and
  properties** to public and published — for **fields** they include all four visibilities — so the
  container finds it in the most restrictive one there is.
- **Mind where the field sits in the class.** In Delphi a `class var` block stays open until the
  next visibility specifier, so a field written after one is a class var too — invisible to RTTI,
  and shared by every closure. This is Delphi, not DAF, and it no longer fails the registration:
  now that declaring the field is optional, the container sees no field at all, writes nothing, and
  `_ClosedArgs` reads `nil` in the constructor. Put a visibility specifier between the two.

### What this is for, and what it is not

It serves **forwarders and containers that are agnostic about the type**. `ILogger<T>` is the
canonical case: it holds the real logger and its only relationship with the argument is using its
name as the category. It does not serve anything that needs the type *as a type* beyond what
`_ClosedArgs` can hand over.

The test takes a finger. **Cover every `T` in the body** of the implementation: if the code still
stands, it is registrable with its arguments left open. If something needed `T` to be `T` — a
`T.Create`, a `TypeInfo(T)`, a variable of type `T` holding a real value — it is not.

And three limits, said here rather than left to be tripped over:

- **All the arguments or none.** `IFoo<TKnown, _>`, fixing some and leaving others open, is not
  supported.
- **The argument has to travel as a reference.** A contract where `T` appears in a position whose
  representation depends on `T` — returned by value, a record, an integer — cannot be served this
  way. The container **compares the two contracts method by method and refuses**, naming the
  method: entering such a match would corrupt the stack rather than fail. It refuses whatever the
  implementation declares, `_ClosedArgs` or not — what makes a match safe is a property of the two
  contracts. To know in advance, run the finger test on the **contract** rather than on the
  implementation's body: read the signatures, and if `T` appears anywhere other than as an object,
  an interface or a class reference, the contract is not registrable with its arguments left open.
- **Matching is by name and arity, not by identity.** Two generic families with the same simple
  name and the same arity, declared in different units, match. What sits underneath is not nothing
  — that same method-by-method check refuses the match, naming the first method that does not fit
  — but it is a net, not a check of identity.

### A hand-written factory can be told what it is being asked for

The class route above fills `_ClosedArgs` before the constructor runs, so an implementation
registered open knows its closure. A **hand-written factory** registered open had nothing of the
sort: it received only the provider, so one factory served every closure and returned the same
object to all of them -- an `ILogger<TInvoices>` writing under another closure's category, with
nothing to complain about.

A second factory shape receives the type being resolved as well as the provider:

```pascal
Services.AddSingleton<IRepo<_>>(
  function(P: IServiceProvider; RequestedType: PTypeInfo): IRepo<_>
  begin
    Result := TRepo.Create(RequestedType);   // the closed IRepo<TUser>, IRepo<TOrder>, ...
  end);
```

The overload is chosen by how many parameters the lambda declares -- a one-parameter factory is
still a one-parameter factory, and nothing else changes. `AddScoped`, `AddTransient` and the three
`TryAdd*` have the same overload.

**It is the type being *asked for*, not the closure.** For an open registration it is the closed
type resolved (`IRepo<TUser>`); for an ordinary registration it is the service type itself
(`IRepo`). The datum means the same in both -- "this is what is being resolved" -- so a single
factory registered for several contracts can also tell them apart.

**Why a second type and not an optional parameter of the first.** Widening `TServiceFactory` with a
second, *optional* parameter looks additive and is the opposite. A default value serves the *call
site*, not assignment compatibility: a procedural type demands an identical parameter list, so a
one-parameter lambda assigned to the two-parameter type fails to compile with
`E2010 Incompatible types`. That would break every factory already written, in DAF and in every
consumer. The overload -- which looks like the larger change -- is the one that breaks nothing, and
the compiler picks it by the lambda's arity.

**A factory that does not ask is not a special case.** The short form is what almost every
registration uses, and most have nothing to ask. One that takes only the provider registers and
resolves exactly as before; registered open, it still serves the same object to every closure -- no
longer because it *cannot* know the closure, but because it did not ask for it.

---

## Dependency

This module has **no dependencies** on other DAF modules. It depends only on the Delphi RTL (`System.Rtti`, `System.Generics.Collections`).

The implementation is in [`DependencyInjection`](../DependencyInjection/README.md).
