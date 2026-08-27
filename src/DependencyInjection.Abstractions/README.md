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
