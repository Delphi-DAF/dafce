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
7. [Property Injection](#property-injection)
8. [Keyed Services](#keyed-services)
9. [AppModule Pattern](#appmodule-pattern)
10. [Lazy Resolution](#lazy-resolution)
11. [Duck Typing Decorator](#duck-typing-decorator)
12. [Integration with Hosting](#integration-with-hosting)

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

The container builds `TMyServiceImpl` through RTTI and **resolves the arguments of its
constructor**. That is constructor injection, and it is the default route:

```pascal
type
  TMyServiceImpl = class(TInterfacedObject, IMyService)
  public
    constructor Create(const Config: IConfig; const Audit: IAuditLog);
  end;

Services.AddSingleton<IConfig, TFileConfig>;
Services.AddSingleton<IAuditLog, TDbAuditLog>;
Services.AddSingleton<IMyService, TMyServiceImpl>;   // both arguments are resolved
```

Which constructor it picks:

- The candidates are the **public** constructors the class declares. One that declares none uses
  its ancestor's.
- Among the ones the container can satisfy, the one taking **most parameters** wins. Another one
  that asks for a service the winner does not ask for competes with it, and the resolution fails
  naming the class and both constructors.
- `[ActivatorUtilitiesConstructor]` only counts in `TActivatorUtilities.CreateInstance`, which takes
  the marked constructor as its only candidate; the container ignores it. It lives in
  `Daf.DependencyInjection.ActivatorUtilities`. Without a mark, `CreateInstance` fails when two
  constructors it can satisfy tie in number of parameters.
- A parameter nobody registered rules its constructor out, and so does an `ILazy<X>` whose `X`
  nobody registers. When that leaves none, the resolution fails naming the parameter, its type and
  the class.

A parameter can ask for a key — see [Keyed Services](#keyed-services) — and a dependency that
would close a cycle enters through a property instead; see
[Property Injection](#property-injection).

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
Services.Add(TServiceLifeTime.Transient, TypeInfo(IMyService),
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

### Checking what the container still owns

`IContainerAccess` publishes how many objects the container still owns: the singleton scope plus
every live scope. Transients are not counted — the container does not retain them, so for it they
stop existing the moment it hands one over.

It is reached by `QueryInterface`, not by resolving it as a service:

```pascal
uses System.SysUtils; // Supports

var Access: IContainerAccess;
if Supports(Provider, IContainerAccess, Access) then
  WriteLn(Access.OwnedInstanceCount);
```

Zero after `ShutDown` makes "everything was released" a fact a test asserts over any service, with
no leak detector and without naming the services one by one.

The count belongs to the container and not to the handle it is asked through: a scope reports the
same number as the provider it came from, because both reach the same scope factory. It is safe to
read while other threads create and close scopes.

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

> ⚠️ **Captive dependency**: a `Transient` or `Scoped` service injected into a `Singleton` is captured for the Singleton's lifetime. With `ValidateScopes`, which the host turns on in `Development`, a `Singleton` that resolves a `Scoped` fails with `EServiceProviderError`: by constructor, by factory, through an `ILazy`, or later through the provider it kept. Without the option, the `Scoped` it receives is the one the root provider gives. A captured `Transient` is not detected.

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

### A scope per request, from a thread pool

The pattern a server uses: one scope per unit of work, opened on the thread that handles it and
closed when it ends. Nothing has to be single-threaded for this to be correct.

```pascal
// One provider for the whole process, built once at startup.
procedure THandler.Handle(const Request: TRequest);
begin
  var Scope := FProvider.CreateScope;
  try
    var SP := Scope.ServiceProvider;
    // Scoped services live as long as this request; singletons are shared with every
    // other request, and the container built each of them exactly once.
    SP.GetRequiredService<IRequestHandler>.Run(Request);
  finally
    Scope := nil;
  end;
end;
```

Two things about this that are easy to get wrong:

- **The scope is not a thread boundary.** The scope above could be created on one thread and used
  on another, and several threads could share it. What a scope bounds is the **lifetime** of its
  Scoped services, nothing else.
- **A scope per request does not isolate singletons.** The singletons scope is one for the whole
  provider, so every request shares those instances. If a singleton holds mutable state, that
  state is shared — and no arrangement of scopes changes it.

Resolving concurrently is safe; the services you resolve are as safe as you wrote them. See
[Threads](../README.md#threads) for the full guarantee and its limits.

---

## Property Injection

A dependency can enter through a marked property instead of the constructor. The constructor stays
the default — what it takes is guaranteed to be there — and this covers the two cases it cannot.

### Two services that need each other

A cycle by constructor is unbuildable: neither constructor can run before the other one finishes. By
property it resolves, because the container publishes each instance before filling its properties.

```pascal
type
  TOrders = class(TInterfacedObject, IOrders)
  private
    FBilling: IBilling;
  public
    [DafInject]
    property Billing: IBilling read FBilling write FBilling;
  end;

  TBilling = class(TInterfacedObject, IBilling)
  private
    FOrders: IOrders;
  public
    [DafInject]
    property Orders: IOrders read FOrders write FOrders;
  end;

Services.AddSingleton<IOrders, TOrders>;
Services.AddSingleton<IBilling, TBilling>;

var Orders := Provider.GetRequiredService<IOrders>;
// Orders.Billing.Orders is the same instance as Orders
```

Both services must be `Singleton` or `Scoped`. A cycle that goes through a `Transient` is rejected:
the container does not retain transients, so the pair holds itself up and nothing can take it apart.
A cycle with any edge through a constructor is rejected too: the constructor cannot run before its
argument exists.

Both rejections name the whole path of the cycle. `ValidateOnBuild` reports every cycle in the graph
at startup; with validation off, the resolution that walks into one fails naming the path it walked.

⚠️ **During the resolution of a cycle, one participant receives another whose own properties are not
filled yet.** A constructor can assume nothing about a marked property.

Closing the container empties the injected properties of everything it wired before releasing any of
it, so a cycle is taken apart instead of leaking.

**Only what the container builds from a class is wired.** What a factory returns, and an instance
registered as such, belong to whoever built them: the container fills no property of theirs, runs no
code wiring on them and does not empty their properties when it closes. Their owner wires them on
request with `InjectInto`. When a service registered by class is decorated, the original reaches the
decorator already wired.

### Wiring an instance the container did not build

A form is built by the platform, so there is no constructor for the container to choose:

```pascal
constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  TActivatorUtilities.InjectInto(Provider, Self);
end;
```

Same marks, same requirements, same errors as a resolution. What is wired this way is **not** owned
by the container: it is not counted among its instances, closing does not destroy it and does not
empty its properties, and the services it holds live for as long as it holds them.

### Wiring by code

```pascal
Services.AddInjector<TReportService>(
  procedure(const Instance: TReportService; const Provider: IServiceProvider)
  begin
    Instance.Storage := Provider.GetRequiredKeyedService<IStorage>(BlobKey);
  end);
```

It is a policy of the type: it reaches every instance the container builds of that class or of one
descending from it, and several wirings run in the order they were registered. It runs after the
constructor and after the attribute injection, so it sees the marked properties already filled. It
does **not** reach an instance the container did not build — that one is wired by its owner with
`InjectInto`.

### The limit: a trimmed property RTTI

`{$RTTI EXPLICIT PROPERTIES([])}` leaves the properties of a whole unit out of the RTTI. For a class
in such a unit, a marked property **does not exist** for the sweep: taking part is deduced from
marking, so that class takes part in nothing. It resolves with no error, its dependency stays `nil`,
and the failure surfaces at the first use as an access violation.

⚠️ **The container cannot warn about it.** The cause is a directive written on purpose, at the top of
the very unit that holds the marked property.

Wiring by code is the way out: it uses no RTTI at all, so it is the only route immune to the trim.

---

## Keyed Services

Two implementations of the same contract, told apart by a key. `IStorage` for blobs and for files,
`INotifier` by SMS and by mail. Without it the choice moves to the consumer — ask for all of them and
pick by `is` — or the contract gets split in two interfaces that exist only to have different names.

```pascal
type
  // In a unit of its own -- see "The application's own mark" below
  TStorageKind = (Blob, FileSystem);

Services.AddKeyedSingleton<IStorage, TBlobStorage>(TValue.From<TStorageKind>(Blob));
Services.AddKeyedSingleton<IStorage, TFileStorage>(TValue.From<TStorageKind>(FileSystem));

var Blobs := Provider.GetRequiredKeyedService<IStorage>(TValue.From<TStorageKind>(Blob));
var Files := Provider.GetKeyedService<IStorage>(TValue.From<TStorageKind>(FileSystem));
var Every := Provider.GetKeyedServices<IStorage>(TValue.From<TStorageKind>(Blob));
```

Two registrations of the same type under different keys are **two registrations**: each has its own
instance and its own lifetime.

### What a key can be

A string, an integer, a boolean, a value of an enumerated type, a metaclass or a `PTypeInfo`. Two
keys are the same when their **type and value** match, which has three consequences worth stating:

```pascal
Provider.GetKeyedService<IStorage>('42');            // is NOT the key 42
Provider.GetKeyedService<IStorage>(TValue.From<Boolean>(True));  // is NOT the key 1
// and two enumerated values of different types that share an ordinal are different keys
```

Anything else — a record, an object — is rejected where it enters, naming the type: the container
cannot define an equality those types do not have. A `TGUID` is written as its string and loses
nothing. And a **character** is not a key: a one-character key is a string key.

**The enumerated key is the recommended form.** Not for style: renaming a member propagates to every
use, and a typo is a compile error. `'bolb'` compiles.

### The two spaces are isolated, in both directions

This is the rule that decides whether the feature helps or gets in the way:

```pascal
Services.AddKeyedSingleton<IStorage, TBlobStorage>('blob');

Provider.GetService<IStorage>;            // nil -- the keyed one is not reachable without a key
Provider.GetServices<IStorage>;           // empty -- it does not drag in the keyed ones
Provider.CanResolve(TypeInfo(IStorage));  // False

Services.AddSingleton<IStorage, TPlainStorage>;
Provider.GetKeyedService<IStorage>('blob');   // the blob one
Provider.GetRequiredService<IStorage>;        // the keyless one -- both live together
```

It reaches the destructive operations too, and that is where it matters most:

| Operation | Reaches |
|---|---|
| `Replace`, `RemoveAll` | only the keyless registrations |
| `RemoveAllKeyed(Type, Key)` | only the registrations under that key |
| `TryAdd*` | asks about the pair, so one key does not block another nor the keyless one |
| `Decorate` | wraps the keyless registrations, leaves the keyed ones alone |

⚠️ **`RemoveAll` changed meaning.** Before there were no keys, so "all of the type" and "all of the
type without a key" were the same thing. A test harness that used it to dismantle a registration now
dismantles less than it thinks — and finds out in green.

⚠️ **Decorating a type that has only keyed registrations fails** saying nothing is registered for it.
It is reading the keyless space, and the keyless space is empty.

### Asking for a key from a constructor or a property

The same mark serves both positions, and the key travels inside it — the model of Unity's
`[Dependency("name")]`:

```pascal
type
  TReportService = class(TInterfacedObject, IReportService)
  private
    FStorage: IStorage;
  public
    constructor Create([DafInject('audit')] const Audit: IAuditLog);
    [DafInject('file')]
    property Storage: IStorage read FStorage write FStorage;
  end;
```

A parameter or a property **without** a key keeps resolving without one, and therefore does not reach
a keyed registration: the isolation makes no exception for coming from a property.

### The application's own mark

`[DafInject]` takes a string, an integer, a boolean, a metaclass or a `TypeInfo(...)`. For an
**enumerated** key — the recommended one — the application declares its own mark in one line:

```pascal
unit MyApp.InjectionMarks;   // ⚠️ a unit of its own

interface

uses Daf.Extensions.DependencyInjection;

type
  TStorageKind = (Blob, FileSystem);

  StorageInjectAttribute = class(DafInjectAttribute<TStorageKind>);

implementation
end.
```

```pascal
[StorageInject(FileSystem)]
property Storage: IStorage read FStorage write FStorage;
```

The container recognises it **by descent**, so it never needs to know that enumeration, and the type
of the enumeration is preserved — another enumeration with the same ordinal does not reach it.

⚠️ **The mark goes in a unit of its own, separate from the classes that use it.** Declared in the same
unit, reading the attribute aborts with an access violation. The framework cannot detect it, and the
failure is loud but happens at run time.

⚠️ **The generic is never written inside a `[ ]`.** `[DafInject<TStorageKind>(...)]` **compiles**, with
a `W1074 Unknown custom attribute` almost nobody reads, and at run time `GetAttributes` returns
**zero**: the injection does not happen and nothing says so.

For a one-off enumerated key, without declaring a mark, there is an escape:

```pascal
[DafInject(TypeInfo(TStorageKind), Ord(FileSystem))]
```

The ordinal is checked against the range of the type. One that does not belong fails naming the
property.

### One registration for any key

`ServiceKey.Any` is the wildcard: it serves every key of its service type that has no registration of
its own, and its factory is told which one was asked for.

```pascal
Services.AddKeyedSingleton<IStorage>(ServiceKey.Any,
  function(P: IServiceProvider; const Key: TValue): IStorage
  begin
    Result := TFolderStorage.Create(Key.ToString);
  end);
```

- A **concrete registration beats it**: the wildcard is asked only when the direct lookup fails, the
  same order that already governs a closed type against an unbound generic one.
- **Each key it serves gets its own instance**, and asking twice for the same key gives the same one.
- **It does not cross into the keyless space**: with only a wildcard registered, `GetService<IStorage>`
  finds nothing.

The sentinel is a value of a type declared inside the DI module, so nobody outside can name it.

### The limit: keys and unbound generics are not combined

```pascal
Services.AddKeyedSingleton<IBox<_>, TBox<_>>('blob');   // fails at registration
```

It fails **when registering** and not when resolving.

---

## AppModule Pattern

Split registrations across logical modules using `IAppModule`; `TAppModule` is the base class that
implements it. The services and the modules live in a unit and not in the program file: a type
declared in a `.dpr` is in no unit's `interface` section, and resolving it aborts with
`ENonPublicType`.

The two blocks below are `samples/ModuleComposition/`, verbatim:

<!-- quickstart: src/DependencyInjection/samples/ModuleComposition/ModuleComposition.Services.pas -->

```pascal
unit ModuleComposition.Services;

// Los tipos van en una unit y no en el .dpr por obligacion del contenedor: un tipo declarado en el
// fichero de programa no esta en la seccion interface de ninguna unit, y resolverlo aborta en
// tiempo de ejecucion con ENonPublicType.

interface

uses
  Daf.Extensions.DependencyInjection,
  Daf.AppModule.DependencyInjection;

type
  IClock = interface(IInvokable)
    ['{0ED801D1-8529-4C2F-860F-472419E770B3}']
    function Today: string;
  end;

  TSystemClock = class(TInterfacedObject, IClock)
  public
    function Today: string;
  end;

  IGreeter = interface(IInvokable)
    ['{6803CD07-1D7D-4FA0-A890-65ED845860B1}']
    function Greet(const Name: string): string;
  end;

  TGreeter = class(TInterfacedObject, IGreeter)
  strict private
    FClock: IClock;
  public
    constructor Create(const AClock: IClock);
    function Greet(const Name: string): string;
  end;

  IBanner = interface(IInvokable)
    ['{E9A88893-3081-4181-8279-B189E6CF33DB}']
    function Text: string;
  end;

  TBanner = class(TInterfacedObject, IBanner)
  strict private
    FText: string;
  public
    constructor Create(const AText: string);
    function Text: string;
  end;

  // Los modulos se declaran en la interface para que el linker no los descarte: un tipo que solo
  // aparece en implementation y no se referencia se elimina como codigo muerto.
  TInfrastructureModule = class(TAppModule)
  public
    procedure AddServices(const ServiceCollection: IServiceCollection); override;
  end;

  // Su registro resuelve IClock cuando alguien pide el greeter, no al componer: para entonces
  // estan aplicados los tres modulos, asi que el orden entre este y el que aporta IClock da igual.
  TDomainModule = class(TAppModule)
  public
    procedure AddServices(const ServiceCollection: IServiceCollection); override;
  end;

  // Solo tiene constructor con parametro: es el caso que unicamente cubre la sobrecarga de
  // instancia, porque el modulo corre antes de que exista el proveedor.
  TBannerModule = class(TAppModule)
  strict private
    FText: string;
  public
    constructor Create(const AText: string);
    procedure AddServices(const ServiceCollection: IServiceCollection); override;
  end;

implementation

uses
  System.SysUtils;

{ TSystemClock }

function TSystemClock.Today: string;
begin
  Result := FormatDateTime('yyyy-mm-dd', Now);
end;

{ TGreeter }

constructor TGreeter.Create(const AClock: IClock);
begin
  inherited Create;
  FClock := AClock;
end;

function TGreeter.Greet(const Name: string): string;
begin
  Result := Format('Hola, %s. Hoy es %s', [Name, FClock.Today]);
end;

{ TBanner }

constructor TBanner.Create(const AText: string);
begin
  inherited Create;
  FText := AText;
end;

function TBanner.Text: string;
begin
  Result := FText;
end;

{ TInfrastructureModule }

procedure TInfrastructureModule.AddServices(const ServiceCollection: IServiceCollection);
begin
  ServiceCollection.AddSingleton<IClock, TSystemClock>;
end;

{ TDomainModule }

procedure TDomainModule.AddServices(const ServiceCollection: IServiceCollection);
begin
  ServiceCollection.AddSingleton<IGreeter>(
    function(Provider: IServiceProvider): IGreeter
    begin
      Result := TGreeter.Create(Provider.GetRequiredService<IClock>);
    end);
end;

{ TBannerModule }

constructor TBannerModule.Create(const AText: string);
begin
  inherited Create;
  FText := AText;
end;

procedure TBannerModule.AddServices(const ServiceCollection: IServiceCollection);
begin
  ServiceCollection.AddSingleton<IBanner>(TBanner.Create(FText) as IBanner);
end;

end.
```

Apply them from the app entry point, one `AddModule<T>` per module:

<!-- quickstart: src/DependencyInjection/samples/ModuleComposition/ModuleComposition.dpr -->

```pascal
program ModuleComposition;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.Extensions.DependencyInjection,
  Daf.DependencyInjection,
  ModuleComposition.Services in 'ModuleComposition.Services.pas';

begin
  var Services: IServiceCollection := TServiceCollection.Create;

  // AddModule<T> instancia el modulo y llama a su AddServices en el acto: lo que se escribe
  // despues de un modulo se registra despues de el.
  Services
    .AddModule<TInfrastructureModule>
    .AddModule<TDomainModule>;

  // Un modulo con configuracion no la puede recibir del contenedor —corre antes de que exista el
  // proveedor—, asi que la trae el llamante por la sobrecarga de instancia. Entregarlo como
  // IAppModule es lo que le da un dueño.
  var Banner: IAppModule := TBannerModule.Create('Composicion por modulos');
  Services.AddModule(Banner);

  var Provider := Services.BuildServiceProvider;

  Writeln(Provider.GetRequiredService<IBanner>.Text);
  Writeln(Provider.GetRequiredService<IGreeter>.Greet('DAF'));
end.
```

`AddModule<T>` instantiates the module, calls its `AddServices` on the spot, and owns its
lifetime. Do not call `TInfrastructureModule.Create.AddServices(Services)` by hand: the module is
a `TInterfacedObject` created with a reference count of zero, so nobody frees it.

A module carrying configuration cannot get it from the container — it runs before the provider
exists — so the caller builds it and hands it over as an `IAppModule`, which is what gives it an
owner. That is `TBannerModule` above.

**There is no automatic discovery**, with `Hosting` or without it: a module is registered where it
is written. The registration order, `TryAdd`/`Replace`/`Decorate` and the alternative of grouping
with a function are in [Startup composition](STARTUP-COMPOSITION.md).

---

## Lazy Resolution

**What it is for: two services that need each other.** A cycle by constructor is unbuildable —
neither constructor can run before the other one finishes. When one of the two sides declares the
other as `ILazy<T>`, its constructor stops waiting and the cycle closes on the first use instead.

**The consumer asks for it, and the registration does not change.** Declare `ILazy<T>` in the
constructor instead of `T`, and the container supplies it. The service is registered as any other,
and another consumer that asks for `T` itself keeps receiving it built.

```pascal
uses Daf.Extensions.DependencyInjection;

type
  TReportService = class(TInterfacedObject, IReportService)
  strict private
    FArchive: ILazy<IArchive>;
  public
    constructor Create(const AArchive: ILazy<IArchive>);
    procedure Publish;
  end;

constructor TReportService.Create(const AArchive: ILazy<IArchive>);
begin
  inherited Create;
  FArchive := AArchive;     // nothing is built here
end;

procedure TReportService.Publish;
begin
  FArchive.Value.Store(BuildReport);   // it is built here, on the first use
end;
```

```pascal
// The archive is registered as any other service. Nothing says "lazy" here.
Services.AddSingleton<IArchive, TArchive>;
Services.AddSingleton<IReportService, TReportService>;
```

**What `Value` does: it asks the container, every time.** Nothing is kept here, so the lifetime is
exactly the one the registration declared — a `Singleton` gives back the same instance on every
`Value`, a `Transient` a new one each time, a `Scoped` the one of its scope. Keeping a copy would
also keep the service alive through the holder that keeps the proxy, which is the very cycle this
exists to break.

**A collateral gain:** a service that is expensive and may not be needed is no longer built just to
build whoever holds it.

The other way to break a cycle, and the one to reach for when the dependency is used in every path,
is [property injection](#property-injection).

⚠️ **A service nobody registered is refused where the holder is built**, naming the parameter and
its class — not on the first `Value`, far from the cause. Deferring the construction does not defer
the question.

### `Lazy.From<T>`, the manual form

`Lazy.From<T>` builds the same kind of proxy from a factory you write, and returns `T` itself
rather than an `ILazy<T>`. It is the escape hatch for what does not go through a constructor; when
there is a constructor, `ILazy<T>` says the same with less, and leaves the registration alone.

⚠️ **This one does keep what it built**, unlike the one the container supplies: the factory is
yours and may cost whatever it costs, so running it on every method call would be a different
thing. It runs once even under several threads, and a factory that gives back nothing fails naming
the contract instead of answering the zero of the return type.

---

## Duck Typing Decorator

`TDuckDecorator<T>` wraps any object and intercepts all method calls. Use it to add cross-cutting behaviour (logging, caching, metrics) without subclassing:

```pascal
uses System.SysUtils; // Supports

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

// Register as decorator. The decorator is a TVirtualInterface: it implements the
// interface at run time, so it is handed over with Supports and not assigned.
Services.AddTransient<IMyService>(
  function(P: IServiceProvider): IMyService
  begin
    var Inner: IMyService := TMyServiceImpl.Create;
    Supports(TLoggingDecorator.Create(Inner), IMyService, Result);
  end);
```

---

## Integration with Hosting

When using the `Hosting` module, the `IServiceCollection` is provided by the application builder. You never call `BuildServiceProvider` directly:

```pascal
uses Daf.Cli, Daf.CommandLine, Daf.Configuration.CommandLine, Daf.Extensions.DependencyInjection;

var B := TCliApplication.CreateBuilder(CommandLineConfig.ProcessArgs);

B.Services.AddSingleton<IMyService, TMyService>;

ExitCode := B.Build.Run;
```

The `IServiceProvider` is then accessible through `IHost.Services` or via constructor injection in hosted services.

See [Hosting](../../Hosting/docs/GUIDE.md) for the full integration guide.
