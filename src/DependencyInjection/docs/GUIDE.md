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
  [DafInjectProperties]
  TOrders = class(TInterfacedObject, IOrders)
  private
    FBilling: IBilling;
  public
    [DafInject]
    property Billing: IBilling read FBilling write FBilling;
  end;

  [DafInjectProperties]
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

Both services must be `Singleton` or `Scoped`. A cycle that goes through a `Transient` is rejected —
not because it cannot be resolved, but because it cannot be undone: the container does not retain
transients, so the pair holds itself up and nothing can take it apart. A cycle with any edge through
a constructor is rejected too, and for the older reason: the constructor cannot run before its
argument exists.

Both rejections name the whole path of the cycle. `ValidateOnBuild` reports every cycle in the graph
at startup; with validation off, the resolution that walks into one fails naming the path it walked.

⚠️ **During the resolution of a cycle, one participant receives another whose own properties are not
filled yet.** That is unavoidable — the cycle has to be broken somewhere — and it is why a constructor
can assume nothing about a marked property.

Closing the container empties the injected properties of everything it owns before releasing any of
it, so a cycle is taken apart instead of leaking.

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
in such a unit, a marked property **does not exist** for the sweep: nothing would be injected and
nothing would say so. That is why taking part is declared on the class — a class attribute survives
the trim — and why a class that declares it and exposes no marked property is an error that names the
class and points at the directives.

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
`[Dependency("name")]`, rather than a second attribute that has to be written next to the first every
time and can be written on its own:

```pascal
type
  [DafInjectProperties]
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
unit, reading the attribute aborts with an access violation. It is a defect of the compiler and not a
matter of taste; the framework cannot detect it, and the failure is loud but happens at run time.

⚠️ **The generic is never written inside a `[ ]`.** `[DafInject<TStorageKind>(...)]` **compiles**, with
a `W1074 Unknown custom attribute` almost nobody reads, and at run time `GetAttributes` returns
**zero**: the injection does not happen and nothing says so. It is what anyone who sees the generic
declared will try first.

For a one-off enumerated key, without declaring a mark, there is an escape:

```pascal
[DafInject(TypeInfo(TStorageKind), Ord(FileSystem))]
```

The ordinal is checked against the range of the type. One that does not belong fails naming the
property, instead of producing a key that is nobody's and reading as "that service is not registered".

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

The sentinel is a value of a type declared inside the DI module, so nobody outside can name it. That
is the point: with a reserved string, whoever registered that string would become the wildcard without
noticing — the registration works, and what changes is who it serves.

### The limit: keys and unbound generics are not combined

```pascal
Services.AddKeyedSingleton<IBox<_>, TBox<_>>('blob');   // fails at registration
```

Two matchings crossed multiply the precedence cases, and this is not the change that takes them on.
It fails **when registering** and not when resolving, which is what makes it a limit and not a trap:
the alternative is a registration that is accepted and then never shows up.

---

## AppModule Pattern

Split registrations across logical modules using `IAppModule`; `TAppModule` is the base class that
implements it. The services and the modules live in a unit and not in the program file: a type
declared in a `.dpr` is in no unit's `interface` section, and resolving it aborts with
`ENonPublicType`.

The two blocks below are `samples/ModuleComposition/`, verbatim;
`eng/check-quickstart-snippet.py` compares them with the files, so this is code the compiler
reads:

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

  // Su registro resuelve IClock desde el proveedor, asi que necesita que el modulo que lo aporta
  // se haya aplicado antes.
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
  // IAppModule es lo que le da un dueno.
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

When using the `Hosting` module, the `IServiceCollection` is provided by the application builder. You never call `BuildServiceProvider` directly:

```pascal
uses Daf.Cli, Daf.CommandLine, Daf.Configuration.CommandLine, Daf.Extensions.DependencyInjection;

var B := TCliApplication.CreateBuilder(CommandLineConfig.ProcessArgs);

B.Services.AddSingleton<IMyService, TMyService>;

ExitCode := B.Build.Run;
```

The `IServiceProvider` is then accessible through `IHost.Services` or via constructor injection in hosted services.

See [Hosting](../../Hosting/docs/GUIDE.md) for the full integration guide.
