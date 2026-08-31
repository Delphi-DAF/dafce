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
