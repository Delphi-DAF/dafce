# Dependency Injection with [Inject]

**🌍 Language: English | [Español](injection.es.md)**

[← Back to Guide](../GUIDE.md)

---

MiniSpec includes a lightweight dependency injection system for properties marked with `[Inject]`.

## Basic Usage

Automatic injection of FeatureContext:

```pascal
uses
  Daf.MiniSpec,
  Daf.MiniSpec.Injection;  // For the [Inject] attribute

type
  TFeatureContext = class
  public
    SharedValue: Integer;
  end;

  TWorld = class
  private
    [Inject] FCtx: TFeatureContext;  // Automatically injected
  public
    property Ctx: TFeatureContext read FCtx;
  end;

Feature('...')
  .UseFeatureContext<TFeatureContext>  // Registers TFeatureContext in Injector
  .UseWorld<TWorld>             // When creating World, injects FCtx
```

## Custom Services at Suite Level

```pascal
MiniSpec
  .Before('Setup services', procedure
    begin
      TInjectorService.Register(TDatabaseMock.Create);
      TInjectorService.Register(THttpClientMock.Create);
    end)
  .After('Cleanup', procedure
    begin
      TInjectorService.Clear;  // Frees all services
    end);
```

## TInjectorService API

| Method | Description |
|--------|-------------|
| `Register(Instance)` | Registers a service (object's class is the key) |
| `Unregister(Instance)` | Removes a service registration |
| `Resolve(AClass)` | Gets registered service for that class |
| `Resolve<T>` | Generic version of Resolve |
| `InjectInto(Target)` | Injects services into `[Inject]` marked properties |
| `Clear` | Frees and removes all registered services |

## Injection Errors

If a property marked with `[Inject]` cannot be injected, `EInjectionError` is raised:

- Property is not a class type
- Property has no setter
- No registered service compatible with type

---

[← Configuration](configuration.md) | [Next: Reporters →](reporters.md)
