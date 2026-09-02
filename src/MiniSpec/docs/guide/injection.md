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

## The three contexts, and which one wins

An `[Inject]` property receives any of the three contexts alive while a step runs: the scenario's
own World, the feature context declared with `UseFeatureContext<T>`, and the suite context declared
with `UseSuiteContext<T>`.

When more than one of them satisfies the type asked for, the winner is declared:

1. An exact type match, looking at scenario, then feature, then suite.
2. Failing that, a descendant of the type asked for, in that same order.

So a property declared with a base class two contexts descend from receives the one nearest to the
step, and a property declared with the exact type of the suite context receives that one even when a
feature context descends from it.

The injector belongs to the run: what it holds are the contexts of the run in progress, and it does
not outlive it. A nested run does not see the contexts of the one that launched it.

## TInjectorService API

| Method | Description |
|--------|-------------|
| `Register(Scope, Instance)` | Publishes the context of a scope: `csScenario`, `csFeature`, `csSuite` |
| `Unregister(Scope)` | Empties a scope |
| `Resolve(AClass)` | Gets the live context compatible with that class, by the precedence above |
| `Resolve<T>` | Generic version of Resolve |
| `InjectInto(Target)` | Injects contexts into `[Inject]` marked properties |
| `Clear` | Empties the three scopes |

The runner calls `Register` and `Unregister` itself as each context is created and destroyed. There
is no registry of loose services: a collaborator a scenario needs is built in the World, or in the
feature or suite context if it has to be shared.

## Injection Errors

If a property marked with `[Inject]` cannot be injected, `EInjectionError` is raised:

- Property is not a class type
- Property has no setter
- No registered service compatible with type

---

[← Configuration](configuration.md) | [Next: Reporters →](reporters.md)
