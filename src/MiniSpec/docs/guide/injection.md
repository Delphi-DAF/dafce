# Dependency Injection with [MiniInject]

**🌍 Language: English | [Español](injection.es.md)**

[← Back to Guide](../GUIDE.md)

---

MiniSpec includes a lightweight dependency injection system for fields and properties marked with
`[MiniInject]`.

## Basic Usage

Automatic injection of FeatureContext:

```pascal
uses
  Daf.MiniSpec;  // MiniInject travels in the facade

type
  TFeatureContext = class
  public
    SharedValue: Integer;
  end;

  TWorld = class
  private
    [MiniInject] FCtx: TFeatureContext;  // Automatically injected
  public
    property Ctx: TFeatureContext read FCtx;
  end;

Feature('...')
  .UseFeatureContext<TFeatureContext>  // Registers TFeatureContext in Injector
  .UseWorld<TWorld>             // When creating World, injects FCtx
```

## Where the mark may go

On a **field** or on a **property**. A field is reached whatever its visibility, `strict private`
included; a property is reached only when it is public or published, which is what Delphi's default
RTTI emits for properties. Marking a private property has no effect and raises no error.

`TInjectorService` also needs the property to be writable. A field always is.

## The three contexts, and which one wins

An `[MiniInject]` property receives any of the three contexts alive while a step runs: the scenario's
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
| `InjectInto(Target)` | Injects contexts into `[MiniInject]` marked fields and properties |
| `Clear` | Empties the three scopes |

The runner calls `Register` and `Unregister` itself as each context is created and destroyed. There
is no registry of loose services: a collaborator a scenario needs is built in the World, or in the
feature or suite context if it has to be shared.

## Injection Errors

If a member marked with `[MiniInject]` cannot be injected, `EInjectionError` is raised:

- The member is not a class type
- The property has no setter
- No registered service compatible with type

The message names the member, its type and the class it belongs to.

---

[← Configuration](configuration.md) | [Next: Reporters →](reporters.md)
