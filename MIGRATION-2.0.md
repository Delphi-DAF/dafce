# Migrating to DAF 2.0

This release is a **major**: some changes break compilation. Every one of them shows up at compile
time — none degrades silently — and the list below covers all of them.

You upgrade from **1.6.0**. There was no published 1.7.0: it was planned, then cut as a major, and
its content ships here, so 1.6 to 2.0 is a single step.

## What to change, most likely first

### 1. Application modules are registered by name

`AddModules(Package)` — the RTTI discovery of `IAppModule` — **is gone**. Each module is applied by
name, in whatever order the application wants:

```pascal
// before
ServiceCollection.AddModules(_T.PackageOf<TSomething>);

// now
ServiceCollection
  .AddModule<TEnvironmentModule>
  .AddModule<TEvaluationModule>
  .AddModule<TSuiteModule>;
```

The `uses` marked `// force-link:` go with it — they only existed so the RTTI would see the module,
and naming the type in `AddModule<T>` already links its unit.

`IAppModule` and `TAppModule` **do not change**, and each module's body — its `AddServices` — is
untouched. What disappears is how they were found, not what they are.

⚠️ **If your application derives from `TDafApplication`, this affects you even if you never wrote
`AddModules`.** `TDafApplicationBuilder` used to discover the modules of the application class's
package on its own; now it registers none. Declare yours where you compose services:

```pascal
Builder.ConfigureServices(
  procedure(Context: IHostBuilderContext; Services: IServiceCollection)
  begin
    Services
      .AddModule<TEnvironmentModule>
      .AddModule<TEvaluationModule>;
  end);
```

**Why.** Under native compilation, discovery did not save the declaration: it hid it. For the RTTI
to see a class, its unit must be in the image, and the linker only keeps it if somebody wrote
`uses` — so the declaration was already there, in disguise. In exchange for saving nothing it added
two failure modes: a trimmed RTTI left discovery **blind in silence**, and the registration order
was decided by the reflection walk. `AddModule<T>` also honours the module's own constructor, which
discovery skipped without saying so.

### 2. `Daf.Commons` has been split

`Daf.Commons` now holds **only what extends the standard library**. What left has its own package,
and it must be added to the `requires` of whichever package uses it:

| What you used | Now lives in |
|---|---|
| `Daf.CmdLn.Parser` | package `Daf.CmdLn` |
| `Daf.SystemProcess` | package `Daf.SystemProcess` |

### 3. Types renamed or moved out of `Daf.Commons`

| Before | Now |
|---|---|
| `TExpresion<T>` | **`TDeferred<T>`**, in `Daf.Types` |
| `TFilterSpec<T>`, in `Daf.Expression` | same class, in `Daf.FilterSpec` |
| unit `Daf.Expression` | dissolved into `Daf.FilterSpec` and `Daf.Types` |
| `TBooleanExpresion` | removed |
| `TSmartArray<T>.Select(TDeferred<TMapper<T, TTarget>>)` | `Select(TFunc<T, TTarget>)` |

`Daf.Linq.Where` / `Filter` take `TFilterSpec<T>`, the type that already lived in Commons:
`Daf.Linq` does not introduce a filtering vocabulary of its own, it evaluates the one already
named.

### 4. Query modules renamed

| Before | Now |
|---|---|
| `Daf.Query.Expressions` | `Daf.Specification` |
| `Daf.SQLCute.Expressions` | `Daf.SQLCute.Specification` |
| `Daf.SQLCute.Compilers` | dissolved into `Daf.SQLCute` |

The new names state the pattern they implement. `Daf.SQLCute.Compilers` disappears as a package:
its six dialect compilers live in `Daf.SQLCute`, next to the ANSI one that was already there, so
there is one line less in `requires`.

### 5. Logging: the mute mode is gone

- **`DAFInternalLogger` removed.** It was the only route while the channel category was pinned to
  `DAF`; without that filter it is a second door that also labels as DAF whoever is not.
- **Composing the pipeline with no provider at all now fails when the host is built**, with
  `ELoggingCompositionError`, instead of handing back an `ILogger` that accepts everything and
  writes nothing. If your startup composed logging without providers and you had not noticed, this
  is what will tell you.
- Startup reports go out under the category of the class reporting, through the `ILogger` pipeline.
  That is not API, it is output: if you filtered on the `DAF` category, revisit the filter.

## How to do it

1. Compile. Whatever is missing comes out as a compiler error, not as odd behaviour at run time.
2. Replace each `AddModules` with one call per module, and drop the force-link `uses`.
3. Add the new packages you need to `requires` (`Daf.CmdLn`, `Daf.SystemProcess`) and remove
   `Daf.SQLCute.Compilers`.
4. Rename the types and units from the tables above.
5. Compile again and run your tests. `ValidateOnBuild` helps confirm the composition registers what
   it did before.
