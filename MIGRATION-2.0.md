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
| `Daf.CmdLn.Parser` | nowhere: withdrawn — see the next section |
| `Daf.SystemProcess` | package `Daf.SystemProcess` |

### 3. `Daf.CmdLn.Parser` is gone

The unit 1.6.0 shipped inside `Daf.Commons` **is no longer distributed**. Its replacement is
`Daf.CommandLine`, a port of .NET's
[`System.CommandLine`](https://learn.microsoft.com/dotnet/standard/commandline) — the library the
`dotnet` CLI itself is built on. There is **no adapter and no unit alias**: they are different APIs
on purpose, and a layer joining them would keep the very thing being replaced.

The same grammar, declared both ways:

```pascal
// before
uses Daf.CmdLn.Parser;

var Parser := TCmdLnParserBuilder.Create
  .Arg<string>('file')
  .Arg<Integer>('port')
  .Arg<Boolean>('verbose')
  .Build;

var Params := Parser.Parse(GetCommandLine);
if Params = nil then Exit;                    // nil was the whole report of a bad invocation

var FilePath  := Params['file'].AsString;     // 'file' written a second time, unchecked
var Port      := Params['port'].AsInteger;    // and the type chosen again, at run time
var IsVerbose := Params['verbose'].AsBoolean;
```

```pascal
// now
uses Daf.CommandLine;

var FileOpt    := CommandLine.Option<string>('--file', 'File to compile');
var PortOpt    := CommandLine.Option<Integer>('--port', 'Port to listen on');
var VerboseOpt := CommandLine.Option<Boolean>('--verbose', 'Say more');

var Root := CommandLine.Root('mytool', 'A tool');
Root.Add([FileOpt, PortOpt, VerboseOpt]);

var Parsed := Root.Parse;                     // no argument: reads ParamStr
if not Parsed.Succeeded then
begin
  Writeln(Parsed.ErrorText);                  // names the option and the text it received
  Exit;
end;

var FilePath  := FileOpt.ValueIn(Parsed);     // a string, statically
var Port      := PortOpt.ValueIn(Parsed);     // an Integer, statically
var IsVerbose := VerboseOpt.ValueIn(Parsed);
```

**Where the repeated name goes.** On the left, `'file'` is written once to declare and again to
read, and nothing checks that the two agree: a typo gives back an empty value rather than an error.
On the right the option object **is** the key, so the name exists once and asking for one that was
never declared is not representable. Reading an `IOption<Integer>` into a `string` is a **compile**
error, where `AsInteger` was a decision taken again at every read.

The mechanical part, term by term:

| `Daf.CmdLn` | `Daf.CommandLine` |
|---|---|
| `TCmdLnParserBuilder.Create` … `.Build` | `CommandLine.Root(name, description)` |
| `.Arg<T>('name')` | `CommandLine.Option<T>('--name', description)`, added with `Root.Add` — the dashes are written now |
| `.Arg<string>('output\|o')` | `CommandLine.Option<string>('--output', …).WithAlias('-o')` |
| `A.Default(V)` | `.WithDefault(V)`, and a computed form for a default that has to be worked out |
| `A.Required(True)` | `.Required` |
| `A.Help('…')` | the description, second parameter of `Option<T>` — and this one is **shown**: help is generated |
| `.Command('build')` … `.EndCommand` | `CommandLine.Command('build', description)`, hung with `Parent.Add(Child)` |
| `Params['x'].AsString` / `.AsInteger` / `.AsBoolean` | `XOpt.ValueIn(Parsed)`, typed by the option itself |
| `Params.GetSection('build')['output']` | `OutputOpt.ValueIn(Parsed)`: there is no section to navigate |
| `Params.Command = 'build'` | `Parsed.Command`, an `ICommand` — compare it against the object you declared |
| `Parser.Parse(…) = nil` | `Parsed.Succeeded` false, with `Parsed.Errors` and `Parsed.ErrorText` |

**What does not carry over unchanged.** Naming it because a migration that only shows the easy half
leaves you thinking you are done:

- **`Parse` no longer takes a whole command-line string.** `Parse` reads `ParamStr`, and
  `Parse(Args)` takes a `TArray<string>`. Code that handed it a string it had built — typically a
  test — passes the array instead.
- **Order stopped mattering, so some invocations that used to fail now work.** In `Daf.CmdLn` a
  command's arguments were consumed *before* its subcommand, so `app build -o dir --flag` did not
  parse. A `Recursive` option is read from the leaf, and that invocation now parses. Nothing that
  worked stops working; a test that asserted the failure will.
- **Two limitations of the old value engine are gone, and their outcome changed.** A negative number
  could not be passed at all, and `--port 80xyz` was accepted as `80`. Now `-5` is a value, and
  `80xyz` is refused naming the option. If anything relied on that truncation, it gets an error
  where it used to get a number.
- **There is no invocation.** Neither module dispatches to a handler: `Daf.CommandLine` parses and
  hands back typed values, and what to do with them stays the application's decision.

`Daf.Configuration.CommandLine` is untouched. Reading the command line as a **configuration
source** — `--Logging:LogLevel=Debug` overriding `appsettings.json` — is a different job and shares
no engine with either module.

### 4. Types renamed or moved out of `Daf.Commons`

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

### 5. Query modules renamed

| Before | Now |
|---|---|
| `Daf.Query.Expressions` | `Daf.Specification` |
| `Daf.SQLCute.Expressions` | `Daf.SQLCute.Specification` |
| `Daf.SQLCute.Compilers` | dissolved into `Daf.SQLCute` |

The new names state the pattern they implement. `Daf.SQLCute.Compilers` disappears as a package:
its six dialect compilers live in `Daf.SQLCute`, next to the ANSI one that was already there, so
there is one line less in `requires`.

### 6. Logging: the mute mode is gone

- **`DAFInternalLogger` removed.** It was the only route while the channel category was pinned to
  `DAF`; without that filter it is a second door that also labels as DAF whoever is not.
- **Composing the pipeline with no provider at all now fails when the host is built**, with
  `ELoggingCompositionError`, instead of handing back an `ILogger` that accepts everything and
  writes nothing. If your startup composed logging without providers and you had not noticed, this
  is what will tell you.
- Startup reports go out under the category of the class reporting, through the `ILogger` pipeline.
  That is not API, it is output: if you filtered on the `DAF` category, revisit the filter.

### 7. A closed container no longer answers

- **Resolving from a closed container raises `EContainerShutDownError`**, and the message names the
  service that was asked for. It used to do two different things and neither was that: an
  `EAccessViolation` if the service was a Singleton, and serving it silently if it was Scoped — an
  instance nobody was going to collect. `EContainerShutDownError` derives from
  `EServiceProviderError`, so whoever already catches the parent still catches it.
- **Creating a scope from a closed container fails the same way**, instead of handing out a scope
  that cannot resolve.
- **Closing twice no longer breaks.** If your code closed in two places — the application's explicit
  shutdown and the teardown of whoever owns the provider — and you avoided the second one with a
  flag, that flag is no longer needed.

The only thing to revisit is the last resolution before the shutdown:

```pascal
// Before: worked by accident if the service was Scoped
Provider.ShutDown;
var Report := Provider.GetRequiredService<IReport>;   // now: EContainerShutDownError

// Now: resolve while the container is still alive
var Report := Provider.GetRequiredService<IReport>;
Provider.ShutDown;
```

## How to do it

1. Compile. Whatever is missing comes out as a compiler error, not as odd behaviour at run time.
2. Replace each `AddModules` with one call per module, and drop the force-link `uses`.
3. Add the new packages you need to `requires` — `Daf.SystemProcess`, and `Daf.CommandLine` if
   you parsed a command line — and remove `Daf.CmdLn` and `Daf.SQLCute.Compilers`.
4. Rename the types and units from the tables above.
5. Compile again and run your tests. `ValidateOnBuild` helps confirm the composition registers what
   it did before.
