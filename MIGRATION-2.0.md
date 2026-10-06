# Migrating to DAF 2.0

This release is a **major**: some changes break compilation, and others change behaviour at run time
where the compiler says nothing. The list below covers both; step 6 of "How to do it" names the
run-time ones.

You upgrade from **1.6.0**. There was no published 1.7.0 — its content ships here —, so 1.6 to 2.0
is a single step.

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
`AddModules`.** That builder used to discover the modules of the application class's package on
its own. `TDafApplication` is **retired** in 2.0 — your entry point is your variant's builder,
further down — and no variant registers a module for you. Declare yours where you compose
services:

```pascal
Builder.Services
  .AddModule<TEnvironmentModule>
  .AddModule<TEvaluationModule>;
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

`KillAfterTimeout` is gone from `ISystemProcess` and from its builder: the deadline always kills the
process. Drop the call.

### 3. `Daf.CmdLn.Parser` is gone

The unit 1.6.0 shipped inside `Daf.Commons` **is no longer distributed**. Its replacement is
`Daf.CommandLine`, a port of .NET's
[`System.CommandLine`](https://learn.microsoft.com/dotnet/standard/commandline) — the library the
`dotnet` CLI itself is built on. There is **no adapter and no unit alias**: they are different APIs,
and existing `uses` clauses do not keep compiling.

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

var FileOpt    := CommandLine.Option<string>('--file').Description('File to compile');
var PortOpt    := CommandLine.Option<Integer>('--port').Description('Port to listen on');
var VerboseOpt := CommandLine.Option<Boolean>('--verbose').Description('Say more');

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
| `.Arg<T>('name')` | `CommandLine.Option<T>('--name')`, added with `Root.Add` — the dashes are written now |
| `.Arg<string>('output\|o')` | `CommandLine.Option<string>('--output', ['-o'])` |
| `A.Default(V)` | `.DefaultValue(V)`, and `.DefaultValueFactory(…)` for a default that has to be worked out |
| `A.Required(True)` | `.Required` |
| `A.Help('…')` | `.Description('…')` — and this one is **shown**: help is generated |
| `.Command('build')` … `.EndCommand` | `CommandLine.Command('build', description)`, hung with `Parent.Add(Child)` |
| `Params['x'].AsString` / `.AsInteger` / `.AsBoolean` | `XOpt.ValueIn(Parsed)`, typed by the option itself |
| `Params.GetSection('build')['output']` | `OutputOpt.ValueIn(Parsed)`: there is no section to navigate |
| `Params.Command = 'build'` | `Parsed.Command`, an `ICommand` — compare it against the object you declared |
| `Parser.Parse(…) = nil` | `Parsed.Succeeded` false. `Parsed.Errors` holds each error with the result of its symbol, and `Parsed.ErrorText` their text |

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
  `80xyz` is refused naming the option, and reading the option raises that error. If anything
  relied on that truncation, it gets an error where it used to get a number.
- **Dispatching can move into the grammar.** `Daf.CmdLn` handed back values and left dispatching to
  the application. In `Daf.CommandLine` each command can declare its action with `SetAction`, and
  `Invoke` on the parse result runs the one of the command reached and answers the exit code. Code
  that dispatches by hand on `Parsed.Command` keeps working.

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
| `&If.Exp<T>` | removed — Delphi 13's ternary operator, `if … then … else` as an expression, replaces it and evaluates only the branch it takes |
| `TGenericRTTI.GetGenericArguments(TRttiType)` | **`_T.GenericArgs(PTypeInfo)`**, in `Daf.Rtti` — returns type information rather than reflected types, and counts an argument that is itself generic as one |
| `TGenericRTTI.MakeGenericType` | removed with no replacement — see below |
| `TSmartArray<T>.Select(TDeferred<TMapper<T, TTarget>>)` | `Select(TFunc<T, TTarget>)` |

`Daf.Linq.Where` / `Filter` take `TFilterSpec<T>`, the type that already lived in Commons:
`Daf.Linq` does not introduce a filtering vocabulary of its own, it evaluates the one already
named.

`MakeGenericType` has no replacement because it promised something Delphi does not do: build a
generic instantiation at run time. It failed with an access violation even in the case it was
written for. If you were calling it to obtain a closed generic the compiler never saw, there is
nothing to migrate to — but if what you actually needed was one registration serving every closure
of a contract, that is now
[registering a generic contract once](src/DependencyInjection.Abstractions/README.md#registering-a-generic-contract-once).

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
- **`ILogger` gained `IsEnabled(Level)`, and this one breaks at compile time.** A logger now answers
  whether an entry of a given level would reach any destination, so a caller can skip building
  arguments that cost more than the line itself — a state dump, a query. A `TLogger`
  descendant inherits a default that answers `True`, so a logging provider written outside DAF keeps
  compiling untouched; only a type that implements `ILogger` **by hand** has to add it. Answer
  `True` if in doubt — dropping a line by not knowing is worse than composing one too many:

  ```pascal
  // add to any hand-written ILogger implementer
  function TMyLogger.IsEnabled(const Level: TLogLevel): Boolean;
  begin
    Result := True;  // or your own level test, matching what Log actually writes
  end;
  ```

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

### 8. The application family, and one builder style

This is the largest front of 2.0 for anything that had a `.dpr`.

- **`IHostBuilder` is gone, and with it the deferred style.** Registering through
  `ConfigureServices` / `ConfigureAppConfiguration` / `ConfigureHostConfiguration` callbacks no
  longer exists. There is one style, the immediate one: the builder is usable the moment it is
  created, and what you register happens where it is written.

  ```pascal
  // Before
  var Builder: IHostBuilder := THostBuilder.Create;
  Builder.ConfigureServices(
    procedure(Context: IHostBuilderContext; Services: IServiceCollection)
    begin
      Services.AddSingleton<IMyService, TMyService>;
    end);
  var Host := Builder.Build;

  // Now
  var B := TCliApplication.CreateBuilder(CommandLineConfig.ProcessArgs);
  B.Services.AddSingleton<IMyService, TMyService>;
  ExitCode := B.Build.Run;
  ```

  The machinery survives whole — `THostBuilder` drops to implementation detail. What changed is who
  calls it and when.

- **Your entry point is now your variant.** `TCliApplication` (`Daf.Cli`), `TWebApplication`
  (`Daf.Web`), `TVclApplication` (`Daf.VCL`) and `TFmxApplication` (`Daf.FMX`). They share
  `Services`, `Configuration`, `Environment`, `Logging`, `Properties` and `ServiceProviderOptions`
  through `IHostApplicationBuilder`.

- **`TDafApplication` and `TDafApplicationBuilder` are retired**, and so is the `Daf.Application`
  package. Their replacement is the variant from the entry above, and each member goes somewhere:
  `CreateHostBuilder` becomes your variant's `CreateBuilder`; `IDafApplication` becomes the
  `I…Application` that `Build` returns; `RunAsync` is `Run` under its own name;
  `VersionRequested` is `IParseResult.IsVersionRequested`, answered by the grammar; and
  `App` becomes `DafApp`, which exists **only in VCL and FMX** — in CLI and Web there is no
  application global on purpose.

  The template-method side of `TDafApplication` —`AppClass`, the virtual `Create(Host)`, `Execute`
  and `VersionInfo`— has no counterpart in the variants. In CLI the work is the root's action,
  declared with `B.SetAction`, which receives the `IServiceProvider`; what ran around it is a startup
  task (`AddStartupTask<T>`) or a hosted service; and the version is a `[Version]` attribute on any
  class.

- **The `try`/`except` around `Run` becomes a policy.** Catching what escapes, reporting it and
  choosing the exit code is `B.OnException(Policy)` in the CLI and Web variants, declared before the
  rest of the composition. The `except` of the `.dpr` only hands what escapes before `Run` to
  `B.HandleException(E)`, which passes it to that same policy and returns the exit code. `Daf.Cli`'s
  `GreetTool` sample shows the whole `.dpr`.

- **`Daf.Application.Version` moved to `Daf.Commons`.** Add `Daf.Commons` to `requires` if you used
  `TVersionInfo` and did not already. Two things changed in it:

  - `TVersionInfo.Debug` is read-only, and the binary declares it: a fourth argument to the attribute
    or to `TVersionInfo.Create`. Without it, `Debug` is `False` — in 1.6 it reported how DAF itself
    was compiled, whatever the configuration of your binary.
  - `VersionTag` takes a template instead of the `WithCompiledMeta` flag.

  ```pascal
  // before
  Info.Debug := {$IFDEF DEBUG}True{$ELSE}False{$ENDIF};
  Text := Info.VersionTag(True);

  // now
  [Version('2.9.0', 'MyApp', 'Acme', {$IFDEF DEBUG}True{$ELSE}False{$ENDIF})]
  TMyApp = class
  end;

  Text := TVersionInfo.GetFrom(TMyApp).VersionTag(VersionTemplateWithBuild);
  ```

- **`ILoggingBuilder` moved to `Daf.Logging.Abstractions`**, where .NET has it. Composing logging no
  longer requires the implementation package. And the builder exposes it as a live property — the
  pipeline is only composed **if you touch it**, so an application that never configures logging
  still starts.

- **The environment is now derived from the host configuration**, so it can be set on the command
  line as well as through a variable. Its keys have been renamed too: see section 20.

- **`IConfiguration` gained two members**, so a key that is absent can be told from one whose value
  is empty. This breaks **implementors** of the interface, not callers: the indexer still returns a
  `string`, and every existing expression through it still compiles.

  ```pascal
  if Config.Exists('Features:Beta') then ;                   // presence
  var Read := Config.Find('Features:Beta');                  // presence AND value, one lookup
  if Read.HasValue and (Read.Value = 'on') then ;
  var Value := Config['Features:Beta'];                      // '' when absent AND when empty
  ```

- **A failure reading a configuration file is now reported when the source is added**, not inside
  `Build` — because that is when the file is opened now. It still goes through the bootstrap
  channel and still propagates; what changed is which category signs it.

### 9. MiniSpec: the injection mark is renamed, and the injector holds the three contexts

- **`[Inject]` is now `[MiniInject]`.** `Inject` on its own is a name several tools claim, and two
  units exporting the same one in the same `uses` is not an error — the last one wins, in silence.
  The attribute travels in the facade now, so the extra `uses` goes away:

  ```pascal
  // Before
  uses Daf.MiniSpec, Daf.MiniSpec.Injection;
  ...
    [Inject]
    property Shared: TSharedContext read FShared write FShared;

  // Now
  uses Daf.MiniSpec;
  ...
    [MiniInject]
    property Shared: TSharedContext read FShared write FShared;
  ```

  Leaving `[Inject]` in place does **not** fail the build: the compiler emits `W1074 Unknown custom
  attribute` and at run time the member has no attribute, so nothing is injected and nothing says
  so. Search your specs for `[Inject]`.

- **The mark now works on a field too**, which is what the guide always showed and what never
  worked. A field is reached whatever its visibility; a property only when it is public or
  published.

- **`TInjectorService` is no longer a class-wide registry of loose services.** It is an instance
  owned by the run, and what it holds are the three contexts alive while a step runs. `Register`
  takes the scope, so a suite that registered mocks of its own does not compile:

  ```pascal
  // Before: any object, keyed by its class
  TInjectorService.Register(TDatabaseMock.Create);

  // Now: there is no loose registry. Build the collaborator in the World, or share it
  // through the feature or suite context, which is what gets injected.
  MiniSpec.UseSuiteContext<TTestResources>;   // TTestResources owns the mocks
  ```

- **When more than one context satisfies an `[MiniInject]` property, the winner is now declared**:
  exact type first — scenario, then feature, then suite — and only then a descendant, in that same
  order. It used to answer with the first entry that fitted according to a dictionary's internal
  order. This one does not break compilation: a World that receives a different context than before
  is one that had two of them fitting the same property, and until now which one arrived could not
  be predicted.

### 10. A scope now destroys what it created

- **Releasing a scope destroys the Scoped services it built.** The container used to retain every
  scope it handed out and release nothing until it was closed, so a process that opens a scope per
  request — the pattern the guide recommends — accumulated one instance per request for its whole
  life.
- **What breaks**: keeping a reference to a Scoped service without holding its scope. Until now it
  survived the scope it came from; now it does not.

```pascal
// Before: the cache outlived its scope by accident
var Scope := Provider.CreateScope;
FCache := Scope.ServiceProvider.GetRequiredService<ICache>;
Scope := nil;
FCache.Get('k');                       // now: the instance is already gone

// Now: hold the scope for as long as you hold what it built
FScope := Provider.CreateScope;
FCache := FScope.ServiceProvider.GetRequiredService<ICache>;
```

### 11. Property injection changes when an instance is published, and when it is cleared

- **The instance is published before its properties are filled**, for every registration, whether or
  not it takes part in property injection. That is what makes a cycle between two services
  resolvable: whoever re-enters to resolve the same service receives the instance already published.
- **What breaks**: a constructor that resolves its own service. It used to recurse without end; it
  now receives the instance half built.
- **Closing the container clears the injected properties** of everything it owns before releasing
  anything. Without that pass, two services that injected each other hold one another up and are
  never destroyed. A destructor that reads an injected property now reads `nil`.

```pascal
// Now: whatever the destructor needs, keep it in a field of its own
destructor TReportService.Destroy;
begin
  FLogger.Info('closing');             // FLogger is [DafInject]: nil by now
  inherited;
end;
```

### 12. `Replace` and `RemoveAll` only reach unkeyed registrations

Keyed services introduce a second space, and these two operations stay in the unkeyed one. Before
there were no keys, so "all of the type" and "all of the type without a key" were the same thing.

- **What breaks**: using them to dismantle a registration in a spec harness. They dismantle less
  than they used to, and you find out in green.

```pascal
Services.RemoveAll<IClock>;                                  // unkeyed registrations only
Services.RemoveAllKeyed<IClock>(TValue.From<string>('utc')); // the keyed one
```

### 13. A duplicate registration now yields two instances

Registering the same pair twice and then asking for all of them returned the first one twice: the
second registration was declared, was counted, and was never built.

- **What breaks**: a composition that registers a default and then registers it again instead of
  replacing it. `TryAdd*` registers only if absent, and `Replace` substitutes.
- Resolving a single service does not change — the last registration still wins — and neither does
  the order in which they are closed.

### 14. SQLCute: a query declares its kind

`IQuery` gains `Kind: TQueryKind`. It defaults to `Select` and is set by `AsInsert`, `AsUpdate` and
their siblings, instead of being deduced from the clauses present.

- **What breaks**: implementing `IQuery` yourself. The new member has to be provided.
- Declaring two kinds on the same query now keeps the last one, where the first used to win. A query
  with a single kind emits exactly what it emitted before.

### 15. MiniSpec: which context satisfies a `[MiniInject]` is now declared

- **The precedence is declared, and it was the internal order of a dictionary before.** Two passes:
  first an **exact** type match, walking scenario → feature → suite; and only if there is none, a
  descendant, in the same order. So an exact match in the suite beats a nearer descendant in the
  scenario — asking for the type you wrote is not a guess.
- **What breaks**: a step that injects a base type while two scopes hold compatible objects. It
  compiles, and it gets a different instance than before — the old winner depended on hash order,
  so it was never a decision. Name the exact type you want, or keep one of the two.

### 16. `csoReloadOnChange` is gone

`TConfigurationSourceOption` is left with `csoOptional`. The withdrawn value was never read by any provider, so nothing that a source did changes.

- **What breaks**: naming it. Drop it from the set — `[csoOptional, csoReloadOnChange]` becomes `[csoOptional]`, and a set that only carried it becomes `[]`.

### 17. NNLog: a target that cannot write no longer raises

A write that fails inside a target used to travel back to whoever logged. `TTarget.Write` now
catches it and reports it on the bootstrap channel, under the target's class name and naming the
target, so a line that never reaches its destination stops aborting the code that was only trying
to log.

- **What breaks**: a spec that asserts the propagation. It goes red, and that is the change
  arriving rather than a regression — there is nothing to restore on your side.
- **Where the failure shows now**: `BootstrapLog`, as a `LogError` carrying the exception and the
  target's name. A target whose failures have to be seen is watched there.
- **A custom target overrides `DoWrite`, not `Write`**, and this half the compiler does tell you:
  `Write` is no longer virtual. Move the body across and drop your own locking — `TTarget.Write`
  takes a critical section held by that target instance before calling `DoWrite`, so two threads
  writing through one target no longer overlap.

```pascal
// before
procedure TMyTarget.Write(const Entry: TLogEntry);
begin
  Send(RenderEvent(Entry));   // a failure here reached the caller
end;

// now
procedure TMyTarget.DoWrite(const Entry: TLogEntry);
begin
  Send(RenderEvent(Entry));   // a failure here is reported on the bootstrap channel
end;
```

### 18. Hosted services stop in reverse order

The `IHostedService`s stop in the reverse order of their registration — the order the container
releases in —, so a service started later stops before the one it may be using. In 1.6 they stopped
in registration order.

- **What breaks**: a service whose `Stop` counted on one registered after it having stopped already.
  Register the service that is used before the one that uses it.

### 19. The host loads `appsettings.json` and the environment by default

The builder loads `appsettings.json`, `appsettings.{Environment}.json`,
`{ApplicationName}.settings.json` and `{ApplicationName}.settings.{Environment}.json` from the
content root, all optional, and every environment variable, with `__` as the section separator, before the
command line: the sources of `Host.CreateApplicationBuilder`. In 1.6 the host loaded no source of
its own.

- **What breaks**: an environment variable whose name matches a configuration key now overrides it
  — `Db__Server` overrides `Db:Server` —, and an `appsettings.json` in the content root is read
  without being asked for. Remove the calls that loaded those two files by hand.

### 20. The host settings take .NET's names, and the content root is the current directory

The host reads its own settings under the keys of .NET's `HostDefaults` — `environment`,
`applicationName` and `contentRoot` —, as an argument or as a variable with the `DAF_` prefix,
which is stripped: `--environment Staging` and `DAF_ENVIRONMENT=Staging` set the same key.
`THostDefaults` replaces `TDafEnvVars`. And with no `contentRoot`, the content root is the current
directory, as in .NET, except in the system directory — where a Windows service starts —, which
keeps the folder of the executable.

```pascal
// before
TDafEnvVars.APP_ENV            // 'DAF_APP_ENV'

// now
THostDefaults.EnvironmentKey   // 'environment', and DAF_ENVIRONMENT as a variable
```

- **What breaks**: `DAF_APP_ENV`, `DAF_APP_NAME` and `DAF_CONTENT_ROOT` are no longer read, and
  nothing says so — an application started with `DAF_APP_ENV=Development` runs as `Production`.
  Rename them to `DAF_ENVIRONMENT`, `DAF_APPLICATIONNAME` and `DAF_CONTENTROOT`. An application
  started from another directory no longer finds the `appsettings.json` next to its executable:
  set `contentRoot`, or start it from its folder. And a log configuration that writes under
  `${contentRootPath}` now follows the current directory; write `${binPath}` to keep the log next
  to the executable.

### 21. Logging: an entry's message no longer carries the scope

`TLogEntry.Message` is the formatted message and nothing else; the open scopes travel only in
`TLogEntry.Scope`, innermost first, each prefixed with ` | `. In 1.6 the message carried them
appended, so every line showed them without asking.

- **What breaks**, and the compiler does not say it: an NNLog layout with `${message}` and no
  `${scope}` stops showing the scopes — the default layout among them —, and one with both stops
  writing them twice. `${message}${scope}` gives the line of 1.6; a target that relied on the
  default layout needs one of its own.

```
before  ${level} [${category}] ${message}
now     ${level} [${category}] ${message}${scope}
```

- **A provider of your own** that writes `Entry.Message` stops writing the scopes. Append
  `Entry.Scope`, which already brings its separator:

```pascal
// before
procedure TMyLogger.Log(const Entry: TLogEntry);
begin
  Send(Entry.Message);                 // the scopes came inside the message
end;

// now
procedure TMyLogger.Log(const Entry: TLogEntry);
begin
  Send(Entry.Message + Entry.Scope);   // empty outside any scope
end;
```

## How to do it

1. Compile. Whatever is missing comes out as a compiler error, not as odd behaviour at run time.
2. Replace each `AddModules` with one call per module, and drop the force-link `uses`.
3. Add the new packages you need to `requires` — `Daf.SystemProcess`, and `Daf.CommandLine` if
   you parsed a command line — and remove `Daf.CmdLn` and `Daf.SQLCute.Compilers`.
4. Rename the types and units from the tables above.
5. Compile again and run your tests. Then start the application once with `DAF_ENVIRONMENT=Development`:
   the host validates the container, and a registration that went missing stops the build naming it.
   A Scoped service asked of the root provider is rejected too, when it is resolved: resolve it from a
   scope the work opens, `Services.CreateScope`.
6. Sections 10 to 13, 15 and 17 to 21 are **run-time** changes: the compiler says nothing about them. Read them
   against your own composition — scope lifetimes, duplicate registrations, and anything a
   destructor reads.
