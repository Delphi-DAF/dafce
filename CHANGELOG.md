# Changelog

**🌍 Language: English | [Español](CHANGELOG.es.md)**

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html)
and [Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/).

## [Unreleased]

The changes that break an application, at compile time or at run time, are collected with their
example diff in `MIGRATION-2.0.md` (`MIGRATION-2.0.es.md` in Spanish).

### Added

- **DependencyInjection**: Keyed services — two implementations of the same contract told apart by a key, with .NET's `AddKeyed*` / `TryAddKeyed*` / `GetKeyed*` set
- **DependencyInjection**: Property injection — `[DafInject]` on the property and the container fills it once the constructor has finished, which makes **a cycle between two services resolvable**
- **DependencyInjection**: `TActivatorUtilities.InjectInto` — wires an instance the container did not build, such as a form the platform creates, without taking ownership of it
- **DependencyInjection**: `AddInjector<T>` — wiring in code, with no attributes and no RTTI, so it reaches a class from another library
- **DependencyInjection**: `IOptions<T>` — a service asks for its settings in the constructor and gets them built and validated, from `AppOptions.Configure<T>` or from a configuration section. Configurations of one type accumulate and its validations fail together, through the .NET contracts `IConfigureOptions<T>` and `IValidateOptions<T>`
- **DependencyInjection**: Open generics — `AddSingleton<IRepo<_>, TRepo<_>>` serves every closure of the contract. What it covers and what it does not, in the README
- **DependencyInjection**: `ValidateOnBuild` and `ValidateScopes` on `BuildServiceProvider(TServiceProviderOptions)` — the graph is checked at build time and every failure is reported together. Factory registrations are left out. The family's host turns both on in the Development environment, and `ServiceProviderOptions` on the builder fixes them by hand. With `ValidateScopes`, a Scoped resolved from the root provider or from a Singleton is also rejected, when it is resolved
- **DependencyInjection**: Improvements to composing the collection
  - `TryAddSingleton` / `TryAddScoped` / `TryAddTransient`, `Replace` and `RemoveAll`, with .NET's semantics
  - `Descriptors` — a service's registrations, read-only
  - `Decorate<S>` — wraps the existing registrations keeping their lifetime
  - `AddModule<T>` and `AddModule(Module)` — registrations grouped in a class, applied in the order written
  - `IContainerAccess.OwnedInstanceCount` — how many objects the container still owns, to assert "everything was released"
- **DependencyInjection**: `ILazy<T>` — breaks a cycle between two services that need each other: one of the two declares the other in its constructor as `ILazy<T>` and its constructor stops waiting. The registration does not change, and `Value` asks the container on every use, so the lifetime stays the declared one
- **Logging**: `ILogger.IsEnabled(Level)` — asking whether an entry of that level would reach any sink before building its arguments
- **Logging**: `ILogger<T>` — a service asks for it by constructor and gets a logger carrying its own category
- **Logging**: Bootstrap channel — an `ILogger` from the process's first line, with no dependency injection and no configuration, for the phase before `BuildServiceProvider`: report with `BootstrapLog.LoggerFor` and add sinks with `BootstrapLog.AddSink`. The sink catalogue, in the README
- **Logging**: `Builder.AddNullProvider` — discarding entries on purpose, stated while composing
- **Configuration**: A new provider, the command line — `CommandLineConfig.AddArgs`, in `Daf.Configuration.CommandLine`, reads the process arguments as keys. The accepted forms, in the README
- **Configuration**: `ConfigOptions.Bind<T>` — a section arrives typed and owned to whoever needs it, and the service stops naming its section: startup decides it
- ⚠️ **Configuration**: `IConfiguration.Exists` and `Find` — presence is **asked**, not inferred from an empty value; `Find` answers presence and value in one lookup, as a `TNullable<string>`. The indexer stays a `string`. **Breaks** whoever implements `IConfiguration` without inheriting from `TConfigurationRoot`
- **Configuration**: `IConfigurationManager` — builder and reader on the same object, so configuration can be read while it is still being composed. Each `Add` builds and loads only the new provider
- **Configuration**: `ConfigurationKeyName` — a member names the configuration key it is read from, so a key a file writes as `connection-string` reaches a property Delphi cannot name that way. It replaces the member name, renames the whole section on a member of object type, and applies to properties and fields alike
- **Hosting**: `IStartupTask` and `AddStartupTask<T>` — initialization that runs once at startup, with the provider already built and before the `IHostedService`s. If one fails, startup aborts
- **Hosting**: `THost.CreateBuilder` and `IGenericHostBuilder` — the generic host's entry point, in the family's shape: `var Builder := THost.CreateBuilder;`. `THostBuilder.Create` still exists, and chaining off it still leaks the instance
- **Hosting**: `StopAndNil` — stops the host and empties the reference, which after `Host.Stop` pointed at a released host
- **Application**: The application family — `TCliApplication` (`Daf.Cli`), `TVclApplication` (`Daf.VCL`), `TFmxApplication` (`Daf.FMX`) and `TWebApplication` (`Daf.Web`): the same startup up to where the platform takes over the loop, and around `Run` the exit code and the version metadata. The CLI variant runs the action of the command reached, declared with `B.SetAction`, and a service asks for the parse result (`IParseResult`) by constructor; everything that escapes, from the composition to the shutdown, goes to one policy, `OnException`; `B.ParseArgs(Args)` gives the grammar its input and lets the composition decide with the parse result, while the builder's arguments go to the configuration; and `B.SetStandaloneAction` declares a command that runs without a host. The Web variant decides what escapes with the same policy, and its `Run` answers how it ended; VCL and FMX declare a single instance with `SingleInstance`. FMX adds no targets: they are still Win32 and Win64
- **MiniSpec**: `TSpecRun` — a run with its own suite, context, injector and runner, launchable inside another without disturbing its counts or its report
- **MiniSpec**: `--allow-undefined` — an undefined step stops setting the exit code; `MiniSpec.AllowUndefined(True)` declares it in the suite. By default it breaks
- **MiniSpec**: `SpecContext.ScenarioContext` returns the scenario's World, and a `[MiniInject]` of its type receives it
- **MiniSpec**: `Arg.Matching<T>` — a matcher that takes the condition itself, so a range or any other check is written where the setup is. The argument reaches the predicate already converted to `T`; one of another type does not match
- **NNLog**: `TNNLogLoader.BuildProvider` accepts a `TNNLogConfiguration` — settings written in code, not only read from a section
- **NNLog**: `${scope}` layout token — the scopes that `BeginScope` opens reach the written line; add it to a target's `layout`
- **SQLCute**: Improvements to the query builder
  - `WhereNot(Callback)` and `OrWhereNot(Callback)` — WHERE groups negated as a unit, `NOT (…)`
  - Per-column NULL ordering — `OrderBy` / `OrderByDesc` with `TNullsOrder`, and `NullsFirst` / `NullsLast`
  - An alias written in a column name —`Select('users.name as n')`— is split off and quoted, also in memory
  - `OrHaving` — a HAVING condition joined to the previous one with OR
  - `EscapeChar` in the LIKE family — makes literal the `%` or `_` it precedes, with `ESCAPE '<c>'` in the SQL
- **Commons**: New utilities
  - `TNullable<T>` in `Daf.Nullable`
  - `TDeferredEnumerable<T>` and `TGeneratorEnumerator<T>` in `Daf.Enumerable`
  - `Daf.Comparers` — NULL-aware comparers for `TValue` and `TNullable<T>`
- **Daf.CommandLine**: New module — a port of .NET's `System.CommandLine`: a grammar declared once and read back with the objects that declared it, an action per command, declared with `SetAction` and run by `Invoke`, and .NET's help, parse rules and texts, which `CommandLine.Texts` replaces with the application's. A parse error answers 2, not 1; the other divergences, in its README
- **Daf.Linq**: New module — the LINQ-to-Objects equivalent: lazy sequences over `System.IEnumerable<T>`, with `TQuery<T>` and `Linq.From<T>`
- **Daf.Specification**: New module — the Specification pattern: `TSpecification<T>` gathers filter, order and paging into a reusable rule that can be asserted without a database, with `IRepository<T>` as the port
- **SQLCute.InMemory**: New module — a second back-end: `TInMemory` runs an `IQuery` over loaded collections —JOINs, GROUP BY, subqueries, set operations— with ANSI NULL semantics
- **SQLCute.Specification**: New module — lowers a whole `Specification` onto an `IQuery`, so the same rule goes to the database or to a loaded collection
- **Daf.Web**: New package — a lightweight HTTP server over Indy and WebBroker, with controllers routed by `[WebRoute]` and built by the container. The package **links neither DataSnap nor DBX**
- **Samples**: `QuickStart`, `ServiceWiring` and `PackageInit` (Cli); `HostQuickStart`, `StartupReport` and `HostLifetime` (Hosting)
- **Samples**: `LinqPipeline` (Linq), `SpecAuthoring` (Specification), `CustomProvider` (Logging), `NNLogTargets` (NNLog), `MiniApi` (web) and `FmxQuickStart` (FMX)
- **Docs**: "Startup composition", the public AST of `SQLCute.Abstractions`, and the criterion behind splitting the logging packages (EN/ES)
- **Commons**: `TVersionInfo.IsEmpty` and `IsSemVer` — «no version was set», «the tag was not understood» and «the version is this one» stop sharing a single representation that looked like a valid version
- **Commons**: `VersionTag` takes a template of `${...}` markers, with `VersionTemplate` and `VersionTemplateWithBuild` as its two defaults. The marker list, in the GUIDE
- **Commons**: `TMacrosList.MissingMacroPolicy` and `OnMissingMacro` decide what a reference to a macro that is not registered becomes: kept as written, a fallback value, or `EMacroNotFound`. With nothing configured it still resolves to empty
- **Commons**: `TShutdownHook.RequestShutdown` — asking for the shutdown the way Ctrl+C does. A request made while no host is listening is delivered to the next one that starts
- **SystemProcess**: `TProcessResult.TStatus.Killed` — an explicit `Kill` and an expired deadline stop sharing the `Timeout` value, so a result says which of the two cut the run short
- **SystemProcess**: `StandardOutput` and `StandardError` give each stream of the child its destination, a `TProcessOutput`: captured, inherited from the parent, discarded or written to a file
- **SystemProcess**: `StandardOutputEncoding` and `StandardErrorEncoding` — the encoding of what is captured
- **SystemProcess**: `Id` — the identifier the system gave the child, from the moment it is launched
- **Linq**: `Linq.FromOwned` — query a temporary list that nobody else keeps: Linq frees it with the query, and each object a terminal returns passes to the caller
- **Linq**: `MoveToList` and `MoveToArray` — terminals that take the selected elements out of the source list as they deliver them, so each object keeps a single owner

### Changed

- ⚠️ **DependencyInjection**: Releasing a scope **destroys the Scoped services it created**. The container used to retain them until it closed, so one scope per request piled up one instance per request. **Breaks** whoever kept a reference to a Scoped service without holding it
- ⚠️ **DependencyInjection**: A repeated registration **now gives two instances**; asking for all of them returned the first one twice. Resolving a single one, and the shutdown order, do not change
- ⚠️ **DependencyInjection**: `CanResolve` on a closed container raises `EContainerShutDownError`, like resolving; it used to answer as if the container were open. **Breaks** whoever asked it after `ShutDown`
- ⚠️ **DependencyInjection**: A class whose constructors the container cannot satisfy fails with `EServiceProviderError` naming the parameter, instead of being built through an ancestor's constructor with the dependency at `nil`. **Breaks** a registration that only worked thanks to that fallback
- ⚠️ **DependencyInjection**: `[ActivatorUtilitiesConstructor]` only counts in `TActivatorUtilities.CreateInstance`, where the marked constructor is the only candidate; the container ignores it, as in .NET. **Breaks** a registration that relied on the container choosing the marked constructor
- ⚠️ **DependencyInjection**: Two constructors that compete make the choice fail with `EServiceProviderError` naming both, as in .NET; the first of the longest used to win. **Breaks** a class with two constructors the container can satisfy that do not contain one another
- ⚠️ **DependencyInjection**: A Singleton that resolves a Scoped gets, without `ValidateScopes`, the root provider's instance, as in .NET; with it, the resolution fails with `EServiceProviderError` at any moment, also through a provider the Singleton kept. It used to raise an `Exception` while the Singleton was being built. **Breaks** whoever caught that `Exception`
- ⚠️ **Logging**: A line no sink would accept **stops being composed**: the level is decided before parsing the template. `ILogger` gains `IsEnabled`, which **breaks** whoever implements the contract without inheriting from `TLogger`
- ⚠️ **Logging**: An entry's `Message` **no longer carries the scope**, which travels only in `TLogEntry.Scope`. **Breaks, with no compiler error,** a provider of its own that writes `Entry.Message`: it stops writing the scopes until it also writes `Entry.Scope`
- ⚠️ **SQLCute**: A query **declares** which statement it is —`IQuery.Kind`, `Select` by default— instead of being inferred from its clauses; declaring two keeps the last one, where the first used to win. The new member **breaks** whoever implements `IQuery` on their own
- ⚠️ **SQLCute**: The protected methods of `TAnsiSqlCompiler` that emit SQL receive the compilation, a `TSqlCompilation`, as their first parameter, and write every placeholder as `?`; `FormatPlaceholder(Index)` replaces `ParamPlaceholder`. **Breaks** a compiler derived from `TAnsiSqlCompiler` that overrides one of them
- ⚠️ **SQLCute**: `Having`, `WhereColumns` and `OrWhereColumns` reject `in` and `not in` with `EArgumentException`, as SqlKata does; they used to emit `=`. In the AST, the `Op` of `TJoinClause` and `TDateWhereClause` is a `TWhereOp`. **Breaks** whoever passed `in` there, and whoever reads that `Op` as a string
- ⚠️ **SQLCute**: `Where`, `OrWhere`, `WhereNot`, `OrWhereNot` and `Having` with the value `Null` emit `IS NULL` for `=` and `IS NOT NULL` for any other operator, with no binding, as SqlKata does; they used to bind `Null` to `col = ?`, which matches no row. **Breaks** whoever counted that binding
- ⚠️ **SQLCute**: `WhereTrue`, `WhereFalse`, and `Where`, `OrWhere`, `WhereNot` and `OrWhereNot` with a logical value compare with the dialect's literal —`true`, `1`, `cast(1 as bit)`— with no binding, as SqlKata does; they used to bind the value. With an operator other than `=`, the comparison is negated. **Breaks** whoever counted or typed that binding, and Oracle before 23
- ⚠️ **SQLCute**: The column of `Having` is quoted like any other column, as SqlKata does: it names a grouped column or an output alias, and an aggregate goes through `HavingRaw`. **Breaks** `Having('count(*)', …)` in the dialects that quote, where it emits `"count(*)"`
- ⚠️ **MiniSpec**: `[Inject]` is renamed to **`[MiniInject]`** and travels in the facade, so `uses Daf.MiniSpec` is enough to mark
- ⚠️ **MiniSpec**: `TInjectorService` no longer takes loose services: it is an instance of the run with the three scopes, `Register(Scope, Instance)`. A collaborator that needs a scenario is built in the World
- ⚠️ **MiniSpec**: When more than one context satisfies a `[MiniInject]`, the nearest to the step wins —scenario, feature, suite—, and an exact match beats any descendant
- **DependencyInjection**: A service type no longer needs to be visible to reflection in order to resolve: it used to register without complaint and blow up on resolution, far from its cause
- **Commons**: The module now holds **only what extends the standard library**
  - `Daf.SystemProcess` moves out to the new package of the same name, with no change of unit or types
  - `Daf.CmdLn.Parser` moves out on the same criterion, and does not continue: the command line is `Daf.CommandLine`, with a different API
  - `TExpresion<T>` is renamed to **`TDeferred<T>`** and moves to `Daf.Types`; `TMacroExpression` is still the alias
  - `TFilterSpec<T>`, the closure-based filter, moves to its own unit `Daf.FilterSpec`
  - `TSmartArray<T>.Select` takes `TFunc<T, TTarget>` instead of `TDeferred<TMapper<T, TTarget>>`
  - `Daf.Enumerable.ToArray` goes from O(n²) to O(n)
- **SQLCute**: `Daf.SQLCute.Compilers` disappears: the six dialect compilers move into `Daf.SQLCute`. The units keep their names; whoever referenced that package in a `requires` or a `DCCReference` must use `Daf.SQLCute`
- **SQLCute**: `CompileWhere` and `CompileHaving` emit explicit precedence parentheses
- **Hosting**: `THost.Start` and `THost.Stop` report through the `ILogger` pipeline —with the bootstrap channel as a fallback—, and DAF's own reports come out under the category of the class reporting instead of under `DAF`
- ⚠️ **Hosting**: The `IHostedService`s stop in the **reverse order of their registration**, the same order the container releases in: a service started later can be using what one started earlier releases on stop. They used to stop in registration order
- ⚠️ **Hosting**: The host loads `appsettings.json`, `appsettings.{Environment}.json`, `{ApplicationName}.settings.json` and `{ApplicationName}.settings.{Environment}.json` from the content root, all optional, and every environment variable, before the command line, as `Host.CreateApplicationBuilder` does. **Breaks** whoever counted on configuration holding only what they added: an environment variable such as `Db__Server` now overrides `Db:Server`
- ⚠️ **Hosting**: The host settings take .NET's names: `environment`, `applicationName` and `contentRoot`, as an argument —`--environment Staging`— or as a `DAF_` variable with the prefix stripped —`DAF_ENVIRONMENT`—. `THostDefaults` replaces `TDafEnvVars`, and with no `contentRoot` the content root is the current directory, except in the system directory, where a Windows service starts. **Breaks**, silently, whoever sets `DAF_APP_ENV`, `DAF_APP_NAME` or `DAF_CONTENT_ROOT`, which are no longer read, and whoever starts the application from another directory counting on the `appsettings.json` next to the executable
- ⚠️ **Web**: `TDafHttpServer.Start` on a port already in use **raises an exception that names the port**, after the `OnError` event. It used to end with `Abort`: an `except on EAbort` around `Start` no longer catches it
- **Logging**: Composing the pipeline without a single provider **fails at host build time**, with `ELoggingCompositionError`, instead of handing out an `ILogger` that accepts everything and writes nothing. Deliberate silence is declared with `AddNullProvider`
- **Samples**: Every sample is filed under the module that declares what it uses: `ConsoleSample` to `src/Cli/samples/`, `TicTacToeApp` to `src/VCL/samples/`, and `HostedService` to `src/samples/`

- **MediatR**: The base class for a notification handler is `TNotificationHandler<T>`. The previous spelling, `TNotificacionHandler<T>`, still compiles

- ⚠️ **NNLog**: A target implements `DoWrite` instead of overriding `Write`. `TTarget.Write` takes a critical section held by that target instance and then calls it, so writes from several threads through one target no longer overlap — every logger in a process shares the same target instances. **Breaks** whoever wrote a custom target
- ⚠️ **NNLog**: A target that cannot write no longer raises towards whoever logged; it reports on the bootstrap channel, naming the target, once per run of failures, and reports how many lines it lost when it writes again; a file target that cannot archive its file keeps writing in it, tries again on the next line and reports it once per run. **Observable change**: a logging failure stops aborting the code that was only trying to log
- ⚠️ **NNLog**: `${message}` no longer carries the scope, and `${scope}` is the only token that writes it. **Breaks, with no compiler error,** a layout with `${message}` and no `${scope}`, the default one included: it stops showing the scopes. One with both stops writing them twice
- **NNLog**: A layout token with no renderer behind it is left unsubstituted instead of becoming the empty string, so a misspelled `${mesage}` shows up in the line
- ⚠️ **Commons**: `VersionTag`'s `WithCompiledMeta` flag is now a template parameter, and `TVersionInfo.Debug` is read-only and declared by the binary — a fourth argument to `[Version(...)]` or `TVersionInfo.Create`, `False` when omitted; it used to report how DAF itself was compiled. **Breaks** whoever passed the flag or assigned `Debug`
- ⚠️ **SystemProcess**: A stream with no declared destination **inherits the parent's**, so a child launched without handlers writes where the calling process writes. `TProcessOutput.Discard` on both streams keeps it quiet
- ⚠️ **SystemProcess**: A second destination for the same stream —`OnStdOut` twice, or `OnStdOut` and `StandardOutput`— **raises `EInvalidOpException`**; it used to replace the first
- ⚠️ **SystemProcess**: `HideWindow(False)`, the default, **no longer opens a console of its own**: the child runs in the parent's console
- ⚠️ **SystemProcess**: `ExecuteAsync` **launches the child before it returns**, on the calling thread, and only the wait goes to the task. The `OnFailed` of a launch that fails runs on the calling thread, no longer in the task

### Deprecated

- **MediatR**: `TNotificacionHandler<T>`, in favour of `TNotificationHandler<T>`. It is a subclass of it, so what already inherits from it keeps working, and it goes in the next major
- **SQLCute**: The free `RegisterIQueryFactory`, in favour of `TQuery.RegisterFactory`. It forwards, so the two are indistinguishable, and it goes in the next major

### Removed

- ⚠️ **Hosting**: `IHostBuilder`, and with it the deferred style: the `ConfigureServices`, `ConfigureAppConfiguration` and `ConfigureHostConfiguration` callbacks, plus `TConfigureServicesAction`. One style is left, the immediate one; the entry point is `THost.CreateBuilder` or the family's variant
- ⚠️ **Application**: The `Daf.Application` package, with `TDafApplication`, `TDafApplicationBuilder` and `IDafApplication`. The replacement is the variant of the application family — `TCliApplication`, `TWebApplication`, `TVclApplication`, `TFmxApplication` — and `MIGRATION-2.0.md` says where each member went. `TVersionInfo` and `[Version]` move to `Daf.Commons`
- ⚠️ **DependencyInjection**: `AddModules(Package)`, the RTTI discovery of `IAppModule`s. The replacement is `AddModule<T>`, one call per module and in whatever order the application wants; `IAppModule` and `TAppModule` do not change
- **Logging**: The global `DAFInternalLogger`, replaced by `BootstrapLog.LoggerFor(<category>)`
- ⚠️ **Configuration**: `csoReloadOnChange`, a value of `TConfigurationSourceOption` that no provider ever read. `csoOptional` is untouched
- **Hosting**: Leftovers of the fluent style and of the builder's scaffolding
  - `THostBuilder.AddHostedService<T>` — the way is `Builder.Services.AddHostedService<T>`; the `Hosting.AddHostedService(Services, TClass)` facade **stays**
  - `UseProperty` — `Properties`, on `IHostApplicationBuilder`, does the same
  - `TSingleton<T>`, the base of `THostEnvironment`, `THostBuilderContext` and `THostBuilder`, which now derive from `TInterfacedObject`
- **Commons**: Withdrawn
  - `TGenericRTTI` — its `MakeGenericType` promised to build a generic at run time, which is exactly what Delphi does not do. What was worth keeping comes back as `_T.GenericArgs`
  - `&If.Exp<T>`, the ternary written as a function: it evaluated **both** branches, so it did not short-circuit. Delphi 13's ternary operator replaces it
  - `TBooleanExpresion`
  - The `Daf.Expression` unit, dissolved into `Daf.FilterSpec` and `Daf.Types`
- ⚠️ **SystemProcess**: `KillAfterTimeout`, from `ISystemProcess` and from the builder. The deadline always kills, which is the only thing the flag ever did: `Execute` owns the run from the pipes to the teardown, so it is what enforces it. **Breaks** whoever called `.KillAfterTimeout(...)`

### Fixed

- **DependencyInjection**: Container fixes
  - Closing a scope released **one reference too many for every service it had created**, so the service was destroyed while its holder was still using it
  - Resolving from several threads gives **a single instance per registration** and its constructor runs once; it used to run once per thread and most of them failed with `EListError`. Composing the collection is still single-threaded
  - A cycle the container cannot break **stops killing the process**: resolution fails naming the path, instead of exhausting the stack with no exception and no message
  - Resolving after `ShutDown` raises `EContainerShutDownError` naming the service, instead of `EAccessViolation`. Closing twice stops breaking
  - `Lazy.From<T>` builds its service once even when several threads reach it at the same time —eight threads used to build eight—, and a factory that returns nothing fails naming the contract instead of answering the zero of the return type
  - `Lazy.From<T>` releases what it built together with the lazy: every call through it kept one more reference, so the service was never destroyed
  - A factory that returns an object that does not implement its service fails with `EIntfCastError` naming both, and one that returns `nil` hands `nil`, as in .NET: the error message itself failed, and `nil` was an `EAccessViolation`
- **MiniSpec**: Fixes
  - The injection marker works **on a field**: the sweep only walked properties, so the field stayed `nil` with no warning
  - `UseSuiteContext<T>` builds the suite context: `SpecContext.SuiteContext` was `nil` in every step
  - A feature that fails before its scenarios shows up in the report and turns the suite red; it used to vanish and the process exited with 0
  - A generic in a step's description —`AsType<T>`, `TList<T>`— was read as a Scenario Outline placeholder and failed with `T not found in World`
  - The failure of an exception escaping a When step named an API that does not exist; it now names `ExpectException(Raised)`
  - The JUnit report gives the failure message of every failing scenario —it was always empty— and reports a scenario that raised as failed instead of passed
  - A failing row of a Scenario Outline says which step failed and why, in the console, JSON, gherkin-results and live reports; it only showed its cross
  - A failing Background step fails the scenario it prepares, which no longer runs; the scenario used to run and could pass
  - A Pending or Undefined step of a Background leaves the scenario Pending or Undefined, with its own steps unrun: the scenario ran, and could count as passed
  - Each Background runs once per scenario: the feature's ran twice, and a rule's replaced the feature's in every scenario of the feature
  - A step declared `.Pending` in a Scenario Outline is pending in every row: the rows ran it
  - A Scenario Outline without `.Examples` runs once as a scenario: it was left out of the run and of the report
  - A hook of the suite that raises counts as a failure and shows in the report, and a `Before` that raises stops the features from running: the error was lost and the run exited with 0
  - JUnit gives an Undefined scenario as a failure and a Pending one as skipped, lists neither a filtered scenario nor a hook that passed, and gives a hook that raised as a failed case: Undefined and Pending came out as passed, and a failing hook was missing
  - The JSON report gives a Pending or Undefined scenario its own status, a `status` to each step, and `undefinedCount` among its totals: Pending and Undefined came out as `fail`, and a skipped step with `success`
- **Hosting**: `IHostApplicationLifetime.StopApplication` really stops the application: it cancelled `ApplicationStopping` instead of requesting the stop, so `WaitForShutdown` stayed blocked and the `IHostedService`s were never stopped
- **Hosting**: A process can build more than one host: "Build already called" and the "already stopped" flag were class state, so a harness that sets hosts up and tears them down could only set the first one up
- **Hosting**: A hosted service whose `Stop` raised left the services after it unstopped and the host unreleased, container included. The shutdown now runs to the end, and `Host.Stop` raises what failed once the host is released: the exception itself, or an `EAggregateException` holding every failure when several steps failed
- **Hosting**: Freeing a host that was never stopped freed it twice, with `EInvalidPointer`. It now stops it and frees it once
- **NNLog**: A rule's `enabled` and `name` were silently ignored: a missing `enabled` now means **enabled** and `name` really filters. Only the literal `true`, in any case, keeps a rule enabled; any other value disables it. **Observable change**: a config with any rule whose `name` is not `*` stops receiving everything
- **NNLog**: `TFileTarget` could reach a state where every later write failed for the rest of the process: a failed open left the path set with no stream, and the guard then refused to reopen. It retries instead
- **NNLog**: The rotated file's name doubled the dot before the extension (`app_<mark>..log`) and carried the moment of rotation instead of the date of the content it archives
- **NNLog**: Compressing the archive or cleaning the history no longer costs the line that triggered the rotation, nor leaves the target unusable when it fails
- **NNLog**: With `${pid}` in its `FileName`, `TFileTarget` left the file of every finished run in the folder for ever. Those files now move to `history` when their rotation period expires
- **NNLog**: Cleaning the history counted and deleted every file in `history`, so targets sharing a folder removed each other's archives. `MaxArchivedFiles` now counts only the target's own archives, from all its runs
- **NNLog**: A rule built in code arrived disabled, with its maximum level at the lowest of all, and raised while matching a category. `TLogRule.Create` now sets the same defaults the loader writes
- **MediatR**: Composing twice in the same process gives the same result; the accounting of already-scanned packages was unit-global state
- **SQLCute**: `WhereIn` with an **empty** set of values emitted `col IN ()`, invalid on almost any engine; it now matches no row and `WhereNotIn` matches them all. It also affects `OrWhereIn` / `OrWhereNotIn`
- **SQLCute**: A compiler instance can be reused and shared between threads: through `IQueryCompiler`, the Postgres, SQL Server and Oracle compilers started numbering where the previous compilation had ended, and two compilations at once mixed their bindings. The `?` of `FromRaw`, `WithRaw` and the other raw forms are numbered with the rest; `\?` writes a literal one
- **SQLCute**: A quoting dialect quotes each part of a qualified name on its own —`"public"."users"`, `[users].[id]`— instead of the whole name with its dot, leaves a trailing `*` bare, and doubles its closing character inside a name. Oracle writes its aliases without `AS`, which it rejects before a table alias
- **SQLCute**: A case-insensitive LIKE lower-cased only the ASCII letters of the pattern, so `WhereStarts('name', 'ÑAN')` bound `'Ñan%'` and matched no `Ñandú`; the pattern now folds across Unicode
- **Commons**: `TVersionInfo.Tag` was **always empty**, and `GetFrom` on a class with no attribute returned whatever was in the target's memory
- **Configuration**: Binding no longer writes a member whose key no source mentions: a default set in the constructor survives the bind, and a second bind does not erase what the first one brought. A key present with an empty value is still written
- **Configuration**: A chained key whose value **is** the empty string no longer disappears on its way through the chain: `TChainedConfigurationProvider` asks for presence instead of inferring it from the value
- **Commons**: `TMacrosList.GetValueFromIndex` returned an empty string on every macro read
- **Docs**: The startup the documentation taught **did not compile**, and Hosting's guide still promised automatic discovery of `IAppModule`s. It now teaches `AddModule<T>`
- **Docs**: Hosting's examples waited for the shutdown signal and never called `Host.Stop`: copied as they were, the hosted services never stopped and the container was never released. They now stop the host in a `finally`
- **Docs**: The Scenario Outline example in the English MiniSpec guide compared the result with itself, so copied as it was it could never fail
- **Commons**: A version tag that is not SemVer was reported as `0.0.0`; the declared text now survives
- **Commons**: A tag carrying its own build metadata produced two `+` sections, which SemVer does not admit; what the tag declares and what the compiler saw now share the one section
- **SystemProcess**: An explicit `Kill` reported `Timeout`, and killed the child twice — once when asked and again when the wait loop noticed. It now reports `Killed` and kills once
- **SystemProcess**: A child that wrote more than 4 KB to a stream with no handler blocked until its deadline and reported `Timeout`; with no deadline, `Execute` never returned
- **SystemProcess**: Captured text turned non-ASCII characters into others —`ñ` from `cmd` arrived as `¤`— and broke a character split between two reads; it is decoded with the code page of the console the child runs in
- **SystemProcess**: `ExecuteAsyncAndFree` never launched the child, and called neither `OnCompleted` nor `OnFailed`. It now runs the child to the end before freeing it
- **SystemProcess**: `ExecuteAndFree` and `ExecuteAsyncAndFree` leaked their builder on every call
- **Web**: A literal route segment is tried before a parameter in the same position; the action declared first used to win, so `/items/search` could land on `/items/{Id}`
- **Web**: A `[FromServices]` parameter receives its service. With the service registered the request failed with `EInvalidCast`, and without it the action received `nil` with no warning; that case now fails the request naming the service
- **Web**: Destroying a `TDafHttpServer` frees the web modules WebBroker kept for reuse; a later server in the same process was handed the ones built for the previous one
- **Web**: `Ok(Boolean)` no longer leaks the JSON value it serializes, one per response
- **NNLog**: On NTFS, the file target no longer stops rotating after its first rotation, and an archive of the same period in `history`, plain or zipped, is appended to instead of overwritten
- **NNLog**: Changing on disk the creation date of the file a file target has open no longer makes it rotate

### Security

- **SQLCute**: `Where(col, 'in', V)` and `'not in'`, also through `OrWhere`, `WhereNot` and `OrWhereNot`, bind their values like `WhereIn` instead of writing `V` into the SQL. A string with a list (`'1,2,3'`) is now a single value
- **SQLCute**: The operator of `WhereTime`, `OrWhereTime` and of a join by columns is validated against the list `Where` accepts, and what is emitted is the recognised operator; it used to be written into the SQL as it came
- **Web**: The router serves static files only once `RootDirectory` is declared, and only from inside it. With no root it served any file under the working directory, `appsettings.json` included; with one, `..` and absolute paths reached outside it

## [1.6.0] - 2026-07-20

### Added

- **MediatR**: Pipeline behaviors — `TPipelineBehavior<TRequest, TResponse>` with automatic RTTI discovery; support for global behaviors and for request-type-specific ones
- **MediatR**: `TNext<TResponse>` and `TNextValue` — typed delegate to invoke the next link of the behavior chain
- **MediatR**: Redesigned sample showing the four key concepts (handler, logging behavior, validation behavior, full pipeline)
- **SQLCute**: Complete fluent SQL query builder module — `IQuery`, `TAnsiSqlCompiler`, support for WHERE, JOINs, GROUP BY/HAVING, UNION, CTE, subqueries, EXISTS, DML (INSERT/UPDATE/DELETE), paging, set operations
- **SQLCute**: `SQLCute.Abstractions` — core package with `TQueryImpl` and compilers behind a factory pattern
- **SQLCute**: `SQLCuteSample` — full sample demonstrating the whole API
- **SQLCute**: Bilingual documentation (EN/ES) integrated into the Quarto site
- **MiniSpec**: `srrSkipped` and `srrUndefined` in `TSpecRunResult` — explicit result for skipped and undefined steps
- **MiniSpec**: `MarkAsSkipped` / `MarkAsUndefined` on `ISpecItem`
- **MiniSpec**: `UndefinedCount` in the runner and in the summary footer
- **MiniSpec**: `--strict-pending` flag: makes pending specs count as a process failure
- **MiniSpec**: Progressive console reporter — Feature and Scenario are emitted as they are found, steps with Gherkin symbols (`✓ ✗ ~ P ?`) as they complete

### Changed

- **MiniSpec**: Summary footer renamed from `Pass/Fail/Skip` to `Passed/Failed/Pending/Undefined/Skipped`

### Fixed

- **SQLCute**: `SqlServerCompiler` did not emit the `TOP` clause when `LIMIT` was given without `OFFSET`
- **MediatR**: Closure variable capture bug in the behavior chain
- **MediatR**: Discovery of global behaviors with `StrictExtensions=False`
- **MediatR**: Hiding warnings in the `TPipelineBehavior` hierarchy
- **MiniSpec**: A step with no lambda in a `ScenarioOutline` was treated as `Undefined`; it is now a no-op (display-only), because `ExampleInit` injects the values through RTTI
- **MiniSpec**: Outline examples table: header misaligned with respect to the data rows
- **MiniSpec**: Features with no scenarios run still showed up when filtering with `-f`; they are now suppressed in every reporter (console, JSON, Gherkin, JUnit)
- **MiniSpec**: Summary footer stuck to the last Gherkin line; it now carries a blank line before it

## [1.5.0] - 2026-03-20

### Added

- **Docs**: Bilingual (EN/ES) Quarto documentation site for every module

### Fixed

- **MiniSpec**: Remove the dead `PendingStep` from every builder
- **MiniSpec**: Rename `Expect(TProc)` → `ExpectException` to resolve the ambiguity with `TValue`
- **MiniSpec**: Step binding fixes — lazy binding, `NoAction` lskThen, DI assertions in Then, memory leak
- **MiniSpec**: `CmdLnParser` uses `Build(False)` to avoid a double free with the destructor

## [1.4.0] - 2026-02-16

### Added

- **Samples**: TicTacToe - BDD sample app with MVVM + Clean Architecture (VCL dark-theme Form, ViewModel, ViewPort)
- **Samples**: TicTacToe - Feature specs: Movement, NewGame, UX (8 scenarios), with `@e2e`/`@integration` tags
- **Samples**: TicTacToe - Algebraic notation (`a1`–`c3`), visual board DataTables, ScenarioOutline with Examples
- **MiniSpec**: DataTable binding for ScenarioOutline; multiple reporters in INI config

### Fixed

- **MiniSpec**: Deferred binding in ScenarioOutline; Live Dashboard Rule/DataTable rendering

## [1.3.0] - 2026-02-02

### Added

- **MiniSpec**: Test Doubles - `Stub<T>`, `Mock<T>`, `Spy<T>` for interface mocking
- **MiniSpec**: Fluent API for stubs with `.Setup().Returns()`, `.WithArgs()`, `.Raises()`
- **MiniSpec**: Mock expectations: `.Expects().Once`, `.AtLeastOnce`, `.Never`, `.Exactly(N)`
- **MiniSpec**: Argument matchers: `Arg.Any<T>`, `Arg.Is<T>(predicate)`, `Arg.IsNil`
- **MiniSpec**: Spy tracking with `.Track()` and call history inspection
- **MiniSpec**: `.Pending` fluent method - mark specs as not yet implemented
- **MiniSpec**: `.NoAction` fluent method - descriptive steps without code
- **MiniSpec**: `@skip` reserved tag - always excluded from runs (mark broken tests)
- **MiniSpec**: JUnit XML reporter for CI/CD integration
- **MiniSpec**: SpecSuite - root node containing all Features with title and hooks
- **MiniSpec**: `Category(Title)` - fluent API to set Suite title
- **MiniSpec**: `Before(Description, Hook)` / `After(Description, Hook)` - Suite-level hooks
- **MiniSpec**: `ISpecSuite` interface and `TSpecSuite` implementation
- **MiniSpec**: Suite events: `OnBeginSuite` / `OnEndSuite` for listeners
- **MiniSpec**: All reporters display Suite title (Console, JSON, Gherkin comment, Live header)
- **MiniSpec**: DataTables support - inline data tables for steps (Gherkin standard)
- **MiniSpec**: `TDataTable` type and `TDataTableObj` wrapper with Row/Cell/Headers access
- **MiniSpec**: All reporters display DataTables (Console ASCII, JSON array, Gherkin pipes)
- **MiniSpec**: `TFeatureWorld` base class for Worlds needing execution context access
- **MiniSpec**: `ISpecContext` interface with `CurrentStep`, `CurrentScenario`, `CurrentRule`, `CurrentFeature`, `DataTable`
- **MiniSpec**: Extended filter system with `Feat:`, `Scen:`, `Rule:`, `Cat:` prefixes
- **MiniSpec**: `EndRule` method to return to Feature context from Rule
- **MiniSpec**: Live Dashboard auto-reconnect for new test runs
- **MiniSpec**: Live Dashboard export/print support
- **MiniSpec**: Cleaner exception testing API - `Expect(Raised).ToBe<EException>`
- **MiniSpec**: Dedicated test project for framework tests (`src/MiniSpec/test/`)
- **MiniSpec**: `Before`/`After` hooks for feature-level setup/teardown (run once per feature)

### Changed

- **MiniSpec**: Refactored `InUnit` → `Category` concept for flexible feature grouping
- **MiniSpec**: Filter prefixes changed: `F:`→`Feat:`, `S:`→`Scen:`, `R:`→`Rule:`, `U:`→`Cat:`
- **MiniSpec**: `TSourceUnit` marker renamed to `TUnitMarker`
- **MiniSpec**: Extracted reporters to individual units for better modularity
- **MiniSpec**: Uses `ISpecReporter` interface instead of class reference
- **Samples**: Renamed `.Feature.pas` → `.Feat.pas` per naming convention

### Fixed

- **MiniSpec**: AST parent navigation and phantom World elimination
- **MiniSpec**: Listeners `FCliOptions` dangling pointer and config logic
- **MiniSpec**: World create/destroy lifecycle balance verified with tests

### Removed

- **MiniSpec**: HTML reporter (replaced by Live Dashboard)
- **Tools**: MiniSpecWatcher project (replaced by Live reporter)

### Documentation

- **MiniSpec**: Updated README with new exception testing API
- **MiniSpec**: Added LiveDashboard unit to README file list

## [1.2.0] - 2026-01-24

### Added

- **MiniSpec**: And/But step keywords for more expressive scenarios
- **MiniSpec**: Tag filtering with expression parser (`@tag`, `not @tag`, `@a and @b`)
- **MiniSpec**: Rule support for enhanced scenario organization
- **MiniSpec**: ScenarioOutline with grouped table format
- **MiniSpec**: Skip state and enhanced reporting
- **MiniSpec**: Expanded Expect assertions
- **MiniSpec**: TGherkinReporter - generates `.feature` files from specs
- **MiniSpec**: TLiveReporter - real-time dashboard reporter
- **MiniSpec**: Per-reporter help via `-r <reporter>:help`
- **MiniSpec**: Configuration persistence via `.cfg` files
- **MiniSpec**: Logo display in console output

### Changed

- **MiniSpec**: New reporter CLI syntax for v1.2.0
- **MiniSpec**: TLiveReporter refactored to independent reporter
- **MiniSpec**: CLI cleanup - DryRun, Pause, CompletedAt options
- **BDS.bat**: Updated to v4.1.0 with `--project` option

### Fixed

- **MiniSpec**: UTF-8 encoding issues in reporters
- **MiniSpec**: Add Indy packages to requires (eliminates implicit import warnings)
- **MiniSpec**: Use fixed config filename `MiniSpec.ini`

### Documentation

- **MiniSpec**: Complete README rewrite for v1.5.0

## [1.1.0] - 2026-01-12

### Added

- Initial public release of DAF Community Edition
- Core modules: Commons, Configuration, DependencyInjection, Hosting, Logging, MediatR
- MiniSpec BDD testing framework
- Sample projects: Calculator, Config, Console, HostedService, Mastermind, MediatR, Ping

[Unreleased]: https://github.com/Delphi-DAF/dafce/compare/v1.6.0...HEAD
[1.6.0]: https://github.com/Delphi-DAF/dafce/compare/v1.5.0...v1.6.0
[1.5.0]: https://github.com/Delphi-DAF/dafce/compare/v1.4.0...v1.5.0
[1.4.0]: https://github.com/Delphi-DAF/dafce/compare/v1.3.0...v1.4.0
[1.3.0]: https://github.com/Delphi-DAF/dafce/compare/v1.2.0...v1.3.0
[1.2.0]: https://github.com/Delphi-DAF/dafce/compare/v1.1.0...v1.2.0
[1.1.0]: https://github.com/Delphi-DAF/dafce/releases/tag/v1.1.0
