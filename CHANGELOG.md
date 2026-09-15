# Changelog

**🌍 Language: English | [Español](CHANGELOG.es.md)**

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html)
and [Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/).

## [Unreleased]

The changes that break compilation are collected, with their example diff, in `MIGRATION-2.0.md`
(`MIGRATION-2.0.es.md` in Spanish).

### Added

- **DependencyInjection**: Keyed services — two implementations of the same contract told apart by a key, with .NET's `AddKeyed*` / `TryAddKeyed*` / `GetKeyed*` set
- **DependencyInjection**: Property injection — `[DafInject]` on the property and the container fills it once the constructor has finished, which makes **a cycle between two services resolvable**
- **DependencyInjection**: `TActivatorUtilities.InjectInto` — wires an instance the container did not build, such as a form the platform creates, without taking ownership of it
- **DependencyInjection**: `AddInjector<T>` — wiring in code, with no attributes and no RTTI, so it reaches a class from another library
- **DependencyInjection**: `IOptions<T>` — a service asks for its settings in the constructor and gets them built and validated, from `AppOptions.Configure<T>` or from a configuration section
- **DependencyInjection**: Open generics — `AddSingleton<IRepo<_>, TRepo<_>>` serves every closure of the contract. What it covers and what it does not, in the README
- **DependencyInjection**: `ValidateOnBuild` and `ValidateScopes`, opt-in on `BuildServiceProvider(TServiceProviderOptions)` — the graph is checked at build time and every failure is reported together. Factory registrations are left out
- **DependencyInjection**: Improvements to composing the collection
  - `TryAddSingleton` / `TryAddScoped` / `TryAddTransient`, `Replace` and `RemoveAll`, with .NET's semantics
  - `Descriptors` — a service's registrations, read-only
  - `Decorate<S>` — wraps the existing registrations keeping their lifetime
  - `AddModule<T>` and `AddModule(Module)` — registrations grouped in a class, applied in the order written
  - `IContainerAccess.OwnedInstanceCount` — how many objects the container still owns, to assert "everything was released"
- **Logging**: `ILogger.IsEnabled(Level)` — asking whether an entry of that level would reach any sink before building its arguments
- **Logging**: `ILogger<T>` — a service asks for it by constructor and gets a logger carrying its own category
- **Logging**: Bootstrap channel — an `ILogger` from the process's first line, with no dependency injection and no configuration, for the phase before `BuildServiceProvider`: report with `BootstrapLog.LoggerFor` and add sinks with `BootstrapLog.AddSink`. The sink catalogue, in the README
- **Logging**: `Builder.AddNullProvider` — discarding entries on purpose, stated while composing
- **Configuration**: A fourth provider, the command line — `CommandLineConfig.AddArgs`, in `Daf.Configuration.CommandLine`, reads the process arguments as keys. The accepted forms, in the README
- **Configuration**: `ConfigOptions.Bind<T>` — a section arrives typed and owned to whoever needs it, and the service stops naming its section: startup decides it
- ⚠️ **Configuration**: `IConfiguration.Exists` and `Find` — presence is **asked**, not inferred from an empty value; `Find` answers presence and value in one lookup, as a `TNullable<string>`. The indexer stays a `string`. **Breaks** whoever implements `IConfiguration` without inheriting from `TConfigurationRoot`
- **Configuration**: `IConfigurationManager` — builder and reader on the same object, so configuration can be read while it is still being composed. Each `Add` builds and loads only the new provider
- **Configuration**: `ConfigurationKeyName` — a member names the configuration key it is read from, so a key a file writes as `connection-string` reaches a property Delphi cannot name that way. It replaces the member name, renames the whole section on a member of object type, and applies to properties and fields alike
- **Hosting**: `IStartupTask` and `AddStartupTask<T>` — initialization that runs once at startup, with the provider already built and before the `IHostedService`s. If one fails, startup aborts
- **Hosting**: `THost.CreateBuilder` and `IGenericHostBuilder` — the generic host's entry point, in the family's shape: `var Builder := THost.CreateBuilder;`. `THostBuilder.Create` still exists, and chaining off it still leaks the instance
- **Application**: The application family — `TCliApplication` (`Daf.Cli`), `TVclApplication` (`Daf.VCL`), `TFmxApplication` (`Daf.FMX`) and `TWebApplication` (`Daf.Web`): the same startup up to where the platform takes over the loop, and around `Run` the exit code and the version metadata. FMX adds no targets: they are still Win32 and Win64
- **MiniSpec**: `TSpecRun` — a run with its own suite, context, injector and runner, launchable inside another without disturbing its counts or its report
- **MiniSpec**: `--allow-undefined` — an undefined step stops setting the exit code; `MiniSpec.AllowUndefined(True)` declares it in the suite. By default it breaks
- **MiniSpec**: `SpecContext.ScenarioContext` returns the scenario's World, and a `[MiniInject]` of its type receives it
- **NNLog**: `TNNLogLoader.BuildProvider` accepts a `TNNLogConfiguration` — settings written in code, not only read from a section
- **NNLog**: `${scope}` layout token — the scopes that `BeginScope` opens reach the written line; add it to a target's `layout`
- **SQLCute**: Improvements to the query builder
  - `WhereNot(Callback)` and `OrWhereNot(Callback)` — WHERE groups negated as a unit, `NOT (…)`
  - Per-column NULL ordering — `OrderBy` / `OrderByDesc` with `TNullsOrder`, and `NullsFirst` / `NullsLast`
- **Commons**: New utilities
  - `TNullable<T>` in `Daf.Nullable`
  - `TDeferredEnumerable<T>` and `TGeneratorEnumerator<T>` in `Daf.Enumerable`
  - `Daf.Comparers` — NULL-aware comparers for `TValue` and `TNullable<T>`
- **Daf.CommandLine**: New module — a port of .NET's `System.CommandLine`, with `--version` and a `--help` that `TCommandLineHelp` renders from the grammar
- **Daf.Linq**: New module — the LINQ-to-Objects equivalent: lazy sequences over `System.IEnumerable<T>`, with `TQuery<T>` and `Linq.From<T>`
- **Daf.Specification**: New module — the Specification pattern: `TSpecification<T>` gathers filter, order and paging into a reusable rule that can be asserted without a database, with `IRepository<T>` as the port
- **SQLCute.InMemory**: New module — a second back-end: `TInMemory` runs an `IQuery` over loaded collections —JOINs, GROUP BY, subqueries, set operations— with ANSI NULL semantics
- **SQLCute.Specification**: New module — lowers a whole `Specification` onto an `IQuery`, so the same rule goes to the database or to a loaded collection
- **Daf.Web**: New package — a lightweight HTTP server over Indy and WebBroker, with controllers routed by `[WebRoute]` and built by the container. The package **links neither DataSnap nor DBX**
- **Samples**: `QuickStart`, `ServiceWiring` and `PackageInit` (Cli); `HostQuickStart` and `StartupReport` (Hosting)
- **Docs**: "Startup composition", the public AST of `SQLCute.Abstractions`, and the criterion behind splitting the logging packages (EN/ES)

### Changed

- ⚠️ **DependencyInjection**: `Replace` and `RemoveAll` now reach only the registrations **without a key**; for the others there is `RemoveAllKeyed`. **Breaks**, and stays green, whoever uses them to tear a registration down in a spec harness: it tears down less than before
- ⚠️ **DependencyInjection**: Releasing a scope **destroys the Scoped services it created**. The container used to retain them until it closed, so one scope per request piled up one instance per request. **Breaks** whoever kept a reference to a Scoped service without holding it
- ⚠️ **DependencyInjection**: Resolution **publishes the instance before filling its properties**, whether or not it takes part in property injection. **Breaks** the constructor that resolves its own service: it now receives the half-built instance instead of recursing without end
- ⚠️ **DependencyInjection**: Shutting the container down **clears the injected properties** of what it owns before releasing anything, so that two services that injected each other are destroyed
- ⚠️ **DependencyInjection**: A repeated registration **now gives two instances**; asking for all of them returned the first one twice. Resolving a single one, and the shutdown order, do not change
- ⚠️ **Logging**: A line no sink would accept **stops being composed**: the level is decided before parsing the template. `ILogger` gains `IsEnabled`, which **breaks** whoever implements the contract without inheriting from `TLogger`
- ⚠️ **SQLCute**: A query **declares** which statement it is —`IQuery.Kind`, `Select` by default— instead of being inferred from its clauses; declaring two keeps the last one, where the first used to win. The new member **breaks** whoever implements `IQuery` on their own
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
- **SQLCute**: From **5 packages to 4**: the six dialect compilers move into `Daf.SQLCute` and `Daf.SQLCute.Compilers` disappears. The units keep their names; whoever referenced that package in a `requires` or a `DCCReference` must use `Daf.SQLCute`
- **SQLCute**: `CompileWhere` and `CompileHaving` emit explicit precedence parentheses
- **Hosting**: `THost.Start` and `THost.Stop` report through the `ILogger` pipeline —the bootstrap channel stays as a fallback—, and DAF's own reports come out under the category of the class reporting instead of under `DAF`
- **Logging**: Composing the pipeline without a single provider **fails at host build time**, with `ELoggingCompositionError`, instead of handing out an `ILogger` that accepts everything and writes nothing. Deliberate silence is declared with `AddNullProvider`
- **Logging**: A bootstrap-channel sink that cannot write **declares itself unavailable** instead of accepting the report and losing it, and one that raises is left out without taking startup down. `BootstrapLog.HasAvailableSink` tells them apart
- **Samples**: Every sample is filed under the module that declares what it uses: `ConsoleSample` to `src/Cli/samples/`, `TicTacToeApp` to `src/VCL/samples/`, and `HostedService` to `src/samples/`

- **MediatR**: The base class for a notification handler is `TNotificationHandler<T>`. The previous spelling, `TNotificacionHandler<T>`, still compiles

- ⚠️ **NNLog**: A target implements `DoWrite` instead of overriding `Write`. `TTarget.Write` takes a critical section held by that target instance and then calls it, so writes from several threads through one target no longer overlap — every logger in a process shares the same target instances. **Breaks** whoever wrote a custom target
- ⚠️ **NNLog**: A target that cannot write no longer raises towards whoever logged; it reports on the bootstrap channel, naming the target. **Observable change**: a logging failure stops aborting the code that was only trying to log
- **NNLog**: A layout token with no renderer behind it is left unsubstituted instead of becoming the empty string, so a misspelled `${mesage}` shows up in the line

### Deprecated

- **MediatR**: `TNotificacionHandler<T>`, in favour of `TNotificationHandler<T>`. It is a subclass of it, so what already inherits from it keeps working, and it goes in the next major
- **SQLCute**: The free `RegisterIQueryFactory`, in favour of `TQuery.RegisterFactory`. It forwards, so the two are indistinguishable, and it goes in the next major

### Removed

- ⚠️ **Hosting**: `IHostBuilder`, and with it the deferred style: the `ConfigureServices`, `ConfigureAppConfiguration` and `ConfigureHostConfiguration` callbacks, plus `TConfigureServicesAction`. One style is left, the immediate one; the entry point is `THost.CreateBuilder` or the family's variant
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

### Fixed

- **DependencyInjection**: Container fixes
  - Closing a scope released **one reference too many for every service it had created**, so the service was destroyed while its holder was still using it
  - Resolving from several threads gives **a single instance per registration** and its constructor runs once; it used to run once per thread and most of them failed with `EListError`. Composing the collection is still single-threaded
  - A cycle the container cannot break **stops killing the process**: resolution fails naming the path, instead of exhausting the stack with no exception and no message
  - Resolving after `ShutDown` raises `EContainerShutDownError` naming the service, instead of `EAccessViolation`. Closing twice stops breaking
- **MiniSpec**: Fixes
  - The injection marker works **on a field**: the sweep only walked properties, so the field stayed `nil` with no warning
  - `UseSuiteContext<T>` builds the suite context: `SpecContext.SuiteContext` was `nil` in every step
  - A feature that fails before its scenarios shows up in the report and turns the suite red; it used to vanish and the process exited with 0
  - A generic in a step's description —`AsType<T>`, `TList<T>`— was read as a Scenario Outline placeholder and failed with `T not found in World`
- **Hosting**: `IHostApplicationLifetime.StopApplication` really stops the application: it cancelled `ApplicationStopping` instead of requesting the stop, so `WaitForShutdown` stayed blocked and the `IHostedService`s were never stopped
- **Hosting**: A process can build more than one host: "Build already called" and the "already stopped" flag were class state, so a harness that sets hosts up and tears them down could only set the first one up
- **NNLog**: A rule's `enabled` and `name` were silently ignored: a missing `enabled` now means **enabled** and `name` really filters. Only the literal `true`, in any case, keeps a rule enabled; any other value disables it. **Observable change**: a config with any rule whose `name` is not `*` stops receiving everything
- **NNLog**: `TFileTarget` could reach a state where every later write failed for the rest of the process: a failed open left the path set with no stream, and the guard then refused to reopen. It retries instead
- **NNLog**: The rotated file's name doubled the dot before the extension (`app_<mark>..log`) and carried the moment of rotation instead of the date of the content it archives
- **NNLog**: Compressing the archive or cleaning the history no longer costs the line that triggered the rotation, nor leaves the target unusable when it fails
- **NNLog**: A rule built in code arrived disabled, with its maximum level at the lowest of all, and raised while matching a category. `TLogRule.Create` now sets the same defaults the loader writes
- **MediatR**: Composing twice in the same process gives the same result; the accounting of already-scanned packages was unit-global state
- **SQLCute**: `WhereIn` with an **empty** set of values emitted `col IN ()`, invalid on almost any engine; it now matches no row and `WhereNotIn` matches them all, alike on both back-ends. It also affects `OrWhereIn` / `OrWhereNotIn` and `IsIn([])`
- **Application**: `TVersionInfo.Tag` was **always empty**, and `GetFrom` on a class with no attribute returned whatever was in the target's memory
- **Configuration**: Binding no longer writes a member whose key no source mentions: a default set in the constructor survives the bind, and a second bind does not erase what the first one brought. A key present with an empty value is still written
- **Configuration**: A chained key whose value **is** the empty string no longer disappears on its way through the chain: `TChainedConfigurationProvider` asks for presence instead of inferring it from the value
- **Commons**: `TMacrosList.GetValueFromIndex` returned an empty string on every macro read
- **Docs**: The startup the documentation taught **did not compile**, the guide described a surface of `IDafApplication` that was not the real one, and Hosting's still promised automatic discovery of `IAppModule`s. It now teaches `AddModule<T>`

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
