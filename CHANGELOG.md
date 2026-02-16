# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html)
and [Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/).

## [Unreleased]

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

[Unreleased]: https://github.com/Delphi-DAF/dafce/compare/v1.4.0...HEAD
[1.4.0]: https://github.com/Delphi-DAF/dafce/compare/v1.3.0...v1.4.0
[1.3.0]: https://github.com/Delphi-DAF/dafce/compare/v1.2.0...v1.3.0
[1.2.0]: https://github.com/Delphi-DAF/dafce/compare/v1.1.0...v1.2.0
[1.1.0]: https://github.com/Delphi-DAF/dafce/releases/tag/v1.1.0