![](assets/DAF-Logo.png)

# DAF – Delphi Application Foundation (Community Edition) 

**Bringing modern development practices to Delphi — welcoming .NET developers to a familiar architecture**

DAF (Delphi Application Framework) is a modular and extensible library for building structured, maintainable, and testable Delphi applications.  
It aims to modernize Delphi development by promoting architectural patterns and coding standards inspired by the excellent .NET framework — such as dependency injection, structured logging, CQRS, in-process messaging, configuration binding, and clean abstractions.

DAF is also designed as a bridge for .NET developers entering the Delphi ecosystem, offering a familiar API style and clear, idiomatic structure.  
It fosters openness, collaboration, and community-driven quality.

To deliver on this familiar and modern architecture, DAF models several foundational components of the .NET ecosystem — including hosting, configuration, and logging — following their design closely.  
It also brings in community-proven patterns by porting influential projects such as [NLog](https://nlog-project.org/) and [MediatR](https://github.com/jbogard/MediatR), adapted natively to Delphi.

The Community Edition already includes:

- **Dependency Injection** — based on `Microsoft.Extensions.DependencyInjection`
- **Logging** — based on `Microsoft.Extensions.Logging`, including:
  - **Structured Logging** — with support for log scopes and message templates
- **Configuration System** — based on `Microsoft.Extensions.Configuration`, with support for:
  - In-memory collections
  - `.json` files
  - `.ini` files
  - Environment variables
  - The command line
  - Binding to custom Delphi classes, and the options pattern (`IOptions<T>`)
- **Host Model** — ports of `IHost` and `IHostApplicationBuilder` for application composition and lifecycle management. Includes:
  - **IHostEnvironment** — for environment-based configuration
  - **IHostApplicationLifetime** — for application lifecycle events
  - **The application family** — `TCliApplication`, `TWebApplication`, `TVclApplication` and `TFmxApplication`, one entry point per kind of application over the same builder contract
- **CQRS and In-Process Messaging** — modeled after [MediatR](https://github.com/jbogard/MediatR), supporting commands, queries, and notifications
- **NNLog** — a Delphi-native port of [NLog](https://nlog-project.org/) ("**NNLog is Not NLog**"), fully integrated with DAF's logging abstractions
- **Command Line** — `Daf.CommandLine`, a port of [System.CommandLine](https://learn.microsoft.com/dotnet/standard/commandline): commands, options and arguments, help, and invocation
- **Web** — controllers with attribute routing and action results, modeled after ASP.NET Core MVC, over Indy and WebBroker
- **Process Launching** — `Daf.SystemProcess`, modeled after `System.Diagnostics.Process`
- **Linq** — LINQ-to-Objects: lazy queries over `IEnumerable<T>`, with sources Linq can own and release
- **SQLCute** — a port of [SqlKata](https://sqlkata.com/): a fluent query builder compiled to ANSI SQL, SQL Server, PostgreSQL, MySQL, SQLite, Oracle and Firebird, including:
  - **SQLCute.InMemory** — runs the same query over loaded collections
  - **Specification** — the Specification pattern (filter, order and paging), lowered onto a SQLCute query by `Daf.SQLCute.Specification`
- **MiniSpec** — BDD testing framework inspired by [Cucumber](https://cucumber.io/) and [RSpec](https://rspec.info/), including:
  - **Gherkin Syntax** — Given/When/Then directly in Delphi code
  - **Scenario Outline** — data-driven testing with Examples tables
  - **Multiple Reporters** — Console, JSON, JUnit (CI/CD), Live Dashboard
  - **Test Doubles** — elegant Stub, Mock, and Spy API

DAF Community Edition is distributed under the [MIT License](./legal/LICENSE.md).

A Pro edition is available: everything in this edition, plus the DAL — a data access layer over
FireDAC — documentation and commercial support.

## Getting the sources

Every release on GitHub carries its sources as a zip, **Source code (zip)** under the release's
*Assets*: <https://github.com/Delphi-DAF/dafce/releases>. Unzip it anywhere, a path with spaces
included, and run everything below from that folder. Cloning the repository also works.

## Building

Requires Delphi 12 (Athens) or later, on Windows: Win32 and Win64. No package declares a Linux
target.

`buildall.bat` builds from the command line, with nothing to set up:

| Command | Builds |
|---|---|
| `buildall.bat` | everything: the framework in Debug and Release, the samples and the specs |
| `buildall.bat --framework` | the framework, in Debug and Release |
| `buildall.bat --samples` | the framework in Debug, then the samples |
| `buildall.bat --specs` | the framework in Debug, then the specs |

The three options combine. `Win32`, `Win64` or `all` choose the platforms, all by default, and
`--clean` deletes `out\` first. `runtests.bat all` runs every suite the specs group declares.

### Choosing the Delphi version

`project.cmd` selects Delphi 13 (`PRJ_IDE=D370`). Delphi 12 is `PRJ_IDE=D290`, and either of these
selects it without editing `project.cmd`:

- a file named after the machine, `%COMPUTERNAME%.project.cmd`, next to `project.cmd`, holding the
  line `set "PRJ_IDE=D290"`;
- `set "PRJ_IDE=D290"` on its own line in the console, before running `buildall.bat`.

With Delphi 12, `buildall.bat --framework --samples` builds the framework and the samples. The
specs of the VCL module need Delphi 13, so `--specs`, and `buildall.bat` with no options, end in
`BUILD FAILED`.

When the version asked for is not installed, `BDS.bat` builds nothing, exits with code 2 and lists
the installed versions with the `PRJ_IDE` of each.

### Using what is built

Add `out\Win32\Release\lib` or `out\Win64\Release\lib` to your project's search path, and the DAF
units your program uses compile against it. `out\<Platform>\Debug\lib` holds the Debug build of the
same units, for debugging.

### The IDE

Every project takes its output paths from `src/paths.optset`, which derives all of them from
`PRJ_OUT_DIR`. `BDS.bat` sets that variable from its own location, so the IDE is started through it:

```cmd
BDS start
```

That opens RAD Studio with the project's environment and loads `src/DAFGroup.groupproj`. An IDE
started any other way leaves `PRJ_OUT_DIR` undefined, and every output path is then built from an
empty value.

`BDS.bat` keeps each folder's IDE profile under `HKCU\Software\Embarcadero\DAF\<folder name>`.
Deleting the folder leaves that key behind, and this removes it:

```cmd
reg delete "HKCU\Software\Embarcadero\DAF\<folder name>" /f
```

The full documentation is at <https://delphi-daf.github.io/dafce>.

## Editions

| Edition | License | Description |
|--------|---------|-------------|
| **DAF Community** | MIT | Free for personal and commercial use |
| **DAF Pro**       | Commercial | Everything in Community, plus the DAL, documentation and professional support |


