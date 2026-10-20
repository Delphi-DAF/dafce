# Daf.Cli

**🌍 Language: English | [Español](README.es.md)**

The **CLI member** of DAF's application family. It ports the shape of .NET's
[`Host.CreateApplicationBuilder`](https://learn.microsoft.com/dotnet/core/extensions/generic-host)
and adds what a console tool actually needs and .NET leaves to `System.CommandLine`: a grammar, and
the **edge around `Run`**.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

---

## The whole thing

<!-- quickstart: src/Cli/samples/GreetTool/GreetTool.dpr -->

```pascal
program GreetTool;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  System.SysUtils,
  Daf.Extensions.DependencyInjection,
  Daf.CommandLine,
  Daf.Configuration.CommandLine,
  Daf.Cli;

// Una herramienta CLI de la familia, con gramatica de verdad. Lo que enseña es el BORDE del Run:
// --version y --help los contesta el parse, una invocacion que no encaja sale con su propio
// codigo, y lo que se escape --de la accion o de la composicion-- lo decide la politica declarada.
// El .dpr solo escribe el except que se la pasa.
//
//   GreetTool --name Jorge      -> 0, saluda
//   GreetTool --name Jorge --shout -> 0, saluda gritando
//   GreetTool --version         -> 0, la version
//   GreetTool --help            -> 0, la ayuda
//   GreetTool                   -> 2, falta un argumento obligatorio
//   GreetTool --name boom       -> 7, la politica traduce la excepcion
begin
  var B := TCliApplication.CreateBuilder(CommandLineConfig.ProcessArgs);

  // La politica es un callback, asi que una suite puede compartir la suya sin heredar nada mas. Va
  // lo primero porque decide tambien lo que se escape de la composicion.
  B.OnException(
    function(const Failure: TAppFailure): Integer
    begin
      Writeln(ErrOutput, 'greet: ', Failure.Error.Message);
      Result := 7;
    end);

  try
    var NameOpt := CommandLine.Option<string>('--name').Description('A quien saludar').Required;
    var ShoutOpt := CommandLine.Option<Boolean>('--shout', ['-s']).Description('En mayusculas');
    B.RootCommand.WithVersion('2.0.0').Add([NameOpt, ShoutOpt]);

    // Solo una instancia: la segunda no llega a construir el contenedor.
    B.SingleInstance('daf-greet-tool');

    B.SetAction(
      function(const Services: IServiceProvider; const Parsed: IParseResult): Integer
      begin
        var Who := NameOpt.ValueIn(Parsed);
        if Who = 'boom' then
          raise Exception.Create('ese nombre no me gusta');

        var Greeting := 'Hola, ' + Who;
        if ShoutOpt.ValueIn(Parsed) then
          Greeting := Greeting.ToUpper;

        Writeln(Greeting);
        Result := TCliExit.Ok;
      end);

    B.ParseArgs(CommandLineConfig.ProcessArgs);
    ExitCode := B.Build.Run;
  except
    // Lo que se escapa antes del Run --de la composicion o del Build-- va a la misma politica.
    on E: Exception do
      ExitCode := B.HandleException(E);
  end;
end.
```

`--version`, `--help`, a mistyped option and whatever escapes the action are already covered: the
**grammar** answers the first three, the declared policy takes the last, and `Run` turns each
outcome into an exit code. None of those four lines is written in the `.dpr`.

---

## The samples

| Sample | What it uses |
|---|---|
| [`QuickStart`](samples/QuickStart/) | the builder, the root's action, the exit code — nothing else |
| [`ServiceWiring`](samples/ServiceWiring/) | the same, plus a service registered and resolved |
| [`GreetTool`](samples/GreetTool/) | the grammar, `--help`, exit codes by policy — the block above |
| [`ConsoleSample`](samples/ConsoleSample/) | the immediate style: the environment composes the settings path before anything is built, and `IOptions<T>` reaches a service by constructor |
| [`PackageInit`](samples/PackageInit/) | a package that registers its own `IStartupTask`, so the composition root names neither the task nor what it does |

---

## The immediate style

The builder is usable **the moment it is created**: `Configuration` and `Environment` answer as soon
as they are read, so a source can be chosen from what is already configured without a deferred
callback. They are loaded at that first read, or at `Build` if the composition never reads them:
creating the builder opens no file.

```pascal
var B := TCliApplication.CreateBuilder(Args);

// The environment already responds -- this is the case the callback style existed for.
if B.Environment.IsDevelopment then
  JsonConfig.AddFile(B.Configuration,
    TPath.Combine(B.Environment.ContentRootPath, 'devtools.json'), [csoOptional]);

if B.Configuration.Find('Features:Beta').GetValueOrDefault('') = 'on' then
  B.Services.AddModule<TBetaModule>;
```

DAF has **one** builder style, and it is this one. The deferred style — registering through
`ConfigureServices` callbacks — is gone: .NET keeps it only for backwards compatibility, and two ways
to express one thing is the defect, not the convenience.

---

## The edge around `Run`

This is what the layer adds over the host, and what every console tool used to rewrite by hand:

| Member | What it decides | When |
|---|---|---|
| `SingleInstance(Id)` | Whether the process starts at all. Without it there is no lock, and several instances can run at once | After the parse, **before** the configuration is loaded and the container built: `--help` and `--version` answer without it |
| `OnException(Policy)` | What happens to what escapes, and with which exit code. Without it, `ExitPolicy.LogAndFail` | Around `Run`, and in the `.dpr`'s `except` |
| `HandleException(E)` | Hands what escapes before `Run` — the composition, `Build` — to the same policy, and returns the exit code | In the `.dpr`'s `except` |
| `SetAction(Action)` | What the root does | Inside `Run` |
| `SetAction(Command, Action)` | What a command does when the invocation reaches it | Inside `Run` |
| `SetStandaloneAction(Command, Action)` | What a command does without a host: no lock, no startup tasks, no hosted services, no configuration, no container | Inside `Run` |
| `RootCommand` | The grammar | Frozen at the first parse |
| `ParseArgs(Args)` | What the grammar parses. Without it, `Build` parses an empty invocation | In the composition, before `Build` |

`RootCommand` is a `Daf.CommandLine` root: a banner above `--help` and `--version` goes on its
`HelpAction` and `VersionAction`, as the [Daf.CommandLine README](../CommandLine/README.md) shows.
They run without a host: what escapes them goes to the `OnException` policy, with
`Failure.Services` as `nil`.

### The parse comes first

`Build` parses — an empty invocation, unless `ParseArgs` already parsed — before it takes the
single-instance lock, loads the configuration or builds the container. `--version`, `--help` and an invocation that does not fit the grammar are answered from
the parse, so neither another running instance nor a host configuration file that cannot be read
keeps them from answering.

What the composition runs before `Build` is its own, and runs before the parse unless it calls
`ParseArgs` first:

- reading `Configuration` or `Environment` loads the configuration at that read;
- a source the application adds itself is opened when it is added, and fails there.

A default file of the host that cannot be read — a malformed `appsettings.json` — is reported through
the startup channel where it is opened, and makes `Build` fail when the invocation reaches an action.

### Each command, its action

`Run` starts the host, runs the action of the command the invocation reached, and stops the host; the
process ends with the code the action answers. The action receives the `IServiceProvider` as a
parameter, next to the parse result:

```pascal
var Greet := CommandLine.Command('greet', 'Say hello');
var Farewell := CommandLine.Command('farewell', 'Say goodbye');
B.RootCommand.Add([Greet, Farewell]);

B.SetAction(Greet,
  function(const Services: IServiceProvider; const Parsed: IParseResult): Integer
  begin
    Writeln(Services.GetRequiredService<IGreeter>.Greet);
    Result := TCliExit.Ok;
  end);
```

- `SetAction(Action)` declares the root's action; `SetAction(Command, Action)`, a command's. The
  action is installed on the command there and then, so a command with subcommands and an action is
  an invocation of its own.
- Declaring in the builder the action of a command that already has one raises
  `ECliCompositionError` at that call. A `Command.SetAction` written afterwards replaces it, as it
  replaces any action.
- An application that declares no action is its startup tasks and its hosted services, and `Run`
  answers `TCliExit.Ok`.
- What escapes the host's startup, an action or the shutdown goes to the `OnException` policy. A
  startup failure arrives with `Failure.Reported` as `True`, because the host has already reported
  it; the action does not run, and the host is stopped anyway.
- A failure while stopping the host arrives with `Failure.Services` as `nil` and `Failure.Reported`
  as `True`: the container is already shut down, and the host has already reported the failure. If
  the startup or the action had already failed, the exit code is the one the policy gave that first
  failure, and the shutdown failure is only reported.
- What escapes before `Run` — the composition, or `Build` — reaches the same policy when the
  `.dpr`'s `except` hands it to `HandleException`, as the sample above does. It arrives with
  `Failure.Services` as `nil`, and with `Failure.Reported` as `True` when it has already gone out
  through the startup channel: a configuration file that cannot be read, a host `Build` that fails,
  or a report of the application's own that carries the exception, through
  `BootstrapLog.LoggerFor`. What the policy already let out of `Run` is raised again without going
  through it a second time. Before `OnException` is declared, the default policy decides.
- An `EAbort` — what `Abort` raises — does not reach the policy: the process ends with
  `TCliExit.Failure` and nothing is reported.
- While `Run` is running, the RTL's `ApplicationHandleException` hook points at the policy, as in a
  VCL application: what a queued procedure raises when `CheckSynchronize` processes it, and what the
  `OnCreate` or `OnDestroy` of a data module raises, reach it with the container. As in VCL, an
  `OnCreate` that raises does not keep the module from being created.
- The exit code is the one the policy gave the first failure, even if the action ends well
  afterwards.

⚠️ Use the `Services` parameter. An action that captures the application to read `App.Services`
forms a reference cycle with it — the application holds the grammar, the grammar the action, the
action the application — and the application is never released.

### A command without a host

`SetStandaloneAction(Command, Action)` declares the action of a command that runs without a host.
When the invocation reaches it there is no single-instance lock, no startup task, no hosted service,
no configuration and no container: the action receives only the parse result, as a `TCommandAction`
of `Daf.CommandLine`.

```pascal
var Init := CommandLine.Command('init', 'Write the starting configuration');
B.RootCommand.Add(Init);
B.SetStandaloneAction(Init,
  function(const Parsed: IParseResult): Integer
  begin
    TFile.WriteAllText('appsettings.json', '{}');
    Result := TCliExit.Ok;
  end);
```

It is for what the arguments alone answer, such as an `init` that writes the configuration an
ordinary run requires. Like `SetAction`, it raises `ECliCompositionError` when the command already
has an action. A `Command.SetAction` written afterwards replaces the action, and the command stays
without a host. What escapes it goes to the `OnException` policy, with `Failure.Services` as `nil`.

### The policies that come as standard

`OnException` takes any callback, and the policy is the application family's: the Web variant takes
the same one. It receives a `TAppFailure` — the exception in `Error`, the
container in `Services`, `nil` when there is none, and `Reported` when it has already been reported,
through the startup channel or by the host while starting or stopping — and returns the exit code.
Three are already written:

| | What it does |
|---|---|
| `ExitPolicy.LogAndFail` | reports through the startup channel and returns `TCliExit.Failure`; what arrives already reported (`Failure.Reported`) is not reported again. **This is the one that applies when `OnException` is not called** |
| `ExitPolicy.Rethrow` | lets the exception out of `Run`, for whoever called it. The host is stopped either way — the shutdown is in the `finally` |
| `ExitPolicy.Silent` | returns `TCliExit.Failure` and says nothing |

`ExitPolicy` is a `class` and its three members are `virtual`, so a suite of applications can derive
from it to group its own.

### Exit codes

| Code | Meaning |
|---|---|
| `TCliExit.Ok` (0) | The action succeeded, there was none, or `--version` / `--help` answered |
| `TCliExit.Failure` (1) | What `ExitPolicy.LogAndFail` returns for an unhandled exception, and how an `Abort` ends |
| `TCliExit.InvocationError` (2) | The invocation does not fit the declared grammar |
| `TCliExit.AlreadyRunning` (3) | Another instance holds the declared single-instance id |

`Run` returns an `Integer` and that is not decoration: a CLI application that cannot tell the system
how it ended is not usable in a script.

### The startup channel

What fails before a logging provider exists goes out through the
[startup channel](../Logging.Abstractions/README.md#bootstrap-channel): a configuration file that
cannot be read, a host `Build` that fails, and what `ExitPolicy.LogAndFail` reports. Out of the box
it writes to stderr and to the system's debug output.

A file destination is added by the application, in the first lines of the `.dpr` and before
`CreateBuilder`:

```pascal
var Data := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), 'data');
TDirectory.CreateDirectory(Data);
BootstrapLog.AddSink(TFileBootstrapSink.Create(TPath.Combine(Data, 'startup.log')));

var B := TCliApplication.CreateBuilder(CommandLineConfig.ProcessArgs);
```

The path cannot come from `Configuration` or `Environment`: reading either one loads the
configuration, and a file that cannot be read is reported at that read, through whatever
destinations exist then. The Hosting sample [`StartupReport`](../Hosting/samples/StartupReport/README.md)
does the same with the generic host.

A grammar error does not go through the channel: the grammar writes the error to stderr, and the
suggestions and the help to stdout, and `Run` returns `TCliExit.InvocationError`. To have it in the
file as well, the composition reports it between `ParseArgs` and `Build`:

```pascal
var Parsed := B.ParseArgs(CommandLineConfig.ProcessArgs);
if not Parsed.Succeeded then
begin
  for var Sink in BootstrapLog.Sinks do
    if (Sink as TObject) is TStdErrBootstrapSink then
      BootstrapLog.RemoveSink(Sink);
  var Log := BootstrapLog.LoggerFor('mytool');
  for var Error in Parsed.Errors do
    Log.LogWarning('{Error}', [TValue.From<string>(Error.Message)]);
end;
ExitCode := B.Build.Run;
```

The loop over `Sinks` removes the channel's stderr destination; with it in place, the error shows
twice on the console. On `mytool --verbsoe`, the file receives:

```
<date and time> #<process>:<thread> [WARN] mytool | Unrecognized command or argument '--verbsoe'.
```

### When there is no container

`Services` is the container, and it answers **between `Build` and `Run`**. It is `nil` in two cases:

- The invocation was settled without building one: `--version`, `--help`, an invocation that does not
  fit the grammar, a standalone command, or another instance already holding the single-instance id.
- After `Run` has returned, always — `Stop` releases the host.

So whatever a program needs from the container once the action has run, it keeps from inside the
action, which receives the `IServiceProvider`.

That `IServiceProvider` is the root one: no scope is opened around the action. A Scoped service the
action needs comes from a scope the action opens — `Services.CreateScope` —; with the validation on,
as it is in `Development`, asking the root provider for it is rejected.

### The parse result in the container

When the invocation reaches an action, the parse result is also in the container, as
`IParseResult`: the same object the action receives. A service asks for it by
constructor:

```pascal
constructor TExportJob.Create(const Parsed: IParseResult);
begin
  inherited Create;
  FParsed := Parsed;
end;
```

And a registration can depend on an argument through a factory, which reads the parse result when it
resolves:

```pascal
var DryRun := CommandLine.Option<Boolean>('--dry-run').Description('Report, do not write');
B.RootCommand.Add(DryRun);

B.Services.AddSingleton<IWriter>(
  function(Provider: IServiceProvider): IWriter
  begin
    if DryRun.ValueIn(Provider.GetRequiredService<IParseResult>) then
      Result := TReportWriter.Create
    else
      Result := TFileWriter.Create;
  end);
```

### The composition decides with the parse

`B.ParseArgs(Args)` parses `Args` with the grammar and freezes it, so the composition can decide
with what was parsed — which source to add, which implementation to register.
`B.ParseArgs(Args, Configuration)` parses them with a `TParserConfiguration`, as
`rootCommand.Parse(args, configuration)` does. It parses once: called again with the same arguments
— element by element, case-sensitive — and the same configuration — its two switches, and whether it
reads response files — it returns the same result, and so does `Build`; called with other arguments
or another configuration, it raises `ECliCompositionError`.

```pascal
var Parsed := B.ParseArgs(Args);
if Loud.ValueIn(Parsed) then
  B.Services.AddSingleton<IGreeter, TLoudGreeter>
else
  B.Services.AddSingleton<IGreeter, TGreeter>;
```

After `ParseArgs`, changing the grammar raises `ECommandLine`; the actions can still be declared.

A composition that skips the rest when the parse answers on its own — help, version, or an
invocation that does not fit — and goes straight to `Build` lets the grammar answer even when what it
would add next cannot be read:

```pascal
var Parsed := B.ParseArgs(Args);
if Parsed.Succeeded and not Parsed.IsHelpRequested and not Parsed.IsVersionRequested then
  JsonConfig.AddFile(B.Configuration, 'web.json');
ExitCode := B.Build.Run;
```

A validation that reads the configuration sees only the sources added before `ParseArgs`.

---

## The arguments, in the grammar and in the configuration

`CreateBuilder(Args)` loads the arguments into `Configuration`, as .NET's
`Host.CreateApplicationBuilder(args)` does. The grammar (`RootCommand`) parses what `ParseArgs(Args)`
receives, as `rootCommand.Parse(args)` does: the builder's arguments do not reach it, and without
`ParseArgs` it parses an empty invocation.

A console tool hands the process arguments to both, and calls `ParseArgs` once the grammar is
declared:

```pascal
var B := TCliApplication.CreateBuilder(CommandLineConfig.ProcessArgs);
// ...the grammar, the services and the actions
B.ParseArgs(CommandLineConfig.ProcessArgs);
ExitCode := B.Build.Run;
```

A process whose arguments are not to be trusted, such as a CGI — which receives the query of a URL
without `=` as arguments — creates the builder without them, and hands them only to the grammar with
response files turned off: `--help` and `--version` answer, the configuration does not see them, and
a token starting with `@` arrives as a value, so no file is read.

```pascal
var B := TCliApplication.CreateBuilder;
var Untrusted: TParserConfiguration;
Untrusted.ResponseFileTokenReplacer := nil;
B.ParseArgs(CommandLineConfig.ProcessArgs, Untrusted);
```

When both receive the same arguments, the configuration reads them without the grammar. It takes `--key=value` and `--key value`, ignores
bare flags and positionals, and reads a flag followed by a positional as a key and its value. On
`mytool --verbose report.txt`:

- the grammar sees the flag `--verbose` and the positional argument `report.txt`;
- the configuration sees the key `verbose` with the value `report.txt`.

What the grammar declares is read from the parse result, not from `Configuration`.

Handed to both, a configuration key the grammar does not declare —`--Db:Server=localhost`— keeps the
application from starting: the grammar rejects what it does not recognise by default, and `Run` returns
`TCliExit.InvocationError`. To accept it, the root says so:

```pascal
B.RootCommand.AllowUnmatchedTokens;
```

---

## Where it diverges from .NET

| .NET | Here | Why |
|---|---|---|
| Extension methods (`AddX`, `UseX`) | Methods of the record, or a `class` facade | Delphi has no extension methods, and only **one** `record helper` is visible per type — a fluent API spread over units breaks silently when a `uses` is reordered |
| `IHostApplicationBuilder.Metrics` | **Not ported** | There is no equivalent piece in DAF to point at |
| `ConfigureContainer<T>` | **Not ported** | A generic method is not expressible in a Delphi interface — not rejected, unrepresentable. The default provider's options, which .NET sets through it, are `ServiceProviderOptions` |
| `Command.SetAction`, whose action receives the `ParseResult` | `B.SetAction(Command, Action)`, whose action also receives the `IServiceProvider` | `System.CommandLine` has no host. Here the host starts before the action runs and its provider arrives as a parameter |
| No app-global | Same | In console there is no `Application` global the user expects: the action receives the `IServiceProvider` it needs |

The last row is the one that separates this variant from the desktop ones: **VCL and FMX do offer a
global**, because their platform already has one.

---

## Package dependencies

`Daf.Cli` requires `Daf.Hosting` and `Daf.CommandLine`. The parser is **not optional** here:
`--version` and `--help` are answered by the grammar, so every CLI application of the family links
the parser — the same price .NET pays.
