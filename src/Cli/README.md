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

// Una herramienta CLI de la familia, con gramatica de verdad. Lo que ensena es el BORDE del Run:
// --version y --help los contesta el parse, una invocacion que no encaja sale con su propio
// codigo, y lo que se escape aplica la politica declarada. Ninguna de esas cuatro lineas se
// escribe en el .dpr: se declaran.
//
//   GreetTool --name Jorge      -> 0, saluda
//   GreetTool --name Jorge --shout -> 0, saluda gritando
//   GreetTool --version         -> 0, la version
//   GreetTool --help            -> 0, la ayuda
//   GreetTool                   -> 2, falta un argumento obligatorio
//   GreetTool --name boom       -> 7, la politica traduce la excepcion
begin
  var NameOpt := CommandLine.Option<string>('--name', 'A quien saludar').Required;
  var ShoutOpt := CommandLine.Option<Boolean>('--shout', 'En mayusculas').WithAlias('-s');

  var B := TCliApplication.CreateBuilder(CommandLineConfig.ProcessArgs);
  B.RootCommand.WithVersion('2.0.0').Add([NameOpt, ShoutOpt]);

  // Solo una instancia: la segunda no llega a construir el contenedor.
  B.SingleInstance('daf-greet-tool');

  // La politica es un callback, asi que una suite puede compartir la suya sin heredar nada mas.
  B.OnUnhandled(
    function(const E: Exception; const Services: IServiceProvider): Integer
    begin
      Writeln(ErrOutput, 'greet: ', E.Message);
      Result := 7;
    end);

  B.OnExecute(
    function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
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

  ExitCode := B.Build.Run;
end.
```

`--version`, `--help` and a mistyped option are already handled: the **grammar** answers them, and
`Run` turns each outcome into an exit code. None of those four lines is written in the `.dpr`.

---

## The three samples

| Sample | What it uses |
|---|---|
| [`QuickStart`](samples/QuickStart/) | the builder, the work, the exit code — nothing else |
| [`ServiceWiring`](samples/ServiceWiring/) | the same, plus a service registered and resolved |
| [`GreetTool`](samples/GreetTool/) | the grammar, `--help`, exit codes by policy — the block above |

---

## The immediate style

The builder is usable **the moment it is created**: `Configuration` and `Environment` already answer,
so a source can be chosen from what is already configured without a deferred callback.

```pascal
var B := TCliApplication.CreateBuilder(Args);

// The environment already responds -- this is the case the callback style existed for.
JsonConfig.AddFile(B.Configuration, TPath.Combine(B.Environment.BinPath, 'appsettings.json'));

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
| `SingleInstance(Id)` | Whether the process starts at all | **Before** the container is built |
| `OnUnhandled(Policy)` | What happens to what escapes, and with which exit code | Around `Run` |
| `OnExecute(Handler)` | The work, written in place | Inside `Run` |
| `ICliHandler` registered in `Services` | The work, resolved from the container | Inside `Run` |
| `RootCommand` | The grammar that owns `argv` | At `Build` |

Declaring the work **both** ways raises `ECliCompositionError`. Two ways of saying the same thing is
a precedence rule nobody remembers.

### Exit codes

| Code | Meaning |
|---|---|
| `TCliExit.Ok` (0) | The work succeeded, or `--version` / `--help` answered |
| `TCliExit.Failure` (1) | What `ExitPolicy.LogAndFail` returns for an unhandled exception |
| `TCliExit.InvocationError` (2) | The invocation does not fit the declared grammar |
| `TCliExit.AlreadyRunning` (3) | Another instance holds the declared single-instance id |

`Run` returns an `Integer` and that is not decoration: a CLI application that cannot tell the system
how it ended is not usable in a script.

---

## Two readers of `argv`, and only one owner

`TCommandLineConfigurationProvider` and the grammar both read `argv`, with **different rules**, and
they disagree by construction. On `mytool --verbose report.txt`:

- the grammar sees the flag `--verbose` and the positional `report.txt`;
- the configuration provider sees `verbose = report.txt`.

Both answer without error, and one of the two is false. **In this variant the grammar owns `argv`**,
and the builder does not add the arguments as a configuration source on its own. Whoever wants that
view asks for it, knowing the two coexist:

```pascal
CommandLineConfig.AddArgs(B.Configuration, CommandLineConfig.ProcessArgs);
```

The Web, VCL and FMX variants — which declare no grammar — do add them: there is nothing to clash
with there.

---

## Where it diverges from .NET

| .NET | Here | Why |
|---|---|---|
| Extension methods (`AddX`, `UseX`) | Methods of the record, or a `class` facade | Delphi has no extension methods, and only **one** `record helper` is visible per type — a fluent API spread over units breaks silently when a `uses` is reordered |
| `IHostApplicationBuilder.Metrics` | **Not ported** | There is no equivalent piece in DAF to point at |
| `ConfigureContainer<T>` | **Not ported** | A generic method is not expressible in a Delphi interface — not rejected, unrepresentable |
| Handler dispatch inside `System.CommandLine` | `OnExecute` / `ICliHandler` | `Daf.CommandLine` parses and does not dispatch, on purpose: the outcome of an invocation is data |
| No app-global | Same | In console there is no `Application` global the user expects, so adding one would be a service locator with nothing to justify it |

The last row is the one that separates this variant from the desktop ones: **VCL and FMX do offer a
global**, because their platform already has one and denying it forces a worse mechanism.

---

## Package dependencies

`Daf.Cli` requires `Daf.Hosting` and `Daf.CommandLine`. The parser is **not optional** here: an
application that did not opt in would be left without `--version`, and covering that inside `Daf.Cli`
anyway would mean implementing it twice. The cost is that every CLI application of the family links
the parser — the same price .NET pays.
