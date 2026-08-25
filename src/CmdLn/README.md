# CmdLn

**🌍 Language: English | [Español](README.es.md)**

A typed command-line grammar: declare commands, subcommands and arguments, then parse an
invocation against that declaration instead of walking `ParamStr` by hand.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

---

## ⚠️ This module is feature-frozen

`Daf.CmdLn` still works and its specs pass, but **no new features will be added to it**. Its
replacement exists and is named [`Daf.CommandLine`](../CommandLine/README.md) — a port of .NET's
`System.CommandLine`, the library the `dotnet` CLI itself uses. **Start there instead**; this module
is retired in the next major.

The reason is the shape of the API rather than any missing piece. In `System.CommandLine` the option
object you declare *is* the key you read the value with, so a name is never spelled twice. Here you
write `'file'` when declaring and `'build.file'` when reading, and nothing checks that the two agree:
a typo yields an empty value, not an error. Retrofitting that would touch the core of how the grammar
is consumed, so it is being replaced rather than extended.

Also missing, and not planned here: generated help, positional arguments, options inherited by
subcommands, and command aliases. `Daf.CommandLine` has all four.

**Known limitations**, left as they are for the same reason:

- Integer and float arguments match unsigned patterns, so a **negative number cannot be passed**.
- Patterns are anchored at the start but not at the end, so `--port 80xyz` is accepted as `80`.

Both are fixed in `Daf.CommandLine`, which does not inherit this engine.

If you are starting a new command-line application, use
[`Daf.CommandLine`](../CommandLine/README.md). There is no adapter and no automatic migration between
the two: they are different APIs on purpose.

---

## Declaring a grammar

```pascal
uses Daf.CmdLn.Parser;

var Parser := TCmdLnParserBuilder.Create
  .Arg<string>('file')      // named string argument
  .Arg<Integer>('port')     // named integer argument
  .Arg<Boolean>('verbose')  // boolean flag: naming it is enough
  .Build;

var Params := Parser.Parse(GetCommandLine);

var FilePath  := Params['file'].AsString;
var Port      := Params['port'].AsInteger;
var IsVerbose := Params['verbose'].AsBoolean;
```

`Parse` returns `nil` when the invocation does not match the grammar.

### Short names, defaults and required arguments

```pascal
.Arg<string>('output|o', procedure(A: TArgNode)
  begin
    A.Default('out').Help('Target directory').Required(True);
  end)
```

The type drives the conversion: an enumerated argument accepts only its own value names, derived
from RTTI.

### Subcommands

Every command owns its arguments and its subcommands, and nesting has no depth limit. A command that
declares subcommands requires one of them to be named.

```pascal
var Parser := TCmdLnParserBuilder.Create
  .Command('build')
    .Arg<string>('output')
  .EndCommand
  .Command('run')
    .Arg<Boolean>('watch')
  .EndCommand
  .Build;

var Params := Parser.Parse(GetCommandLine);
if Params.Command = 'build' then
begin
  var Section := Params.GetSection('build');
  var OutDir  := Section['output'].AsString;
end;
```

⚠️ Arguments of a command are consumed **before** its subcommand, so `app --flag build -o dir` parses
and `app build -o dir --flag` does not.

---

## Not to be confused with configuration

Reading the command line as a **configuration source** — `--Logging:LogLevel=Debug` overriding a
value from `appsettings.json` — is a different job, and it belongs to
[`Daf.Configuration.CommandLine`](../Configuration/README.md). The two do not share an engine, the
same way `Microsoft.Extensions.Configuration.CommandLine` does not use `System.CommandLine`.
