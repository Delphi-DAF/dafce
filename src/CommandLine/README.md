# Daf.CommandLine

**🌍 Language: English | [Español](README.es.md)**

A port of .NET's [`System.CommandLine`](https://learn.microsoft.com/dotnet/standard/commandline) —
the library the `dotnet` CLI itself is built on. Declare commands, subcommands, options and
positional arguments, then read the values back **with the very objects you declared**.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Why the declared object is the key

```pascal
uses Daf.CommandLine;

var FileOpt := CommandLine.Option<string>('--file', 'File to compile');
var Root := CommandLine.Root('mytool', 'A tool');
Root.Add(FileOpt);

var Parsed := Root.Parse;                  // reads ParamStr
var Path := FileOpt.ValueIn(Parsed);       // a string, statically
```

`Path` is a `string`, not a `TValue`, and the name `--file` is written **once**. A name spelled
twice — once to declare, once to read — is a mismatch no compiler can see, and asking for the wrong
one gives back something that looks exactly like an option the user did not pass. With the object as
the key that mistake is not representable.

Reading an `IOption<Integer>` into a `string` is a **compile** error, not a conversion that fails at
run time.

---

## Concepts

| Concept | Description |
|---------|-------------|
| `ICommand` | A command: its options, its positional arguments and its subcommands |
| `IRootCommand` | Where a grammar starts; the only one that can `Parse`, and the one that carries the version |
| `IOption<T>` | A named, dashed value — `--file main.pas`. Also the key it is read with |
| `IArgument<T>` | A **positional** value, identified by its place: `mytool build main.pas` |
| `TArity` | How many values a symbol admits, minimum and maximum |
| `ICommandLineParseResult` | What an invocation produced: the leaf reached, the values, the errors, the help, the version |
| `CommandLine` | The entry point: `Option<T>`, `Argument<T>`, `Command`, `Root` |

---

## Declaring a grammar

Fluent methods live **on the interfaces**, never on a `class helper`: only one helper per type is in
scope and helpers never apply to interfaces, so a fluent API spread over units would break — silently,
last `uses` winning — the moment somebody reordered a clause.

```pascal
var VerboseOpt := CommandLine.Option<Boolean>('--verbose', 'Say more').Recursive;
var OutOpt     := CommandLine.Option<string>('--out', 'Where it goes').WithDefault('bin\out.exe');
var LevelOpt   := CommandLine.Option<TVerbosity>('--level', 'How much to say');
var FilesOpt   := CommandLine.Option<string>('--file', 'Files').WithArity(TArity.ZeroOrMore);
var KeyOpt     := CommandLine.Option<string>('--key', 'API key').Required;
var TargetArg  := CommandLine.Argument<string>('target', 'The project to build');

var Native := CommandLine.Command('native', 'Build a native binary').Add([TargetArg, OutOpt]);
var Build  := CommandLine.Command('build', 'Build things').WithAlias('b').Add(Native);

var Root := CommandLine.Root('buildtool', 'A small build tool').WithVersion('1.0.0');
Root.Add([VerboseOpt, Build]);
```

`Add` takes one symbol or an open array of them — the nearest thing Delphi has to C#'s collection
initializer.

| Feature | How |
|---------|-----|
| Subcommands, any depth | `Parent.Add(Child)`. A command that only groups requires a leaf to be named |
| Positional arguments | `CommandLine.Argument<T>(...)`, bound by position in declaration order |
| Command aliases | `.WithAlias('b')` — `mytool b native` reaches `mytool build native` |
| Recursive options | `.Recursive` — declared once, available to every descendant |
| Defaults | `.WithDefault(Value)`, returned when the invocation did not carry the option |
| Required | `.Required` on an option; an argument is required by its arity |
| Arity | `.WithArity(TArity.ZeroOrMore)` and the other four |
| Bounded values | an enumerated `T`, or `.AcceptOnly(['fast', 'safe'])` |
| Several values per mention | `.AllowMultipleArgumentsPerToken` — `--items a b c` without repeating the name |
| Collecting what is not ours | `Command.AllowUnmatchedTokens`, read back as `UnmatchedTokens` |
| Version | `.WithVersion('1.0.0')` on the root |

---

## Reading values

```pascal
var Path  := OutOpt.ValueIn(Parsed);        // one value, statically typed
var Files := FilesOpt.ValuesIn(Parsed);     // TArray<string>, for an arity that admits many
```

`IParseResult.WasProvided` tells **absent** from **present but empty**: `--file ''` is not the same
answer as no `--file` at all.

---

## Arity, and why it is not a detail

Arity is a minimum and a maximum, and it is **derived from the type** so the ordinary case is never
written by hand: a `Boolean` admits none or one, anything else demands exactly one.

| | Meaning |
|---|---|
| `TArity.Zero` | no values |
| `TArity.ZeroOrOne` | none or one — the default for a `Boolean` |
| `TArity.ExactlyOne` | one, and it has to be there — the default for everything else |
| `TArity.ZeroOrMore` | any number |
| `TArity.OneOrMore` | at least one |

It buys three things that look unrelated:

- **Several values.** A maximum above one is what lets `--file a.pas --file b.pas` mean both — and,
  with `.AllowMultipleArgumentsPerToken`, `--file a.pas b.pas` as well.
- **Required, at the argument end.** A minimum of one *is* required; there is no second concept.
- **No silent last-wins.** With a maximum of one, repeating the option is an **error**. Quietly
  keeping the last value is a value the parser chose on its own — the same sin as reading `80xyz`
  as `80`.

On **options**, `Required` is a mark of its own, as in .NET: an option that takes one value may be
obligatory or not, and arity cannot say which.

---

## Values

A conversion consumes the **whole** token or fails. `--port 80xyz` is refused, naming the option and
the text received, rather than becoming `80`; and `-5` is a value, not an unknown option.

Types: `string`, `Integer`, `Boolean`, `Double` and **enumerated types**. An enumerated type names its
own accepted values, so they appear in the help and in the error without anybody writing them twice —
and they are matched without regard to case, as in .NET. `.AcceptOnly([...])` is the same closure for
a type that does not express it.

A `Boolean` option is a flag: naming it is enough, and `--flag false`, `--flag=false` and
`--flag true` all work.

`--` ends the options: everything after it is a value, dashes and all. Without it a positional whose
value is `--verbose` could not be passed at all.

---

## How an invocation can be written

The same invocation, written the way whoever is typing it already writes them. None of these forms
says anything the others do not: they exist so a tool built with this needs no README section
explaining why its own syntax is the exception.

| Form | Example | Same as |
|------|---------|---------|
| Value after a space | `--out bin\app.exe` | — |
| Value after `=` | `--out=bin\app.exe` | `--out bin\app.exe` |
| Value after `:` | `--out:bin\app.exe` | `--out bin\app.exe` |
| One-character alias glued to its value | `-obin\app.exe` | `-o bin\app.exe` |
| Bundled one-character options | `-fdx` | `-f -d -x` |
| Bundle followed by a value | `-fdo bin\app.exe` | `-f -d -o bin\app.exe` |
| Windows prefix | `/out bin\app.exe` | `--out bin\app.exe` |
| Response file | `@build.rsp` | the tokens its lines carry |

Every one of them is bounded the same way: **each fires only when the token does not already name
something declared**. `-fdx` becomes three because `f`, `d` and `x` are declared aliases; were there
an option called `-fdx`, that is what the token would stay. And a group with a letter nobody declared
is refused **whole**, naming the letter — taking the ones that exist and keeping quiet about the one
that does not would leave you believing you asked for something you did not.

### ⚠️ What admitting them takes away

A form admitted is a token that used to mean one thing and now means another. Three, concretely:

- **`-abc` may stop being an option called `-abc`.** Where `a`, `b` and `c` are declared
  one-character aliases, it is now three options.
- **A positional value starting with `/` may be read as an option** — but only when what follows the
  slash names a declared one. `/tmp/app.dpr` is still a value; `/out` is not, if `--out` exists.
- **A positional value starting with `@` is read as a response file**, and one naming a file that is
  not there is a parse error rather than a token.

`--` gives all three back: everything behind it is a value, at signs, slashes and dashes included.
There is no switch to turn a form off, and none is needed — a grammar that has to take one of those
values as written has `--`.

### Response files

`@file.rsp` puts the file's lines in as tokens, where the `@` was. It exists for two concrete
reasons: a command line that does not fit the console's limit, and a long invocation that repeats.

```
# what this invocation is for -- a line starting with # contributes nothing
build
native
app.dpr
--out
bin\app.exe
@common.rsp
```

One line, one token: nothing is split on spaces, no variable is expanded and no path is resolved
against anything — the less it does, the less there is to explain about where a token came from. A
file may name another; the nesting is bounded, and one that names itself fails saying so instead of
hanging.

### Several values behind one name

```pascal
var ItemsOpt := CommandLine.Option<string>('--items', 'Several at once')
  .WithArity(TArity.ZeroOrMore).AllowMultipleArgumentsPerToken;
```

`--items a b c` then means all three. It is **opt-in, and could not be anything else**: were every
option with room for many values greedy by default, a positional written behind one would stop being
a positional. The right default depends on the grammar and only whoever wrote it knows — .NET calls
it the same thing, for the same reason.

### A command that wraps another

```pascal
var Sudo := CommandLine.Root('sudo', 'Runs something as somebody else');
Sudo.Add(UserOpt);
Sudo.AllowUnmatchedTokens;   // a statement, not a link in a chain: it answers ICommand

var Parsed := Sudo.Parse(['-u', 'admin', 'apt', 'update']);
var Rest   := Parsed.UnmatchedTokens;   // ['apt', 'update']
```

Without it — and that is still the default — a token matching nothing is a parse error. For anything
that wraps nobody an unrecognised token is a typo, and swallowing it in silence is the worse of the
two failures.

---

## Help, `--help` and `--version`

Help is rendered by walking the grammar, every time it is asked for. No composed text is stored
anywhere, so it cannot drift from what the program actually accepts — add an option and it appears.
The only text you declare is each description; the accepted values, the defaults and which symbols
are required come from the grammar itself.

```
$ buildtool --help
A small tool that builds and serves things

Usage:
  buildtool <command> [options]

Commands:
  build, b              Build things
  serve                 Serve what was built

Options:
  --verbose, -v         Say more about what is happening
  --help, -h            Show help and usage information
  --version             Show version information
```

Every grammar answers `--help` and `-h`, and every **root** answers `--version`, without declaring
anything. Asking for either is a **valid outcome**, not a parse failure: `HelpRequested` /
`VersionRequested` are true and `Errors` is empty. Help is resolved *before* the grammar is validated,
because a tool that demands you get the syntax right in order to explain the syntax is no use. An
application that declares its own `--help` or `--version` wins — it is the one that knows its case.
`--version` sits on the root and not on each subcommand, as in .NET: a version belongs to the tool.

---

## Divergences from `System.CommandLine`, each with its cause

**1 · The value is read the other way round.** In .NET you write `result.GetValue(fileOption)`. Here
you write `fileOption.ValueIn(parsed)`: the object asks the result. Measured 2026-08-24, Delphi 37:

- An interface **cannot declare a generic method at all** — `E2535 Interface methods must not have
  parameterized methods`. So `ICommandLineParseResult.GetValue<T>` cannot exist, whatever else were
  true.
- Even on a class, Delphi does not infer `T` through an `IOption<T>` parameter — `E2010 Incompatible
  types: 'IOption<T>' and 'IOption<string>'`. Inference works when the parameter *is* `T`, not when
  `T` is nested inside another generic.

The .NET call would cost giving up counted interfaces **and** spelling `<string>` out by hand at
every read — reintroducing exactly the repetition this module exists to remove. `ValueIn` needs no
inference, because `T` is already fixed by the receiver.

**2 · Names are matched without regard to case.** .NET is case-**sensitive** by POSIX convention;
here `--OUT` and `--out` are the same option. The cause is the surroundings and not convenience:
Delphi is a case-insensitive language and Windows CLIs are case-insensitive by habit, so a DAF tool
that distinguished `--Out` from `--out` would surprise its own audience. The accepted cost: two
aliases differing only in case are the same alias, and a CLI where the case *is* the meaning — `git
clean -x` against `-X` — cannot be expressed. Declare two options for that.

---

## What is not here (yet)

Nothing below changes the shape of the API, so all of it is additive:

- **Types and validation** — the rest of what .NET binds (`Int64`, `DateTime`, `Guid`, `FileInfo`,
  arrays), custom validators, custom parsers, and defaults computed by a lambda.

**There is no invocation, and that one is a decision rather than a gap.** `SetAction` / `Invoke` —
dispatching to the leaf and running a handler — are deliberately out: that turns the library into the
runner of the whole CLI and drags exit codes, cancellation and middleware behind it. `Daf.CommandLine`
parses and hands back typed values; what to do with them is the application's decision. The
`[diagram]` and `[suggest]` directives and shell completion ride on invocation, so they are out with
it.

---

## Not to be confused with configuration

Reading the command line as a **configuration source** — `--Logging:LogLevel=Debug` overriding a
value from `appsettings.json` — is a different job and belongs to
[`Daf.Configuration.CommandLine`](../Configuration/README.md), the same way
`Microsoft.Extensions.Configuration.CommandLine` does not use `System.CommandLine`.

[`Daf.CmdLn`](../CmdLn/README.md) is the earlier, feature-frozen module this one replaces. There is
no adapter between them and no automatic migration: they are different APIs on purpose, and a layer
joining them would preserve precisely what is being replaced.

---

## Sample

[`samples/BuildTool`](samples/BuildTool) declares a two-level grammar and reads its values. Run it
with no arguments and it walks a set of invocations, including the four that fail.
