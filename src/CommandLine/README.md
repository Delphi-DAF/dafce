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

var FileOpt := CommandLine.Option<string>('--file').Description('File to compile');
var Root := CommandLine.Root('mytool', 'A tool');
Root.Add(FileOpt);

var Parsed := Root.Parse;                  // reads ParamStr
var Path := FileOpt.ValueIn(Parsed);       // a string, statically
```

`Path` is a `string`, not a `TValue`, and the name `--file` is written **once**. A name spelled
twice — once to declare, once to read — is a mismatch no compiler can see.

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
| `TArgumentArity` | How many values a symbol admits, minimum and maximum |
| `IParseResult` | What an invocation produced: the tree of results, the leaf reached, the errors, the help, the version. `Invoke` runs it |
| `ICommandResult`, `IOptionResult`, `IArgumentResult` | The result of one symbol the invocation reached: its tokens, its errors, its parent |
| `TParseError` | One error of the parse, and the result of the symbol it belongs to |
| `TToken` | One token of the invocation, as the parse read it |
| `ICommandLineAction` | What an invocation runs: the action of a command or of an option. `TSynchronousCommandLineAction` is the base of one written as a class |
| `TCommandAction` | The action of a command written as a function: receives the parse result, answers the exit code |
| `IHelpOption`, `IVersionOption` | The help and the version: two options every root has, each with an action that writes them |
| `IParseErrorAction` | What `Invoke` runs for a parse error: what was meant, the errors and the help |
| `TInvocationConfiguration` | Where `Invoke` writes, and whether a default handler catches what the action lets escape |
| `TCommandLineExit` | The codes `Invoke` answers when the action is not the one deciding |
| `CommandLine` | The entry point: `Option<T>`, `Argument<T>`, `Command`, `Root`, `HelpOption`, `VersionOption` |

---

## Declaring a grammar

Fluent methods live **on the interfaces**, never on a `class helper`.

```pascal
var VerboseOpt := CommandLine.Option<Boolean>('--verbose').Description('Say more').Recursive;
var OutOpt     := CommandLine.Option<string>('--out').Description('Where it goes').DefaultValue('bin\out.exe');
var LevelOpt   := CommandLine.Option<TVerbosity>('--level').Description('How much to say');
var FilesOpt   := CommandLine.Option<string>('--file').Description('Files').Arity(TArgumentArity.ZeroOrMore);
var KeyOpt     := CommandLine.Option<string>('--key').Description('API key').Required;
var TargetArg  := CommandLine.Argument<string>('target').Description('The project to build');

var Native := CommandLine.Command('native', 'Build a native binary').Add([TargetArg, OutOpt]);
var Build  := CommandLine.Command('build', 'Build things').Add(Native);
Build.Aliases.Add('b');

var Root := CommandLine.Root('buildtool', 'A small build tool').WithVersion('1.0.0');
Root.Add([VerboseOpt, Build]);
```

`Add` takes one symbol or an open array of them — the nearest thing Delphi has to C#'s collection
initializer.

| Feature | How |
|---------|-----|
| Subcommands, any depth | `Parent.Add(Child)`. A command with subcommands and no action requires a leaf to be named |
| Positional arguments | `CommandLine.Argument<T>(...)`, bound by position in declaration order |
| Aliases | `CommandLine.Option<T>('--verbose', ['-v'])` on an option; `Command.Aliases.Add('b')` on a command — `mytool b native` reaches `mytool build native` |
| Recursive options | `.Recursive` — declared once, available to every descendant |
| Defaults | `.DefaultValue(Value)`, returned when the invocation did not carry the option |
| Required | `.Required` on an option; an argument is required by its arity |
| Arity | `.Arity(TArgumentArity.ZeroOrMore)` and the other four |
| Bounded values | an enumerated `T`, or `.AcceptOnlyFromAmong(['fast', 'safe'])` |
| Several values per mention | `.AllowMultipleArgumentsPerToken` — `--items a b c` without repeating the name |
| Collecting what is not ours | `Command.AllowUnmatchedTokens`; `UnmatchedTokens` gives back what nothing claimed, error or not |
| Version | `.WithVersion('1.0.0')` on the root; also `.WithVersion(tag, name, company)` or `.WithVersion(TSomeClass)` to take it from a `[Version]` annotation. `IParseResult.VersionInfo` gives it back |

---

## Reading values

```pascal
var Path  := OutOpt.ValueIn(Parsed);        // one value, statically typed
var Files := FilesOpt.ValuesIn(Parsed);     // TArray<string>, for an arity that admits many
```

What `ValueIn` answers depends on what the parse made of the symbol:

| The symbol | `ValueIn` answers |
|---|---|
| carried by the invocation | its value |
| not carried, with a declared default, on a command the invocation went through | the declared default |
| not carried, in any other case | the default of its **type**: `0`, `''`, an empty array |
| refused by the parse: a value that does not convert, a repetition, a required option that is missing | nothing: it raises `ECommandLine` with that error |

The parse validates the command reached whole, and the commands above it only in their recursive
options and in the ones with a default. An option of a parent that is neither, written before the
subcommand, is taken but not validated: `tool --port x run` parses, and reading `--port` raises
`"x" is not a valid integer for --port`.

`ValueIn` also takes the result of a symbol — `PortOpt.ValueIn(Parsed.CommandResult)` — and answers
the same: the tree is one, whichever node asks. `RequiredValueIn` reads the same way and raises when
the symbol has no result at all. On an argument with no result it answers what .NET's
`GetRequiredValue` answers for the type: it raises for a `string`, a class or an interface, and any
other type gives its default.

`IParseResult.WasProvided` says whether the invocation **named** the symbol. `--file ''` is not the
same answer as no `--file` at all, and `--port 80xyz` was provided although reading it raises.

---

## The parse result is a tree

Every command the invocation reached has a result, from the root to the innermost one, and so does
every option and argument it carried or that took its default. `Parsed.RootCommandResult` is where
the tree starts and `Parsed.CommandResult` is the innermost command. `GetResult` finds the result of
a declared symbol, on the parse result or on any of its nodes, and answers `nil` when there is none.

```pascal
var Port := Parsed.GetResult(PortOpt);         // an IOptionResult, or nil
if (Port <> nil) and not Port.IsImplicit then
  Writeln('named ', Port.IdentifierTokenCount, ' time(s), with ', Length(Port.Tokens), ' value(s)');
```

| A result | answers |
|---|---|
| `Parent` | the result of the command it hangs from; `nil` for the root |
| `Tokens` | the tokens of the invocation that produced it |
| `Errors` | the errors of the parse that belong to it |
| `Children`, on a command | the results that hang from it |
| `IsImplicit`, on an option | the invocation did not name it: the parse created it for its default, or for a required option that is missing, and then it carries that error |
| `IsImplicit`, on an argument | it took its default |
| `IdentifierToken`, `IdentifierTokenCount`, on an option | the first token that named it, and how many times it was named |

Each error says whose it is: `TParseError.SymbolResult` is the result of its symbol, and `nil` for an
error of reading the invocation, such as a response file that does not exist. A token that nothing
claims is an error of the command reached, unless the root collects it.

The errors come out in this order:

1. a missing subcommand;
2. the errors of reading the invocation;
3. the tokens nothing claimed, in the order they were written;
4. the validation, from the innermost command up to the root: the innermost command's own
   validations, then each command's options in the order they were declared, then its arguments.

An option's arity is checked before its value is converted, and a failed one is all that option
reports: repeated with two values, it fails for the repetition whatever the values are. A required
option or argument whose value does not convert has a result, so it reports the conversion and not
that it is missing.

---

## Arity, and why it is not a detail

Arity is a minimum and a maximum, and it is **derived from the type** so the ordinary case is never
written by hand: a `Boolean` admits none or one, anything else demands exactly one.

| | Meaning |
|---|---|
| `TArgumentArity.Zero` | no values |
| `TArgumentArity.ZeroOrOne` | none or one — the default for a `Boolean` |
| `TArgumentArity.ExactlyOne` | one, and it has to be there — the default for everything else |
| `TArgumentArity.ZeroOrMore` | any number |
| `TArgumentArity.OneOrMore` | at least one |

It buys three things that look unrelated:

- **Several values.** A maximum above one is what lets `--file a.pas --file b.pas` mean both — and,
  with `.AllowMultipleArgumentsPerToken`, `--file a.pas b.pas` as well.
- **Required, at the argument end.** A minimum of one *is* required; there is no second concept.
- **No silent last-wins.** With a maximum of one, repeating the option is an **error**.

On **options**, `Required` is a mark of its own, as in .NET: an option that takes one value may be
obligatory or not, and arity cannot say which.

---

## Values

A conversion consumes the **whole** token or fails. `--port 80xyz` is refused, naming the option and
the text received, rather than becoming `80`; and `-5` is a value, not an unknown option.

### The types of serie, and what each one translates

The list is **translated** from .NET's rather than copied. What a command line needs is numbers of
the widths people use, dates, identifiers and paths.

| `System.CommandLine` | Here | Why |
|---|---|---|
| `int`, `long`, `short`, `byte` and unsigned | `Integer`, `Int64` | the two that cover the real use |
| `float`, `double` | `Double` | a binary float, which is what a ratio or a threshold is |
| `decimal` | `Currency` | the exact decimal: an amount written with up to four decimals comes back with those four, and does not round on its own |
| `bool` | `Boolean` | it is the flag: naming it is enough |
| enums | enumerated types | the type names its own values, matched without regard to case |
| `DateTime`, `DateTimeOffset`, `DateOnly`, `TimeOnly` | `TDateTime` | Delphi has **one**, so "date only" and "time only" are not declarable. Read as ISO 8601 and nothing else — divergence 3 below |
| `Guid` | `TGUID` | direct, and the braces are optional |
| `FileInfo`, `DirectoryInfo`, `FileSystemInfo` | `string` + `AcceptExistingOnly` | here a path is text; what those types contributed was the validation, and the validation is what gets ported |
| arrays and lists | `TArray<T>` | a declared collection derives `ZeroOrMore`, as in .NET |

An enumerated type names its own accepted values, so they appear in the help and in the error without
anybody writing them twice — and they are matched without regard to case, as in .NET.
`.AcceptOnlyFromAmong([...])` is the same closure for a type that does not express it.

A `Boolean` option is a flag: naming it is enough, and `--flag false`, `--flag=false` and
`--flag true` all work. Only `true` and `false`, in any casing, are booleans: `--flag 0` names the
flag, and the `0` goes to the next positional.

A **collection** is one value made of the tokens it was given, so reading it answers the whole array:

```pascal
FilesOpt := CommandLine.Option<TArray<string>>('--files').Description('Files to compile');
// tool --files a.pas --files b.pas   ->  FilesOpt.ValueIn(Parsed) = ['a.pas', 'b.pas']
```

Its arity comes out of the type, so `ZeroOrMore` is not written by hand. Repeating the name is what
fills it; `.AllowMultipleArgumentsPerToken` also allows `--files a.pas b.pas`.

`--` ends the options: everything after it is a value, dashes and all. Without it a positional whose
value is `--verbose` could not be passed at all.

---

## A type the engine does not know, and a validation it cannot express

Two hooks, both declared **on the symbol** and nowhere else.

### A conversion of its own

It comes in two shapes. The first converts **one token**, and is passed where the symbol is declared:

```pascal
MinOpt := CommandLine.Option<TSemVer>('--min',
  function(const Raw: string; out Value: TSemVer; out Error: string): Boolean
  begin
    Error := '';
    Result := TSemVer.TryParse(Raw, Value);
    if not Result then
      Error := Format('"%s" is not a version: write it as major.minor.patch', [Raw]);
  end,
  'Lowest version accepted');
```

The refusal comes out through `ParseResult.Errors` like any other, so a type a third party added fails
as well as one of serie. It also serves a type the engine already knows, for reading it some other way
— hexadecimal into an `Integer`, say.

The second converts **the whole result**. `CustomParser` receives the result of the symbol — every
token, `AddError` to refuse, the rest of the invocation through `GetResult` — and answers the value
whole. For a collection it answers the whole array:

```pascal
var SortedOpt := CommandLine.Option<TArray<string>>('--sorted').AllowMultipleArgumentsPerToken
  .CustomParser(
  function(const Found: IArgumentResult): TArray<string>
  var
    I: Integer;
  begin
    SetLength(Result, Length(Found.Tokens));
    for I := 0 to High(Result) do
      Result[I] := Found.Tokens[I].Value;
    TArray.Sort<string>(Result);
  end);
```

On an argument it can keep part of the tokens: `Found.OnlyTake(1)` keeps the first one and hands the
rest to the arguments that follow. An exception it raises leaves the parse as it is.

**On the symbol and not in a registry of types.** Its reach is exactly where it was written.

The limit **moved but did not disappear**: a type the engine does not cover and for which no
conversion is declared is refused **when the symbol is added to a command**, before any invocation is
read. A `CustomParser` is declared before that `Add`.

### Validations

`Validate` receives the value **already converted** and may refuse it. It runs **inside** the parse:

```pascal
CountOpt := CommandLine.Option<Integer>('--count').Description('How many to make')
  .Validate(
  function(const Value: Integer; out Error: string): Boolean
  begin
    Result := Value > 0;
    if not Result then
      Error := Format('--count has to be greater than zero, and %d is not', [Value]);
  end);
```

Several can be declared; the first refusal is the error for that value. A validation should be a pure
question about the value.

Two come as standard, written over that same hook:

| | What it asks |
|---|---|
| `.AcceptExistingOnly` | the path exists, file or directory. ⚠️ **It asks the file system** — the conscious exception to a validation being pure, so parsing stops being free |
| `.AcceptLegalFileNamesOnly` | it is a legal file name; the separators are refused too, because a name is not a path |

Both apply to a path, which here is a `string` — including a `TArray<string>`, where they run per
element. Declaring either on something that is not a path is refused at the declaration.

A validation of the **result** receives the result of its symbol and refuses with `AddError`.
`Validators.Add` declares one on a command, an option or an argument:

```pascal
Root.Validators.Add(
  procedure(const Result: ICommandResult)
  begin
    if (Result.GetResult(JsonOpt) <> nil) and (Result.GetResult(XmlOpt) <> nil) then
      Result.AddError('--json and --xml cannot be used together');
  end);
```

- The validations of a command run only when it is the innermost command the invocation reached, and
  before its options and arguments: an error of theirs keeps those from being validated.
- The ones of an option or an argument run after its arity is checked and before its value is
  converted: an error of theirs keeps it from being converted.
- An error that a validation of an option adds stays among the errors, and reading the option still
  gives its value. `Validate` and the two of serie fail the value itself, and reading it raises.

⚠️ **A conversion of one token, or a `Validate`, cannot be declared on a collection.** Both receive the
value of **one** token, and for a collection that value is an element — which `T` cannot name, because
Delphi has no way of saying "the element of `T`" in an interface. It is refused where it is declared.
`CustomParser`, which receives the whole result, is declared on a collection as on anything else.

---

## A default value that is a calculation

`DefaultValue(Value)` takes a constant, evaluated when the grammar is declared. `DefaultValueFactory`
takes a calculation, which the parse runs **when the invocation reaches the command that declares the
symbol and does not carry it**:

```pascal
OutOpt := CommandLine.Option<string>('--out').Description('Where to write')
  .DefaultValueFactory(
  function: string
  begin
    Result := TPath.GetTempPath;
  end, 'a temporary directory');
```

A default that depends on the surroundings cannot be written as a constant in a grammar that runs
before there is anything to know. Without this, the application leaves the default empty and fills it
in after reading — and then the help shows a default the program does not use.

Each parse runs it once, whether the value is read afterwards or not, and **the help does not run
it**: it shows the text declared beside it, and no default at all when there is none.

The calculation can also receive the result of the symbol, and read the rest of the invocation through
it:

```pascal
FileOpt := CommandLine.Option<string>('--file').Description('Where to write')
  .DefaultValueFactory(
  function(const Found: IArgumentResult): string
  begin
    Result := WhoOpt.ValueIn(Found) + '.txt';
  end, '<who>.txt');
```

---

## A symbol that does not appear in the help

```pascal
DiagOpt := CommandLine.Option<Integer>('--diag').Description('Diagnostics level').Hidden;
```

It is what a diagnostic option, one in transition, or one only an internal script uses needs: to exist
without being put in front of everybody who asks how the tool works.

Hiding is the **help's** business and nothing else. A hidden symbol is declared, parsed and read the
same, and when it gets an invalid value the error **names it**.

---

## How an invocation can be written

The same invocation, written the way whoever is typing it already writes them. None of these forms
says anything the others do not.

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
is refused **whole**, naming the letter. The help letter fits inside a group — `-fh` asks for help —
but does not make one: a token none of whose letters the command declares stays the token it was.

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
against anything. A file may name another; the nesting is bounded, and one that names itself fails
saying so instead of hanging.

### Several values behind one name

```pascal
var ItemsOpt := CommandLine.Option<string>('--items').Description('Several at once')
  .Arity(TArgumentArity.ZeroOrMore).AllowMultipleArgumentsPerToken;
```

`--items a b c` then means all three. It is **opt-in**: .NET calls it the same thing.

### A command that wraps another

```pascal
var Sudo := CommandLine.Root('sudo', 'Runs something as somebody else');
Sudo.Add(UserOpt);
Sudo.AllowUnmatchedTokens;   // a statement, not a link in a chain: it answers ICommand

var Parsed := Sudo.Parse(['-u', 'admin', 'apt', 'update']);
var Rest   := Parsed.UnmatchedTokens;   // ['apt', 'update']
```

Without it — and that is still the default — a token matching nothing is a parse error, and
`UnmatchedTokens` carries it all the same. A root that collects outranks a subcommand that does not.

### An argument that keeps the tail

An argument declared with `CaptureRemainingTokens` keeps every token that follows its place, options
included, so a command hands its tail to another program without asking for `--`:

```pascal
var Exec := CommandLine.Root('exec', 'Runs a command');
Exec.Add([VerboseOpt, CommandLine.Argument<TArray<string>>('command').CaptureRemainingTokens]);
// exec app --verbose -x   ->   command = ['app', '--verbose', '-x'], and --verbose is not given
```

An option before the first value is still an option: `exec --verbose app` gives `--verbose`.

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

Every **root** has a help option and a version option among its options, without declaring anything:
the help answers to `--help`, `-h`, `/h`, `-?` and `/?` on every command, and the version to
`--version`. Asking for either is a **valid outcome**, not a parse failure: `IsHelpRequested` /
`IsVersionRequested` are true. The whole invocation is still read, and asking for the help clears the
errors of the rest of it: `mytool --port abc --help` writes the help and answers 0. `--version` with
another option, argument or subcommand is an error of its own,
`--version option cannot be combined with other arguments.`, which stays in `Errors` while the version
is written all the same. An application that declares its own `--help` or `--version` wins — it is
the one that knows its case.
`--version` sits on the root and not on each subcommand, as in .NET: a version belongs to the tool.
What they write can be wrapped with something of your own: see *A banner above the help and the
version*.

---

## Each command, its action

A command declares what it does when an invocation reaches it, and the parse result runs it:

```pascal
var NameOpt := CommandLine.Option<string>('--name').Description('Who to greet').DefaultValue('world');
var Greet := CommandLine.Command('greet', 'Say hello').Add(NameOpt).SetAction(
  function(const Parsed: IParseResult): Integer
  begin
    Writeln('Hello, ', NameOpt.ValueIn(Parsed));
    Result := TCommandLineExit.Ok;
  end);

var Root := CommandLine.Root('greeter', 'Greets people');
Root.Add(Greet);

var Parsed := Root.Parse;                  // reads ParamStr
ExitCode := Parsed.Invoke;
```

The action receives the parse result and reads the values from it like any other code. `Invoke`
settles the invocation and answers the code the process ends with:

| The parse result | What `Invoke` does | What it answers |
|---|---|---|
| named an option with a terminating action — `--help` and `--version` among them | runs it: the default ones write the help of the command reached, or the version, to the output | the code of the action: `TCommandLineExit.Ok` (0) by default |
| carries errors | writes what was probably meant and the help of the command reached to the output, and the errors to the error — see *What a parse error writes* | `InvocationError` (2) |
| reached a command with an action | runs it | the code the action answers |
| reached a command without one | nothing | `Ok` (0) |

No command's action runs in the first two rows.

- An action with no code of its own is a `procedure(const Parsed: IParseResult)`, and the
  invocation answers `Ok` for it.
- Declaring the action again replaces it.
- A command with subcommands **and** an action is an invocation of its own: `mytool` alone runs
  `mytool`'s action. Without one it only groups, and `mytool` alone fails with
  `Command "mytool" requires a subcommand: …`.
- `SetAction` answers `ICommand`, so on a root it is a statement, like `Description(Text)`.
- `SetAction` also takes an `ICommandLineAction`: an action written as a class, on
  `TSynchronousCommandLineAction`. `Command.Action` gives it back.

### An option with an action

An option carries an action too: `Option.SetAction(Action)`, with an `ICommandLineAction`. One whose
`IsTerminating` is true runs instead of the action of the command reached, as the help and the version
do. Every one whose `IsTerminating` is false runs before it, in the order they were named, and so does
the one of an option that took its default. When one of those answers a code other than 0, that code
is what `Invoke` answers.

```pascal
type
  TTraceAction = class(TSynchronousCommandLineAction)
  public
    function IsTerminating: Boolean; override;
    function Invoke(const Parsed: IParseResult): Integer; override;
  end;

function TTraceAction.IsTerminating: Boolean;
begin
  Result := False;
end;

function TTraceAction.Invoke(const Parsed: IParseResult): Integer;
begin
  Parsed.InvocationConfiguration.WriteOutput('tracing on');
  Result := TCommandLineExit.Ok;
end;

var TraceOpt := CommandLine.Option<Boolean>('--trace').SetAction(TTraceAction.Create);
```

`IParseResult.Action` is the action `Invoke` runs last: the terminating one of a named option, the one
for a parse error, or else the action of the command reached.

### What a parse error writes

```
$ mytool --verbse
'--verbse' was not matched. Did you mean one of the following?
--verbose

Unknown option "--verbse" for mytool

<the help of mytool>
```

The suggestions and the help go to the output, and the errors to the error, followed by a blank
line. A name is suggested when it is at most 3 edits away from a token nothing claimed, and only the
closest are; a hidden option or command never is. `Parsed.Action` is then an `IParseErrorAction`,
whose `ShowTypoCorrections` and `ShowHelp` turn those two parts off:

```pascal
var OnError: IParseErrorAction;
if Supports(Parsed.Action, IParseErrorAction, OnError) then
  OnError.ShowHelp := False;
```

### Where it writes, and what it does with an exception

`Invoke(Configuration)` takes a `TInvocationConfiguration`; `Invoke` alone uses its defaults.

| Field | Default | What it says |
|---|---|---|
| `Output` | `nil`, the console | where the help, the version and the suggestions of what was meant are written — any `TTextWriter` |
| `Error` | `nil`, the console's error | where parse errors and escaped exceptions are written |
| `EnableDefaultExceptionHandler` | `True` | on, an exception leaving an action is written to `Error` as `Unhandled exception: EClass: message`, with its stack when a provider of stack traces is installed, and `Invoke` answers `TCommandLineExit.Failure` (1); off, it leaves `Invoke` and the caller sees it |

A `TInvocationConfiguration` declared and never assigned already holds those defaults:

```pascal
var Configuration: TInvocationConfiguration;
Configuration.Output := Writer;            // any TTextWriter
ExitCode := Parsed.Invoke(Configuration);
```

### A banner above the help and the version

The root gives the actions of its help and version options as `HelpAction` and `VersionAction`,
functions of the type a command's action takes. The default ones write the help of the command
reached, or the version, to the output of the invocation and answer `Ok`. To put something
of your own above them, replace the action with one that writes it and then calls the default one:

```pascal
function WithBanner(const Default: TCommandAction): TCommandAction;
begin
  Result :=
    function(const Parsed: IParseResult): Integer
    begin
      Parsed.InvocationConfiguration.WriteOutput('buildtool -- (c) ACME');
      Result := Default(Parsed);
    end;
end;

Root.HelpAction := WithBanner(Root.HelpAction);
Root.VersionAction := WithBanner(Root.VersionAction);
```

- The help is still the one derived from the grammar. A help action replaced on the root also wraps
  the help of every subcommand.
- `Parsed.InvocationConfiguration` is the configuration of the invocation that is running, and
  `WriteOutput` writes a line to its `Output`, or to the console when it is `nil`: the banner goes
  where the help goes.
- What the action answers is what `Invoke` answers, and the default exception handler covers it as
  it covers the action of a command.
- Assigning `nil` brings the default action back.
- Replacing them on the root replaces the action of its help or version option, which is where
  `System.CommandLine` keeps them. The replacement clears the parse errors, as the help does.

⚠️ Capture the default action, never the root. An action that holds the root it is installed on
forms a reference cycle with it, and neither is ever released.

---

## A frozen grammar

`Freeze` closes a grammar. From then on, adding a command, an option or an argument, or changing one
already declared — an alias, the arity, the accepted values, the default, a validation, the version —
raises `ECommandLine` naming the symbol. It reaches everything already declared below the root. The
actions stay open, the help and the version ones included, because they are read when the result is
invoked:

```pascal
Root.Freeze;
Root.SetAction(Greet);                             // fine
Root.Add(CommandLine.Option<Boolean>('--loud'));   // ECommandLine
```

Code that decides with a parse result can rely on the grammar staying the one it was parsed with.
`System.CommandLine` has no counterpart.

## Divergences from `System.CommandLine`

**1 · The value is read the other way round.** In .NET you write `result.GetValue(fileOption)`. Here
you write `fileOption.ValueIn(parsed)`: the object asks the result:

- An interface **cannot declare a generic method at all** — `E2535 Interface methods must not have
  parameterized methods`. So `IParseResult.GetValue<T>` cannot exist, whatever else were
  true.
- Even on a class, Delphi does not infer `T` through an `IOption<T>` parameter — `E2010 Incompatible
  types: 'IOption<T>' and 'IOption<string>'`. Inference works when the parameter *is* `T`, not when
  `T` is nested inside another generic.

`ValueIn` needs no inference, because `T` is already fixed by the receiver.

**2 · Names are matched without regard to case.** .NET is case-**sensitive** by POSIX convention;
here `--OUT` and `--out` are the same option. The accepted cost: two aliases differing only in case
are the same alias, and a CLI where the case *is* the meaning — `git clean -x` against `-X` — cannot
be expressed. Declare two options for that.

**3 · A date is read as ISO 8601, and in no other way.** .NET's `DateTime` also takes the local
convention. Here `--since 2026-08-25` works and `--since 25/08/2026` is refused, naming the option
and the text. The accepted cost: somebody typing by hand has to write the date the way the machine
reads it. And since Delphi has a single date type, "only a date" and "only a time" are not
declarable; whoever needs that bounds it with a validation.

**4 · A parse error answers 2, not 1.** .NET answers 1 both to a parse error and to an exception
nobody caught. Here `Invoke` answers `TCommandLineExit.InvocationError` (2) to an invocation that
does not fit the grammar, and 1 stays for an action that failed.

**5 · The invocation is synchronous.** There is no asynchronous action, no cancellation and no
handling of the process termination — no Ctrl+C, no grace period before terminating.
`TInvocationConfiguration` carries the output, the error and the default exception handler, and
nothing else.

**6 · Reading a refused value raises `ECommandLine`.** .NET raises `InvalidOperationException`. Here
it is the module's own exception, the one it raises for a grammar declared wrong or changed once
frozen.

**7 · Nothing is written in color.** .NET writes parse errors and an escaped exception in red on a
console. Here they are plain text, and an escaped exception carries its stack only when a provider of
stack traces is installed.

**8 · An option the application declares wins over the help and the version of the root.** In .NET
the help and version options a root brings answer first; here an option declared with one of their
names does.

---

## What is not here (yet)

Nothing below changes the shape of the API, so all of it is additive:

- **The widths nobody has asked for yet** — `short`, `byte` and the unsigned ones. A type enters when
  somebody asks for it; until then a conversion of its own covers it in one line.

The `[diagram]` and `[suggest]` directives and shell completion are not ported.

---

## Not to be confused with configuration

Reading the command line as a **configuration source** — `--Logging:LogLevel=Debug` overriding a
value from `appsettings.json` — is a different job and belongs to
[`Daf.Configuration.CommandLine`](../Configuration/README.md), the same way
`Microsoft.Extensions.Configuration.CommandLine` does not use `System.CommandLine`.

`Daf.CmdLn` was the earlier module this one replaces. It was withdrawn in 2.0.0, with no adapter and
no unit alias: they are different APIs. `MIGRATION-2.0.md` has the two declarations side by side.

---

## Sample

[`samples/BuildTool`](samples/BuildTool) declares a two-level grammar, gives each leaf its action and
invokes what it parses. Run it with no arguments and it walks a set of invocations, including the six
that fail, with the exit code of each.
