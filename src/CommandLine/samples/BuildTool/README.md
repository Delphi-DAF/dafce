# BuildTool — a two-level grammar

Declares a command tree two levels deep and reads its values back through the objects that declared
it. It is the smallest thing that shows every piece of [`Daf.CommandLine`](../../README.md) at once:
subcommands, a command alias, a positional argument, a recursive option, defaults, generated help,
a value the engine refuses rather than guesses, a type of its own with the conversion that reads it,
a validation of a whole command, the `[diagram]` directive, a text of its own in the help, and the
several ways the same invocation may be written.

```
buildtool                        --verbose, -v   recursive: every leaf inherits it
|                                [diagram]       writes how an invocation was understood
+-- build   (alias: b)                           only groups: not invocable itself
|   +-- native <target>          --out           default bin\out.exe
|   |                            --min-sdk       a type of this program's own
|   +-- web    <target>          --minify, -m    never together with --pretty
|                                --pretty
|
+-- serve                        --port          default 8080, from 1 to 65535
                                 --level         Quiet|Normal|Detailed
```

`native`, `web` and `serve` are the leaves. `build` only groups, so `buildtool build` is an
**incomplete** invocation rather than an invocation of `build`.

## Running it

With no arguments it walks a set of invocations, including the seven that fail, and prints the exit
code of each:

```
out\Win32\samples\bin\BuildTool.exe
```

With arguments it parses those instead:

```
BuildTool.exe --help
BuildTool.exe build native app.dpr --out bin\app.exe --verbose
BuildTool.exe b web site.dpr --minify
BuildTool.exe serve --port 8080
BuildTool.exe [diagram] b native app.dpr -v
```

Three of the walkthrough are invocations from above, written differently: `serve --port:9090`,
`serve --port=9090` and `b web site.dpr -vm`. The last is a bundle of the one-character aliases `-v`
and `-m`, and reads as `-v -m`.

## What to look at

Everything lives in one class, `TBuildTool`, and each part answers a different question.

- **`DeclareGrammar` — the grammar is held in fields, and that is not tidiness.** Those objects
  **are** the keys the values are read back with, so whoever declares an option has to keep it. No
  name is written twice anywhere in the program.

- **Every call in it is `Add`, and no two add the same *kind* of thing.** It is worth reading line by
  line, because the whole shape of the grammar comes from which kind goes where: a leaf gets a
  positional argument and its own options; `build` gets **subcommands**, which is what turns it into a
  group; the root mixes a recursive option with two subcommands. Adding `--verbose` once, at the root,
  is what puts it on every leaf.

- **`DeclareActions` gives each leaf its action, and `build` none.** `FNative.SetAction(ReportNative)`
  takes a method of the class, with no code of its own, so the invocation answers 0 for it. Nothing
  in the program compares `Parsed.Command` against anything: `Invoke` runs the action of the leaf
  reached. `build` has no action, and that is what keeps it a group.

- **Each value arrives with its declared type.** `--out` is a `string`; `--port` is an `Integer` that
  goes straight into arithmetic with no conversion. Reading either into the other's type is a
  **compile** error, not a run-time surprise.

- **`--min-sdk` is a type the engine knows nothing about, and it took one place to admit it.** The
  conversion is passed where the option is declared, so it travels with the option and nothing in DAF
  changed. Its refusal comes out through `Errors` like any other invalid invocation, with the message
  the conversion itself wrote — a type a third party adds fails as well as one of serie.

- **`web` refuses `--minify` together with `--pretty`, and neither option can say so.** The rule is
  about the invocation of the command as a whole, so it is a validation of the command:
  `FWeb.Validators.Add(RefuseMinifyWithPretty)`. It receives the result of the command, asks it for
  the result of each option, and refuses with `AddError`.

- **`[diagram]` is declared on the root, and writes how an invocation was understood.**
  `buildtool [diagram] b native app.dpr -v` runs no action and writes
  `[ buildtool [ b [ native <app.dpr> [ -v ] *[ --out <bin\out.exe> ] ] ] ]`: the alias as it was
  written, the recursive option on the leaf, and `*` on the default `--out` took.

- **The help writes `Synopsis:` where .NET writes `Usage:`.** `UseOwnTexts` assigns that one text to
  `CommandLine.Texts` before the grammar is declared; every other text stays .NET's.

- **`Run` is `Parse(Args).Invoke`, and what it answers is the exit code.** Help and version are
  written and answer 0. A parse error writes what was probably meant, then the errors to the error
  output, then the help of the command reached, and answers 2. Asking for help clears the errors of
  the rest of the invocation.

- **`Walk` invokes with a `TInvocationConfiguration` whose `Error` is a `TStringWriter`**, so the
  walkthrough writes each error right after the invocation that caused it, followed by its exit
  code. It leaves out the help that follows an error: the `Action` of the parse result is then an
  `IParseErrorAction`, and its `ShowHelp` turns it off. Run with arguments, the tool writes all of it.

## The invocations that fail on purpose

| Invocation | What it shows |
|---|---|
| `buildtool build` | A command that only groups is not an invocation of its own: `Required command was not provided.` |
| `buildtool build native` | `<target>` is required by its arity: a string argument admits exactly one value, and nobody wrote "required" |
| `buildtool serve --port 80xyz` | The engine refuses the whole token, naming the option and the text, instead of quietly meaning `80` |
| `buildtool serve --level shouting` | The enumerated type refuses what is not one of its values, and lists the ones it has |
| `buildtool build native app.dpr --min-sdk 11.4` | The conversion declared with the option refuses it, with its own message |
| `buildtool serve --port 70000` | A validation of the application's own: the type admits the number and the range does not, and a range is written over that same hook |
| `buildtool b web site.dpr --minify --pretty` | Each option is valid on its own, and the validation of `web` refuses the two |

And `--help` is never declared anywhere, yet answers.
