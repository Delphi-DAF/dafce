# BuildTool — a two-level grammar

Declares a command tree two levels deep and reads its values back through the objects that declared
it. It is the smallest thing that shows every piece of [`Daf.CommandLine`](../../README.md) at once:
subcommands, a command alias, a positional argument, a recursive option, defaults, generated help,
and a value the engine refuses rather than guesses.

```
buildtool                        --verbose    recursive: every leaf inherits it
|
+-- build   (alias: b)                        only groups: not invocable itself
|   +-- native <target>          --out        default bin\out.exe
|   +-- web    <target>          --minify
|
+-- serve                        --port       default 8080
```

`native`, `web` and `serve` are the leaves. `build` only groups, so `buildtool build` is an
**incomplete** invocation rather than an invocation of `build`.

## Running it

With no arguments it walks a set of invocations, including the two that fail:

```
out\samples\bin\Win32\BuildTool.exe
```

With arguments it parses those instead:

```
BuildTool.exe --help
BuildTool.exe build native app.dpr --out bin\app.exe --verbose
BuildTool.exe b web site.dpr --minify
BuildTool.exe serve --port 8080
```

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

- **`BindActions` maps each leaf to a handler, and keys it by the command *object*.**
  `Parsed.Command = Action.Leaf`, never `Parsed.Command.Name = 'native'` — that would put a quoted
  name back in the middle of the program, which is the mistake this module exists to remove. There is
  no ladder of `if`s and no `case` on a string.

- **That table is application code, and deliberately so.** `Daf.CommandLine` parses and hands back
  the leaf it reached; **it does not invoke anything**. Dispatch is a non-goal of the module, because a
  library that runs your handlers ends up owning exit codes, cancellation and middleware too. The
  table is where that frontier sits — three lines, and yours.

- **Each value arrives with its declared type.** `--out` is a `string`; `--port` is an `Integer` that
  goes straight into arithmetic with no conversion. Reading either into the other's type is a
  **compile** error, not a run-time surprise.

- **`Run` answers help before judging anything.** Asking how a tool works is an outcome, not a
  mistake, so it is never reported as a parse error.

## The two invocations that fail on purpose

| Invocation | What it shows |
|---|---|
| `buildtool build` | A command that only groups is not an invocation of its own — the error names the subcommands it expected |
| `buildtool serve --port 80xyz` | The engine refuses the whole token, naming the option and the text, instead of quietly meaning `80` |

And `--help` is never declared anywhere, yet answers.
