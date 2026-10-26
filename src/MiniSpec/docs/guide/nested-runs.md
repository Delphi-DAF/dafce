# Nested Runs

**🌍 Language: English | [Español](nested-runs.es.md)**

[← Back to Guide](../GUIDE.md)

---

A `TSpecRun` is a self-contained execution: it owns its suite, its context, its injector, its
options and its runner with its listeners. It can be launched **inside** a running scenario without
altering the counts of the run that contains it and without writing a line into its report.

That is what makes the rules of the runner checkable with specs instead of by hand.

## Building and running one

```pascal
uses Daf.MiniSpec, Daf.MiniSpec.Types, Daf.MiniSpec.Runner;

type
  TToyWorld = class
  public
    Touched: Boolean;
  end;

var Run := TSpecRun.Create('Nested');
try
  Feature('A toy feature that fails', Run.Suite)
    .UseWorld<TToyWorld>
    .Scenario('It fails on purpose')
      .&Then('one is not two', procedure(W: TToyWorld)
        begin
          Expect(1).ToEqual(2);
        end);

  Run.Execute;

  Expect(Run.FailCount).ToEqual(1);   // the nested run counted its own failure
  Expect(Run.ExitCode).ToEqual(1);    // and says which exit code its counts deserve
  Expect(ExitCode).ToEqual(0);        // the process is untouched
finally
  Run.Free;
end;
```

`TSpecRun` is a plain class, so it is freed by whoever created it — usually the World of the
scenario that runs it.

`Feature(Description, Suite)` is the overload that declares the feature **in the suite it is
given** instead of in the suite of the process. It is the only way a feature tree reaches a nested
run: the one-argument `Feature` always declares into the process suite.

## What stays outside a nested run

The split with `MiniSpec` follows one rule: whatever only makes sense once per process stays in the
facade — the command-line arguments, the `.ini` file, the banner and the exit code of the process.

`Run.ExitCode` is the code that execution deserves, as a function of its own counts. Whoever
launched it decides what to do with it; only the root run carries it to the process.

## Reports

A nested run prints nothing until it is given a destination:

```pascal
Run.AddListener(Listener);
```

With no listener there is no output, which is what allows a run to be nested inside a scenario
without dirtying its report. The available destinations are in [Reporters](reporters.md).

## Its own suite context

```pascal
Run.UseSuiteContext<TMyContext>;
```

Fixes how the suite context of **this** execution is built. An exception escaping that constructor
is the one case that escapes `Run.Execute`: whatever blows up inside a scenario is caught by the
runner itself and counted as a failure.

## What a nested run reports

| Member | Answers |
|--------|---------|
| `Suite` | The suite of this execution — what `Feature(Description, Suite)` takes |
| `Runner` | Its runner, with its own listeners |
| `Options` | The options of this execution, independent of the process ones |
| `Injector` | Its injector |
| `PassCount` `FailCount` `SkipCount` `PendingCount` `UndefinedCount` `TotalCount` | Its own counts |
| `FeatureCount` | Features declared in its suite |
| `ElapsedMs` | What this execution took |
| `ExitCode` | The code its counts deserve |

---

[← Command Line](cli.md) | [Back to Guide →](../GUIDE.md)
