# SystemProcess

**🌍 Language: English | [Español](README.es.md)**

`TSystemProcess` — run external processes synchronously or asynchronously, with callbacks for
standard output, standard error, completion and failure. The .NET analogue is
`System.Diagnostics.Process`.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Running a process

```pascal
uses Daf.SystemProcess;

var Proc := TSystemProcess.Builder
  .Command('git')
  .CmdArgs(['log', '--oneline', '-10'])
  .WorkingDir('C:\MyRepo')
  .Timeout(30000)               // ms; without this call there is no deadline
  .HideWindow(True)
  .OnStdOut(procedure(Text: string) begin Write(Text); end)
  .OnStdErr(procedure(Text: string) begin Write(Text); end)
  .OnCompleted(procedure(R: TProcessResult)
    begin
      WriteLn('Exit=', R.ExitCode, ' duration=', R.Duration.TotalMilliseconds, 'ms');
    end)
  .OnFailed(procedure(R: TProcessResult)
    begin
      WriteLn('Failed: ', R.LastError);
    end)
  .Build;

var Result := Proc.Execute;      // synchronous
var Future := Proc.ExecuteAsync; // or asynchronous
```

`Timeout` sets a deadline in milliseconds, and there is none unless you call it. **When it expires
the process is killed** and the result comes back as `Timeout`. `Timeout(0)` is a deadline of zero:
it kills the process on the first poll, 250 ms in; a child that has already exited by then comes
back `Completed`.

`System.Diagnostics.Process` writes the same in two steps — `WaitForExit(ms)` returns `false` and
`Kill()` is up to you —, and .NET 11 joins them in `SafeProcessHandle.WaitForExitOrKillOnTimeout`.
Here `Execute` owns the whole run and enforces the deadline itself. To stop the child on your own
terms, call `Kill` from another thread or from a shutdown hook, and the result says `Killed`, not
`Timeout`.

`OnStdOut` and `OnStdErr` get whatever the pipe read returned — up to 2 KB at a time, **not one
line**. A line can arrive split across two calls, or several lines in one; split them yourself if
you need lines.

## Once the child is launched

`ExecuteAsync` launches the child before it returns, on the calling thread, and the task it returns
only waits for the child to end. When it returns, `Id` answers and `IsRunning` is `True` while the
child runs. A launch that fails returns a task already settled with `Failed`, and `OnFailed` has
already run on the calling thread.

`Id` is the identifier the system gave the child, from the moment it is launched and also after it
ends. Read with no child launched — before running, or after a launch that failed — it raises
`EInvalidOpException`: `No process is associated with this object.`

```pascal
var Future := Proc.ExecuteAsync;
WriteLn('Started PID ', Proc.Id);
```

`Process.Id` in .NET is an `int`. Here `Id` is a `Cardinal`, the type `TProcess.KillById` and
`TProcess.HasExited` take, so `TProcess.KillById(Proc.Id)` needs no conversion.

`Kill` terminates the child the instance launched, with exit code `-1` (`$FFFFFFFF`), as .NET does,
and that run comes back `Killed`. On a child that has already exited it does nothing. With no child
launched — before running, or after a launch that failed — it raises `EInvalidOpException` with the
same message as `Id`.

The instance keeps its child's process until the next launch or until it is freed, as `Process` does
until `Dispose`: in the meantime `Id` cannot pass to another process, and a `Kill` after the end
finds the same child, already exited. `TProcess.KillById` also kills with `-1`, and returns `False`
for a process that has already exited.

## Where each stream goes

Standard output and standard error each go to one destination, a `TProcessOutput`:

| Destination | The child writes to |
|---|---|
| `TProcessOutput.Inherit` | the standard handle the parent holds when the child starts |
| `TProcessOutput.Discard` | the null device |
| `TProcessOutput.Capture(Proc)` | a pipe read for as long as the child runs; `Proc` gets what each read returns |
| `TProcessOutput.ToFile(Path, Append)` | a file, with `Path` resolved against the parent's current directory; emptied first unless `Append` is `True` |

`StandardOutput` and `StandardError` declare them, and `OnStdOut(Proc)` is short for
`StandardOutput(TProcessOutput.Capture(Proc))`:

```pascal
var Proc := TSystemProcess.Builder
  .Command('git')
  .CmdArgs(['fetch'])
  .OnStdOut(procedure(Text: string) begin Write(Text); end)
  .StandardError(TProcessOutput.Discard)
  .Build;
```

- **A stream with no declaration inherits**: the child writes where the calling process writes.
  Standard input is always the parent's.
- **Only a captured stream goes through a pipe**, and that pipe is read while the child runs: no
  amount of output keeps the child from finishing.
- **A file is opened before the child starts.** One that cannot be opened ends the run as `Failed`,
  with the path and the reason in `LastError`, and the child never starts.
- **A stream takes one declaration.** A second one — `OnStdOut` after `StandardOutput`, or the same
  call twice — raises `EInvalidOpException`:
  `StandardOutput already has a destination: a stream takes exactly one`.

`System.Diagnostics.Process` in .NET 11 takes a handle per stream, in `StandardOutputHandle` and
`StandardErrorHandle`. Here the value is a destination, `TProcessOutput`, and the methods are
`StandardOutput` and `StandardError`.

### Captured text

It is decoded with the code page of the console the child runs in: the output code page of the
parent's console when the child shares it, and the system's OEM code page when it has one of its own —
with `HideWindow(True)`, or when the parent has no console. A character whose bytes arrive in two reads
is delivered whole, and a byte that does not fit the code page arrives replaced.

A child that does not write with its console's code page needs its encoding declared, stream by
stream, with `StandardOutputEncoding` and `StandardErrorEncoding`. A Delphi program writes in ANSI
unless it changes the code page of its `Output`:

```pascal
  .OnStdOut(procedure(Text: string) begin Write(Text); end)
  .StandardOutputEncoding(TEncoding.ANSI)
```

The process does not free the encoding: `TEncoding.ANSI`, `TEncoding.UTF8` and the other standard ones
belong to the RTL, and one made with `TEncoding.GetEncoding` is freed by whoever made it, after
`Execute`.

`System.Diagnostics.Process` decodes with the output code page of the parent's console in every case.

### When the wait for captured text ends

A captured stream is read until it closes, and a process that inherits it from the child — a
grandchild left running in the background — keeps it open. With no deadline and no token, `Execute`
returns when the child has exited and every captured stream has closed, as `WaitForExit()` does in
.NET: such a descendant holds the run until it closes the stream, and what it writes reaches the
handler.

A deadline or a token bound that wait too, as the `timeout` of `RunAndCaptureText` does in .NET 11.
When either is met with a captured stream still open, the child is killed if it is alive, the
handlers get what is already in the pipe, and the result is `Timeout` or `Canceled`.

The end notifications — `OnCompleted`, `OnKilled`, `OnCancelled` — run once that wait is over, so the
captured output is complete when they arrive.

### A capture handler that raises

An exception raised by `OnStdOut` or `OnStdErr` aborts the run at once: the handler receives nothing
more, both captures stop and their pipes close — so the child is not left blocked writing to them —,
no end notification runs, and `Execute` raises that same exception, with its class and message; with
`ExecuteAsync`, inside the task's `EAggregateException`. The child is not killed, and the instance
keeps it: ask `IsRunning`, or kill it in your `except`:

```pascal
try
  Proc.Execute;
except
  if Proc.IsRunning then
    Proc.Kill;
  raise;
end;
```

In .NET the exception of an `OutputDataReceived` handler terminates the whole process; here the
application stays alive and the exception comes out of `Execute`.

An exception raised by `OnIdle` follows the same rule: the run aborts at once, the captures stop, no
end notification runs, `Execute` raises it, and the child stays alive in the instance. .NET has no
equivalent, since it runs no user code while it waits.

Two differences from .NET: `RunAndCaptureText` throws `TimeoutException` or
`OperationCanceledException` and loses the text it read, whereas here an expired deadline is a
`Timeout` result and what the handlers received stays received; and nothing is delivered after
`Execute` returns, as .NET's events are after `WaitForExit(ms)`.

### The child's console

`HideWindow(False)`, the default, runs the child in the parent's console; `HideWindow(True)` runs it
with no window, in a console of its own. A parent with no console — a windowed application — gives the
child a console of its own either way, and the streams it inherits write to that console.

A child that shares the console receives its Ctrl+C, as the parent does, and ends with it unless it
handles it; one launched with `HideWindow(True)` runs in a console of its own and does not receive it.

## TProcessResult

| Field | Type | Description |
|-------|------|-------------|
| `Status` | `TStatus` | `Failed`, `Timeout`, `Running`, `Completed`, `Canceled`, `Killed` — see below |
| `ExitCode` | `DWORD` | The process exit code whenever the child has exited — `Completed`, `Killed`, `Timeout` or `Canceled` —, `$FFFFFFFF` (-1) if it was killed; `GetLastError` when `Failed`; 0 while `Running` |
| `Duration` | `TTimeSpan` | Wall-clock execution time |
| `LastError` | `string` | Error description. Only `Failed` fills it in |
| `Succeeded` | `function: Boolean` | `Status = Completed` and `ExitCode = 0` |

`TStatus` is scoped, so its values are written `TProcessResult.TStatus.Completed`:

| Value | When |
|-------|------|
| `Running` | the process is alive — the value `OnIdle` gets on every poll |
| `Completed` | it exited on its own; `ExitCode` is the process's |
| `Failed` | it could not be launched, or the wait ended without it signalling |
| `Timeout` | the deadline expired and the process was killed |
| `Killed` | `Kill` was called; the deadline had nothing to do with it |
| `Canceled` | the cancellation token was signalled and the process is killed. A child that had already exited comes back `Completed`; `Failed` if the kill does not succeed |

The three ways a run ends without the child exiting on its own — deadline, `Kill`, cancellation —
each have their own value, so a result says **why** and not only that it was cut short.

## Cooperating with shutdown

`Daf.Threading` provides the hook; killing the process from it keeps a long-running child from
outliving the application. The request can arrive before the child is launched or after it ends,
and `Kill` raises with no child launched, so ask `IsRunning` first:

```pascal
TShutdownHook.OnShutdownRequested := procedure
begin
  if Proc.IsRunning then
    Proc.Kill;
end;
```

A Ctrl+C reaches a child that shares the console as well as the application, so the child may
already be ending when the hook runs: `IsRunning` covers that too, and a `Kill` on a child that has
just exited does nothing.

---

## Sample

- [`PingProcess`](samples/PingProcess/) — runs `ping` in the background, shows the child's `Id` and
  streams its output as it arrives.

## Note

This unit was part of `Daf.Commons` until it moved to a package of its own. The unit name and its
types did not change: add a reference to `Daf.SystemProcess` and existing `uses` clauses keep
compiling.
