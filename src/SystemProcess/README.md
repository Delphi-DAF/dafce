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
  .KillAfterTimeout(True)
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

`Timeout` sets a deadline in milliseconds, and there is none unless you call it. `Timeout(0)` is a
deadline of zero: it kills the process on the first poll, 250 ms in.

`OnStdOut` and `OnStdErr` get whatever the pipe read returned — up to 2 KB at a time, **not one
line**. A line can arrive split across two calls, or several lines in one; split them yourself if
you need lines.

## TProcessResult

| Field | Type | Description |
|-------|------|-------------|
| `Status` | `TStatus` | `Failed`, `Timeout`, `Running`, `Completed`, `Canceled` — see below |
| `ExitCode` | `DWORD` | The process exit code when `Completed`; `GetLastError` when `Failed`; 0 otherwise |
| `Duration` | `TTimeSpan` | Wall-clock execution time |
| `LastError` | `string` | Error description. Only `Failed` fills it in |
| `Succeeded` | `function: Boolean` | `Status = Completed` and `ExitCode = 0` |

`TStatus` is scoped, so its values are written `TProcessResult.TStatus.Completed`:

| Value | When |
|-------|------|
| `Running` | the process is alive — the value `OnIdle` gets on every poll |
| `Completed` | it exited on its own; `ExitCode` is the process's |
| `Failed` | it could not be launched, or the wait ended without it signalling |
| `Timeout` | the deadline expired, or `Kill` was called; the process is killed in both cases |
| `Canceled` | the cancellation token was signalled and the process is killed. `Failed` if the kill does not succeed |

There is no `Killed` value: a `Kill` and an expired deadline both land on `Timeout`.

## Cooperating with shutdown

`Daf.Threading` provides the hook; killing the process from it keeps a long-running child from
outliving the application.

```pascal
TShutdownHook.OnShutdownRequested := procedure
begin
  Proc.Kill;
end;
```

---

## Sample

- [`PingProcess`](samples/PingProcess/) — runs `ping` and streams its output as it arrives.

## Note

This unit was part of `Daf.Commons` until it moved to a package of its own. The unit name and its
types did not change: add a reference to `Daf.SystemProcess` and existing `uses` clauses keep
compiling. It left because `Daf.Commons` is the floor every other package stands on, and launching
processes is something an *application* does — not an extension of the runtime library.
