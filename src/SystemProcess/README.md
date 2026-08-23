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
  .Timeout(30000)               // ms, 0 = INFINITE
  .KillAfterTimeout(True)
  .HideWindow(True)
  .OnStdOut(procedure(Line: string) begin WriteLn(Line); end)
  .OnStdErr(procedure(Line: string) begin WriteLn('ERR: ', Line); end)
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

## TProcessResult

| Field | Type | Description |
|-------|------|-------------|
| `Status` | `TStatus` | `Completed`, `Failed`, `Timeout`, `Canceled`, `Killed` |
| `ExitCode` | `DWORD` | Process exit code |
| `Duration` | `TTimeSpan` | Wall-clock execution time |
| `LastError` | `string` | Error description (failed / killed cases) |
| `Succeeded` | `function: Boolean` | `Status = Completed` and `ExitCode = 0` |

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
