# Commons — Usage Guide

**🌍 Language: English | [Español](GUIDE.es.md)**

---

## Table of Contents

1. [Daf.MemUtils — Smart pointers](#dafmemutils--smart-pointers)
2. [Daf.Threading — Cancellation and futures](#dafthreading--cancellation-and-futures)
3. [Daf.Enumerable](#dafenumerable)
4. [Daf.SystemProcess](#dafsystemprocess)
5. [Daf.CmdLn.Parser](#dafcmdlnparser)
6. [Daf.Rtti](#dafrtti)
7. [Daf.Activator](#dafactivator)
8. [Daf.Arrays](#dafarrays)

---

## Daf.MemUtils — Smart pointers

### ARC\<T\>

`ARC<T>` is a smart pointer implemented as `reference to function: T`. Invoke it as a function to obtain the object; when all references go out of scope the object is freed automatically.

```pascal
uses Daf.MemUtils;

// Create from an existing instance
var Doc := ARC.From<TDocument>(TDocument.Create);

// Create and auto-instantiate (requires parameterless constructor)
var Doc2 := ARC.From<TDocument>;

// Access the object — invoke as a function
Doc().Load('report.pdf');
WriteLn(Doc().Title);

// Pass around — ref count increases; freed when last copy exits scope
var Copy := Doc;
```

**Key rules**

- `ARC<T>` is a `reference to function: T` (interface closure), not a record.
- Access the object by calling the variable as a function: `MyArc()`, not `.Value`.
- `ARC.From<T>(nil)` returns `nil`.

---

## Daf.Threading — Cancellation and futures

### ICancellationToken / ICancellationTokenSource

```pascal
uses Daf.Threading;

var Cts   := TCancellationTokenSource.Create;
var Token := Cts.Token;          // ICancellationToken

// In worker:
while not Token.IsCancellationRequested do
  ProcessNextItem;

// Caller:
Cts.Cancel;
```

### IFuture\<T\>

Run work on a background thread and retrieve the result later.

```pascal
var Future := TFuture<string>.Run(
  function: string
  begin
    Result := FetchFromAPI;
  end);

// … do other work …

var Data := Future.Value;   // blocks until complete
```

### TShutdownHook

Connect OS shutdown signals (Ctrl+C, SIGTERM) to application cleanup:

```pascal
uses Daf.Threading;

TShutdownHook.OnShutdownRequested := procedure
begin
  Logger.LogInformation('Shutdown requested, stopping…', []);
  App.Stop;
end;
```

---

## Daf.Enumerable

### IEnumerable\<T\>

Lazy forward-only sequence over any data source.

```pascal
uses Daf.Enumerable;

var Seq := TListEnumerable<integer>.Create(MyList);
for var Item in Seq do
  Process(Item);
```

### IInterfaceList\<T\>

Type-safe list of interface references.

```pascal
var List: IInterfaceList<IMyService>;
List := TInterfaceList<IMyService>.Create;
List.Add(ServiceA);
List.Add(ServiceB);

for var Svc in List do
  Svc.Execute;
```

### TOrderedDictionary

Dictionary that preserves insertion order when iterating.

---

## Daf.SystemProcess

Run external processes asynchronously with event callbacks.

```pascal
uses Daf.SystemProcess;

var Proc := TSystemProcess.Builder
  .Command('git')
  .CmdArgs(['log', '--oneline', '-10'])
  .WorkingDir('C:\MyRepo')
  .Timeout(30000)               // ms, INFINITE = 0
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

// Synchronous
var Result := Proc.Execute;

// Or asynchronous
var Future := Proc.ExecuteAsync;
```

### TProcessResult

| Field | Type | Description |
|-------|------|-------------|
| `Status` | `TStatus` | `Completed`, `Failed`, `Timeout`, `Canceled`, `Killed` |
| `ExitCode` | `DWORD` | Process exit code |
| `Duration` | `TTimeSpan` | Wall-clock execution time |
| `LastError` | `string` | Error description (failed/killed cases) |
| `Succeeded` | `function: Boolean` | `Status = Completed` and `ExitCode = 0` |

### Integration with TShutdownHook

```pascal
TShutdownHook.OnShutdownRequested := procedure
begin
  Proc.Kill;
end;
```

---

## Daf.CmdLn.Parser

Parse command-line arguments into named switches and positional values.

```pascal
uses Daf.CmdLn.Parser;

var Parser := TCmdLnParser.Create;
Parser.Parse(ParamStr(0));

var Verbose := Parser.Flag('v');           // --v or -v
var Port    := Parser.Value('port', '80'); // --port 8080 (default '80')
var Files   := Parser.Positionals;         // remaining args
```

---

## Daf.Rtti

Helpers for Delphi's Extended RTTI.

```pascal
uses Daf.Rtti;

// Check inheritance at runtime
if _T.Extends(SomeClass, TMyBaseClass) then
  WriteLn('Is derived');
```

---

## Daf.Activator

Create class instances by class reference or type info via RTTI.

```pascal
uses Daf.Activator;

var Instance := TActivator.CreateInstance(TMyServiceClass) as IMyService;
```

---

## Daf.Arrays

`TArray` extension helpers.

```pascal
uses Daf.Arrays;

var Idx := TArrayHelper.IndexOf<string>(MyArray, 'needle');
if Idx >= 0 then
  WriteLn('Found at ', Idx);
```
