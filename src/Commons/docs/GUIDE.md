# Commons — Usage Guide

**🌍 Language: English | [Español](GUIDE.es.md)**

---

## Table of Contents

1. [Daf.MemUtils — Smart pointers](#dafmemutils--smart-pointers)
2. [Daf.Threading — Cancellation tokens and shutdown](#dafthreading--cancellation-tokens-and-shutdown)
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

## Daf.Threading — Cancellation tokens and shutdown

### ICancellationToken / ICancellationTokenSource

Use the factory functions to create a cancellation source — `TCancellationTokenSource` is an internal implementation class and is not instantiated directly.

```pascal
uses Daf.Threading;

// Create via factory functions
var Cts := CreateCancellationTokenSource;           // standard
// var Cts := CreateCancellationTokenSourceWithTimeout(5000); // auto-cancel after 5 s
// var Cts := CreateCanceledCancellationTokenSource;          // already canceled

var Token := Cts.Token;  // ICancellationToken

// In worker:
while not Token.IsCancellationRequested do
  ProcessNextItem;

// Notify when canceled:
Token.Register(procedure begin WriteLn('Canceled!'); end);

// Cancel from caller:
Cts.Cancel;
```

> **Note:** `IFuture<T>` is part of the Delphi RTL (`System.Threading`), not DAF.
> Use `TTask.Future<T>` (RTL) for background computations, or `TSystemProcess.ExecuteAsync`
> (DAF) for external process execution.

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

### ToIEnumerable\<T\>

Bridge record that adapts any `TEnumerable<T>` (class) to the `IEnumerable<T>` interface. Implicit conversion operators make usage transparent.

```pascal
uses Daf.Enumerable;

var List := TList<Integer>.Create;
List.Add(1); List.Add(2); List.Add(3);

// Implicit conversion to IEnumerable<T>
var Seq: IEnumerable<Integer> := ToIEnumerable<Integer>(List);
for var Item in Seq do
  Process(Item);

// Convert back to a plain array
var Arr := ToIEnumerable<Integer>(List).ToArray;
```

### IInterfaceList\<T\>

Type-safe list of interface references, with `GetEnumerator` support.

```pascal
var List := TInterfaceList<IMyService>.Create;
List.Add(ServiceA);
List.Add(ServiceB);

if not List.IsEmpty then
  for var Svc in List do
    Svc.Execute;

// Search by predicate
var Found: IMyService;
if List.TryFind(
    function(I: IInterface): Boolean
    begin Result := (I as IMyService).Name = 'target'; end,
    Found) then
  Found.Execute;
```

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

Builds a typed argument grammar with `TCmdLnParserBuilder`, then parses the command line into `ICmdLParams`.

```pascal
uses Daf.CmdLn.Parser;

// 1. Define the grammar
var Parser := TCmdLnParserBuilder.Create
  .Arg<string>('file')      // named string argument
  .Arg<Integer>('port')     // named integer argument
  .Arg<Boolean>('verbose')  // boolean flag
  .Build;                   // returns ICmdLnParser

// 2. Parse the actual command line
var Params := Parser.Parse(GetCommandLine);

// 3. Read values (ICmdLParams.Item[] is the default property)
var FilePath  := Params['file'].AsString;
var Port      := Params['port'].AsInteger;
var IsVerbose := Params['verbose'].AsBoolean;
```

### Subcommands

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
var Cmd := Params.Command;   // 'build' or 'run'
if Cmd = 'build' then
begin
  var Section := Params.GetSection('build');
  var OutDir  := Section['output'].AsString;
end;
```

---

## Daf.Rtti

Helpers for Delphi's Extended RTTI, centralised in the `_T` utility class.

```pascal
uses Daf.Rtti;

// Type names and GUIDs
var Name := _T.NameOf<IMyService>;            // 'IMyService'
var Guid := _T.GUID<IMyService>;              // TGUID
var Unit := _T.UnitNameOf<IMyService>;        // 'MyUnit'

// Check interface hierarchy (PTypeInfo overloads)
if _T.Extends(TypeInfo(IChildService), TypeInfo(IMyService)) then
  WriteLn('IChildService extends IMyService');

// Check whether a class has a property
if _T.HasProperty<TMyClass>('Name') then
  WriteLn('Name property exists');

// Create default (zero) value for any type
var Zero := _T.Default<Integer>;             // 0
```

### TRttiTypeHelper / TRttiPackageHelper

```pascal
var RC := TRttiContext.Create;
var T  := RC.GetType(TypeInfo(TMyClass));

if T.IsCollection then WriteLn('Is a collection');
if T.Implements<IMyService> then WriteLn('Implements IMyService');

// Discover all implementations of an interface in a package
RC.FindType('MyUnit.TMyClass').AsInstance.DeclaringUnitPackage
  .DiscoverImpl<IMyService>(False, nil,
    procedure(Intf: TRttiInterfaceType; Impl: TRttiInstanceType)
    begin WriteLn(Impl.Name); end);
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

### TArrayHelper

Class helper for the stock `TArray` — adds `Map` (projection) and `Trim` (strip whitespace / remove empty strings).

```pascal
uses Daf.Arrays;

// Map: project [string] -> [Integer]
var Lengths := TArray.Map<string, Integer>(Names,
  function(S: string): Integer begin Result := S.Length; end);

// Trim: strip whitespace and remove empty elements from a string array
var Clean := TArray.Trim(['  hello  ', '', '  world  ']);
// Clean = ['hello', 'world']
```

### TSmartArray\<T\>

Value-type wrapper around `TArray<T>` with a rich LINQ-style API. Supports operators `in`, `+`, `=`, implicit conversion to/from `TArray<T>`, and CSV-string implicit conversion.

```pascal
var Items: TSmartArray<string>;
Items.Concat('alpha');
Items.Concat('beta');
Items.Concat('gamma');

// Membership
if 'beta' in Items then WriteLn('Found');

// Filter
var Long := Items.Where(
  function(S: string): Boolean begin Result := Length(S) > 4; end);

// Project
var Upper := Items.Select<string>(
  function(S: string): string begin Result := S.ToUpper; end);

// Concatenation via operator +
var More: TSmartArray<string> := Items + ['delta', 'epsilon'];

// Sort
Items.Sort;
```
