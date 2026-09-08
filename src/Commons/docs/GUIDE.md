# Commons — Usage Guide

**🌍 Language: English | [Español](GUIDE.es.md)**

---

## Table of Contents

1. [Daf.MemUtils — Smart pointers](#dafmemutils--smart-pointers)
2. [Daf.Threading — Cancellation tokens and shutdown](#dafthreading--cancellation-tokens-and-shutdown)
3. [Daf.Enumerable](#dafenumerable)
4. [Daf.Rtti](#dafrtti)
5. [Daf.Activator](#dafactivator)
6. [Daf.Arrays](#dafarrays)
7. [Daf.Nullable](#dafnullable)
8. [Daf.Comparers](#dafcomparers)

`Daf.SystemProcess` and `Daf.CmdLn.Parser` used to be documented here. The first lives in a package
of its own — see [SystemProcess](../../SystemProcess/README.md); the second has been withdrawn, and
the command line is [`Daf.CommandLine`](../../CommandLine/README.md).

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

### TDeferredEnumerable\<T\>

`IEnumerable<T>` built from a closure that returns the enumerator — the deferred sequence
primitive. Nothing runs until `GetEnumerator` is called, and every iteration invokes the factory
again, so the same sequence can be walked more than once.

```pascal
uses Daf.Enumerable;

var Calls := 0;
var Seq: IEnumerable<Integer> := TDeferredEnumerable<Integer>.Create(
  function: IEnumerator<Integer>
  var
    N: Integer;
  begin
    Inc(Calls);
    N := 0;
    Result := TGeneratorEnumerator<Integer>.Create(
      function(var Current: Integer): Boolean
      begin
        Result := N < 3;
        if Result then
        begin
          Inc(N);
          Current := N;
        end;
      end);
  end);

// Calls is still 0: declaring the sequence enumerated nothing

for var I in Seq do    // Calls = 1, yields 1, 2, 3
  Process(I);

for var I in Seq do    // Calls = 2 — a new enumerator, walked from the start
  Process(I);
```

This is the primitive `Daf.Linq` is built on: `Where`, `Select`, `Take` and the rest each return one
of these wrapping the previous sequence, which is what makes a chain of operators traverse the
source once, and only when a terminal asks for an element. See [Daf.Linq](../../Linq/README.md).

### TGeneratorEnumerator\<T\>

The enumerator of the pair above, built from an advance-and-yield closure: a `TGeneratorFunc<T>` is
a `function(var Current: T): Boolean` that returns `False` when the sequence is exhausted and
otherwise writes the next element into `Current`. The state of the walk lives in the closure, so a
sequence with no collection behind it — a counter, a file being read, a computed series — needs no
enumerator class of its own.

`Reset` raises `ENotImplemented`: a closure that has already advanced cannot be rewound. Walking a
sequence a second time is what `TDeferredEnumerable<T>` is for — it asks its factory for a **new**
enumerator, with its own state.

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
`Where` / `Any` take a `TFilterSpec<T>` and `Select` a `TFunc<T, TTarget>` — the very
types the lazy `Daf.Linq` operators take, so a filter or a
mapper written once works with both APIs. Inline closures convert implicitly, as
above; build the value separately when you want to reuse or compose it:

```pascal
uses Daf.FilterSpec;

var Long: TFilterSpec<string> := TPredicate<string>(
  function(S: string): Boolean begin Result := Length(S) > 4; end);
var Loud: TFilterSpec<string> := TPredicate<string>(
  function(S: string): Boolean begin Result := S = S.ToUpper; end);

Items.Where(Long and not Loud);          // composed with and / or / not
```

---

## Daf.Nullable

### TNullable\<T\>

An optional value: either an absent value — SQL NULL — or a present value of `T`. What it buys is
the distinction between **no value** and a present default: `0`, `''` and `False` are values, and a
`T` alone cannot say which of the two it is.

```pascal
uses Daf.Nullable;

var Age: TNullable<Integer> := TNullable<Integer>.Null;
Age.IsNull;                     // True
Age.GetValueOrDefault(-1);      // -1

Age := 42;                      // implicit conversion from T
Age.HasValue;                   // True
Age.Value;                      // 42

var Plain: Integer := Age;      // implicit conversion back to T
```

| Member | Answers |
|--------|---------|
| `Null` | An explicitly absent nullable |
| `From(Value)` | Wraps a present value; the implicit conversion from `T` does the same |
| `IsNull` | True when no value is present |
| `HasValue` | True when one is. `HasValue = not IsNull` |
| `GetValueOrDefault(Fallback)` | The value when present, otherwise the fallback given |
| `Value` | The present value — `Default(T)` when NULL |

**Both presence predicates ship on purpose.** `IsNull` is the data-domain idiom, the one that
matches Delphi's own `TField.IsNull` and SQLCute's `IS NULL`; `HasValue` is the .NET one. Whichever
reads better where it is written is the right one.

A newly declared `TNullable<T>` starts absent: `Initialize` sets it so, and that is a managed
record operator, so a field of a class or an element of an array starts absent too without anybody
initializing it.

## Daf.Comparers

NULL-aware comparison, in two coordinated policies: **ordering** puts NULL below every non-NULL
value — nulls-first ascending, nulls-last descending — and two NULLs compare equal; **equality**
makes NULLs equal, which is what collapses duplicate NULLs under `DISTINCT`.

```pascal
uses Daf.Comparers;

// Over TValue — the value currency of the in-memory query engine
var Order := TValueComparers.Ordinal;
var Same  := TValueComparers.Equality;

// Over TNullable<T>
var ByAge := TNullableComparers.Ordinal<Integer>;
```

| Entry point | Answers |
|-------------|---------|
| `TValueComparers.Ordinal` | `IComparer<TValue>`: NULL below every non-NULL value, NULLs equal |
| `TValueComparers.Equality` | `IEqualityComparer<TValue>`: equal by content, NULLs equal |
| `TNullableComparers.Ordinal<T>` | The same order over `TNullable<T>` |
| `TNullableComparers.Equality<T>` | Two absent values are equal |
| `IsNullValue(V)` | Whether a `TValue` represents SQL NULL: empty, or a `Null`/`Unassigned` Variant |
| `CompareValuesNullSmallest(L, R)` | The comparison itself, without going through a comparer |

Over `TValue`, SQL NULL is modelled as an **empty** `TValue`, or a Variant-kind one holding `Null`
or `Unassigned`. This is what `SQLCute.InMemory` builds its ANSI NULL semantics on — see
[SQLCute.InMemory](../../SQLCute.InMemory/README.md).

