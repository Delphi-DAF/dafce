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
9. [Daf.Application.Version](#dafapplicationversion)
10. [Daf.Types](#daftypes)

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

The RTL's `TInterfaceList` with a typed `GetEnumerator` on top, which is all it adds: the loop
variable is a `T`, not an `IInterface`.

```pascal
var List := TInterfaceList<IMyService>.Create;
List.Add(ServiceA);
List.Add(ServiceB);

for var Svc in List do          // Svc is IMyService
  Svc.Execute;
```

`IsEmpty`, `TryFind`, `First`, `Last` and `ItemType` are on the **non-generic** `TInterfaceList`
this unit also declares, not on the generic one:

```pascal
var Any := TInterfaceList.Create;
Any.Add(ServiceA);

var Found: IMyService;
if not Any.IsEmpty and Any.TryFind(
    function(I: IInterface): Boolean
    begin Result := Supports(I, IMyService); end,
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
var UnitName := _T.UnitNameOf<IMyService>;    // 'MyUnit'

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

---

## Daf.Application.Version

### TVersionInfo / [Version]

A class declares its version with the `[Version]` attribute — SemVer tag, application name and
company — and `TVersionInfo.GetFrom` reads it back by RTTI:

```pascal
uses
  Daf.Application.Version;

type
  [Version('1.2.3-rc.2+build.7', 'Invoicer', 'Acme')]
  TInvoicerApp = class
  end;

var Info := TVersionInfo.GetFrom(TInvoicerApp);
// Info.Major = 1, Info.Minor = 2, Info.Patch = 3
// Info.PreRelease = 'rc.2', Info.Metadata = 'build.7'
// Info.ApplicationName = 'Invoicer', Info.Company = 'Acme'
```

| Member | What it answers |
|---|---|
| `Tag` | the string the attribute declared, verbatim |
| `VersionTag(Template)` | the version formatted with a template of `${...}` markers; `VersionTemplate` is the default, `VersionTemplateWithBuild` adds what the compiler saw |
| `IsEmpty` | whether a version was set at all |
| `IsSemVer` | whether the declared tag follows the SemVer grammar |
| `Major` / `Minor` / `Patch` / `PreRelease` / `Metadata` | the SemVer pieces |
| `ApplicationName` / `Company` | the other two arguments of the attribute |
| `ArchBits` / `Platform` / `Debug` | what the **compiler** saw: 32 or 64, the target platform, and whether `DEBUG` was defined |

**A class with no attribute gives an empty record**, not an error: `IsEmpty` answers `True`, `Tag`
and the two names come back as empty strings, the numeric pieces as zero, and `VersionTag` as the
empty string.

**A tag that is not SemVer keeps its text.** `IsSemVer` answers `False`, and `VersionTag` reports the
declared string rather than rebuilding it from pieces that were never filled.

The markers a template can name:

| Marker | Emits |
|---|---|
| `${Major}` `${Minor}` `${Patch}` `${Metadata}` `${Tag}` | the SemVer pieces and the declared string, raw |
| `${PreRelease}` | the pre-release with its own `-`, or nothing |
| `${Platform}` `${ArchBits}` `${CompiledMeta}` | what the compiler saw |
| `${Build}` / `${BuildFull}` | the build section, opened with `+`. The first carries what the tag declared; the second joins it with the compiled metadata inside that single section. They are alternatives: a template names one |

SemVer admits one build section, so `${BuildFull}` separates the two sources with `.` rather than
opening a second `+`.

The attribute is read by RTTI, so the linker has to keep the class: a program that relies on this
declares `STRONGLINKTYPES` and puts the class in its `interface` section.

---

## Daf.Types

### TDeferred\<T\>

A value given either directly or as a thunk, and **re-evaluated on every read**. The implicit
conversions are what make the two forms interchangeable at the call site:

```pascal
uses Daf.Types;

var Source := 'before';

var Fixed: TDeferred<string> := 'literal';
var Live: TDeferred<string> := TFunc<string>(
  function: string
  begin
    Result := Source;
  end);

Source := 'after';

var A: string := Fixed;   // 'literal'
var B: string := Live;    // 'after' — read now, not when it was registered
```

**It is not `Lazy<T>`.** That one memoizes: it runs the thunk once and keeps the answer. This one
runs it again on each read, which is what macro resolution needs — a macro whose value tracks
something that moves.

**Nor is it .NET's `Expression<TDelegate>`.** An expression there is an inspectable tree; a
`TDeferred<T>` is a closure and nothing can look inside it. The inspectable tree is `IExpr` and
`TSpecification<T>` in [Daf.Specification](../../Specification/README.md).

`TMacroExpression` is `TDeferred<string>` under the name the macro list uses.

### TMacrosList

A named set of `TMacroExpression`, and the substitution of `${name}` inside a string.

```pascal
var Macros := TMacrosList.Create;
try
  Macros['product'] := 'DAF';
  Macros['edition'] := TFunc<string>(
    function: string begin Result := CurrentEdition; end);

  Macros.ResolveMacros('${product} ${edition}');   // 'DAF Community'
finally
  Macros.Free;
end;
```

| Member | Answers |
|--------|---------|
| `Value[Name]` (the default property) | Reads and writes a macro. Reading resolves the thunk |
| `ResolveMacros(Source, MaxRecursionLevel)` | Substitutes every `${name}` in `Source`. A macro whose value names another macro is resolved too, up to `MaxRecursionLevel` — 100 by default, which is what stops a pair that names each other |
| `Contains(Key)` `Count` | What is registered |
| `Keys[I]` `ValueFromIndex[I]` | Registration order is kept: the list is an ordered dictionary |

#### A macro that is not registered

Out of the box, a reference to a macro that is not registered resolves to the empty string.
`MissingMacroPolicy` changes that, and `OnMissingMacro` is asked first:

```pascal
Macros.MissingMacroPolicy := mmpError;
Macros.ResolveMacros('${root}/charts');
// EMacroNotFound: No macro is registered with the name "root"
```

| Member | Answers |
|--------|---------|
| `MissingMacroPolicy` | `mmpReturnDefault`, the default, substitutes `MissingMacroValue`, which is empty unless set · `mmpKeepToken` leaves `${name}` in the result · `mmpError` raises `EMacroNotFound` |
| `OnMissingMacro` | Called for each reference to a macro that is not registered, before the policy. It sets `Value` and `Handled` to resolve it, or leaves `Handled` False to hand it to the policy |
| `MissingMacroError(Name)` | The exception `mmpError` raises. A hook that has to fail raises this one: `raise TMacrosList(Sender).MissingMacroError(Name)` |

The policy applies to `ResolveMacros` only: reading `Macros['name']` of a macro that is not
registered answers empty whatever the policy. `Contains` asks whether it is there.

### TEnvVars

The process's environment variables read into a dictionary, and written back out as the
environment block a child process takes.

```pascal
var Env := TEnvVars.Create;
try
  for var Pair in Env.Vars do
    Writeln(Pair.Key, '=', Pair.Value);

  Env.Refresh;             // read the system again, in case it moved
finally
  Env.Free;
end;
```

| Member | Answers |
|--------|---------|
| `Vars` | The dictionary collected at construction. `Refresh` reads the system again |
| `Buffer` | The same set as the null-separated block `CreateProcess` expects |
| `WriteTo(Buffer)` | Writes that block into a caller's pointer — how a child process is given an environment with entries added to the parent's |
| `ClearBuffer` | Releases the block |

`Daf.Configuration` reads `Vars` to turn the environment into a configuration source, and
`Daf.SystemProcess` uses `WriteTo` to hand a child its own environment.

### Coalesce

The first of several interface references that is not `nil`.

```pascal
var Chosen := Coalesce.From<ILogger>(Explicit, FromContainer);
var First  := Coalesce.From<ILogger>([Explicit, FromContainer, Fallback]);
```

Both arguments are evaluated before the call, as in any function: this picks between references
already held, and it is not a short-circuit.

### Debugger

```pascal
Debugger.Write('cache primed with %d entries', [Count]);
if Debugger.Enabled then
  Debugger.Write(Report);
```

| Member | Answers |
|--------|---------|
| `Write(Text)` / `Write(FmtStr, Args)` | Writes to the system debug output. On Windows that is `OutputDebugString`; **outside Windows it writes nothing** — for a destination that works there, see `TPlatformLogBootstrapSink` in [Daf.FMX](../../FMX/README.md) |
| `Enabled` | Whether a debugger is **attached** (`System.DebugHook`). Not whether the build is a Debug one |
| `CurrentProcessId` `CurrentThreadId` | The ids, for a report that has to say which process or thread wrote it |

