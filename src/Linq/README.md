# Daf.Linq — Lazy Sequences for Delphi

**🌍 Language: English | [Español](README.es.md)**

A standalone, ref-counted, lazy collection-transformation library — LINQ-to-Objects / Kotlin sequences / Rust iterators in spirit. It knows nothing about SQL and is useful on its own.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Concepts

| Concept | Description |
|---------|-------------|
| `TQuery<T>` | The fluent **operator surface**: a value type wrapping a deferred `System.IEnumerable<T>`, converting implicitly to and from it |
| `IGrouping<TKey, T>` | A group produced by `GroupBy`: a `Key` plus its `Members` |
| `Linq.From<T>` | Entry point — builds a `TQuery<T>` over an array, a `TEnumerable<T>`, an `IEnumerable<T>` or a `TSmartArray<T>` |
| `Linq.FromOwned<T>` | Entry point over a temporary list that Linq frees with the query — see [Ownership](#ownership) |

Delphi interfaces cannot declare generic methods, which is why the generic operators (`Select`, `Map`, `Zip`, `GroupBy`) live on the `TQuery<T>` record rather than on the interface. Pass `IEnumerable<T>` around; chain on `TQuery<T>`.

---

## Quick Start

```pascal
uses Daf.Linq;

var Adults := Linq.From<TPerson>(People)      // People: TList<TPerson>
  .Where(function(P: TPerson): Boolean begin Result := P.Age >= 18 end)
  .OrderBy(TComparer<TPerson>.Construct(
    function(const A, B: TPerson): Integer begin Result := CompareStr(A.Name, B.Name) end))
  .Take(10);

for var P in Adults do                        // nothing ran until this line
  Writeln(P.Name);
```

`Adults` is an `IEnumerable<TPerson>` — deferred, ref-counted, no `Free`.

---

## Operators

| Operator | Kind | Behavior |
|----------|------|----------|
| `Where` / `Filter` | lazy | Keeps elements matching a `TFilterSpec<T>` (a bare closure converts implicitly) |
| `Select<TResult>` / `Map<TResult>` | lazy | Projects each element through a `TFunc<T, TResult>` |
| `Take(N)` | lazy | Yields at most `N`, then **stops consuming the source** |
| `Skip(N)` | lazy | Drops the first `N` |
| `Zip<TOther, TResult>` | lazy | Pairs with another sequence element-wise until either ends |
| `Distinct(Comparer)` | buffered | De-duplicates by value; pass a NULL-aware comparer for SQL semantics |
| `OrderBy(Comparer)` | buffered | Sorts by an `IComparer<T>`, and is stable, as in .NET: elements the comparer considers equal keep their source order. Descending is an inverted comparer: there is no `OrderByDescending` |
| `GroupBy<TKey>(KeySelector)` | buffered | Yields `IEnumerable<IGrouping<TKey, T>>`, key order preserved |
| `IsEmpty` | terminal | True when nothing is produced; stops at the first element |
| `Any(Spec)` | terminal | True when some element matches; stops at the first match |
| `FirstOrDefault` / `LastOrDefault` | terminal | The first / last element, or `Default(T)` (or a supplied fallback) |
| `TryFirst(out Value)` | terminal | The first element **plus** a found/not-found result |
| `Single` / `SingleOrDefault` / `TrySingle` | terminal | The one and only element — see below |
| `ToArray` / `ToList` / `ToList(List)` / `ToSmartArray` | terminal | Materializes the sequence |
| `MoveToList(List)` / `MoveToList` / `MoveToArray` | terminal | Moves the elements: takes them out of the source list as it delivers them — see [Ownership](#ownership) |

**Deferred by default.** `Where`/`Select`/`Take`/`Skip`/`Zip` run element-by-element while iterating. `OrderBy`/`Distinct`/`GroupBy` need every element, so they materialize once when enumeration starts — but still nothing happens until then.

**Terminals are non-destructive.** They enumerate without consuming the query, so the same value stays usable and a terminal can be called twice:

```pascal
var Q := Linq.From<TPerson>(People).Where(IsAdult);
if not Q.IsEmpty then
  for var P in Q do ...          // still yields everything
```

The flip side is that each terminal call **re-enumerates the source**. With an expensive or single-shot source, materialize once (`ToArray` / `ToList`) and query that.

The exceptions are in [Ownership](#ownership): over a source Linq owns, an element a terminal hands to the application is not yielded again, and the `MoveTo…` terminals take what they deliver out of the source list.

### `First…` or `Single…`?

`Single` asserts a shape: it raises `ELinqError` when the sequence is empty **or holds more than one element**. `SingleOrDefault` tolerates empty but still raises on more than one. Use them when "more than one" means your data is wrong and you want to hear about it:

```pascal
Users.Where(EmailIs(X)).SingleOrDefault;   // a duplicate email is a bug → raise
Entries.OrderBy(NewestFirst).FirstOrDefault;  // many entries expected → take the newest
```

`TryFirst` / `TrySingle` are the non-raising forms. `TryFirst` is also the only way to tell an empty sequence from one whose first element happens to equal `Default(T)` — something `FirstOrDefault` cannot express for value types.

---

## Ownership

Linq handles references, never objects: which list frees an object is decided by five rules.

1. **`Linq.From(X)`** — X belongs to the application. Linq never frees it, nor any of its objects.
2. **`Linq.FromOwned(X)`** — X is a temporary list that nobody else keeps. Linq frees it when the last query built on it is released.
3. **The operators** (`Where`, `Select`, `Take`…) create nothing: they keep referencing the source.
4. **What a terminal returns belongs to the application.** The container it creates, always. The objects, with `From`, stay with whoever owned them; with `FromOwned`, they pass to the application. An object a lambda creates belongs to the application in both cases.
5. **`MoveToList` / `MoveToArray`** take what they deliver out of the source list, and free nothing. They are the only terminals that change the source.

### A source Linq owns

`FromOwned` takes a `TObjectList<T>` — or a list derived from it — of a class `T`. It is the entry for a list that a call returns and nobody keeps in a variable:

```pascal
Orders := TObjectList<TOrder>.Create;          // owns its objects
Linq.FromOwned<TOrder>(LoadOrders(Customer))    // a temporary list
  .Where(IsPending)
  .ToList(Orders);
// the pending orders now belong to Orders; the temporary list and the
// other orders are freed with the query
```

What each terminal does over an owned source:

| Terminal | Over an owned source |
|----------|----------------------|
| `ToList(List)` / `ToList` / `ToArray` / `ToSmartArray` | Every element put in the container passes to the application |
| `FirstOrDefault` / `LastOrDefault` / `TryFirst` / `Single` / `SingleOrDefault` / `TrySingle` | The element returned passes to the application; a fallback value passed in is not the source's and is not touched |
| `IsEmpty` / `Any` | No element changes owner |
| `for..in` | Lends the elements while the query lives |

- An element that passed to the application is not yielded again by any later terminal over the same source.
- When the list does not own its objects (`OwnsObjects = False`), Linq frees only the list, and no element changes owner.
- When adding an element to the target list raises, the element stays where it ended up — in the target if the target kept it, in the source otherwise — and the exception reaches the caller.
- To look at an object without taking it, project before the terminal: `Linq.FromOwned<TOrder>(LoadOrders(Customer)).Select<string>(OrderNumber).FirstOrDefault` returns a string, and every order stays with the source.
- The query's temporaries live until the end of the routine that builds it, or inside a loop until the next iteration, and the source lives with them. To release it earlier, keep the query in a variable and reset it: `Q := Default(TQuery<TOrder>)`.

Limits:

- Converting the query to `IEnumerable<T>` — implicitly or with `AsEnumerable` — and `GroupBy` lend the elements: what is read through them is valid while the query lives, and no terminal reached that way hands anything over.
- A projection that returns an object owned by an element — a `Select` returning `Order.Customer` — does not change its owner: it lives as long as the element.
- A list the application still uses is freed with the query: `FromOwned` is only for a list nobody else keeps.

### Moving elements to another list

When the application keeps the source and wants part of it in another list, `MoveToList(List)` takes each selected element out of the source list — with `Extract`, which does not free it — and adds it to `List`. `MoveToList` without arguments does the same into a new list, and `MoveToArray` into a new array.

```pascal
Linq.From<TOrder>(Orders)          // Orders stays with the application
  .Where(IsArchived)
  .MoveToList(Archived);
// the archived orders left Orders and are in Archived: one owner each
```

- Moving needs a query that reaches a list: `Linq.From` or `Linq.FromOwned` over a `TList<T>`, through operators that keep the element type (`Where`, `Take`, `Skip`, `Distinct`, `OrderBy`). Over an array or an `IEnumerable<T>`, or after `Select` or `Zip`, the three raise `ELinqError` (`MoveToList: the query does not reach a source list…`) and the source is left unchanged.
- The selection is collected before anything moves, so the order of the chain is the order in the target.
- When adding an element to the target raises, the element stays where it ended up, as with an owned source.
- Over a source Linq owns, what is moved leaves the list and is not freed with it.

---

## One vocabulary, two execution strategies

Commons ships an eager collection (`TSmartArray<T>`) that predates this module. Both speak the **same** filter and mapper types, so a value written once works with either:

| Concern | Shared type | Eager (`TSmartArray<T>`) | Lazy (`TQuery<T>`) |
|---------|-------------|--------------------------|--------------------|
| filter | `TFilterSpec<T>` | `Where`, `Any` | `Where` / `Filter`, `Any` |
| projection | `TFunc<T, U>` | `Select<U>` | `Select<U>` / `Map<U>` |
| eager → lazy | — | `Linq.From<T>(SmartArray)` | |
| lazy → eager | — | | `ToSmartArray` |

```pascal
var Adults: TFilterSpec<TPerson> := ...;
var Names:  TFunc<TPerson, string> := ...;

Eager.Where(Adults).Select<string>(Names);                  // eager, Commons
Linq.From<TPerson>(People).Where(Adults).Select<string>(Names);  // lazy, here
```

Deliberately different on each side:

- **`Sort` vs `OrderBy`** — the eager `Sort` mutates in place; `OrderBy` returns a new deferred sequence. Different semantics, different names.
- **Lazy only** — `Take`, `Skip`, `Zip`, `GroupBy`, `Distinct` and the terminals above.
- **Eager only** — `Concat`, `BinarySearch`, `FreeItems`, `First`/`Last` properties: container concerns, not sequence ones.

**Why both exist.** Commons' own `Daf.Rtti` chains `Where(...).Select(...)` over a `TSmartArray` and sits *below* this module in the dependency graph, so it cannot use the lazy API without inverting that graph. The duplication of *implementation* is therefore structural, not accidental — but the vocabulary is not duplicated.

---

## Common Patterns

### Group and summarize

```pascal
for var G in Linq.From<TPerson>(People).GroupBy<string>(
      function(P: TPerson): string begin Result := P.Country end) do
  Writeln(Format('%s: %d', [G.Key, Length(G.Members)]));
```

### Bridge to and from the eager `TSmartArray<T>`

Commons already ships an eager, array-backed collection. `Daf.Linq` reuses its vocabulary rather than inventing a second one, so eager and lazy are two execution strategies over one API:

```pascal
uses Daf.Arrays, Daf.Linq;

var Lazy  := Linq.From<TPerson>(Eager);       // TSmartArray<T> → lazy
var Eager2 := Lazy.Where(IsAdult).ToSmartArray;  // lazy → TSmartArray<T>
```

### Short-circuiting a large source

```pascal
Linq.From<Integer>(Million).Where(IsPrime).Take(5).ToArray;
// stops scanning as soon as the fifth prime is produced
```

---

## Notes

- `Reset` on the underlying enumerators is **not supported** — sequences are forward-only.
- Value-type `T` (records, `TNullable<T>`) is supported: the `Daf.Enumerable` adapters resolve the non-generic `GetCurrent: TObject` baggage through a method-resolution clause.
- For NULL-aware ordering and de-duplication, use the comparers in Commons' `Daf.Comparers` (`TValueComparers`, `TNullableComparers`).

---

## Documentation

| Resource | Description |
|----------|-------------|
| [**SQLCute In-Memory**](../SQLCute.InMemory/README.md) | Runs a SQLCute `IQuery` over in-memory collections on top of this library |
| [In-Memory Usage Guide](../SQLCute.InMemory/docs/GUIDE.md) | Two lanes, NULL semantics, DISTINCT scope |
| [SQLCute](../SQLCute/README.md) | The fluent SQL query builder |
