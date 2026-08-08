# Daf.SQLCute.Expressions — Usage Guide

**🌍 Language: English | [Español](GUIDE.es.md)**

← [Back to README](../README.md)

---

## Table of Contents

1. [Emitting an expression](#emitting-an-expression)
2. [Emitting a specification](#emitting-a-specification)
3. [The name-resolution seam](#the-name-resolution-seam)
4. [How emission stays faithful](#how-emission-stays-faithful)
5. [The in-memory repository](#the-in-memory-repository)
6. [What fails, and when](#what-fails-and-when)

---

## Emitting an expression

One call, one target: WHERE clauses on the query you hand it, which is also what comes back.

```pascal
uses Daf.SQLCute, Daf.Query.Expressions, Daf.SQLCute.Expressions;

var Q := TExprEmitter.Where(TQuery.New.From('Product'),
  (Prop('Price') > 100) and (Prop('Active') = True));
```

The query is yours: add to it before or after, compile it, clone it, run it.

```pascal
Q.Select(['Name', 'Price']).OrderBy('Name');
```

No FROM is required. The in-memory lane takes its source as a parameter and never resolves a table, so a rule can be emitted and run over a list without inventing a table name:

```pascal
var Q := TExprEmitter.Where(TQuery.New, Prop('Active') = True);
for var P in TInMemory.Elements<TProduct>(Q, Loaded) do ...
```

Add a FROM when the query is headed for SQL, since that is who needs it.

---

## Emitting a specification

`Apply<T>` emits the whole rule — filter, ordering and page — onto one query, so a caller never has to remember to apply the ordering separately from the rule it belongs to.

```pascal
var Spec := TSpecification<TProduct>.New
  .Where((Prop('Price') > 100) and (Prop('Active') = True))
  .OrderByDesc('Price')
  .Skip(10)
  .Take(5);

var Q := TExprEmitter.Apply<TProduct>(TQuery.New.From('Product'), Spec);
// → WHERE … ORDER BY Price DESC LIMIT 5 OFFSET 10
```

`Skip(0)` and `Take(0)` are *unset*, so an unpaged rule adds no clause.

---

## The name-resolution seam

By default a member becomes a column verbatim, which is already the shipped convention. Supply an `INameResolver` to change that, and the emission asks it once per member, at emit time:

```pascal
TExprEmitter.Where(Q, MyExpr, MyNames, TypeInfo(TProduct));
TExprEmitter.Apply<TProduct>(Q, Spec, MyNames);
```

`ColumnFor` is exposed for the same purpose outside the walk — for instance to resolve an ordering column or a table name yourself:

```pascal
ColumnFor(MyNames, TypeInfo(TProduct), 'Price');   // 'price_eur'
```

Expression leaves carry property names, never column names. The column is decided here, once, and nowhere else — which is why a mapping change is a strategy change and not a sweep through your rules.

---

## How emission stays faithful

`Daf.SQLCute.WhereTree` reads a flat clause list as OR-of-AND-runs. The emitter therefore encodes the authored tree in that grammar:

```pascal
(Prop('a') = 1 or Prop('b') = 1) and (Prop('c') = 1)
```

emits a **nested group** for the disjunction plus one plain clause, and normalizes back to the conjunction of `(a or b)` with `c`. Inlining it would have produced `a OR (b AND c)` — a different predicate that both back-ends would have agreed on.

Where the emitter does *not* nest:

```pascal
Prop('a') = 1 and Prop('b') = 1 or Prop('c') = 1 and Prop('d') = 1
```

Four plain clauses, no group. Delphi's precedence already grouped the tree as `(a and b) or (c and d)`, and the shared normalizer reproduces exactly that from the flat list. The layer contains no precedence logic of its own.

Negation:

```pascal
not (Prop('Name').Like('%son%'))         // one clause, operator NOT LIKE — no group
not (Prop('a') = 1)                      // one clause with IsNot — no group
not ((A and B) or (C and D))             // one negated nested group
```

The last one emits `NOT (…)` around the authored shape. There is no De Morgan pass, so the SQL still resembles what you wrote.

Case sensitivity travels with the LIKE family:

```pascal
Prop('Name').Like('%son%')         // LOWER(Name) LIKE LOWER(?)
Prop('Name').Like('%son%', True)   // Name LIKE ?
```

---

## The in-memory repository

```pascal
var Loaded := LoadEverything;                       // a TObjectList<TProduct>
var Repo: IRepository<TProduct> := TInMemoryRepository<TProduct>.Create(Loaded);

var Cheap := TSpecification<TProduct>.New.Where(Prop('Price') < 50);
var Rich  := TSpecification<TProduct>.New.Where(Prop('Price') > 500);

Repo.List(Cheap);
Repo.List(Rich);
Repo.List(Cheap);      // the plan for Cheap is reused; nothing is reloaded
```

Plans are cached by `TSpecification<T>.Key`, so a repeated rule compiles nothing the second time. The source is referenced, not owned — free the list yourself, after the repository.

Scoping to a subset takes a base query, which is cloned per rule so yours is never mutated:

```pascal
var Tools := TQuery.New.From('Product').Where('kind', 'tool');
var Repo: IRepository<TProduct> := TInMemoryRepository<TProduct>.Create(Loaded, Tools);
```

The `Compiled` property on the concrete class reports how many plans have been built, which is how the plan-reuse claim is a spec rather than a comment.

---

## What fails, and when

| Situation | What happens |
|-----------|--------------|
| A member name that resolves to nothing | `EExprMemberError`, where the expression is authored (in `Daf.Query.Expressions`) |
| A non-predicate node where a condition belongs | `EExprEmissionError` at emission time |
| An operator given the wrong number of operands | `EExprEmissionError` at emission time |
| A query the in-memory lane cannot execute | `EInMemoryQueryError` when the plan is compiled |

Nothing is skipped and nothing degrades. A clause silently dropped during emission would *widen* the query, which is the worst failure mode a filter has — so a node the emitter does not understand raises instead.

And the asymmetry worth repeating: **translatable is not the same as in-memory-executable.** Subquery/`EXISTS` conditions, `GROUP BY`/`HAVING`, joins and set operations compile to SQL but raise on the single-list lane. The adapter never catches that error and never filters the collection in Delphi instead.
