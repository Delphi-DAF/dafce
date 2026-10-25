# SQLCute In-Memory — One Query, Two Back-Ends

**🌍 Language: English | [Español](README.es.md)**

A second SQLCute back-end. Where `IQueryCompiler` turns an `IQuery` into SQL text, `TInMemory` turns the **same** `IQuery` into an executable plan over already-loaded Delphi collections.

The payoff is **symmetry**: one query object filters a database or a `TList<T>` you already have in memory — which is what makes client-side filtering of cached lists, and repositories backed by a plain list in tests, possible with no second query language.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Concepts

| Concept | Description |
|---------|-------------|
| `TInMemory` | Entry point — compiles an `IQuery` onto the in-memory engine |
| `IInMemoryPlan<T>` | A reusable compiled plan; `Over(Source)` runs it against any source of `T` |
| `IQueryContext` | Per-call catalog mapping table names to sources (never global) |
| `IRow` | The dynamic row model (`Get` / `Has` / `Columns`, values as `TValue`) |
| `TRttiRow<T>` / `TDictRow` / `TJoinRow` | Row adapters: object view, generic bag, join composite |
| `EInMemoryQueryError` | Raised when a query cannot be executed in memory — always fail-fast, never a wrong result |

Results are `IEnumerable<T>` from [`Daf.Linq`](../Linq/README.md) — deferred, ref-counted, `for..in`-able.

---

## Quick Start

```pascal
uses Daf.SQLCute, Daf.SQLCute.Compiler, Daf.SQLCute.InMemory;

var Q := TQuery.New.From('people')
  .Where('age', '>', 18)
  .OrderBy('name')
  .Limit(10);

// A database…
var Compiler: IQueryCompiler := TAnsiSqlCompiler.Create;  // ref-counted: it needs an owner
var R := Compiler.Compile(Q);       // SQL + bindings

// …or the list you already loaded — same query object.
for var P in TInMemory.Elements<TPerson>(Q, People) do
  Writeln(P.Name);
```

Column names bridge naming styles: SQL `user_id` resolves to a Delphi member `UserId`.

---

## The Two Lanes

The compiler inspects the AST and picks a lane.

### Fast typed lane — one list, no wrappers

A query over a single source with no JOIN, subquery, set operation or projection compiles to a predicate and comparer **over `T` itself**. No `IRow` is allocated, no catalog is needed, and the result still yields `T`:

```pascal
var Plan := TInMemory.Compile<TPerson>(Q);         // compile once
for var P in Plan.Over(People) do ...              // …run over any source of T
for var P in Plan.Over(OtherPeople) do ...
```

### General lane — several sources, via a per-call context

JOINs, subqueries, set operations and GROUP BY need named sources. They are resolved through an `IQueryContext` supplied at execution time — explicit, never a global registry:

```pascal
var Ctx := TQueryContext.Create;
try
  Ctx.Map<TPerson>('people', People).Map<TOrder>('orders', Orders);
  for var R in TInMemory.Rows(
    TQuery.New.From('people').Join('orders', 'people.id', 'orders.user_id'), Ctx) do
    Writeln(R.Get('people.name').AsString, ' ', R.Get('orders.total').AsInteger);
finally
  Ctx.Free;                                        // the caller owns the context
end;
```

Running a multi-source query through the single-list entry point raises `EInMemoryQueryError` rather than silently returning wrong rows.

---

## Output Shapes

| You call | You get |
|----------|---------|
| `TInMemory.Elements<T>(Q, Source)` | `IEnumerable<T>` — the original elements (no projection allowed) |
| `TInMemory.Rows<T>(Q, Source)` | `IEnumerable<IRow>` — dynamic rows, projected when the query has a `SELECT` |
| `TInMemory.Project<TSource, TResult>(Q, Source)` | `IEnumerable<TResult>` — each row materialized into a result class by RTTI |
| `TInMemory.Rows(Q, Context)` | `IEnumerable<IRow>` — the general lane |
| `TInMemory.Project<TResult>(Q, Context)` | `IEnumerable<TResult>` — the general lane, materialized |

```pascal
// Projection into a user class; a NULL column leaves a TNullable member IsNull.
for var V in TInMemory.Project<TPerson, TPersonView>(
    TQuery.New.From('people').Select(['name', 'age']), People) do
  if V.Age.HasValue then Writeln(V.Name, ': ', V.Age.Value);
```

---

## SQL Semantics

The in-memory engine follows ANSI semantics, so both back-ends answer the same question the same way, with
one exception — the default NULL order, in the last point:

- **Three-valued logic.** A comparison against NULL is *unknown*, not false; `AND`/`OR`/`NOT` combine by the SQL truth tables; a row is kept only when the predicate is *true*. So `WhereNot('age','>',18)` excludes a NULL age — exactly as a database does.
- **WHERE precedence.** `AND` binds tighter than `OR`. Both back-ends build from one shared normalizer (`Daf.SQLCute.WhereTree`), so they cannot disagree; the SQL side emits explicit parentheses.
- **DISTINCT** de-duplicates over the *output* columns, by value, with NULLs comparing equal.
- **Aggregates** skip NULLs (`COUNT(col)`, `SUM`, `AVG`, `MIN`, `MAX`); `COUNT(*)` counts rows. `SUM` of integers is an `Int64`, of `Currency` values a `Currency`, and a `Double` only when a value is floating point; an overflow, or a value that is not a number in `SUM` or `AVG`, raises `EInMemoryQueryError`.
- **GROUP BY** puts all NULL keys in one group.
- **ORDER BY** treats NULL as the smallest value by default — nulls-first ascending, nulls-last descending — and a query overrides that per column with `NullsFirst` / `NullsLast`, which the plan honors.
  Without them the two back-ends can differ: the database applies its own default, and Postgres and
  Oracle put NULL last in ascending order.

The full rules, with examples, are in the [usage guide](docs/GUIDE.md).

---

## Limitations

The back-end is **read-only** — DML (`INSERT`/`UPDATE`/`DELETE`) stays SQL-only. It checks this by the query's **declared kind**: anything that is not a retrieval is rejected naming the kind it is — `The in-memory backend is read-only; it cannot execute a Delete statement.` Raw SQL fragments (raw `FROM`, raw `ORDER BY`, raw JOIN `ON`, `CombineRaw`), CTEs (`WITH`), subquery JOINs and the date predicates (`WhereDate`, `WhereTime`, `WhereDatePart`) cannot be evaluated in memory and raise `EInMemoryQueryError`. Execution is a naive scan — no indexing or optimization.

---

## Documentation

| Resource | Description |
|----------|-------------|
| [**Usage Guide**](docs/GUIDE.md) | Lanes, row model, projection, NULL semantics, DISTINCT |
| [InMemoryQuerySample](samples/InMemoryQuerySample/README.md) | Filter, sort, paginate and project one `TList<T>` |
| [InMemoryJoinSample](samples/InMemoryJoinSample/README.md) | Two sources joined through a per-call context |
| [Daf.Linq](../Linq/README.md) | The lazy sequence library the plans are built on |
| [SQLCute](../SQLCute/README.md) | The fluent query builder producing the AST |
| [Dialect Compilers](../SQLCute/README.md) | The SQL back-end this one mirrors |
