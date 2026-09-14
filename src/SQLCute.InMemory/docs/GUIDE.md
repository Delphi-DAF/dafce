# SQLCute In-Memory — Usage Guide

**🌍 Language: English | [Español](GUIDE.es.md)**

---

## Introduction

SQLCute builds a portable query AST (`IQuery.Clauses`) and compiles it with a back-end. There are two:

| Back-end | Unit | Produces |
|----------|------|----------|
| SQL | `Daf.SQLCute.Compiler` (+ dialects) | `TSQLResult` — SQL text plus bindings |
| In-memory | `Daf.SQLCute.InMemory` | An executable plan over Delphi collections |

Both read the same clauses, read-only. Neither mutates the query, so one `IQuery` can be sent to a database and run over a cached list in the same breath.

```pascal
uses Daf.SQLCute, Daf.SQLCute.Compiler, Daf.SQLCute.InMemory;

var Q := TQuery.New.From('people').Where('age', '>', 18).OrderBy('name');

var Compiler: IQueryCompiler := TAnsiSqlCompiler.Create;  // ref-counted: it needs an owner
var Sql := Compiler.Compile(Q);       // → SQL + bindings
var Rows := TInMemory.Elements<TPerson>(Q, People); // → IEnumerable<TPerson>
```

There is a third entry point, `Linq.From<T>` in [`Daf.Linq`](../../Linq/README.md), for entering the same execution engine directly with typed closures and no SQLCute involved. All three converge on `IEnumerable<T>`.

---

## Table of Contents

1. [Choosing a lane](#choosing-a-lane)
2. [Plans and sources](#plans-and-sources)
3. [The row model](#the-row-model)
4. [Typed projection](#typed-projection)
5. [NULL semantics](#null-semantics)
6. [DISTINCT](#distinct)
7. [Multi-source queries](#multi-source-queries)
8. [Errors and limitations](#errors-and-limitations)
9. [Parity with the SQL back-end](#parity-with-the-sql-back-end)

---

## Choosing a lane

The compiler analyses the AST and picks one of two execution lanes. You do not select it — you only pick the entry point that matches your data.

| | Fast typed lane | General lane |
|---|---|---|
| Source | one `TList<T>` / array / `IEnumerable<T>` | an `IQueryContext` mapping named sources |
| Row representation | `T` itself — no wrapper allocated | `IRow` composites |
| Supports | WHERE, ORDER BY, LIMIT/OFFSET, DISTINCT | everything, incl. JOIN, GROUP BY, subqueries, set ops |
| Output | `IEnumerable<T>` | `IEnumerable<IRow>` or `IEnumerable<TResult>` |
| Entry point | `TInMemory.Elements<T>` / `Compile<T>` | `TInMemory.Rows` / `Project<TResult>` |

Anything in the following list forces the general lane, and using it with a single list raises `EInMemoryQueryError`:

- `Join` / `LeftJoin` / … (any join)
- `Union` / `UnionAll` / `Intersect` / `Except`
- a subquery WHERE (`WhereInQuery`, `WhereExists`, …)
- a derived-table or raw `FROM`
- `WITH` (CTE)
- `GroupBy` / `Having`

A `SELECT` projection does not force the general lane, but it does rule out `Elements<T>` (which preserves `T`): use `Rows<T>` or `Project<TSource, TResult>` instead.

---

## Plans and sources

`Compile<T>` produces a reusable plan. Compile once, run many times:

```pascal
var Plan := TInMemory.Compile<TPerson>(TQuery.New.From('people').Where('age', '>', 18));
for var P in Plan.Over(TeamA) do ...
for var P in Plan.Over(TeamB) do ...
```

`Over` accepts a `TEnumerable<T>` (so `TList<T>`, `TObjectList<T>`, …), a `TArray<T>` or an `IEnumerable<T>`. Nothing is copied: the plan reads the source as it iterates.

`TInMemory.Elements<T>(Q, Source)` is sugar for `Compile<T>(Q).Over(Source)`.

**Execution is deferred.** Building the queryable scans nothing; work starts when the `for..in` begins. `Limit` short-circuits — a `Limit(10)` over a million elements stops consuming the source after the tenth result.

### Column names

Columns are matched against members by a normalized key: the table prefix is dropped, case is ignored and underscores are stripped. So the SQL column `user_id` resolves to the Delphi member `UserId`, and `people.name` resolves to `Name`.

---

## The row model

Wherever no static Delphi type exists — joined rows, group rows, generic bags — the engine carries values as `IRow`:

```pascal
IRow = interface
  function Get(const Column: string): TValue;
  function Has(const Column: string): Boolean;
  function Columns: TArray<string>;
end;
```

Values are `TValue`, not `Variant`: RTTI is `TValue`-native, and `TValue` carries records and enums that a `Variant` cannot. SQLCute's `Variant` bindings are converted once, when the plan is built — never per row.

| Adapter | Backed by | Notes |
|---------|-----------|-------|
| `TRttiRow<T>` | an object or record | zero-copy; member lookups cached per (type, column) |
| `TDictRow` | a name → `TValue` map | no RTTI; `TDictRow.Make(['age','name'], [30, 'Bob'])` |
| `TJoinRow` | child rows with aliases | resolves `orders.total` to the `orders` side |
| `TNullRow` | nothing | the all-NULL side of an outer join |

`TJoinRow` resolves a qualified name against the matching alias, and an unqualified name against the first side that carries it.

```pascal
var Row: IRow := TRttiRow<TPerson>.Create(Accessors, Person);
Row.Get('people.name').AsString;    // prefix stripped → Name
```

---

## Typed projection

With a result class, each final row is materialized by RTTI — output column or alias to member, with the `TValue` coerced to the member's type. Properties and public fields both work.

```pascal
type
  TPersonView = class
  private
    FName: string;
    FAge: TNullable<Integer>;
  public
    property Name: string read FName write FName;
    property Age: TNullable<Integer> read FAge write FAge;
  end;

for var V in TInMemory.Project<TPerson, TPersonView>(
    TQuery.New.From('people').Select(['name', 'age']), People) do
  ...
```

Aliases map to members by name: `SelectAs('full_name', 'display_name')` populates a `DisplayName` member.

Without a result class the sequence yields `IRow` (`Rows<T>` / `Rows`), and an unprojected single-source query yields the original `T` (`Elements<T>`).

The caller owns materialized instances — collect them into a `TObjectList<TResult>` if they must outlive the loop.

---

## NULL semantics

SQL NULL surfaces in three places, and the engine handles all three consistently — that consistency is what keeps the two back-ends in agreement.

### Three-valued logic

Predicate evaluation is three-valued. A comparison with a NULL operand is *unknown*, not false, and a row is kept only when the predicate evaluates to *true*.

| `A` | `NOT A` |
|---|---|
| true | false |
| false | true |
| unknown | unknown |

| `AND` | true | false | unknown |
|---|---|---|---|
| **true** | true | false | unknown |
| **false** | false | false | false |
| **unknown** | unknown | false | unknown |

| `OR` | true | false | unknown |
|---|---|---|---|
| **true** | true | true | true |
| **false** | true | false | unknown |
| **unknown** | true | unknown | unknown |

A negated comparison over a NULL column shows the effect:

```pascal
TQuery.New.From('people').WhereNot('age', '>', 18)
// age = NULL → (age > 18) is unknown → NOT unknown is unknown → row excluded.
// A database excludes it too.
```

`WhereNull` / `WhereNotNull` test presence explicitly and always answer true or false.

### Precedence

`AND` binds tighter than `OR`, as in SQL. `Where('a',1).Where('b',2).OrWhere('c',3).Where('d',4)` means `(a AND b) OR (c AND d)`, never a left-to-right fold. Both back-ends build from the same normalizer (`Daf.SQLCute.WhereTree`), so precedence has a single source of truth; the SQL side emits the parentheses explicitly.

### Aggregates

`SUM`, `AVG`, `MIN`, `MAX` and `COUNT(col)` skip NULLs; `COUNT(*)` counts rows. An aggregate over a group with no non-NULL values yields NULL.

### GROUP BY

All NULL keys fall into exactly one group.

### ORDER BY

An unqualified `OrderBy` treats NULL as the smallest value: nulls first ascending, nulls last descending.

A query can override that per column, and the plan honors it:

```pascal
TInMemory.Elements<TPerson>(
  TQuery.New.From('people').OrderBy('age').NullsLast, People);   // NULLs at the end
```

Both fluent forms work (`NullsFirst` / `NullsLast`, or the `OrderBy(col, TNullsOrder.Last)` overload); the compiler reads the clause's option into `TOrderSpec.Nulls`.

> **The one place the two back-ends can still disagree.** An *unqualified* sort over a nullable column follows each side's own default: NULL-is-smallest in memory, whatever the dialect does in SQL — and Postgres and Oracle do the opposite (nulls last ascending). This lane has no notion of a dialect. **Qualify the sort — `NullsFirst` / `NullsLast` — to get identical results from both.**

### Materialization

A NULL column projected into a `TNullable<T>` member leaves it `IsNull`, distinguishing "no value" from a present `0` or `''`. A NULL projected into a plain member leaves that member at its default.

```pascal
if V.Age.HasValue then Writeln(V.Age.Value) else Writeln('(no age)');
```

---

## DISTINCT

`Distinct` de-duplicates over the **output** columns — the projected set when the query has a `SELECT`, otherwise every column — comparing **by value**, never by reference identity. NULLs compare equal, so duplicate NULLs collapse to one.

```pascal
TQuery.New.From('people').Select('country').Distinct
// one row per distinct country, not one per person
```

In the fast typed lane with no projection, "every column" means every readable public or published property of `T` when `T` is a class, and every field when `T` is a record — a public field of a class does not count. They are compared by value, so two distinct `TPerson` instances holding equal values are one row.

---

## Multi-source queries

Map every named source into a per-call context. The context is a plain object you own:

```pascal
var Ctx := TQueryContext.Create;
try
  Ctx.Map<TPerson>('people', People)
     .Map<TOrder>('orders', Orders);
  // …run queries…
finally
  Ctx.Free;
end;
```

`Map` also accepts a `TArray<IRow>` directly, for data that is not a Delphi object graph. Referencing a name the context does not map raises `EInMemoryQueryError` naming the missing source.

### JOINs

INNER, LEFT, RIGHT, CROSS and FULL OUTER are supported, with the ON condition given as a column pair or a callback. Unmatched sides read as NULL.

```pascal
TQuery.New.From('people').LeftJoin('orders', 'people.id', 'orders.user_id')
// people with no orders still appear; orders.* reads as NULL
```

### GROUP BY, aggregates, HAVING

Grouping emits one flat row per group carrying the group-key columns and the aggregate columns, which is the shape SQL returns:

```pascal
TQuery.New.From('people')
  .Select('country').SelectCount('*', 'n')
  .GroupBy('country')
  .Having('n', '>', 1)
// rows: (country, n)
```

The sequence library's own `GroupBy` yields the richer `IGrouping<TKey, T>` — the key plus its members — for code that goes through [`Daf.Linq`](../../Linq/README.md) directly.

### Subqueries and set operations

`WhereInQuery` / `WhereNotInQuery` and EXISTS / NOT EXISTS are evaluated against the context; each uncorrelated subquery runs once and is cached. `Union`, `UnionAll`, `Intersect`, `Except` and the `ALL` variants combine result sets, comparing rows by output column value with NULLs equal.

---

## Errors and limitations

Everything the engine cannot faithfully execute raises `EInMemoryQueryError` at compile time rather than returning approximate rows:

| Rejected | Why |
|----------|-----|
| `INSERT` / `UPDATE` / `DELETE` | the back-end is read-only; DML stays SQL-only |
| raw `FROM`, raw `ORDER BY`, raw JOIN `ON`, `CombineRaw` | raw SQL text has no in-memory meaning |
| `WITH` (CTEs), subquery JOINs | not supported |
| `WhereDate`, `WhereTime`, `WhereDatePart` and their `Or*` | the date casts are dialect-specific; SQL-only |
| raw / subquery `SELECT` columns | not supported |
| a multi-source query run with a single list | needs a context |
| a projecting query run through `Elements<T>` | use `Rows` or `Project` |

Execution is a naive full scan — no indexes, no optimizer.

---

## Parity with the SQL back-end

`Parity.Feat.pas` in this module's `test/` folder is the symmetry harness: a shared query set is pushed through the SQL compiler and through all three in-memory lanes over one seeded dataset, and every lane must reproduce the same declared result set. It also asserts that neither back-end mutates the AST, that bindings match their placeholders, and that the emitted SQL carries the very precedence parentheses the in-memory predicate was built from.

That set is the guard on the "one query, two back-ends" promise.
