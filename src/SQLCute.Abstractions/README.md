# SQLCute.Abstractions

**🌍 Language: English | [Español](README.es.md)**

The query AST and the contracts a back-end talks. This module defines **interfaces, enums and clause nodes only** — no builder, no compiler, no engine. Reference it when you write a dialect compiler or a back-end of your own, or when a library of yours passes queries around without linking either.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

---

## What's in this module

Two units, and the order matters — `Daf.SQLCute` uses `Daf.SQLCute.Clauses`:

| Unit | Holds |
|------|-------|
| `Daf.SQLCute.Clauses` | The clause nodes — `TAbstractClause` and 21 of its 22 descendants — and the enums they carry |
| `Daf.SQLCute` | `IQuery`, `IQueryCompiler`, `TSQLResult`, `TQueryKind`, the `TQuery` factory, two helpers, and the twenty-second clause node — `TFromSubqueryClause`, which needs `IQuery` |

Both units are `{$SCOPEDENUMS ON}`, so every enum value is qualified: `TWhereOp.Equal`, `TSortDir.Desc`, `TJoinType.Left`.

### The two contracts

| Type | Role |
|------|------|
| `IQuery` | Fluent builder — every method returns `IQuery`, ref-counted, no `Free` |
| `IQueryCompiler` | `function Compile(const Query: IQuery): TSQLResult` — one method, nothing else |

`TSQLResult` is a record with `SQL: string` and `Bindings: TArray<Variant>`. The `?` placeholders in `SQL` map positionally to `Bindings`; a dialect compiler in its native placeholder style emits `$1` or `:p1` instead, in the same order. `TSQLResult.Empty` is the zero value.

---

## Reading a query: the AST is public

`IQuery` carries two members a back-end reads:

| Member | Returns |
|--------|---------|
| `Clauses` | `TArray<TAbstractClause>` — the clause nodes, in the order they were added |
| `Kind` | `TQueryKind` — `Select`, `Insert`, `Update` or `Delete` |

The DML facades (`AsInsert`, `AsInsertRows`, `AsInsertFrom`, `AsUpdate`, `AsDelete`) set `Kind`, so a back-end has the statement kind without walking the clauses. `Select` is the default.

The query owns its nodes. `Clauses` hands out references: don't free them, and don't hold one past the `IQuery` it came from. `IQuery.Clone` deep-copies the whole query, which is what every node's `Clone` method serves.

### The clause nodes

| Area | Nodes |
|------|-------|
| SELECT | `TSelectClause` (an array of `TSelectColumn`), `TDistinctClause` (marker, no fields) |
| FROM | `TFromClause`, `TFromRawClause`, `TFromSubqueryClause` |
| JOIN | `TJoinClause`, `TJoinSubqueryClause` |
| WHERE | `TWhereClause`, `TNestedWhereClause`, `TWhereInClause`, `TDateWhereClause` |
| GROUP BY / HAVING | `TGroupByClause`, `THavingClause` |
| ORDER BY | `TOrderByClause` |
| Paging | `TLimitClause`, `TOffsetClause` |
| Set operations | `TUnionClause` |
| CTE | `TWithClause` |
| Raw SQL | `TRawClause` |
| DML | `TInsertClause`, `TUpdateSetClause`, `TDeleteClause` (marker — the table comes from `TFromClause`) |

One call produces one node: `WhereIn` adds a `TWhereInClause`, and `AsUpdate` adds one `TUpdateSetClause` per column–value pair.

Three fields decide how a node is read, and they appear across the tree:

- **`IsRaw`** (`TSelectColumn`, `TOrderByClause`, `TGroupByClause`, `TUpdateSetClause`) — emit the value verbatim.
- **`Connector: TBoolOp`** (`TWhereClause`, `THavingClause`, `TNestedWhereClause`, `TDateWhereClause`) — how this condition joins the *previous* sibling. ⚠️ `TWhereInClause.Connector` is a `string`, `'AND'` or `'OR'`.
- **`SubQuery`** — a nested query, typed `IInterface` (see below).

A LIKE condition is a `TWhereClause` whose `Op` is `Like` or `NotLike`. With `CaseSensitive = False` its `Value` holds the pattern already lower-cased, and `EscapeChar` is `#0` when the call declared no escape character.

`TJoinClause` carries three ways to express its ON condition, with a precedence: `ConditionClauses` (from a callback) wins, then `Col1`/`Op`/`Col2`, then the raw `Condition` string.

### Sub-queries are typed `IInterface`

`Daf.SQLCute.Clauses` is compiled before the unit that declares `IQuery`, so a node holding a nested query declares it as `IInterface`. Cast on the way out:

```pascal
if Assigned(W.SubQuery) then
begin
  var Inner := W.SubQuery as IQuery;
  // walk Inner.Clauses
end;
```

The nodes with a sub-query field: `TWhereClause`, `TJoinSubqueryClause`, `TUnionClause`, `TWithClause`, `TInsertClause` — and `TSelectColumn`, a record inside `TSelectClause` and not a node of its own.

⚠️ `TFromSubqueryClause` is the exception: it is declared in `Daf.SQLCute` alongside `IQuery`, so its `SubQuery` field is an `IQuery` already and needs no cast.

### The hierarchy is closed

The descendants of `TAbstractClause` are the ones this module declares. A back-end handles those and no others — there is no registration point for a node of your own, and nothing outside this module derives from `TAbstractClause`.

---

## The enums

| Enum | Values |
|------|--------|
| `TWhereOp` | `Equal`, `NotEqual`, `Less`, `LessOrEqual`, `Greater`, `GreaterOrEqual`, `Like`, `NotLike`, `&In`, `NotIn`, `IsNull`, `IsNotNull`, `&Between`, `NotBetween`, `&Exists`, `NotExists`, `Raw` |
| `TBoolOp` | `opAnd`, `opOr` |
| `TSortDir` | `Asc`, `Desc` |
| `TNullsOrder` | `Default`, `First`, `Last` |
| `TJoinType` | `Inner`, `Left`, `Right`, `Cross`, `FullOuter` |
| `TUnionKind` | `Union`, `UnionAll`, `Intersect`, `&Except`, `IntersectAll`, `ExceptAll` |
| `TDatePart` | `dpDate`, `dpTime`, `dpYear`, `dpMonth`, `dpDay`, `dpHour`, `dpMinute` |
| `TQueryKind` | `Select`, `Insert`, `Update`, `Delete` |

⚠️ Four values collide with Delphi reserved words and carry the `&` in their declaration, so you type it too: `TWhereOp.&In`, `TWhereOp.&Between`, `TWhereOp.&Exists`, `TUnionKind.&Except`.

`TNullsOrder.Default` emits nothing and leaves NULL placement to the database, where each dialect has its own rule. On a `TOrderByClause` with `IsRaw` set, `Nulls` is ignored — a raw term goes out verbatim.

### The two helpers

`ParseWhereOp(const Op: string): TWhereOp` maps an operator string, case-insensitively and trimmed. It accepts eleven spellings:

```
'='  '<>'  '!='  '<'  '<='  '>'  '>='  'like'  'not like'  'in'  'not in'
```

Anything else raises `EArgumentException`:

```
Unknown SQL operator: "=="
```

The remaining `TWhereOp` values — `IsNull`, `IsNotNull`, `&Between`, `NotBetween`, `&Exists`, `NotExists`, `Raw` — have no string spelling. The `IQuery` methods that produce those conditions set them directly.

`QueryKindName(const K: TQueryKind): string` gives the value's name (`'Select'`, `'Insert'`, …), so a guard can name the kind it received.

---

## Getting an `IQuery`: `TQuery.New` needs a factory

`TQuery.New` returns a fresh query builder, and this module holds no implementation to build one. `Daf.SQLCute.Core` — in the `SQLCute` package — registers one in its `initialization` section. Linking that package is what makes `TQuery.New` work.

Link only `Daf.SQLCute.Abstractions` and the first call raises `EInvalidOpException`:

```
IQuery factory not registered. Ensure Daf.SQLCute.Core is linked.
```

To supply an `IQuery` implementation of your own, register it:

```pascal
uses Daf.SQLCute;

TQuery.RegisterFactory(
  function: IQuery
  begin
    Result := TMyQuery.Create;
  end);
```

`TIQueryFactory` is `reference to function: IQuery`. The last registration wins. A `nil` argument raises `EInvalidOpException`:

```
Argument Factory cannot be nil
```

⚠️ `RegisterIQueryFactory` is the same registration as a free function and is **deprecated**. It forwards to `TQuery.RegisterFactory`, so both entry points behave alike; new code calls the class method.

---

## Who implements these contracts

| Package | What it provides |
|---------|------------------|
| [`SQLCute`](../SQLCute/README.md) | `IQuery` (`Daf.SQLCute.Core`) and `IQueryCompiler` (`TAnsiSqlCompiler`), plus six dialect compilers deriving from it: Firebird, MySQL, Oracle, PostgreSQL, SQL Server, SQLite |
| [`SQLCute.InMemory`](../SQLCute.InMemory/README.md) | A back-end that emits no SQL: it consumes the same `Clauses` and `Kind` and runs them over loaded Delphi collections |
| [`SQLCute.Specification`](../SQLCute.Specification/README.md) | Produces an `IQuery` from a [`Daf.Specification`](../Specification/README.md) filter, ordering and page |

A worked query, end to end, is in [`SQLCute/samples/SQLCuteSample`](../SQLCute/samples/SQLCuteSample/README.md).

---

## Dependencies

No dependencies on other DAF modules. Delphi RTL only.

The builder and the compilers are in [`SQLCute`](../SQLCute/README.md).
