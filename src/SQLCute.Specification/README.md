# Daf.SQLCute.Specification — One Specification, One Emission

**🌍 Language: English | [Español](README.es.md)**

The SQL-side half. It takes a specification authored with [`Daf.Specification`](../Specification/README.md) and lowers it onto a SQLCute `IQuery`: the filter becomes WHERE, the ordering ORDER BY, the page LIMIT/OFFSET. That `IQuery` is the only thing it produces.

```pascal
uses Daf.SQLCute, Daf.SQLCute.Compiler, Daf.SQLCute.InMemory,
     Daf.Specification, Daf.SQLCute.Specification;

var Q := TExprEmitter.Where(TQuery.New.From('Product'),
  (Prop('Price') > 100) and (Prop('Active') = True));

// …a database:
var Compiler: IQueryCompiler := TAnsiSqlCompiler.Create;  // ref-counted: it needs an owner
var Sql := Compiler.Compile(Q);
// …or the list you already loaded — same query object:
for var P in TInMemory.Elements<TProduct>(Q, Loaded) do ...
```

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Why there is only one emission

Both back-ends are already downstream of the SQLCute AST:

```
              expression tree
                    │  one emission
                    ▼
          SQLCute clauses (IQuery)
                    │
             ┌──────┴──────┐
             ▼             ▼
      TAnsiSqlCompiler   TInMemory.Compile<T>
        (+ dialects)     .Over(TEnumerable<T>)
             │             │
             ▼             ▼
         SQL text     in-memory execution
```

So "one query, two back-ends" is a property this package **inherits**, not one it implements. There is no expression → `Daf.Linq` translator here: the in-memory lane is reached through the same `IQuery`.

---

## Concepts

| Concept | Description |
|---------|-------------|
| `TExprEmitter.Where` | Expression + `IQuery` → the same `IQuery`, carrying the conditions |
| `TExprEmitter.Apply<T>` | A whole specification — filter, ordering, page — onto one query |
| `TInMemoryRepository<T>` | The in-memory `IRepository<T>` adapter, with a per-rule plan cache |
| `ColumnFor` | Member → column, through the supplied strategy or verbatim |
| `WhereOpFor` | The model's operators mapped onto SQLCute's `TWhereOp` — a table |
| `EExprEmissionError` | A node that is not a predicate, or an operator given the wrong arity |

---

## Emission preserves what you wrote

`TWhereTree.Build` **reconstructs** a boolean tree from a flat clause list; it does not preserve one. It reads that list as OR-of-AND-runs, a grammar with exactly one level of alternation. Emit `(a or b) and c` as the flat list `[a AND, b OR, c AND]` and the normalizer groups it as `a OR (b AND c)` — a different predicate. And because both back-ends consume that same normalized tree, **parity testing cannot catch it**: SQL and in-memory agree, on the wrong answer.

So the emitter nests where alternation deepens:

| Child | inside… | emitted as |
|-------|---------|------------|
| `Or` | an AND-run | a nested group |
| `And` | an OR-list | inline — the first element carries OR, the rest AND |
| `And` | an AND-run | inline (associativity) |
| `Or` | an OR-list | inline (associativity) |

Negation is emitted, never rewritten:

| Authored | Emitted as |
|----------|------------|
| `not (leaf)` | the leaf's own negation — `IsNot`, the paired operator (`NOT LIKE`, `NOT IN`, `IS NOT NULL`, `NOT BETWEEN`), or `Negated` |
| `not (subtree)` | a negated nested group — `WhereNot(Callback)` / `OrWhereNot(Callback)` |

There is **no De Morgan pass**. The SQL reads as the shape you authored, not as a distributed equivalent of it.

The guarantee is semantic, not structural: the normalized clause list and the authored tree evaluate identically under three-valued logic, for every assignment of *True* / *False* / *Unknown* to the leaves.

**Precedence is not implemented here.** `a and b or c and d` is emitted as four plain clauses with no group of the emitter's own, and the shared normalizer groups it as `(a and b) or (c and d)`.

---

## Breadth without new nodes

Everything SQLCute already expresses is reached by emitting the clause that says it:

```pascal
Prop('Name').Like('%son%')       // → LIKE clause
Prop('Name').StartsWith('And')   // → WhereStarts
Prop('Price').Between(10, 200)   // → BETWEEN clause
Prop('Price').IsIn([5, 120])     // → TWhereInClause
Prop('Price').IsNull             // → IS NULL
```

Every clause is produced through the fluent `IQuery` surface, which is also what keeps `TWhereInClause.Connector` the string `'AND'`/`'OR'` the normalizer compares it as, and what keeps emission to the four clause types the normalizer accepts.

Going through that surface is also why the empty set needs no special case here:

```pascal
Prop('Price').IsIn([])           // → WHERE (Price IS NULL AND Price IS NOT NULL)
```

`x IN (a, b)` is `x = a OR x = b`, so an empty list is the empty disjunction — false without consulting the column, and true for `NotIn`. SQLCute's builder resolves that where the condition is added, so this layer emits `IsIn([])` through the same call every other operator uses. No arity floor, no error, no branch of its own.

---

## The in-memory repository

```pascal
var Repo: IRepository<TProduct> := TInMemoryRepository<TProduct>.Create(Loaded);

Repo.List(TSpecification<TProduct>.New
  .Where(Prop('Active') = True)
  .OrderByDesc('Price')
  .Take(10));
```

The source is referenced, never owned or copied, and plans are cached by the rule's key — so N rules over one loaded collection compile N plans and reload nothing. **This is not a test stub:** running several rules over one already-loaded set is a production path, with the same specification type that goes to a database.

A base query scopes the repository to a subset, and is cloned per rule so the caller's query is never mutated:

```pascal
TInMemoryRepository<TProduct>.Create(Loaded, TQuery.New.From('Product').Where('kind', 'tool'));
```

---

## The documented asymmetry

**Translatable is not the same as in-memory-executable.** Clauses the in-memory lane has not implemented — subquery/`EXISTS` conditions, `GROUP BY`/`HAVING`, joins, set operations — compile to SQL but raise `EInMemoryQueryError` on the single-list lane.

That failure is **loud, and at plan-compile time**. The adapter never catches it and never filters the collection in Delphi instead — there is no client-side fallback.

Note that a specification cannot reach that path on its own — the expression model has no `EXISTS` and no raw-SQL operator, by design. It takes a base query to get there.

As the in-memory back-end grows those phases, this package inherits them with no change of its own.

---

## Documentation

| Resource | Description |
|----------|-------------|
| [**Usage Guide**](docs/GUIDE.md) | Emission, the name seam, the adapter, what fails and when |
| [CatalogServiceSample](samples/CatalogServiceSample/README.md) | A domain service over `IRepository<T>`, resolved from a `TList<T>` |
| [Daf.Specification](../Specification/README.md) | The tree, the authoring surface, specifications and the port |
| [SQLCute In-Memory](../SQLCute.InMemory/README.md) | The back-end this emission reaches through `IQuery` |
| [SQLCute](../SQLCute/README.md) | The AST and the fluent surface this emits into |
