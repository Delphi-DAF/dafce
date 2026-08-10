# Daf.Specification — Usage Guide

**🌍 Language: English | [Español](GUIDE.es.md)**

← [Back to README](../README.md)

---

## Introduction

This package holds the domain-side half of the expression layer: a tree, the operators that build it, the seam that names things, and the specification and port that carry it across a boundary. It compiles against Commons and nothing else, so everything here is usable and testable with no database and no SQL unit linked.

The SQL-side half is [`Daf.SQLCute.Specification`](../../SQLCute.Specification/README.md), which emits a tree into a SQLCute `IQuery` and ships the in-memory repository adapter.

```pascal
uses Daf.Specification;

var Spec := TSpecification<TProduct>.New
  .Where((Prop('Price') > 100) and (Prop('Active') = True))
  .OrderByDesc('Price');
```

---

## Table of Contents

| Topic | What it covers |
|-------|----------------|
| [Authoring with operators](guide/authoring.md) | The operators, the breadth surface, values and NULL |
| [The entity proxy](guide/entity-proxy.md) | `TEntityExpr<T>`, name resolution, failing where it is written |
| [The optional metamodel](guide/metamodel.md) | `TPropExpr<T>`, what it buys and what it does not |
| [Specifications and the port](guide/specifications.md) | `TSpecification<T>`, `IRepository<T>`, hexagonal composition |

---

## The node model at a glance

Six node types and three operator enums, and that is the whole model:

| Kind | Interface | Carries |
|------|-----------|---------|
| `Compare` | `ICompareExpr` | an operator, a target, zero or more operands, case sensitivity |
| `Logical` | `ILogicalExpr` | an operator and two operands |
| `Unary` | `IUnaryExpr` | an operator and one operand |
| `Member` | `IMemberExpr` | a **property** name (never a column) and a reserved accessor |
| `Value` | `IValueExpr` | one `Variant` |
| `Param` | `IParamExpr` | a name and one `Variant` |

Arity travels with the operator rather than with the type: `IsNull` takes no operand, `Between` takes two, `In` takes as many as the set holds. That is why breadth grows without the model growing — a new comparison is a new enum value and a new row in the emitter's lookup table.

Lifetime is interface ref-counting throughout. There is no `Acquire`/`Release`, no manual `Free`, and no second idiom.

---

## Reading a tree back

`ExprToText` renders a tree canonically — structural, stable, and blind to how the tree was authored:

```pascal
ExprToText(Prop('Price') > 100)
// (m:Price Greater v:100#17)
```

Use it to assert that two expressions are the same thing, as a cache key, or in a log line. It is never a step on the way to execution; the emitter reads the tree itself.

`TSpecification<T>.Key` does the same for a whole rule, filter and ordering and page together, which is what lets an adapter compile a plan once and reuse it.

---

## What is deliberately absent

| Absent | Why |
|--------|-----|
| A closure filter on a specification | Untranslatable must be unrepresentable; use `Daf.Linq` for that world |
| Eager-loading directives (`includes`) | Nothing to lower into, and the wrong owner — assembling an aggregate is the adapter's job |
| A `T: TEntity` constraint | Nothing downstream needs one; records work too |
| Entity↔table mapping metadata | ORM territory, and the ORM is not designed yet |
| Instance-driven DML | Needs the persistent-member set, the primary key and change tracking — all ORM metadata |
| Raw SQL and `EXISTS` operators | A domain expression must not be *able* to say them |

The last one is enforced by the package boundary, not by discipline: this unit does not reference SQLCute, and its spec runner links the domain half alone. Acquire a SQL dependency and the project stops compiling.
