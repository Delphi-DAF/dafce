# Daf.Query.Expressions — Typed Query Authoring, SQL-Free

**🌍 Language: English | [Español](README.es.md)**

A query condition, written as Delphi code with operators, that stays an **inspectable tree** instead of becoming a closure.

```pascal
var Spec := TSpecification<TProduct>.New
  .Where((Prop('Price') > 100) and (Prop('Active') = True))
  .OrderByDesc('Price')
  .Take(10);
```

That value is a domain object. It can be asserted in a unit test, passed across a port, stored, logged — and later emitted to SQL by [`Daf.SQLCute.Expressions`](../SQLCute.Expressions/README.md). **This package contains no SQL and does not reference SQLCute.** A domain service that depends on it links none.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Concepts

| Concept | Description |
|---------|-------------|
| `IExpr` | A node of the expression tree; ref-counted, no manual `Free` |
| `TExprKind` | The bounded node set: `Compare` / `Logical` / `Unary` + `Member` / `Value` / `Param` |
| `TCompareOp` / `TLogicalOp` / `TUnaryOp` | The model's own operators — no raw SQL, no `EXISTS` |
| `TExpr` | The authoring record: overloaded operators, LIKE / BETWEEN / IN |
| `Prop(Name)` | A member reference by name — the always-available path |
| `TEntityExpr<T>` | RTTI proxy: an entity's members as leaves, validated where written |
| `TPropExpr<T>` | The **optional** metamodel leaf, for compile-time-checked authoring |
| `INameResolution` | The one seam: member → column, type → table |
| `TSpecification<T>` | A reusable rule: filter + ordering + paging |
| `IRepository<T>` | The port; `T` is constrained to **no** base class |
| `ExprToText` | A canonical rendering — for assertions, cache keys and diagnostics |

---

## Why it is a tree and not a closure

Commons already has a closure-based filter, `TFilterSpec<T>` in `Daf.Expression`, and `Daf.Linq` runs it over loaded sequences. This package borrows that operator-overloading **shape** and none of its type, because a closure is opaque: you can run it, never translate it.

| World | Filter | Runs on |
|-------|--------|---------|
| SQL, or one query for both back-ends | this package → `IQuery` | a database *or* a loaded collection |
| Already-loaded objects, arbitrary Delphi code | `Daf.Linq` + `TFilterSpec<T>` | a materialized sequence |

They are siblings serving different worlds. Which is why `TSpecification<T>` has **no overload taking a closure**: a specification that cannot be translated must be unrepresentable rather than silently evaluated client-side.

---

## The model is data-driven

Six node types, three operator enums. Adding a comparison is a new **enum value**, never a new class — the same binary node carries every operator, and `Like`, `Between` and `In` are emitted as the SQLCute clause that already says them.

```pascal
Prop('Name').Like('%son%')          // one node, operator Like
Prop('Price').Between(10, 200)      // one node, operator Between
Prop('Price').IsIn([5, 120])        // one node, operator In
Prop('Price').IsNull
not (Prop('Active') = True)
```

Values are carried as a single `Variant` — the representation SQLCute clauses already hold — so no typed-accessor parameter model is reimplemented. NULL is Commons' `TNullable`, narrowed at the authoring edge:

```pascal
Prop('Id')  = SomeGuid                       // typed overload → canonical string
Prop('Age') = TExprValue.From<Integer>(N)    // TNullable → its value, or Null
```

Narrowing happens **where you write it**, never inside the emission: a leaf holding a richer value would leave the in-memory lane comparing a record while SQL compares its text.

---

## Two authoring paths, one node

The runtime path is always sufficient and validates each member against the entity:

```pascal
var P := TEntityExpr<TProduct>.Create(nil);
P.Prop('Price') > 100
P.Prop('Pricee')     // raises EExprMemberError, naming the member and the type
```

For an entity where it earns its keep, a hand-written metamodel adds typo-safety and completion:

```pascal
ProductMeta = class
public
  class var Price: TPropExpr<TProduct>;
  class constructor Create;
end;

class constructor ProductMeta.Create;
begin
  Price := TPropExpr<TProduct>.Create('Price');   // the PROPERTY name
end;

// ProductMeta.Price > 100
```

It stays an **option**. Both forms produce identical nodes, mix freely in one expression, and nothing downstream can tell them apart — so adopting or dropping a metamodel later is a local edit, not a migration. Be precise about the trade: typo-safety yes, rename-safety no (the field holds a string). Validating on construction is what turns a stale name into a loud failure the first time a test touches the class.

---

## Names, and what this layer refuses to own

One strategy answers both naming questions, and richer mapping is the ORM's:

```pascal
INameResolution.ColumnFor(EntityType, Member)   // default: the member name
INameResolution.TableFor(EntityType)            // default: type name minus a leading T
```

The table default strips `T` only when an upper-case letter follows, so `TProduct` → `Product` while `Thing` is left alone. No pluralization, no case conversion — those are naming *policy*, and guessing them would collide with whatever the ORM decides while looking like a feature.

Keys, lock columns, attributes and instance-driven DML (`Update(Instance)`) are deliberately **absent**. The ORM is not designed yet; this layer commits to the seam, not the mapping.

---

## Failing where the mistake is

A member name that resolves to nothing raises at the point the expression is authored, naming the member and the type. The hazard being avoided is a pass-through default: it makes a wrong name look right on whichever path tolerates it — SQL will happily send an unmapped column to the server, while the in-memory lane cannot — so the divergence surfaces far from its cause and on only one back-end.

---

## Documentation

| Resource | Description |
|----------|-------------|
| [**Usage Guide**](docs/GUIDE.md) | Authoring, the proxy, the metamodel, specifications |
| [Daf.SQLCute.Expressions](../SQLCute.Expressions/README.md) | The emission into `IQuery`, and the in-memory adapter |
| [SQLCute](../SQLCute.Abstractions/README.md) | The query builder this eventually emits into |
| [Daf.Linq](../Linq/README.md) | The closure world — the sibling, not the rival |
