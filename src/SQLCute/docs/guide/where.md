# WHERE

**🌍 Language: English | [Español](where.es.md)**

← [Back to Guide](../GUIDE.md)

---

## Basic Equality

```pascal
.Where('status', 'active')
// → WHERE status = ?   Bindings: ['active']
```

With an explicit operator:

```pascal
.Where('price', '>', 100)
.Where('stock', '<=', 0)
// → WHERE price > ? AND stock <= ?   Bindings: [100, 0]
```

The operator is one of `=`, `<>`, `!=`, `<`, `<=`, `>`, `>=`, `like`, `not like`, `in` and `not in`, in any case; any other raises `EArgumentException`.

OR connector:

```pascal
.Where('status', 'pending')
.OrWhere('status', 'processing')
// → WHERE status = ? OR status = ?
```

---

## WhereNot

```pascal
.WhereNot('role', 'guest')
// → WHERE NOT (role = ?)

.OrWhereNot('status', 'closed')
// → OR NOT (status = ?)
```

---

## NULL Checks

```pascal
.WhereNull('deleted_at')       // → WHERE deleted_at IS NULL
.WhereNotNull('email')         // → WHERE email IS NOT NULL
.OrWhereNull('archived_at')    // → OR archived_at IS NULL
```

Passing `Null` directly to `Where`, `OrWhere`, `WhereNot` or `OrWhereNot` compares with NULL as SqlKata does: with `=` or no operator it emits `IS NULL`, and with any other operator `IS NOT NULL`, with no binding. `WhereNot` keeps its negation, where SqlKata drops it:

```pascal
.Where('deleted_at', Null)         // → WHERE deleted_at IS NULL
.Where('deleted_at', '<>', Null)   // → WHERE deleted_at IS NOT NULL
.WhereNot('deleted_at', Null)      // → WHERE NOT (deleted_at IS NULL)
```

---

## Boolean Shorthand

```pascal
.WhereTrue('active')    // → WHERE active = true
.WhereFalse('blocked')  // → WHERE blocked = false
```

The comparison is with the dialect's literal, with no binding: `true` and `false` in ANSI, Postgres, MySQL and Oracle, `1` and `0` in SQLite and Firebird, and `cast(1 as bit)` and `cast(0 as bit)` in SQL Server — see [Dialect compilers](dialects.md). `Where`, `OrWhere`, `WhereNot` and `OrWhereNot` with a logical value emit the same: with `=` or no operator, the comparison with the literal; with any other operator, its negation, which adds to that of `WhereNot`:

```pascal
.Where('active', True)          // → WHERE active = true
.Where('active', '<>', False)   // → WHERE NOT (active = false)
```

---

## BETWEEN

```pascal
.WhereBetween('price', 10, 100)
// → WHERE price BETWEEN ? AND ?   Bindings: [10, 100]

.WhereNotBetween('age', 13, 17)
.OrWhereBetween('score', 90, 100)
```

---

## WHERE IN

```pascal
.WhereIn('status', ['pending', 'processing', 'shipped'])
// → WHERE status IN (?, ?, ?)

.WhereNotIn('id', [1, 2, 3])
.OrWhereIn('category_id', [10, 20])
.OrWhereNotIn('tag', ['spam', 'bot'])
```

### The empty value set

A computed set can come out empty, and `IN` over nothing has an answer: `x IN (a, b)`
is `x = a OR x = b`, so an empty list is the empty disjunction — **false without
consulting the column**, and true for `NOT IN`.

The four verbs resolve that case where the condition is added, so no `IN ()` is ever
emitted:

```pascal
.WhereIn('price', [])
// → WHERE (price IS NULL AND price IS NOT NULL)      matches no row

.WhereNotIn('price', [])
// → WHERE NOT (price IS NULL AND price IS NOT NULL)  matches every row
```

`IS NULL` and `IS NOT NULL` are the only predicates in the model that never yield
*unknown*: exactly one of them holds for any value. That is what makes the group false
for every row **including one whose column is NULL**, where `price <> price` or
`price = NULL` would yield *unknown* and quietly drop the row. It also names the column,
so a logged statement points back at the call that produced it — which `1 = 0` would not.

Both back-ends read the same clauses, so the SQL lane and the in-memory lane answer
identically.

---

## WHERE EXISTS (subquery)

```pascal
var Sub := TQuery.New.From('order_items').Where('order_id', '=', 'orders.id');

TQuery.New.From('orders')
  .WhereExists(Sub)
// → WHERE EXISTS (SELECT * FROM order_items WHERE order_id = ?)
```

---

## WHERE IN (subquery)

```pascal
var ActiveUsers := TQuery.New.From('users').Select('id').Where('active', True);

TQuery.New.From('orders')
  .WhereInQuery('customer_id', ActiveUsers)
// → WHERE customer_id IN (SELECT id FROM users WHERE active = ?)
```

---

## WhereRaw

Append a raw SQL fragment (AND connector). Use with caution — no escaping applied:

```pascal
.WhereRaw('created_at > NOW() - INTERVAL ''7 days''')
```

---

## Grouped WHERE (Callback)

Wrap conditions in parentheses using a callback:

```pascal
TQuery.New.From('products')
  .Where('active', True)
  .Where(function(Q: IQuery): IQuery
    begin
      Result := Q.Where('stock', '>', 0)
                 .OrWhere('backorder_allowed', True);
    end)
// → WHERE active = ? AND (stock > ? OR backorder_allowed = ?)
```

OR-connected group:

```pascal
.OrWhere(function(Q: IQuery): IQuery
  begin
    Result := Q.Where('role', 'admin').OrWhere('role', 'moderator');
  end)
// → OR (role = ? OR role = ?)
```

---

## Negated Grouped WHERE (Callback)

`WhereNot` and `OrWhereNot` also accept a callback. The group is built exactly as
with `Where` / `OrWhere`, then wrapped in `NOT (…)`:

```pascal
TQuery.New.From('orders')
  .Where('active', True)
  .WhereNot(function(Q: IQuery): IQuery
    begin
      Result := Q.Where('status', 'cancelled')
                 .Where('refunded', True);
    end)
// → WHERE active = ? AND NOT (status = ? AND refunded = ?)
```

OR-connected group:

```pascal
.OrWhereNot(function(Q: IQuery): IQuery
  begin
    Result := Q.Where('role', 'guest').OrWhere('banned', True);
  end)
// → OR NOT (role = ? OR banned = ?)
```

> ⚠️ **The negation applies to the group as a whole, not to each condition.**
> `NOT (status = ? AND refunded = ?)` matches whenever *either* inner condition
> fails — a cancelled order that was never refunded still matches. If you meant
> "neither of them", negate each condition instead:
> `.WhereNot('status', 'cancelled').WhereNot('refunded', True)`.

The negation applies to its own subtree only: sibling conditions and the
enclosing group are untouched.

```pascal
.WhereNot(function(Q: IQuery): IQuery
  begin
    Result := Q.Where('a', 1);
  end)
.Where('b', 2)
// → WHERE NOT (a = ?) AND b = ?     — b is not negated
```

To negate a **single** condition, use the leaf form. It is the idiomatic way and
produces the same SQL as a one-element group:

```pascal
.WhereNot('role', 'guest')          // → WHERE NOT (role = ?)   ← idiomatic
```

A negated group whose callback adds no condition is omitted from the query, just
like an empty unnegated group — `NOT ()` is never emitted:

```pascal
.Where('a', 1)
.WhereNot(function(Q: IQuery): IQuery begin Result := Q end)
// → WHERE a = ?
```

Under SQL three-valued logic, a negated group containing a comparison against
`NULL` evaluates to *unknown*, so the row is **excluded** — the same rule `NOT`
already follows for a single condition. The in-memory back-end applies it
identically.

---

## Precedence, and the tree behind it

`AND` binds tighter than `OR`. A flat chain of conditions is not read left to right: consecutive
`AND` runs are grouped first, and those groups are then `OR`-ed.

```pascal
Query.From('users')
  .Where('active', '=', True)
  .Where('age', '>', 18)
  .OrWhere('role', '=', 'admin')
// -> WHERE (active = ? AND age > ?) OR role = ?
```

The parentheses in that SQL are **emitted**, not assumed. Each condition carries the connector that
joins it with the previous one, and `Daf.SQLCute.WhereTree` normalizes that flat list into a
precedence-correct boolean tree before anything is compiled:

```pascal
uses Daf.SQLCute.WhereTree;

var Tree := TWhereTree.Build(Query.Clauses);
try
  // Tree.Kind is TWhereNodeKind.Disjunction for the query above
finally
  Tree.Free;
end;
```

| Member | Answers |
|--------|---------|
| `TWhereTree.Build(Clauses)` | The tree of the WHERE-relevant clauses. `nil` when there are none. **The caller owns it and frees it** |
| `TWhereTree.BuildHaving(Clauses)` | The same normalization over `THavingClause`. HAVING has no nesting and no `NOT`, so the tree is a flat AND/OR grouping |
| `TWhereNode.Kind` | `TWhereNodeKind.Leaf`, `.Neg`, `.Conjunction` or `.Disjunction` — the unit compiles with scoped enums |
| `TWhereNode.Clause` | The condition, on a `Leaf`. **Not owned**: it points back into the query's AST |
| `TWhereNode.Children` | The operands of a conjunction or disjunction, or the single child of a `Neg`. Owned, and freed recursively |

A grouped callback recurses as a nested node, and a negated one wraps its node in a `Neg`.

**Both back-ends consume the same tree, and that is the point.** The ANSI compiler walks it and
writes the parentheses out, so precedence stops depending on how a particular database parses; the
in-memory compiler builds its three-valued predicate from it. Two engines that read one tree cannot
disagree about precedence — see [SQLCute.InMemory](../../../SQLCute.InMemory/README.md).

The walk is depth-first, left to right, which preserves the original order of the conditions. That
is what lets a consumer emit positional bindings while walking and get the same order the flat list
produced.

---

## WhereColumns

Column-to-column comparison (no binding — values are emitted verbatim):

```pascal
.WhereColumns('start_date', '<', 'end_date')
// → WHERE start_date < end_date

.OrWhereColumns('price', '=', 'list_price')
```

Its operator is one of `=`, `<>`, `!=`, `<`, `<=`, `>`, `>=`, `like` and `not like`; `in` and `not in` raise `EArgumentException`.

> ⚠️ Only pass developer-controlled column names to `WhereColumns`. Never pass user input.

---

## When — Conditional Chaining

Apply conditions only when a runtime flag is true:

```pascal
var IncludeInactive := False;

TQuery.New.From('users')
  .When(not IncludeInactive, function(Q: IQuery): IQuery
    begin
      Result := Q.Where('active', True);
    end)
// → WHERE active = ?   (only when IncludeInactive is False)
```

With an else branch:

```pascal
.When(IsAdmin,
  function(Q: IQuery): IQuery begin Result := Q; end,        // no extra filter
  function(Q: IQuery): IQuery begin Result := Q.Where('tenant_id', TenantId); end)
```

---

## String / LIKE Conditions

See [String Operations](string-ops.md) for `WhereLike`, `WhereStarts`, `WhereEnds`, `WhereContains`.

---

## Date Conditions

See [Date Operations](date-ops.md) for `WhereDate`, `WhereTime`, `WhereDatePart`.
