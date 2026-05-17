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

Passing `Null` directly to `Where` also generates `IS NULL`:

```pascal
.Where('deleted_at', Null)     // → WHERE deleted_at IS NULL
```

---

## Boolean Shorthand

```pascal
.WhereTrue('active')    // → WHERE active = TRUE
.WhereFalse('blocked')  // → WHERE blocked = FALSE
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

## WhereColumns

Column-to-column comparison (no binding — values are emitted verbatim):

```pascal
.WhereColumns('start_date', '<', 'end_date')
// → WHERE start_date < end_date

.OrWhereColumns('price', '=', 'list_price')
```

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
