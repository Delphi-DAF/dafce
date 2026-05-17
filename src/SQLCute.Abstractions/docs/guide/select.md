# SELECT & FROM

**🌍 Language: English | [Español](select.es.md)**

← [Back to Guide](../GUIDE.md)

---

## From

Sets the source table. All queries start here.

```pascal
TQuery.New.From('orders')
// → SELECT * FROM orders
```

With alias:

```pascal
TQuery.New.From('orders', 'o')
// → SELECT * FROM orders o
```

---

## Select — Column List

```pascal
TQuery.New.From('users')
  .Select(['id', 'name', 'email'])
// → SELECT id, name, email FROM users
```

Single column:

```pascal
.Select('name')
// → SELECT name FROM users
```

With alias:

```pascal
.SelectAs('full_name', 'name')
// → SELECT full_name AS name FROM users
```

---

## Select * (default)

When no `Select` call is made, `SELECT *` is emitted:

```pascal
TQuery.New.From('products')
// → SELECT * FROM products
```

---

## Distinct

```pascal
TQuery.New.From('orders')
  .Select('customer_id')
  .Distinct
// → SELECT DISTINCT customer_id FROM orders
```

---

## SelectRaw

Emit a raw SQL expression in the SELECT list (no quoting or escaping applied):

```pascal
TQuery.New.From('sales')
  .SelectRaw('YEAR(created_at) AS year')
  .SelectRaw('COUNT(*) AS total')
// → SELECT YEAR(created_at) AS year, COUNT(*) AS total FROM sales
```

---

## Aggregate Functions

| Method | Default alias | SQL generated |
|--------|--------------|---------------|
| `SelectCount(col, alias)` | `'count'` | `COUNT(col) AS alias` |
| `SelectSum(col, alias)` | `'sum'` | `SUM(col) AS alias` |
| `SelectAvg(col, alias)` | `'avg'` | `AVG(col) AS alias` |
| `SelectMin(col, alias)` | `'min'` | `MIN(col) AS alias` |
| `SelectMax(col, alias)` | `'max'` | `MAX(col) AS alias` |

```pascal
TQuery.New.From('orders')
  .SelectCount('*', 'total_orders')
  .SelectSum('amount', 'revenue')
  .SelectAvg('amount', 'avg_order')
  .GroupBy('status')
// → SELECT COUNT(*) AS total_orders, SUM(amount) AS revenue, AVG(amount) AS avg_order
//   FROM orders GROUP BY status
```

---

## Subquery as Column

```pascal
var Sub := TQuery.New.From('order_items')
  .SelectSum('qty')
  .Where('order_id', '=', TQuery.New.From('...'));  // conceptual — use as scalar subquery

TQuery.New.From('orders')
  .Select(Sub, 'item_count')
// → SELECT (SELECT SUM(qty) FROM order_items) AS item_count FROM orders
```

---

## ORDER BY

```pascal
.OrderBy('name')          // ASC
.OrderByDesc('created_at') // DESC
.OrderByRaw('FIELD(status, ''pending'', ''shipped'', ''closed'')')
```

Multiple ORDER BY: call `OrderBy`/`OrderByDesc` multiple times:

```pascal
TQuery.New.From('products')
  .OrderBy('category')
  .OrderByDesc('price')
// → SELECT * FROM products ORDER BY category ASC, price DESC
```

---

## LIMIT and OFFSET

```pascal
TQuery.New.From('logs').Limit(100)
// → SELECT * FROM logs LIMIT 100

TQuery.New.From('logs').Limit(20).Offset(40)
// → SELECT * FROM logs LIMIT 20 OFFSET 40
```

`Take` and `Skip` are aliases for `Limit` and `Offset`:

```pascal
.Take(20).Skip(40)   // same as Limit(20).Offset(40)
```

For page-based pagination see [Set Operations](setops.md#pagination).
