# Subqueries & CTEs

**🌍 Language: English | [Español](subqueries.es.md)**

← [Back to Guide](../GUIDE.md)

---

## Derived Table (FROM subquery)

Use an `IQuery` as the FROM source with an alias:

```pascal
var Inner := TQuery.New.From('order_items')
  .Select('order_id')
  .SelectSum('qty', 'total_qty')
  .GroupBy('order_id');

TQuery.New
  .From(Inner, 'oi')
  .Select(['oi.order_id', 'oi.total_qty'])
  .Where('oi.total_qty', '>', 10)
// → SELECT oi.order_id, oi.total_qty
//   FROM (SELECT order_id, SUM(qty) AS total_qty
//         FROM order_items GROUP BY order_id) oi
//   WHERE oi.total_qty > ?
```

---

## FROM Raw

Use a raw SQL expression as the FROM source (e.g. table-valued functions):

```pascal
TQuery.New
  .FromRaw('generate_series(1, 100) AS s(n)', [])
  .Select('n')
// → SELECT n FROM generate_series(1, 100) AS s(n)
```

With bindings:

```pascal
.FromRaw('fn_get_events(?, ?) AS e', [StartDate, EndDate])
```

---

## Subquery in WHERE IN

```pascal
var RecentOrders := TQuery.New.From('orders')
  .Select('customer_id')
  .Where('created_at', '>', LastWeek);

TQuery.New.From('customers')
  .WhereInQuery('id', RecentOrders)
// → SELECT * FROM customers
//   WHERE id IN (SELECT customer_id FROM orders WHERE created_at > ?)
```

Variants: `WhereNotInQuery`, `OrWhereInQuery`, `OrWhereNotInQuery`.

---

## Subquery in WHERE EXISTS

```pascal
TQuery.New.From('orders')
  .WhereExists(
    TQuery.New.From('payments')
      .Where('order_id', '=', 'orders.id')
      .Where('status', 'complete'))
// → WHERE EXISTS (SELECT * FROM payments
//                WHERE order_id = orders.id AND status = ?)
```

---

## Scalar Subquery in SELECT

```pascal
var ItemCount := TQuery.New.From('order_items')
  .SelectCount('*')
  .Where('order_id', '=', 'orders.id');

TQuery.New.From('orders')
  .Select(ItemCount, 'item_count')
// → SELECT (SELECT COUNT(*) FROM order_items WHERE order_id = orders.id)
//          AS item_count FROM orders
```

---

## WITH (CTE)

```pascal
var RecentSales := TQuery.New.From('sales')
  .Where('year', 2024)
  .Select(['product_id', 'amount']);

TQuery.New
  .&With('recent', RecentSales)
  .From('recent')
  .SelectSum('amount', 'total')
// → WITH recent AS (SELECT product_id, amount FROM sales WHERE year = ?)
//   SELECT SUM(amount) AS total FROM recent
```

---

## RECURSIVE CTE

```pascal
var Anchor := TQuery.New.From('categories')
  .Where('parent_id', Null);
var Recursive := TQuery.New.From('categories')
  .Join('tree', 'categories.parent_id', 'tree.id');

TQuery.New
  .WithRecursive('tree', Anchor.Union(Recursive))
  .From('tree')
// → WITH RECURSIVE tree AS (
//     SELECT * FROM categories WHERE parent_id IS NULL
//     UNION
//     SELECT categories.* FROM categories INNER JOIN tree ON categories.parent_id = tree.id
//   ) SELECT * FROM tree
```

---

## WITH Raw

```pascal
TQuery.New
  .WithRaw('cte_name', 'SELECT id FROM legacy_table WHERE flag = ?', [1])
  .From('cte_name')
```

---

## Nesting Depth

SQLCute supports arbitrary nesting — subqueries can themselves contain subqueries. Bindings are flattened into a single array in the order they appear in the compiled SQL.
