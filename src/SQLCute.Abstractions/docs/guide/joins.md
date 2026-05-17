# JOINs

**🌍 Language: English | [Español](joins.es.md)**

← [Back to Guide](../GUIDE.md)

---

## Basic Joins

All join methods follow the same signature: `JoinType(table, col1, col2, op)`. The default operator is `=`.

### INNER JOIN

```pascal
TQuery.New.From('orders')
  .Join('customers', 'orders.customer_id', 'customers.id')
// → SELECT * FROM orders INNER JOIN customers ON orders.customer_id = customers.id
```

### LEFT JOIN

```pascal
.LeftJoin('addresses', 'users.id', 'addresses.user_id')
// → LEFT JOIN addresses ON users.id = addresses.user_id
```

### RIGHT JOIN

```pascal
.RightJoin('departments', 'employees.dept_id', 'departments.id')
// → RIGHT JOIN departments ON employees.dept_id = departments.id
```

### CROSS JOIN

```pascal
.CrossJoin('sizes')
// → CROSS JOIN sizes
```

### FULL OUTER JOIN

```pascal
.FullOuterJoin('b', 'a.id', 'b.a_id')
// → FULL OUTER JOIN b ON a.id = b.a_id
```

---

## Join with Raw Condition String

Pass a raw SQL ON clause when the condition cannot be expressed as column=column:

```pascal
.Join('prices', 'prices.product_id = products.id AND prices.active = 1')
// → INNER JOIN prices ON prices.product_id = products.id AND prices.active = 1
```

---

## Join with Callback (Complex ON)

Use a callback for multiple conditions joined with AND/OR:

```pascal
TQuery.New.From('orders')
  .Join('order_items', function(Q: IQuery): IQuery
    begin
      Result := Q
        .Where('order_items.order_id', '=', 'orders.id')
        .OrWhere('order_items.alt_order_id', '=', 'orders.id');
    end)
// → INNER JOIN order_items ON (order_items.order_id = orders.id
//                               OR order_items.alt_order_id = orders.id)
```

`LeftJoin`, `RightJoin`, and `FullOuterJoin` all support the same callback overload.

---

## Subquery Join

```pascal
var Sub := TQuery.New.From('order_items')
  .Select('order_id')
  .SelectSum('qty', 'total_qty')
  .GroupBy('order_id');

TQuery.New.From('orders')
  .Join(Sub, 'oi', function(Q: IQuery): IQuery
    begin
      Result := Q.Where('oi.order_id', '=', 'orders.id');
    end)
// → INNER JOIN (SELECT order_id, SUM(qty) AS total_qty
//               FROM order_items GROUP BY order_id) oi
//   ON oi.order_id = orders.id
```

---

## Multiple Joins

Chain as many joins as needed — they are emitted in declaration order:

```pascal
TQuery.New.From('invoices')
  .Join('customers', 'invoices.customer_id', 'customers.id')
  .LeftJoin('discounts', 'invoices.discount_id', 'discounts.id')
  .Select(['invoices.id', 'customers.name', 'discounts.pct'])
```
