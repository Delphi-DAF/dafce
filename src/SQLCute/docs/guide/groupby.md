# GROUP BY / HAVING

**🌍 Language: English | [Español](groupby.es.md)**

← [Back to Guide](../GUIDE.md)

---

## GROUP BY

```pascal
TQuery.New.From('sales')
  .SelectCount('*', 'qty')
  .GroupBy('product_id')
// → SELECT COUNT(*) AS qty FROM sales GROUP BY product_id
```

Multiple columns — call `GroupBy` multiple times or pass an array:

```pascal
.GroupBy(['year', 'month', 'category'])
// → GROUP BY year, month, category
```

Raw expression:

```pascal
.GroupByRaw('YEAR(created_at), MONTH(created_at)')
// → GROUP BY YEAR(created_at), MONTH(created_at)
```

---

## HAVING

A condition on an aggregate goes through `HavingRaw`, which every engine accepts:

```pascal
TQuery.New.From('orders')
  .GroupBy('customer_id')
  .SelectCount('*', 'order_count')
  .SelectSum('total', 'revenue')
  .HavingRaw('count(*) > 5')
  .HavingRaw('sum(total) >= 1000')
// → SELECT COUNT(*) AS order_count, SUM(total) AS revenue
//   FROM orders
//   GROUP BY customer_id
//   HAVING count(*) > 5 AND sum(total) >= 1000
```

`Having` compares a column with a bound value. The column names a grouped column or the alias of an output column, and is quoted like any other column of the dialect. Its operator is one of `=`, `<>`, `!=`, `<`, `<=`, `>`, `>=`, `like` and `not like`; `in` and `not in` raise `EArgumentException`, and an `IN` on an aggregate goes through `HavingRaw`. MySQL, SQLite and Oracle 23 accept the alias of an output column in `HAVING`; Postgres, SQL Server and Firebird reject it.

```pascal
.Having('order_count', '>', 5)
// Postgres → HAVING "order_count" > ?
```

OR connector:

```pascal
.Having('order_count', '>', 10)
.OrHaving('revenue', '>=', 1000)
// → HAVING order_count > ? OR revenue >= ?
```

Raw HAVING expression:

```pascal
.HavingRaw('AVG(response_time) < 200')
```

`Having` with `Null` compares the same way as `Where`: `IS NULL` for `=` and `IS NOT NULL` for any other operator, with no binding.

---

## Combining with WHERE

`WHERE` filters rows before grouping; `HAVING` filters groups after:

```pascal
TQuery.New.From('logs')
  .Where('level', 'error')          // applied before GROUP BY
  .GroupBy('service')
  .SelectCount('*', 'errors')
  .HavingRaw('count(*) > 50')       // applied after GROUP BY
// → SELECT COUNT(*) AS errors FROM logs
//   WHERE level = ?
//   GROUP BY service
//   HAVING count(*) > 50
```
