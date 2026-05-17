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

`Having` filters groups after aggregation. The column argument accepts any aggregate expression.

```pascal
TQuery.New.From('orders')
  .GroupBy('customer_id')
  .SelectCount('*', 'order_count')
  .SelectSum('total', 'revenue')
  .Having('count(*)', '>', 5)
  .Having('sum(total)', '>=', 1000)
// → SELECT COUNT(*) AS order_count, SUM(total) AS revenue
//   FROM orders
//   GROUP BY customer_id
//   HAVING count(*) > ? AND sum(total) >= ?
//   Bindings: [5, 1000]
```

OR connector:

```pascal
.Having('count(*)', '>', 10)
// not yet exposed as OrHaving — use HavingRaw for OR
.HavingRaw('sum(amount) > 500 OR count(*) > 100')
```

Raw HAVING expression:

```pascal
.HavingRaw('AVG(response_time) < 200')
```

---

## Combining with WHERE

`WHERE` filters rows before grouping; `HAVING` filters groups after:

```pascal
TQuery.New.From('logs')
  .Where('level', 'error')          // applied before GROUP BY
  .GroupBy('service')
  .SelectCount('*', 'errors')
  .Having('count(*)', '>', 50)      // applied after GROUP BY
// → SELECT COUNT(*) AS errors FROM logs
//   WHERE level = ?
//   GROUP BY service
//   HAVING count(*) > ?
```
