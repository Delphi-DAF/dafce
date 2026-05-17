# DML — INSERT / UPDATE / DELETE

**🌍 Language: English | [Español](dml.es.md)**

← [Back to Guide](../GUIDE.md)

---

## INSERT — Single Row

Pass parallel arrays of column names and values:

```pascal
TQuery.New.From('users')
  .AsInsert(
    ['name',    'email',             'role'],
    ['Alice',   'alice@example.com', 'admin'])
// → INSERT INTO users (name, email, role) VALUES (?, ?, ?)
//   Bindings: ['Alice', 'alice@example.com', 'admin']
```

---

## INSERT — Multiple Rows

```pascal
TQuery.New.From('tags')
  .AsInsertRows(
    ['name', 'color'],
    [['Urgent', 'red'],
     ['Normal', 'green'],
     ['Low',    'gray']])
// → INSERT INTO tags (name, color) VALUES (?, ?), (?, ?), (?, ?)
//   Bindings: ['Urgent', 'red', 'Normal', 'green', 'Low', 'gray']
```

---

## INSERT … SELECT

Copy rows from one table to another using a subquery:

```pascal
var Sub := TQuery.New.From('temp_users')
  .Select(['name', 'email'])
  .Where('imported', True);

TQuery.New.From('users')
  .AsInsertFrom(['name', 'email'], Sub)
// → INSERT INTO users (name, email)
//   SELECT name, email FROM temp_users WHERE imported = ?
```

---

## UPDATE

```pascal
TQuery.New.From('products')
  .Where('id', 42)
  .AsUpdate(
    ['price', 'stock'],
    [29.99,   100])
// → UPDATE products SET price = ?, stock = ? WHERE id = ?
//   Bindings: [29.99, 100, 42]
```

> ⚠️ Without a `Where` clause, ALL rows are updated. Add a WHERE condition to scope the update.

---

## DELETE

```pascal
TQuery.New.From('sessions')
  .Where('expired', True)
  .AsDelete
// → DELETE FROM sessions WHERE expired = ?
```

Soft-delete pattern using UPDATE:

```pascal
TQuery.New.From('users')
  .Where('id', UserId)
  .AsUpdate(['deleted_at'], [Now])
// → UPDATE users SET deleted_at = ? WHERE id = ?
```

---

## Combining DML with Subqueries

```pascal
// Delete orders older than 1 year whose items are all shipped
TQuery.New.From('orders')
  .Where('created_at', '<', EncodeDate(Year - 1, Month, Day))
  .WhereNotExists(
    TQuery.New.From('order_items')
      .Where('order_id', '=', 'orders.id')
      .WhereNot('status', 'shipped'))
  .AsDelete
```
