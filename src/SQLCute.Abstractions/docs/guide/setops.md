# Set Operations & Pagination

**🌍 Language: English | [Español](setops.es.md)**

← [Back to Guide](../GUIDE.md)

---

## UNION

```pascal
var Q1 := TQuery.New.From('active_users').Select(['id', 'name']);
var Q2 := TQuery.New.From('archived_users').Select(['id', 'name']);

Q1.Union(Q2)
// → SELECT id, name FROM active_users
//   UNION
//   SELECT id, name FROM archived_users
```

`UNION ALL` (keeps duplicates):

```pascal
Q1.UnionAll(Q2)
// → … UNION ALL …
```

---

## INTERSECT / EXCEPT

```pascal
Q1.Intersect(Q2)
// → … INTERSECT …

Q1.&Except(Q2)    // & prefix avoids conflict with Delphi keyword
// → … EXCEPT …

Q1.IntersectAll(Q2)
Q1.ExceptAll(Q2)
```

---

## CombineRaw

Append a raw set-operation string:

```pascal
Q1.CombineRaw('MINUS SELECT id, name FROM deleted_users')
// → … MINUS SELECT id, name FROM deleted_users
```

---

## Pagination

### ForPage (recommended)

1-based page number, configurable page size:

```pascal
TQuery.New.From('products')
  .Where('active', True)
  .OrderBy('name')
  .ForPage(3, 25)      // page 3, 25 items per page
// → … LIMIT 25 OFFSET 50
```

Default page size is 15:

```pascal
.ForPage(2)   // → LIMIT 15 OFFSET 15
```

### Manual LIMIT / OFFSET

```pascal
.Limit(20).Offset(40)
// → LIMIT 20 OFFSET 40

// Aliases
.Take(20).Skip(40)
```

### Typical pagination loop

```pascal
const PageSize = 50;
var Page := 1;

repeat
  var R := Compiler.Compile(
    TQuery.New.From('orders').OrderBy('id').ForPage(Page, PageSize));
  // execute R, process rows…
  Inc(Page);
until RowCount < PageSize;
```
