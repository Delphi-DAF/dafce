# String Operations

**🌍 Language: English | [Español](string-ops.es.md)**

← [Back to Guide](../GUIDE.md)

---

## Overview

All string-match methods wrap the value in `LIKE` patterns. By default (`CaseSensitive = False`) SQLCute wraps the column in `LOWER(…)` and lower-cases the pattern, which travels as a binding. Pass `True` as the last argument to emit a plain `LIKE` with the pattern as you wrote it.

---

## WhereLike — Raw Pattern

Supply the full LIKE pattern yourself:

```pascal
.WhereLike('name', '%John%')
// Case-insensitive (default):
// → WHERE LOWER(name) LIKE ?      binding: '%john%'

.WhereLike('code', 'ABC-%', True)
// Case-sensitive:
// → WHERE code LIKE ?             binding: 'ABC-%'
```

Variants: `WhereNotLike`, `OrWhereLike`, `OrWhereNotLike`.

---

## WhereStarts — Prefix Match

```pascal
.WhereStarts('email', 'admin')
// → WHERE LOWER(email) LIKE ?     binding: 'admin%'
```

Variants: `WhereNotStarts`, `OrWhereStarts`, `OrWhereNotStarts`.

---

## WhereEnds — Suffix Match

```pascal
.WhereEnds('filename', '.pdf')
// → WHERE LOWER(filename) LIKE ?  binding: '%.pdf'
```

Variants: `WhereNotEnds`, `OrWhereEnds`, `OrWhereNotEnds`.

---

## WhereContains — Substring Match

```pascal
.WhereContains('description', 'urgent')
// → WHERE LOWER(description) LIKE ?   binding: '%urgent%'
```

Variants: `WhereNotContains`, `OrWhereContains`, `OrWhereNotContains`.

---

## Combining String Filters

```pascal
TQuery.New.From('products')
  .Where(function(Q: IQuery): IQuery
    begin
      Result := Q.WhereContains('name', 'coffee')
                 .OrWhereContains('tags', 'coffee');
    end)
  .WhereNotStarts('sku', 'DISC-')
// → WHERE (LOWER(name) LIKE ? OR LOWER(tags) LIKE ?)
//     AND LOWER(sku) NOT LIKE ?
//   bindings: '%coffee%', '%coffee%', 'disc-%'
```

Chaining the two `OR` terms without that group means something else: a flat list is read as OR-of-AND-runs, so `WhereContains … OrWhereContains … WhereNotStarts` groups as `name OR (tags AND sku)`. Nest the alternation you mean.

---

## Dialect Notes on Case Sensitivity

> The `LOWER`-wrapping approach works reliably across all SQL dialects, but each database has its own native LIKE behavior:

| Dialect | LIKE case-sensitive? | Notes |
|---------|---------------------|-------|
| ANSI / ANSI SQL | Yes | Standard behavior |
| PostgreSQL | Yes | Use `ILIKE` natively; SQLCute uses `LOWER(…)` instead |
| MySQL | No (by default) | `LOWER` wrapping is harmless |
| SQLite | No for ASCII | Unicode chars are case-sensitive unless ICU extension enabled |
| SQL Server | Depends on collation | `Latin1_General_CI_AS` → case-insensitive |
| Oracle | Yes | `LOWER` wrapping recommended |
| Firebird | Yes | `LOWER` wrapping recommended |

When `CaseSensitive = True`, no `LOWER` wrapping is emitted regardless of dialect.
