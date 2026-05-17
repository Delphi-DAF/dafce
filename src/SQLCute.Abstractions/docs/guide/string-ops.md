# String Operations

**🌍 Language: English | [Español](string-ops.es.md)**

← [Back to Guide](../GUIDE.md)

---

## Overview

All string-match methods wrap the value in `LIKE` patterns. By default (`CaseSensitive = False`) SQLCute wraps both column and value in `LOWER(…)` to achieve case-insensitive matching. Pass `True` as the last argument to skip the `LOWER` wrapping.

---

## WhereLike — Raw Pattern

Supply the full LIKE pattern yourself:

```pascal
.WhereLike('name', '%john%')
// Case-insensitive (default):
// → WHERE LOWER(name) LIKE lower('%john%')

.WhereLike('code', 'ABC-%', True)
// Case-sensitive:
// → WHERE code LIKE 'ABC-%'
```

Variants: `WhereNotLike`, `OrWhereLike`, `OrWhereNotLike`.

---

## WhereStarts — Prefix Match

```pascal
.WhereStarts('email', 'admin')
// → WHERE LOWER(email) LIKE lower('admin%')
```

Variants: `WhereNotStarts`, `OrWhereStarts`, `OrWhereNotStarts`.

---

## WhereEnds — Suffix Match

```pascal
.WhereEnds('filename', '.pdf')
// → WHERE LOWER(filename) LIKE lower('%.pdf')
```

Variants: `WhereNotEnds`, `OrWhereEnds`, `OrWhereNotEnds`.

---

## WhereContains — Substring Match

```pascal
.WhereContains('description', 'urgent')
// → WHERE LOWER(description) LIKE lower('%urgent%')
```

Variants: `WhereNotContains`, `OrWhereContains`, `OrWhereNotContains`.

---

## Combining String Filters

```pascal
TQuery.New.From('products')
  .WhereContains('name', 'coffee')
  .OrWhereContains('tags', 'coffee')
  .WhereNotStarts('sku', 'DISC-')
// → WHERE (LOWER(name) LIKE lower('%coffee%')
//          OR LOWER(tags) LIKE lower('%coffee%'))
//     AND NOT (LOWER(sku) LIKE lower('DISC-%'))
```

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
