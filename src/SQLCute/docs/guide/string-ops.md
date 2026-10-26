# String Operations

**🌍 Language: English | [Español](string-ops.es.md)**

← [Back to Guide](../GUIDE.md)

---

## Overview

All string-match methods wrap the value in `LIKE` patterns. By default (`CaseSensitive = False`) SQLCute wraps the column in `LOWER(…)` and lower-cases the pattern, which travels as a binding. Pass `True` as `CaseSensitive`, the argument after the value, to emit a plain `LIKE` with the pattern as you wrote it.

The wildcards `%` and `_` inside the value stay active in every method: `WhereContains('code', '50%')` matches any code that contains `50`. To match them literally, declare an [escape character](#escape-character).

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

## Escape Character

Every method of the family takes an optional escape character after `CaseSensitive`. The character that follows it in the value is literal, and the SQL adds `ESCAPE '<c>'`:

```pascal
.WhereContains('code', '50!%', False, '!')
// → WHERE LOWER(code) LIKE ? ESCAPE '!'   binding: '%50!%%'
```

SQLCute escapes nothing on its own: the wildcards it adds around the value stay wildcards, and only what the escape character precedes is literal. A single quote or a `?` as escape character raise `EArgumentException`.

With `CaseSensitive = False` the pattern is lower-cased and the escape character is not, so pick one with no case, such as `!`. The in-memory back-end reads the escape character the same way.

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

The pattern is lower-cased across all of Unicode, so `WhereLike('name', 'ÑANDÚ%')` binds `'ñandú%'`. The column is lower-cased by the engine's `LOWER`, and SQLite's folds only ASCII letters: on SQLite, `WhereStarts('name', 'ñan')` does not match a stored `Ñandú`. The in-memory back-end folds both sides across Unicode.

Each database also has its own native LIKE behavior:

| Dialect | LIKE case-sensitive? | Notes |
|---------|---------------------|-------|
| ANSI / ANSI SQL | Yes | Standard behavior |
| PostgreSQL | Yes | Use `ILIKE` natively; SQLCute uses `LOWER(…)` instead |
| MySQL | No (by default) | `LOWER` wrapping is harmless |
| SQLite | No for ASCII | Non-ASCII letters are case-sensitive, and its `LOWER` folds only ASCII, unless the ICU extension is loaded |
| SQL Server | Depends on collation | `Latin1_General_CI_AS` → case-insensitive |
| Oracle | Yes | `LOWER` wrapping recommended |
| Firebird | Yes | `LOWER` wrapping recommended |

When `CaseSensitive = True`, no `LOWER` wrapping is emitted regardless of dialect.
