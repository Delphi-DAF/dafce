# SQLCute — Usage Guide

**🌍 Language: English | [Español](GUIDE.es.md)**

---

## Introduction

SQLCute builds parameterized SQL through a fluent method chain. The entry point is always `TQuery.New`:

```pascal
uses Daf.SQLCute, Daf.SQLCute.Compiler;

var Q := TQuery.New          // IQuery — ref-counted, no Free needed
  .From('products')
  .Where('status', 'active')
  .OrderBy('name')
  .Limit(50);

var Compiler: IQueryCompiler := TAnsiSqlCompiler.Create;  // ref-counted: it needs an owner
var R := Compiler.Compile(Q);
// R.SQL      → 'SELECT * FROM products WHERE status = ? ORDER BY name LIMIT 50'
// R.Bindings → ['active']
```

`TSQLResult` always contains:

| Field | Type | Description |
|-------|------|-------------|
| `SQL` | `string` | The compiled SQL string with `?` placeholders (dialect compilers may use `$1`, `:p1`, `@p0`) |
| `Bindings` | `TArray<Variant>` | Positional values matching the placeholders |

Pass `R.SQL` and `R.Bindings` to your database driver — SQLCute never executes queries itself.

---

## Table of Contents

1. [SELECT & FROM](guide/select.md) — columns, DISTINCT, aggregates, ORDER BY, LIMIT/OFFSET
2. [WHERE](guide/where.md) — equality, comparisons, NULL, BETWEEN, IN, EXISTS, groups, columns
3. [JOINs](guide/joins.md) — INNER, LEFT, RIGHT, CROSS, FULL OUTER, callbacks
4. [GROUP BY / HAVING](guide/groupby.md) — grouping and aggregate filters
5. [Set Operations](guide/setops.md) — UNION, INTERSECT, EXCEPT, pagination
6. [DML](guide/dml.md) — INSERT, UPDATE, DELETE
7. [Subqueries & CTEs](guide/subqueries.md) — derived tables, WITH, recursive CTEs
8. [String Operations](guide/string-ops.md) — LIKE, starts/ends/contains
9. [Date Operations](guide/date-ops.md) — WhereDate, per-dialect date casting
10. [Dialect Compilers](guide/dialects.md) — quoting, placeholders, dialect matrix

---

## The Compiler Pattern

Every `IQuery` is compiler-agnostic. You choose the dialect at compile time:

```pascal
uses Daf.SQLCute;                      // IQueryCompiler
uses Daf.SQLCute.Compiler;             // TAnsiSqlCompiler (default)
uses Daf.SQLCute.Compiler.Postgres;    // TPostgresCompiler
uses Daf.SQLCute.Compiler.MySql;       // TMySqlCompiler
// … etc.

var Compiler: IQueryCompiler := TPostgresCompiler.Create;  // ref-counted: it needs an owner
var R := Compiler.Compile(Q);
```

You can also call `Q.Compile(MyCompiler)` as a shorthand when you have a compiler instance.

---

## Clone — Query Variants

`Clone` creates a deep copy, useful for building related queries from a shared base:

```pascal
var Base := TQuery.New.From('orders').Where('year', 2024);

var Pending  := Base.Clone.Where('status', 'pending');
var Shipped  := Base.Clone.Where('status', 'shipped');
```

---

## Null Safety

All `Where*` methods that take a `Variant` accept `Null` values. When the value is `Null`, the generated SQL uses `IS NULL` with `=` or no operator, and `IS NOT NULL` with any other, with no binding; `WhereNot` keeps its negation.

```pascal
.Where('deleted_at', Null)      // → WHERE deleted_at IS NULL
.WhereNot('deleted_at', Null)   // → WHERE NOT (deleted_at IS NULL)
```

---

## Differences from SqlKata

SQLCute follows the builder and the compilers of [SqlKata](https://sqlkata.com/). These are the places where the SQL it emits, or what it accepts, differs:

| Case | SqlKata | SQLCute |
|------|---------|---------|
| Case-insensitive LIKE on Postgres | `ILIKE` | `LOWER(column) LIKE ?`, as in every dialect |
| Placeholders | `@p0`, `@p1`… in every dialect, and `:p0`… on Oracle | `$1`… on Postgres, `@p0`… on SQL Server, `:p1`… on Oracle, `?` in the others |
| Base compiler | quotes identifiers with `"` | `TAnsiSqlCompiler` quotes nothing; each dialect compiler quotes with its own character |
| `WhereIn` with an empty set | `1 = 0` | `col IS NULL AND col IS NOT NULL`, which names the column |
| `Where(col, 'in', values)` | rejected | binds the values, like `WhereIn` |
| `WhereNot` with `Null` or a logical value | drops the negation | keeps it: `NOT (col IS NULL)` |
| A negated comparison with a logical value | `col != true` | `NOT (col = true)` |
| Escape character of the LIKE family | any character | a single quote or a `?` raise `EArgumentException` |
| `\?` in a raw expression | stays `\?` in the SQL | becomes a literal `?` |

---

## See Also

- [Sample Application](../samples/SQLCuteSample/README.md)
