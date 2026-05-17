# SQLCute — Fluent SQL Query Builder for Delphi

**🌍 Language: English | [Español](README.es.md)**

Build SQL queries with a clean, type-safe fluent API. No string concatenation, no raw SQL juggling — just method chains that compile to parameterized SQL.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Concepts

| Concept | Description |
|---------|-------------|
| `IQuery` | Fluent builder interface — every method returns `IQuery` for chaining |
| `TQuery.New` | Factory entry point — creates a ref-counted query, no `Free` needed |
| `TSQLResult` | Output: `SQL` string + `Bindings` array of positional parameters |
| `IQueryCompiler` | Strategy for dialect SQL generation (ANSI default, or a dialect compiler) |

---

## Quick Start

```pascal
uses Daf.SQLCute;

var Q := TQuery.New
  .From('orders')
  .Select(['id', 'total', 'status'])
  .Where('status', 'pending')
  .Where('total', '>', 100)
  .OrderByDesc('created_at')
  .Limit(20);

var R := TAnsiSqlCompiler.Create.Compile(Q);
// R.SQL      → 'SELECT id, total, status FROM orders WHERE status = ? AND total > ? ORDER BY created_at DESC LIMIT 20'
// R.Bindings → ['pending', 100]
```

---

## Common Patterns

### Filter and paginate

```pascal
TQuery.New
  .From('users')
  .Where('active', True)
  .ForPage(Page, 25)   // LIMIT 25 OFFSET (Page-1)*25
```

### JOIN

```pascal
TQuery.New
  .From('orders')
  .Join('customers', 'orders.customer_id', 'customers.id')
  .Select(['orders.id', 'customers.name'])
```

### Aggregate

```pascal
TQuery.New
  .From('sales')
  .GroupBy('product_id')
  .SelectCount('*', 'qty')
  .SelectSum('amount', 'total')
  .Having('count(*)', '>', 10)
```

### DML

```pascal
// INSERT
TQuery.New.From('logs')
  .AsInsert(['level', 'message'], ['error', 'disk full']);

// UPDATE
TQuery.New.From('users')
  .Where('id', 42)
  .AsUpdate(['email'], ['new@example.com']);

// DELETE
TQuery.New.From('sessions')
  .Where('expired', True)
  .AsDelete;
```

### Dialect compiler

```pascal
uses Daf.SQLCute.Compiler.Postgres;

var R := TPostgresCompiler.Create.Compile(Q);
// identifiers quoted with "", placeholders $1 $2 …
```

---

## Documentation

| Resource | Description |
|----------|-------------|
| [**Usage Guide**](docs/GUIDE.md) | Complete API reference with all methods |
| [SELECT & FROM](docs/guide/select.md) | Columns, DISTINCT, aggregates, ORDER BY, LIMIT |
| [WHERE](docs/guide/where.md) | Conditions, IN, EXISTS, groups, LIKE, date ops |
| [JOINs](docs/guide/joins.md) | All join types and callbacks |
| [GROUP BY / HAVING](docs/guide/groupby.md) | Aggregation and HAVING |
| [Set Operations](docs/guide/setops.md) | UNION, INTERSECT, EXCEPT, pagination |
| [DML](docs/guide/dml.md) | INSERT, UPDATE, DELETE |
| [Subqueries & CTEs](docs/guide/subqueries.md) | Derived tables, WITH, recursive CTEs |
| [String Operations](docs/guide/string-ops.md) | LIKE, starts/ends/contains |
| [Date Operations](docs/guide/date-ops.md) | WhereDate, dialect casting |
| [Dialect Compilers](docs/guide/dialects.md) | Postgres, MySQL, SQLite, SQL Server, Oracle, Firebird |
| [**Compilers Package**](../SQLCute.Compilers/README.md) | Dialect compiler classes |
