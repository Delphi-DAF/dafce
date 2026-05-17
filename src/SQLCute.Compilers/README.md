# SQLCute.Compilers — Dialect Compilers

**🌍 Language: English | [Español](README.es.md)**

Dialect-specific SQL compilers for [SQLCute](../SQLCute.Abstractions/README.md). Each compiler extends the ANSI base with database-native identifier quoting, parameter placeholders, date casting, and pagination.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Available Compilers

| Class | Unit | Quote | Placeholder | Notes |
|-------|------|-------|-------------|-------|
| `TPostgresCompiler` | `Daf.SQLCute.Compiler.Postgres` | `"` | `$1`, `$2`… | `::date` casting |
| `TMySqlCompiler` | `Daf.SQLCute.Compiler.MySql` | `` ` `` | `?` | `DATE(col)` casting |
| `TSQLiteCompiler` | `Daf.SQLCute.Compiler.SQLite` | `"` | `?` | `date(col)` / `strftime` |
| `TSqlServerCompiler` | `Daf.SQLCute.Compiler.SqlServer` | `[`…`]` | `@p1`, `@p2`… | `FETCH NEXT` pagination |
| `TOracleCompiler` | `Daf.SQLCute.Compiler.Oracle` | `"` | `:p1`, `:p2`… | `TRUNC(col)` date, `FETCH` pagination |
| `TFirebirdCompiler` | `Daf.SQLCute.Compiler.Firebird` | `"` | `?` | `EXTRACT` date parts |

The ANSI base compiler (`TAnsiSqlCompiler`) lives in the `SQLCute` core package (`Daf.SQLCute.Compiler`).

---

## Quick Example

```pascal
uses
  Daf.SQLCute,
  Daf.SQLCute.Compiler.Postgres;

var Q := TQuery.New
  .From('orders')
  .Join('customers', 'orders.customer_id', 'customers.id')
  .Where('orders.status', 'pending')
  .WhereDate('orders.created_at', '2024-06-01')
  .OrderByDesc('orders.created_at')
  .Limit(50);

var R := TPostgresCompiler.Create.Compile(Q);
// R.SQL →
//   SELECT * FROM "orders"
//   INNER JOIN "customers" ON "orders"."customer_id" = "customers"."id"
//   WHERE "orders"."status" = $1
//     AND "orders"."created_at"::date = $2
//   ORDER BY "orders"."created_at" DESC
//   LIMIT 50
```

---

## Documentation

- [Dialect Compilers Guide](../SQLCute.Abstractions/docs/guide/dialects.md) — full reference, DI pattern, custom compiler
- [Date Operations](../SQLCute.Abstractions/docs/guide/date-ops.md) — dialect date-cast matrix
