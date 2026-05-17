# SQLCute.Compilers — Compiladores de Dialecto

**🌍 Idioma: [English](README.md) | Español**

Compiladores SQL específicos de dialecto para [SQLCute](../SQLCute.Abstractions/README.es.md). Cada compilador extiende la base ANSI con entrecomillado de identificadores, marcadores de parámetros, cast de fechas y paginación nativos de cada base de datos.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![Licencia](https://img.shields.io/badge/licencia-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Compiladores Disponibles

| Clase | Unidad | Comillas | Marcador | Notas |
|-------|--------|---------|---------|-------|
| `TPostgresCompiler` | `Daf.SQLCute.Compiler.Postgres` | `"` | `$1`, `$2`… | Cast `::date` |
| `TMySqlCompiler` | `Daf.SQLCute.Compiler.MySql` | `` ` `` | `?` | Cast `DATE(col)` |
| `TSQLiteCompiler` | `Daf.SQLCute.Compiler.SQLite` | `"` | `?` | `date(col)` / `strftime` |
| `TSqlServerCompiler` | `Daf.SQLCute.Compiler.SqlServer` | `[`…`]` | `@p1`, `@p2`… | Paginación `FETCH NEXT` |
| `TOracleCompiler` | `Daf.SQLCute.Compiler.Oracle` | `"` | `:p1`, `:p2`… | Cast `TRUNC(col)`, paginación `FETCH` |
| `TFirebirdCompiler` | `Daf.SQLCute.Compiler.Firebird` | `"` | `?` | Partes de fecha con `EXTRACT` |

El compilador base ANSI (`TAnsiSqlCompiler`) vive en el paquete núcleo de `SQLCute` (`Daf.SQLCute.Compiler`).

---

## Ejemplo Rápido

```pascal
uses
  Daf.SQLCute,
  Daf.SQLCute.Compiler.Postgres;

var Q := TQuery.New
  .From('pedidos')
  .Join('clientes', 'pedidos.cliente_id', 'clientes.id')
  .Where('pedidos.estado', 'pendiente')
  .WhereDate('pedidos.creado_en', '2024-06-01')
  .OrderByDesc('pedidos.creado_en')
  .Limit(50);

var R := TPostgresCompiler.Create.Compile(Q);
// R.SQL →
//   SELECT * FROM "pedidos"
//   INNER JOIN "clientes" ON "pedidos"."cliente_id" = "clientes"."id"
//   WHERE "pedidos"."estado" = $1
//     AND "pedidos"."creado_en"::date = $2
//   ORDER BY "pedidos"."creado_en" DESC
//   LIMIT 50
```

---

## Documentación

- [Guía de Compiladores de Dialecto](../SQLCute.Abstractions/docs/guide/dialects.es.md) — referencia completa, patrón DI, compilador personalizado
- [Operaciones de Fecha](../SQLCute.Abstractions/docs/guide/date-ops.es.md) — matriz de cast de fechas por dialecto
