# Date Operations

**🌍 Language: English | [Español](date-ops.es.md)**

← [Back to Guide](../GUIDE.md)

---

## WhereDate — Exact Date Match

Filters rows where the DATE part of a datetime column equals a value, ignoring the time component. The cast expression is dialect-specific (see table below).

```pascal
TQuery.New.From('orders')
  .WhereDate('created_at', '2024-06-15')
// ANSI: WHERE CAST(created_at AS DATE) = ?
// Bindings: ['2024-06-15']
```

OR connector:

```pascal
.OrWhereDate('updated_at', '2024-06-15')
```

---

## WhereTime — Time Comparison

```pascal
TQuery.New.From('appointments')
  .WhereTime('start_time', '>=', '09:00:00')
// ANSI: WHERE CAST(start_time AS TIME) >= ?
```

OR variant: `OrWhereTime`.

---

## WhereDatePart — Extract a Part

Filter by year, month, day, hour, or minute:

```pascal
uses Daf.SQLCute.Clauses;  // TDatePart

TQuery.New.From('events')
  .WhereDatePart(TDatePart.dpYear,  'event_date', 2024)
  .WhereDatePart(TDatePart.dpMonth, 'event_date', 12)
// ANSI: WHERE EXTRACT(year FROM event_date) = ? AND EXTRACT(month FROM event_date) = ?
```

Available `TDatePart` values: `dpDate`, `dpTime`, `dpYear`, `dpMonth`, `dpDay`, `dpHour`, `dpMinute`.

OR variant: `OrWhereDatePart`.

---

## Dialect Date-Cast Matrix

Each dialect compiler overrides the cast expression for `WhereDate`:

| Dialect | `WhereDate('col', val)` generates |
|---------|----------------------------------|
| ANSI (default) | `CAST(col AS DATE) = ?` |
| PostgreSQL | `col::date = ?` |
| MySQL | `DATE(col) = ?` |
| SQLite | `date(col) = ?` |
| SQL Server | `CAST(col AS date) = ?` |
| Oracle | `TRUNC(col) = ?` |
| Firebird | `CAST(col AS DATE) = ?` |

### PostgreSQL example

```pascal
uses Daf.SQLCute.Compiler.Postgres;

var R := TPostgresCompiler.Create.Compile(
  TQuery.New.From('t').WhereDate('d', '2024-01-01'));
// → WHERE "d"::date = $1
```

### MySQL example

```pascal
uses Daf.SQLCute.Compiler.MySql;

var R := TMySqlCompiler.Create.Compile(
  TQuery.New.From('t').WhereDate('d', '2024-01-01'));
// → WHERE DATE(`d`) = ?
```

---

## Dialect DatePart Matrix

| Dialect | EXTRACT year | EXTRACT month | EXTRACT day | EXTRACT hour |
|---------|-------------|--------------|------------|-------------|
| ANSI | `EXTRACT(year FROM col)` | `EXTRACT(month FROM col)` | `EXTRACT(day FROM col)` | `EXTRACT(hour FROM col)` |
| PostgreSQL | same as ANSI | same | same | same |
| MySQL | `YEAR(col)` | `MONTH(col)` | `DAY(col)` | `HOUR(col)` |
| SQLite | `strftime('%Y', col)` | `strftime('%m', col)` | `strftime('%d', col)` | `strftime('%H', col)` |
| SQL Server | `DATEPART(year, col)` | `DATEPART(month, col)` | `DATEPART(day, col)` | `DATEPART(hour, col)` |
| Oracle | `EXTRACT(YEAR FROM col)` | `EXTRACT(MONTH FROM col)` | `EXTRACT(DAY FROM col)` | — |
| Firebird | `EXTRACT(YEAR FROM col)` | `EXTRACT(MONTH FROM col)` | `EXTRACT(DAY FROM col)` | `EXTRACT(HOUR FROM col)` |

> Pass date values as strings (`'2024-06-15'`) or as Delphi `TDateTime` values — the database driver handles the type conversion from the `Variant` binding.
