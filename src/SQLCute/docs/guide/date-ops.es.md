# Operaciones de Fecha

**🌍 Idioma: [English](date-ops.md) | Español**

← [Volver a la Guía](../GUIDE.es.md)

---

## WhereDate — Coincidencia de Fecha Exacta

Filtra filas donde la parte DATE de una columna datetime es igual a un valor, ignorando el componente de tiempo. La expresión de conversión es específica del dialecto (ver tabla más abajo).

```pascal
TQuery.New.From('pedidos')
  .WhereDate('creado_en', '2024-06-15')
// ANSI: WHERE CAST(creado_en AS DATE) = ?
// Bindings: ['2024-06-15']
```

Conector OR:

```pascal
.OrWhereDate('actualizado_en', '2024-06-15')
```

---

## WhereTime — Comparación de Hora

```pascal
TQuery.New.From('citas')
  .WhereTime('hora_inicio', '>=', '09:00:00')
// ANSI: WHERE CAST(hora_inicio AS TIME) >= ?
```

Variante OR: `OrWhereTime`. El operador es uno de `=`, `<>`, `!=`, `<`, `<=`, `>`, `>=`, `like` y `not like`; cualquier otro lanza `EArgumentException` al añadir la condición.

---

## WhereDatePart — Extraer una Parte

Filtra por año, mes, día, hora o minuto:

```pascal
uses Daf.SQLCute.Clauses;  // TDatePart

TQuery.New.From('eventos')
  .WhereDatePart(TDatePart.dpYear,  'fecha_evento', 2024)
  .WhereDatePart(TDatePart.dpMonth, 'fecha_evento', 12)
// ANSI: WHERE EXTRACT(year FROM fecha_evento) = ? AND EXTRACT(month FROM fecha_evento) = ?
```

Valores disponibles de `TDatePart`: `dpDate`, `dpTime`, `dpYear`, `dpMonth`, `dpDay`, `dpHour`, `dpMinute`.

Variante OR: `OrWhereDatePart`.

---

## Matriz de Cast de Fecha por Dialecto

Cada compilador de dialecto sobreescribe la expresión de conversión para `WhereDate`:

| Dialecto | `WhereDate('col', val)` genera |
|----------|-------------------------------|
| ANSI (por defecto) | `CAST(col AS DATE) = ?` |
| PostgreSQL | `col::date = ?` |
| MySQL | `DATE(col) = ?` |
| SQLite | `date(col) = ?` |
| SQL Server | `CAST(col AS date) = ?` |
| Oracle | `TRUNC(col) = ?` |
| Firebird | `CAST(col AS DATE) = ?` |

### Ejemplo PostgreSQL

```pascal
uses Daf.SQLCute, Daf.SQLCute.Compiler.Postgres;

var Compiler: IQueryCompiler := TPostgresCompiler.Create;  // contado por referencia: ha de tener dueño
var R := Compiler.Compile(
  TQuery.New.From('t').WhereDate('d', '2024-01-01'));
// → WHERE "d"::date = $1
```

### Ejemplo MySQL

```pascal
uses Daf.SQLCute, Daf.SQLCute.Compiler.MySql;

var Compiler: IQueryCompiler := TMySqlCompiler.Create;  // contado por referencia: ha de tener dueño
var R := Compiler.Compile(
  TQuery.New.From('t').WhereDate('d', '2024-01-01'));
// → WHERE DATE(`d`) = ?
```

---

## Matriz de DatePart por Dialecto

| Dialecto | EXTRACT año | EXTRACT mes | EXTRACT día | EXTRACT hora |
|----------|------------|------------|------------|-------------|
| ANSI | `EXTRACT(year FROM col)` | `EXTRACT(month FROM col)` | `EXTRACT(day FROM col)` | `EXTRACT(hour FROM col)` |
| PostgreSQL | igual que ANSI | igual | igual | igual |
| MySQL | `YEAR(col)` | `MONTH(col)` | `DAY(col)` | `HOUR(col)` |
| SQLite | `strftime('%Y', col)` | `strftime('%m', col)` | `strftime('%d', col)` | `strftime('%H', col)` |
| SQL Server | `DATEPART(year, col)` | `DATEPART(month, col)` | `DATEPART(day, col)` | `DATEPART(hour, col)` |
| Oracle | `EXTRACT(YEAR FROM col)` | `EXTRACT(MONTH FROM col)` | `EXTRACT(DAY FROM col)` | — |
| Firebird | `EXTRACT(YEAR FROM col)` | `EXTRACT(MONTH FROM col)` | `EXTRACT(DAY FROM col)` | `EXTRACT(HOUR FROM col)` |

> Pasa valores de fecha como cadenas (`'2024-06-15'`) o como valores `TDateTime` de Delphi — el driver de base de datos gestiona la conversión de tipo desde el binding `Variant`.

⚠️ **Estos predicados son solo de SQL.** El cast es propio de cada dialecto, así que [`SQLCute.InMemory`](../../../SQLCute.InMemory/README.es.md) los rechaza con `EInMemoryQueryError` en vez de aproximarlos. Una consulta pensada para los dos back-ends filtra fechas con un `Where` corriente sobre una columna comparable.
