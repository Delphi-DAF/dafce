# SELECT & FROM

**🌍 Idioma: [English](select.md) | Español**

← [Volver a la Guía](../GUIDE.es.md)

---

## From

Establece la tabla fuente. Todas las consultas empiezan aquí.

```pascal
TQuery.New.From('pedidos')
// → SELECT * FROM pedidos
```

Con alias:

```pascal
TQuery.New.From('pedidos', 'p')
// → SELECT * FROM pedidos p
```

---

## Select — Lista de Columnas

```pascal
TQuery.New.From('usuarios')
  .Select(['id', 'nombre', 'email'])
// → SELECT id, nombre, email FROM usuarios
```

Columna única:

```pascal
.Select('nombre')
// → SELECT nombre FROM usuarios
```

Con alias:

```pascal
.SelectAs('nombre_completo', 'nombre')
// → SELECT nombre_completo AS nombre FROM usuarios
```

---

## SELECT * (por defecto)

Cuando no hay llamada a `Select`, se emite `SELECT *`:

```pascal
TQuery.New.From('productos')
// → SELECT * FROM productos
```

---

## Distinct

```pascal
TQuery.New.From('pedidos')
  .Select('cliente_id')
  .Distinct
// → SELECT DISTINCT cliente_id FROM pedidos
```

---

## SelectRaw

Emite una expresión SQL cruda en la lista SELECT (sin entrecomillado ni escape):

```pascal
TQuery.New.From('ventas')
  .SelectRaw('YEAR(creado_en) AS año')
  .SelectRaw('COUNT(*) AS total')
// → SELECT YEAR(creado_en) AS año, COUNT(*) AS total FROM ventas
```

---

## Funciones de Agregado

| Método | Alias por defecto | SQL generado |
|--------|------------------|--------------|
| `SelectCount(col, alias)` | `'count'` | `COUNT(col) AS alias` |
| `SelectSum(col, alias)` | `'sum'` | `SUM(col) AS alias` |
| `SelectAvg(col, alias)` | `'avg'` | `AVG(col) AS alias` |
| `SelectMin(col, alias)` | `'min'` | `MIN(col) AS alias` |
| `SelectMax(col, alias)` | `'max'` | `MAX(col) AS alias` |

```pascal
TQuery.New.From('pedidos')
  .SelectCount('*', 'total_pedidos')
  .SelectSum('importe', 'ingresos')
  .SelectAvg('importe', 'pedido_medio')
  .GroupBy('estado')
// → SELECT COUNT(*) AS total_pedidos, SUM(importe) AS ingresos, AVG(importe) AS pedido_medio
//   FROM pedidos GROUP BY estado
```

---

## ORDER BY

```pascal
.OrderBy('nombre')              // ASC
.OrderByDesc('creado_en')       // DESC
.OrderByRaw('FIELD(estado, ''pendiente'', ''enviado'', ''cerrado'')')
```

Múltiples ORDER BY — llama a `OrderBy`/`OrderByDesc` varias veces:

```pascal
TQuery.New.From('productos')
  .OrderBy('categoria')
  .OrderByDesc('precio')
// → SELECT * FROM productos ORDER BY categoria ASC, precio DESC
```

### Dónde caen los NULL

Un `OrderBy` sobre una columna nullable no dice nada de los NULL, así que **cada
base de datos aplica su propia regla**: Postgres y Oracle los ordenan al final en
ascendente; MySQL, SQL Server, SQLite y Firebird los ponen al principio. Cuando
importa, dilo explícitamente con `NullsFirst` / `NullsLast`:

```pascal
TQuery.New.From('personas').OrderBy('edad').NullsLast
// → SELECT * FROM personas ORDER BY edad ASC NULLS LAST

TQuery.New.From('personas').OrderByDesc('edad').NullsFirst
// → SELECT * FROM personas ORDER BY edad DESC NULLS FIRST
```

El modificador cualifica la **última** columna añadida al ORDER BY, de modo que
cada columna conserva su propia colocación:

```pascal
TQuery.New.From('personas')
  .OrderBy('edad').NullsLast
  .OrderBy('nombre')
// → SELECT * FROM personas ORDER BY edad ASC NULLS LAST, nombre ASC
```

Lo mismo en una sola llamada, cuando pasar la opción se lee mejor que encadenar:

```pascal
TQuery.New.From('personas').OrderBy('edad', TNullsOrder.Last)       // Daf.SQLCute.Clauses
TQuery.New.From('personas').OrderByDesc('edad', TNullsOrder.First)
```

`NullsFirst` / `NullsLast` lanzan excepción si no hay columna ordenada que
cualificar — una expresión `OrderByRaw` no cuenta, porque se emite tal cual.

No todos los dialectos tienen la sintaxis `NULLS FIRST/LAST`; los que no la
tienen la emulan, así que una ordenación cualificada significa lo mismo en todas
partes. Consulta [Compiladores de Dialecto](dialects.es.md#ordenación-de-null-por-dialecto)
para el detalle por dialecto y el valor por omisión de cada base de datos.

---

## LIMIT y OFFSET

```pascal
TQuery.New.From('logs').Limit(100)
// → SELECT * FROM logs LIMIT 100

TQuery.New.From('logs').Limit(20).Offset(40)
// → SELECT * FROM logs LIMIT 20 OFFSET 40
```

`Take` y `Skip` son alias de `Limit` y `Offset`:

```pascal
.Take(20).Skip(40)   // igual que Limit(20).Offset(40)
```

Para paginación basada en páginas, consulta [Operaciones de Conjunto](setops.es.md#paginación).
