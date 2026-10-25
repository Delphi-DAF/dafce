# GROUP BY / HAVING

**🌍 Idioma: [English](groupby.md) | Español**

← [Volver a la Guía](../GUIDE.es.md)

---

## GROUP BY

```pascal
TQuery.New.From('ventas')
  .SelectCount('*', 'qty')
  .GroupBy('producto_id')
// → SELECT COUNT(*) AS qty FROM ventas GROUP BY producto_id
```

Múltiples columnas — llama a `GroupBy` varias veces o pasa un array:

```pascal
.GroupBy(['año', 'mes', 'categoria'])
// → GROUP BY año, mes, categoria
```

Expresión cruda:

```pascal
.GroupByRaw('YEAR(creado_en), MONTH(creado_en)')
// → GROUP BY YEAR(creado_en), MONTH(creado_en)
```

---

## HAVING

Una condición sobre un agregado va por `HavingRaw`, que aceptan todos los motores:

```pascal
TQuery.New.From('pedidos')
  .GroupBy('cliente_id')
  .SelectCount('*', 'num_pedidos')
  .SelectSum('total', 'ingresos')
  .HavingRaw('count(*) > 5')
  .HavingRaw('sum(total) >= 1000')
// → SELECT COUNT(*) AS num_pedidos, SUM(total) AS ingresos
//   FROM pedidos
//   GROUP BY cliente_id
//   HAVING count(*) > 5 AND sum(total) >= 1000
```

`Having` compara una columna con un valor ligado. La columna nombra una columna agrupada o el alias de una columna de salida, y se entrecomilla como cualquier otra columna del dialecto. Su operador es uno de `=`, `<>`, `!=`, `<`, `<=`, `>`, `>=`, `like` y `not like`; `in` y `not in` lanzan `EArgumentException`, y un `IN` sobre un agregado va por `HavingRaw`. MySQL, SQLite y Oracle 23 aceptan el alias de una columna de salida en `HAVING`; Postgres, SQL Server y Firebird lo rechazan.

```pascal
.Having('num_pedidos', '>', 5)
// Postgres → HAVING "num_pedidos" > $1
```

Conector OR:

```pascal
.Having('num_pedidos', '>', 10)
.OrHaving('ingresos', '>=', 1000)
// → HAVING num_pedidos > ? OR ingresos >= ?
```

Expresión HAVING cruda:

```pascal
.HavingRaw('AVG(tiempo_respuesta) < 200')
```

`Having` con `Null` compara igual que `Where`: `IS NULL` con `=` e `IS NOT NULL` con cualquier otro operador, sin enlace.

---

## Combinando con WHERE

`WHERE` filtra filas antes de agrupar; `HAVING` filtra grupos después:

```pascal
TQuery.New.From('logs')
  .Where('nivel', 'error')         // aplicado antes de GROUP BY
  .GroupBy('servicio')
  .SelectCount('*', 'errores')
  .HavingRaw('count(*) > 50')      // aplicado después de GROUP BY
// → SELECT COUNT(*) AS errores FROM logs
//   WHERE nivel = ?
//   GROUP BY servicio
//   HAVING count(*) > 50
```
