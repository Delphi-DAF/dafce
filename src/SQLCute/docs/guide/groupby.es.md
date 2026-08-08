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

`Having` filtra grupos tras la agregación. El argumento columna acepta cualquier expresión de agregado.

```pascal
TQuery.New.From('pedidos')
  .GroupBy('cliente_id')
  .SelectCount('*', 'num_pedidos')
  .SelectSum('total', 'ingresos')
  .Having('count(*)', '>', 5)
  .Having('sum(total)', '>=', 1000)
// → SELECT COUNT(*) AS num_pedidos, SUM(total) AS ingresos
//   FROM pedidos
//   GROUP BY cliente_id
//   HAVING count(*) > ? AND sum(total) >= ?
//   Bindings: [5, 1000]
```

Expresión HAVING cruda:

```pascal
.HavingRaw('AVG(tiempo_respuesta) < 200')
```

---

## Combinando con WHERE

`WHERE` filtra filas antes de agrupar; `HAVING` filtra grupos después:

```pascal
TQuery.New.From('logs')
  .Where('nivel', 'error')         // aplicado antes de GROUP BY
  .GroupBy('servicio')
  .SelectCount('*', 'errores')
  .Having('count(*)', '>', 50)     // aplicado después de GROUP BY
// → SELECT COUNT(*) AS errores FROM logs
//   WHERE nivel = ?
//   GROUP BY servicio
//   HAVING count(*) > ?
```
