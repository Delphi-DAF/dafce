# Subconsultas y CTEs

**🌍 Idioma: [English](subqueries.md) | Español**

← [Volver a la Guía](../GUIDE.es.md)

---

## Tabla Derivada (FROM con subconsulta)

Usa un `IQuery` como fuente FROM con un alias:

```pascal
var Interno := TQuery.New.From('lineas_pedido')
  .Select('pedido_id')
  .SelectSum('qty', 'qty_total')
  .GroupBy('pedido_id');

TQuery.New
  .From(Interno, 'li')
  .Select(['li.pedido_id', 'li.qty_total'])
  .Where('li.qty_total', '>', 10)
// → SELECT li.pedido_id, li.qty_total
//   FROM (SELECT pedido_id, SUM(qty) AS qty_total
//         FROM lineas_pedido GROUP BY pedido_id) li
//   WHERE li.qty_total > ?
```

---

## FROM Raw

Usa una expresión SQL cruda como fuente FROM (p.ej., funciones con valores de tabla):

```pascal
TQuery.New
  .FromRaw('generate_series(1, 100) AS s(n)', [])
  .Select('n')
// → SELECT n FROM generate_series(1, 100) AS s(n)
```

Con bindings:

```pascal
.FromRaw('fn_obtener_eventos(?, ?) AS e', [FechaInicio, FechaFin])
```

---

## Subconsulta en WHERE IN

```pascal
var PedidosRecientes := TQuery.New.From('pedidos')
  .Select('cliente_id')
  .Where('creado_en', '>', SemanaAnterior);

TQuery.New.From('clientes')
  .WhereInQuery('id', PedidosRecientes)
// → SELECT * FROM clientes
//   WHERE id IN (SELECT cliente_id FROM pedidos WHERE creado_en > ?)
```

Variantes: `WhereNotInQuery`, `OrWhereInQuery`, `OrWhereNotInQuery`.

---

## Subconsulta en WHERE EXISTS

```pascal
TQuery.New.From('pedidos')
  .WhereExists(
    TQuery.New.From('pagos')
      .Where('pedido_id', '=', 'pedidos.id')
      .Where('estado', 'completo'))
// → WHERE EXISTS (SELECT * FROM pagos
//                WHERE pedido_id = pedidos.id AND estado = ?)
```

---

## Subconsulta Escalar en SELECT

```pascal
var NumLineas := TQuery.New.From('lineas_pedido')
  .SelectCount('*')
  .Where('pedido_id', '=', 'pedidos.id');

TQuery.New.From('pedidos')
  .Select(NumLineas, 'num_lineas')
// → SELECT (SELECT COUNT(*) FROM lineas_pedido WHERE pedido_id = pedidos.id)
//          AS num_lineas FROM pedidos
```

---

## WITH (CTE)

```pascal
var VentasRecientes := TQuery.New.From('ventas')
  .Where('año', 2024)
  .Select(['producto_id', 'importe']);

TQuery.New
  .&With('recientes', VentasRecientes)
  .From('recientes')
  .SelectSum('importe', 'total')
// → WITH recientes AS (SELECT producto_id, importe FROM ventas WHERE año = ?)
//   SELECT SUM(importe) AS total FROM recientes
```

---

## CTE RECURSIVO

```pascal
var Ancla := TQuery.New.From('categorias')
  .Where('padre_id', Null);
var Recursivo := TQuery.New.From('categorias')
  .Join('arbol', 'categorias.padre_id', 'arbol.id');

TQuery.New
  .WithRecursive('arbol', Ancla.Union(Recursivo))
  .From('arbol')
// → WITH RECURSIVE arbol AS (
//     SELECT * FROM categorias WHERE padre_id IS NULL
//     UNION
//     SELECT categorias.* FROM categorias INNER JOIN arbol ON categorias.padre_id = arbol.id
//   ) SELECT * FROM arbol
```

---

## WITH Raw

```pascal
TQuery.New
  .WithRaw('nombre_cte', 'SELECT id FROM tabla_legacy WHERE flag = ?', [1])
  .From('nombre_cte')
```

---

## Profundidad de Anidamiento

SQLCute admite anidamiento arbitrario — las subconsultas pueden contener a su vez otras subconsultas. Los bindings se aplanan en un único array en el orden en que aparecen en el SQL compilado.
