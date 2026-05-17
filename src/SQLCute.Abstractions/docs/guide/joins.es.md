# JOINs

**🌍 Idioma: [English](joins.md) | Español**

← [Volver a la Guía](../GUIDE.es.md)

---

## Joins Básicos

Todos los métodos de join siguen la misma firma: `TipoJoin(tabla, col1, col2, op)`. El operador por defecto es `=`.

### INNER JOIN

```pascal
TQuery.New.From('pedidos')
  .Join('clientes', 'pedidos.cliente_id', 'clientes.id')
// → SELECT * FROM pedidos INNER JOIN clientes ON pedidos.cliente_id = clientes.id
```

### LEFT JOIN

```pascal
.LeftJoin('direcciones', 'usuarios.id', 'direcciones.usuario_id')
// → LEFT JOIN direcciones ON usuarios.id = direcciones.usuario_id
```

### RIGHT JOIN

```pascal
.RightJoin('departamentos', 'empleados.dept_id', 'departamentos.id')
// → RIGHT JOIN departamentos ON empleados.dept_id = departamentos.id
```

### CROSS JOIN

```pascal
.CrossJoin('tallas')
// → CROSS JOIN tallas
```

### FULL OUTER JOIN

```pascal
.FullOuterJoin('b', 'a.id', 'b.a_id')
// → FULL OUTER JOIN b ON a.id = b.a_id
```

---

## Join con Cadena de Condición Cruda

Pasa una cláusula ON de SQL crudo cuando la condición no puede expresarse como columna=columna:

```pascal
.Join('precios', 'precios.producto_id = productos.id AND precios.activo = 1')
// → INNER JOIN precios ON precios.producto_id = productos.id AND precios.activo = 1
```

---

## Join con Callback (ON Complejo)

Usa un callback para múltiples condiciones unidas con AND/OR:

```pascal
TQuery.New.From('pedidos')
  .Join('lineas_pedido', function(Q: IQuery): IQuery
    begin
      Result := Q
        .Where('lineas_pedido.pedido_id', '=', 'pedidos.id')
        .OrWhere('lineas_pedido.pedido_alt_id', '=', 'pedidos.id');
    end)
// → INNER JOIN lineas_pedido ON (lineas_pedido.pedido_id = pedidos.id
//                                 OR lineas_pedido.pedido_alt_id = pedidos.id)
```

`LeftJoin`, `RightJoin` y `FullOuterJoin` admiten el mismo overload de callback.

---

## Join con Subconsulta

```pascal
var Sub := TQuery.New.From('lineas_pedido')
  .Select('pedido_id')
  .SelectSum('qty', 'qty_total')
  .GroupBy('pedido_id');

TQuery.New.From('pedidos')
  .Join(Sub, 'li', function(Q: IQuery): IQuery
    begin
      Result := Q.Where('li.pedido_id', '=', 'pedidos.id');
    end)
// → INNER JOIN (SELECT pedido_id, SUM(qty) AS qty_total
//               FROM lineas_pedido GROUP BY pedido_id) li
//   ON li.pedido_id = pedidos.id
```

---

## Múltiples Joins

Encadena tantos joins como necesites — se emiten en orden de declaración:

```pascal
TQuery.New.From('facturas')
  .Join('clientes', 'facturas.cliente_id', 'clientes.id')
  .LeftJoin('descuentos', 'facturas.descuento_id', 'descuentos.id')
  .Select(['facturas.id', 'clientes.nombre', 'descuentos.pct'])
```
