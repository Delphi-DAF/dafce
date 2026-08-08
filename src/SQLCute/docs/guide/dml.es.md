# DML — INSERT / UPDATE / DELETE

**🌍 Idioma: [English](dml.md) | Español**

← [Volver a la Guía](../GUIDE.es.md)

---

## INSERT — Fila Única

Pasa arrays paralelos de nombres de columna y valores:

```pascal
TQuery.New.From('usuarios')
  .AsInsert(
    ['nombre',  'email',               'rol'],
    ['Alicia',  'alicia@ejemplo.com',  'admin'])
// → INSERT INTO usuarios (nombre, email, rol) VALUES (?, ?, ?)
//   Bindings: ['Alicia', 'alicia@ejemplo.com', 'admin']
```

---

## INSERT — Múltiples Filas

```pascal
TQuery.New.From('etiquetas')
  .AsInsertRows(
    ['nombre', 'color'],
    [['Urgente', 'rojo'],
     ['Normal',  'verde'],
     ['Bajo',    'gris']])
// → INSERT INTO etiquetas (nombre, color) VALUES (?, ?), (?, ?), (?, ?)
//   Bindings: ['Urgente', 'rojo', 'Normal', 'verde', 'Bajo', 'gris']
```

---

## INSERT … SELECT

Copia filas de una tabla a otra usando una subconsulta:

```pascal
var Sub := TQuery.New.From('usuarios_temp')
  .Select(['nombre', 'email'])
  .Where('importado', True);

TQuery.New.From('usuarios')
  .AsInsertFrom(['nombre', 'email'], Sub)
// → INSERT INTO usuarios (nombre, email)
//   SELECT nombre, email FROM usuarios_temp WHERE importado = ?
```

---

## UPDATE

```pascal
TQuery.New.From('productos')
  .Where('id', 42)
  .AsUpdate(
    ['precio', 'stock'],
    [29.99,    100])
// → UPDATE productos SET precio = ?, stock = ? WHERE id = ?
//   Bindings: [29.99, 100, 42]
```

> ⚠️ Sin cláusula `Where`, se actualizan TODAS las filas. Añade una condición WHERE para acotar la actualización.

---

## DELETE

```pascal
TQuery.New.From('sesiones')
  .Where('expirado', True)
  .AsDelete
// → DELETE FROM sesiones WHERE expirado = ?
```

Patrón de borrado lógico usando UPDATE:

```pascal
TQuery.New.From('usuarios')
  .Where('id', IdUsuario)
  .AsUpdate(['eliminado_en'], [Now])
// → UPDATE usuarios SET eliminado_en = ? WHERE id = ?
```

---

## Combinando DML con Subconsultas

```pascal
// Eliminar pedidos con más de 1 año cuyas líneas están todas enviadas
TQuery.New.From('pedidos')
  .Where('creado_en', '<', EncodeDate(Año - 1, Mes, Dia))
  .WhereNotExists(
    TQuery.New.From('lineas_pedido')
      .Where('pedido_id', '=', 'pedidos.id')
      .WhereNot('estado', 'enviado'))
  .AsDelete
```
