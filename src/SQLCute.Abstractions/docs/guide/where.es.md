# WHERE

**🌍 Idioma: [English](where.md) | Español**

← [Volver a la Guía](../GUIDE.es.md)

---

## Igualdad Básica

```pascal
.Where('estado', 'activo')
// → WHERE estado = ?   Bindings: ['activo']
```

Con operador explícito:

```pascal
.Where('precio', '>', 100)
.Where('stock', '<=', 0)
// → WHERE precio > ? AND stock <= ?   Bindings: [100, 0]
```

Conector OR:

```pascal
.Where('estado', 'pendiente')
.OrWhere('estado', 'procesando')
// → WHERE estado = ? OR estado = ?
```

---

## WhereNot

```pascal
.WhereNot('rol', 'invitado')
// → WHERE NOT (rol = ?)

.OrWhereNot('estado', 'cerrado')
// → OR NOT (estado = ?)
```

---

## Comprobaciones NULL

```pascal
.WhereNull('eliminado_en')       // → WHERE eliminado_en IS NULL
.WhereNotNull('email')           // → WHERE email IS NOT NULL
.OrWhereNull('archivado_en')     // → OR archivado_en IS NULL
```

Pasar `Null` directamente a `Where` también genera `IS NULL`:

```pascal
.Where('eliminado_en', Null)     // → WHERE eliminado_en IS NULL
```

---

## Abreviatura Booleana

```pascal
.WhereTrue('activo')    // → WHERE activo = TRUE
.WhereFalse('bloqueado') // → WHERE bloqueado = FALSE
```

---

## BETWEEN

```pascal
.WhereBetween('precio', 10, 100)
// → WHERE precio BETWEEN ? AND ?   Bindings: [10, 100]

.WhereNotBetween('edad', 13, 17)
.OrWhereBetween('puntuacion', 90, 100)
```

---

## WHERE IN

```pascal
.WhereIn('estado', ['pendiente', 'procesando', 'enviado'])
// → WHERE estado IN (?, ?, ?)

.WhereNotIn('id', [1, 2, 3])
.OrWhereIn('categoria_id', [10, 20])
.OrWhereNotIn('etiqueta', ['spam', 'bot'])
```

---

## WHERE EXISTS (subconsulta)

```pascal
var Sub := TQuery.New.From('lineas_pedido').Where('pedido_id', '=', 'pedidos.id');

TQuery.New.From('pedidos')
  .WhereExists(Sub)
// → WHERE EXISTS (SELECT * FROM lineas_pedido WHERE pedido_id = ?)
```

---

## WHERE IN (subconsulta)

```pascal
var UsuariosActivos := TQuery.New.From('usuarios').Select('id').Where('activo', True);

TQuery.New.From('pedidos')
  .WhereInQuery('cliente_id', UsuariosActivos)
// → WHERE cliente_id IN (SELECT id FROM usuarios WHERE activo = ?)
```

---

## WhereRaw

Añade un fragmento SQL crudo (conector AND). Usar con precaución — sin escape aplicado:

```pascal
.WhereRaw('creado_en > NOW() - INTERVAL ''7 days''')
```

---

## WHERE Agrupado (Callback)

Envuelve condiciones en paréntesis usando un callback:

```pascal
TQuery.New.From('productos')
  .Where('activo', True)
  .Where(function(Q: IQuery): IQuery
    begin
      Result := Q.Where('stock', '>', 0)
                 .OrWhere('pedido_pendiente_permitido', True);
    end)
// → WHERE activo = ? AND (stock > ? OR pedido_pendiente_permitido = ?)
```

Grupo con conector OR:

```pascal
.OrWhere(function(Q: IQuery): IQuery
  begin
    Result := Q.Where('rol', 'admin').OrWhere('rol', 'moderador');
  end)
// → OR (rol = ? OR rol = ?)
```

---

## WhereColumns

Comparación columna a columna (sin binding — los valores se emiten literalmente):

```pascal
.WhereColumns('fecha_inicio', '<', 'fecha_fin')
// → WHERE fecha_inicio < fecha_fin

.OrWhereColumns('precio', '=', 'precio_lista')
```

> ⚠️ Pasa solo nombres de columnas controlados por el desarrollador a `WhereColumns`. Nunca pases input del usuario.

---

## When — Encadenamiento Condicional

Aplica condiciones solo cuando un flag en tiempo de ejecución es verdadero:

```pascal
var IncluirInactivos := False;

TQuery.New.From('usuarios')
  .When(not IncluirInactivos, function(Q: IQuery): IQuery
    begin
      Result := Q.Where('activo', True);
    end)
// → WHERE activo = ?   (solo cuando IncluirInactivos es False)
```

Con rama else:

```pascal
.When(EsAdmin,
  function(Q: IQuery): IQuery begin Result := Q; end,            // sin filtro extra
  function(Q: IQuery): IQuery begin Result := Q.Where('tenant_id', TenantId); end)
```

---

## Condiciones de Cadena / LIKE

Consulta [Operaciones de Cadena](string-ops.es.md) para `WhereLike`, `WhereStarts`, `WhereEnds`, `WhereContains`.

---

## Condiciones de Fecha

Consulta [Operaciones de Fecha](date-ops.es.md) para `WhereDate`, `WhereTime`, `WhereDatePart`.
