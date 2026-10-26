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

El operador es uno de `=`, `<>`, `!=`, `<`, `<=`, `>`, `>=`, `like`, `not like`, `in` y `not in`, en mayúsculas o minúsculas; cualquier otro lanza `EArgumentException`.

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

Pasar `Null` directamente a `Where`, `OrWhere`, `WhereNot` u `OrWhereNot` compara con NULL como SqlKata: con `=` o sin operador emite `IS NULL`, y con cualquier otro operador `IS NOT NULL`, sin enlace. `WhereNot` conserva su negación, que SqlKata pierde:

```pascal
.Where('eliminado_en', Null)         // → WHERE eliminado_en IS NULL
.Where('eliminado_en', '<>', Null)   // → WHERE eliminado_en IS NOT NULL
.WhereNot('eliminado_en', Null)      // → WHERE NOT (eliminado_en IS NULL)
```

---

## Abreviatura Booleana

```pascal
.WhereTrue('activo')     // → WHERE activo = true
.WhereFalse('bloqueado') // → WHERE bloqueado = false
```

La comparación es con el literal del dialecto, sin enlace: `true` y `false` en ANSI, Postgres, MySQL y Oracle, `1` y `0` en SQLite y Firebird, y `cast(1 as bit)` y `cast(0 as bit)` en SQL Server —ver [Compiladores de dialecto](dialects.es.md)—. `Where`, `OrWhere`, `WhereNot` y `OrWhereNot` con un valor lógico emiten lo mismo: con `=` o sin operador, la comparación con el literal; con cualquier otro operador, su negación, que se suma a la de `WhereNot`:

```pascal
.Where('activo', True)          // → WHERE activo = true
.Where('activo', '<>', False)   // → WHERE NOT (activo = false)
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

### El conjunto vacío

Un conjunto calculado puede salir vacío, e `IN` sobre nada tiene respuesta: `x IN (a, b)`
es `x = a OR x = b`, así que una lista vacía es la disyunción vacía — **falsa sin
consultar la columna**, y verdadera para `NOT IN`.

Los cuatro verbos resuelven ese caso al añadir la condición, de modo que nunca se emite
un `IN ()`:

```pascal
.WhereIn('precio', [])
// → WHERE (precio IS NULL AND precio IS NOT NULL)      no casa ninguna fila

.WhereNotIn('precio', [])
// → WHERE NOT (precio IS NULL AND precio IS NOT NULL)  casa todas las filas
```

`IS NULL` e `IS NOT NULL` son los únicos predicados del modelo que nunca dan
*desconocido*: exactamente uno de los dos se cumple para cualquier valor. Eso es lo que
hace el grupo falso para todas las filas **incluida aquella cuya columna es NULL**, donde
`precio <> precio` o `precio = NULL` darían *desconocido* y la descartarían en silencio.
Además nombra la columna, así que una sentencia registrada apunta de vuelta a la llamada
que la produjo — cosa que `1 = 0` no haría.

Los dos back-ends leen las mismas cláusulas, así que el lane SQL y el lane en memoria
responden igual.

---

## WHERE EXISTS (subconsulta)

```pascal
var Sub := TQuery.New.From('lineas_pedido').WhereColumns('pedido_id', '=', 'pedidos.id');

TQuery.New.From('pedidos')
  .WhereExists(Sub)
// → WHERE EXISTS (SELECT * FROM lineas_pedido WHERE pedido_id = pedidos.id)
```

---

## WHERE IN (subconsulta)

```pascal
var UsuariosActivos := TQuery.New.From('usuarios').Select('id').Where('activo', True);

TQuery.New.From('pedidos')
  .WhereInQuery('cliente_id', UsuariosActivos)
// → WHERE cliente_id IN (SELECT id FROM usuarios WHERE activo = true)
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
// → WHERE activo = true AND (stock > ? OR pedido_pendiente_permitido = true)
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

## WHERE Agrupado Negado (Callback)

`WhereNot` y `OrWhereNot` también aceptan un callback. El grupo se construye
igual que con `Where` / `OrWhere` y después se envuelve en `NOT (…)`:

```pascal
TQuery.New.From('pedidos')
  .Where('activo', True)
  .WhereNot(function(Q: IQuery): IQuery
    begin
      Result := Q.Where('estado', 'cancelado')
                 .Where('reembolsado', True);
    end)
// → WHERE activo = true AND NOT (estado = ? AND reembolsado = true)
```

Grupo con conector OR:

```pascal
.OrWhereNot(function(Q: IQuery): IQuery
  begin
    Result := Q.Where('rol', 'invitado').OrWhere('bloqueado', True);
  end)
// → OR NOT (rol = ? OR bloqueado = true)
```

> ⚠️ **La negación se aplica al grupo entero, no a cada condición.**
> `NOT (estado = ? AND reembolsado = true)` se cumple cuando falla *cualquiera* de
> las condiciones internas — un pedido cancelado pero no reembolsado sigue
> cumpliendo. Si lo que querías es "ninguna de las dos", niega cada condición:
> `.WhereNot('estado', 'cancelado').WhereNot('reembolsado', True)`.

La negación afecta solo a su propio subárbol: ni las condiciones hermanas ni el
grupo que lo contiene se ven alterados.

```pascal
.WhereNot(function(Q: IQuery): IQuery
  begin
    Result := Q.Where('a', 1);
  end)
.Where('b', 2)
// → WHERE NOT (a = ?) AND b = ?     — b no queda negada
```

Para negar una **sola** condición usa la forma de hoja. Es la idiomática y
produce el mismo SQL que un grupo de un solo elemento:

```pascal
.WhereNot('rol', 'invitado')        // → WHERE NOT (rol = ?)   ← idiomática
```

Un grupo negado cuyo callback no añade ninguna condición se omite de la consulta,
igual que un grupo sin negar vacío — nunca se emite `NOT ()`:

```pascal
.Where('a', 1)
.WhereNot(function(Q: IQuery): IQuery begin Result := Q end)
// → WHERE a = ?
```

Bajo la lógica trivaluada de SQL, un grupo negado que contiene una comparación
contra `NULL` se evalúa como *desconocido*, así que la fila queda **excluida** —
la misma regla que `NOT` ya aplica sobre una condición suelta. El back-end en
memoria la aplica de forma idéntica.

---

## Precedencia, y el árbol que hay detrás

`AND` liga más fuerte que `OR`. Una cadena plana de condiciones no se lee de izquierda a derecha:
primero se agrupan las tiradas consecutivas de `AND`, y esos grupos se unen luego con `OR`.

```pascal
Query.From('users')
  .Where('active', '=', True)
  .Where('age', '>', 18)
  .OrWhere('role', '=', 'admin')
// -> WHERE (active = true AND age > ?) OR role = ?
```

Los paréntesis de ese SQL se **emiten**, no se dan por supuestos. Cada condición lleva el conector
que la une con la anterior, y `Daf.SQLCute.WhereTree` normaliza esa lista plana en un árbol
booleano con la precedencia correcta antes de compilar nada:

```pascal
uses Daf.SQLCute.WhereTree;

var Tree := TWhereTree.Build(Query.Clauses);
try
  // Tree.Kind es TWhereNodeKind.Disjunction para la consulta de arriba
finally
  Tree.Free;
end;
```

| Miembro | Responde |
|---------|----------|
| `TWhereTree.Build(Clauses)` | El árbol de las cláusulas que afectan al WHERE. `nil` cuando no hay ninguna. **Quien lo pide lo posee y lo libera** |
| `TWhereTree.BuildHaving(Clauses)` | La misma normalización sobre `THavingClause`. HAVING no tiene anidamiento ni `NOT`, así que el árbol es una agrupación AND/OR plana |
| `TWhereNode.Kind` | `TWhereNodeKind.Leaf`, `.Neg`, `.Conjunction` o `.Disjunction` — la unit compila con enumerados con ámbito |
| `TWhereNode.Clause` | La condición, en un `Leaf`. **No la posee**: apunta de vuelta al AST de la consulta |
| `TWhereNode.Children` | Los operandos de una conjunción o disyunción, o el único hijo de un `Neg`. Los posee, y los libera recursivamente |

Un grupo por callback baja como un nodo anidado, y uno negado envuelve su nodo en un `Neg`.

**Los dos back-ends consumen el mismo árbol, y eso es el punto.** El compilador ANSI lo recorre y
escribe los paréntesis, así que la precedencia deja de depender de cómo parsee una base de datos
concreta; el compilador en memoria construye a partir de él su predicado de tres valores. Dos
motores que leen un solo árbol no pueden discrepar sobre la precedencia — ver
[SQLCute.InMemory](../../../SQLCute.InMemory/README.es.md).

El recorrido es en profundidad y de izquierda a derecha, lo que conserva el orden original de las
condiciones. Es lo que permite que un consumidor emita bindings posicionales mientras recorre y
obtenga el mismo orden que producía la lista plana.

---

## WhereColumns

Comparación columna a columna (sin binding — los valores se emiten literalmente):

```pascal
.WhereColumns('fecha_inicio', '<', 'fecha_fin')
// → WHERE fecha_inicio < fecha_fin

.OrWhereColumns('precio', '=', 'precio_lista')
```

Su operador es uno de `=`, `<>`, `!=`, `<`, `<=`, `>`, `>=`, `like` y `not like`; `in` y `not in` lanzan `EArgumentException`.

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
// → WHERE activo = true   (solo cuando IncluirInactivos es False)
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
