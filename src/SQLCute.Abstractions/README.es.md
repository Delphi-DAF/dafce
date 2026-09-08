# SQLCute.Abstractions

**🌍 Idioma: [English](README.md) | Español**

El AST de la consulta y los contratos que habla un back-end. Este módulo define **solo interfaces, enumerados y nodos de cláusula** — sin builder, sin compilador, sin motor. Referéncialo cuando escribas un compilador de dialecto o un back-end propio, o cuando una librería tuya pase consultas de un lado a otro sin enlazar ninguno de los dos.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![Licencia](https://img.shields.io/badge/licencia-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Qué hay en este módulo

Dos units, y el orden importa — `Daf.SQLCute` usa `Daf.SQLCute.Clauses`:

| Unit | Contiene |
|------|----------|
| `Daf.SQLCute.Clauses` | Los nodos de cláusula — `TAbstractClause` y sus 22 descendientes — y los enumerados que llevan |
| `Daf.SQLCute` | `IQuery`, `IQueryCompiler`, `TSQLResult`, `TQueryKind`, la factoría `TQuery` y dos helpers |

Las dos units son `{$SCOPEDENUMS ON}`, así que cada valor de enumerado va cualificado: `TWhereOp.Equal`, `TSortDir.Desc`, `TJoinType.Left`.

### Los dos contratos

| Tipo | Papel |
|------|-------|
| `IQuery` | Builder fluido — cada método devuelve `IQuery`, con conteo de referencias, sin `Free` |
| `IQueryCompiler` | `function Compile(const Query: IQuery): TSQLResult` — un método, nada más |

`TSQLResult` es un record con `SQL: string` y `Bindings: TArray<Variant>`. Los marcadores `?` de `SQL` corresponden por posición a los elementos de `Bindings`; un compilador de dialecto puede emitir `$1` o `:p1` y mantener el mismo orden. `TSQLResult.Empty` es el valor cero.

---

## Leer una consulta: el AST es público

`IQuery` expone dos miembros que un back-end lee:

| Miembro | Devuelve |
|---------|----------|
| `Clauses` | `TArray<TAbstractClause>` — los nodos de cláusula, en el orden en que se añadieron |
| `Kind` | `TQueryKind` — `Select`, `Insert`, `Update` o `Delete` |

Las fachadas de DML (`AsInsert`, `AsInsertRows`, `AsInsertFrom`, `AsUpdate`, `AsDelete`) escriben `Kind`, así que un back-end tiene el tipo de sentencia sin recorrer las cláusulas. `Select` es el valor por defecto.

La consulta es la dueña de sus nodos. `Clauses` entrega referencias: no las liberes, y no conserves una más allá del `IQuery` del que salió. `IQuery.Clone` copia en profundidad la consulta entera, y para eso sirve el método `Clone` de cada nodo.

### Los nodos de cláusula

| Área | Nodos |
|------|-------|
| SELECT | `TSelectClause` (un array de `TSelectColumn`), `TDistinctClause` (marcador, sin campos) |
| FROM | `TFromClause`, `TFromRawClause`, `TFromSubqueryClause` |
| JOIN | `TJoinClause`, `TJoinSubqueryClause` |
| WHERE | `TWhereClause`, `TNestedWhereClause`, `TWhereInClause`, `TDateWhereClause` |
| GROUP BY / HAVING | `TGroupByClause`, `THavingClause` |
| ORDER BY | `TOrderByClause` |
| Paginación | `TLimitClause`, `TOffsetClause` |
| Operaciones de conjunto | `TUnionClause` |
| CTE | `TWithClause` |
| SQL literal | `TRawClause` |
| DML | `TInsertClause`, `TUpdateSetClause`, `TDeleteClause` (marcador — la tabla sale de `TFromClause`) |

Una llamada produce un nodo: `WhereIn` añade un `TWhereInClause`, y `AsUpdate` añade un `TUpdateSetClause` por cada par columna–valor.

Tres campos deciden cómo se lee un nodo, y aparecen repartidos por el árbol:

- **`IsRaw`** (`TSelectColumn`, `TOrderByClause`, `TGroupByClause`, `TUpdateSetClause`) — emite el valor literalmente.
- **`Connector: TBoolOp`** (`TWhereClause`, `THavingClause`, `TNestedWhereClause`, `TDateWhereClause`) — cómo enlaza esta condición con la *anterior*. ⚠️ `TWhereInClause.Connector` es un `string`, `'AND'` o `'OR'`.
- **`SubQuery`** — una consulta anidada, de tipo `IInterface` (ver más abajo).

`TJoinClause` lleva tres formas de expresar su condición ON, con una precedencia: manda `ConditionClauses` (venida de un callback), luego `Col1`/`Op`/`Col2`, y por último la cadena `Condition`.

### Las subconsultas son de tipo `IInterface`

`Daf.SQLCute.Clauses` se compila antes que la unit que declara `IQuery`, así que un nodo que guarda una consulta anidada la declara como `IInterface`. Haz el cast al leerla:

```pascal
if Assigned(W.SubQuery) then
begin
  var Inner := W.SubQuery as IQuery;
  // recorrer Inner.Clauses
end;
```

Los nodos con campo de subconsulta: `TWhereClause`, `TJoinSubqueryClause`, `TUnionClause`, `TWithClause`, `TInsertClause` — y `TSelectColumn`, que es un record dentro de `TSelectClause` y no un nodo propio.

⚠️ `TFromSubqueryClause` es la excepción: se declara en `Daf.SQLCute`, junto a `IQuery`, así que su campo `SubQuery` ya es un `IQuery` y no necesita cast.

### La jerarquía es cerrada

Los descendientes de `TAbstractClause` son los que declara este módulo. Un back-end atiende esos y ningún otro — no hay punto de registro para un nodo propio, y nada fuera de este módulo deriva de `TAbstractClause`.

---

## Los enumerados

| Enumerado | Valores |
|-----------|---------|
| `TWhereOp` | `Equal`, `NotEqual`, `Less`, `LessOrEqual`, `Greater`, `GreaterOrEqual`, `Like`, `NotLike`, `&In`, `NotIn`, `IsNull`, `IsNotNull`, `&Between`, `NotBetween`, `&Exists`, `NotExists`, `Raw` |
| `TBoolOp` | `opAnd`, `opOr` |
| `TSortDir` | `Asc`, `Desc` |
| `TNullsOrder` | `Default`, `First`, `Last` |
| `TJoinType` | `Inner`, `Left`, `Right`, `Cross`, `FullOuter` |
| `TUnionKind` | `Union`, `UnionAll`, `Intersect`, `&Except`, `IntersectAll`, `ExceptAll` |
| `TDatePart` | `dpDate`, `dpTime`, `dpYear`, `dpMonth`, `dpDay`, `dpHour`, `dpMinute` |
| `TQueryKind` | `Select`, `Insert`, `Update`, `Delete` |

⚠️ Cuatro valores chocan con palabras reservadas de Delphi y llevan el `&` en su declaración, así que también lo tecleas tú: `TWhereOp.&In`, `TWhereOp.&Between`, `TWhereOp.&Exists`, `TUnionKind.&Except`.

`TNullsOrder.Default` no emite nada y deja la colocación de los NULL a la base de datos, donde cada dialecto tiene su propia regla. En un `TOrderByClause` con `IsRaw` activo, `Nulls` se ignora — un término literal sale tal cual.

### Los dos helpers

`ParseWhereOp(const Op: string): TWhereOp` traduce una cadena de operador, sin distinguir mayúsculas y quitando los espacios de los extremos. Acepta once escrituras:

```
'='  '<>'  '!='  '<'  '<='  '>'  '>='  'like'  'not like'  'in'  'not in'
```

Cualquier otra levanta `EArgumentException`:

```
Unknown SQL operator: "=="
```

Los demás valores de `TWhereOp` — `IsNull`, `IsNotNull`, `&Between`, `NotBetween`, `&Exists`, `NotExists`, `Raw` — no tienen escritura en cadena. Los métodos de `IQuery` que producen esas condiciones los ponen directamente.

`QueryKindName(const K: TQueryKind): string` da el nombre del valor (`'Select'`, `'Insert'`, …), para que una guarda pueda nombrar el tipo que recibió.

---

## Conseguir un `IQuery`: `TQuery.New` necesita una factoría

`TQuery.New` devuelve un builder nuevo, y este módulo no tiene implementación con la que construirlo. `Daf.SQLCute.Core` — en el paquete `SQLCute` — registra una en su sección `initialization`. Enlazar ese paquete es lo que hace funcionar a `TQuery.New`.

Enlaza solo `Daf.SQLCute.Abstractions` y la primera llamada levanta `EInvalidOpException`:

```
IQuery factory not registered. Ensure Daf.SQLCute.Core is linked.
```

Para aportar una implementación propia de `IQuery`, regístrala:

```pascal
uses Daf.SQLCute;

TQuery.RegisterFactory(
  function: IQuery
  begin
    Result := TMyQuery.Create;
  end);
```

`TIQueryFactory` es `reference to function: IQuery`. Manda el último registro. Un argumento `nil` levanta `EInvalidOpException`:

```
Argument Factory cannot be nil
```

⚠️ `RegisterIQueryFactory` es el mismo registro como función suelta y está **deprecado**. Reenvía a `TQuery.RegisterFactory`, así que los dos puntos de entrada se comportan igual; el código nuevo llama al método de clase.

---

## Quién implementa estos contratos

| Paquete | Qué aporta |
|---------|------------|
| [`SQLCute`](../SQLCute/README.es.md) | `IQuery` (`Daf.SQLCute.Core`) e `IQueryCompiler` (`TAnsiSqlCompiler`), más seis compiladores de dialecto que derivan de él: Firebird, MySQL, Oracle, PostgreSQL, SQL Server, SQLite |
| [`SQLCute.InMemory`](../SQLCute.InMemory/README.es.md) | Un back-end que no emite SQL: consume los mismos `Clauses` y `Kind` y los ejecuta sobre colecciones Delphi ya cargadas |
| [`SQLCute.Specification`](../SQLCute.Specification/README.es.md) | Produce un `IQuery` a partir del filtro, la ordenación y la página de una [`Daf.Specification`](../Specification/README.es.md) |

Una consulta completa, de punta a punta, está en [`SQLCute/samples/SQLCuteSample`](../SQLCute/samples/SQLCuteSample/README.md).

---

## Dependencias

Sin dependencias de otros módulos DAF. Solo la RTL de Delphi.

El builder y los compiladores están en [`SQLCute`](../SQLCute/README.es.md).
