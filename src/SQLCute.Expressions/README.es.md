# Daf.SQLCute.Expressions — Una expresión, un descenso

**🌍 Idioma: [English](README.md) | Español**

La mitad de SQL de la capa de expresiones. Toma un árbol escrito con [`Daf.Query.Expressions`](../Query.Expressions/README.es.md) y produce exactamente una cosa: cláusulas WHERE sobre un `IQuery` de SQLCute.

```pascal
uses Daf.SQLCute, Daf.Query.Expressions, Daf.SQLCute.Expressions;

var Q := TExprLowering.Where(TQuery.New.From('Product'),
  (Prop('Price') > 100) and (Prop('Active') = True));

// …una base de datos:
var Sql := TAnsiSqlCompiler.Create.Compile(Q);
// …o la lista que ya tienes cargada — el mismo objeto de consulta:
for var P in TInMemory.Queryable<TProduct>(Q, Loaded) do ...
```

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Por qué hay un solo descenso

Los dos back-ends ya están aguas abajo del AST de SQLCute:

```
              árbol de expresión
                    │  un descenso
                    ▼
          cláusulas SQLCute (IQuery)
                    │
             ┌──────┴──────┐
             ▼             ▼
      TAnsiSqlCompiler   TInMemory.Compile<T>
        (+ dialectos)    .Over(TEnumerable<T>)
             │             │
             ▼             ▼
        texto SQL     ejecución en memoria
```

Así que "una consulta, dos back-ends" es una propiedad que este paquete **hereda**, no una que implemente. Un segundo descenso a un predicado de `Daf.Linq` reimplementaría el compilador en memoria con otra forma y convertiría la paridad entre back-ends en una carga de tests permanente. Aquí no hay tal traductor, y nunca debería haberlo.

---

## Conceptos

| Concepto | Descripción |
|----------|-------------|
| `TExprLowering.Where` | Expresión + `IQuery` → el mismo `IQuery`, con las condiciones |
| `TExprLowering.Apply<T>` | Una especificación completa — filtro, orden, página — sobre una consulta |
| `TInMemoryRepository<T>` | El adaptador `IRepository<T>` en memoria, con caché de planes por regla |
| `ColumnFor` | Miembro → columna, mediante la estrategia aportada o tal cual |
| `WhereOpFor` | Los operadores del modelo mapeados al `TWhereOp` de SQLCute — una tabla |
| `EExprLoweringError` | Un nodo que no es un predicado, o un operador con la aridad equivocada |

---

## La emisión conserva lo que escribiste

Este es el único riesgo de corrección que el resto de la arquitectura no puede absorber, así que conviene decirlo claro.

`TWhereTree.Build` **reconstruye** un árbol booleano desde una lista plana de cláusulas; no lo conserva. Lee esa lista como OR-de-tramos-AND, una gramática con exactamente un nivel de alternancia. Emite `(a or b) and c` como la lista plana `[a AND, b OR, c AND]` y el normalizador lo agrupa como `a OR (b AND c)` — un predicado distinto. Y como los dos back-ends consumen ese mismo árbol normalizado, **los tests de paridad no pueden verlo**: SQL y memoria coinciden, en la respuesta equivocada.

Por eso el emisor anida allí donde la alternancia se profundiza:

| Hijo | dentro de… | se emite como |
|------|------------|---------------|
| `Or` | un tramo AND | un grupo anidado |
| `And` | una lista OR | en línea — el primer elemento lleva OR, el resto AND |
| `And` | un tramo AND | en línea (asociatividad) |
| `Or` | una lista OR | en línea (asociatividad) |

La negación se emite, nunca se reescribe:

| Escrito | Se emite como |
|---------|---------------|
| `not (hoja)` | la negación propia de la hoja — `IsNot`, el operador emparejado (`NOT LIKE`, `NOT IN`, `IS NOT NULL`, `NOT BETWEEN`) o `Negated` |
| `not (subárbol)` | un grupo anidado negado — `WhereNot(Callback)` / `OrWhereNot(Callback)` |

**No hay pasada de De Morgan.** El SQL se lee como la forma que escribiste, no como un equivalente distribuido.

La spec que guarda todo esto es semántica, no estructural: sobre tres formas de expresión, las 3⁴ asignaciones de *Cierto* / *Falso* / *Desconocido* a cuatro hojas deben evaluar igual en el árbol escrito y en la lista de cláusulas normalizada. Comparar árboles sería el test equivocado, porque poner en línea los tramos asociativos cambia legítimamente la forma.

**La precedencia no se implementa aquí.** `a and b or c and d` se emite como cuatro cláusulas planas sin ningún grupo propio del emisor, y el normalizador compartido lo agrupa como `(a and b) or (c and d)`.

---

## Amplitud sin nodos nuevos

Todo lo que SQLCute ya expresa se alcanza bajando a la cláusula que lo dice:

```pascal
Prop('Name').Like('%son%')       // → cláusula LIKE
Prop('Name').StartsWith('And')   // → WhereStarts
Prop('Price').Between(10, 200)   // → cláusula BETWEEN
Prop('Price').IsIn([5, 120])     // → TWhereInClause
Prop('Price').IsNull             // → IS NULL
```

Cada cláusula se produce a través de la superficie fluida de `IQuery`, que es también lo que mantiene `TWhereInClause.Connector` como la cadena `'AND'`/`'OR'` con la que el normalizador lo compara, y lo que mantiene la emisión dentro de los cuatro tipos de cláusula que el normalizador acepta.

Pasar por esa superficie es también la razón de que el conjunto vacío no necesite ningún caso especial aquí:

```pascal
Prop('Price').IsIn([])           // → WHERE (Price IS NULL AND Price IS NOT NULL)
```

`x IN (a, b)` es `x = a OR x = b`, así que una lista vacía es la disyunción vacía — falsa sin consultar la columna, y verdadera para `IsNotIn`. El constructor de SQLCute resuelve eso al añadir la condición, de modo que esta capa baja `IsIn([])` con la misma llamada que usa cualquier otro operador. Sin aridad mínima, sin error y sin rama propia.

---

## El repositorio en memoria

```pascal
var Repo: IRepository<TProduct> := TInMemoryRepository<TProduct>.Create(Loaded);

Repo.List(TSpecification<TProduct>.New
  .Where(Prop('Active') = True)
  .OrderByDesc('Price')
  .Take(10));
```

La fuente se referencia, nunca se posee ni se copia, y los planes se cachean por la clave de la regla — así que N reglas sobre una colección cargada compilan N planes y no recargan nada. Ahí está el punto: **esto no es un doble de test.** Ejecutar varias reglas sobre un conjunto ya cargado es un camino de producción, con el mismo tipo de especificación que iría a una base de datos.

Una consulta base acota el repositorio a un subconjunto, y se clona por regla para no mutar nunca la consulta del llamante:

```pascal
TInMemoryRepository<TProduct>.Create(Loaded, TQuery.New.From('Product').Where('kind', 'tool'));
```

---

## La asimetría documentada

**Traducible no es lo mismo que ejecutable en memoria.** Las cláusulas que el carril en memoria no ha implementado — condiciones con subconsulta o `EXISTS`, `GROUP BY`/`HAVING`, joins, operaciones de conjuntos — compilan a SQL pero lanzan `EInMemoryQueryError` en el carril de lista única.

Ese fallo es **ruidoso y en el momento de compilar el plan**. El adaptador nunca lo captura y nunca filtra la colección en Delphi en su lugar: un recurso silencioso de evaluación en el cliente es fácil de añadir, invisible cuando funciona y catastrófico cuando esconde una consulta que debería haberse reescrito. EF Core lo llevó y lo quitó en la 3.0.

Nótese que una especificación no puede llegar a ese camino por sí sola — el modelo de expresión no tiene `EXISTS` ni operador de SQL crudo, por diseño. Hace falta una consulta base para llegar ahí.

A medida que el back-end en memoria vaya cubriendo esas fases, este paquete las hereda sin cambio propio.

---

## Documentación

| Recurso | Descripción |
|---------|-------------|
| [**Guía de uso**](docs/GUIDE.es.md) | El descenso, la costura de nombres, el adaptador, qué falla y cuándo |
| [CatalogServiceSample](samples/CatalogServiceSample/README.md) | Un servicio de dominio sobre `IRepository<T>`, resuelto desde un `TList<T>` |
| [Daf.Query.Expressions](../Query.Expressions/README.es.md) | El árbol, la superficie de autoría, especificaciones y el puerto |
| [SQLCute In-Memory](../SQLCute.InMemory/README.es.md) | El back-end al que este descenso llega vía `IQuery` |
| [SQLCute](../SQLCute.Abstractions/README.es.md) | El AST y la superficie fluida a la que esto emite |
