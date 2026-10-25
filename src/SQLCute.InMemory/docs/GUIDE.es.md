# SQLCute In-Memory — Guía de Uso

**🌍 Idioma: [English](GUIDE.md) | Español**

---

## Introducción

SQLCute construye un AST portable (`IQuery.Clauses`) y lo compila con un back-end. Hay dos:

| Back-end | Unidad | Produce |
|----------|--------|---------|
| SQL | `Daf.SQLCute.Compiler` (+ dialectos) | `TSQLResult` — texto SQL y bindings |
| En memoria | `Daf.SQLCute.InMemory` | Un plan ejecutable sobre colecciones Delphi |

Ambos leen las mismas cláusulas, en solo lectura. Ninguno modifica la consulta, así que un mismo `IQuery` puede enviarse a la base de datos y ejecutarse sobre una lista cacheada sin más.

```pascal
uses Daf.SQLCute, Daf.SQLCute.Compiler, Daf.SQLCute.InMemory;

var Q := TQuery.New.From('people').Where('age', '>', 18).OrderBy('name');

var Compiler: IQueryCompiler := TAnsiSqlCompiler.Create;  // el compilador es contado: ha de tener dueño
var Sql := Compiler.Compile(Q);        // → SQL + bindings
var Filas := TInMemory.Elements<TPersona>(Q, Personas); // → IEnumerable<TPersona>
```

Existe un tercer punto de entrada, `Linq.From<T>` en [`Daf.Linq`](../../Linq/README.es.md), para entrar al mismo motor de ejecución directamente con closures tipados y sin SQLCute de por medio. Los tres convergen en `IEnumerable<T>`.

---

## Índice

1. [Elegir carril](#elegir-carril)
2. [Planes y fuentes](#planes-y-fuentes)
3. [El modelo de fila](#el-modelo-de-fila)
4. [Proyección tipada](#proyección-tipada)
5. [Semántica NULL](#semántica-null)
6. [DISTINCT](#distinct)
7. [Consultas multi-fuente](#consultas-multi-fuente)
8. [Errores y limitaciones](#errores-y-limitaciones)
9. [Paridad con el back-end SQL](#paridad-con-el-back-end-sql)

---

## Elegir carril

El compilador analiza el AST y elige uno de los dos carriles de ejecución. No lo seleccionas tú: solo eliges el punto de entrada que encaja con tus datos.

| | Carril rápido tipado | Carril general |
|---|---|---|
| Fuente | un `TList<T>` / array / `IEnumerable<T>` | un `IQueryContext` con fuentes nombradas |
| Representación de fila | el propio `T` — sin envoltorio | compuestos `IRow` |
| Soporta | WHERE, ORDER BY, LIMIT/OFFSET, DISTINCT | todo, incl. JOIN, GROUP BY, subconsultas, conjuntos |
| Salida | `IEnumerable<T>` | `IEnumerable<IRow>` o `IEnumerable<TResult>` |
| Punto de entrada | `TInMemory.Elements<T>` / `Compile<T>` | `TInMemory.Rows` / `Project<TResult>` |

Cualquiera de estos elementos fuerza el carril general, y usarlo con una lista única lanza `EInMemoryQueryError`:

- `Join` / `LeftJoin` / … (cualquier join)
- `Union` / `UnionAll` / `Intersect` / `Except`
- un WHERE con subconsulta (`WhereInQuery`, `WhereExists`, …)
- un `FROM` derivado o crudo
- `WITH` (CTE)
- `GroupBy` / `Having`

Una proyección `SELECT` no fuerza el carril general, pero sí descarta `Elements<T>` (que preserva `T`): usa `Rows<T>` o `Project<TSource, TResult>`.

---

## Planes y fuentes

`Compile<T>` produce un plan reutilizable. Compila una vez, ejecuta muchas:

```pascal
var Plan := TInMemory.Compile<TPersona>(TQuery.New.From('people').Where('age', '>', 18));
for var P in Plan.Over(EquipoA) do ...
for var P in Plan.Over(EquipoB) do ...
```

`Over` acepta un `TEnumerable<T>` (por tanto `TList<T>`, `TObjectList<T>`, …), un `TArray<T>` o un `IEnumerable<T>`. No se copia nada: el plan lee la fuente mientras itera.

`TInMemory.Elements<T>(Q, Source)` es azúcar para `Compile<T>(Q).Over(Source)`.

**La ejecución es diferida.** Construir el queryable no recorre nada; el trabajo empieza con el `for..in`. `Limit` cortocircuita — un `Limit(10)` sobre un millón de elementos deja de consumir la fuente tras el décimo resultado.

### Nombres de columna

Las columnas se emparejan con miembros mediante una clave normalizada: se descarta el prefijo de tabla, se ignoran mayúsculas y se eliminan los guiones bajos. Así, la columna SQL `user_id` resuelve al miembro Delphi `UserId`, y `people.name` resuelve a `Name`.

Sobre un contexto el prefijo no se descarta: nombra el lado del que se lee la columna (ver [El modelo de fila](#el-modelo-de-fila)).

---

## El modelo de fila

Allí donde no existe un tipo Delphi estático — filas de join, filas de grupo, bolsas genéricas — el motor transporta los valores como `IRow`:

```pascal
IRow = interface
  function Get(const Column: string): TValue;
  function Has(const Column: string): Boolean;
  function Columns: TArray<string>;
end;
```

Los valores son `TValue`, no `Variant`: RTTI es nativo en `TValue`, y `TValue` transporta records y enumerados que un `Variant` no puede. Los bindings `Variant` de SQLCute se convierten una sola vez, al construir el plan — nunca por fila.

| Adaptador | Respaldado por | Notas |
|-----------|----------------|-------|
| `TRttiRow<T>` | un objeto o record | sin copia; búsquedas de miembro cacheadas por (tipo, columna) |
| `TDictRow` | un mapa nombre → `TValue` | sin RTTI; `TDictRow.Make(['age','name'], [30, 'Bob'])` |
| `TJoinRow` | filas hijas con alias | resuelve `orders.total` al lado `orders` |
| `TNullRow` | nada | el lado todo-NULL de un outer join, con las columnas de la fuente a la que sustituye |

`TJoinRow` resuelve un nombre cualificado solo contra el alias correspondiente, y uno sin cualificar contra el primer lado que lo tenga.

```pascal
var Row: IRow := TRttiRow<TPersona>.Create(Accessors, Persona);
Row.Get('people.name').AsString;    // prefijo descartado → Name
```

---

## Proyección tipada

Con una clase de resultado, cada fila final se materializa por RTTI — columna o alias de salida a miembro, con el `TValue` convertido al tipo del miembro. Funciona tanto con propiedades como con campos públicos.

```pascal
type
  TPersonaView = class
  private
    FName: string;
    FAge: TNullable<Integer>;
  public
    property Name: string read FName write FName;
    property Age: TNullable<Integer> read FAge write FAge;
  end;

for var V in TInMemory.Project<TPersona, TPersonaView>(
    TQuery.New.From('people').Select(['name', 'age']), Personas) do
  ...
```

Los alias se mapean a miembros por nombre: `SelectAs('full_name', 'display_name')` puebla un miembro `DisplayName`.

Sin clase de resultado, la secuencia produce `IRow` (`Rows<T>` / `Rows`), y una consulta de fuente única sin proyección produce el `T` original (`Elements<T>`).

Las instancias materializadas pertenecen a quien llama — recógelas en un `TObjectList<TResult>` si deben sobrevivir al bucle.

---

## Semántica NULL

El NULL de SQL aparece en tres sitios, y el motor los trata de forma coherente — esa coherencia es lo que mantiene de acuerdo a los dos back-ends.

Un miembro se lee como NULL cuando es un `Variant` que guarda `Null` o un `TNullable<T>` ausente; un `TNullable<T>` presente se lee como su valor. Vale en WHERE, ORDER BY, DISTINCT, GROUP BY y los agregados.

### Lógica de tres valores

La evaluación de predicados es de tres valores. Una comparación con un operando NULL es *desconocida*, no falsa, y una fila se conserva solo cuando el predicado resulta *verdadero*.

| `A` | `NOT A` |
|---|---|
| verdadero | falso |
| falso | verdadero |
| desconocido | desconocido |

| `AND` | verdadero | falso | desconocido |
|---|---|---|---|
| **verdadero** | verdadero | falso | desconocido |
| **falso** | falso | falso | falso |
| **desconocido** | desconocido | falso | desconocido |

| `OR` | verdadero | falso | desconocido |
|---|---|---|---|
| **verdadero** | verdadero | verdadero | verdadero |
| **falso** | verdadero | falso | desconocido |
| **desconocido** | verdadero | desconocido | desconocido |

Una comparación negada sobre una columna NULL muestra el efecto:

```pascal
TQuery.New.From('people').WhereNot('age', '>', 18)
// age = NULL → (age > 18) es desconocido → NOT desconocido es desconocido → fila excluida.
// Una base de datos también la excluye.
```

`WhereNull` / `WhereNotNull` comprueban la presencia explícitamente y siempre responden verdadero o falso.

### Precedencia

`AND` liga más fuerte que `OR`, como en SQL. `Where('a',1).Where('b',2).OrWhere('c',3).Where('d',4)` significa `(a AND b) OR (c AND d)`, nunca un plegado de izquierda a derecha. Ambos back-ends parten del mismo normalizador (`Daf.SQLCute.WhereTree`), así que la precedencia tiene una única fuente de verdad; el lado SQL emite los paréntesis explícitamente.

### Agregados

`SUM`, `AVG`, `MIN`, `MAX` y `COUNT(col)` ignoran los NULL; `COUNT(*)` cuenta filas. Un agregado sobre un grupo sin valores no NULL da NULL.

### GROUP BY

Todas las claves NULL caen en exactamente un grupo.

### ORDER BY

Un `OrderBy` sin cualificar trata NULL como el valor más pequeño: nulls primero en ascendente, al final en descendente.

Una consulta puede anularlo por columna, y el plan lo respeta:

```pascal
TInMemory.Elements<TPersona>(
  TQuery.New.From('personas').OrderBy('edad').NullsLast, Personas);   // NULL al final
```

Sirven ambas formas fluidas (`NullsFirst` / `NullsLast`, o la sobrecarga `OrderBy(col, TNullsOrder.Last)`); el compilador lee la opción de la cláusula en `TOrderSpec.Nulls`.

Un `OrderBy` sobre el alias de una columna de SELECT ordena por esa columna, como SQL:

```pascal
TInMemory.Rows<TPersona>(
  TQuery.New.From('personas').SelectAs('edad', 'anios').OrderByDesc('anios'), Personas);
```

> **El único punto donde los dos back-ends aún pueden discrepar.** Una ordenación *sin cualificar* sobre una columna nullable sigue el valor por omisión de cada lado: NULL-es-el-menor en memoria, y lo que haga el dialecto en SQL — donde Postgres y Oracle hacen lo contrario (nulls al final en ascendente). Este carril no conoce dialectos. **Cualifica la ordenación —`NullsFirst` / `NullsLast`— para obtener resultados idénticos en ambos.**

### Materialización

Una columna NULL proyectada sobre un miembro `TNullable<T>` lo deja en `IsNull`, distinguiendo "sin valor" de un `0` o `''` presentes. Un NULL proyectado sobre un miembro simple deja ese miembro en su valor por defecto.

```pascal
if V.Age.HasValue then Writeln(V.Age.Value) else Writeln('(sin edad)');
```

---

## DISTINCT

`Distinct` deduplica sobre las columnas **de salida** — el conjunto proyectado si la consulta tiene `SELECT`, y todas las columnas en caso contrario — comparando **por valor**, nunca por identidad de referencia. Los NULL se comparan como iguales, así que los NULL duplicados colapsan en uno.

```pascal
TQuery.New.From('people').Select('country').Distinct
// una fila por país distinto, no una por persona
```

En el carril rápido tipado sin proyección, "todas las columnas" significa todas las propiedades legibles públicas o publicadas de `T` cuando `T` es una clase, y todos los campos cuando `T` es un record — un campo público de una clase no cuenta. Se comparan por valor, así que dos instancias distintas de `TPersona` con valores iguales son una sola fila.

---

## Consultas multi-fuente

Mapea cada fuente nombrada en un contexto por llamada. El contexto es un objeto normal que tú posees:

```pascal
var Ctx := TQueryContext.Create;
try
  Ctx.Map<TPersona>('people', Personas)
     .Map<TPedido>('orders', Pedidos);
  // …ejecutar consultas…
finally
  Ctx.Free;
end;
```

`Map` también acepta directamente un `TArray<IRow>`, para datos que no son un grafo de objetos Delphi. Referenciar un nombre que el contexto no mapea lanza `EInMemoryQueryError` indicando la fuente ausente.

### JOINs

Se soportan INNER, LEFT, RIGHT, CROSS y FULL OUTER, con la condición ON dada como par de columnas o como callback. Los lados sin correspondencia se leen como NULL.

```pascal
TQuery.New.From('people').LeftJoin('orders', 'people.id', 'orders.user_id')
// las personas sin pedidos siguen apareciendo; orders.* se lee como NULL
```

Una unión por columnas con `like` o `not like`, y `WhereColumns` con ellos, toman el patrón de la columna derecha de la misma fila:

```pascal
TQuery.New.From('filas').WhereColumns('nombre', 'like', 'patron')
// casa una fila cuyo nombre encaja en el patrón que guarda esa misma fila
```

### GROUP BY, agregados, HAVING

La agrupación emite una fila plana por grupo con las columnas de clave y las de agregado, que es la forma que devuelve SQL:

```pascal
TQuery.New.From('people')
  .Select('country').SelectCount('*', 'n')
  .GroupBy('country')
  .Having('n', '>', 1)
// filas: (country, n)
```

El `GroupBy` de la propia biblioteca de secuencias produce el `IGrouping<TKey, T>` más rico —la clave y sus miembros— para el código que pasa por [`Daf.Linq`](../../Linq/README.es.md) directamente.

### Subconsultas y operaciones de conjuntos

`WhereInQuery` / `WhereNotInQuery` y EXISTS / NOT EXISTS se evalúan contra el contexto; cada subconsulta no correlacionada se ejecuta una vez y se cachea. `Union`, `UnionAll`, `Intersect`, `Except` y las variantes `ALL` combinan conjuntos de resultados, comparando filas por el valor de sus columnas de salida con los NULL como iguales.

---

## Errores y limitaciones

Todo lo que el motor no puede ejecutar fielmente lanza `EInMemoryQueryError`, en lugar de devolver filas aproximadas. Esto se rechaza al construir el plan:

| Rechazado | Motivo |
|-----------|--------|
| `INSERT` / `UPDATE` / `DELETE` | el back-end es de solo lectura; el DML sigue siendo de SQL |
| `FROM` de tabla derivada, `FROM` crudo, `ORDER BY` crudo, `ON` crudo de JOIN, `CombineRaw` | el texto SQL crudo no tiene significado en memoria, y un `FROM` es una fuente mapeada |
| `WITH` (CTE), JOIN sobre subconsulta | no soportado |
| `WhereDate`, `WhereTime`, `WhereDatePart` y sus `Or*` | los casts de fecha son propios de cada dialecto; solo SQL |
| columnas `SELECT` crudas o con subconsulta | no soportado |
| una consulta multi-fuente ejecutada con una sola lista | necesita un contexto |
| una consulta con proyección vía `Elements<T>` | usa `Rows` o `Project` |

Una columna que la fila no lleva lanza `EInMemoryQueryError` nombrándola al ejecutar la consulta, en SELECT, WHERE, ORDER BY, GROUP BY, HAVING o un agregado: una errata no se lee como NULL. Un nombre cualificado se lee solo del lado que nombra su prefijo, así que una subconsulta correlacionada —la que nombra una columna de la consulta de fuera— se rechaza igual.

Dos límites vienen de las fuentes de `IRow`, cuyas columnas no las declara un tipo:

- Cada fila tiene que llevar las columnas que lee la consulta; una fila a la que le falta una lanza donde se lee.
- Mapeada sin filas, la fuente no tiene columnas conocidas, y como lado ausente de un outer join acepta cualquier columna.

La ejecución es un recorrido completo ingenuo — sin índices ni optimizador.

---

## Paridad con el back-end SQL

`Parity.Feat.pas`, en la carpeta `test/` de este módulo, es el arnés de simetría: un conjunto de consultas compartido pasa por el compilador SQL y por los tres carriles en memoria sobre un mismo juego de datos sembrado, y todos deben reproducir el mismo conjunto de resultados declarado. También verifica que ningún back-end modifica el AST, que los bindings casan con sus marcadores, y que el SQL emitido lleva exactamente los paréntesis de precedencia con los que se construyó el predicado en memoria.

Ese conjunto es la guardia de la promesa "una consulta, dos back-ends".
