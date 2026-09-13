# SQLCute In-Memory — Una Consulta, Dos Back-Ends

**🌍 Idioma: [English](README.md) | Español**

Un segundo back-end para SQLCute. Donde `IQueryCompiler` convierte un `IQuery` en texto SQL, `TInMemory` convierte **el mismo** `IQuery` en un plan ejecutable sobre colecciones Delphi ya cargadas.

La ganancia es la **simetría**: un mismo objeto consulta filtra una base de datos o un `TList<T>` que ya tienes en memoria — lo que hace posible filtrar listas cacheadas en cliente, o respaldar repositorios con una lista simple en los tests, sin un segundo lenguaje de consulta.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![Licencia](https://img.shields.io/badge/licencia-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Conceptos

| Concepto | Descripción |
|----------|-------------|
| `TInMemory` | Punto de entrada — compila un `IQuery` sobre el motor en memoria |
| `IInMemoryPlan<T>` | Un plan compilado reutilizable; `Over(Source)` lo ejecuta sobre cualquier fuente de `T` |
| `IQueryContext` | Catálogo por llamada que mapea nombres de tabla a fuentes (nunca global) |
| `IRow` | El modelo de fila dinámico (`Get` / `Has` / `Columns`, valores como `TValue`) |
| `TRttiRow<T>` / `TDictRow` / `TJoinRow` | Adaptadores de fila: vista sobre objeto, bolsa genérica, compuesto de join |
| `EInMemoryQueryError` | Se lanza cuando una consulta no puede ejecutarse en memoria — siempre fallo temprano, nunca un resultado incorrecto |

Los resultados son `IEnumerable<T>` de [`Daf.Linq`](../Linq/README.es.md) — diferidos, con conteo de referencias y recorribles con `for..in`.

---

## Inicio Rápido

```pascal
uses Daf.SQLCute, Daf.SQLCute.InMemory;

var Q := TQuery.New.From('people')
  .Where('age', '>', 18)
  .OrderBy('name')
  .Limit(10);

// Una base de datos…
var R := TAnsiSqlCompiler.Create.Compile(Q);       // SQL + bindings

// …o la lista que ya cargaste — el mismo objeto consulta.
for var P in TInMemory.Elements<TPerson>(Q, Personas) do
  Writeln(P.Name);
```

Los nombres de columna tienden un puente entre estilos: el `user_id` de SQL resuelve al miembro Delphi `UserId`.

---

## Los Dos Carriles

El compilador inspecciona el AST y elige carril.

### Carril rápido tipado — una lista, sin envoltorios

Una consulta sobre una sola fuente, sin JOIN, subconsulta, operación de conjuntos ni proyección, compila a un predicado y un comparador **sobre el propio `T`**. No se reserva ningún `IRow`, no hace falta catálogo, y el resultado sigue siendo `T`:

```pascal
var Plan := TInMemory.Compile<TPersona>(Q);        // compila una vez
for var P in Plan.Over(Personas) do ...            // …ejecuta sobre cualquier fuente de T
for var P in Plan.Over(OtrasPersonas) do ...
```

### Carril general — varias fuentes, vía contexto por llamada

JOINs, subconsultas, operaciones de conjuntos y GROUP BY necesitan fuentes con nombre. Se resuelven mediante un `IQueryContext` suministrado en tiempo de ejecución — explícito, nunca un registro global:

```pascal
var Ctx := TQueryContext.Create;
try
  Ctx.Map<TPersona>('people', Personas).Map<TPedido>('orders', Pedidos);
  for var R in TInMemory.Rows(
    TQuery.New.From('people').Join('orders', 'people.id', 'orders.user_id'), Ctx) do
    Writeln(R.Get('people.name').AsString, ' ', R.Get('orders.total').AsInteger);
finally
  Ctx.Free;                                        // el contexto lo posee quien lo crea
end;
```

Ejecutar una consulta multi-fuente por el punto de entrada de lista única lanza `EInMemoryQueryError` en vez de devolver filas incorrectas en silencio.

---

## Formas de Salida

| Llamas a | Obtienes |
|----------|----------|
| `TInMemory.Elements<T>(Q, Source)` | `IEnumerable<T>` — los elementos originales (no admite proyección) |
| `TInMemory.Rows<T>(Q, Source)` | `IEnumerable<IRow>` — filas dinámicas, proyectadas si la consulta tiene `SELECT` |
| `TInMemory.Project<TSource, TResult>(Q, Source)` | `IEnumerable<TResult>` — cada fila materializada en una clase de resultado por RTTI |
| `TInMemory.Rows(Q, Context)` | `IEnumerable<IRow>` — el carril general |
| `TInMemory.Project<TResult>(Q, Context)` | `IEnumerable<TResult>` — el carril general, materializado |

```pascal
// Proyección a una clase de usuario; una columna NULL deja el miembro TNullable en IsNull.
for var V in TInMemory.Project<TPersona, TPersonaView>(
    TQuery.New.From('people').Select(['name', 'age']), Personas) do
  if V.Age.HasValue then Writeln(V.Name, ': ', V.Age.Value);
```

---

## Semántica SQL

El motor en memoria sigue la semántica ANSI para que ambos back-ends respondan igual a la misma pregunta:

- **Lógica de tres valores.** Una comparación contra NULL es *desconocida*, no falsa; `AND`/`OR`/`NOT` se combinan según las tablas de verdad de SQL; una fila se conserva solo cuando el predicado es *verdadero*. Así, `WhereNot('age','>',18)` excluye una edad NULL — exactamente como hace una base de datos.
- **Precedencia en WHERE.** `AND` liga más fuerte que `OR`. Ambos back-ends parten de un mismo normalizador compartido (`Daf.SQLCute.WhereTree`), así que no pueden discrepar; el lado SQL emite paréntesis explícitos.
- **DISTINCT** deduplica sobre las columnas *de salida*, por valor, y los NULL se comparan como iguales.
- **Los agregados** ignoran los NULL (`COUNT(col)`, `SUM`, `AVG`, `MIN`, `MAX`); `COUNT(*)` cuenta filas.
- **GROUP BY** agrupa todas las claves NULL en un único grupo.
- **ORDER BY** trata NULL como el valor más pequeño **por defecto** —nulls primero en ascendente, al final en descendente— y una consulta lo cambia por columna con `NullsFirst` / `NullsLast`, que el plan respeta.

Las reglas completas, con ejemplos, están en la [guía de uso](docs/GUIDE.es.md).

---

## Limitaciones

El back-end es **de solo lectura** — el DML (`INSERT`/`UPDATE`/`DELETE`) sigue siendo exclusivo de SQL. Lo comprueba por el **tipo declarado** de la consulta, no detectando una cláusula de DML: todo lo que no sea una recuperación se rechaza nombrando el tipo que es. Por eso la guarda ya cubre un tipo de sentencia que todavía no existe — uno nuevo se rechaza igual, sin una lista negra cláusula a cláusula que haya que acordarse de actualizar. Los fragmentos de SQL crudo (`FROM` crudo, `ORDER BY` crudo, `ON` crudo de JOIN, `CombineRaw`), las CTE (`WITH`), los JOIN sobre subconsulta y los predicados de fecha (`WhereDate`, `WhereTime`, `WhereDatePart`) no pueden evaluarse en memoria y lanzan `EInMemoryQueryError`. La ejecución es un recorrido ingenuo — sin índices ni optimización.

---

## Documentación

| Recurso | Descripción |
|---------|-------------|
| [**Guía de uso**](docs/GUIDE.es.md) | Carriles, modelo de fila, proyección, semántica NULL, DISTINCT |
| [InMemoryQuerySample](samples/InMemoryQuerySample/README.md) | Filtrar, ordenar, paginar y proyectar un `TList<T>` |
| [InMemoryJoinSample](samples/InMemoryJoinSample/README.md) | Dos fuentes unidas mediante un contexto por llamada |
| [Daf.Linq](../Linq/README.es.md) | La biblioteca de secuencias perezosas sobre la que se construyen los planes |
| [SQLCute](../SQLCute/README.es.md) | El constructor fluido que produce el AST |
| [Compiladores de dialecto](../SQLCute/README.es.md) | El back-end SQL que este refleja |
