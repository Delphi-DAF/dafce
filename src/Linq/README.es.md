# Daf.Linq — Secuencias Perezosas para Delphi

**🌍 Idioma: [English](README.md) | Español**

Biblioteca autónoma de transformación de colecciones: perezosa y con conteo de referencias — en el espíritu de LINQ-to-Objects, las secuencias de Kotlin o los iteradores de Rust. No sabe nada de SQL y tiene valor por sí sola.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![Licencia](https://img.shields.io/badge/licencia-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Conceptos

| Concepto | Descripción |
|----------|-------------|
| `TQuery<T>` | La **superficie de operadores** fluida: un tipo valor que envuelve un `System.IEnumerable<T>` diferido y convierte implícitamente hacia y desde él |
| `IGrouping<TKey, T>` | Un grupo producido por `GroupBy`: una `Key` y sus `Members` |
| `Linq.From<T>` | Punto de entrada — construye un `TQuery<T>` sobre un array, un `TEnumerable<T>`, un `IEnumerable<T>` o un `TSmartArray<T>` |

Las interfaces de Delphi no pueden declarar métodos genéricos: por eso los operadores genéricos (`Select`, `Map`, `Zip`, `GroupBy`) viven en el record `TQuery<T>` y no en la interfaz. Pasa `IEnumerable<T>`; encadena sobre `TQuery<T>`.

---

## Inicio Rápido

```pascal
uses Daf.Linq;

var Adultos := Linq.From<TPersona>(Personas)   // Personas: TList<TPersona>
  .Where(function(P: TPersona): Boolean begin Result := P.Edad >= 18 end)
  .OrderBy(TComparer<TPersona>.Construct(
    function(const A, B: TPersona): Integer begin Result := CompareStr(A.Nombre, B.Nombre) end))
  .Take(10);

for var P in Adultos do                        // nada se ejecutó hasta esta línea
  Writeln(P.Nombre);
```

`Adultos` es un `IEnumerable<TPersona>` — diferido, con conteo de referencias, sin `Free`.

---

## Operadores

| Operador | Tipo | Comportamiento |
|----------|------|----------------|
| `Where` / `Filter` | perezoso | Conserva los elementos que cumplen un `TFilterSpec<T>` (un closure pelado convierte implícitamente) |
| `Select<TResult>` / `Map<TResult>` | perezoso | Proyecta cada elemento con un `TFunc<T, TResult>` |
| `Take(N)` | perezoso | Produce como mucho `N` y **deja de consumir la fuente** |
| `Skip(N)` | perezoso | Descarta los primeros `N` |
| `Zip<TOther, TResult>` | perezoso | Empareja con otra secuencia elemento a elemento hasta que una termina |
| `Distinct(Comparer)` | con buffer | Elimina duplicados por valor; pasa un comparador consciente de NULL para semántica SQL |
| `OrderBy(Comparer)` | con buffer | Ordena mediante un `IComparer<T>` |
| `GroupBy<TKey>(KeySelector)` | con buffer | Devuelve `IEnumerable<IGrouping<TKey, T>>`, preservando el orden de las claves |
| `IsEmpty` | terminal | True si no produce nada; se detiene en el primer elemento |
| `Any(Spec)` | terminal | True si algún elemento cumple; se detiene en la primera coincidencia |
| `FirstOrDefault` / `LastOrDefault` | terminal | El primer / último elemento, o `Default(T)` (o el fallback indicado) |
| `TryFirst(out Value)` | terminal | El primer elemento **y además** un resultado encontrado/no encontrado |
| `Single` / `SingleOrDefault` / `TrySingle` | terminal | El único elemento — ver más abajo |
| `ToArray` / `ToList` / `ToList(List)` / `ToSmartArray` | terminal | Materializa la secuencia |

**Diferido por defecto.** `Where`/`Select`/`Take`/`Skip`/`Zip` se ejecutan elemento a elemento durante la iteración. `OrderBy`/`Distinct`/`GroupBy` necesitan todos los elementos, así que materializan una vez al comenzar la enumeración — pero tampoco antes de ella.

**Los terminales no son destructivos.** Enumeran sin consumir la query, así que el mismo valor sigue usable y un terminal puede llamarse dos veces:

```pascal
var Q := Linq.From<TPersona>(Personas).Where(EsAdulto);
if not Q.IsEmpty then
  for var P in Q do ...          // sigue produciendo todo
```

La contrapartida es que cada llamada a un terminal **vuelve a recorrer la fuente**. Con una fuente costosa o de una sola pasada, materializa una vez (`ToArray` / `ToList`) y consulta eso.

### ¿`First…` o `Single…`?

`Single` afirma una forma: lanza `ELinqError` si la secuencia está vacía **o tiene más de un elemento**. `SingleOrDefault` tolera vacío pero sigue lanzando con más de uno. Úsalos cuando "más de uno" signifique que tus datos están mal y quieras enterarte:

```pascal
Usuarios.Where(EmailEs(X)).SingleOrDefault;   // un email duplicado es un bug → lanza
Log.OrderByDesc(Fecha).FirstOrDefault;        // se esperan muchos → coge el más reciente
```

`TryFirst` / `TrySingle` son las formas que no lanzan. `TryFirst` es además la única manera de distinguir una secuencia vacía de una cuyo primer elemento vale justo `Default(T)` — algo que `FirstOrDefault` no puede expresar en tipos valor.

---

## Un vocabulario, dos estrategias de ejecución

Commons trae una colección ansiosa (`TSmartArray<T>`) anterior a este módulo. Ambas hablan **los mismos** tipos de filtro y de mapper, así que un valor escrito una vez sirve para las dos:

| Concepto | Tipo compartido | Ansioso (`TSmartArray<T>`) | Perezoso (`TQuery<T>`) |
|----------|-----------------|----------------------------|------------------------|
| filtro | `TFilterSpec<T>` | `Where`, `Any` | `Where` / `Filter`, `Any` |
| proyección | `TFunc<T, U>` | `Select<U>` | `Select<U>` / `Map<U>` |
| ansioso → perezoso | — | `Linq.From<T>(SmartArray)` | |
| perezoso → ansioso | — | | `ToSmartArray` |

```pascal
var Adultos: TFilterSpec<TPersona> := ...;
var Nombres: TFunc<TPersona, string> := ...;

Ansioso.Where(Adultos).Select<string>(Nombres);                        // ansioso, Commons
Linq.From<TPersona>(Personas).Where(Adultos).Select<string>(Nombres);  // perezoso, aquí
```

Deliberadamente distinto en cada lado:

- **`Sort` frente a `OrderBy`** — el `Sort` ansioso muta en sitio; `OrderBy` devuelve una secuencia diferida nueva. Semánticas distintas, nombres distintos.
- **Solo perezoso** — `Take`, `Skip`, `Zip`, `GroupBy`, `Distinct` y los terminales de arriba.
- **Solo ansioso** — `Concat`, `BinarySearch`, `FreeItems`, propiedades `First`/`Last`: son asuntos de contenedor, no de secuencia.

**Por qué existen los dos.** El propio `Daf.Rtti` de Commons encadena `Where(...).Select(...)` sobre un `TSmartArray` y está *por debajo* de este módulo en el grafo de dependencias, así que no puede usar la API perezosa sin invertir ese grafo. La duplicación de *implementación* es por tanto estructural, no un descuido — pero el vocabulario no está duplicado.

---

## Patrones Comunes

### Agrupar y resumir

```pascal
for var G in Linq.From<TPersona>(Personas).GroupBy<string>(
      function(P: TPersona): string begin Result := P.Pais end) do
  Writeln(Format('%s: %d', [G.Key, Length(G.Members)]));
```

### Puente con el `TSmartArray<T>` ansioso

Commons ya trae una colección ansiosa respaldada por array. `Daf.Linq` reutiliza su vocabulario en lugar de inventar un segundo, de modo que ansioso y perezoso son dos estrategias de ejecución sobre una misma API:

```pascal
uses Daf.Arrays, Daf.Linq;

var Perezoso := Linq.From<TPersona>(Ansioso);          // TSmartArray<T> → perezoso
var Ansioso2 := Perezoso.Where(EsAdulto).ToSmartArray; // perezoso → TSmartArray<T>
```

### Cortocircuito sobre una fuente grande

```pascal
Linq.From<Integer>(Millon).Where(EsPrimo).Take(5).ToArray;
// deja de recorrer en cuanto produce el quinto primo
```

---

## Notas

- `Reset` sobre los enumeradores subyacentes **no está soportado** — las secuencias son de una sola pasada.
- Se admiten tipos valor como `T` (records, `TNullable<T>`): los adaptadores de `Daf.Enumerable` resuelven el lastre del `GetCurrent: TObject` no genérico mediante una cláusula de resolución de métodos.
- Para ordenación y deduplicación conscientes de NULL, usa los comparadores de `Daf.Comparers` en Commons (`TValueComparers`, `TNullableComparers`).

---

## Documentación

| Recurso | Descripción |
|---------|-------------|
| [**SQLCute In-Memory**](../SQLCute.InMemory/README.es.md) | Ejecuta un `IQuery` de SQLCute sobre colecciones en memoria apoyándose en esta biblioteca |
| [Guía de uso In-Memory](../SQLCute.InMemory/docs/GUIDE.es.md) | Los dos carriles, semántica NULL, alcance de DISTINCT |
| [SQLCute](../SQLCute/README.es.md) | El constructor fluido de consultas SQL |
