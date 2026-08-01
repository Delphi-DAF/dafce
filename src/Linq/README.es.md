# Daf.Linq — Secuencias Perezosas para Delphi

**🌍 Idioma: [English](README.md) | Español**

Biblioteca autónoma de transformación de colecciones: perezosa y con conteo de referencias — en el espíritu de LINQ-to-Objects, las secuencias de Kotlin o los iteradores de Rust. No sabe nada de SQL y tiene valor por sí sola.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![Licencia](https://img.shields.io/badge/licencia-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Conceptos

| Concepto | Descripción |
|----------|-------------|
| `IQueryable<T>` | La **secuencia** diferida y con conteo de referencias — desciende de `System.IEnumerable<T>`, así que `for..in` e interoperabilidad salen gratis |
| `TQuery<T>` | La **superficie de operadores** fluida: un tipo valor que envuelve un `IQueryable<T>` y convierte implícitamente hacia y desde él |
| `IGrouping<TKey, T>` | Un grupo producido por `GroupBy`: una `Key` y sus `Members` |
| `Linq.From<T>` | Punto de entrada — construye un `TQuery<T>` sobre un array, un `TEnumerable<T>`, un `IEnumerable<T>` o un `TSmartArray<T>` |

Las interfaces de Delphi no pueden declarar métodos genéricos: por eso los operadores genéricos (`Select`, `Map`, `Zip`, `GroupBy`) viven en el record `TQuery<T>` y no en la interfaz. Pasa `IQueryable<T>`; encadena sobre `TQuery<T>`.

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

`Adultos` es un `IQueryable<TPersona>` — con conteo de referencias, sin `Free`, y asignable allí donde se espere un `System.IEnumerable<TPersona>`.

---

## Operadores

| Operador | Tipo | Comportamiento |
|----------|------|----------------|
| `Where` / `Filter` | perezoso | Conserva los elementos que cumplen un `TPredicate<T>` |
| `Select<TResult>` / `Map<TResult>` | perezoso | Proyecta cada elemento con un `TFunc<T, TResult>` |
| `Take(N)` | perezoso | Produce como mucho `N` y **deja de consumir la fuente** |
| `Skip(N)` | perezoso | Descarta los primeros `N` |
| `Zip<TOther, TResult>` | perezoso | Empareja con otra secuencia elemento a elemento hasta que una termina |
| `Distinct(Comparer)` | con buffer | Elimina duplicados por valor; pasa un comparador consciente de NULL para semántica SQL |
| `OrderBy(Comparer)` | con buffer | Ordena mediante un `IComparer<T>` |
| `GroupBy<TKey>(KeySelector)` | con buffer | Devuelve `IQueryable<IGrouping<TKey, T>>`, preservando el orden de las claves |
| `ToArray` / `ToList` / `ToSmartArray` | terminal | Materializa la secuencia |

**Diferido por defecto.** `Where`/`Select`/`Take`/`Skip`/`Zip` se ejecutan elemento a elemento durante la iteración. `OrderBy`/`Distinct`/`GroupBy` necesitan todos los elementos, así que materializan una vez al comenzar la enumeración — pero tampoco antes de ella.

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
| [SQLCute](../SQLCute.Abstractions/README.es.md) | El constructor fluido de consultas SQL |
