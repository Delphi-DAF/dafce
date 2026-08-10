# Daf.SQLCute.Specification — Guía de uso

**🌍 Idioma: [English](GUIDE.md) | Español**

← [Volver al README](../README.es.md)

---

## Índice

1. [Emitir una expresión](#emitir-una-expresión)
2. [Emitir una especificación](#emitir-una-especificación)
3. [La costura de resolución de nombres](#la-costura-de-resolución-de-nombres)
4. [Cómo la emisión sigue siendo fiel](#cómo-la-emisión-sigue-siendo-fiel)
5. [El repositorio en memoria](#el-repositorio-en-memoria)
6. [Qué falla, y cuándo](#qué-falla-y-cuándo)

---

## Emitir una expresión

Una llamada, un destino: cláusulas WHERE sobre la consulta que le pasas, que es también lo que devuelve.

```pascal
uses Daf.SQLCute, Daf.Specification, Daf.SQLCute.Specification;

var Q := TExprEmitter.Where(TQuery.New.From('Product'),
  (Prop('Price') > 100) and (Prop('Active') = True));
```

La consulta es tuya: añádele cosas antes o después, compílala, clónala, ejecútala.

```pascal
Q.Select(['Name', 'Price']).OrderBy('Name');
```

No hace falta un FROM. El carril en memoria recibe su fuente como parámetro y nunca resuelve una tabla, así que una regla se puede emitir y ejecutar sobre una lista sin inventar un nombre de tabla:

```pascal
var Q := TExprEmitter.Where(TQuery.New, Prop('Active') = True);
for var P in TInMemory.Elements<TProduct>(Q, Loaded) do ...
```

Añade el FROM cuando la consulta vaya a SQL, que es quien lo necesita.

---

## Emitir una especificación

`Apply<T>` emite la regla completa — filtro, orden y página — sobre una sola consulta, así que el llamante nunca tiene que acordarse de aplicar el orden aparte de la regla a la que pertenece.

```pascal
var Spec := TSpecification<TProduct>.New
  .Where((Prop('Price') > 100) and (Prop('Active') = True))
  .OrderByDesc('Price')
  .Skip(10)
  .Take(5);

var Q := TExprEmitter.Apply<TProduct>(TQuery.New.From('Product'), Spec);
// → WHERE … ORDER BY Price DESC LIMIT 5 OFFSET 10
```

`Skip(0)` y `Take(0)` significan *sin fijar*, así que una regla sin paginar no añade ninguna cláusula.

---

## La costura de resolución de nombres

Por defecto un miembro se convierte en columna tal cual, que es ya la convención vigente. Aporta una `INameResolver` para cambiarlo, y la emisión le pregunta una vez por miembro, en el momento de emitir:

```pascal
TExprEmitter.Where(Q, MyExpr, MyNames, TypeInfo(TProduct));
TExprEmitter.Apply<TProduct>(Q, Spec, MyNames);
```

`ColumnFor` está expuesta para lo mismo fuera del recorrido — por ejemplo para resolver tú una columna de orden o un nombre de tabla:

```pascal
ColumnFor(MyNames, TypeInfo(TProduct), 'Price');   // 'price_eur'
```

Las hojas de expresión llevan nombres de propiedad, nunca de columna. La columna se decide aquí, una vez, y en ningún otro sitio — por eso un cambio de mapeo es un cambio de estrategia y no un barrido por todas tus reglas.

---

## Cómo la emisión sigue siendo fiel

`Daf.SQLCute.WhereTree` lee una lista plana de cláusulas como OR-de-tramos-AND. Por eso el emisor codifica el árbol escrito en esa gramática:

```pascal
(Prop('a') = 1 or Prop('b') = 1) and (Prop('c') = 1)
```

emite un **grupo anidado** para la disyunción más una cláusula plana, y normaliza de vuelta a la conjunción de `(a or b)` con `c`. Ponerla en línea habría producido `a OR (b AND c)` — un predicado distinto en el que los dos back-ends habrían coincidido.

Donde el emisor *no* anida:

```pascal
Prop('a') = 1 and Prop('b') = 1 or Prop('c') = 1 and Prop('d') = 1
```

Cuatro cláusulas planas, ningún grupo. La precedencia de Delphi ya agrupó el árbol como `(a and b) or (c and d)`, y el normalizador compartido reproduce exactamente eso desde la lista plana. La capa no contiene lógica de precedencia propia.

Negación:

```pascal
not (Prop('Name').Like('%son%'))         // una cláusula, operador NOT LIKE — sin grupo
not (Prop('a') = 1)                      // una cláusula con IsNot — sin grupo
not ((A and B) or (C and D))             // un grupo anidado negado
```

La última emite `NOT (…)` alrededor de la forma escrita. No hay pasada de De Morgan, así que el SQL sigue pareciéndose a lo que escribiste.

La sensibilidad a mayúsculas viaja con la familia LIKE:

```pascal
Prop('Name').Like('%son%')         // LOWER(Name) LIKE LOWER(?)
Prop('Name').Like('%son%', True)   // Name LIKE ?
```

---

## El repositorio en memoria

```pascal
var Loaded := LoadEverything;                       // un TObjectList<TProduct>
var Repo: IRepository<TProduct> := TInMemoryRepository<TProduct>.Create(Loaded);

var Cheap := TSpecification<TProduct>.New.Where(Prop('Price') < 50);
var Rich  := TSpecification<TProduct>.New.Where(Prop('Price') > 500);

Repo.List(Cheap);
Repo.List(Rich);
Repo.List(Cheap);      // el plan de Cheap se reutiliza; no se recarga nada
```

Los planes se cachean por `TSpecification<T>.Key`, así que una regla repetida no compila nada la segunda vez. La fuente se referencia, no se posee — libera la lista tú, después del repositorio.

Acotar a un subconjunto requiere una consulta base, que se clona por regla para no mutar la tuya:

```pascal
var Tools := TQuery.New.From('Product').Where('kind', 'tool');
var Repo: IRepository<TProduct> := TInMemoryRepository<TProduct>.Create(Loaded, Tools);
```

La propiedad `Compiled` de la clase concreta informa de cuántos planes se han construido, que es lo que convierte la reutilización de planes en una spec y no en un comentario.

---

## Qué falla, y cuándo

| Situación | Qué ocurre |
|-----------|------------|
| Un nombre de miembro que no resuelve a nada | `EExprMemberError`, donde se escribe la expresión (en `Daf.Specification`) |
| Un nodo que no es predicado donde va una condición | `EExprEmissionError` al emitir |
| Un operador con el número de operandos equivocado | `EExprEmissionError` al emitir |
| Una consulta que el carril en memoria no puede ejecutar | `EInMemoryQueryError` al compilar el plan |

Nada se omite y nada degrada. Una cláusula descartada en silencio durante la emisión *ensancharía* la consulta, que es el peor modo de fallo de un filtro — así que un nodo que el emisor no entiende lanza.

Y la asimetría que conviene repetir: **traducible no es lo mismo que ejecutable en memoria.** Las condiciones con subconsulta o `EXISTS`, `GROUP BY`/`HAVING`, los joins y las operaciones de conjuntos compilan a SQL pero lanzan en el carril de lista única. El adaptador nunca captura ese error y nunca filtra la colección en Delphi en su lugar.
