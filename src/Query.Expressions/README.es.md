# Daf.Query.Expressions — Consultas tipadas, sin SQL

**🌍 Idioma: [English](README.md) | Español**

Una condición de consulta, escrita como código Delphi con operadores, que sigue siendo un **árbol inspeccionable** en lugar de convertirse en una clausura.

```pascal
var Spec := TSpecification<TProduct>.New
  .Where((Prop('Price') > 100) and (Prop('Active') = True))
  .OrderByDesc('Price')
  .Take(10);
```

Ese valor es un objeto de dominio: se puede afirmar en un test unitario, pasar por un puerto, guardar, registrar — y más tarde emitirlo a SQL con [`Daf.SQLCute.Expressions`](../SQLCute.Expressions/README.es.md). **Este paquete no contiene SQL y no referencia SQLCute.** Un servicio de dominio que dependa de él no enlaza nada de eso.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Conceptos

| Concepto | Descripción |
|----------|-------------|
| `IExpr` | Un nodo del árbol de expresión; con conteo de referencias, sin `Free` manual |
| `TExprKind` | El conjunto acotado de nodos: `Compare` / `Logical` / `Unary` + `Member` / `Value` / `Param` |
| `TCompareOp` / `TLogicalOp` / `TUnaryOp` | Los operadores propios del modelo — sin SQL crudo, sin `EXISTS` |
| `TExpr` | El registro de autoría: operadores sobrecargados, LIKE / BETWEEN / IN |
| `Prop(Name)` | Una referencia a miembro por nombre — el camino siempre disponible |
| `TEntityExpr<T>` | Proxy RTTI: los miembros de una entidad como hojas, validados donde se escriben |
| `TPropExpr<T>` | La hoja del metamodelo **opcional**, para autoría comprobada en compilación |
| `INameResolver` | La única costura: miembro → columna, tipo → tabla |
| `TSpecification<T>` | Una regla reutilizable: filtro + orden + paginación |
| `IRepository<T>` | El puerto; `T` **no** está restringido a ninguna clase base |
| `ExprToText` | Una representación canónica — para aserciones, claves de caché y diagnóstico |

---

## Por qué es un árbol y no una clausura

Commons ya tiene un filtro basado en clausuras, `TFilterSpec<T>` en `Daf.Expression`, y `Daf.Linq` lo ejecuta sobre secuencias ya cargadas. Este paquete toma prestada esa **forma** de sobrecarga de operadores y ninguno de sus tipos, porque una clausura es opaca: se puede ejecutar, nunca traducir.

| Mundo | Filtro | Se ejecuta sobre |
|-------|--------|------------------|
| SQL, o una consulta para ambos back-ends | este paquete → `IQuery` | una base de datos *o* una colección cargada |
| Objetos ya cargados, código Delphi arbitrario | `Daf.Linq` + `TFilterSpec<T>` | una secuencia materializada |

Son hermanos que sirven a mundos distintos. Por eso `TSpecification<T>` **no tiene sobrecarga que acepte una clausura**: una especificación que no se puede traducir debe ser irrepresentable, no evaluarse en el cliente en silencio.

---

## El modelo es dirigido por datos

Seis tipos de nodo, tres enumerados de operadores. Añadir una comparación es un **valor nuevo del enumerado**, nunca una clase nueva — el mismo nodo binario lleva todos los operadores, y `Like`, `Between` e `In` se bajan a la cláusula de SQLCute que ya lo dice.

```pascal
Prop('Name').Like('%son%')          // un nodo, operador Like
Prop('Price').Between(10, 200)      // un nodo, operador Between
Prop('Price').IsIn([5, 120])        // un nodo, operador In
Prop('Price').IsNull
not (Prop('Active') = True)
```

Los valores viajan como un único `Variant` — la representación que ya llevan las cláusulas de SQLCute — así que no se reimplementa ningún modelo de parámetros con accesores tipados. NULL es el `TNullable` de Commons, estrechado en el borde de la autoría:

```pascal
Prop('Id')  = SomeGuid                       // sobrecarga tipada → cadena canónica
Prop('Age') = TExprValue.From<Integer>(N)    // TNullable → su valor, o Null
```

El estrechamiento ocurre **donde se escribe**, nunca dentro de la emisión: una hoja con un valor más rico dejaría al carril en memoria comparando un registro mientras SQL compara su texto.

---

## Dos caminos de autoría, un mismo nodo

El camino en tiempo de ejecución siempre es suficiente y valida cada miembro contra la entidad:

```pascal
var P := TEntityExpr<TProduct>.Create(nil);
P.Prop('Price') > 100
P.Prop('Pricee')     // lanza EExprMemberError, nombrando el miembro y el tipo
```

Para una entidad donde compensa, un metamodelo escrito a mano añade seguridad ante erratas y autocompletado:

```pascal
ProductMeta = class
public
  class var Price: TPropExpr<TProduct>;
  class constructor Create;
end;

class constructor ProductMeta.Create;
begin
  Price := TPropExpr<TProduct>.Create('Price');   // el nombre de la PROPIEDAD
end;

// ProductMeta.Price > 100
```

Sigue siendo una **opción**. Ambas formas producen nodos idénticos, se combinan libremente en una misma expresión, y nada aguas abajo puede distinguirlas — así que adoptar o abandonar un metamodelo más tarde es una edición local, no una migración. Conviene ser precisos con el intercambio: seguridad ante erratas sí, seguridad ante renombrados no (el campo guarda una cadena). Validar en la construcción es lo que convierte un nombre obsoleto en un fallo ruidoso la primera vez que un test toca la clase.

---

## Los nombres, y lo que esta capa se niega a poseer

Una sola estrategia responde a las dos preguntas de nombres, y el mapeo más rico es del ORM:

```pascal
INameResolver.ColumnFor(EntityType, Member)   // por defecto: el nombre del miembro
INameResolver.TableFor(EntityType)            // por defecto: el nombre del tipo sin la T inicial
```

El valor por defecto de la tabla quita la `T` solo cuando le sigue una mayúscula, así que `TProduct` → `Product` mientras `Thing` se deja en paz. Sin pluralización y sin conversión de caso: eso es *política* de nombres, y adivinarla chocaría con lo que decida el ORM mientras parece una funcionalidad.

Claves, columnas de bloqueo, atributos y DML dirigido por instancia (`Update(Instance)`) están **ausentes** deliberadamente. El ORM aún no está diseñado; esta capa se compromete con la costura, no con el mapeo.

---

## Fallar donde está el error

Un nombre de miembro que no resuelve a nada lanza en el punto donde se escribe la expresión, nombrando el miembro y el tipo. El peligro que se evita es un valor por defecto que pasa de largo: hace que un nombre equivocado parezca correcto en el camino que lo tolera — SQL enviará tranquilamente una columna sin mapear al servidor, mientras el carril en memoria no puede — de modo que la divergencia aparece lejos de su causa y en un solo back-end.

---

## Documentación

| Recurso | Descripción |
|---------|-------------|
| [**Guía de uso**](docs/GUIDE.es.md) | Autoría, el proxy, el metamodelo, especificaciones |
| [Daf.SQLCute.Expressions](../SQLCute.Expressions/README.es.md) | La emisión a `IQuery` y el adaptador en memoria |
| [SQLCute](../SQLCute.Abstractions/README.es.md) | El constructor de consultas al que esto acaba emitiendo |
| [Daf.Linq](../Linq/README.es.md) | El mundo de las clausuras — el hermano, no el rival |
