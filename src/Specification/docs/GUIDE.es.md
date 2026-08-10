# Daf.Specification — Guía de uso

**🌍 Idioma: [English](GUIDE.md) | Español**

← [Volver al README](../README.es.md)

---

## Introducción

Este paquete contiene la mitad de dominio de la capa de expresiones: un árbol, los operadores que lo construyen, la costura que da nombres, y la especificación y el puerto que lo llevan al otro lado de una frontera. Compila contra Commons y nada más, así que todo esto se usa y se prueba sin base de datos y sin enlazar ninguna unidad de SQL.

La mitad de SQL es [`Daf.SQLCute.Specification`](../../SQLCute.Specification/README.es.md), que emite un árbol a un `IQuery` de SQLCute y trae el adaptador de repositorio en memoria.

```pascal
uses Daf.Specification;

var Spec := TSpecification<TProduct>.New
  .Where((Prop('Price') > 100) and (Prop('Active') = True))
  .OrderByDesc('Price');
```

---

## Índice

| Tema | Qué cubre |
|------|-----------|
| [Autoría con operadores](guide/authoring.es.md) | Los operadores, la superficie de amplitud, valores y NULL |
| [El proxy de entidad](guide/entity-proxy.es.md) | `TEntityExpr<T>`, resolución de nombres, fallar donde se escribe |
| [El metamodelo opcional](guide/metamodel.es.md) | `TPropExpr<T>`, qué aporta y qué no |
| [Especificaciones y el puerto](guide/specifications.es.md) | `TSpecification<T>`, `IRepository<T>`, composición hexagonal |

---

## El modelo de nodos de un vistazo

Seis tipos de nodo y tres enumerados de operadores; ese es todo el modelo:

| Clase | Interfaz | Lleva |
|-------|----------|-------|
| `Compare` | `ICompareExpr` | un operador, un objetivo, cero o más operandos, sensibilidad a mayúsculas |
| `Logical` | `ILogicalExpr` | un operador y dos operandos |
| `Unary` | `IUnaryExpr` | un operador y un operando |
| `Member` | `IMemberExpr` | un nombre de **propiedad** (nunca de columna) y un accesor reservado |
| `Value` | `IValueExpr` | un `Variant` |
| `Param` | `IParamExpr` | un nombre y un `Variant` |

La aridad viaja con el operador, no con el tipo: `IsNull` no toma operandos, `Between` toma dos, `In` toma tantos como tenga el conjunto. Por eso la amplitud crece sin que crezca el modelo — una comparación nueva es un valor nuevo del enumerado y una fila nueva en la tabla de correspondencias del emisor.

El tiempo de vida es conteo de referencias de interfaz de principio a fin. No hay `Acquire`/`Release`, ni `Free` manual, ni un segundo idioma.

---

## Volver a leer un árbol

`ExprToText` representa un árbol de forma canónica — estructural, estable y ciega a cómo se escribió:

```pascal
ExprToText(Prop('Price') > 100)
// (m:Price Greater v:100#17)
```

Sirve para afirmar que dos expresiones son la misma cosa, como clave de caché o en una línea de log. Nunca es un paso hacia la ejecución; el emisor lee el árbol directamente.

`TSpecification<T>.Key` hace lo mismo con una regla completa — filtro, orden y página juntos — que es lo que permite a un adaptador compilar un plan una vez y reutilizarlo.

---

## Lo que falta a propósito

| Ausente | Por qué |
|---------|---------|
| Un filtro con clausura en una especificación | Lo intraducible debe ser irrepresentable; para ese mundo está `Daf.Linq` |
| Directivas de carga anticipada (`includes`) | No hay a qué emitirlas, y es el dueño equivocado — montar un agregado es del adaptador |
| Una restricción `T: TEntity` | Nada aguas abajo la necesita; los registros también funcionan |
| Metadatos de mapeo entidad↔tabla | Territorio del ORM, y el ORM aún no está diseñado |
| DML dirigido por instancia | Necesita el conjunto de miembros persistentes, la clave primaria y seguimiento de cambios — todo metadato del ORM |
| Operadores de SQL crudo y `EXISTS` | Una expresión de dominio no debe *poder* decirlos |

El último lo impone la frontera del paquete, no la disciplina: esta unidad no referencia SQLCute, y su ejecutor de specs enlaza solo la mitad de dominio. En cuanto adquiera una dependencia de SQL, el proyecto deja de compilar.
