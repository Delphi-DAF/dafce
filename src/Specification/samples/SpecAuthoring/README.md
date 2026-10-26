# SpecAuthoring — escribir el árbol, y mirarlo por dentro

Las tres vías de escritura de una expresión, la anchura del operador y una `TSpecification<T>`
completa —filtro, orden y paginación— con su clave. No hay back-end: el sample es sobre **escribir
y leer** la regla, no sobre ejecutarla.

Para ver una regla ejecutada, sobre una colección y traducida a SQL, está
`SQLCute.Specification/samples/CatalogServiceSample`.

## Qué enseña

- **Tres vías, un solo nodo.** `Prop('Price') > 100`, `TEntityExpr<TProduct>.Prop('Price') > 100` y
  `ProductMeta.Price > 100` producen el mismo texto, porque producen el mismo árbol. Nada
  río abajo —emisor, especificación, repositorio— puede distinguirlos.
- **Lo que cada vía cuesta y da.** La de ejecución acepta cualquier cadena. El proxy resuelve el
  nombre contra el RTTI de la entidad, así que `Prop('Pricee')` levanta `EExprMemberError`
  nombrando al miembro y al tipo — y contesta además por la tabla y la columna. El metamodelo
  convierte la errata en un error de compilación, y valida sus nombres al inicializarse la clase,
  o sea en el arranque y no a mitad de una consulta.
- **La anchura es acotada a propósito.** `Contains`, `StartsWith`, `Between`, `IsIn`, `IsNull` son
  valores de operador sobre los mismos nodos, no tipos de nodo nuevos. Lo que no se puede
  representar no se puede escribir, que es lo que hace finito un back-end.
- **Un opcional ausente se estrecha a NULL.** `TExprValue.From<Integer>` sobre un `TNullable`
  vacío da `v:null`; con valor da `v:500`. Sin ese paso el `Default(T)` de un `Integer` entraría
  como un `0` perfectamente comparable.
- **Una especificación es un valor con clave.** `Key` concatena el tipo, el filtro, el orden y la
  paginación, así que dos páginas de la misma regla tienen claves distintas y la misma página
  tiene siempre la misma. Es lo que permite cachear el resultado de una consulta.

El `#17`, `#258` y `#1` del texto son el `TVarType` del valor —`varInteger`, `varUString`,
`varNull`—: `ExprToText` los imprime para que dos árboles iguales de vista no pasen por iguales.

## Ejecutar

```
BDS make --project:src/Specification/samples/SpecAuthoring/SpecAuthoring.dproj
out\Win32\samples\bin\SpecAuthoring.exe
```

Salida:

```
== Las tres vias de escritura producen el mismo nodo
  Prop                   (m:Price Greater v:100#17)
  TEntityExpr<TProduct>  (m:Price Greater v:100#17)
  ProductMeta            (m:Price Greater v:100#17)

== El proxy contesta ademas por los nombres del almacen
  tabla  : Product
  columna: Price

== Un nombre que ya no existe se queja, y dice de quien
  Member "Pricee" resolves to no property or field of TProduct.

== Componer: and, or, not
  barato y con stock     ((m:Price Between v:0#17 v:50#17) opAnd (m:Stock Greater v:0#17))
  barato o sin precio    ((m:Price Between v:0#17 v:50#17) opOr (m:Price IsNull))
  ni barato              (opNot (m:Price Between v:0#17 v:50#17))

== La anchura del arbol, que es acotada a proposito
  Contains               (m:Name Contains v:taladro#258)
  StartsWith             (m:Name StartsWith v:Ta#258)
  Between                (m:Price Between v:10#17 v:200#17)
  IsIn                   (m:Category In v:Herramientas#258 v:Jardin#258)
  IsNull                 (m:Price IsNull)

== Un opcional sin valor se estrecha a NULL, no a cero
  ausente                (m:Price Equal v:null#1)
  presente               (m:Price Equal v:500#3)

== Una especificacion lleva filtro, orden y paginacion
  filtro : ((m:Category Equal v:Herramientas#258) opAnd (m:Stock Greater v:0#17))
  orden  : Name
  pagina : skip=20 take=10

== Y sabe decir su clave, que es lo que hace cacheable una consulta
  TProduct|((m:Category Equal v:Herramientas#258) opAnd (m:Stock Greater v:0#17))|asc:Name|skip:20|take:10
  la misma regla, otra pagina:
  TProduct|((m:Category Equal v:Herramientas#258) opAnd (m:Stock Greater v:0#17))|asc:Name|skip:30|take:10
```
