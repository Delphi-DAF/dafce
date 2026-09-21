# LinqPipeline — una consulta perezosa sobre una colección en memoria

Seis pedidos y la cadena completa de operadores: filtrar, ordenar, paginar, proyectar y agrupar,
más los terminales que distinguen una secuencia vacía de un `Default(T)`.

## Qué enseña

- **Un filtro es un valor de Commons, no de Linq.** `Where` toma un `TFilterSpec<T>`, que es el
  mismo tipo que toma el `Where` ansioso de `TSmartArray<T>`, así que una regla se escribe una vez
  y sirve a las dos APIs. `TSales.Large and TSales.InNorth` compone dos reglas **antes** de que la
  consulta exista, y `not` sobre el resultado es otra regla. Un cierre suelto también vale: se
  convierte solo.
- **Perezoso de verdad, y se ve.** El proyector cuenta cuántos elementos toca. Con `Take(2)` toca
  **dos de seis**: el operador deja de pedirle a la fuente en cuanto tiene los suyos. Sin un
  contador esto no se distingue de una implementación ansiosa que devuelve lo mismo.
- **Un terminal no consume la consulta.** El mismo `TQuery<T>` se recorre dos veces y contesta lo
  mismo, y encima admite que le encadenen otro operador después. Cada llamada vuelve a recorrer la
  fuente, así que con una fuente cara o de un solo uso hay que materializar primero.
- **Vacío y `Default(T)` no son la misma respuesta.** `FirstOrDefault` sobre una secuencia vacía
  devuelve un `TOrder` con `Id=0`, que es indistinguible de un pedido cuyo `Id` sea 0. `TryFirst`
  es la forma que sí los separa, y `IsEmpty` para en el primer elemento.
- **`GroupBy` conserva el orden de aparición de las claves**, y cada grupo lleva su `Key` y sus
  `Members`.

Para el mismo modelo de consulta traducido a SQL en vez de ejecutado en memoria, ver los samples de
`SQLCute` y `SQLCute.InMemory`.

## Ejecutar

```
BDS make --project:src/Linq/samples/LinqPipeline/LinqPipeline.dproj
out\Win32\samples\bin\LinqPipeline.exe
```

Salida:

```
== Filtrar, ordenar y quedarse con los primeros
  #4 Delta     Este      2100,75
  #6 Faro      Sur       1500,00

== El filtro se compone antes de entrar en la consulta
  #1 Acme      Norte     1250,00
  y su negacion deja 5 de 6

== Proyectar a otro tipo
  Acme debe 1250,00
  Cima debe 980,50
  Eolo debe 75,20

== Agrupar por clave
  Norte  3 pedido(s)
  Sur    2 pedido(s)
  Este   1 pedido(s)

== Perezoso: Take deja de consumir la fuente
  2 de 6 pedidos proyectados para devolver 2

== Una secuencia vacia y un Default(T) no son lo mismo
  TryFirst contesta False, y FirstOrDefault devuelve Id=0
  IsEmpty contesta True

== Un terminal no consume la consulta
  contados 3 y otra vez 3
  el mayor de ellos es Acme
```

Los importes salen con coma decimal: los escribe `Format` con la configuración regional de la
máquina.
