# Especificaciones y el puerto de repositorio

**🌍 Idioma: [English](specifications.md) | Español**

← [Volver a la guía](../GUIDE.es.md)

---

## Una regla como valor

`TSpecification<T>` agrupa una expresión de filtro con orden y paginación opcionales. Es un valor: se construye, se pasa, se guarda, se afirma.

```pascal
var Spec := TSpecification<TProduct>.New
  .Where((Prop('Price') > 100) and (Prop('Active') = True))
  .OrderByDesc('Price')
  .Skip(10)
  .Take(5);
```

Cada constructor devuelve una especificación nueva, así que una regla base se puede refinar sin mutarla:

```pascal
var Active := TSpecification<TProduct>.New.Where(Prop('Active') = True);
var Page1  := Active.OrderBy('Name').Take(20);
var Page2  := Active.OrderBy('Name').Skip(20).Take(20);
```

Los miembros de ordenación se validan contra `T` al declararlos, por la misma razón que las hojas de miembro: un nombre que no resuelve a nada debe fallar donde se escribe la regla.

Volver a leerla no necesita base de datos:

```pascal
var Root := Spec.Filter as ILogicalExpr;
Root.Op                                        // TLogicalOp.opAnd
Spec.Order[0].Member                           // 'Price'
Spec.Order[0].Descending                       // True
Spec.SkipCount                                 // 10
Spec.TakeCount                                 // 5
Spec.Key                                       // canónica, de la regla completa
```

`Skip(0)` y `Take(0)` significan *sin fijar*, así que una regla sin paginar no añade ninguna cláusula al emitirla.

---

## Sin clausuras, a propósito

No existe una sobrecarga de `Where` que acepte un predicado. Una especificación lleva solo el árbol de expresión, lo que la hace traducible por construcción — y no deja ningún recurso silencioso de evaluación en el cliente del que arrepentirse después. EF Core lo llevó y lo quitó en la 3.0 justo por esto.

No se pierde nada. Cuando de verdad quieras código Delphi arbitrario sobre una secuencia materializada, para eso está [`Daf.Linq`](../../../Linq/README.es.md):

```pascal
for var P in Linq.From<TProduct>(Loaded).Where(
    function(P: TProduct): Boolean
    begin
      Result := AlgunaReglaQueNingunSQLExpresa(P);
    end) do ...
```

Los dos son hermanos. Uno es traducible; el otro no, y lo dice siendo un tipo distinto.

---

## Tampoco hay includes

Las directivas de carga anticipada no forman parte de una especificación. Más allá de que no hay cláusula a la que emitirlas, son el dueño equivocado: un repositorio devuelve agregados, y cómo se hidrata un agregado es asunto del adaptador. Una especificación dice *qué* seleccionar.

---

## El puerto

```pascal
IRepository<T> = interface
  function List(const Spec: TSpecification<T>): TArray<T>;
end;
```

Ni SQL, ni conexión, ni fila, ni consulta en la firma — y ningún `T: TEntity` ni otra restricción de clase base. Una clase corriente con propiedades públicas funciona; un registro también.

Un servicio de dominio depende del puerto y de nada más:

```pascal
function TCatalogService.Bargains: TArray<TProduct>;
begin
  Result := FProducts.List(
    TSpecification<TProduct>.New
      .Where((Prop('Price') < 50) and (Prop('Active') = True))
      .OrderBy('Price'));
end;
```

Esa unidad no enlaza ni SQLCute ni ningún adaptador. Es comprobable con un doble de cinco líneas, y la raíz de composición es el único sitio que nombra un repositorio concreto.

---

## Conseguir un adaptador

Este paquete define el puerto; [`Daf.SQLCute.Specification`](../../../SQLCute.Specification/README.es.md) trae el de memoria:

```pascal
uses Daf.SQLCute.Specification;

var Repo: IRepository<TProduct> := TInMemoryRepository<TProduct>.Create(Loaded);
var Service := TCatalogService.Create(Repo);
```

El adaptador en memoria no es un doble de test. Ejecutar varias reglas sobre una colección ya cargada es un camino de producción — cargar una vez y resolver N especificaciones contra el conjunto cargado, con el mismo tipo de especificación que iría a una base de datos. Por eso la simetría "una regla, dos back-ends" tiene un motivo concreto y no estético.

No hay adaptador respaldado por base de datos: la ejecución SQL, el driver y la materialización de filas no están ni en este paquete ni en `Daf.SQLCute.Specification`.
