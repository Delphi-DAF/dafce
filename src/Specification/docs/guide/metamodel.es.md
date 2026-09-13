# El metamodelo opcional

**🌍 Idioma: [English](metamodel.md) | Español**

← [Volver a la guía](../GUIDE.es.md)

---

## Declarar uno

Un metamodelo es una clase acompañante escrita a mano que expone los miembros de una entidad como hojas de expresión:

```pascal
ProductMeta = class
public
  class var Price:  TPropExpr<TProduct>;
  class var Active: TPropExpr<TProduct>;
  class constructor Create;
end;

class constructor ProductMeta.Create;
begin
  Price  := TPropExpr<TProduct>.Create('Price');     // el nombre de la PROPIEDAD
  Active := TPropExpr<TProduct>.Create('Active');
end;
```

Y luego se escribe contra él:

```pascal
ProductMeta.Price > 100
ProductMeta.Active = True
ProductMeta.Price.Between(10, 200)
```

`class var` más un `class constructor` en lugar de funciones de clase: el punto de uso es la lectura de un campo, y la inicialización corre una vez por clase. La entidad no se toca — el metamodelo es un tipo aparte, así que ninguna cadena de persistencia se cuela en el POCO.

---

## Qué aporta, con precisión

| Ganancia | ¿Real? |
|----------|--------|
| Seguridad ante erratas | **Sí** — `ProductMeta.Pric` no compila |
| Descubribilidad | **Sí** — el autocompletado lista los miembros |
| Seguridad ante renombrados | **No** — el campo guarda una cadena, así que renombrar `Price` sigue compilando |

Ser honestos con la tercera importa, porque es la que la gente supone que está comprando. Lo que convierte un nombre obsoleto de fallo tardío en fallo temprano y ruidoso es la **autovalidación**: la hoja es genérica sobre la entidad, así que comprueba el nombre cuando la clase se inicializa — que es la primera vez que un test la toca.

```pascal
class constructor StaleMeta.Create;
begin
  Price := TPropExpr<TProduct>.Create('Pricee');   // lanza EExprMemberError aquí
end;
```

Ojo a la consecuencia del `class constructor`: corre durante la inicialización de la unidad, así que un metamodelo obsoleto falla al arrancar y no a mitad de una consulta, y se lleva por delante al ejecutor entero antes de que corra un solo escenario. Para dispararlo a propósito en un test, usa un `class procedure` como inicializador.

---

## Sigue siendo una opción

Esta es la parte que evita que el metamodelo se convierta en un impuesto:

- **El camino en tiempo de ejecución siempre es suficiente.** `TEntityExpr<T>.Prop` alcanza cualquier capacidad, y ninguna funcionalidad del modelo, de las especificaciones o del repositorio exige un metamodelo.
- **Ambos caminos producen nodos idénticos.** Una hoja de metamodelo sale del mismo constructor que llama el camino en tiempo de ejecución. Nada aguas abajo — emisor, especificación, repositorio — puede distinguirlas.
- **Las dos formas se mezclan libremente.**

```pascal
var Runtime := TEntityExpr<TProduct>.Create(nil);
(ProductMeta.Price > 100) and (Runtime.Prop('Active') = True)      // compone con normalidad
```

Así que adoptar un metamodelo para una entidad caliente, o abandonarlo más tarde, es una edición local y no una migración.

---

## Cuándo merece la pena

A grandes rasgos: para entidades consultadas desde muchos sitios, donde un renombrado o una errata los encontraría un test de integración fallido en lugar del compilador. Para una entidad que se consulta una vez desde un servicio, `Prop('Price')` dentro de un `TEntityExpr<T>` ya valida el nombre, y el metamodelo añade un fichero que mantener sincronizado a cambio de poco.

