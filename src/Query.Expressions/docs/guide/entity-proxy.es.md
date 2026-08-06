# El proxy de entidad y la resolución de nombres

**🌍 Idioma: [English](entity-proxy.md) | Español**

← [Volver a la guía](../GUIDE.es.md)

---

## Por qué hace falta un proxy

`Prop('Price')` nombra un miembro y no pregunta nada sobre él. Eso vale para una regla escrita junto a la entidad, pero significa que una errata viaja: el back-end de SQL enviará tranquilamente una columna sin mapear al servidor, mientras el carril en memoria no puede resolverla. El fallo aparece entonces lejos de su causa, y en un solo back-end.

`TEntityExpr<T>` cierra ese hueco validando el miembro contra el RTTI de la entidad allí donde se escribe la expresión:

```pascal
var P := TEntityExpr<TProduct>.Create(nil);

P.Prop('Price') > 100         // bien
P.Prop('price') > 100         // también — resuelve a 'Price', como lo escribe el RTTI
P.Prop('Pricee') > 100        // EExprMemberError: nombra el miembro y el tipo
```

Las propiedades ganan a los campos, igual que en la resolución del propio carril en memoria. Un registro funciona igual que una clase — aquí nada necesita una clase base.

---

## La hoja lleva un nombre de propiedad

No un nombre de columna. Tres razones independientes:

- Un nombre de columna es vocabulario de persistencia. Una hoja que lo llevara sería consciente de la persistencia y no podría vivir en un paquete que debe permanecer libre de SQL.
- Declararía el mapeo dos veces — una donde lo describa el ORM, otra aquí — sin ningún enlace del compilador entre las copias. La divergencia sería entonces solo cuestión de tiempo, y divergiría *en silencio*.
- El nombre de propiedad es la moneda correcta para el carril en memoria, que no necesita mapeo alguno. Solo SQL necesita la columna, y la obtiene en el momento de emitir, en un único sitio, una sola vez.

---

## Una estrategia, dos preguntas

`INameResolution` responde a las dos preguntas de nombres, porque son la misma clase de pregunta:

```pascal
INameResolution = interface
  function ColumnFor(EntityType: PTypeInfo; const Member: string): string;
  function TableFor(EntityType: PTypeInfo): string;
end;
```

Los valores por defecto son deliberadamente mínimos:

| Pregunta | Por defecto |
|----------|-------------|
| ¿Qué columna para este miembro? | El nombre del miembro, sin cambios |
| ¿Qué tabla para este tipo? | El nombre del tipo sin la `T` inicial |

La `T` se quita solo cuando le sigue una mayúscula, así que `TProduct` → `Product` mientras `Thing` se deja en paz. Sin pluralización y sin conversión de caso: eso es *política* de nombres, y adivinarla chocaría con lo que decida el ORM mientras parece una funcionalidad.

Aportar la tuya es una clase:

```pascal
TMappedNames = class(TInterfacedObject, INameResolution)
public
  function ColumnFor(EntityType: PTypeInfo; const Member: string): string;
  function TableFor(EntityType: PTypeInfo): string;
end;

function TMappedNames.ColumnFor(EntityType: PTypeInfo; const Member: string): string;
begin
  if SameText(Member, 'Price') then Result := 'price_eur' else Result := Member;
end;

function TMappedNames.TableFor(EntityType: PTypeInfo): string;
begin
  Result := 'products';
end;
```

Luego, o se le pregunta al proxy:

```pascal
var P := TEntityExpr<TProduct>.Create(TMappedNames.Create);
P.Column('Price');    // 'price_eur'
P.Table;              // 'products'
```

…o se le entrega la estrategia a la emisión, que es donde de verdad hace falta un nombre de columna:

```pascal
TExprEmitter.Where(Q, MyExpr, TMappedNames.Create, TypeInfo(TProduct));
TExprEmitter.Apply<TProduct>(Q, Spec, TMappedNames.Create);
```

---

## Lo que la estrategia no es

Es una costura para nombres, no un modelo de mapeo. Claves, columnas de bloqueo, atributos, mapeo de herencia y DML dirigido por instancia son todos metadatos del ORM, y el ORM aún no está diseñado. Meter aquí un modelo de mapeo prejuzgaría ese diseño desde la capa equivocada.

Así que esta capa se compromete con la costura: sea lo que sea el ORM, aportará su propia `INameResolution` y aquí no cambiará nada.

---

## El accesor reservado

`IMemberExpr.Accessor` existe y hoy siempre vale `nil`. El sitio está reservado porque el verdadero beneficio de un metamodelo generado no serían los nombres, sino adjuntar aquí un accesor ya resuelto, permitiendo al carril en memoria saltarse la normalización de nombre, la búsqueda en diccionario y el encajonado que hace por fila y por columna. Reservar el campo ahora permite que eso llegue sin reformar el nodo.

Los dos caminos de autoría lo dejan en `nil`, a propósito: si uno lo rellenara y el otro no, se convertiría en aquello que distingue una hoja de metamodelo de una de tiempo de ejecución.
