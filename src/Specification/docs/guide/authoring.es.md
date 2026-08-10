# Autoría con operadores

**🌍 Idioma: [English](authoring.md) | Español**

← [Volver a la guía](../GUIDE.es.md)

---

## Comparaciones

`Prop` nombra un miembro; los operadores sobrecargados construyen el nodo.

```pascal
Prop('Price') > 100
Prop('Price') >= 100
Prop('Price') < 100
Prop('Price') <= 100
Prop('Active') = True
Prop('Country') <> 'US'
```

Cada una de ellas es un nodo de comparación que lleva un valor de operador. `Prop('Price') > 100` se lee de vuelta como mayor-que, una referencia al miembro `Price` y un valor `100`:

```pascal
var C := (Prop('Price') > 100).Node as ICompareExpr;
C.Op                                   // TCompareOp.Greater
(C.Target as IMemberExpr).Name         // 'Price'
(C.Operands[0] as IValueExpr).Value    // 100
```

---

## Combinar

```pascal
(Prop('Price') > 100) and (Prop('Active') = True)
(Prop('Country') = 'US') or (Prop('Country') = 'UK')
not (Prop('Active') = True)
```

Delphi exige los paréntesis alrededor de cada comparación, porque aquí `=` y `>` devuelven una expresión y no un `Boolean`.

La precedencia propia de Delphi liga `and` más fuerte que `or`, exactamente como SQL, así que esto no necesita paréntesis:

```pascal
A and B or C and D      // (A and B) or (C and D)
```

La capa no implementa ninguna precedencia. Escribe la agrupación explícita cuando quieras otra cosa y el emisor la anidará.

---

## La superficie de amplitud

Todo lo que SQLCute ya expresa se alcanza emitiendo la cláusula que lo dice, así que cada una de estas es un valor de operador y **ningún tipo de nodo nuevo**:

```pascal
Prop('Name').Like('%son%')
Prop('Name').NotLike('%son%')
Prop('Name').StartsWith('And')
Prop('Name').EndsWith('son')
Prop('Name').Contains('nder')
Prop('Price').Between(10, 200)
Prop('Price').NotBetween(10, 200)
Prop('Price').IsIn([5, 120, 900])
Prop('Price').NotIn([5, 120])
Prop('Price').IsNull
Prop('Price').IsNotNull
```

La familia LIKE es insensible a mayúsculas por defecto, igual que SQLCute. Pasa `True` para una coincidencia sensible:

```pascal
Prop('Name').Like('%son%', True)
```

---

## Valores

Una hoja de valor lleva un único `Variant`. Esa es la representación que ya llevan las cláusulas de SQLCute, así que no se reimplementa nada — y no se pierde nada: los modelos de parámetros que esto sustituye tenían el mismo techo, envuelto en decenas de accesores tipados cuya única aportación extra era una pista para el driver.

Un tipo sin forma directa de `Variant` se estrecha **donde lo escribes**:

```pascal
Prop('Id') = SomeGuid                        // sobrecarga tipada → GUIDToString
Prop('Id') = TExprValue.From(SomeGuid)       // la misma conversión, explícita
```

Eso importa más de lo que parece. Una hoja con un `TGUID` real dejaría al carril en memoria comparando un registro mientras SQL compara su texto — divergencia en el único punto donde este diseño promete paridad. Estrechar en el borde hace que ambos carriles vean el mismo valor canónico.

---

## NULL

NULL tiene una sola representación: el `TNullable` de Commons a la entrada, el `Null` de Variant desde la hoja en adelante. Ningún nodo lleva una marca de nulidad propia.

```pascal
var Age: TNullable<Integer> := TNullable<Integer>.Null;
Prop('Age') = TExprValue.From<Integer>(Age)      // → Variant Null
```

Para comprobar NULL usa el predicado, no una comparación — como en SQL, comparar contra NULL es *desconocido*, no cierto:

```pascal
Prop('Age').IsNull
Prop('Age').IsNotNull
```

Ambos back-ends siguen la lógica trivaluada ANSI, así que `not (Prop('Age') > 18)` excluye una edad NULL exactamente como lo haría una base de datos.

---

## Parámetros

Un marcador con nombre que lleva su valor:

```pascal
Prm('minPrice', 100)
Prop('Price') > 100                 // equivalente para la ejecución
```

Una hoja de parámetro es una hoja de valor con nombre. El nombre es para ti — para logs, para construir una regla y volver a leerla — no un segundo mecanismo de vinculación.
