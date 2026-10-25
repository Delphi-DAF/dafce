# Operaciones de Cadena

**🌍 Idioma: [English](string-ops.md) | Español**

← [Volver a la Guía](../GUIDE.es.md)

---

## Resumen

Todos los métodos de coincidencia de cadenas envuelven el valor en patrones `LIKE`. Por defecto (`CaseSensitive = False`), SQLCute envuelve la columna en `LOWER(…)` y pasa el patrón a minúsculas, que viaja como binding. Pasa `True` como `CaseSensitive`, el argumento que sigue al valor, para emitir un `LIKE` llano con el patrón tal y como lo escribiste.

Los comodines `%` y `_` dentro del valor siguen activos en todos los métodos: `WhereContains('codigo', '50%')` casa con cualquier código que contenga `50`. Para casarlos como literales, declara un [carácter de escape](#carácter-de-escape).

---

## WhereLike — Patrón Libre

Proporciona el patrón LIKE completo:

```pascal
.WhereLike('nombre', '%Juan%')
// Insensible a mayúsculas (por defecto):
// → WHERE LOWER(nombre) LIKE ?      binding: '%juan%'

.WhereLike('codigo', 'ABC-%', True)
// Sensible a mayúsculas:
// → WHERE codigo LIKE ?             binding: 'ABC-%'
```

Variantes: `WhereNotLike`, `OrWhereLike`, `OrWhereNotLike`.

---

## WhereStarts — Coincidencia de Prefijo

```pascal
.WhereStarts('email', 'admin')
// → WHERE LOWER(email) LIKE ?     binding: 'admin%'
```

Variantes: `WhereNotStarts`, `OrWhereStarts`, `OrWhereNotStarts`.

---

## WhereEnds — Coincidencia de Sufijo

```pascal
.WhereEnds('archivo', '.pdf')
// → WHERE LOWER(archivo) LIKE ?   binding: '%.pdf'
```

Variantes: `WhereNotEnds`, `OrWhereEnds`, `OrWhereNotEnds`.

---

## WhereContains — Coincidencia de Subcadena

```pascal
.WhereContains('descripcion', 'urgente')
// → WHERE LOWER(descripcion) LIKE ?   binding: '%urgente%'
```

Variantes: `WhereNotContains`, `OrWhereContains`, `OrWhereNotContains`.

---

## Carácter de escape

Todos los métodos de la familia aceptan un carácter de escape opcional tras `CaseSensitive`. El carácter que lo sigue en el valor es literal, y el SQL añade `ESCAPE '<c>'`:

```pascal
.WhereContains('codigo', '50!%', False, '!')
// → WHERE LOWER(codigo) LIKE ? ESCAPE '!'   binding: '%50!%%'
```

SQLCute no escapa nada por su cuenta: los comodines que añade alrededor del valor siguen siendo comodines, y solo es literal lo que precede el carácter de escape. Una comilla simple o un `?` como carácter de escape lanzan `EArgumentException`.

Con `CaseSensitive = False` el patrón pasa a minúsculas y el carácter de escape no, así que elige uno sin mayúscula, como `!`. El back-end en memoria lee el carácter de escape igual.

---

## Combinando Filtros de Cadena

```pascal
TQuery.New.From('productos')
  .Where(function(Q: IQuery): IQuery
    begin
      Result := Q.WhereContains('nombre', 'cafe')
                 .OrWhereContains('etiquetas', 'cafe');
    end)
  .WhereNotStarts('sku', 'DESC-')
// → WHERE (LOWER(nombre) LIKE ? OR LOWER(etiquetas) LIKE ?)
//     AND LOWER(sku) NOT LIKE ?
//   bindings: '%cafe%', '%cafe%', 'desc-%'
```

Encadenar los dos términos `OR` sin ese grupo significa otra cosa: una lista plana se lee como OR-de-tramos-AND, así que `WhereContains … OrWhereContains … WhereNotStarts` agrupa como `nombre OR (etiquetas AND sku)`. Anida la alternancia que quieres decir.

---

## Notas sobre Sensibilidad a Mayúsculas por Dialecto

El patrón pasa a minúsculas en todo Unicode, así que `WhereLike('nombre', 'ÑANDÚ%')` enlaza `'ñandú%'`. La columna la pasa a minúsculas el `LOWER` del motor, y el de SQLite solo pliega las letras ASCII: en SQLite, `WhereStarts('nombre', 'ñan')` no casa con un `Ñandú` guardado. El back-end en memoria pliega los dos lados en todo Unicode.

Cada base de datos tiene además su propio comportamiento nativo de LIKE:

| Dialecto | ¿LIKE sensible a mayúsculas? | Notas |
|----------|------------------------------|-------|
| ANSI SQL | Sí | Comportamiento estándar |
| PostgreSQL | Sí | Admite `ILIKE` nativamente; SQLCute usa `LOWER(…)` |
| MySQL | No (por defecto) | El envoltorio `LOWER` no tiene efecto negativo |
| SQLite | No para ASCII | Las letras no ASCII son sensibles, y su `LOWER` solo pliega ASCII, sin la extensión ICU |
| SQL Server | Depende de la intercalación | `Latin1_General_CI_AS` → insensible a mayúsculas |
| Oracle | Sí | Se recomienda el envoltorio `LOWER` |
| Firebird | Sí | Se recomienda el envoltorio `LOWER` |

Cuando `CaseSensitive = True`, no se emite ningún envoltorio `LOWER` independientemente del dialecto.
