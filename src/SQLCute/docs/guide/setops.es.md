# Operaciones de Conjunto y Paginación

**🌍 Idioma: [English](setops.md) | Español**

← [Volver a la Guía](../GUIDE.es.md)

---

## UNION

```pascal
var Q1 := TQuery.New.From('usuarios_activos').Select(['id', 'nombre']);
var Q2 := TQuery.New.From('usuarios_archivados').Select(['id', 'nombre']);

Q1.Union(Q2)
// → SELECT id, nombre FROM usuarios_activos
//   UNION
//   SELECT id, nombre FROM usuarios_archivados
```

`UNION ALL` (mantiene duplicados):

```pascal
Q1.UnionAll(Q2)
// → … UNION ALL …
```

---

## INTERSECT / EXCEPT

```pascal
Q1.Intersect(Q2)
// → … INTERSECT …

Q1.&Except(Q2)    // & para evitar conflicto con keyword de Delphi
// → … EXCEPT …

Q1.IntersectAll(Q2)
Q1.ExceptAll(Q2)
```

---

## CombineRaw

Añade una operación de conjunto cruda:

```pascal
Q1.CombineRaw('MINUS SELECT id, nombre FROM usuarios_eliminados')
// → … MINUS SELECT id, nombre FROM usuarios_eliminados
```

---

## Paginación

### ForPage (recomendado)

Número de página base-1, tamaño de página configurable:

```pascal
TQuery.New.From('productos')
  .Where('activo', True)
  .OrderBy('nombre')
  .ForPage(3, 25)      // página 3, 25 elementos por página
// → … LIMIT 25 OFFSET 50
```

Tamaño de página por defecto: 15:

```pascal
.ForPage(2)   // → LIMIT 15 OFFSET 15
```

### LIMIT / OFFSET Manual

```pascal
.Limit(20).Offset(40)
// → LIMIT 20 OFFSET 40

// Alias
.Take(20).Skip(40)
```

### Bucle típico de paginación

```pascal
const TamanoPagina = 50;
var Pagina := 1;

repeat
  var R := Compilador.Compile(
    TQuery.New.From('pedidos').OrderBy('id').ForPage(Pagina, TamanoPagina));
  // ejecutar R, procesar filas…
  Inc(Pagina);
until NumFilas < TamanoPagina;
```
