# SQLCute — Guía de Uso

**🌍 Idioma: [English](GUIDE.md) | Español**

---

## Introducción

SQLCute construye SQL parametrizado mediante una cadena fluida de métodos. El punto de entrada siempre es `TQuery.New`:

```pascal
uses Daf.SQLCute, Daf.SQLCute.Compiler;

var Q := TQuery.New          // IQuery — con conteo de referencias, sin Free
  .From('productos')
  .Where('estado', 'activo')
  .OrderBy('nombre')
  .Limit(50);

var Compiler: IQueryCompiler := TAnsiSqlCompiler.Create;  // el compilador es contado: ha de tener dueño
var R := Compiler.Compile(Q);
// R.SQL      → 'SELECT * FROM productos WHERE estado = ? ORDER BY nombre LIMIT 50'
// R.Bindings → ['activo']
```

`TSQLResult` siempre contiene:

| Campo | Tipo | Descripción |
|-------|------|-------------|
| `SQL` | `string` | El SQL compilado con marcadores `?` (los compiladores de dialecto pueden usar `$1`, `:p1`, `@p0`) |
| `Bindings` | `TArray<Variant>` | Valores posicionales que corresponden a los marcadores |

Pasa `R.SQL` y `R.Bindings` a tu driver de base de datos — SQLCute nunca ejecuta consultas por sí solo.

---

## Tabla de Contenidos

1. [SELECT & FROM](guide/select.es.md) — columnas, DISTINCT, agregados, ORDER BY, LIMIT/OFFSET
2. [WHERE](guide/where.es.md) — igualdad, comparaciones, NULL, BETWEEN, IN, EXISTS, grupos, columnas
3. [JOINs](guide/joins.es.md) — INNER, LEFT, RIGHT, CROSS, FULL OUTER, callbacks
4. [GROUP BY / HAVING](guide/groupby.es.md) — agrupación y filtros de agregados
5. [Operaciones de Conjunto](guide/setops.es.md) — UNION, INTERSECT, EXCEPT, paginación
6. [DML](guide/dml.es.md) — INSERT, UPDATE, DELETE
7. [Subconsultas y CTEs](guide/subqueries.es.md) — tablas derivadas, WITH, CTEs recursivos
8. [Operaciones de Cadena](guide/string-ops.es.md) — LIKE, starts/ends/contains
9. [Operaciones de Fecha](guide/date-ops.es.md) — WhereDate, casting de fecha por dialecto
10. [Compiladores de Dialecto](guide/dialects.es.md) — entrecomillado, marcadores, matriz de dialectos

---

## El Patrón del Compilador

Todo `IQuery` es agnóstico al compilador. Eliges el dialecto en el momento de compilar:

```pascal
uses Daf.SQLCute;                      // IQueryCompiler
uses Daf.SQLCute.Compiler;             // TAnsiSqlCompiler (por defecto)
uses Daf.SQLCute.Compiler.Postgres;    // TPostgresCompiler
uses Daf.SQLCute.Compiler.MySql;       // TMySqlCompiler
// … etc.

var Compiler: IQueryCompiler := TPostgresCompiler.Create;  // contado por referencia: ha de tener dueño
var R := Compiler.Compile(Q);
```

También puedes usar el atajo `Q.Compile(MiCompilador)` cuando tienes una instancia del compilador.

---

## Clone — Variantes de Consulta

`Clone` crea una copia profunda, útil para construir consultas relacionadas a partir de una base común:

```pascal
var Base := TQuery.New.From('pedidos').Where('año', 2024);

var Pendientes := Base.Clone.Where('estado', 'pendiente');
var Enviados   := Base.Clone.Where('estado', 'enviado');
```

---

## Seguridad con NULL

Todos los métodos `Where*` que reciben un `Variant` aceptan valores `Null`. Cuando el valor es `Null`, el SQL generado usa `IS NULL` con `=` o sin operador, e `IS NOT NULL` con cualquier otro, sin enlace; `WhereNot` conserva su negación.

```pascal
.Where('eliminado_en', Null)      // → WHERE eliminado_en IS NULL
.WhereNot('eliminado_en', Null)   // → WHERE NOT (eliminado_en IS NULL)
```

---

## Diferencias con SqlKata

SQLCute sigue el constructor y los compiladores de [SqlKata](https://sqlkata.com/). Estos son los puntos en que el SQL que emite, o lo que acepta, es distinto:

| Caso | SqlKata | SQLCute |
|------|---------|---------|
| LIKE insensible a mayúsculas en Postgres | `ILIKE` | `LOWER(columna) LIKE ?`, como en todos los dialectos |
| Marcadores | `@p0`, `@p1`… en todos los dialectos, y `:p0`… en Oracle | `$1`… en Postgres, `@p0`… en SQL Server, `:p1`… en Oracle, `?` en los demás |
| Compilador base | entrecomilla los identificadores con `"` | `TAnsiSqlCompiler` no entrecomilla; cada compilador de dialecto entrecomilla con su carácter |
| `WhereIn` con un conjunto vacío | `1 = 0` | `col IS NULL AND col IS NOT NULL`, que nombra la columna |
| `Where(col, 'in', valores)` | lo rechaza | liga los valores, como `WhereIn` |
| `WhereNot` con `Null` o con un valor lógico | pierde la negación | la conserva: `NOT (col IS NULL)` |
| Una comparación negada con un valor lógico | `col != true` | `NOT (col = true)` |
| Carácter de escape de la familia LIKE | cualquier carácter | una comilla simple o un `?` lanzan `EArgumentException` |
| `\?` en una expresión en bruto | queda `\?` en el SQL | pasa a ser un `?` literal |
| Una página de SQL Server sobre una consulta `Distinct` sin `OrderBy` | `ORDER BY (SELECT 0)`, que SQL Server rechaza con `DISTINCT` | `ORDER BY 1` |

---

## Ver También

- [Aplicación de Muestra](../samples/SQLCuteSample/README.md)
