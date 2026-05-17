# SQLCute — Guía de Uso

**🌍 Idioma: [English](GUIDE.md) | Español**

---

## Introducción

SQLCute construye SQL parametrizado mediante una cadena fluida de métodos. El punto de entrada siempre es `TQuery.New`:

```pascal
uses Daf.SQLCute;

var Q := TQuery.New          // IQuery — con conteo de referencias, sin Free
  .From('productos')
  .Where('activo', True)
  .OrderBy('nombre')
  .Limit(50);

var R := TAnsiSqlCompiler.Create.Compile(Q);
// R.SQL      → 'SELECT * FROM productos WHERE activo = ? ORDER BY nombre LIMIT 50'
// R.Bindings → [True]
```

`TSQLResult` siempre contiene:

| Campo | Tipo | Descripción |
|-------|------|-------------|
| `SQL` | `string` | El SQL compilado con marcadores `?` (los compiladores de dialecto pueden usar `$1`, `:p1`, `@p1`) |
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
uses Daf.SQLCute.Compiler;             // TAnsiSqlCompiler (por defecto)
uses Daf.SQLCute.Compiler.Postgres;    // TPostgresCompiler
uses Daf.SQLCute.Compiler.MySql;       // TMySqlCompiler
// … etc.

var R := TPostgresCompiler.Create.Compile(Q);
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

Todos los métodos `Where*` que reciben un `Variant` aceptan valores `Null`. Cuando el valor es `Null`, el SQL generado usa `IS NULL` (o `IS NOT NULL` para variantes negadas).

```pascal
.Where('eliminado_en', Null)      // → WHERE eliminado_en IS NULL
.WhereNot('eliminado_en', Null)   // → WHERE eliminado_en IS NOT NULL
```

---

## Ver También

- [Paquete de Compiladores](../../../SQLCute.Compilers/README.es.md)
- [Aplicación de Muestra](../samples/SQLCuteSample/README.md)
