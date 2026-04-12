# SQLCute — Roadmap: Cerrar la brecha con SqlKata

## Progreso general

| Fase | Descripción | Estado |
|------|-------------|--------|
| F1 | WHERE variantes OR/Not/Between + Set ops + Paginación + JOIN op | ⬜ Pendiente |
| F2 | Grupos WHERE anidados + WhereColumns + When (callbacks) | ⬜ Pendiente |
| F3 | String Operations (WhereLike/Starts/Ends/Contains) | ⬜ Pendiente |
| F4 | JOIN callbacks compuestos + subquery JOIN | ⬜ Pendiente |
| F5 | SELECT subquery + FromRaw + WithRaw | ⬜ Pendiente |
| F6 | Compiladores de dialecto (módulo nuevo) | ⬜ Pendiente |
| F7 | Date Operations (WhereDate/Time/DatePart) | ⬜ Pendiente |

> Leyenda: ⬜ Pendiente · 🔄 En progreso · ✅ Completado

---

## Contexto

SQLCute es un query builder fluent para Delphi inspirado en [SqlKata](https://sqlkata.com/docs).
La base existente es sólida: AST con 16 tipos de cláusulas, `TAnsiSqlCompiler` completamente
virtualizado, y tests BDD con MiniSpec. Este documento lista las capacidades de SqlKata que
aún faltan y un plan detallado para implementarlas.

## Archivos clave

| Archivo | Rol |
|---|---|
| `Daf.SQLCute.Clauses.pas` | Tipos AST (enums + subclases de `TAbstractClause`) |
| `Daf.SQLCute.pas` | `IQuery` interface + `TQueryImpl` + `TQuery.New` |
| `Daf.SQLCute.Compiler.pas` | `TAnsiSqlCompiler` — todos los `Compile*` virtuales |
| `test/SQLCute.Steps.pas` | Definitions de steps BDD |
| `test/SQLCute.Where.Feat.pas` | Specs WHERE (ampliar) |
| `test/SQLCute.Select.Feat.pas` | Specs SELECT (ampliar) |
| `test/SQLCute.Join.Feat.pas` | Specs JOIN (ampliar) |

## Base implementada ✅

Todo lo siguiente ya existe en `IQuery` / `TQueryImpl` / `TAnsiSqlCompiler`.

### SELECT
- [x] `Select(columns)` / `Select(column)` / `SelectRaw(expr)`
- [x] `SelectAs(column, alias)`
- [x] `SelectCount` / `SelectSum` / `SelectAvg` / `SelectMin` / `SelectMax`
- [x] `Distinct`

### FROM
- [x] `From(table)` / `From(table, alias)`
- [x] `From(subQuery, alias)` — subquery en FROM

### WHERE
- [x] `Where(column, value)` / `Where(column, op, value)`
- [x] `OrWhere(column, value)` / `OrWhere(column, op, value)`
- [x] `WhereNull(column)` / `WhereNotNull(column)`
- [x] `WhereBetween(column, low, high)`
- [x] `WhereRaw(sql)`
- [x] `WhereExists(subQuery)` / `WhereNotExists(subQuery)`
- [x] `WhereInQuery(column, subQuery)`
- [x] `WhereIn(column, values)` / `WhereNotIn(column, values)`
- [x] `OrWhereIn(column, values)` / `OrWhereNotIn(column, values)`

### JOIN
- [x] `Join(table, col1, col2)` / `Join(table, condition)`
- [x] `LeftJoin(table, col1, col2)` / `LeftJoin(table, condition)`
- [x] `RightJoin(table, col1, col2)` / `RightJoin(table, condition)`
- [x] `CrossJoin(table)`
- [x] `FullOuterJoin(table, col1, col2)` / `FullOuterJoin(table, condition)`

### GROUP BY / HAVING
- [x] `GroupBy(column)` / `GroupBy(columns[])` / `GroupByRaw(expr)`
- [x] `Having(column, op, value)` / `HavingRaw(sql)`

### ORDER BY / LIMIT / OFFSET
- [x] `OrderBy(column)` / `OrderByDesc(column)` / `OrderByRaw(expr)`
- [x] `Limit(n)` / `Offset(n)`

### Set Operations
- [x] `Union(other)` / `UnionAll(other)` / `Intersect(other)` / `Except(other)`

### CTE
- [x] `With(name, subQuery)` / `WithRecursive(name, subQuery)`

### DML
- [x] `AsInsert(columns, values)` (fila única)
- [x] `AsInsertRows(columns, rows)` (múltiples filas)
- [x] `AsInsertFrom(columns, subQuery)` (INSERT … SELECT)
- [x] `AsUpdate(columns, values)`
- [x] `AsDelete`

### Utilidades
- [x] `Clone` — copia profunda del query
- [x] `Compile(compiler)` / `Clauses` (interno)

### Compilador base
- [x] `TAnsiSqlCompiler` — ANSI SQL con `?` como placeholder
- [x] Todos los `Compile*` virtuales (15 métodos): `CompileSelect`, `CompileFrom`, `CompileJoin`, `CompileWhere`, `CompileGroupBy`, `CompileHaving`, `CompileOrderBy`, `CompileLimit`, `CompileOffset`, `CompileWith`, `CompileUnion`, `CompileInsert`, `CompileUpdate`, `CompileDelete`, `AssembleQuery`
- [x] `WrapColumn` / `WrapTable` virtuales (sin quoting por defecto)
- [x] `ParamPlaceholder` virtual (retorna `'?'` por defecto)

---

## Patrón de implementación

Cada feature sigue siempre el mismo patrón:

1. Ampliar `Clauses.pas` (tipo o campo nuevo en enum/record/clase)
2. Declarar método(s) en `IQuery` + implementar en `TQueryImpl`
3. Extender el método `Compile*` correspondiente en `TAnsiSqlCompiler`
4. Añadir escenario `.Feat.pas` + step en `SQLCute.Steps.pas`

---

## Fase 1 — WHERE: variantes OR/Not/Between + Set ops + paginación + JOIN op

> Trivial: no requiere nuevos tipos de cláusula.
> **Paralela** con Fases 3, 4, 5.

### WHERE (12 métodos nuevos)

- [ ] `Clauses.pas`: añadir `NotBetween` a `TWhereOp`
- [ ] `OrWhereNull(Column)` / `OrWhereNotNull(Column)` — `TBoolOp.opOr`
- [ ] `WhereTrue(Column)` / `WhereFalse(Column)` — wrappers de `Where(col, True/False)`
- [ ] `WhereNot(Column, Value)` / `OrWhereNot(Column, Value)` — `IsNot=True`
- [ ] `WhereNot(Column, Op, Value)` / `OrWhereNot(Column, Op, Value)`
- [ ] `WhereNotBetween(Column, Low, High)` / `OrWhereBetween(...)` / `OrWhereNotBetween(...)`
- [ ] `OrWhereExists(SubQuery)` / `OrWhereNotExists(SubQuery)`
- [ ] `OrWhereInQuery(Column, SubQuery)` / `WhereNotInQuery(Column, SubQuery)` / `OrWhereNotInQuery(...)`
- [ ] `CompileWhere`: caso `NotBetween` → `NOT BETWEEN`

### Set Operations

- [ ] `Clauses.pas`: añadir `IntersectAll`, `ExceptAll` a `TUnionKind`
- [ ] `Clauses.pas`: añadir `RawSql: string` a `TUnionClause` para `CombineRaw`
- [ ] `IQuery`: `IntersectAll(Other)`, `ExceptAll(Other)`, `CombineRaw(Sql)`
- [ ] `CompileUnion`: manejar los nuevos kinds y el caso raw

### Paginación

- [ ] `IQuery`: `ForPage(Page: Integer; PerPage: Integer = 15)`
- [ ] `IQuery`: `Skip(N)` alias de `Offset`; `Take(N)` alias de `Limit`

### JOIN con operador personalizado

- [ ] `Clauses.pas`: añadir `Op: string` (default `'='`) a `TJoinClause`
- [ ] `IQuery`: 4º parámetro `Op: string = '='` en `Join`, `LeftJoin`, `RightJoin`, `FullOuterJoin`
- [ ] `CompileJoin`: emitir `col1 {op} col2` usando el campo `Op`

### Tests

- [ ] Specs + Steps: extender `SQLCute.Where.Feat.pas` y `SQLCute.Steps.pas`

---

## Fase 2 — Grupos WHERE anidados + WhereColumns + When (callbacks)

> Requiere `TNestedWhereClause`. **Paralela** con Fase 1.

- [ ] `Clauses.pas`: nuevo `TNestedWhereClause` con `SubClauses: TArray<TAbstractClause>` + `Connector: TBoolOp`
- [ ] `Daf.SQLCute.pas`: `TQueryBuilderCallback = reference to function(Q: IQuery): IQuery`
- [ ] `IQuery`: `Where(Callback: TQueryBuilderCallback)` (AND) / `OrWhere(Callback)` (OR)
- [ ] `IQuery`: `Having(Callback: TQueryBuilderCallback)`
- [ ] `TQueryImpl`: construir `TQueryImpl` temporal, invocar Callback, extraer WHERE-clauses → crear `TNestedWhereClause`
- [ ] `CompileWhere`: detectar `TNestedWhereClause` → `'(' + CompileInner + ')'` con conector AND/OR
- [ ] `CompileHaving`: igual para HAVING anidado
- [ ] `Clauses.pas`: añadir `IsColumnValue: Boolean` a `TWhereClause`
- [ ] `IQuery`: `WhereColumns(Col1, Op, Col2)` / `OrWhereColumns(Col1, Op, Col2)`
- [ ] `TQueryImpl`: `TWhereClause` con `IsColumnValue=True`; `Value` = nombre de la 2ª columna
- [ ] `CompileWhere`: si `IsColumnValue` → emitir `col1 op col2` **sin** binding
- [ ] `IQuery`: `When(Condition: Boolean; TrueCallback: TQueryBuilderCallback; FalseCallback = nil)`

### Tests

- [ ] Nuevo spec: `test/SQLCute.Groups.Feat.pas`

---

## Fase 3 — String Operations

> Reutiliza `TWhereOp.Like/NotLike` ya existentes. **Paralela** con Fases 1, 2.

SqlKata: `WhereLike`, `WhereStarts`, `WhereEnds`, `WhereContains` (+ variantes OR/Not/OrNot).
Por defecto case-insensitive via `LOWER()`.

- [ ] `Clauses.pas`: añadir `CaseSensitive: Boolean` a `TWhereClause`
- [ ] `WhereLike(Column, Pattern; CaseSensitive=False)` / `WhereNotLike` / `OrWhereLike` / `OrWhereNotLike`
- [ ] `WhereStarts(Column, Value; CaseSensitive=False)` / Not / Or / OrNot — añade `%` al final
- [ ] `WhereEnds(Column, Value; CaseSensitive=False)` / Not / Or / OrNot — añade `%` al inicio
- [ ] `WhereContains(Column, Value; CaseSensitive=False)` / Not / Or / OrNot — envuelve en `%`
- [ ] `TQueryImpl`: si `CaseSensitive=False` → `Value := LowerCase(Value)`; modificar patrón según método
- [ ] `CompileWhere` (caso Like/NotLike): sin CS → `LOWER(col) LIKE ?`; con CS → `col LIKE ?`

### Tests

- [ ] Nuevo spec: `test/SQLCute.String.Feat.pas`

---

## Fase 4 — JOIN: callbacks compuestos + subquery JOIN

> **Paralela** con Fases 1-3. Más compleja.

- [ ] `Daf.SQLCute.pas`: `TJoinBuilderCallback = reference to function(J: IQuery): IQuery`
- [ ] `Clauses.pas`: añadir `ConditionClauses: TArray<TAbstractClause>` a `TJoinClause`
- [ ] `IQuery`: `Join(Table, Callback)` / `LeftJoin(Table, Callback)` / `RightJoin(Table, Callback)` / `FullOuterJoin(Table, Callback)`
- [ ] `TQueryImpl`: `TQueryImpl` temporal como join-builder; copiar sus WHERE-clauses a `TJoinClause.ConditionClauses`
- [ ] `CompileJoin`: si `ConditionClauses.Length > 0` → `ON (CompileWhere-inner)`
- [ ] `Clauses.pas`: nuevo `TJoinSubqueryClause` con `SubQuery: IInterface` + `Alias: string` + `ConditionClauses`
- [ ] `IQuery`: sobrecargas `LeftJoin(SubQuery, Alias, Callback)`, `Join(SubQuery, Alias, Callback)`, etc.
- [ ] `CompileJoin`: detectar `TJoinSubqueryClause` → `JOIN (subquery) AS alias ON (...)`

### Tests

- [ ] Extender spec: `test/SQLCute.Join.Feat.pas`

---

## Fase 5 — SELECT subquery + FromRaw + WithRaw

> **Paralela** con Fases 1-4.

- [ ] `Clauses.pas`: añadir `SubQuery: IInterface` a `TSelectColumn`
- [ ] `IQuery`: `Select(SubQuery: IQuery; Alias: string): IQuery`
- [ ] `CompileSelect`: si `SubQuery ≠ nil` → `(CompileSubQuery) AS alias`
- [ ] `Clauses.pas`: nuevo `TFromRawClause` con `RawSql: string` + `Bindings: TArray<Variant>` + `Alias: string`
- [ ] `IQuery`: `FromRaw(Sql: string; Bindings: TArray<Variant>; Alias: string = ''): IQuery`
- [ ] `CompileFrom`: detectar `TFromRawClause` → emitir raw + registrar bindings
- [ ] `Clauses.pas`: ampliar `TWithClause` con `IsRawSql: Boolean` + `RawSql: string` + `RawBindings: TArray<Variant>`
- [ ] `IQuery`: `WithRaw(Name: string; Sql: string; Bindings: TArray<Variant>): IQuery`
- [ ] `CompileWith`: si `IsRawSql` → CTE `name AS (rawSql)` + bindings

### Tests

- [ ] Extender spec: `test/SQLCute.Select.Feat.pas`

---

## Fase 6 — Compiladores de dialecto (nuevo módulo `Daf.SQLCute.Compilers`)

> Sin dependencias de Fases 1-5. **Prerequisito de Fase 7**.
> **Nuevo paquete** en `src/SQLCute.Compilers/`.

### Estructura de archivos

```
src/SQLCute.Compilers/
  Daf.SQLCute.Compiler.SqlServer.pas
  Daf.SQLCute.Compiler.Postgres.pas
  Daf.SQLCute.Compiler.MySql.pas
  Daf.SQLCute.Compiler.SQLite.pas
  Daf.SQLCute.Compiler.Oracle.pas
  Daf.SQLCute.Compiler.Firebird.pas
  Daf.SQLCute.Compilers.dpk
  Daf.SQLCute.Compilers.dproj
  test/
    SQLCute.Compilers.Feat.pas
    SQLCute.Compilers.Steps.pas
    SQLCuteCompilersSpecs.dpr
```

### Características por compilador

| Compilador | Quoting | Parámetros | Paginación |
|---|---|---|---|
| `TSqlServerCompiler` | `[col]` | `@p0, @p1, ...` | `SELECT TOP (N)` + `OFFSET N ROWS FETCH NEXT M ROWS ONLY` |
| `TPostgresCompiler` | `"col"` | `$1, $2, ...` | `LIMIT N OFFSET M` |
| `TMySqlCompiler` | `` `col` `` | `?` | `LIMIT N OFFSET M` |
| `TSQLiteCompiler` | `"col"` | `?` | `LIMIT N OFFSET M` |
| `TOracleCompiler` | `"COL"` (uppercase) | `:p1, :p2, ...` | `FETCH FIRST N ROWS ONLY` / `OFFSET N ROWS FETCH NEXT M ROWS ONLY` |
| `TFirebirdCompiler` | sin quoting | `?` | `ROWS M TO N` |

### Checklist

- [ ] Crear directorio `src/SQLCute.Compilers/` + `.dpk` + `.dproj`
- [ ] `TSqlServerCompiler`: `WrapColumn` → `[col]`, `WrapTable` → `[table]`, params `@p0`, `CompileLimit` con TOP, `CompileOffset` con ROWS FETCH
- [ ] `TPostgresCompiler`: `WrapColumn` → `"col"`, params `$1`/`$2` (contador en compilador), `LIMIT N OFFSET M`
- [ ] `TMySqlCompiler`: `WrapColumn` → `` `col` ``, params `?`, `LIMIT N OFFSET M`
- [ ] `TSQLiteCompiler`: `WrapColumn` → `"col"`, params `?`, `LIMIT N OFFSET M`
- [ ] `TOracleCompiler`: `WrapColumn` → `"COL"` (uppercase), params `:p1`/`:p2`, `FETCH FIRST N ROWS ONLY`
- [ ] `TFirebirdCompiler`: sin quoting, params `?`, `ROWS M TO N`
- [ ] Añadir `FParamIndex: Integer` (reset en cada `Compile`) para compiladores con params indexados

### Tests

- [ ] Nuevo spec: `test/SQLCute.Compilers.Feat.pas` con scenarios por dialecto (quoting, TOP, LIMIT, params)

---

## Fase 7 — Date Operations (WhereDate / WhereTime / WhereDatePart)

> **Depende de Fase 6** (emisión SQL distinta por motor).

- [ ] `Clauses.pas`: enum `TDatePart = (dpDate, dpTime, dpYear, dpMonth, dpDay, dpHour, dpMinute)`
- [ ] `Clauses.pas`: nuevo `TDateWhereClause` con `DatePart`, `Column`, `Op`, `Value`, `Connector`
- [ ] `IQuery`: `WhereDate(Column, Value)` / `OrWhereDate(...)`
- [ ] `IQuery`: `WhereTime(Column, Op, Value)` / `OrWhereTime(...)`
- [ ] `IQuery`: `WhereDatePart(Part, Column, Value)` / `OrWhereDatePart(...)`
- [ ] `TAnsiSqlCompiler`: nuevo método virtual `CompileDateWhere` — fallback: `col op ?`
- [ ] `CompileWhere`: detectar `TDateWhereClause` → llamar `CompileDateWhere`
- [ ] `TSqlServerCompiler`: `CAST(col as date)` / `CAST(col as time)` / `DATEPART(PART, col)`
- [ ] `TPostgresCompiler`: `col::date` / `col::time` / `DATE_PART('PART', col)`
- [ ] `TMySqlCompiler`: `DATE(col)` / `TIME(col)` / `DAY(col)`, `MONTH(col)`, etc.
- [ ] `TSQLiteCompiler`: `date(col)` / `time(col)` / `strftime('%d', col)`
- [ ] `TOracleCompiler`: `TRUNC(col)` / `EXTRACT(part FROM col)`
- [ ] `TFirebirdCompiler`: `CAST(col AS DATE)` / `EXTRACT(part FROM col)`

### Tests

- [ ] Nuevo spec: `test/SQLCute.Date.Feat.pas` con scenarios multi-dialecto

---

## Dependencias entre fases

```
F1 ─┐
F2 ─┤
F3 ─┼─► (independientes, paralelas)
F4 ─┤
F5 ─┘

F6 ──► F7
```

Fases 1-5 son completamente independientes entre sí y pueden ejecutarse en paralelo.
Fase 6 debe completarse antes de iniciar Fase 7.

---

## Verificación final

1. Compilar `Daf.SQLCute.Abstractions.dpk` + `Daf.SQLCute.Compilers.dpk` sin warnings.
2. Ejecutar `SQLCuteSpecs` — todos los scenarios en verde.
3. Ejecutar `SQLCuteCompilersSpecs` — todos los scenarios en verde.
4. Ampliar `SQLCuteSample.dpr` para cubrir nuevas capacidades; verificar salida manual.

---

## Decisiones de diseño

| Decisión | Resolución |
|---|---|
| Ubicación compiladores | Módulo separado `src/SQLCute.Compilers/` — sin contaminar `SQLCute.Abstractions` |
| `WhereFalse`/`WhereTrue` | Usan param binding (diferencia deliberada con SqlKata que emite literales) |
| `TNestedWhereClause` | Usa `TArray<TAbstractClause>` en lugar de `IQuery` anidado → evita ref-counting extra |
| `TQueryBuilderCallback` | `reference to function(Q: IQuery): IQuery` — Delphi anonymous methods desde D2009 |
| `JoinBuilderCallback` | Mismo mecanismo: `TQueryImpl` temporal; se copian solo las WHERE-clauses al nodo JOIN |
| `ForPage` | 1-based, igual que SqlKata |
| `ParamPlaceholder` con contador | El contador de índice vive en el compilador (campo `FParamIndex: Integer`, reset en cada `Compile`) |
