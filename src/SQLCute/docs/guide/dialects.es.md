# Compiladores de Dialecto

**🌍 Idioma: [English](dialects.md) | Español**

← [Volver a la Guía](../GUIDE.es.md)

---

## Qué Hace un Compilador de Dialecto

El `TAnsiSqlCompiler` por defecto genera SQL ANSI genérico con marcadores `?` y sin entrecomillado de identificadores. Los compiladores de dialecto lo extienden para manejar:

- **Entrecomillado de identificadores** — envolver nombres de tabla/columna para evitar conflictos con palabras reservadas
- **Marcadores de parámetros** — `?`, `$1`, `:p1`, `@p1`, etc.
- **Cast de fechas** — expresiones específicas del dialecto para `WhereDate`/`WhereDatePart`
- **Paginación** — `LIMIT/OFFSET` vs `FETCH NEXT … ROWS ONLY` vs `ROWNUM`
- **Emisión de la ordenación de NULL** — `NULLS FIRST/LAST` nativo, o una emulación donde el dialecto no tiene esa sintaxis

---

## Matriz de Dialectos

| Clase del compilador | Comillas | Marcador | Unidad del paquete |
|---------------------|---------|---------|-------------------|
| `TAnsiSqlCompiler` | ninguna | `?` | `Daf.SQLCute.Compiler` |
| `TPostgresCompiler` | `"` | `$1`, `$2`… | `Daf.SQLCute.Compiler.Postgres` |
| `TMySqlCompiler` | `` ` `` | `?` | `Daf.SQLCute.Compiler.MySql` |
| `TSQLiteCompiler` | `"` | `?` | `Daf.SQLCute.Compiler.SQLite` |
| `TSqlServerCompiler` | `[`…`]` | `@p1`, `@p2`… | `Daf.SQLCute.Compiler.SqlServer` |
| `TOracleCompiler` | `"` | `:p1`, `:p2`… | `Daf.SQLCute.Compiler.Oracle` |
| `TFirebirdCompiler` | `"` | `?` | `Daf.SQLCute.Compiler.Firebird` |

Todos los compiladores de dialecto viven en el paquete `Daf.SQLCute`, junto al `TAnsiSqlCompiler` del que extienden. Basta con requerir `Daf.SQLCute`: no hay un paquete de compiladores aparte que añadir.

---

## Usar un Compilador de Dialecto

```pascal
uses
  Daf.SQLCute,
  Daf.SQLCute.Compiler.Postgres;

var Q := TQuery.New
  .From('usuarios')
  .Where('activo', True)
  .OrderBy('nombre');

var Compilador := TPostgresCompiler.Create;
var R := Compilador.Compile(Q);
// R.SQL      → 'SELECT * FROM "usuarios" WHERE "activo" = $1 ORDER BY "nombre"'
// R.Bindings → [True]
```

O usa el atajo `Q.Compile(Compilador)`:

```pascal
var R := Q.Compile(TPostgresCompiler.Create);
```

---

## Inyección de Dependencias

Registra el compilador como singleton e inyecta `IQueryCompiler` donde sea necesario:

```pascal
// Raíz de composición
Services.AddSingleton<IQueryCompiler, TPostgresCompiler>;

// En un repositorio
type
  TRepositorioPedidos = class
  private
    FCompilador: IQueryCompiler;
  public
    constructor Create(Compilador: IQueryCompiler);
    function BuscarPorEstado(const Estado: string): TSQLResult;
  end;

function TRepositorioPedidos.BuscarPorEstado(const Estado: string): TSQLResult;
begin
  Result := FCompilador.Compile(
    TQuery.New.From('pedidos').Where('estado', Estado));
end;
```

---

## Implementar un Compilador Personalizado

Extiende `TAnsiSqlCompiler` y sobreescribe solo lo que difiere:

```pascal
uses Daf.SQLCute, Daf.SQLCute.Compiler;

type
  TMiDbCompilador = class(TAnsiSqlCompiler)
  protected
    function WrapColumn(const Col: string): string; override;
    function WrapTable(const Table: string): string; override;
    function ParamPlaceholder: string; override;
  private
    FIndice: Integer;
  public
    function Compile(const Query: IQuery): TSQLResult; override;
  end;

function TMiDbCompilador.WrapColumn(const Col: string): string;
begin
  Result := '[' + Col + ']';
end;

function TMiDbCompilador.ParamPlaceholder: string;
begin
  Inc(FIndice);
  Result := ':param' + IntToStr(FIndice);
end;

function TMiDbCompilador.Compile(const Query: IQuery): TSQLResult;
begin
  FIndice := 0;
  Result := inherited Compile(Query);
end;
```

---

## Paginación por Dialecto

SQLCute emite `LIMIT … OFFSET …` por defecto. Los compiladores de SQL Server y Oracle sobreescriben la paginación para usar su sintaxis nativa:

| Dialecto | SQL de paginación |
|----------|------------------|
| ANSI / Postgres / MySQL / SQLite / Firebird | `LIMIT n OFFSET m` |
| SQL Server | `ORDER BY … OFFSET m ROWS FETCH NEXT n ROWS ONLY` |
| Oracle | `OFFSET m ROWS FETCH NEXT n ROWS ONLY` |

> SQL Server requiere una cláusula `ORDER BY` cuando se usa `OFFSET/FETCH`. Añade `.OrderBy(…)` antes de llamar a `.ForPage(…)` con `TSqlServerCompiler`.

---

## Ordenación de NULL por Dialecto

[`NullsFirst` / `NullsLast`](select.es.md#dónde-caen-los-null) significan lo mismo
en todos los dialectos. Donde la sintaxis existe, se emite; donde no, el
compilador la emula con una clave de ordenación previa. Ninguna forma añade
parámetros.

| Dialecto | Un `NullsLast` cualificado compila a | Su propio valor por omisión para un `ORDER BY col` sin cualificar |
|----------|--------------------------------------|-------------------------------------------------------------------|
| ANSI / Postgres | `col ASC NULLS LAST` (nativo) | NULL al **final** en ascendente, al **principio** en descendente (NULL ordena como el valor mayor) |
| Oracle | `"COL" ASC NULLS LAST` (nativo) | NULL al **final** en ascendente, al **principio** en descendente |
| SQLite | `"col" ASC NULLS LAST` (nativo, **requiere SQLite ≥ 3.30**) | NULL al **principio** en ascendente, al **final** en descendente (NULL es el valor menor) |
| Firebird | `col ASC NULLS LAST` (nativo) | **Firebird ≥ 2.0:** NULL al **principio** en ascendente, al **final** en descendente. Firebird 1.0/1.5 los ponía al final en ambos sentidos |
| MySQL | `` (`col` IS NULL) ASC, `col` ASC `` (emulado) | NULL al **principio** en ascendente, al **final** en descendente |
| SQL Server | `CASE WHEN [col] IS NULL THEN 1 ELSE 0 END ASC, [col] ASC` (emulado) | NULL al **principio** en ascendente, al **final** en descendente |

`col IS NULL` vale 1 para NULL y 0 en caso contrario, así que la clave emulada
ordenada `ASC` retiene los NULL y ordenada `DESC` los adelanta. SQL Server
necesita el `CASE` porque T-SQL no admite expresiones booleanas en `ORDER BY`.

Notas:

- **Los valores por omisión son de las bases de datos, no de SQLCute.** Un
  `OrderBy` sin cualificar no emite ninguna cláusula NULL en ningún dialecto, así
  que la última columna es lo que hace tu base de datos con él. Cualifica la
  ordenación para obtener una única respuesta en todas partes.
- **SQLite < 3.30** rechaza la sintaxis nativa. Como la emisión vive detrás del
  método sobreescribible `CompileOrderByColumn`, un proyecto atado a un motor
  antiguo puede derivar de `TSQLiteCompiler` y emular como hace MySQL.
- **Firebird** no usa índice en una columna ordenada con una colocación `NULLS`
  distinta de la suya por omisión.
