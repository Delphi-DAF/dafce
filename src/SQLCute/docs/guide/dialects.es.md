# Compiladores de Dialecto

**🌍 Idioma: [English](dialects.md) | Español**

← [Volver a la Guía](../GUIDE.es.md)

---

## Qué Hace un Compilador de Dialecto

El `TAnsiSqlCompiler` por defecto genera SQL ANSI genérico con marcadores `?` y sin entrecomillado de identificadores. Los compiladores de dialecto lo extienden para manejar:

- **Entrecomillado de identificadores** — envolver nombres de tabla/columna para evitar conflictos con palabras reservadas
- **Marcadores de parámetros** — `?` por defecto, y el del motor (`$1`, `:p1`, `@p0`) en el estilo nativo
- **Cast de fechas** — expresiones específicas del dialecto para `WhereDate`/`WhereDatePart`
- **Paginación** — `LIMIT/OFFSET` vs `FETCH NEXT … ROWS ONLY` vs `ROWS m TO n`
- **Emisión de la ordenación de NULL** — `NULLS FIRST/LAST` nativo, o una emulación donde el dialecto no tiene esa sintaxis

---

## Matriz de Dialectos

Todos los compiladores escriben `?` por defecto. La columna del marcador es lo que escribe cada uno en el [estilo nativo](#estilos-de-marcador).

| Clase del compilador | Comillas | Marcador del estilo nativo | Unidad del paquete |
|---------------------|---------|---------|-------------------|
| `TAnsiSqlCompiler` | ninguna | `?` | `Daf.SQLCute.Compiler` |
| `TPostgresCompiler` | `"` | `$1`, `$2`… | `Daf.SQLCute.Compiler.Postgres` |
| `TMySqlCompiler` | `` ` `` | `?` | `Daf.SQLCute.Compiler.MySql` |
| `TSQLiteCompiler` | `"` | `?` | `Daf.SQLCute.Compiler.SQLite` |
| `TSqlServerCompiler` | `[`…`]` | `@p0`, `@p1`… | `Daf.SQLCute.Compiler.SqlServer` |
| `TOracleCompiler` | `"`, en mayúsculas | `:p1`, `:p2`… | `Daf.SQLCute.Compiler.Oracle` |
| `TFirebirdCompiler` | ninguna | `?` | `Daf.SQLCute.Compiler.Firebird` |

Un dialecto que entrecomilla entrecomilla por separado cada parte de un nombre cualificado —`"public"."users"`, `[users].[id]`—, deja sin comillas un `*` final y duplica dentro de un nombre su carácter de cierre. Una columna escrita `nombre as alias` se separa: `Select('users.name as n')` compila a `SELECT "users"."name" AS "n"` en Postgres. Oracle escribe sus alias sin `AS`: `FROM "USERS" "U"`.

Una comparación lógica —`WhereTrue`, `WhereFalse`, o `Where` con un valor lógico— compara con el literal del dialecto, sin enlace: `true` y `false` en ANSI, Postgres, MySQL, Oracle y Firebird, `1` y `0` en SQLite, y `cast(1 as bit)` y `cast(0 as bit)` en SQL Server. Oracle entiende `true` desde Oracle 23. En Firebird, una columna `SMALLINT` que hace de indicador rechaza `= true` con un error de conversión: se compara con un número, `Where('flag', 1)`.

Todos los compiladores de dialecto viven en el paquete `Daf.SQLCute`, junto al `TAnsiSqlCompiler` del que extienden. Basta con requerir `Daf.SQLCute`: no hay un paquete de compiladores aparte que añadir.

---

## Estilos de Marcador

La propiedad `Placeholders` de un compilador fija lo que escribe por cada enlace, y por un `\?` de una expresión en bruto:

| `TPlaceholderStyle` | Cada enlace | `\?` |
|---|---|---|
| `FireDAC`, el de por defecto | `?`, en todos los dialectos | `??` fuera de una cadena, `?` dentro de una |
| `Native` | el del dialecto: `$1`… en Postgres, `@p0`… en SQL Server, `:p1`… en Oracle, `?` en los demás | `?` |

FireDAC ejecuta el estilo por defecto tal cual, en cualquier motor: liga `?` y lo traduce al marcador del motor, lee `??` como un `?` literal, y no busca parámetros dentro de una cadena.

Una cadena es lo que va entre comillas simples, y un `''` dentro de ella no la cierra. Otras formas de cadena, como `$$…$$` y `E'…'` de Postgres, no se siguen: un `\?` dentro de una sale como `??`. El estilo nativo escribe ahí `?`.

Para el estilo nativo, fija la propiedad antes de que la instancia compile. El compilador cuenta referencias, así que guárdalo en un `IQueryCompiler`:

```pascal
uses
  Daf.SQLCute,
  Daf.SQLCute.Compiler,
  Daf.SQLCute.Compiler.Postgres;

var Postgres := TPostgresCompiler.Create;
Postgres.Placeholders := TPlaceholderStyle.Native;
var Compilador: IQueryCompiler := Postgres;
var R := Compilador.Compile(TQuery.New.From('usuarios').Where('estado', 'abierto'));
// R.SQL → 'SELECT * FROM "usuarios" WHERE "estado" = $1'
```

`TPlaceholderStyle` vive en `Daf.SQLCute.Compiler`. Cambiar `Placeholders` mientras otro hilo compila con la misma instancia no está soportado.

---

## Marcadores en Expresiones en Bruto

Cada `?` de una expresión en bruto —`WhereRaw`, `FromRaw`, `WithRaw` y las demás formas `Raw`— es un marcador, cuenta con los que genera la consulta, y su enlace ocupa ese mismo lugar; el estilo nativo lo numera con ellos. Para un signo de interrogación literal, como el operador `?` de Postgres sobre `jsonb`, se escribe `\?`:

```pascal
TQuery.New.From('docs').WhereRaw('data \? ''tag''').Where('id', 1)
// Postgres                  → SELECT * FROM "docs" WHERE data ?? 'tag' AND "id" = ?
// Postgres, estilo nativo   → SELECT * FROM "docs" WHERE data ? 'tag' AND "id" = $1
```

---

## Usar un Compilador de Dialecto

```pascal
uses
  Daf.SQLCute,
  Daf.SQLCute.Compiler.Postgres;

var Q := TQuery.New
  .From('usuarios')
  .Where('estado', 'abierto')
  .OrderBy('nombre');

var Compilador: IQueryCompiler := TPostgresCompiler.Create;
var R := Compilador.Compile(Q);
// R.SQL      → 'SELECT * FROM "usuarios" WHERE "estado" = ? ORDER BY "nombre"'
// R.Bindings → ['abierto']
```

O usa el atajo `Q.Compile(Compilador)`:

```pascal
var R := Q.Compile(TPostgresCompiler.Create);
```

---

## Inyección de Dependencias

Una instancia de compilador no guarda nada entre compilaciones: sirve compilaciones sucesivas, anidadas y concurrentes. Regístrala como singleton e inyecta `IQueryCompiler` donde sea necesario:

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

Un compilador registrado por su tipo compila en el estilo por defecto. Para el estilo nativo, registra una instancia preparada como en [Estilos de Marcador](#estilos-de-marcador): `Services.AddSingleton<IQueryCompiler>(Compilador)`.

---

## Implementar un Compilador Personalizado

Extiende `TAnsiSqlCompiler` y sobreescribe solo lo que difiere:

```pascal
uses System.SysUtils, Daf.SQLCute, Daf.SQLCute.Compiler;

type
  TMiDbCompilador = class(TAnsiSqlCompiler)
  protected
    function WrapIdentifier(const Name: string): string; override;
    function FormatPlaceholder(const Index: Integer): string; override;
  end;

function TMiDbCompilador.WrapIdentifier(const Name: string): string;
begin
  Result := '[' + StringReplace(Name, ']', ']]', [rfReplaceAll]) + ']';
end;

function TMiDbCompilador.FormatPlaceholder(const Index: Integer): string;
begin
  Result := ':' + IntToStr(Index + 1);
end;
```

`WrapIdentifier` entrecomilla un solo nombre. `WrapColumn` y `WrapTable` separan un alias y parten un nombre cualificado por cada punto, y lo llaman para cada parte; `AliasKeyword` es lo que une un nombre con su alias.

Cada método que emite SQL —de `CompileSelect` a `CompileDelete`, `CompileWhereList`, `CompileDateWhere`, `CompileOrderByColumn`— recibe la compilación, un `TSqlCompilation`, como primer parámetro, le añade sus enlaces con `AddBinding` y escribe cada marcador como `?`. En el estilo nativo, una pasada final sustituye el `?` de cada posición por lo que devuelve para ella `FormatPlaceholder`, contando desde 0.

---

## Paginación por Dialecto

SQLCute emite `LIMIT … OFFSET …` por defecto. Los compiladores de SQL Server, Oracle y Firebird sobreescriben la paginación para usar su sintaxis nativa:

| Dialecto | SQL de paginación |
|----------|------------------|
| ANSI / Postgres / MySQL / SQLite | `LIMIT n OFFSET m` |
| SQL Server | `ORDER BY … OFFSET m ROWS FETCH NEXT n ROWS ONLY` |
| Oracle | `OFFSET m ROWS FETCH NEXT n ROWS ONLY` |
| Firebird | `ROWS m+1 TO m+n` |

> SQL Server exige un `ORDER BY` con `OFFSET/FETCH`. Si la consulta no lo tiene, `TSqlServerCompiler` añade `ORDER BY (SELECT 0)` —`ORDER BY 1` en una consulta `Distinct`—, así que la página compila; qué filas contiene solo es estable con un `.OrderBy(…)` propio.

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
| Firebird | `col ASC NULLS LAST` (nativo) | NULL al **principio** en ascendente, al **final** en descendente |
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
