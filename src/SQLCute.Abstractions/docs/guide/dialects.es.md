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

Todos los compiladores de dialecto están en el paquete `SQLCute.Compilers` — añádelo a tu proyecto junto con `SQLCute.Abstractions` y `SQLCute`.

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
uses Daf.SQLCute.Compiler;

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
