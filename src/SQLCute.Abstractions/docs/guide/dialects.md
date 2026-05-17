# Dialect Compilers

**🌍 Language: English | [Español](dialects.es.md)**

← [Back to Guide](../GUIDE.md)

---

## What a Dialect Compiler Does

The default `TAnsiSqlCompiler` generates generic ANSI SQL with `?` placeholders and no identifier quoting. Dialect compilers extend it to handle:

- **Identifier quoting** — wrapping table/column names to avoid keyword conflicts
- **Parameter placeholders** — `?`, `$1`, `:p1`, `@p1`, etc.
- **Date casting** — dialect-specific expressions for `WhereDate`/`WhereDatePart`
- **Pagination** — `LIMIT/OFFSET` vs `FETCH NEXT … ROWS ONLY` vs `ROWNUM`

---

## Dialect Matrix

| Dialect class | Quote char | Placeholder | Package unit |
|---------------|-----------|-------------|-------------|
| `TAnsiSqlCompiler` | none | `?` | `Daf.SQLCute.Compiler` |
| `TPostgresCompiler` | `"` | `$1`, `$2`, … | `Daf.SQLCute.Compiler.Postgres` |
| `TMySqlCompiler` | `` ` `` | `?` | `Daf.SQLCute.Compiler.MySql` |
| `TSQLiteCompiler` | `"` | `?` | `Daf.SQLCute.Compiler.SQLite` |
| `TSqlServerCompiler` | `[` … `]` | `@p1`, `@p2`, … | `Daf.SQLCute.Compiler.SqlServer` |
| `TOracleCompiler` | `"` | `:p1`, `:p2`, … | `Daf.SQLCute.Compiler.Oracle` |
| `TFirebirdCompiler` | `"` | `?` | `Daf.SQLCute.Compiler.Firebird` |

All dialect compilers are in the `SQLCute.Compilers` package — add it to your project alongside `SQLCute.Abstractions` and `SQLCute`.

---

## Using a Dialect Compiler

```pascal
uses
  Daf.SQLCute,
  Daf.SQLCute.Compiler.Postgres;

var Q := TQuery.New
  .From('users')
  .Where('active', True)
  .OrderBy('name');

var Compiler := TPostgresCompiler.Create;
var R := Compiler.Compile(Q);
// R.SQL      → 'SELECT * FROM "users" WHERE "active" = $1 ORDER BY "name"'
// R.Bindings → [True]
```

Or use the shorthand `Q.Compile(Compiler)`:

```pascal
var R := Q.Compile(TPostgresCompiler.Create);
```

---

## Dependency Injection

Register the compiler as a singleton and inject `IQueryCompiler` wherever needed:

```pascal
// Composition root
Services.AddSingleton<IQueryCompiler, TPostgresCompiler>;

// In a repository
type
  TOrderRepository = class
  private
    FCompiler: IQueryCompiler;
  public
    constructor Create(Compiler: IQueryCompiler);
    function FindByStatus(const Status: string): TSQLResult;
  end;

function TOrderRepository.FindByStatus(const Status: string): TSQLResult;
begin
  Result := FCompiler.Compile(
    TQuery.New.From('orders').Where('status', Status));
end;
```

---

## Implementing a Custom Compiler

Extend `TAnsiSqlCompiler` and override only what differs:

```pascal
uses Daf.SQLCute.Compiler;

type
  TMyDbCompiler = class(TAnsiSqlCompiler)
  protected
    function WrapColumn(const Col: string): string; override;
    function WrapTable(const Table: string): string; override;
    function ParamPlaceholder: string; override;
  private
    FIndex: Integer;
  public
    function Compile(const Query: IQuery): TSQLResult; override;
  end;

function TMyDbCompiler.WrapColumn(const Col: string): string;
begin
  Result := '[' + Col + ']';
end;

function TMyDbCompiler.ParamPlaceholder: string;
begin
  Inc(FIndex);
  Result := ':param' + IntToStr(FIndex);
end;

function TMyDbCompiler.Compile(const Query: IQuery): TSQLResult;
begin
  FIndex := 0;
  Result := inherited Compile(Query);
end;
```

---

## Pagination per Dialect

SQLCute emits `LIMIT … OFFSET …` by default. SQL Server and Oracle compilers override pagination to use their native syntax:

| Dialect | Pagination SQL |
|---------|---------------|
| ANSI / Postgres / MySQL / SQLite / Firebird | `LIMIT n OFFSET m` |
| SQL Server | `ORDER BY … OFFSET m ROWS FETCH NEXT n ROWS ONLY` |
| Oracle | `OFFSET m ROWS FETCH NEXT n ROWS ONLY` |

> SQL Server requires an `ORDER BY` clause when using `OFFSET/FETCH`. Add `.OrderBy(…)` before calling `.ForPage(…)` with `TSqlServerCompiler`.
