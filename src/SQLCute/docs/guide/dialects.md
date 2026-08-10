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
- **NULL-ordering emission** — native `NULLS FIRST/LAST`, or an emulation where the dialect has no such syntax

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

All dialect compilers ship in the `Daf.SQLCute` package, alongside the `TAnsiSqlCompiler` they extend. Requiring `Daf.SQLCute` is enough — there is no separate compilers package to add.

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

---

## NULL Ordering per Dialect

[`NullsFirst` / `NullsLast`](select.md#where-nulls-land) mean the same thing on
every dialect. Where the syntax exists it is emitted; where it doesn't, the
compiler emulates it with a leading sort key. Neither form adds a binding.

| Dialect | Qualified `NullsLast` compiles to | Its own default for an unqualified `ORDER BY col` |
|---------|-----------------------------------|--------------------------------------------------|
| ANSI / Postgres | `col ASC NULLS LAST` (native) | NULLs **last** ascending, **first** descending (NULL sorts as the largest value) |
| Oracle | `"COL" ASC NULLS LAST` (native) | NULLs **last** ascending, **first** descending |
| SQLite | `"col" ASC NULLS LAST` (native, **needs SQLite ≥ 3.30**) | NULLs **first** ascending, **last** descending (NULL is the smallest value) |
| Firebird | `col ASC NULLS LAST` (native) | **Firebird ≥ 2.0:** NULLs **first** ascending, **last** descending. Firebird 1.0/1.5 put them last in both directions |
| MySQL | `` (`col` IS NULL) ASC, `col` ASC `` (emulated) | NULLs **first** ascending, **last** descending |
| SQL Server | `CASE WHEN [col] IS NULL THEN 1 ELSE 0 END ASC, [col] ASC` (emulated) | NULLs **first** ascending, **last** descending |

`col IS NULL` is 1 for NULL and 0 otherwise, so the emulated key sorted `ASC`
holds NULLs back and sorted `DESC` brings them forward. SQL Server needs the
`CASE` because T-SQL allows no boolean expression in `ORDER BY`.

Notes:

- **The defaults are the databases', not SQLCute's.** An unqualified `OrderBy`
  emits no NULL clause on any dialect, so the column above is what your database
  does with it. Qualify the sort to get one answer everywhere.
- **SQLite < 3.30** rejects the native syntax. Since emission lives behind the
  overridable `CompileOrderByColumn`, a project pinned to an older engine can
  subclass `TSQLiteCompiler` and emulate as MySQL does.
- **Firebird** does not use an index on a column ordered with a non-default
  `NULLS` placement.
