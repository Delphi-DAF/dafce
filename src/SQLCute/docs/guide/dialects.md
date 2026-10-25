# Dialect Compilers

**🌍 Language: English | [Español](dialects.es.md)**

← [Back to Guide](../GUIDE.md)

---

## What a Dialect Compiler Does

The default `TAnsiSqlCompiler` generates generic ANSI SQL with `?` placeholders and no identifier quoting. Dialect compilers extend it to handle:

- **Identifier quoting** — wrapping table/column names to avoid keyword conflicts
- **Parameter placeholders** — `?` by default, and the engine's own (`$1`, `:p1`, `@p0`) in the native style
- **Date casting** — dialect-specific expressions for `WhereDate`/`WhereDatePart`
- **Pagination** — `LIMIT/OFFSET` vs `FETCH NEXT … ROWS ONLY` vs `ROWS m TO n`
- **NULL-ordering emission** — native `NULLS FIRST/LAST`, or an emulation where the dialect has no such syntax

---

## Dialect Matrix

Every compiler writes `?` by default. The placeholder column is what each one writes in the [native style](#placeholder-styles).

| Dialect class | Quote char | Native-style placeholder | Package unit |
|---------------|-----------|-------------|-------------|
| `TAnsiSqlCompiler` | none | `?` | `Daf.SQLCute.Compiler` |
| `TPostgresCompiler` | `"` | `$1`, `$2`, … | `Daf.SQLCute.Compiler.Postgres` |
| `TMySqlCompiler` | `` ` `` | `?` | `Daf.SQLCute.Compiler.MySql` |
| `TSQLiteCompiler` | `"` | `?` | `Daf.SQLCute.Compiler.SQLite` |
| `TSqlServerCompiler` | `[` … `]` | `@p0`, `@p1`, … | `Daf.SQLCute.Compiler.SqlServer` |
| `TOracleCompiler` | `"`, upper-cased | `:p1`, `:p2`, … | `Daf.SQLCute.Compiler.Oracle` |
| `TFirebirdCompiler` | none | `?` | `Daf.SQLCute.Compiler.Firebird` |

A quoting dialect quotes each part of a qualified name on its own — `"public"."users"`, `[users].[id]` — leaves a trailing `*` bare, and doubles its closing character inside a name. A column written `name as alias` is split: `Select('users.name as n')` compiles to `SELECT "users"."name" AS "n"` in Postgres. Oracle writes its aliases without `AS`: `FROM "USERS" "U"`.

A logical comparison — `WhereTrue`, `WhereFalse`, or `Where` with a logical value — compares with the dialect's literal, with no binding: `true` and `false` in ANSI, Postgres, MySQL and Oracle, `1` and `0` in SQLite and Firebird, and `cast(1 as bit)` and `cast(0 as bit)` in SQL Server. Oracle understands `true` from Oracle 23. In Firebird, `= 1` suits a `SMALLINT` flag column, and a `BOOLEAN` column (Firebird 3 and later) rejects it with a conversion error: for one, write the condition with `WhereRaw`.

All dialect compilers ship in the `Daf.SQLCute` package, alongside the `TAnsiSqlCompiler` they extend. Requiring `Daf.SQLCute` is enough — there is no separate compilers package to add.

---

## Placeholder Styles

The `Placeholders` property of a compiler sets what it writes for each binding, and for a `\?` in a raw expression:

| `TPlaceholderStyle` | Each binding | `\?` |
|---|---|---|
| `FireDAC`, the default | `?`, in every dialect | `??` outside a string, `?` inside one |
| `Native` | the dialect's own: `$1`… on Postgres, `@p0`… on SQL Server, `:p1`… on Oracle, `?` in the others | `?` |

FireDAC executes the default style as it is, on any engine: it binds `?` and translates it into the engine's own placeholder, reads `??` as a literal `?`, and looks for no parameter inside a string.

A string is what goes between single quotes, and a `''` inside it does not close it. Other string forms, such as `$$…$$` and `E'…'` on Postgres, are not followed: a `\?` inside one comes out as `??`. The native style writes `?` there.

For the native style, set the property before the instance compiles. The compiler is reference counted, so hold it in an `IQueryCompiler`:

```pascal
uses
  Daf.SQLCute,
  Daf.SQLCute.Compiler,
  Daf.SQLCute.Compiler.Postgres;

var Postgres := TPostgresCompiler.Create;
Postgres.Placeholders := TPlaceholderStyle.Native;
var Compiler: IQueryCompiler := Postgres;
var R := Compiler.Compile(TQuery.New.From('users').Where('status', 'open'));
// R.SQL → 'SELECT * FROM "users" WHERE "status" = $1'
```

`TPlaceholderStyle` lives in `Daf.SQLCute.Compiler`. Changing `Placeholders` while another thread compiles with the same instance is not supported.

---

## Placeholders in Raw Expressions

Every `?` in a raw expression — `WhereRaw`, `FromRaw`, `WithRaw` and the other `Raw` forms — is a placeholder, counted with those the query generates, and its binding takes that same place; the native style numbers it with them. Write `\?` for a literal question mark, such as the `?` operator of Postgres on `jsonb`:

```pascal
TQuery.New.From('docs').WhereRaw('data \? ''tag''').Where('id', 1)
// Postgres               → SELECT * FROM "docs" WHERE data ?? 'tag' AND "id" = ?
// Postgres, native style → SELECT * FROM "docs" WHERE data ? 'tag' AND "id" = $1
```

---

## Using a Dialect Compiler

```pascal
uses
  Daf.SQLCute,
  Daf.SQLCute.Compiler.Postgres;

var Q := TQuery.New
  .From('users')
  .Where('status', 'open')
  .OrderBy('name');

var Compiler: IQueryCompiler := TPostgresCompiler.Create;
var R := Compiler.Compile(Q);
// R.SQL      → 'SELECT * FROM "users" WHERE "status" = ? ORDER BY "name"'
// R.Bindings → ['open']
```

Or use the shorthand `Q.Compile(Compiler)`:

```pascal
var R := Q.Compile(TPostgresCompiler.Create);
```

---

## Dependency Injection

A compiler instance keeps nothing between compilations: one instance serves successive, nested and concurrent compilations. Register it as a singleton and inject `IQueryCompiler` wherever needed:

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

A compiler registered by its type compiles in the default style. For the native style, register an instance set up as in [Placeholder Styles](#placeholder-styles): `Services.AddSingleton<IQueryCompiler>(Compiler)`.

---

## Implementing a Custom Compiler

Extend `TAnsiSqlCompiler` and override only what differs:

```pascal
uses System.SysUtils, Daf.SQLCute, Daf.SQLCute.Compiler;

type
  TMyDbCompiler = class(TAnsiSqlCompiler)
  protected
    function WrapIdentifier(const Name: string): string; override;
    function FormatPlaceholder(const Index: Integer): string; override;
  end;

function TMyDbCompiler.WrapIdentifier(const Name: string): string;
begin
  Result := '[' + StringReplace(Name, ']', ']]', [rfReplaceAll]) + ']';
end;

function TMyDbCompiler.FormatPlaceholder(const Index: Integer): string;
begin
  Result := ':' + IntToStr(Index + 1);
end;
```

`WrapIdentifier` quotes a single name. `WrapColumn` and `WrapTable` split off an alias and split a qualified name at each dot, and call it for each part; `AliasKeyword` is what joins a name with its alias.

Every method that emits SQL — `CompileSelect` to `CompileDelete`, `CompileWhereList`, `CompileDateWhere`, `CompileOrderByColumn` — receives the compilation, a `TSqlCompilation`, as its first parameter, adds its bindings to it with `AddBinding`, and writes each placeholder as `?`. In the native style, a final pass replaces the `?` at each position with what `FormatPlaceholder` returns for it, counted from 0.

---

## Pagination per Dialect

SQLCute emits `LIMIT … OFFSET …` by default. The SQL Server, Oracle and Firebird compilers override pagination to use their native syntax:

| Dialect | Pagination SQL |
|---------|---------------|
| ANSI / Postgres / MySQL / SQLite | `LIMIT n OFFSET m` |
| SQL Server | `ORDER BY … OFFSET m ROWS FETCH NEXT n ROWS ONLY` |
| Oracle | `OFFSET m ROWS FETCH NEXT n ROWS ONLY` |
| Firebird | `ROWS m+1 TO m+n` |

> SQL Server requires an `ORDER BY` with `OFFSET/FETCH`. When the query has none, `TSqlServerCompiler` adds `ORDER BY (SELECT 0)` —`ORDER BY 1` on a `Distinct` query—, so the page compiles; which rows it holds is only stable with an `.OrderBy(…)` of your own.

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
