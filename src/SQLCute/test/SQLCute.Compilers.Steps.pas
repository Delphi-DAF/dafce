unit SQLCute.Compilers.Steps;

{
  Step bindings for SQLCute dialect compiler specs.
  Provides Given/When/Then steps for testing dialect-specific SQL generation.
}

interface

implementation

uses
  System.SysUtils,
  System.Variants,
  Daf.MiniSpec,
  Daf.MiniSpec.Binding,
  Daf.SQLCute,
  Daf.SQLCute.Clauses,
  Daf.SQLCute.Compiler,
  Daf.SQLCute.Compiler.SqlServer,
  Daf.SQLCute.Compiler.Postgres,
  Daf.SQLCute.Compiler.MySql,
  Daf.SQLCute.Compiler.SQLite,
  Daf.SQLCute.Compiler.Oracle,
  Daf.SQLCute.Compiler.Firebird,
  SQLCute.SpecHelpers;

type
  TSQLCuteCompilersSteps = class
  public

    // -----------------------------------------------------------------------
    //  GIVEN — query construction
    // -----------------------------------------------------------------------

    [Given('a dialect query selecting "([\w, ]+)" from "(\w+)" where "(\w+)" equals (\d+)')]
    procedure GivenSelectWhereEq(W: TSQLCuteWorld; Cols, Table, Col: string; Val: Integer);

    [Given('a dialect query from "(\w+)" where "(\w+)" equals (\d+) and "(\w+)" equals (\d+)')]
    procedure GivenWhereTwoEq(W: TSQLCuteWorld; Table, Col1: string; Val1: Integer; Col2: string; Val2: Integer);

    [Given('a dialect query from "(\w+)" limited to (\d+)$')]
    procedure GivenFromLimit(W: TSQLCuteWorld; Table: string; Lim: Integer);

    [Given('a dialect query from "(\w+)" limited to (\d+) offset (\d+)')]
    procedure GivenFromLimitOffset(W: TSQLCuteWorld; Table: string; Lim, Off: Integer);

    [Given('a dialect query from "(\w+)" ordered by "(\w+)" limited to (\d+) offset (\d+)')]
    procedure GivenFromOrderByLimitOffset(W: TSQLCuteWorld; Table, Col: string; Lim, Off: Integer);

    [Given('a distinct dialect query selecting "(\w+)" from "(\w+)" limited to (\d+) offset (\d+)')]
    procedure GivenDistinctLimitOffset(W: TSQLCuteWorld; Col, Table: string; Lim, Off: Integer);

    [Given('a dialect query from "(\w+)" ordered by "(\w+)"$')]
    procedure GivenFromOrderBy(W: TSQLCuteWorld; Table, Col: string);

    [Given('a dialect query from "(\w+)" ordered by "(\w+)" nulls (first|last)')]
    procedure GivenFromOrderByNulls(W: TSQLCuteWorld; Table, Col, Placement: string);

    [Given('a dialect query from "(\w+)" ordered by "(\w+)" desc nulls (first|last)')]
    procedure GivenFromOrderByDescNulls(W: TSQLCuteWorld; Table, Col, Placement: string);

    [Given('a date query from "(\w+)" wheredate "(\w+)" equals "([^"]+)"')]
    procedure GivenWhereDateEq(W: TSQLCuteWorld; Table, Col, Val: string);

    [Given('a date query from "(\w+)" wheretime "(\w+)" "([^"]+)" "([^"]+)"')]
    procedure GivenWhereTime(W: TSQLCuteWorld; Table, Col, Op, Val: string);

    [Given('a date query from "(\w+)" wheredatepart "(\w+)" "(\w+)" equals (\d+)')]
    procedure GivenWhereDatePart(W: TSQLCuteWorld; Table, PartName, Col: string; Val: Integer);

    [Given('a date query from "(\w+)" wheredate "(\w+)" equals "([^"]+)" orwheredate "(\w+)" equals "([^"]+)"')]
    procedure GivenWhereDateOrWhereDate(W: TSQLCuteWorld; Table, Col1, Val1, Col2, Val2: string);

    // -----------------------------------------------------------------------
    //  WHEN — compile with a specific dialect
    // -----------------------------------------------------------------------

    [When('compiled with the "(\w+)" dialect')]
    procedure WhenCompiledWithDialect(W: TSQLCuteWorld; Dialect: string);

    [When('compiled with the "(\w+)" dialect in native style')]
    procedure WhenCompiledWithDialectNative(W: TSQLCuteWorld; Dialect: string);

    // -----------------------------------------------------------------------
    //  THEN — assertions
    // -----------------------------------------------------------------------

    [ThenAttribute('SQL is "(.*)"')]
    procedure ThenSqlIs(W: TSQLCuteWorld; Expected: string);

    [ThenAttribute('SQL contains "(.*)"')]
    procedure ThenSqlContains(W: TSQLCuteWorld; Expected: string);

    [ThenAttribute('SQL does not contain "(.*)"')]
    procedure ThenSqlDoesNotContain(W: TSQLCuteWorld; Unexpected: string);

    [ThenAttribute('has (\d+) bindings?')]
    procedure ThenBindingCount(W: TSQLCuteWorld; Count: Integer);

  end;

{ TSQLCuteCompilersSteps }

// -----------------------------------------------------------------------
//  GIVEN implementations
// -----------------------------------------------------------------------

procedure TSQLCuteCompilersSteps.GivenSelectWhereEq(W: TSQLCuteWorld;
  Cols, Table, Col: string; Val: Integer);
var
  Parts: TArray<string>;
  Trimmed: TArray<string>;
  I: Integer;
begin
  Parts := Cols.Split([',']);
  SetLength(Trimmed, Length(Parts));
  for I := 0 to High(Parts) do
    Trimmed[I] := Trim(Parts[I]);
  W.Query := TQuery.New
    .Select(Trimmed)
    .From(Table)
    .Where(Col, Val);
end;

procedure TSQLCuteCompilersSteps.GivenWhereTwoEq(W: TSQLCuteWorld;
  Table, Col1: string; Val1: Integer; Col2: string; Val2: Integer);
begin
  W.Query := TQuery.New.From(Table).Where(Col1, Val1).Where(Col2, Val2);
end;

procedure TSQLCuteCompilersSteps.GivenFromLimit(W: TSQLCuteWorld;
  Table: string; Lim: Integer);
begin
  W.Query := TQuery.New.From(Table).Limit(Lim);
end;

procedure TSQLCuteCompilersSteps.GivenFromLimitOffset(W: TSQLCuteWorld;
  Table: string; Lim, Off: Integer);
begin
  W.Query := TQuery.New.From(Table).Limit(Lim).Offset(Off);
end;

procedure TSQLCuteCompilersSteps.GivenFromOrderByLimitOffset(W: TSQLCuteWorld;
  Table, Col: string; Lim, Off: Integer);
begin
  W.Query := TQuery.New.From(Table).OrderBy(Col).Limit(Lim).Offset(Off);
end;

procedure TSQLCuteCompilersSteps.GivenDistinctLimitOffset(W: TSQLCuteWorld;
  Col, Table: string; Lim, Off: Integer);
begin
  W.Query := TQuery.New.Select([Col]).Distinct.From(Table).Limit(Lim).Offset(Off);
end;

procedure TSQLCuteCompilersSteps.GivenFromOrderBy(W: TSQLCuteWorld;
  Table, Col: string);
begin
  W.Query := TQuery.New.From(Table).OrderBy(Col);
end;

procedure TSQLCuteCompilersSteps.GivenFromOrderByNulls(W: TSQLCuteWorld;
  Table, Col, Placement: string);
begin
  W.Query := TQuery.New.From(Table).OrderBy(Col);
  if SameText(Placement, 'first') then
    W.Query.NullsFirst
  else
    W.Query.NullsLast;
end;

procedure TSQLCuteCompilersSteps.GivenFromOrderByDescNulls(W: TSQLCuteWorld;
  Table, Col, Placement: string);
begin
  W.Query := TQuery.New.From(Table).OrderByDesc(Col);
  if SameText(Placement, 'first') then
    W.Query.NullsFirst
  else
    W.Query.NullsLast;
end;

// -----------------------------------------------------------------------
//  WHEN implementations
// -----------------------------------------------------------------------

function NewCompiler(const Dialect: string): TAnsiSqlCompiler;
begin
  if Dialect = 'ANSI' then
    Result := TAnsiSqlCompiler.Create
  else if Dialect = 'SqlServer' then
    Result := TSqlServerCompiler.Create
  else if Dialect = 'Postgres' then
    Result := TPostgresCompiler.Create
  else if Dialect = 'MySQL' then
    Result := TMySqlCompiler.Create
  else if Dialect = 'SQLite' then
    Result := TSQLiteCompiler.Create
  else if Dialect = 'Oracle' then
    Result := TOracleCompiler.Create
  else if Dialect = 'Firebird' then
    Result := TFirebirdCompiler.Create
  else
    raise Exception.CreateFmt('Unknown dialect: %s', [Dialect]);
end;

procedure TSQLCuteCompilersSteps.WhenCompiledWithDialect(W: TSQLCuteWorld;
  Dialect: string);
begin
  W.Compiler := NewCompiler(Dialect);
  W.LastResult := W.Query.Compile(W.Compiler);
end;

procedure TSQLCuteCompilersSteps.WhenCompiledWithDialectNative(W: TSQLCuteWorld;
  Dialect: string);
var
  Compiler: TAnsiSqlCompiler;
begin
  Compiler := NewCompiler(Dialect);
  Compiler.Placeholders := TPlaceholderStyle.Native;
  W.Compiler := Compiler;
  W.LastResult := W.Query.Compile(W.Compiler);
end;

// -----------------------------------------------------------------------
//  THEN implementations
// -----------------------------------------------------------------------

procedure TSQLCuteCompilersSteps.ThenSqlIs(W: TSQLCuteWorld; Expected: string);
begin
  Expect(W.LastResult.SQL).ToEqual(Expected);
end;

procedure TSQLCuteCompilersSteps.ThenSqlContains(W: TSQLCuteWorld; Expected: string);
begin
  Expect(W.LastResult.SQL).ToContain(Expected);
end;

procedure TSQLCuteCompilersSteps.ThenSqlDoesNotContain(W: TSQLCuteWorld;
  Unexpected: string);
begin
  Expect(W.LastResult.SQL.Contains(Unexpected)).ToBeFalse;
end;

procedure TSQLCuteCompilersSteps.ThenBindingCount(W: TSQLCuteWorld; Count: Integer);
begin
  Expect(Length(W.LastResult.Bindings)).ToEqual(Count);
end;

// -----------------------------------------------------------------------
//  F7 — date/time step implementations
// -----------------------------------------------------------------------

procedure TSQLCuteCompilersSteps.GivenWhereDateEq(W: TSQLCuteWorld;
  Table, Col, Val: string);
begin
  W.Query := TQuery.New.From(Table).WhereDate(Col, Val);
end;

procedure TSQLCuteCompilersSteps.GivenWhereTime(W: TSQLCuteWorld;
  Table, Col, Op, Val: string);
begin
  W.Query := TQuery.New.From(Table).WhereTime(Col, Op, Val);
end;

procedure TSQLCuteCompilersSteps.GivenWhereDatePart(W: TSQLCuteWorld;
  Table, PartName, Col: string; Val: Integer);
var
  Part: TDatePart;
begin
  if PartName = 'dpYear' then
    Part := TDatePart.dpYear
  else if PartName = 'dpMonth' then
    Part := TDatePart.dpMonth
  else if PartName = 'dpDay' then
    Part := TDatePart.dpDay
  else if PartName = 'dpHour' then
    Part := TDatePart.dpHour
  else if PartName = 'dpMinute' then
    Part := TDatePart.dpMinute
  else
    Part := TDatePart.dpDate;
  W.Query := TQuery.New.From(Table).WhereDatePart(Part, Col, Val);
end;

procedure TSQLCuteCompilersSteps.GivenWhereDateOrWhereDate(W: TSQLCuteWorld;
  Table, Col1, Val1, Col2, Val2: string);
begin
  W.Query := TQuery.New.From(Table).WhereDate(Col1, Val1).OrWhereDate(Col2, Val2);
end;

initialization
  Bindings.RegisterSteps<TSQLCuteCompilersSteps>;

end.
