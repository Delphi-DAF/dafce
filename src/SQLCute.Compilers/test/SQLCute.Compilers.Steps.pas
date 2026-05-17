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

    [Given('a dialect query from "(\w+)" limited to (\d+)$')]
    procedure GivenFromLimit(W: TSQLCuteWorld; Table: string; Lim: Integer);

    [Given('a dialect query from "(\w+)" limited to (\d+) offset (\d+)')]
    procedure GivenFromLimitOffset(W: TSQLCuteWorld; Table: string; Lim, Off: Integer);

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

    // -----------------------------------------------------------------------
    //  THEN — assertions
    // -----------------------------------------------------------------------

    [ThenAttribute('SQL is "(.*)"')]
    procedure ThenSqlIs(W: TSQLCuteWorld; Expected: string);

    [ThenAttribute('SQL contains "(.*)"')]
    procedure ThenSqlContains(W: TSQLCuteWorld; Expected: string);

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

// -----------------------------------------------------------------------
//  WHEN implementations
// -----------------------------------------------------------------------

procedure TSQLCuteCompilersSteps.WhenCompiledWithDialect(W: TSQLCuteWorld;
  Dialect: string);
begin
  if Dialect = 'ANSI' then
    W.Compiler := TAnsiSqlCompiler.Create
  else if Dialect = 'SqlServer' then
    W.Compiler := TSqlServerCompiler.Create
  else if Dialect = 'Postgres' then
    W.Compiler := TPostgresCompiler.Create
  else if Dialect = 'MySQL' then
    W.Compiler := TMySqlCompiler.Create
  else if Dialect = 'SQLite' then
    W.Compiler := TSQLiteCompiler.Create
  else if Dialect = 'Oracle' then
    W.Compiler := TOracleCompiler.Create
  else if Dialect = 'Firebird' then
    W.Compiler := TFirebirdCompiler.Create
  else
    raise Exception.CreateFmt('Unknown dialect: %s', [Dialect]);
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
