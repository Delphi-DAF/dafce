unit SQLCute.Steps.Core;

{
  Basic query construction, WHEN and THEN steps
}

interface

implementation

uses
  System.SysUtils,
  System.Variants,
  Daf.MiniSpec,
  Daf.MiniSpec.Binding,
  Daf.SQLCute,
  SQLCute.SpecHelpers;

type
  TSQLCuteStepsCore = class
  public
    [Given('an empty query')]
    procedure GivenEmpty(W: TSQLCuteWorld);

    [Given('a query from "(\w+)"')]
    procedure GivenFrom(W: TSQLCuteWorld; Table: string);

    [Given('a query selecting "([\w, ]+)" from "(\w+)"')]
    procedure GivenSelectFrom(W: TSQLCuteWorld; Cols, Table: string);

    [Given('a query from "(\w+)" with limit (\d+)$')]
    procedure GivenFromLimit(W: TSQLCuteWorld; Table: string; Lim: Integer);

    [Given('a query from "(\w+)" with limit (\d+) offset (\d+)')]
    procedure GivenFromLimitOffset(W: TSQLCuteWorld; Table: string; Lim, Off: Integer);

    [Given('a query from "(\w+)" ordered by "(\w+)"$')]
    procedure GivenFromOrderAsc(W: TSQLCuteWorld; Table, Col: string);

    [Given('a query from "(\w+)" ordered by "(\w+)" desc')]
    procedure GivenFromOrderDesc(W: TSQLCuteWorld; Table, Col: string);

    [Given('a query from "(\w+)" where "(\w+)" = (.+)')]
    procedure GivenFromWhereEq(W: TSQLCuteWorld; Table, Col, Val: string);

    [Given('a query from "(\w+)" where "(\w+)" > (.+)')]
    procedure GivenFromWhereGt(W: TSQLCuteWorld; Table, Col, Val: string);

    [Given('a query from "(\w+)" where "(\w+)" <= (.+)')]
    procedure GivenFromWhereLe(W: TSQLCuteWorld; Table, Col, Val: string);

    [Given('a query from "(\w+)" where "(\w+)" LIKE (.+)')]
    procedure GivenFromWhereLike(W: TSQLCuteWorld; Table, Col, Val: string);

    [Given('a query from "(\w+)" where "(\w+)" is null$')]
    procedure GivenFromWhereNull(W: TSQLCuteWorld; Table, Col: string);

    [Given('a query from "(\w+)" where "(\w+)" is not null')]
    procedure GivenFromWhereNotNull(W: TSQLCuteWorld; Table, Col: string);

    [Given('a query from "(\w+)" with two AND conditions')]
    procedure GivenFromWhereTwoAnd(W: TSQLCuteWorld; Table: string);

    [Given('a query from "(\w+)" with two OR conditions')]
    procedure GivenFromWhereTwoOr(W: TSQLCuteWorld; Table: string);

    [Given('a query from "(\w+)" where "(\w+)" between (\d+) and (\d+)')]
    procedure GivenFromWhereBetween(W: TSQLCuteWorld;
      Table, Col: string; Low, High: Integer);

    [Given('a query from "(\w+)" with raw where "(.+)"')]
    procedure GivenFromWhereRaw(W: TSQLCuteWorld; Table, RawSql: string);

    [Given('a combined SELECT WHERE LIMIT query')]
    procedure GivenCombinedSelectWhereLimitQuery(W: TSQLCuteWorld);

    [When('I compile with ANSI')]
    procedure WhenCompile(W: TSQLCuteWorld);

    [When('I clone the query and compile the clone')]
    procedure WhenCompileClone(W: TSQLCuteWorld);

    [ThenAttribute('SQL is "(.*)"')]
    procedure ThenSqlIs(W: TSQLCuteWorld; Expected: string);

    [ThenAttribute('has (\d+) bindings?')]
    procedure ThenBindingCount(W: TSQLCuteWorld; Expected: Integer);

  end;

// -----------------------------------------------------------------------
//  TSQLCuteStepsCore helpers
// -----------------------------------------------------------------------

function StringToVariant(const S: string): Variant;
begin
  if SameText(S, 'True') then  Exit(True);
  if SameText(S, 'False') then Exit(False);
  var IntVal: Integer;
  if TryStrToInt(S, IntVal) then Exit(IntVal);
  Result := S;
end;

// -----------------------------------------------------------------------
//  TSQLCuteStepsCore implementations
// -----------------------------------------------------------------------

procedure TSQLCuteStepsCore.GivenEmpty(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New;
end;

procedure TSQLCuteStepsCore.GivenFrom(W: TSQLCuteWorld; Table: string);
begin
  W.Query := TQuery.New.From(Table);
end;

procedure TSQLCuteStepsCore.GivenSelectFrom(W: TSQLCuteWorld; Cols, Table: string);
var
  ColArr: TArray<string>;
  Trimmed: TArray<string>;
  I: Integer;
begin
  ColArr := Cols.Split([',']);
  SetLength(Trimmed, Length(ColArr));
  for I := 0 to High(ColArr) do
    Trimmed[I] := ColArr[I].Trim;
  W.Query := TQuery.New.Select(Trimmed).From(Table);
end;

procedure TSQLCuteStepsCore.GivenFromLimit(W: TSQLCuteWorld; Table: string; Lim: Integer);
begin
  W.Query := TQuery.New.From(Table).Limit(Lim);
end;

procedure TSQLCuteStepsCore.GivenFromLimitOffset(W: TSQLCuteWorld; Table: string; Lim, Off: Integer);
begin
  W.Query := TQuery.New.From(Table).Limit(Lim).Offset(Off);
end;

procedure TSQLCuteStepsCore.GivenFromOrderAsc(W: TSQLCuteWorld; Table, Col: string);
begin
  W.Query := TQuery.New.From(Table).OrderBy(Col);
end;

procedure TSQLCuteStepsCore.GivenFromOrderDesc(W: TSQLCuteWorld; Table, Col: string);
begin
  W.Query := TQuery.New.From(Table).OrderByDesc(Col);
end;

procedure TSQLCuteStepsCore.GivenFromWhereEq(W: TSQLCuteWorld; Table, Col, Val: string);
begin
  W.Query := TQuery.New.From(Table).Where(Col, StringToVariant(Val));
end;

procedure TSQLCuteStepsCore.GivenFromWhereGt(W: TSQLCuteWorld; Table, Col, Val: string);
begin
  W.Query := TQuery.New.From(Table).Where(Col, '>', StringToVariant(Val));
end;

procedure TSQLCuteStepsCore.GivenFromWhereLe(W: TSQLCuteWorld; Table, Col, Val: string);
begin
  W.Query := TQuery.New.From(Table).Where(Col, '<=', StringToVariant(Val));
end;

procedure TSQLCuteStepsCore.GivenFromWhereLike(W: TSQLCuteWorld; Table, Col, Val: string);
begin
  W.Query := TQuery.New.From(Table).Where(Col, 'LIKE', Val);
end;

procedure TSQLCuteStepsCore.GivenFromWhereNull(W: TSQLCuteWorld; Table, Col: string);
begin
  W.Query := TQuery.New.From(Table).WhereNull(Col);
end;

procedure TSQLCuteStepsCore.GivenFromWhereNotNull(W: TSQLCuteWorld; Table, Col: string);
begin
  W.Query := TQuery.New.From(Table).WhereNotNull(Col);
end;

procedure TSQLCuteStepsCore.GivenFromWhereTwoAnd(W: TSQLCuteWorld; Table: string);
begin
  W.Query := TQuery.New
    .From(Table)
    .Where('active', True)
    .Where('age', '>', 18);
end;

procedure TSQLCuteStepsCore.GivenFromWhereTwoOr(W: TSQLCuteWorld; Table: string);
begin
  // Table is "roles" — hardcoded to match the Where scenario exactly
  W.Query := TQuery.New
    .From(Table)
    .Where('code', 'admin')
    .OrWhere('code', 'superadmin');
end;

procedure TSQLCuteStepsCore.GivenFromWhereBetween(W: TSQLCuteWorld;
  Table, Col: string; Low, High: Integer);
begin
  W.Query := TQuery.New.From(Table).WhereBetween(Col, Low, High);
end;

procedure TSQLCuteStepsCore.GivenFromWhereRaw(W: TSQLCuteWorld; Table, RawSql: string);
begin
  W.Query := TQuery.New.From(Table).WhereRaw(RawSql);
end;

procedure TSQLCuteStepsCore.GivenCombinedSelectWhereLimitQuery(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('users')
    .Select(['id', 'name'])
    .Where('active', True)
    .Limit(10);
end;

procedure TSQLCuteStepsCore.WhenCompile(W: TSQLCuteWorld);
begin
  W.LastResult := W.Query.Compile(W.Compiler);
end;

procedure TSQLCuteStepsCore.WhenCompileClone(W: TSQLCuteWorld);
begin
  W.LastResult := W.Query.Clone.Compile(W.Compiler);
end;

procedure TSQLCuteStepsCore.ThenSqlIs(W: TSQLCuteWorld; Expected: string);
begin
  Expect(W.LastResult.SQL).ToEqual(Expected);
end;

procedure TSQLCuteStepsCore.ThenBindingCount(W: TSQLCuteWorld; Expected: Integer);
begin
  Expect(Length(W.LastResult.Bindings)).ToEqual(Expected);
end;


initialization
  Bindings.RegisterSteps<TSQLCuteStepsCore>;

end.
