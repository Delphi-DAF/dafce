unit SQLCute.Steps;

{
  Step bindings for all SQLCute Phase-1 specs.
  Handles: SELECT.Feat and Where.Feat world construction + assertions.
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
  TSQLCuteSteps = class
  public

    // -----------------------------------------------------------------------
    //  GIVEN — query construction
    // -----------------------------------------------------------------------

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

    // --- WHERE given steps ---

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

    [Given('the Phase-1 acceptance query')]
    procedure GivenAcceptance(W: TSQLCuteWorld);

    // -----------------------------------------------------------------------
    //  WHEN
    // -----------------------------------------------------------------------

    [When('I compile with ANSI')]
    procedure WhenCompile(W: TSQLCuteWorld);

    [When('I clone the query and compile the clone')]
    procedure WhenCompileClone(W: TSQLCuteWorld);

    // -----------------------------------------------------------------------
    //  THEN
    // -----------------------------------------------------------------------

    [ThenAttribute('SQL is "(.*)"')]
    procedure ThenSqlIs(W: TSQLCuteWorld; Expected: string);

    [ThenAttribute('has (\d+) bindings?')]
    procedure ThenBindingCount(W: TSQLCuteWorld; Expected: Integer);

  end;

{ TSQLCuteSteps }

// -----------------------------------------------------------------------
//  GIVEN implementations
// -----------------------------------------------------------------------

procedure TSQLCuteSteps.GivenEmpty(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New;
end;

procedure TSQLCuteSteps.GivenFrom(W: TSQLCuteWorld; Table: string);
begin
  W.Query := TQuery.New.From(Table);
end;

procedure TSQLCuteSteps.GivenSelectFrom(W: TSQLCuteWorld; Cols, Table: string);
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

procedure TSQLCuteSteps.GivenFromLimit(W: TSQLCuteWorld; Table: string; Lim: Integer);
begin
  W.Query := TQuery.New.From(Table).Limit(Lim);
end;

procedure TSQLCuteSteps.GivenFromLimitOffset(W: TSQLCuteWorld; Table: string; Lim, Off: Integer);
begin
  W.Query := TQuery.New.From(Table).Limit(Lim).Offset(Off);
end;

procedure TSQLCuteSteps.GivenFromOrderAsc(W: TSQLCuteWorld; Table, Col: string);
begin
  W.Query := TQuery.New.From(Table).OrderBy(Col);
end;

procedure TSQLCuteSteps.GivenFromOrderDesc(W: TSQLCuteWorld; Table, Col: string);
begin
  W.Query := TQuery.New.From(Table).OrderByDesc(Col);
end;

// --- WHERE step implementations ---

function StringToVariant(const S: string): Variant;
begin
  if SameText(S, 'True') then  Exit(True);
  if SameText(S, 'False') then Exit(False);
  var IntVal: Integer;
  if TryStrToInt(S, IntVal) then Exit(IntVal);
  Result := S;
end;

procedure TSQLCuteSteps.GivenFromWhereEq(W: TSQLCuteWorld; Table, Col, Val: string);
begin
  W.Query := TQuery.New.From(Table).Where(Col, StringToVariant(Val));
end;

procedure TSQLCuteSteps.GivenFromWhereGt(W: TSQLCuteWorld; Table, Col, Val: string);
begin
  W.Query := TQuery.New.From(Table).Where(Col, '>', StringToVariant(Val));
end;

procedure TSQLCuteSteps.GivenFromWhereLe(W: TSQLCuteWorld; Table, Col, Val: string);
begin
  W.Query := TQuery.New.From(Table).Where(Col, '<=', StringToVariant(Val));
end;

procedure TSQLCuteSteps.GivenFromWhereLike(W: TSQLCuteWorld; Table, Col, Val: string);
begin
  W.Query := TQuery.New.From(Table).Where(Col, 'LIKE', Val);
end;

procedure TSQLCuteSteps.GivenFromWhereNull(W: TSQLCuteWorld; Table, Col: string);
begin
  W.Query := TQuery.New.From(Table).WhereNull(Col);
end;

procedure TSQLCuteSteps.GivenFromWhereNotNull(W: TSQLCuteWorld; Table, Col: string);
begin
  W.Query := TQuery.New.From(Table).WhereNotNull(Col);
end;

procedure TSQLCuteSteps.GivenFromWhereTwoAnd(W: TSQLCuteWorld; Table: string);
begin
  W.Query := TQuery.New
    .From(Table)
    .Where('active', True)
    .Where('age', '>', 18);
end;

procedure TSQLCuteSteps.GivenFromWhereTwoOr(W: TSQLCuteWorld; Table: string);
begin
  // Table is "roles" — hardcoded to match the Where scenario exactly
  W.Query := TQuery.New
    .From(Table)
    .Where('code', 'admin')
    .OrWhere('code', 'superadmin');
end;

procedure TSQLCuteSteps.GivenFromWhereBetween(W: TSQLCuteWorld;
  Table, Col: string; Low, High: Integer);
begin
  W.Query := TQuery.New.From(Table).WhereBetween(Col, Low, High);
end;

procedure TSQLCuteSteps.GivenFromWhereRaw(W: TSQLCuteWorld; Table, RawSql: string);
begin
  W.Query := TQuery.New.From(Table).WhereRaw(RawSql);
end;

procedure TSQLCuteSteps.GivenAcceptance(W: TSQLCuteWorld);
begin
  // Acceptance criterion: SELECT id, name FROM users WHERE active = ? LIMIT 10
  W.Query := TQuery.New
    .From('users')
    .Select(['id', 'name'])
    .Where('active', True)
    .Limit(10);
end;

// -----------------------------------------------------------------------
//  WHEN implementations
// -----------------------------------------------------------------------

procedure TSQLCuteSteps.WhenCompile(W: TSQLCuteWorld);
begin
  W.LastResult := W.Query.Compile(W.Compiler);
end;

procedure TSQLCuteSteps.WhenCompileClone(W: TSQLCuteWorld);
begin
  W.LastResult := W.Query.Clone.Compile(W.Compiler);
end;

// -----------------------------------------------------------------------
//  THEN implementations
// -----------------------------------------------------------------------

procedure TSQLCuteSteps.ThenSqlIs(W: TSQLCuteWorld; Expected: string);
begin
  Expect(W.LastResult.SQL).ToEqual(Expected);
end;

procedure TSQLCuteSteps.ThenBindingCount(W: TSQLCuteWorld; Expected: Integer);
begin
  Expect(Length(W.LastResult.Bindings)).ToEqual(Expected);
end;

initialization
  Bindings.RegisterSteps<TSQLCuteSteps>;

end.
