unit SQLCute.Steps;

{
  Step bindings for all SQLCute specs (Phase 1 + Phase 2).
  Handles: SELECT.Feat, Where.Feat and Join.Feat world construction + assertions.
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
    //  GIVEN — Phase 2 (JOIN, DISTINCT, GROUP BY, UNION, CTE, subquery)
    // -----------------------------------------------------------------------

    [Given('a query from "(\w+)" inner joined to "(\w+)" on "(.+)"')]
    procedure GivenInnerJoin(W: TSQLCuteWorld; Table, JoinTable, Cond: string);

    [Given('a query from "(\w+)" joining "(\w+)" on columns "([\w.]+)" and "([\w.]+)"')]
    procedure GivenInnerJoinCols(W: TSQLCuteWorld; Table, JoinTable, Col1, Col2: string);

    [Given('a query from "(\w+)" left joined to "(\w+)" on "(.+)"')]
    procedure GivenLeftJoin(W: TSQLCuteWorld; Table, JoinTable, Cond: string);

    [Given('the Phase-2 two-join query')]
    procedure GivenPhase2TwoJoin(W: TSQLCuteWorld);

    [Given('a distinct query from "(\w+)"')]
    procedure GivenDistinctFrom(W: TSQLCuteWorld; Table: string);

    [Given('a query from "(\w+)" grouped by "(\w+)"')]
    procedure GivenGroupBy(W: TSQLCuteWorld; Table, Col: string);

    [Given('the Phase-2 group-having query')]
    procedure GivenPhase2GroupHaving(W: TSQLCuteWorld);

    [Given('a count query from "(\w+)"')]
    procedure GivenCountFrom(W: TSQLCuteWorld; Table: string);

    [Given('the Phase-2 union query')]
    procedure GivenPhase2Union(W: TSQLCuteWorld);

    [Given('the Phase-2 CTE query')]
    procedure GivenPhase2CTE(W: TSQLCuteWorld);

    [Given('the Phase-2 subquery-from query')]
    procedure GivenPhase2SubqueryFrom(W: TSQLCuteWorld);

    [Given('the Phase-2 where-exists query')]
    procedure GivenPhase2WhereExists(W: TSQLCuteWorld);

    [Given('the Phase-2 acceptance query')]
    procedure GivenPhase2Acceptance(W: TSQLCuteWorld);

    // -----------------------------------------------------------------------
    //  GIVEN — Phase 3 (INSERT, UPDATE, DELETE)
    // -----------------------------------------------------------------------

    [Given('the Phase-3 insert-single query')]
    procedure GivenPhase3InsertSingle(W: TSQLCuteWorld);

    [Given('the Phase-3 insert-multi query')]
    procedure GivenPhase3InsertMulti(W: TSQLCuteWorld);

    [Given('the Phase-3 insert-select query')]
    procedure GivenPhase3InsertSelect(W: TSQLCuteWorld);

    [Given('the Phase-3 update-single query')]
    procedure GivenPhase3UpdateSingle(W: TSQLCuteWorld);

    [Given('the Phase-3 update-multi query')]
    procedure GivenPhase3UpdateMulti(W: TSQLCuteWorld);

    [Given('a query deleting from "(\w+)" where "(\w+)" = (\d+)')]
    procedure GivenDeleteFromWhere(W: TSQLCuteWorld; Table, Col: string; Val: Integer);

    [Given('a query deleting all from "(\w+)"')]
    procedure GivenDeleteAll(W: TSQLCuteWorld; Table: string);

    [Given('the Phase-3 acceptance query')]
    procedure GivenPhase3Acceptance(W: TSQLCuteWorld);

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
//  GIVEN Phase-2 implementations
// -----------------------------------------------------------------------

procedure TSQLCuteSteps.GivenInnerJoin(W: TSQLCuteWorld; Table, JoinTable, Cond: string);
begin
  W.Query := TQuery.New.From(Table).Join(JoinTable, Cond);
end;

procedure TSQLCuteSteps.GivenInnerJoinCols(W: TSQLCuteWorld; Table, JoinTable, Col1, Col2: string);
begin
  W.Query := TQuery.New.From(Table).Join(JoinTable, Col1, Col2);
end;

procedure TSQLCuteSteps.GivenLeftJoin(W: TSQLCuteWorld; Table, JoinTable, Cond: string);
begin
  W.Query := TQuery.New.From(Table).LeftJoin(JoinTable, Cond);
end;

procedure TSQLCuteSteps.GivenPhase2TwoJoin(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('orders')
    .Join('users',    'orders.user_id',    'users.id')
    .LeftJoin('products', 'orders.product_id', 'products.id');
end;

procedure TSQLCuteSteps.GivenDistinctFrom(W: TSQLCuteWorld; Table: string);
begin
  W.Query := TQuery.New.Distinct.From(Table);
end;

procedure TSQLCuteSteps.GivenGroupBy(W: TSQLCuteWorld; Table, Col: string);
begin
  W.Query := TQuery.New.From(Table).GroupBy(Col);
end;

procedure TSQLCuteSteps.GivenPhase2GroupHaving(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('orders')
    .GroupBy('user_id')
    .Having('COUNT(*)', '>', 3);
end;

procedure TSQLCuteSteps.GivenCountFrom(W: TSQLCuteWorld; Table: string);
begin
  W.Query := TQuery.New.From(Table).SelectCount;
end;

procedure TSQLCuteSteps.GivenPhase2Union(W: TSQLCuteWorld);
var
  Other: IQuery;
begin
  Other := TQuery.New.From('archived_users').Select('id');
  W.Query := TQuery.New.From('active_users').Select('id').Union(Other);
end;

procedure TSQLCuteSteps.GivenPhase2CTE(W: TSQLCuteWorld);
var
  CTEQuery: IQuery;
begin
  CTEQuery := TQuery.New.From('orders').Where('created', '>', '2024-01-01');
  W.Query := TQuery.New.&With('recent', CTEQuery).From('recent');
end;

procedure TSQLCuteSteps.GivenPhase2SubqueryFrom(W: TSQLCuteWorld);
var
  Sub: IQuery;
begin
  Sub := TQuery.New.From('users').Select(['id', 'name']);
  W.Query := TQuery.New.From(Sub, 'u');
end;

procedure TSQLCuteSteps.GivenPhase2WhereExists(W: TSQLCuteWorld);
var
  Sub: IQuery;
begin
  Sub := TQuery.New.From('orders').Where('user_id', 42);
  W.Query := TQuery.New.From('users').WhereExists(Sub);
end;

procedure TSQLCuteSteps.GivenPhase2Acceptance(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('orders')
    .Join('users', 'orders.user_id', 'users.id')
    .Select(['user_id'])
    .SelectCount('*', 'total')
    .Where('status', 'active')
    .GroupBy('user_id')
    .Having('COUNT(*)', '>', 5)
    .OrderByDesc('total')
    .Limit(10);
end;

// -----------------------------------------------------------------------
//  GIVEN Phase-3 implementations
// -----------------------------------------------------------------------

procedure TSQLCuteSteps.GivenPhase3InsertSingle(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('users')
    .AsInsert(['name', 'email'], ['John Doe', 'john@example.com']);
end;

procedure TSQLCuteSteps.GivenPhase3InsertMulti(W: TSQLCuteWorld);
var
  R1, R2: TArray<Variant>;
  Rows: TArray<TArray<Variant>>;
begin
  R1   := [1, 'first entry'];
  R2   := [2, 'second entry'];
  Rows := [R1, R2];
  W.Query := TQuery.New
    .From('logs')
    .AsInsertRows(['level', 'msg'], Rows);
end;

procedure TSQLCuteSteps.GivenPhase3InsertSelect(W: TSQLCuteWorld);
var
  Sub: IQuery;
begin
  Sub := TQuery.New.From('users').Select(['id', 'name']).Where('active', True);
  W.Query := TQuery.New
    .From('archive')
    .AsInsertFrom(['id', 'name'], Sub);
end;

procedure TSQLCuteSteps.GivenPhase3UpdateSingle(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('users')
    .Where('id', 1)
    .AsUpdate(['status'], ['inactive']);
end;

procedure TSQLCuteSteps.GivenPhase3UpdateMulti(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('users')
    .Where('id', 1)
    .AsUpdate(['name', 'email'], ['Jane', 'jane@example.com']);
end;

procedure TSQLCuteSteps.GivenDeleteFromWhere(W: TSQLCuteWorld;
  Table, Col: string; Val: Integer);
begin
  W.Query := TQuery.New.From(Table).Where(Col, Val).AsDelete;
end;

procedure TSQLCuteSteps.GivenDeleteAll(W: TSQLCuteWorld; Table: string);
begin
  W.Query := TQuery.New.From(Table).AsDelete;
end;

procedure TSQLCuteSteps.GivenPhase3Acceptance(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('orders')
    .Where('user_id', 5)
    .Where('status', 'pending')
    .AsUpdate(['status', 'updated_at'], ['shipped', '2024-01-15']);
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
