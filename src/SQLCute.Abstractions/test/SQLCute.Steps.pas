unit SQLCute.Steps;

{
  Step bindings for all SQLCute unit specs.
  Handles world construction and compilation for all SQLCute feature files.
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

    [Given('a combined SELECT WHERE LIMIT query')]
    procedure GivenCombinedSelectWhereLimitQuery(W: TSQLCuteWorld);

    // -----------------------------------------------------------------------
    //  GIVEN — JOINs, GROUP BY, set operations, CTEs, subqueries
    // -----------------------------------------------------------------------

    [Given('a query from "(\w+)" inner joined to "(\w+)" on "(.+)"')]
    procedure GivenInnerJoin(W: TSQLCuteWorld; Table, JoinTable, Cond: string);

    [Given('a query from "(\w+)" joining "(\w+)" on columns "([\w.]+)" and "([\w.]+)"')]
    procedure GivenInnerJoinCols(W: TSQLCuteWorld; Table, JoinTable, Col1, Col2: string);

    [Given('a query from "(\w+)" left joined to "(\w+)" on "(.+)"')]
    procedure GivenLeftJoin(W: TSQLCuteWorld; Table, JoinTable, Cond: string);

    [Given('a query with two chained JOINs')]
    procedure GivenTwoChainedJoins(W: TSQLCuteWorld);

    [Given('a distinct query from "(\w+)"')]
    procedure GivenDistinctFrom(W: TSQLCuteWorld; Table: string);

    [Given('a query from "(\w+)" grouped by "(\w+)"')]
    procedure GivenGroupBy(W: TSQLCuteWorld; Table, Col: string);

    [Given('a GROUP BY and HAVING query')]
    procedure GivenGroupByAndHaving(W: TSQLCuteWorld);

    [Given('a count query from "(\w+)"')]
    procedure GivenCountFrom(W: TSQLCuteWorld; Table: string);

    [Given('a UNION query')]
    procedure GivenUnionQuery(W: TSQLCuteWorld);

    [Given('a CTE query')]
    procedure GivenCTEQuery(W: TSQLCuteWorld);

    [Given('a FROM subquery query')]
    procedure GivenFromSubquery(W: TSQLCuteWorld);

    [Given('a WhereExists query')]
    procedure GivenWhereExists(W: TSQLCuteWorld);

    [Given('a combined JOIN GROUP HAVING ORDER LIMIT query')]
    procedure GivenCombinedJoinGroupQuery(W: TSQLCuteWorld);

    // -----------------------------------------------------------------------
    //  GIVEN — DML (INSERT, UPDATE, DELETE)
    // -----------------------------------------------------------------------

    [Given('a single-row INSERT query')]
    procedure GivenInsertSingleRow(W: TSQLCuteWorld);

    [Given('a multi-row INSERT query')]
    procedure GivenInsertMultiRow(W: TSQLCuteWorld);

    [Given('an INSERT FROM SELECT query')]
    procedure GivenInsertFromSelect(W: TSQLCuteWorld);

    [Given('an UPDATE single-column query')]
    procedure GivenUpdateSingleCol(W: TSQLCuteWorld);

    [Given('an UPDATE multi-column query')]
    procedure GivenUpdateMultiCol(W: TSQLCuteWorld);

    [Given('a query deleting from "(\w+)" where "(\w+)" = (\d+)')]
    procedure GivenDeleteFromWhere(W: TSQLCuteWorld; Table, Col: string; Val: Integer);

    [Given('a query deleting all from "(\w+)"')]
    procedure GivenDeleteAll(W: TSQLCuteWorld; Table: string);

    [Given('an UPDATE with multiple columns and conditions')]
    procedure GivenUpdateMultipleColsAndConditions(W: TSQLCuteWorld);

    // -----------------------------------------------------------------------
    //  GIVEN — WhereIn / WhereNotIn / OrWhereIn / OrWhereNotIn / SelectRaw / OrderByRaw
    // -----------------------------------------------------------------------

    [Given('a WhereIn query')]
    procedure GivenWhereIn(W: TSQLCuteWorld);

    [Given('a WhereIn combined with WHERE')]
    procedure GivenWhereInAndWhere(W: TSQLCuteWorld);

    [Given('a WhereNotIn query')]
    procedure GivenWhereNotIn(W: TSQLCuteWorld);

    [Given('an OrWhereIn query')]
    procedure GivenOrWhereIn(W: TSQLCuteWorld);

    [Given('a SelectRaw query')]
    procedure GivenSelectRaw(W: TSQLCuteWorld);

    [Given('an OrderByRaw query')]
    procedure GivenOrderByRaw(W: TSQLCuteWorld);

    [Given('a combined WhereIn WhereNotIn and WHERE query')]
    procedure GivenCombinedWhereInQuery(W: TSQLCuteWorld);

    // -----------------------------------------------------------------------
    //  GIVEN — RightJoin / CrossJoin / FullOuterJoin / UnionAll / Intersect /
    //          Except / WhereNotExists / GroupByRaw / HavingRaw / WithRecursive /
    //          OrWhereNotIn / GroupBy (multi-column)
    // -----------------------------------------------------------------------

    [Given('a RightJoin query')]
    procedure GivenRightJoin(W: TSQLCuteWorld);

    [Given('a CrossJoin query')]
    procedure GivenCrossJoin(W: TSQLCuteWorld);

    [Given('a FullOuterJoin query')]
    procedure GivenFullOuterJoin(W: TSQLCuteWorld);

    [Given('a UnionAll query')]
    procedure GivenUnionAll(W: TSQLCuteWorld);

    [Given('an Intersect query')]
    procedure GivenIntersect(W: TSQLCuteWorld);

    [Given('an Except query')]
    procedure GivenExcept(W: TSQLCuteWorld);

    [Given('a WhereNotExists query')]
    procedure GivenWhereNotExists(W: TSQLCuteWorld);

    [Given('a GroupByRaw query')]
    procedure GivenGroupByRaw(W: TSQLCuteWorld);

    [Given('a HavingRaw query')]
    procedure GivenHavingRaw(W: TSQLCuteWorld);

    [Given('a WithRecursive query')]
    procedure GivenWithRecursive(W: TSQLCuteWorld);

    [Given('an OrWhereNotIn query')]
    procedure GivenOrWhereNotIn(W: TSQLCuteWorld);

    [Given('a multi-column GroupBy query')]
    procedure GivenGroupByMultiColumn(W: TSQLCuteWorld);

    // -----------------------------------------------------------------------
    //  GIVEN — Fase 1 WHERE variants
    // -----------------------------------------------------------------------

    [Given('an OrWhereNull query')]
    procedure GivenOrWhereNull(W: TSQLCuteWorld);

    [Given('an OrWhereNotNull query')]
    procedure GivenOrWhereNotNull(W: TSQLCuteWorld);

    [Given('a WhereTrue query')]
    procedure GivenWhereTrue(W: TSQLCuteWorld);

    [Given('a WhereFalse query')]
    procedure GivenWhereFalse(W: TSQLCuteWorld);

    [Given('a WhereNot query')]
    procedure GivenWhereNot(W: TSQLCuteWorld);

    [Given('an OrWhereNot query')]
    procedure GivenOrWhereNot(W: TSQLCuteWorld);

    [Given('a WhereNotBetween query')]
    procedure GivenWhereNotBetween(W: TSQLCuteWorld);

    [Given('an OrWhereBetween query')]
    procedure GivenOrWhereBetween(W: TSQLCuteWorld);

    [Given('an OrWhereNotBetween query')]
    procedure GivenOrWhereNotBetween(W: TSQLCuteWorld);

    [Given('an OrWhereExists query')]
    procedure GivenOrWhereExists(W: TSQLCuteWorld);

    [Given('an OrWhereNotExists query')]
    procedure GivenOrWhereNotExists(W: TSQLCuteWorld);

    [Given('a WhereNotInQuery query')]
    procedure GivenWhereNotInQuery(W: TSQLCuteWorld);

    [Given('an OrWhereInQuery query')]
    procedure GivenOrWhereInQuery(W: TSQLCuteWorld);

    // -----------------------------------------------------------------------
    //  GIVEN — Fase 1 Set ops + Paginación + JOIN op
    // -----------------------------------------------------------------------

    [Given('an IntersectAll query')]
    procedure GivenIntersectAll(W: TSQLCuteWorld);

    [Given('an ExceptAll query')]
    procedure GivenExceptAll(W: TSQLCuteWorld);

    [Given('a CombineRaw query')]
    procedure GivenCombineRaw(W: TSQLCuteWorld);

    [Given('a ForPage query page (\d+) per-page (\d+)')]
    procedure GivenForPage(W: TSQLCuteWorld; Page, PerPage: Integer);

    [Given('a Take query')]
    procedure GivenTake(W: TSQLCuteWorld);

    [Given('a Skip query')]
    procedure GivenSkip(W: TSQLCuteWorld);

    [Given('a Join with not-equal operator query')]
    procedure GivenJoinWithNotEqualOp(W: TSQLCuteWorld);

    [Given('a LeftJoin with >= operator query')]
    procedure GivenLeftJoinWithGteOp(W: TSQLCuteWorld);

    // -----------------------------------------------------------------------
    //  GIVEN — coverage gaps
    // -----------------------------------------------------------------------

    [Given('a SelectAs query')]
    procedure GivenSelectAs(W: TSQLCuteWorld);

    [Given('a SelectSum query')]
    procedure GivenSelectSum(W: TSQLCuteWorld);

    [Given('a SelectAvg query')]
    procedure GivenSelectAvg(W: TSQLCuteWorld);

    [Given('a SelectMin query')]
    procedure GivenSelectMin(W: TSQLCuteWorld);

    [Given('a SelectMax query')]
    procedure GivenSelectMax(W: TSQLCuteWorld);

    [Given('a From with alias query')]
    procedure GivenFromAlias(W: TSQLCuteWorld);

    [Given('a WhereNot with operator query')]
    procedure GivenWhereNotWithOp(W: TSQLCuteWorld);

    [Given('an OrWhereNot with operator query')]
    procedure GivenOrWhereNotWithOp(W: TSQLCuteWorld);

    [Given('a WhereInQuery query')]
    procedure GivenWhereInQuery(W: TSQLCuteWorld);

    [Given('an OrWhereNotInQuery query')]
    procedure GivenOrWhereNotInQuery(W: TSQLCuteWorld);

    // -----------------------------------------------------------------------
    //  GIVEN — F2: WHERE groups, WhereColumns, When
    // -----------------------------------------------------------------------

    [Given('a WHERE group query')]
    procedure GivenWhereGroup(W: TSQLCuteWorld);

    [Given('a WHERE group with outer AND condition')]
    procedure GivenWhereGroupWithOuter(W: TSQLCuteWorld);

    [Given('an OR WHERE group query')]
    procedure GivenOrWhereGroup(W: TSQLCuteWorld);

    [Given('a WhereColumns equality query')]
    procedure GivenWhereColumnsEquality(W: TSQLCuteWorld);

    [Given('a WhereColumns with operator query')]
    procedure GivenWhereColumnsWithOp(W: TSQLCuteWorld);

    [Given('a WhereColumns combined with WHERE query')]
    procedure GivenWhereColumnsCombined(W: TSQLCuteWorld);

    [Given('a When true query')]
    procedure GivenWhenTrue(W: TSQLCuteWorld);

    [Given('a When false query')]
    procedure GivenWhenFalse(W: TSQLCuteWorld);

    [Given('a When with false callback query')]
    procedure GivenWhenWithFalseCallback(W: TSQLCuteWorld);

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

procedure TSQLCuteSteps.GivenCombinedSelectWhereLimitQuery(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('users')
    .Select(['id', 'name'])
    .Where('active', True)
    .Limit(10);
end;

// -----------------------------------------------------------------------
//  GIVEN: JOINs, GROUP BY, set operations, CTEs, subqueries
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

procedure TSQLCuteSteps.GivenTwoChainedJoins(W: TSQLCuteWorld);
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

procedure TSQLCuteSteps.GivenGroupByAndHaving(W: TSQLCuteWorld);
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

procedure TSQLCuteSteps.GivenUnionQuery(W: TSQLCuteWorld);
var
  Other: IQuery;
begin
  Other := TQuery.New.From('archived_users').Select('id');
  W.Query := TQuery.New.From('active_users').Select('id').Union(Other);
end;

procedure TSQLCuteSteps.GivenCTEQuery(W: TSQLCuteWorld);
var
  CTEQuery: IQuery;
begin
  CTEQuery := TQuery.New.From('orders').Where('created', '>', '2024-01-01');
  W.Query := TQuery.New.&With('recent', CTEQuery).From('recent');
end;

procedure TSQLCuteSteps.GivenFromSubquery(W: TSQLCuteWorld);
var
  Sub: IQuery;
begin
  Sub := TQuery.New.From('users').Select(['id', 'name']);
  W.Query := TQuery.New.From(Sub, 'u');
end;

procedure TSQLCuteSteps.GivenWhereExists(W: TSQLCuteWorld);
var
  Sub: IQuery;
begin
  Sub := TQuery.New.From('orders').Where('user_id', 42);
  W.Query := TQuery.New.From('users').WhereExists(Sub);
end;

procedure TSQLCuteSteps.GivenCombinedJoinGroupQuery(W: TSQLCuteWorld);
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
//  GIVEN: DML
// -----------------------------------------------------------------------

procedure TSQLCuteSteps.GivenInsertSingleRow(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('users')
    .AsInsert(['name', 'email'], ['John Doe', 'john@example.com']);
end;

procedure TSQLCuteSteps.GivenInsertMultiRow(W: TSQLCuteWorld);
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

procedure TSQLCuteSteps.GivenInsertFromSelect(W: TSQLCuteWorld);
var
  Sub: IQuery;
begin
  Sub := TQuery.New.From('users').Select(['id', 'name']).Where('active', True);
  W.Query := TQuery.New
    .From('archive')
    .AsInsertFrom(['id', 'name'], Sub);
end;

procedure TSQLCuteSteps.GivenUpdateSingleCol(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('users')
    .Where('id', 1)
    .AsUpdate(['status'], ['inactive']);
end;

procedure TSQLCuteSteps.GivenUpdateMultiCol(W: TSQLCuteWorld);
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

procedure TSQLCuteSteps.GivenUpdateMultipleColsAndConditions(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('orders')
    .Where('user_id', 5)
    .Where('status', 'pending')
    .AsUpdate(['status', 'updated_at'], ['shipped', '2024-01-15']);
end;

// -----------------------------------------------------------------------
//  GIVEN: WhereIn / WhereNotIn / OrWhereIn / OrWhereNotIn / SelectRaw / OrderByRaw
// -----------------------------------------------------------------------

procedure TSQLCuteSteps.GivenWhereIn(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('users')
    .WhereIn('id', [1, 2, 3]);
end;

procedure TSQLCuteSteps.GivenWhereInAndWhere(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('users')
    .WhereIn('role', ['admin', 'editor'])
    .Where('active', True);
end;

procedure TSQLCuteSteps.GivenWhereNotIn(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('products')
    .WhereNotIn('status', ['discontinued', 'archived']);
end;

procedure TSQLCuteSteps.GivenOrWhereIn(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('users')
    .Where('active', True)
    .OrWhereIn('id', [10, 20]);
end;

procedure TSQLCuteSteps.GivenSelectRaw(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('users')
    .Select('id')
    .SelectRaw('UPPER(name) AS uname');
end;

procedure TSQLCuteSteps.GivenOrderByRaw(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('users')
    .OrderByRaw('FIELD(status, ''active'', ''pending'', ''closed'')');
end;

procedure TSQLCuteSteps.GivenCombinedWhereInQuery(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('orders')
    .Select(['id', 'name'])
    .WhereIn('status', ['pending', 'shipped'])
    .WhereNotIn('user_id', [99, 100])
    .Where('created_at', '>', '2024-01-01');
end;

// -----------------------------------------------------------------------
//  GIVEN: RightJoin / CrossJoin / FullOuterJoin / UnionAll / Intersect /
//        Except / WhereExists / WhereNotExists / GroupByRaw / HavingRaw /
//        WithRecursive / OrWhereNotIn / GroupBy (multi-column)
// -----------------------------------------------------------------------

procedure TSQLCuteSteps.GivenRightJoin(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('users')
    .RightJoin('posts', 'users.id', 'posts.user_id');
end;

procedure TSQLCuteSteps.GivenCrossJoin(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('users')
    .CrossJoin('tags');
end;

procedure TSQLCuteSteps.GivenFullOuterJoin(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('users')
    .FullOuterJoin('logs', 'users.id', 'logs.user_id');
end;

procedure TSQLCuteSteps.GivenUnionAll(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('a')
    .Select('id')
    .UnionAll(TQuery.New.From('b').Select('id'));
end;

procedure TSQLCuteSteps.GivenIntersect(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('a')
    .Select('id')
    .Intersect(TQuery.New.From('b').Select('id'));
end;

procedure TSQLCuteSteps.GivenExcept(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('a')
    .Select('id')
    .&Except(TQuery.New.From('b').Select('id'));
end;

procedure TSQLCuteSteps.GivenWhereNotExists(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('users')
    .WhereNotExists(TQuery.New.From('orders').Where('user_id', 99));
end;

procedure TSQLCuteSteps.GivenGroupByRaw(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('events')
    .Select('event')
    .GroupByRaw('DATE(created_at)');
end;

procedure TSQLCuteSteps.GivenHavingRaw(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('orders')
    .Select('dept_id')
    .GroupBy('dept_id')
    .HavingRaw('SUM(total) > 1000');
end;

procedure TSQLCuteSteps.GivenWithRecursive(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .WithRecursive('nums', TQuery.New.From('base').Select('n'))
    .From('nums')
    .Select('n');
end;

procedure TSQLCuteSteps.GivenOrWhereNotIn(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('users')
    .Where('active', True)
    .OrWhereNotIn('status', ['banned', 'deleted']);
end;

procedure TSQLCuteSteps.GivenGroupByMultiColumn(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('sales')
    .GroupBy(['year', 'month', 'dept_id']);
end;

// -----------------------------------------------------------------------
//  GIVEN: Fase 1 — WHERE variants
// -----------------------------------------------------------------------

procedure TSQLCuteSteps.GivenOrWhereNull(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users').Where('name', 'alice').OrWhereNull('deleted_at');
end;

procedure TSQLCuteSteps.GivenOrWhereNotNull(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users').Where('name', 'alice').OrWhereNotNull('email');
end;

procedure TSQLCuteSteps.GivenWhereTrue(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users').WhereTrue('active');
end;

procedure TSQLCuteSteps.GivenWhereFalse(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users').WhereFalse('active');
end;

procedure TSQLCuteSteps.GivenWhereNot(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users').WhereNot('status', 'banned');
end;

procedure TSQLCuteSteps.GivenOrWhereNot(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users')
    .Where('status', 'active')
    .OrWhereNot('archived', True);
end;

procedure TSQLCuteSteps.GivenWhereNotBetween(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('orders').WhereNotBetween('total', 100, 500);
end;

procedure TSQLCuteSteps.GivenOrWhereBetween(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users')
    .Where('age', '>', 60)
    .OrWhereBetween('score', 10, 20);
end;

procedure TSQLCuteSteps.GivenOrWhereNotBetween(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users')
    .Where('active', True)
    .OrWhereNotBetween('score', 1, 5);
end;

procedure TSQLCuteSteps.GivenOrWhereExists(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users')
    .Where('active', True)
    .OrWhereExists(TQuery.New.From('orders').Where('user_id', 99));
end;

procedure TSQLCuteSteps.GivenOrWhereNotExists(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users')
    .Where('active', True)
    .OrWhereNotExists(TQuery.New.From('orders').Where('user_id', 99));
end;

procedure TSQLCuteSteps.GivenWhereNotInQuery(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users')
    .WhereNotInQuery('id', TQuery.New.From('banned').Select('user_id'));
end;

procedure TSQLCuteSteps.GivenOrWhereInQuery(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users')
    .Where('active', True)
    .OrWhereInQuery('id', TQuery.New.From('vip').Select('id'));
end;

// -----------------------------------------------------------------------
//  GIVEN: Fase 1 — Set ops + Paginación + JOIN op
// -----------------------------------------------------------------------

procedure TSQLCuteSteps.GivenIntersectAll(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('a').Select('id')
    .IntersectAll(TQuery.New.From('b').Select('id'));
end;

procedure TSQLCuteSteps.GivenExceptAll(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('a').Select('id')
    .ExceptAll(TQuery.New.From('b').Select('id'));
end;

procedure TSQLCuteSteps.GivenCombineRaw(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('a').Select('id')
    .CombineRaw('UNION SELECT id FROM b WHERE status = ''active''');
end;

procedure TSQLCuteSteps.GivenForPage(W: TSQLCuteWorld; Page, PerPage: Integer);
begin
  W.Query := TQuery.New.From('users').ForPage(Page, PerPage);
end;

procedure TSQLCuteSteps.GivenTake(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users').Take(5);
end;

procedure TSQLCuteSteps.GivenSkip(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users').Take(10).Skip(20);
end;

procedure TSQLCuteSteps.GivenJoinWithNotEqualOp(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('orders')
    .Join('users', 'orders.user_id', 'users.id', '<>');
end;

procedure TSQLCuteSteps.GivenLeftJoinWithGteOp(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New
    .From('orders')
    .LeftJoin('tiers', 'orders.amount', 'tiers.min_amount', '>=');
end;

// -----------------------------------------------------------------------
//  GIVEN — coverage gaps
// -----------------------------------------------------------------------

procedure TSQLCuteSteps.GivenSelectAs(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users').SelectAs('name', 'full_name');
end;

procedure TSQLCuteSteps.GivenSelectSum(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('orders').SelectSum('total');
end;

procedure TSQLCuteSteps.GivenSelectAvg(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('results').SelectAvg('score');
end;

procedure TSQLCuteSteps.GivenSelectMin(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('products').SelectMin('price');
end;

procedure TSQLCuteSteps.GivenSelectMax(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('products').SelectMax('price');
end;

procedure TSQLCuteSteps.GivenFromAlias(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users', 'u').Select('u.id');
end;

procedure TSQLCuteSteps.GivenWhereNotWithOp(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users').WhereNot('age', '>', 18);
end;

procedure TSQLCuteSteps.GivenOrWhereNotWithOp(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users')
    .Where('active', True)
    .OrWhereNot('score', '<', 10);
end;

procedure TSQLCuteSteps.GivenWhereInQuery(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users')
    .WhereInQuery('id', TQuery.New.From('vip').Select('id'));
end;

procedure TSQLCuteSteps.GivenOrWhereNotInQuery(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users')
    .Where('active', True)
    .OrWhereNotInQuery('id', TQuery.New.From('banned').Select('user_id'));
end;

// -----------------------------------------------------------------------
//  GIVEN — F2: WHERE groups, WhereColumns, When
// -----------------------------------------------------------------------

procedure TSQLCuteSteps.GivenWhereGroup(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users')
    .Where(
      function(Q: IQuery): IQuery
      begin
        Result := Q.Where('age', '>', 18).Where('active', True);
      end);
end;

procedure TSQLCuteSteps.GivenWhereGroupWithOuter(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('orders')
    .Where('status', 'open')
    .Where(
      function(Q: IQuery): IQuery
      begin
        Result := Q.Where('total', '>', 100).OrWhere('priority', 1);
      end);
end;

procedure TSQLCuteSteps.GivenOrWhereGroup(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users')
    .Where('vip', True)
    .OrWhere(
      function(Q: IQuery): IQuery
      begin
        Result := Q.Where('age', '>', 18).Where('active', True);
      end);
end;

procedure TSQLCuteSteps.GivenWhereColumnsEquality(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('employees')
    .WhereColumns('manager_id', '=', 'employee_id');
end;

procedure TSQLCuteSteps.GivenWhereColumnsWithOp(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('products')
    .WhereColumns('price', '>', 'min_price');
end;

procedure TSQLCuteSteps.GivenWhereColumnsCombined(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('products')
    .Where('active', True)
    .WhereColumns('price', '>', 'min_price');
end;

procedure TSQLCuteSteps.GivenWhenTrue(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users')
    .When(True,
      function(Q: IQuery): IQuery
      begin
        Result := Q.Where('active', True);
      end);
end;

procedure TSQLCuteSteps.GivenWhenFalse(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users')
    .When(False,
      function(Q: IQuery): IQuery
      begin
        Result := Q.Where('active', True);
      end);
end;

procedure TSQLCuteSteps.GivenWhenWithFalseCallback(W: TSQLCuteWorld);
begin
  W.Query := TQuery.New.From('users')
    .When(False,
      function(Q: IQuery): IQuery
      begin
        Result := Q.Where('active', True);
      end,
      function(Q: IQuery): IQuery
      begin
        Result := Q.Where('archived', True);
      end);
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
