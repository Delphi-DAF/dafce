unit Parity.Feat;

{
  Phase 4 — the symmetry harness. One shared query set is pushed through every
  back-end over one seeded dataset, and every lane must agree:

    * SQL lane          — TAnsiSqlCompiler.Compile: the query must compile to
                          SQL whose '?' placeholders match its bindings 1:1.
    * general lane      — TInMemory.Rows(Q, Context): composite IRow rows.
    * single-list rows  — TInMemory.Rows<T>(Q, list): IRow over one typed list.
    * fast typed lane   — TInMemory.Queryable<T>(Q, list): preserve-T, no IRow.

  DAFce ships no database driver, so the harness cannot execute the emitted SQL.
  The oracle is therefore declared per case — the row set a SQL database returns
  for that query over this seed. What the harness proves is that all three
  execution lanes agree with that one shared specification, that the SQL
  back-end accepts the very same AST with self-consistent bindings, and that
  neither back-end mutates the query. The mixed AND/OR case additionally pins
  the parentheses the SQL text carries, which is the tree the in-memory
  predicate was built from (Decision 13 — parity by construction).
}

interface

uses
  System.Generics.Collections;

type
  /// <summary>Seed entity; Age is Variant so it can carry SQL NULL.</summary>
  TPerson = class
  private
    FId: Integer;
    FName: string;
    FCountry: string;
    FAge: Variant;
  public
    constructor Create(const AId: Integer; const AName, ACountry: string; const AAge: Variant);
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    property Country: string read FCountry write FCountry;
    property Age: Variant read FAge write FAge;
  end;

  /// <summary>Second seed source, for the JOIN and subquery cases.</summary>
  TOrder = class
  private
    FId: Integer;
    FUserId: Integer;
    FTotal: Integer;
  public
    constructor Create(const AId, AUserId, ATotal: Integer);
    property Id: Integer read FId write FId;
    property UserId: Integer read FUserId write FUserId;
    property Total: Integer read FTotal write FTotal;
  end;

  TParityWorld = class
  public
    People: TObjectList<TPerson>;
    Orders: TObjectList<TOrder>;
    Cases: Integer;
    SqlLaneErrors: string;
    OracleMismatches: string;
    LaneMismatches: string;
    MutationErrors: string;
    PrecedenceSql: string;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,
  System.Variants,
  System.Rtti,
  Daf.MiniSpec,
  Daf.Comparers,
  Daf.Linq,
  Daf.SQLCute,
  Daf.SQLCute.Compiler,
  Daf.SQLCute.InMemory,
  Daf.SQLCute.InMemory.Rows;

{ TPerson }

constructor TPerson.Create(const AId: Integer; const AName, ACountry: string; const AAge: Variant);
begin
  inherited Create;
  FId := AId;
  FName := AName;
  FCountry := ACountry;
  FAge := AAge;
end;

{ TOrder }

constructor TOrder.Create(const AId, AUserId, ATotal: Integer);
begin
  inherited Create;
  FId := AId;
  FUserId := AUserId;
  FTotal := ATotal;
end;

{ TParityWorld }

// The seed. Two NULL ages (Dan, Gil) drive the 3VL / NULL-ordering /
// NULL-collapsing cases; duplicate ages (25) drive DISTINCT; Cara and Gil have
// no orders, which is what the JOIN and subquery cases hinge on.
constructor TParityWorld.Create;
begin
  inherited;
  People := TObjectList<TPerson>.Create(True {owns});
  People.Add(TPerson.Create(1, 'Ann',  'US', 30));
  People.Add(TPerson.Create(2, 'Bob',  'US', 17));
  People.Add(TPerson.Create(3, 'Cara', 'UK', 25));
  People.Add(TPerson.Create(4, 'Dan',  'UK', Null));
  People.Add(TPerson.Create(5, 'Eve',  'US', 40));
  People.Add(TPerson.Create(6, 'Finn', 'UK', 25));
  People.Add(TPerson.Create(7, 'Gil',  'ES', Null));
  Orders := TObjectList<TOrder>.Create(True {owns});
  Orders.Add(TOrder.Create(10, 1, 100));   // Ann
  Orders.Add(TOrder.Create(11, 1, 200));   // Ann
  Orders.Add(TOrder.Create(12, 2, 50));    // Bob
  Orders.Add(TOrder.Create(13, 5, 300));   // Eve
end;

destructor TParityWorld.Destroy;
begin
  Orders.Free;
  People.Free;
  inherited;
end;

{ the shared query set }

type
  /// <summary>One query of the shared set plus the row set SQL yields for it.</summary>
  TParityCase = record
    Name: string;
    Query: IQuery;
    Cols: TArray<string>;    // columns forming each row's signature
    Expected: string;        // oracle: the SQL result set over the seed
    TypedLane: Boolean;      // also runnable through TInMemory.Queryable<T>
    RowLane: Boolean;        // also runnable through TInMemory.Rows<T> (one list)
    Unordered: Boolean;      // no ORDER BY — compare as a sorted set
  end;

function MakeCase(const Name: string; const Query: IQuery; const Cols: TArray<string>;
  const Expected: string; TypedLane: Boolean = True; RowLane: Boolean = True;
  Unordered: Boolean = False): TParityCase;
begin
  Result.Name := Name;
  Result.Query := Query;
  Result.Cols := Cols;
  Result.Expected := Expected;
  Result.TypedLane := TypedLane;
  Result.RowLane := RowLane;
  Result.Unordered := Unordered;
end;

// The mixed AND/OR query — shared by the query set and the precedence scenario
// so both assert the same AST: (country=UK AND age=25) OR (country=US AND age=40).
function PrecedenceQuery: IQuery;
begin
  Result := TQuery.New.From('people')
    .Where('country', 'UK').Where('age', 25)
    .OrWhere('country', 'US').Where('age', 40)
    .OrderBy('name');
end;

function SharedQuerySet: TArray<TParityCase>;
begin
  Result := [
    // --- WHERE, three-valued logic ---------------------------------------
    MakeCase('where-greater',
      TQuery.New.From('people').Where('age', '>', 18).OrderBy('name'),
      ['name'], 'Ann, Cara, Eve, Finn'),
    MakeCase('and-or-precedence',
      PrecedenceQuery,
      ['name'], 'Cara, Eve, Finn'),
    MakeCase('not-of-unknown',
      TQuery.New.From('people').WhereNot('age', '>', 18).OrderBy('name'),
      ['name'], 'Bob'),                       // NOT(unknown) stays unknown
    // Negated group: NOT (country = UK AND age > 18). Dan (UK, NULL) is dropped
    // because the group is unknown, while Gil (ES, NULL) is kept because the
    // country mismatch already makes the group false.
    MakeCase('negated-group',
      TQuery.New.From('people')
        .WhereNot(
          function(Q: IQuery): IQuery
          begin
            Result := Q.Where('country', 'UK').Where('age', '>', 18);
          end)
        .OrderBy('name'),
      ['name'], 'Ann, Bob, Eve, Gil'),
    MakeCase('is-null',
      TQuery.New.From('people').WhereNull('age').OrderBy('name'),
      ['name'], 'Dan, Gil'),
    MakeCase('is-not-null',
      TQuery.New.From('people').WhereNotNull('age').OrderBy('name'),
      ['name'], 'Ann, Bob, Cara, Eve, Finn'),
    MakeCase('between',
      TQuery.New.From('people').WhereBetween('age', 25, 30).OrderBy('name'),
      ['name'], 'Ann, Cara, Finn'),
    MakeCase('in-values',
      TQuery.New.From('people').WhereIn('country', ['US', 'ES']).OrderBy('name'),
      ['name'], 'Ann, Bob, Eve, Gil'),
    MakeCase('not-in-values',
      TQuery.New.From('people').WhereNotIn('country', ['US']).OrderBy('name'),
      ['name'], 'Cara, Dan, Finn, Gil'),
    MakeCase('like-contains',
      TQuery.New.From('people').WhereContains('name', 'an', False).OrderBy('name'),
      ['name'], 'Ann, Dan'),

    // --- ORDER BY / paging / DISTINCT -------------------------------------
    MakeCase('order-nulls-first',
      TQuery.New.From('people').OrderBy('age').OrderBy('name'),
      ['name'], 'Dan, Gil, Bob, Cara, Finn, Ann, Eve'),
    MakeCase('order-desc-nulls-last',
      TQuery.New.From('people').OrderByDesc('age').OrderBy('name'),
      ['name'], 'Eve, Ann, Cara, Finn, Bob, Dan, Gil'),
    // Explicitly qualified ordering: the option is what makes one declared
    // result set valid for every back-end, instead of each applying its own rule.
    MakeCase('order-explicit-nulls-last',
      TQuery.New.From('people').OrderBy('age').NullsLast.OrderBy('name'),
      ['name'], 'Bob, Cara, Finn, Ann, Eve, Dan, Gil'),
    MakeCase('order-desc-explicit-nulls-first',
      TQuery.New.From('people').OrderByDesc('age').NullsFirst.OrderBy('name'),
      ['name'], 'Dan, Gil, Eve, Ann, Cara, Finn, Bob'),
    MakeCase('paging',
      TQuery.New.From('people').OrderBy('name').ForPage(2, 3),
      ['name'], 'Dan, Eve, Finn'),
    MakeCase('distinct-projection',
      TQuery.New.From('people').Select('age').Distinct.OrderBy('age'),
      ['age'], 'NULL, 17, 25, 30, 40',
      {TypedLane} False),                     // projects: no preserve-T lane

    // --- multi-source: grouping, joins, subqueries, set ops ---------------
    MakeCase('group-having',
      TQuery.New.From('people').Select('country').SelectCount('*', 'n')
        .GroupBy('country').Having('n', '>', 1).OrderBy('country'),
      ['country', 'n'], 'UK/3, US/3',
      {TypedLane} False, {RowLane} False),
    MakeCase('group-avg-skips-nulls',
      TQuery.New.From('people').Select('country').SelectAvg('age', 'avg_age')
        .GroupBy('country').OrderBy('country'),
      ['country', 'avg_age'], 'ES/NULL, UK/25, US/29',
      {TypedLane} False, {RowLane} False),
    MakeCase('inner-join',
      TQuery.New.From('people').Join('orders', 'people.id', 'orders.user_id')
        .Select(['people.name', 'orders.total'])
        .OrderBy('people.name').OrderBy('orders.total'),
      ['people.name', 'orders.total'], 'Ann/100, Ann/200, Bob/50, Eve/300',
      {TypedLane} False, {RowLane} False),
    MakeCase('subquery-in',
      TQuery.New.From('people')
        .WhereInQuery('id', TQuery.New.From('orders').Select('user_id'))
        .OrderBy('name'),
      ['name'], 'Ann, Bob, Eve',
      {TypedLane} False, {RowLane} False),
    MakeCase('union',
      TQuery.New.From('people').Select('country').Where('country', 'US')
        .Union(TQuery.New.From('people').Select('country').Where('country', 'ES')),
      ['country'], 'ES, US',
      {TypedLane} False, {RowLane} False, {Unordered} True)
  ];
end;

{ row signatures — the common currency every lane is compared in }

function ValueSig(const V: TValue): string;
begin
  if V.IsEmpty or IsNullValue(V) then
    Exit('NULL');
  Result := VarToStr(V.AsVariant);
end;

function RowSig(const Row: IRow; const Cols: TArray<string>): string;
begin
  Result := '';
  for var C in Cols do
  begin
    if Result <> '' then
      Result := Result + '/';
    Result := Result + ValueSig(Row.Get(C));
  end;
end;

function SetSig(const Sigs: TArray<string>; Unordered: Boolean): string;
begin
  var Items := Copy(Sigs);
  if Unordered then
    TArray.Sort<string>(Items);
  Result := string.Join(', ', Items);
end;

{ the four lanes }

function RunRows(const Rows: IQueryable<IRow>; const C: TParityCase): string;
begin
  var Sigs: TArray<string> := nil;
  for var R in Rows do
    Sigs := Sigs + [RowSig(R, C.Cols)];
  Result := SetSig(Sigs, C.Unordered);
end;

// General multi-source lane: every case runs here, so it is the reference lane.
function RunGeneral(W: TParityWorld; const C: TParityCase): string;
begin
  var Ctx := TQueryContext.Create;
  try
    Ctx.Map<TPerson>('people', W.People).Map<TOrder>('orders', W.Orders);
    Result := RunRows(TInMemory.Rows(C.Query, Ctx), C);
  finally
    Ctx.Free;
  end;
end;

// Single-list dynamic lane: one typed list in, IRow out (projection applied).
function RunSingleListRows(W: TParityWorld; const C: TParityCase): string;
begin
  Result := RunRows(TInMemory.Rows<TPerson>(C.Query, W.People), C);
end;

// Fast typed lane: T in, T out. The element is viewed through TRttiRow only to
// read the signature columns — the lane itself never allocates a row wrapper.
function RunFastTyped(W: TParityWorld; const C: TParityCase): string;
begin
  var Acc: IRttiAccessors<TPerson> := TRttiAccessors<TPerson>.Create;
  var Sigs: TArray<string> := nil;
  for var P in TInMemory.Queryable<TPerson>(C.Query, W.People) do
  begin
    // Bind the view to an interface variable: a const-parameter temporary would
    // never be released.
    var View: IRow := TRttiRow<TPerson>.Create(Acc, P);
    Sigs := Sigs + [RowSig(View, C.Cols)];
  end;
  Result := SetSig(Sigs, C.Unordered);
end;

{ SQL lane }

function CompileSql(const Query: IQuery): TSQLResult;
begin
  var Compiler: IQueryCompiler := TAnsiSqlCompiler.Create;
  Result := Query.Compile(Compiler);
end;

function PlaceholderCount(const Sql: string): Integer;
begin
  Result := 0;
  for var Ch in Sql do
    if Ch = '?' then
      Inc(Result);
end;

{ the harness }

procedure RunHarness(W: TParityWorld);
begin
  W.SqlLaneErrors := '';
  W.OracleMismatches := '';
  W.LaneMismatches := '';
  W.MutationErrors := '';

  var Cases := SharedQuerySet;
  W.Cases := Length(Cases);

  for var C in Cases do
  begin
    // 1. The SQL back-end accepts the same AST, with self-consistent bindings.
    var Sql := CompileSql(C.Query);
    if not Sql.SQL.StartsWith('SELECT') then
      W.SqlLaneErrors := W.SqlLaneErrors
        + Format('[%s] no SELECT compiled: "%s"; ', [C.Name, Sql.SQL]);
    if PlaceholderCount(Sql.SQL) <> Length(Sql.Bindings) then
      W.SqlLaneErrors := W.SqlLaneErrors
        + Format('[%s] %d placeholders vs %d bindings; ',
                 [C.Name, PlaceholderCount(Sql.SQL), Length(Sql.Bindings)]);

    // 2. The general lane must reproduce the declared SQL result set.
    var General := RunGeneral(W, C);
    if General <> C.Expected then
      W.OracleMismatches := W.OracleMismatches
        + Format('[%s] expected "%s" but got "%s"; ', [C.Name, C.Expected, General]);

    // 3. The narrower lanes must agree with the general one.
    if C.RowLane then
    begin
      var Dynamic := RunSingleListRows(W, C);
      if Dynamic <> General then
        W.LaneMismatches := W.LaneMismatches
          + Format('[%s] single-list rows "%s" vs general "%s"; ', [C.Name, Dynamic, General]);
    end;
    if C.TypedLane then
    begin
      var Typed := RunFastTyped(W, C);
      if Typed <> General then
        W.LaneMismatches := W.LaneMismatches
          + Format('[%s] fast typed lane "%s" vs general "%s"; ', [C.Name, Typed, General]);
    end;

    // 4. Neither back-end mutated the shared AST.
    if CompileSql(C.Query).SQL <> Sql.SQL then
      W.MutationErrors := W.MutationErrors
        + Format('[%s] SQL changed after in-memory execution; ', [C.Name]);
  end;
end;

initialization

Feature('''
Feature SQLCute back-end symmetry @inmemory @parity

  As a developer relying on "one IQuery, two back-ends"
  I want one shared query set checked against the SQL compiler and every in-memory lane
  So a query means the same thing whether it hits a database or an already-loaded list
''')

.UseWorld<TParityWorld>

.Rule('One shared query set runs through every back-end over the same seed')

  .Scenario('Every lane reproduces the result set declared for each query')
    .When('each query is compiled to SQL and executed over the seeded people and orders',
      procedure(W: TParityWorld)
      begin
        RunHarness(W);
      end)
    .&Then('the query set is not empty',
      procedure(W: TParityWorld)
      begin
        Expect(W.Cases).ToBeGreaterThan(10);
      end)
    .&And('the SQL back-end compiles every query with bindings matching its placeholders',
      procedure(W: TParityWorld)
      begin
        Expect(W.SqlLaneErrors).ToEqual('');
      end)
    .&And('every in-memory result equals the result set SQL returns for that query',
      procedure(W: TParityWorld)
      begin
        Expect(W.OracleMismatches).ToEqual('');
      end)
    .&And('the fast typed lane and the single-list row lane agree with the general lane',
      procedure(W: TParityWorld)
      begin
        Expect(W.LaneMismatches).ToEqual('');
      end)
    .&And('neither back-end mutated the shared query',
      procedure(W: TParityWorld)
      begin
        Expect(W.MutationErrors).ToEqual('');
      end)

.Rule('Mixed AND/OR precedence comes from one shared tree (Decision 13)')

  .Scenario('The emitted SQL carries the parentheses the in-memory predicate was built from')
    .When('the mixed AND/OR query is compiled by the SQL back-end',
      procedure(W: TParityWorld)
      begin
        W.PrecedenceSql := CompileSql(PrecedenceQuery).SQL;
      end)
    .&Then('the WHERE clause is precedence-explicit, not left-to-right',
      procedure(W: TParityWorld)
      begin
        Expect(W.PrecedenceSql).ToContain(') OR (');
      end);

end.
