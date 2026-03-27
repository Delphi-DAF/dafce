unit Daf.SQLCute;

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils,
  System.Variants,
  System.Generics.Collections,
  Daf.SQLCute.Clauses;

type
  /// <summary>
  /// Result of a compiled query: the SQL string and its ordered bindings.
  /// '?' placeholders in SQL map positionally to Bindings elements.
  /// Dialect compilers may use different placeholder styles (e.g. $1, :p1).
  /// </summary>
  TSQLResult = record
    SQL: string;
    Bindings: TArray<Variant>;
    class function Empty: TSQLResult; static;
  end;

  // ---------------------------------------------------------------------------
  //  IQuery  —  fluent query builder interface
  // ---------------------------------------------------------------------------

  IQueryCompiler = interface;  // forward

  /// <summary>
  /// Fluent interface for building SQL queries.
  /// Every method returns Self (as IQuery) for method chaining.
  /// Obtain an instance via TQuery.New — lifetime is ref-counted, no Free needed.
  /// </summary>
  IQuery = interface
    ['{B3C4D5E6-F780-9ABC-DEF0-123456789ABC}']

    // --- SELECT ---------------------------------------------------------

    /// <summary>Adds one or more columns to the SELECT list.</summary>
    function Select(const Columns: TArray<string>): IQuery; overload;
    /// <summary>Adds a single column to the SELECT list.</summary>
    function Select(const Column: string): IQuery; overload;
    /// <summary>Adds a raw SQL expression to the SELECT list.</summary>
    function SelectRaw(const Expression: string): IQuery;

    // --- FROM -----------------------------------------------------------

    /// <summary>Sets the FROM table.</summary>
    function From(const Table: string): IQuery; overload;
    /// <summary>Sets the FROM table with an alias.</summary>
    function From(const Table, Alias: string): IQuery; overload;

    // --- WHERE ----------------------------------------------------------

    /// <summary>Adds an AND WHERE column = value condition.</summary>
    function Where(const Column: string; const Value: Variant): IQuery; overload;
    /// <summary>Adds an AND WHERE column {op} value condition.</summary>
    function Where(const Column, Op: string; const Value: Variant): IQuery; overload;
    /// <summary>Adds an OR WHERE column = value condition.</summary>
    function OrWhere(const Column: string; const Value: Variant): IQuery; overload;
    /// <summary>Adds an OR WHERE column {op} value condition.</summary>
    function OrWhere(const Column, Op: string; const Value: Variant): IQuery; overload;
    /// <summary>Adds a WHERE column IS NULL condition.</summary>
    function WhereNull(const Column: string): IQuery;
    /// <summary>Adds a WHERE column IS NOT NULL condition.</summary>
    function WhereNotNull(const Column: string): IQuery;
    /// <summary>Adds a WHERE column BETWEEN low AND high condition.</summary>
    function WhereBetween(const Column: string; const Low, High: Variant): IQuery;
    /// <summary>Appends a raw SQL WHERE fragment (AND connector).</summary>
    function WhereRaw(const Sql: string): IQuery;

    // --- ORDER BY -------------------------------------------------------

    /// <summary>Adds an ORDER BY column ASC clause.</summary>
    function OrderBy(const Column: string): IQuery;
    /// <summary>Adds an ORDER BY column DESC clause.</summary>
    function OrderByDesc(const Column: string): IQuery;
    /// <summary>Adds a raw ORDER BY expression.</summary>
    function OrderByRaw(const Expression: string): IQuery;

    // --- LIMIT / OFFSET -------------------------------------------------

    /// <summary>Sets the LIMIT clause.</summary>
    function Limit(const Value: Int64): IQuery;
    /// <summary>Sets the OFFSET clause.</summary>
    function Offset(const Value: Int64): IQuery;

    // --- JOIN -----------------------------------------------------------

    function Join(const Table, Col1, Col2: string): IQuery; overload;
    function Join(const Table, Condition: string): IQuery; overload;
    function LeftJoin(const Table, Col1, Col2: string): IQuery; overload;
    function LeftJoin(const Table, Condition: string): IQuery; overload;
    function RightJoin(const Table, Col1, Col2: string): IQuery; overload;
    function RightJoin(const Table, Condition: string): IQuery; overload;
    function CrossJoin(const Table: string): IQuery;
    function FullOuterJoin(const Table, Col1, Col2: string): IQuery; overload;
    function FullOuterJoin(const Table, Condition: string): IQuery; overload;

    // --- DISTINCT -------------------------------------------------------

    function Distinct: IQuery;

    // --- FROM (sub-query overload) --------------------------------------

    function From(const SubQuery: IQuery; const Alias: string): IQuery; overload;

    // --- GROUP BY / HAVING ----------------------------------------------

    function GroupBy(const Column: string): IQuery; overload;
    function GroupBy(const Columns: TArray<string>): IQuery; overload;
    function GroupByRaw(const Expression: string): IQuery;
    /// <summary>Adds a HAVING condition. Column may be an aggregate expression.</summary>
    function Having(const Column, Op: string; const Value: Variant): IQuery;
    function HavingRaw(const Sql: string): IQuery;

    // --- UNION / INTERSECT / EXCEPT -------------------------------------

    function Union(const Other: IQuery): IQuery;
    function UnionAll(const Other: IQuery): IQuery;
    function Intersect(const Other: IQuery): IQuery;
    function &Except(const Other: IQuery): IQuery;

    // --- WHERE (sub-query variants) -------------------------------------

    function WhereExists(const SubQuery: IQuery): IQuery;
    function WhereNotExists(const SubQuery: IQuery): IQuery;
    function WhereInQuery(const Column: string; const SubQuery: IQuery): IQuery;

    // --- SELECT (aliases + aggregates) ----------------------------------

    /// <summary>Adds a column with an explicit alias to the SELECT list.</summary>
    function SelectAs(const Column, Alias: string): IQuery;
    function SelectCount(const Column: string = '*'; const Alias: string = 'count'): IQuery;
    function SelectSum(const Column: string; const Alias: string = 'sum'): IQuery;
    function SelectAvg(const Column: string; const Alias: string = 'avg'): IQuery;
    function SelectMin(const Column: string; const Alias: string = 'min'): IQuery;
    function SelectMax(const Column: string; const Alias: string = 'max'): IQuery;

    // --- WITH (CTE) -----------------------------------------------------

    function &With(const Name: string; const SubQuery: IQuery): IQuery;
    function WithRecursive(const Name: string; const SubQuery: IQuery): IQuery;

    // --- FORK / CLONE ---------------------------------------------------

    /// <summary>
    /// Returns a deep copy of this query.
    /// Useful for building query variants from a common base.
    /// </summary>
    function Clone: IQuery;

    // --- COMPILE --------------------------------------------------------

    /// <summary>
    /// Convenience: compiles this query using the provided compiler.
    /// Equivalent to Compiler.Compile(Self).
    /// </summary>
    function Compile(const Compiler: IQueryCompiler): TSQLResult;

    // --- INTERNAL (used by compilers) -----------------------------------

    /// <summary>Returns the ordered list of AST clause nodes.</summary>
    function Clauses: TArray<TAbstractClause>;
  end;

  // ---------------------------------------------------------------------------
  //  IQueryCompiler  —  strategy for dialect-specific SQL generation
  // ---------------------------------------------------------------------------

  /// <summary>
  /// Compiles an IQuery into dialect-specific SQL text and bindings.
  /// Inject via DI; default implementation is TAnsiSqlCompiler in
  /// Daf.SQLCute.Compiler unit.
  /// </summary>
  IQueryCompiler = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    function Compile(const Query: IQuery): TSQLResult;
  end;

  /// <summary>
  /// A FROM clause backed by a sub-query instead of a plain table name.
  /// Declared here (not in Clauses.pas) because it holds an IQuery reference.
  /// </summary>
  TFromSubqueryClause = class(TAbstractClause)
  public
    SubQuery: IQuery;
    Alias: string;
    function Clone: TAbstractClause; override;
  end;

  // ---------------------------------------------------------------------------
  //  TQuery  —  factory
  // ---------------------------------------------------------------------------

  /// <summary>
  /// Factory class providing the TQuery.New entry point.
  /// </summary>
  TQuery = class
  public
    /// <summary>Creates a new, empty query builder (ref-counted).</summary>
    class function New: IQuery; static;
  end;

// ---------------------------------------------------------------------------
//  Helpers
// ---------------------------------------------------------------------------

/// <summary>Converts an operator string ('=', '>', '<', etc.) to TWhereOp.</summary>
function ParseWhereOp(const Op: string): TWhereOp;

implementation

type

  // ---------------------------------------------------------------------------
  //  TQueryImpl
  // ---------------------------------------------------------------------------

  TQueryImpl = class(TInterfacedObject, IQuery)
  private
    FClauses: TObjectList<TAbstractClause>;
    function AddWhereClause(const Column: string; Op: TWhereOp;
      const Value: Variant; Conn: TBoolOp; IsNot: Boolean = False): IQuery;
    function AddWhereClause2(const Column: string; Op: TWhereOp;
      const V1, V2: Variant; Conn: TBoolOp): IQuery;
    function AddJoinClause(JoinType: TJoinType; const Table, Condition: string): IQuery;
    function AddHavingClause(const Column: string; Op: TWhereOp;
      const Value: Variant; Conn: TBoolOp): IQuery;
    function AddUnionClause(Kind: TUnionKind; const Other: IQuery): IQuery;
    function AddWithClause(const Name: string; const SubQuery: IQuery;
      IsRecursive: Boolean): IQuery;
    procedure SelectAggregateRaw(const AggFunc, Column, Alias: string);
  public
    constructor Create;
    destructor Destroy; override;

    // IQuery
    function Select(const Columns: TArray<string>): IQuery; overload;
    function Select(const Column: string): IQuery; overload;
    function SelectRaw(const Expression: string): IQuery;
    function From(const Table: string): IQuery; overload;
    function From(const Table, Alias: string): IQuery; overload;
    function Where(const Column: string; const Value: Variant): IQuery; overload;
    function Where(const Column, Op: string; const Value: Variant): IQuery; overload;
    function OrWhere(const Column: string; const Value: Variant): IQuery; overload;
    function OrWhere(const Column, Op: string; const Value: Variant): IQuery; overload;
    function WhereNull(const Column: string): IQuery;
    function WhereNotNull(const Column: string): IQuery;
    function WhereBetween(const Column: string; const Low, High: Variant): IQuery;
    function WhereRaw(const Sql: string): IQuery;
    function OrderBy(const Column: string): IQuery;
    function OrderByDesc(const Column: string): IQuery;
    function OrderByRaw(const Expression: string): IQuery;
    function Limit(const Value: Int64): IQuery;
    function Offset(const Value: Int64): IQuery;
    function Clone: IQuery;
    function Compile(const Compiler: IQueryCompiler): TSQLResult;
    function Clauses: TArray<TAbstractClause>;
    // IQuery — Phase 2
    function Join(const Table, Col1, Col2: string): IQuery; overload;
    function Join(const Table, Condition: string): IQuery; overload;
    function LeftJoin(const Table, Col1, Col2: string): IQuery; overload;
    function LeftJoin(const Table, Condition: string): IQuery; overload;
    function RightJoin(const Table, Col1, Col2: string): IQuery; overload;
    function RightJoin(const Table, Condition: string): IQuery; overload;
    function CrossJoin(const Table: string): IQuery;
    function FullOuterJoin(const Table, Col1, Col2: string): IQuery; overload;
    function FullOuterJoin(const Table, Condition: string): IQuery; overload;
    function Distinct: IQuery;
    function From(const SubQuery: IQuery; const Alias: string): IQuery; overload;
    function GroupBy(const Column: string): IQuery; overload;
    function GroupBy(const Columns: TArray<string>): IQuery; overload;
    function GroupByRaw(const Expression: string): IQuery;
    function Having(const Column, Op: string; const Value: Variant): IQuery;
    function HavingRaw(const Sql: string): IQuery;
    function Union(const Other: IQuery): IQuery;
    function UnionAll(const Other: IQuery): IQuery;
    function Intersect(const Other: IQuery): IQuery;
    function &Except(const Other: IQuery): IQuery;
    function WhereExists(const SubQuery: IQuery): IQuery;
    function WhereNotExists(const SubQuery: IQuery): IQuery;
    function WhereInQuery(const Column: string; const SubQuery: IQuery): IQuery;
    function SelectAs(const Column, Alias: string): IQuery;
    function SelectCount(const Column: string = '*'; const Alias: string = 'count'): IQuery;
    function SelectSum(const Column: string; const Alias: string = 'sum'): IQuery;
    function SelectAvg(const Column: string; const Alias: string = 'avg'): IQuery;
    function SelectMin(const Column: string; const Alias: string = 'min'): IQuery;
    function SelectMax(const Column: string; const Alias: string = 'max'): IQuery;
    function &With(const Name: string; const SubQuery: IQuery): IQuery;
    function WithRecursive(const Name: string; const SubQuery: IQuery): IQuery;
  end;

{ TSQLResult }

class function TSQLResult.Empty: TSQLResult;
begin
  Result.SQL      := '';
  Result.Bindings := [];
end;

{ TFromSubqueryClause }

function TFromSubqueryClause.Clone: TAbstractClause;
var
  C: TFromSubqueryClause;
begin
  C := TFromSubqueryClause.Create;
  C.SubQuery := SubQuery;
  C.Alias    := Alias;
  Result := C;
end;

{ ParseWhereOp }

function ParseWhereOp(const Op: string): TWhereOp;
var
  S: string;
begin
  S := Trim(Op);
  if S = '='  then Exit(TWhereOp.Equal);
  if S = '<>' then Exit(TWhereOp.NotEqual);
  if S = '!=' then Exit(TWhereOp.NotEqual);
  if S = '<'  then Exit(TWhereOp.Less);
  if S = '<=' then Exit(TWhereOp.LessOrEqual);
  if S = '>'  then Exit(TWhereOp.Greater);
  if S = '>=' then Exit(TWhereOp.GreaterOrEqual);
  if SameText(S, 'like')     then Exit(TWhereOp.Like);
  if SameText(S, 'not like') then Exit(TWhereOp.NotLike);
  if SameText(S, 'in')       then Exit(TWhereOp.&In);
  if SameText(S, 'not in')   then Exit(TWhereOp.NotIn);
  raise EArgumentException.CreateFmt('Unknown SQL operator: "%s"', [Op]);
end;

{ TQueryImpl }

constructor TQueryImpl.Create;
begin
  inherited Create;
  FClauses := TObjectList<TAbstractClause>.Create(True {owns objects});
end;

destructor TQueryImpl.Destroy;
begin
  FClauses.Free;
  inherited;
end;

function TQueryImpl.AddWhereClause(const Column: string; Op: TWhereOp;
  const Value: Variant; Conn: TBoolOp; IsNot: Boolean): IQuery;
var
  W: TWhereClause;
begin
  W := TWhereClause.Create;
  W.Column    := Column;
  W.Op        := Op;
  W.Value     := Value;
  W.Connector := Conn;
  W.IsNot     := IsNot;
  FClauses.Add(W);
  Result := Self;
end;

function TQueryImpl.AddWhereClause2(const Column: string; Op: TWhereOp;
  const V1, V2: Variant; Conn: TBoolOp): IQuery;
var
  W: TWhereClause;
begin
  W := TWhereClause.Create;
  W.Column    := Column;
  W.Op        := Op;
  W.Value     := V1;
  W.Value2    := V2;
  W.Connector := Conn;
  FClauses.Add(W);
  Result := Self;
end;

// --- SELECT ---

function TQueryImpl.Select(const Columns: TArray<string>): IQuery;
var
  Sel: TSelectClause;
  I: Integer;
begin
  // Append to existing SELECT columns (allows chaining multiple Select calls)
  Sel := nil;
  for var Cl in FClauses do
    if Cl is TSelectClause then
    begin
      Sel := TSelectClause(Cl);
      Break;
    end;

  if Sel = nil then
  begin
    Sel := TSelectClause.Create;
    FClauses.Add(Sel);
  end;

  var OldLen := Length(Sel.Columns);
  SetLength(Sel.Columns, OldLen + Length(Columns));
  for I := 0 to High(Columns) do
  begin
    var Col: TSelectColumn;
    Col.Column := Columns[I];
    Col.Alias  := '';
    Col.IsRaw  := False;
    Sel.Columns[OldLen + I] := Col;
  end;
  Result := Self;
end;

function TQueryImpl.Select(const Column: string): IQuery;
begin
  Result := Select([Column]);
end;

function TQueryImpl.SelectRaw(const Expression: string): IQuery;
var
  Sel: TSelectClause;
  Col: TSelectColumn;
  Idx: Integer;
begin
  Sel := nil;
  for var Cl in FClauses do
    if Cl is TSelectClause then
    begin
      Sel := TSelectClause(Cl);
      Break;
    end;

  if Sel = nil then
  begin
    Sel := TSelectClause.Create;
    FClauses.Add(Sel);
  end;

  Idx := Length(Sel.Columns);
  SetLength(Sel.Columns, Idx + 1);
  Col.Column := Expression;
  Col.Alias  := '';
  Col.IsRaw  := True;
  Sel.Columns[Idx] := Col;
  Result := Self;
end;

// --- FROM ---

function TQueryImpl.From(const Table: string): IQuery;
begin
  Result := From(Table, '');
end;

function TQueryImpl.From(const Table, Alias: string): IQuery;
var
  From: TFromClause;
begin
  // Replace any existing FROM clause
  for var I := FClauses.Count - 1 downto 0 do
    if FClauses[I] is TFromClause then
      FClauses.Delete(I);

  From := TFromClause.Create;
  From.Table := Table;
  From.Alias := Alias;
  // Insert after SELECT (if any), otherwise at position 0
  var InsertAt := 0;
  for var I := 0 to FClauses.Count - 1 do
    if FClauses[I] is TSelectClause then
    begin
      InsertAt := I + 1;
      Break;
    end;
  FClauses.Insert(InsertAt, From);
  Result := Self;
end;

// --- WHERE ---

function TQueryImpl.Where(const Column: string; const Value: Variant): IQuery;
begin
  Result := AddWhereClause(Column, TWhereOp.Equal, Value, TBoolOp.opAnd);
end;

function TQueryImpl.Where(const Column, Op: string; const Value: Variant): IQuery;
begin
  Result := AddWhereClause(Column, ParseWhereOp(Op), Value, TBoolOp.opAnd);
end;

function TQueryImpl.OrWhere(const Column: string; const Value: Variant): IQuery;
begin
  Result := AddWhereClause(Column, TWhereOp.Equal, Value, TBoolOp.opOr);
end;

function TQueryImpl.OrWhere(const Column, Op: string; const Value: Variant): IQuery;
begin
  Result := AddWhereClause(Column, ParseWhereOp(Op), Value, TBoolOp.opOr);
end;

function TQueryImpl.WhereNull(const Column: string): IQuery;
begin
  Result := AddWhereClause(Column, TWhereOp.IsNull, Null, TBoolOp.opAnd);
end;

function TQueryImpl.WhereNotNull(const Column: string): IQuery;
begin
  Result := AddWhereClause(Column, TWhereOp.IsNotNull, Null, TBoolOp.opAnd);
end;

function TQueryImpl.WhereBetween(const Column: string; const Low, High: Variant): IQuery;
begin
  Result := AddWhereClause2(Column, TWhereOp.&Between, Low, High, TBoolOp.opAnd);
end;

function TQueryImpl.WhereRaw(const Sql: string): IQuery;
var
  W: TWhereClause;
begin
  W := TWhereClause.Create;
  W.Op        := TWhereOp.Raw;
  W.RawSql    := Sql;
  W.Connector := TBoolOp.opAnd;
  FClauses.Add(W);
  Result := Self;
end;

// --- ORDER BY ---

function TQueryImpl.OrderBy(const Column: string): IQuery;
var
  O: TOrderByClause;
begin
  O := TOrderByClause.Create;
  O.Column    := Column;
  O.Direction := TSortDir.Asc;
  O.IsRaw     := False;
  FClauses.Add(O);
  Result := Self;
end;

function TQueryImpl.OrderByDesc(const Column: string): IQuery;
var
  O: TOrderByClause;
begin
  O := TOrderByClause.Create;
  O.Column    := Column;
  O.Direction := TSortDir.Desc;
  O.IsRaw     := False;
  FClauses.Add(O);
  Result := Self;
end;

function TQueryImpl.OrderByRaw(const Expression: string): IQuery;
var
  O: TOrderByClause;
begin
  O := TOrderByClause.Create;
  O.Column := Expression;
  O.IsRaw  := True;
  FClauses.Add(O);
  Result := Self;
end;

// --- LIMIT / OFFSET ---

function TQueryImpl.Limit(const Value: Int64): IQuery;
var
  L: TLimitClause;
begin
  for var I := FClauses.Count - 1 downto 0 do
    if FClauses[I] is TLimitClause then
      FClauses.Delete(I);
  L := TLimitClause.Create;
  L.Value := Value;
  FClauses.Add(L);
  Result := Self;
end;

function TQueryImpl.Offset(const Value: Int64): IQuery;
var
  O: TOffsetClause;
begin
  for var I := FClauses.Count - 1 downto 0 do
    if FClauses[I] is TOffsetClause then
      FClauses.Delete(I);
  O := TOffsetClause.Create;
  O.Value := Value;
  FClauses.Add(O);
  Result := Self;
end;

// --- CLONE ---

function TQueryImpl.Clone: IQuery;
var
  Copy: TQueryImpl;
  Cl: TAbstractClause;
begin
  Copy := TQueryImpl.Create;
  for Cl in FClauses do
    Copy.FClauses.Add(Cl.Clone);
  Result := Copy;
end;

// --- COMPILE ---

function TQueryImpl.Compile(const Compiler: IQueryCompiler): TSQLResult;
begin
  Result := Compiler.Compile(Self);
end;

function TQueryImpl.Clauses: TArray<TAbstractClause>;
begin
  Result := FClauses.ToArray;
end;

// --- JOIN ---

function TQueryImpl.AddJoinClause(JoinType: TJoinType; const Table, Condition: string): IQuery;
var
  J: TJoinClause;
begin
  J := TJoinClause.Create;
  J.JoinType  := JoinType;
  J.Table     := Table;
  J.Condition := Condition;
  FClauses.Add(J);
  Result := Self;
end;

function TQueryImpl.Join(const Table, Col1, Col2: string): IQuery;
begin
  Result := AddJoinClause(TJoinType.Inner, Table, Col1 + ' = ' + Col2);
end;

function TQueryImpl.Join(const Table, Condition: string): IQuery;
begin
  Result := AddJoinClause(TJoinType.Inner, Table, Condition);
end;

function TQueryImpl.LeftJoin(const Table, Col1, Col2: string): IQuery;
begin
  Result := AddJoinClause(TJoinType.Left, Table, Col1 + ' = ' + Col2);
end;

function TQueryImpl.LeftJoin(const Table, Condition: string): IQuery;
begin
  Result := AddJoinClause(TJoinType.Left, Table, Condition);
end;

function TQueryImpl.RightJoin(const Table, Col1, Col2: string): IQuery;
begin
  Result := AddJoinClause(TJoinType.Right, Table, Col1 + ' = ' + Col2);
end;

function TQueryImpl.RightJoin(const Table, Condition: string): IQuery;
begin
  Result := AddJoinClause(TJoinType.Right, Table, Condition);
end;

function TQueryImpl.CrossJoin(const Table: string): IQuery;
begin
  Result := AddJoinClause(TJoinType.Cross, Table, '');
end;

function TQueryImpl.FullOuterJoin(const Table, Col1, Col2: string): IQuery;
begin
  Result := AddJoinClause(TJoinType.FullOuter, Table, Col1 + ' = ' + Col2);
end;

function TQueryImpl.FullOuterJoin(const Table, Condition: string): IQuery;
begin
  Result := AddJoinClause(TJoinType.FullOuter, Table, Condition);
end;

// --- DISTINCT ---

function TQueryImpl.Distinct: IQuery;
begin
  for var Cl in FClauses do
    if Cl is TDistinctClause then
      Exit(Self);
  FClauses.Add(TDistinctClause.Create);
  Result := Self;
end;

// --- FROM (subquery) ---

function TQueryImpl.From(const SubQuery: IQuery; const Alias: string): IQuery;
var
  FSQ: TFromSubqueryClause;
  InsertAt, I: Integer;
begin
  for I := FClauses.Count - 1 downto 0 do
    if (FClauses[I] is TFromClause) or (FClauses[I] is TFromSubqueryClause) then
      FClauses.Delete(I);
  FSQ := TFromSubqueryClause.Create;
  FSQ.SubQuery := SubQuery;
  FSQ.Alias    := Alias;
  InsertAt := 0;
  for I := 0 to FClauses.Count - 1 do
    if FClauses[I] is TSelectClause then
    begin
      InsertAt := I + 1;
      Break;
    end;
  FClauses.Insert(InsertAt, FSQ);
  Result := Self;
end;

// --- GROUP BY ---

function TQueryImpl.GroupBy(const Column: string): IQuery;
var
  G: TGroupByClause;
begin
  G := TGroupByClause.Create;
  G.Column := Column;
  G.IsRaw  := False;
  FClauses.Add(G);
  Result := Self;
end;

function TQueryImpl.GroupBy(const Columns: TArray<string>): IQuery;
var
  Col: string;
begin
  for Col in Columns do
    GroupBy(Col);
  Result := Self;
end;

function TQueryImpl.GroupByRaw(const Expression: string): IQuery;
var
  G: TGroupByClause;
begin
  G := TGroupByClause.Create;
  G.Column := Expression;
  G.IsRaw  := True;
  FClauses.Add(G);
  Result := Self;
end;

// --- HAVING ---

function TQueryImpl.AddHavingClause(const Column: string; Op: TWhereOp;
  const Value: Variant; Conn: TBoolOp): IQuery;
var
  H: THavingClause;
begin
  H := THavingClause.Create;
  H.Column    := Column;
  H.Op        := Op;
  H.Value     := Value;
  H.Connector := Conn;
  FClauses.Add(H);
  Result := Self;
end;

function TQueryImpl.Having(const Column, Op: string; const Value: Variant): IQuery;
begin
  Result := AddHavingClause(Column, ParseWhereOp(Op), Value, TBoolOp.opAnd);
end;

function TQueryImpl.HavingRaw(const Sql: string): IQuery;
var
  H: THavingClause;
begin
  H := THavingClause.Create;
  H.Op        := TWhereOp.Raw;
  H.RawSql    := Sql;
  H.Connector := TBoolOp.opAnd;
  FClauses.Add(H);
  Result := Self;
end;

// --- UNION / INTERSECT / EXCEPT ---

function TQueryImpl.AddUnionClause(Kind: TUnionKind; const Other: IQuery): IQuery;
var
  U: TUnionClause;
begin
  U := TUnionClause.Create;
  U.Kind     := Kind;
  U.SubQuery := Other;
  FClauses.Add(U);
  Result := Self;
end;

function TQueryImpl.Union(const Other: IQuery): IQuery;
begin
  Result := AddUnionClause(TUnionKind.Union, Other);
end;

function TQueryImpl.UnionAll(const Other: IQuery): IQuery;
begin
  Result := AddUnionClause(TUnionKind.UnionAll, Other);
end;

function TQueryImpl.Intersect(const Other: IQuery): IQuery;
begin
  Result := AddUnionClause(TUnionKind.Intersect, Other);
end;

function TQueryImpl.&Except(const Other: IQuery): IQuery;
begin
  Result := AddUnionClause(TUnionKind.&Except, Other);
end;

// --- WHERE (subquery variants) ---

function TQueryImpl.WhereExists(const SubQuery: IQuery): IQuery;
var
  W: TWhereClause;
begin
  W := TWhereClause.Create;
  W.Op        := TWhereOp.&Exists;
  W.SubQuery  := SubQuery;
  W.Connector := TBoolOp.opAnd;
  FClauses.Add(W);
  Result := Self;
end;

function TQueryImpl.WhereNotExists(const SubQuery: IQuery): IQuery;
var
  W: TWhereClause;
begin
  W := TWhereClause.Create;
  W.Op        := TWhereOp.NotExists;
  W.SubQuery  := SubQuery;
  W.Connector := TBoolOp.opAnd;
  FClauses.Add(W);
  Result := Self;
end;

function TQueryImpl.WhereInQuery(const Column: string; const SubQuery: IQuery): IQuery;
var
  W: TWhereClause;
begin
  W := TWhereClause.Create;
  W.Column    := Column;
  W.Op        := TWhereOp.&In;
  W.SubQuery  := SubQuery;
  W.Connector := TBoolOp.opAnd;
  FClauses.Add(W);
  Result := Self;
end;

// --- SELECT (aliases + aggregates) ---

procedure TQueryImpl.SelectAggregateRaw(const AggFunc, Column, Alias: string);
begin
  SelectRaw(AggFunc + '(' + Column + ') AS ' + Alias);
end;

function TQueryImpl.SelectAs(const Column, Alias: string): IQuery;
var
  Sel: TSelectClause;
  Col: TSelectColumn;
  Idx: Integer;
begin
  Sel := nil;
  for var Cl in FClauses do
    if Cl is TSelectClause then
    begin
      Sel := TSelectClause(Cl);
      Break;
    end;
  if Sel = nil then
  begin
    Sel := TSelectClause.Create;
    FClauses.Add(Sel);
  end;
  Idx := Length(Sel.Columns);
  SetLength(Sel.Columns, Idx + 1);
  Col.Column := Column;
  Col.Alias  := Alias;
  Col.IsRaw  := False;
  Sel.Columns[Idx] := Col;
  Result := Self;
end;

function TQueryImpl.SelectCount(const Column: string = '*'; const Alias: string = 'count'): IQuery;
begin
  SelectAggregateRaw('COUNT', Column, Alias);
  Result := Self;
end;

function TQueryImpl.SelectSum(const Column: string; const Alias: string = 'sum'): IQuery;
begin
  SelectAggregateRaw('SUM', Column, Alias);
  Result := Self;
end;

function TQueryImpl.SelectAvg(const Column: string; const Alias: string = 'avg'): IQuery;
begin
  SelectAggregateRaw('AVG', Column, Alias);
  Result := Self;
end;

function TQueryImpl.SelectMin(const Column: string; const Alias: string = 'min'): IQuery;
begin
  SelectAggregateRaw('MIN', Column, Alias);
  Result := Self;
end;

function TQueryImpl.SelectMax(const Column: string; const Alias: string = 'max'): IQuery;
begin
  SelectAggregateRaw('MAX', Column, Alias);
  Result := Self;
end;

// --- WITH (CTE) ---

function TQueryImpl.AddWithClause(const Name: string; const SubQuery: IQuery;
  IsRecursive: Boolean): IQuery;
var
  W: TWithClause;
begin
  W := TWithClause.Create;
  W.Name        := Name;
  W.SubQuery    := SubQuery;
  W.IsRecursive := IsRecursive;
  FClauses.Add(W);
  Result := Self;
end;

function TQueryImpl.&With(const Name: string; const SubQuery: IQuery): IQuery;
begin
  Result := AddWithClause(Name, SubQuery, False);
end;

function TQueryImpl.WithRecursive(const Name: string; const SubQuery: IQuery): IQuery;
begin
  Result := AddWithClause(Name, SubQuery, True);
end;

{ TQuery }

class function TQuery.New: IQuery;
begin
  Result := TQueryImpl.Create;
end;

end.
