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
  end;

{ TSQLResult }

class function TSQLResult.Empty: TSQLResult;
begin
  Result.SQL      := '';
  Result.Bindings := [];
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

{ TQuery }

class function TQuery.New: IQuery;
begin
  Result := TQueryImpl.Create;
end;

end.
