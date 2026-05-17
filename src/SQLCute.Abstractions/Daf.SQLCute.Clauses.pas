unit Daf.SQLCute.Clauses;

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils,
  System.Variants,
  System.Generics.Collections;

type
  /// <summary>
  /// Operator for a WHERE condition.
  /// </summary>
  TWhereOp = (Equal, NotEqual, Less, LessOrEqual, Greater, GreaterOrEqual,
               Like, NotLike, &In, NotIn, IsNull, IsNotNull, &Between, NotBetween,
               &Exists, NotExists, Raw);

  /// <summary>
  /// Boolean connector joining WHERE clauses.
  /// </summary>
  TBoolOp = (opAnd, opOr);

  /// <summary>
  /// The date/time component to extract in a date-filter WHERE condition.
  /// </summary>
  TDatePart = (dpDate, dpTime, dpYear, dpMonth, dpDay, dpHour, dpMinute);

  /// <summary>
  /// Direction for ORDER BY clauses.
  /// </summary>
  TSortDir = (Asc, Desc);

  /// <summary>
  /// Kind of JOIN clause.
  /// </summary>
  TJoinType = (Inner, Left, Right, Cross, FullOuter);

  // ---------------------------------------------------------------------------
  //  Abstract base
  // ---------------------------------------------------------------------------

  /// <summary>
  /// Base for all AST clause nodes. Sealed hierarchy — only the types defined
  /// in this unit are valid nodes.
  /// </summary>
  TAbstractClause = class abstract
  public
    /// <summary>Clone this node for query forking.</summary>
    function Clone: TAbstractClause; virtual; abstract;
  end;

  // ---------------------------------------------------------------------------
  //  SELECT
  // ---------------------------------------------------------------------------

  /// <summary>
  /// A single column expression in the SELECT list.
  /// </summary>
  TSelectColumn = record
    /// Column name, expression or '*'.
    Column: string;
    /// Optional alias (AS …). Empty string = no alias.
    Alias: string;
    /// When True the entire Column value is emitted verbatim (raw SQL).
    IsRaw: Boolean;
    /// When non-nil, this column is a subquery expression: (SELECT …) AS alias.
    SubQuery: IInterface;
  end;

  /// <summary>
  /// Holds the list of columns/expressions for the SELECT clause.
  /// </summary>
  TSelectClause = class(TAbstractClause)
  public
    Columns: TArray<TSelectColumn>;
    function Clone: TAbstractClause; override;
  end;

  // ---------------------------------------------------------------------------
  //  FROM
  // ---------------------------------------------------------------------------

  /// <summary>
  /// The main FROM table or sub-query alias.
  /// </summary>
  TFromClause = class(TAbstractClause)
  public
    /// Table name or sub-query alias.
    Table: string;
    /// Optional schema prefix.
    Schema: string;
    /// Optional alias.
    Alias: string;
    function Clone: TAbstractClause; override;
  end;

  /// <summary>
  /// FROM with a raw SQL expression (table function, lateral, etc.).
  /// Bindings are prepended to the query binding list (FROM precedes WHERE).
  /// </summary>
  TFromRawClause = class(TAbstractClause)
  public
    RawSql: string;
    Bindings: TArray<Variant>;
    Alias: string;
    function Clone: TAbstractClause; override;
  end;

  // ---------------------------------------------------------------------------
  //  WHERE
  // ---------------------------------------------------------------------------

  /// <summary>
  /// A single WHERE condition node.
  /// </summary>
  TWhereClause = class(TAbstractClause)
  public
    Column: string;
    Op: TWhereOp;
    /// Primary value (op-dependent). Null for IS NULL / EXISTS.
    Value: Variant;
    /// Second value for BETWEEN.
    Value2: Variant;
    /// Raw SQL fragment (when Op = Raw).
    RawSql: string;
    /// AND / OR connector with the *previous* condition.
    Connector: TBoolOp;
    /// When True the condition is wrapped in NOT(…).
    IsNot: Boolean;
    /// Sub-query for EXISTS / IN-subquery conditions (IInterface to avoid
    /// circular dependency with IQuery). The compiler casts to IQuery.
    SubQuery: IInterface;
    /// When True, Value holds a column name and the condition is emitted as
    /// col1 op col2 (no parameter binding). Used by WhereColumns.
    IsColumnValue: Boolean;
    /// When True (and Op = Like/NotLike), the compiler emits a case-sensitive LIKE.
    /// When False (default), the compiler wraps the column in LOWER() and
    /// lower-cases the binding value to achieve case-insensitive matching.
    CaseSensitive: Boolean;
    function Clone: TAbstractClause; override;
  end;

  // ---------------------------------------------------------------------------
  //  ORDER BY
  // ---------------------------------------------------------------------------

  TOrderByClause = class(TAbstractClause)
  public
    Column: string;
    Direction: TSortDir;
    IsRaw: Boolean;
    function Clone: TAbstractClause; override;
  end;

  // ---------------------------------------------------------------------------
  //  LIMIT / OFFSET
  // ---------------------------------------------------------------------------

  TLimitClause = class(TAbstractClause)
  public
    Value: Int64;
    function Clone: TAbstractClause; override;
  end;

  TOffsetClause = class(TAbstractClause)
  public
    Value: Int64;
    function Clone: TAbstractClause; override;
  end;

  // ---------------------------------------------------------------------------
  //  RAW (arbitrary SQL fragment with bindings)
  // ---------------------------------------------------------------------------

  TRawClause = class(TAbstractClause)
  public
    Expression: string;
    Bindings: TArray<Variant>;
    function Clone: TAbstractClause; override;
  end;

  // ---------------------------------------------------------------------------
  //  JOIN
  // ---------------------------------------------------------------------------

  TJoinClause = class(TAbstractClause)
  public
    JoinType: TJoinType;
    Table: string;
    Schema: string;
    Alias: string;
    /// Raw ON condition, e.g. 'users.id = orders.user_id'.
    /// Used when the JOIN was specified with a raw string condition.
    Condition: string;
    /// Column-based ON: left column, operator, right column.
    /// When Col1 is non-empty these three fields take precedence over Condition.
    Col1: string;
    Op: string;
    Col2: string;
    /// Callback-specified ON conditions. When non-empty, takes precedence
    /// over Condition and Col1/Col2.
    ConditionClauses: TArray<TAbstractClause>;
    destructor Destroy; override;
    function Clone: TAbstractClause; override;
  end;

  /// <summary>
  /// JOIN against a subquery, with alias and callback-defined ON conditions.
  /// </summary>
  TJoinSubqueryClause = class(TAbstractClause)
  public
    JoinType: TJoinType;
    /// Sub-query body (IInterface to avoid circular dependency with IQuery).
    SubQuery: IInterface;
    Alias: string;
    ConditionClauses: TArray<TAbstractClause>;
    destructor Destroy; override;
    function Clone: TAbstractClause; override;
  end;

  // ---------------------------------------------------------------------------
  //  GROUP BY
  // ---------------------------------------------------------------------------

  TGroupByClause = class(TAbstractClause)
  public
    Column: string;
    IsRaw: Boolean;
    function Clone: TAbstractClause; override;
  end;

  // ---------------------------------------------------------------------------
  //  HAVING
  // ---------------------------------------------------------------------------

  THavingClause = class(TAbstractClause)
  public
    Column: string;
    Op: TWhereOp;
    Value: Variant;
    RawSql: string;
    Connector: TBoolOp;
    function Clone: TAbstractClause; override;
  end;

  // ---------------------------------------------------------------------------
  //  UNION / INTERSECT / EXCEPT
  // ---------------------------------------------------------------------------

  TUnionKind = (Union, UnionAll, Intersect, &Except, IntersectAll, ExceptAll);

  TUnionClause = class(TAbstractClause)
  public
    Kind: TUnionKind;
    /// Sub-query body (IInterface to avoid circular dependency with IQuery).
    /// The compiler casts this to IQuery at compile time.
    SubQuery: IInterface;
    /// Raw SQL fragment for CombineRaw. When non-empty, emitted verbatim
    /// instead of building from Kind + SubQuery.
    RawSql: string;
    function Clone: TAbstractClause; override;
  end;

  // ---------------------------------------------------------------------------
  //  WITH (Common Table Expression)
  // ---------------------------------------------------------------------------

  TWithClause = class(TAbstractClause)
  public
    Name: string;
    IsRecursive: Boolean;
    /// CTE body (IInterface to avoid circular dependency with IQuery).
    SubQuery: IInterface;
    /// When True, use RawSql + RawBindings instead of SubQuery.
    IsRawSql: Boolean;
    RawSql: string;
    RawBindings: TArray<Variant>;
    function Clone: TAbstractClause; override;
  end;

  // ---------------------------------------------------------------------------
  //  DISTINCT (marker — no data fields)
  // ---------------------------------------------------------------------------

  TDistinctClause = class(TAbstractClause)
  public
    function Clone: TAbstractClause; override;
  end;

  // ---------------------------------------------------------------------------
  //  INSERT
  // ---------------------------------------------------------------------------

  TInsertClause = class(TAbstractClause)
  public
    Columns: TArray<string>;
    /// Each element is one row of VALUES. Empty = use SubQuery mode.
    Rows: TArray<TArray<Variant>>;
    /// Sub-query for INSERT … SELECT (IInterface to avoid circular dep with IQuery).
    SubQuery: IInterface;
    function Clone: TAbstractClause; override;
  end;

  // ---------------------------------------------------------------------------
  //  UPDATE SET  (one instance per column–value assignment)
  // ---------------------------------------------------------------------------

  TUpdateSetClause = class(TAbstractClause)
  public
    Column: string;
    Value: Variant;
    IsRaw: Boolean;
    RawSql: string;
    function Clone: TAbstractClause; override;
  end;

  // ---------------------------------------------------------------------------
  //  DELETE (marker — table comes from TFromClause)
  // ---------------------------------------------------------------------------

  TDeleteClause = class(TAbstractClause)
  public
    function Clone: TAbstractClause; override;
  end;

  // ---------------------------------------------------------------------------
  //  NESTED WHERE GROUP  (parenthesized group of WHERE sub-conditions)
  // ---------------------------------------------------------------------------

  /// <summary>
  /// A parenthesized group of WHERE clauses, produced by Where(callback) /
  /// OrWhere(callback). SubClauses holds the inner conditions; Connector
  /// controls how this group connects with the preceding sibling.
  /// IsNot is reserved for future NOT (...) support.
  /// </summary>
  TNestedWhereClause = class(TAbstractClause)
  public
    SubClauses: TArray<TAbstractClause>;
    Connector:  TBoolOp;
    IsNot:      Boolean;
    destructor Destroy; override;
    function Clone: TAbstractClause; override;
  end;

  // ---------------------------------------------------------------------------
  //  WHERE IN / NOT IN  (one instance per WhereIn/WhereNotIn call)
  // ---------------------------------------------------------------------------

  TWhereInClause = class(TAbstractClause)
  public
    Column:    string;
    Values:    TArray<Variant>;
    Negated:   Boolean;   // True = NOT IN
    Connector: string;    // 'AND' or 'OR'
    function Clone: TAbstractClause; override;
  end;

  // ---------------------------------------------------------------------------
  //  DATE WHERE  (date/time portion filter)
  // ---------------------------------------------------------------------------

  /// <summary>
  /// A WHERE condition that filters on a specific date or time portion.
  /// The exact SQL fragment is dialect-specific; the base ANSI compiler
  /// emits <c>col op ?</c> as a fallback.
  /// </summary>
  TDateWhereClause = class(TAbstractClause)
  public
    DatePart:  TDatePart;
    Column:    string;
    Op:        string;     // '=', '>', '<', etc.
    Value:     Variant;
    Connector: TBoolOp;
    function Clone: TAbstractClause; override;
  end;

implementation

{ TSelectClause }

function TSelectClause.Clone: TAbstractClause;
var
  C: TSelectClause;
begin
  C := TSelectClause.Create;
  C.Columns := Copy(Columns);
  Result := C;
end;

{ TFromClause }

function TFromClause.Clone: TAbstractClause;
var
  C: TFromClause;
begin
  C := TFromClause.Create;
  C.Table  := Table;
  C.Schema := Schema;
  C.Alias  := Alias;
  Result := C;
end;

{ TFromRawClause }

function TFromRawClause.Clone: TAbstractClause;
var
  C: TFromRawClause;
begin
  C := TFromRawClause.Create;
  C.RawSql   := RawSql;
  C.Bindings := Copy(Bindings);
  C.Alias    := Alias;
  Result := C;
end;

{ TWhereClause }

function TWhereClause.Clone: TAbstractClause;
var
  C: TWhereClause;
begin
  C := TWhereClause.Create;
  C.Column    := Column;
  C.Op        := Op;
  C.Value     := Value;
  C.Value2    := Value2;
  C.RawSql    := RawSql;
  C.Connector := Connector;
  C.IsNot         := IsNot;
  C.SubQuery      := SubQuery;
  C.IsColumnValue := IsColumnValue;
  C.CaseSensitive := CaseSensitive;
  Result := C;
end;

{ TOrderByClause }

function TOrderByClause.Clone: TAbstractClause;
var
  C: TOrderByClause;
begin
  C := TOrderByClause.Create;
  C.Column    := Column;
  C.Direction := Direction;
  C.IsRaw     := IsRaw;
  Result := C;
end;

{ TLimitClause }

function TLimitClause.Clone: TAbstractClause;
var
  C: TLimitClause;
begin
  C := TLimitClause.Create;
  C.Value := Value;
  Result := C;
end;

{ TOffsetClause }

function TOffsetClause.Clone: TAbstractClause;
var
  C: TOffsetClause;
begin
  C := TOffsetClause.Create;
  C.Value := Value;
  Result := C;
end;

{ TRawClause }

function TRawClause.Clone: TAbstractClause;
var
  C: TRawClause;
begin
  C := TRawClause.Create;
  C.Expression := Expression;
  C.Bindings   := Copy(Bindings);
  Result := C;
end;

{ TJoinClause }

destructor TJoinClause.Destroy;
var
  C: TAbstractClause;
begin
  for C in ConditionClauses do
    C.Free;
  inherited;
end;

function TJoinClause.Clone: TAbstractClause;
var
  C: TJoinClause;
  I: Integer;
begin
  C := TJoinClause.Create;
  C.JoinType  := JoinType;
  C.Table     := Table;
  C.Schema    := Schema;
  C.Alias     := Alias;
  C.Condition := Condition;
  C.Col1      := Col1;
  C.Op        := Op;
  C.Col2      := Col2;
  SetLength(C.ConditionClauses, Length(ConditionClauses));
  for I := 0 to High(ConditionClauses) do
    C.ConditionClauses[I] := ConditionClauses[I].Clone;
  Result := C;
end;

{ TJoinSubqueryClause }

destructor TJoinSubqueryClause.Destroy;
var
  C: TAbstractClause;
begin
  for C in ConditionClauses do
    C.Free;
  inherited;
end;

function TJoinSubqueryClause.Clone: TAbstractClause;
var
  C: TJoinSubqueryClause;
  I: Integer;
begin
  C := TJoinSubqueryClause.Create;
  C.JoinType := JoinType;
  C.SubQuery := SubQuery;
  C.Alias    := Alias;
  SetLength(C.ConditionClauses, Length(ConditionClauses));
  for I := 0 to High(ConditionClauses) do
    C.ConditionClauses[I] := ConditionClauses[I].Clone;
  Result := C;
end;

{ TGroupByClause }

function TGroupByClause.Clone: TAbstractClause;
var
  C: TGroupByClause;
begin
  C := TGroupByClause.Create;
  C.Column := Column;
  C.IsRaw  := IsRaw;
  Result := C;
end;

{ THavingClause }

function THavingClause.Clone: TAbstractClause;
var
  C: THavingClause;
begin
  C := THavingClause.Create;
  C.Column    := Column;
  C.Op        := Op;
  C.Value     := Value;
  C.RawSql    := RawSql;
  C.Connector := Connector;
  Result := C;
end;

{ TUnionClause }

function TUnionClause.Clone: TAbstractClause;
var
  C: TUnionClause;
begin
  C := TUnionClause.Create;
  C.Kind     := Kind;
  C.SubQuery := SubQuery;
  C.RawSql   := RawSql;
  Result := C;
end;

{ TWithClause }

function TWithClause.Clone: TAbstractClause;
var
  C: TWithClause;
begin
  C := TWithClause.Create;
  C.Name        := Name;
  C.IsRecursive := IsRecursive;
  C.SubQuery    := SubQuery;
  C.IsRawSql    := IsRawSql;
  C.RawSql      := RawSql;
  C.RawBindings := Copy(RawBindings);
  Result := C;
end;

{ TDistinctClause }

function TDistinctClause.Clone: TAbstractClause;
begin
  Result := TDistinctClause.Create;
end;

{ TInsertClause }

function TInsertClause.Clone: TAbstractClause;
var
  C: TInsertClause;
  I: Integer;
begin
  C := TInsertClause.Create;
  C.Columns  := Copy(Columns);
  C.SubQuery := SubQuery;
  SetLength(C.Rows, Length(Rows));
  for I := 0 to High(Rows) do
    C.Rows[I] := Copy(Rows[I]);
  Result := C;
end;

{ TUpdateSetClause }

function TUpdateSetClause.Clone: TAbstractClause;
var
  C: TUpdateSetClause;
begin
  C := TUpdateSetClause.Create;
  C.Column := Column;
  C.Value  := Value;
  C.IsRaw  := IsRaw;
  C.RawSql := RawSql;
  Result := C;
end;

{ TDeleteClause }

function TDeleteClause.Clone: TAbstractClause;
begin
  Result := TDeleteClause.Create;
end;

{ TWhereInClause }

function TWhereInClause.Clone: TAbstractClause;
var
  C: TWhereInClause;
begin
  C           := TWhereInClause.Create;
  C.Column    := Column;
  C.Values    := Copy(Values);
  C.Negated   := Negated;
  C.Connector := Connector;
  Result := C;
end;

{ TDateWhereClause }

function TDateWhereClause.Clone: TAbstractClause;
var
  C: TDateWhereClause;
begin
  C           := TDateWhereClause.Create;
  C.DatePart  := DatePart;
  C.Column    := Column;
  C.Op        := Op;
  C.Value     := Value;
  C.Connector := Connector;
  Result := C;
end;

{ TNestedWhereClause }

destructor TNestedWhereClause.Destroy;
var
  Sub: TAbstractClause;
begin
  for Sub in SubClauses do
    Sub.Free;
  inherited;
end;

function TNestedWhereClause.Clone: TAbstractClause;
var
  C: TNestedWhereClause;
  I: Integer;
begin
  C := TNestedWhereClause.Create;
  C.Connector := Connector;
  C.IsNot     := IsNot;
  SetLength(C.SubClauses, Length(SubClauses));
  for I := 0 to High(SubClauses) do
    C.SubClauses[I] := SubClauses[I].Clone;
  Result := C;
end;

end.
