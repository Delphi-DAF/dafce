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
               Like, NotLike, &In, NotIn, IsNull, IsNotNull, &Between,
               &Exists, NotExists, Raw);

  /// <summary>
  /// Boolean connector joining WHERE clauses.
  /// </summary>
  TBoolOp = (opAnd, opOr);

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
  C.IsNot     := IsNot;
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

end.
