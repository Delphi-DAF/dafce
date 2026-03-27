unit Daf.SQLCute.Compiler;

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils,
  System.Variants,
  System.Generics.Collections,
  Daf.SQLCute,
  Daf.SQLCute.Clauses;

type
  /// <summary>
  /// ANSI SQL compiler — baseline implementation of IQueryCompiler.
  /// Produces standard SQL with '?' positional parameters.
  /// All dialect compilers extend this class and override virtual methods.
  /// </summary>
  TAnsiSqlCompiler = class(TInterfacedObject, IQueryCompiler)
  private
    FBindings: TList<Variant>;
    procedure AddBinding(const Value: Variant);
    function CompileSubQuery(const Query: IQuery): string;
  protected
    function WrapColumn(const Col: string): string; virtual;
    function WrapTable(const Table: string): string; virtual;
    function ParamPlaceholder: string; virtual;
    function OperatorSymbol(Op: TWhereOp): string; virtual;
    function CompileSelect(const Clauses: TArray<TAbstractClause>): string; virtual;
    function CompileFrom(const Clauses: TArray<TAbstractClause>): string; virtual;
    function CompileJoin(const Clauses: TArray<TAbstractClause>): string; virtual;
    function CompileWhere(const Clauses: TArray<TAbstractClause>): string; virtual;
    function CompileGroupBy(const Clauses: TArray<TAbstractClause>): string; virtual;
    function CompileHaving(const Clauses: TArray<TAbstractClause>): string; virtual;
    function CompileOrderBy(const Clauses: TArray<TAbstractClause>): string; virtual;
    function CompileLimit(const Clauses: TArray<TAbstractClause>): string; virtual;
    function CompileOffset(const Clauses: TArray<TAbstractClause>): string; virtual;
    function CompileWith(const Clauses: TArray<TAbstractClause>): string; virtual;
    function CompileUnion(const Clauses: TArray<TAbstractClause>): string; virtual;
    function AssembleQuery(const Parts: TArray<string>): string; virtual;
  public
    function Compile(const Query: IQuery): TSQLResult;
  end;

implementation

{ TAnsiSqlCompiler }

procedure TAnsiSqlCompiler.AddBinding(const Value: Variant);
begin
  FBindings.Add(Value);
end;

function TAnsiSqlCompiler.WrapColumn(const Col: string): string;
begin
  // ANSI: no quoting for plain identifiers.
  // Sub-classes override (e.g. MySQL uses backticks, MSSQL uses brackets).
  Result := Col;
end;

function TAnsiSqlCompiler.WrapTable(const Table: string): string;
begin
  Result := Table;
end;

function TAnsiSqlCompiler.ParamPlaceholder: string;
begin
  Result := '?';
end;

function TAnsiSqlCompiler.OperatorSymbol(Op: TWhereOp): string;
begin
  case Op of
    TWhereOp.Equal:          Result := '=';
    TWhereOp.NotEqual:       Result := '<>';
    TWhereOp.Less:           Result := '<';
    TWhereOp.LessOrEqual:    Result := '<=';
    TWhereOp.Greater:        Result := '>';
    TWhereOp.GreaterOrEqual: Result := '>=';
    TWhereOp.Like:           Result := 'LIKE';
    TWhereOp.NotLike:        Result := 'NOT LIKE';
  else
    Result := '=';
  end;
end;

function TAnsiSqlCompiler.CompileSelect(const Clauses: TArray<TAbstractClause>): string;
var
  Parts: TList<string>;
  I: Integer;
  Clause: TAbstractClause;
  Sel: TSelectClause;
  Col: TSelectColumn;
  Expr: string;
  IsDistinct: Boolean;
begin
  IsDistinct := False;
  for Clause in Clauses do
    if Clause is TDistinctClause then
    begin
      IsDistinct := True;
      Break;
    end;

  Parts := TList<string>.Create;
  try
    for Clause in Clauses do
      if Clause is TSelectClause then
      begin
        Sel := TSelectClause(Clause);
        for Col in Sel.Columns do
        begin
          if Col.IsRaw then
            Expr := Col.Column
          else if Col.Column = '*' then
            Expr := '*'
          else
            Expr := WrapColumn(Col.Column);

          if Col.Alias <> '' then
            Expr := Expr + ' AS ' + WrapColumn(Col.Alias);

          Parts.Add(Expr);
        end;
      end;

    var Prefix: string;
    if IsDistinct then
      Prefix := 'SELECT DISTINCT '
    else
      Prefix := 'SELECT ';

    if Parts.Count = 0 then
      Result := Prefix + '*'
    else
    begin
      Result := Prefix;
      for I := 0 to Parts.Count - 1 do
      begin
        if I > 0 then Result := Result + ', ';
        Result := Result + Parts[I];
      end;
    end;
  finally
    Parts.Free;
  end;
end;

function TAnsiSqlCompiler.CompileFrom(const Clauses: TArray<TAbstractClause>): string;
var
  Clause: TAbstractClause;
  From: TFromClause;
  FSQ: TFromSubqueryClause;
  TableExpr: string;
begin
  Result := '';
  for Clause in Clauses do
  begin
    if Clause is TFromSubqueryClause then
    begin
      FSQ := TFromSubqueryClause(Clause);
      Result := 'FROM (' + CompileSubQuery(FSQ.SubQuery) + ') ' + WrapTable(FSQ.Alias);
      Exit;
    end;

    if Clause is TFromClause then
    begin
      From := TFromClause(Clause);
      if From.Schema <> '' then
        TableExpr := WrapTable(From.Schema) + '.' + WrapTable(From.Table)
      else
        TableExpr := WrapTable(From.Table);
      if From.Alias <> '' then
        TableExpr := TableExpr + ' AS ' + WrapTable(From.Alias);
      Result := 'FROM ' + TableExpr;
      Exit;
    end;
  end;
end;

function TAnsiSqlCompiler.CompileWhere(const Clauses: TArray<TAbstractClause>): string;
var
  Sb: TStringBuilder;
  Clause: TAbstractClause;
  W: TWhereClause;
  First: Boolean;
  Expr: string;
begin
  Sb := TStringBuilder.Create;
  try
    First := True;
    for Clause in Clauses do
    begin
      if not (Clause is TWhereClause) then Continue;
      W := TWhereClause(Clause);

      if First then
        Sb.Append('WHERE ')
      else
        case W.Connector of
          TBoolOp.opAnd: Sb.Append(' AND ');
          TBoolOp.opOr:  Sb.Append(' OR ');
        end;
      First := False;

      case W.Op of
        TWhereOp.IsNull:
          Expr := WrapColumn(W.Column) + ' IS NULL';

        TWhereOp.IsNotNull:
          Expr := WrapColumn(W.Column) + ' IS NOT NULL';

        TWhereOp.&Exists:
          Expr := 'EXISTS (' + CompileSubQuery(W.SubQuery as IQuery) + ')';

        TWhereOp.NotExists:
          Expr := 'NOT EXISTS (' + CompileSubQuery(W.SubQuery as IQuery) + ')';

        TWhereOp.&Between:
        begin
          AddBinding(W.Value);
          AddBinding(W.Value2);
          Expr := WrapColumn(W.Column) + ' BETWEEN ' + ParamPlaceholder + ' AND ' + ParamPlaceholder;
        end;

        TWhereOp.&In:
        begin
          if W.SubQuery <> nil then
            Expr := WrapColumn(W.Column) + ' IN (' + CompileSubQuery(W.SubQuery as IQuery) + ')'
          else
            Expr := WrapColumn(W.Column) + ' IN (' + VarToStr(W.Value) + ')';
        end;

        TWhereOp.NotIn:
        begin
          if W.SubQuery <> nil then
            Expr := WrapColumn(W.Column) + ' NOT IN (' + CompileSubQuery(W.SubQuery as IQuery) + ')'
          else
            Expr := WrapColumn(W.Column) + ' NOT IN (' + VarToStr(W.Value) + ')';
        end;

        TWhereOp.Raw:
          Expr := W.RawSql;

        else
        begin
          AddBinding(W.Value);
          Expr := WrapColumn(W.Column) + ' ' + OperatorSymbol(W.Op) + ' ' + ParamPlaceholder;
        end;
      end;

      if W.IsNot then
        Expr := 'NOT (' + Expr + ')';

      Sb.Append(Expr);
    end;

    Result := Sb.ToString;
  finally
    Sb.Free;
  end;
end;

function TAnsiSqlCompiler.CompileOrderBy(const Clauses: TArray<TAbstractClause>): string;
var
  Parts: TList<string>;
  Clause: TAbstractClause;
  O: TOrderByClause;
  Expr: string;
  I: Integer;
begin
  Result := '';
  Parts := TList<string>.Create;
  try
    for Clause in Clauses do
      if Clause is TOrderByClause then
      begin
        O := TOrderByClause(Clause);
        if O.IsRaw then
          Expr := O.Column
        else
        begin
          Expr := WrapColumn(O.Column);
          case O.Direction of
            TSortDir.Asc:  Expr := Expr + ' ASC';
            TSortDir.Desc: Expr := Expr + ' DESC';
          end;
        end;
        Parts.Add(Expr);
      end;

    if Parts.Count > 0 then
    begin
      Result := 'ORDER BY ';
      for I := 0 to Parts.Count - 1 do
      begin
        if I > 0 then Result := Result + ', ';
        Result := Result + Parts[I];
      end;
    end;
  finally
    Parts.Free;
  end;
end;

function TAnsiSqlCompiler.CompileLimit(const Clauses: TArray<TAbstractClause>): string;
var
  Clause: TAbstractClause;
begin
  Result := '';
  for Clause in Clauses do
    if Clause is TLimitClause then
    begin
      Result := 'LIMIT ' + IntToStr(TLimitClause(Clause).Value);
      Exit;
    end;
end;

function TAnsiSqlCompiler.CompileOffset(const Clauses: TArray<TAbstractClause>): string;
var
  Clause: TAbstractClause;
begin
  Result := '';
  for Clause in Clauses do
    if Clause is TOffsetClause then
    begin
      Result := 'OFFSET ' + IntToStr(TOffsetClause(Clause).Value);
      Exit;
    end;
end;

function TAnsiSqlCompiler.AssembleQuery(const Parts: TArray<string>): string;
var
  Parts2: TList<string>;
  Part: string;
begin
  Parts2 := TList<string>.Create;
  try
    for Part in Parts do
      if Part <> '' then
        Parts2.Add(Part);

    Result := '';
    for var I := 0 to Parts2.Count - 1 do
    begin
      if I > 0 then Result := Result + ' ';
      Result := Result + Parts2[I];
    end;
  finally
    Parts2.Free;
  end;
end;

function TAnsiSqlCompiler.Compile(const Query: IQuery): TSQLResult;
var
  Clauses: TArray<TAbstractClause>;
  Parts: TArray<string>;
  WithPart, UnionPart, MainSQL: string;
begin
  FBindings := TList<Variant>.Create;
  try
    Clauses := Query.Clauses;

    WithPart := CompileWith(Clauses);

    SetLength(Parts, 9);
    Parts[0] := CompileSelect(Clauses);
    Parts[1] := CompileFrom(Clauses);
    Parts[2] := CompileJoin(Clauses);
    Parts[3] := CompileWhere(Clauses);
    Parts[4] := CompileGroupBy(Clauses);
    Parts[5] := CompileHaving(Clauses);
    Parts[6] := CompileOrderBy(Clauses);
    Parts[7] := CompileLimit(Clauses);
    Parts[8] := CompileOffset(Clauses);

    UnionPart := CompileUnion(Clauses);
    MainSQL   := AssembleQuery(Parts);

    if WithPart <> '' then
      Result.SQL := WithPart + ' ' + MainSQL
    else
      Result.SQL := MainSQL;

    if UnionPart <> '' then
      Result.SQL := Result.SQL + ' ' + UnionPart;

    Result.Bindings := FBindings.ToArray;
  finally
    FBindings.Free;
    FBindings := nil;
  end;
end;

// ---------------------------------------------------------------------------
//  CompileSubQuery — recursive compilation reusing Self.FBindings
// ---------------------------------------------------------------------------

function TAnsiSqlCompiler.CompileSubQuery(const Query: IQuery): string;
var
  Clauses: TArray<TAbstractClause>;
  Parts: TArray<string>;
  WithPart, UnionPart: string;
begin
  Clauses := Query.Clauses;

  WithPart := CompileWith(Clauses);

  SetLength(Parts, 9);
  Parts[0] := CompileSelect(Clauses);
  Parts[1] := CompileFrom(Clauses);
  Parts[2] := CompileJoin(Clauses);
  Parts[3] := CompileWhere(Clauses);
  Parts[4] := CompileGroupBy(Clauses);
  Parts[5] := CompileHaving(Clauses);
  Parts[6] := CompileOrderBy(Clauses);
  Parts[7] := CompileLimit(Clauses);
  Parts[8] := CompileOffset(Clauses);

  UnionPart := CompileUnion(Clauses);
  Result    := AssembleQuery(Parts);

  if WithPart <> '' then
    Result := WithPart + ' ' + Result;
  if UnionPart <> '' then
    Result := Result + ' ' + UnionPart;
end;

// ---------------------------------------------------------------------------
//  Phase-2 compile methods
// ---------------------------------------------------------------------------

function TAnsiSqlCompiler.CompileJoin(const Clauses: TArray<TAbstractClause>): string;
var
  Sb: TStringBuilder;
  Clause: TAbstractClause;
  J: TJoinClause;
  TableExpr: string;
begin
  Sb := TStringBuilder.Create;
  try
    for Clause in Clauses do
    begin
      if not (Clause is TJoinClause) then Continue;
      J := TJoinClause(Clause);

      if Sb.Length > 0 then Sb.Append(' ');

      case J.JoinType of
        TJoinType.Inner:     Sb.Append('INNER JOIN ');
        TJoinType.Left:      Sb.Append('LEFT JOIN ');
        TJoinType.Right:     Sb.Append('RIGHT JOIN ');
        TJoinType.Cross:     Sb.Append('CROSS JOIN ');
        TJoinType.FullOuter: Sb.Append('FULL OUTER JOIN ');
      end;

      if J.Schema <> '' then
        TableExpr := WrapTable(J.Schema) + '.' + WrapTable(J.Table)
      else
        TableExpr := WrapTable(J.Table);
      if J.Alias <> '' then
        TableExpr := TableExpr + ' AS ' + WrapTable(J.Alias);

      Sb.Append(TableExpr);
      if J.Condition <> '' then
        Sb.Append(' ON ' + J.Condition);
    end;
    Result := Sb.ToString;
  finally
    Sb.Free;
  end;
end;

function TAnsiSqlCompiler.CompileGroupBy(const Clauses: TArray<TAbstractClause>): string;
var
  Parts: TList<string>;
  Clause: TAbstractClause;
  G: TGroupByClause;
  I: Integer;
begin
  Result := '';
  Parts := TList<string>.Create;
  try
    for Clause in Clauses do
      if Clause is TGroupByClause then
      begin
        G := TGroupByClause(Clause);
        if G.IsRaw then
          Parts.Add(G.Column)
        else
          Parts.Add(WrapColumn(G.Column));
      end;
    if Parts.Count > 0 then
    begin
      Result := 'GROUP BY ';
      for I := 0 to Parts.Count - 1 do
      begin
        if I > 0 then Result := Result + ', ';
        Result := Result + Parts[I];
      end;
    end;
  finally
    Parts.Free;
  end;
end;

function TAnsiSqlCompiler.CompileHaving(const Clauses: TArray<TAbstractClause>): string;
var
  Sb: TStringBuilder;
  Clause: TAbstractClause;
  H: THavingClause;
  First: Boolean;
  Expr: string;
begin
  Sb := TStringBuilder.Create;
  try
    First := True;
    for Clause in Clauses do
    begin
      if not (Clause is THavingClause) then Continue;
      H := THavingClause(Clause);

      if First then
        Sb.Append('HAVING ')
      else
        case H.Connector of
          TBoolOp.opAnd: Sb.Append(' AND ');
          TBoolOp.opOr:  Sb.Append(' OR ');
        end;
      First := False;

      case H.Op of
        TWhereOp.Raw:
          Expr := H.RawSql;
        else
        begin
          AddBinding(H.Value);
          // HAVING column is always output verbatim (aggregate expressions like COUNT(*))
          Expr := H.Column + ' ' + OperatorSymbol(H.Op) + ' ' + ParamPlaceholder;
        end;
      end;

      Sb.Append(Expr);
    end;
    Result := Sb.ToString;
  finally
    Sb.Free;
  end;
end;

function TAnsiSqlCompiler.CompileWith(const Clauses: TArray<TAbstractClause>): string;
var
  Parts: TList<string>;
  Clause: TAbstractClause;
  W: TWithClause;
  I: Integer;
  HasRecursive: Boolean;
begin
  Result := '';
  Parts := TList<string>.Create;
  try
    HasRecursive := False;
    for Clause in Clauses do
      if Clause is TWithClause then
      begin
        W := TWithClause(Clause);
        if W.IsRecursive then HasRecursive := True;
        Parts.Add(W.Name + ' AS (' + CompileSubQuery(W.SubQuery as IQuery) + ')');
      end;
    if Parts.Count > 0 then
    begin
      if HasRecursive then
        Result := 'WITH RECURSIVE '
      else
        Result := 'WITH ';
      for I := 0 to Parts.Count - 1 do
      begin
        if I > 0 then Result := Result + ', ';
        Result := Result + Parts[I];
      end;
    end;
  finally
    Parts.Free;
  end;
end;

function TAnsiSqlCompiler.CompileUnion(const Clauses: TArray<TAbstractClause>): string;
var
  Sb: TStringBuilder;
  Clause: TAbstractClause;
  U: TUnionClause;
  SubSQL: string;
begin
  Sb := TStringBuilder.Create;
  try
    for Clause in Clauses do
    begin
      if not (Clause is TUnionClause) then Continue;
      U := TUnionClause(Clause);
      SubSQL := CompileSubQuery(U.SubQuery as IQuery);
      if Sb.Length > 0 then Sb.Append(' ');
      case U.Kind of
        TUnionKind.Union:     Sb.Append('UNION ' + SubSQL);
        TUnionKind.UnionAll:  Sb.Append('UNION ALL ' + SubSQL);
        TUnionKind.Intersect: Sb.Append('INTERSECT ' + SubSQL);
        TUnionKind.&Except:   Sb.Append('EXCEPT ' + SubSQL);
      end;
    end;
    Result := Sb.ToString;
  finally
    Sb.Free;
  end;
end;

end.
