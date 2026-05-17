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
    function CompileSubQuery(const Query: IQuery): string;
  protected
    procedure AddBinding(const Value: Variant);
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
    function CompileInsert(const Clauses: TArray<TAbstractClause>): string; virtual;
    function CompileUpdate(const Clauses: TArray<TAbstractClause>): string; virtual;
    function CompileDelete(const Clauses: TArray<TAbstractClause>): string; virtual;
    function AssembleQuery(const Parts: TArray<string>): string; virtual;
    /// <summary>
    /// Compile a flat list of TWhereClause items into "cond1 AND cond2 …"
    /// without a leading WHERE keyword. Used by CompileWhere nested groups
    /// and CompileJoin ON (…) callback conditions.
    /// </summary>
    function CompileWhereList(const Clauses: TArray<TAbstractClause>): string;
    /// <summary>
    /// Generates the SQL fragment for a date/time WHERE condition.
    /// Override in dialect subclasses for dialect-specific date functions.
    /// ANSI fallback: emits <c>col op ?</c>.
    /// </summary>
    function CompileDateWhere(const Clause: TDateWhereClause): string; virtual;
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
          if Col.SubQuery <> nil then
            Expr := '(' + CompileSubQuery(Col.SubQuery as IQuery) + ')'
          else if Col.IsRaw then
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
  FRaw: TFromRawClause;
  TableExpr: string;
  B: Variant;
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

    if Clause is TFromRawClause then
    begin
      FRaw := TFromRawClause(Clause);
      TableExpr := FRaw.RawSql;
      if FRaw.Alias <> '' then
        TableExpr := TableExpr + ' AS ' + WrapTable(FRaw.Alias);
      for B in FRaw.Bindings do
        AddBinding(B);
      Result := 'FROM ' + TableExpr;
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
  WI: TWhereInClause;
  WN: TNestedWhereClause;
  First: Boolean;
  Expr, Keyword, Placeholders, InnerSql: string;
  I: Integer;

  function ConnectorStr(Conn: TBoolOp): string;
  begin
    case Conn of
      TBoolOp.opAnd: Result := ' AND ';
      TBoolOp.opOr:  Result := ' OR ';
    else
      Result := ' AND ';
    end;
  end;

  // Recursive compilation of a nested group's sub-clauses (no FBindings reset)
  function CompileNestedGroup(const SubClauses: TArray<TAbstractClause>): string; forward;

  function CompileNestedGroup(const SubClauses: TArray<TAbstractClause>): string;
  begin
    Result := CompileWhereList(SubClauses);
  end;

begin
  Sb := TStringBuilder.Create;
  try
    First := True;
    for Clause in Clauses do
    begin
      // --- NESTED GROUP (TNestedWhereClause must be checked BEFORE TWhereClause) ---
      if Clause is TNestedWhereClause then
      begin
        WN := TNestedWhereClause(Clause);
        if First then
          Sb.Append('WHERE ')
        else
          Sb.Append(ConnectorStr(WN.Connector));
        First := False;
        InnerSql := CompileNestedGroup(WN.SubClauses);
        Sb.Append('(' + InnerSql + ')');
        Continue;
      end;

      // --- WHERE IN / NOT IN (value-array variant) ---
      if Clause is TWhereInClause then
      begin
        WI := TWhereInClause(Clause);
        if First then
          Sb.Append('WHERE ')
        else
          Sb.Append(' ' + WI.Connector + ' ');
        First := False;

        Placeholders := '';
        for I := 0 to High(WI.Values) do
        begin
          if I > 0 then Placeholders := Placeholders + ', ';
          AddBinding(WI.Values[I]);
          Placeholders := Placeholders + ParamPlaceholder;
        end;
        if WI.Negated then
          Keyword := ' NOT IN ('
        else
          Keyword := ' IN (';
        Sb.Append(WrapColumn(WI.Column) + Keyword + Placeholders + ')');
        Continue;
      end;

      if Clause is TDateWhereClause then
      begin
        if First then Sb.Append('WHERE ')
        else case TDateWhereClause(Clause).Connector of
          TBoolOp.opAnd: Sb.Append(' AND ');
          TBoolOp.opOr:  Sb.Append(' OR ');
        end;
        First := False;
        Sb.Append(CompileDateWhere(TDateWhereClause(Clause)));
        Continue;
      end;

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

      // --- column-column comparison (no binding) ---
      if W.IsColumnValue then
      begin
        Expr := WrapColumn(W.Column) + ' ' + OperatorSymbol(W.Op) + ' ' + WrapColumn(VarToStr(W.Value));
      end
      else
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

        TWhereOp.NotBetween:
        begin
          AddBinding(W.Value);
          AddBinding(W.Value2);
          Expr := WrapColumn(W.Column) + ' NOT BETWEEN ' + ParamPlaceholder + ' AND ' + ParamPlaceholder;
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

        TWhereOp.Like, TWhereOp.NotLike:
        begin
          AddBinding(W.Value);
          if W.CaseSensitive then
            Expr := WrapColumn(W.Column) + ' ' + OperatorSymbol(W.Op) + ' ' + ParamPlaceholder
          else
            Expr := 'LOWER(' + WrapColumn(W.Column) + ') ' + OperatorSymbol(W.Op) + ' ' + ParamPlaceholder;
        end;

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

function TAnsiSqlCompiler.CompileWhereList(const Clauses: TArray<TAbstractClause>): string;
var
  Sb: TStringBuilder;
  Sub: TAbstractClause;
  SubW: TWhereClause;
  SubWI: TWhereInClause;
  SubWN: TNestedWhereClause;
  First: Boolean;
  SubExpr, SubPlaceholders: string;
  J: Integer;
begin
  Sb := TStringBuilder.Create;
  try
    First := True;
    for Sub in Clauses do
    begin
      if Sub is TNestedWhereClause then
      begin
        SubWN := TNestedWhereClause(Sub);
        if not First then
        begin
          if SubWN.Connector = TBoolOp.opOr then
            Sb.Append(' OR ')
          else
            Sb.Append(' AND ');
        end;
        First := False;
        Sb.Append('(' + CompileWhereList(SubWN.SubClauses) + ')');
        Continue;
      end;

      if Sub is TWhereInClause then
      begin
        SubWI := TWhereInClause(Sub);
        if not First then
          Sb.Append(' ' + SubWI.Connector + ' ');
        First := False;
        SubPlaceholders := '';
        for J := 0 to High(SubWI.Values) do
        begin
          if J > 0 then SubPlaceholders := SubPlaceholders + ', ';
          AddBinding(SubWI.Values[J]);
          SubPlaceholders := SubPlaceholders + ParamPlaceholder;
        end;
        if SubWI.Negated then
          Sb.Append(WrapColumn(SubWI.Column) + ' NOT IN (' + SubPlaceholders + ')')
        else
          Sb.Append(WrapColumn(SubWI.Column) + ' IN (' + SubPlaceholders + ')');
        Continue;
      end;

      if Sub is TDateWhereClause then
      begin
        if not First then
        begin
          if TDateWhereClause(Sub).Connector = TBoolOp.opOr then
            Sb.Append(' OR ')
          else
            Sb.Append(' AND ');
        end;
        First := False;
        Sb.Append(CompileDateWhere(TDateWhereClause(Sub)));
        Continue;
      end;

      if not (Sub is TWhereClause) then Continue;
      SubW := TWhereClause(Sub);

      if not First then
      begin
        if SubW.Connector = TBoolOp.opOr then
          Sb.Append(' OR ')
        else
          Sb.Append(' AND ');
      end;
      First := False;

      if SubW.IsColumnValue then
      begin
        SubExpr := WrapColumn(SubW.Column) + ' ' + OperatorSymbol(SubW.Op) + ' ' + WrapColumn(VarToStr(SubW.Value));
      end
      else
      case SubW.Op of
        TWhereOp.IsNull:    SubExpr := WrapColumn(SubW.Column) + ' IS NULL';
        TWhereOp.IsNotNull: SubExpr := WrapColumn(SubW.Column) + ' IS NOT NULL';
        TWhereOp.&Exists:   SubExpr := 'EXISTS (' + CompileSubQuery(SubW.SubQuery as IQuery) + ')';
        TWhereOp.NotExists: SubExpr := 'NOT EXISTS (' + CompileSubQuery(SubW.SubQuery as IQuery) + ')';
        TWhereOp.&Between:
        begin
          AddBinding(SubW.Value);
          AddBinding(SubW.Value2);
          SubExpr := WrapColumn(SubW.Column) + ' BETWEEN ' + ParamPlaceholder + ' AND ' + ParamPlaceholder;
        end;
        TWhereOp.NotBetween:
        begin
          AddBinding(SubW.Value);
          AddBinding(SubW.Value2);
          SubExpr := WrapColumn(SubW.Column) + ' NOT BETWEEN ' + ParamPlaceholder + ' AND ' + ParamPlaceholder;
        end;
        TWhereOp.&In:
        begin
          if SubW.SubQuery <> nil then
            SubExpr := WrapColumn(SubW.Column) + ' IN (' + CompileSubQuery(SubW.SubQuery as IQuery) + ')'
          else
            SubExpr := WrapColumn(SubW.Column) + ' IN (' + VarToStr(SubW.Value) + ')';
        end;
        TWhereOp.NotIn:
        begin
          if SubW.SubQuery <> nil then
            SubExpr := WrapColumn(SubW.Column) + ' NOT IN (' + CompileSubQuery(SubW.SubQuery as IQuery) + ')'
          else
            SubExpr := WrapColumn(SubW.Column) + ' NOT IN (' + VarToStr(SubW.Value) + ')';
        end;
        TWhereOp.Raw: SubExpr := SubW.RawSql;
        TWhereOp.Like, TWhereOp.NotLike:
        begin
          AddBinding(SubW.Value);
          if SubW.CaseSensitive then
            SubExpr := WrapColumn(SubW.Column) + ' ' + OperatorSymbol(SubW.Op) + ' ' + ParamPlaceholder
          else
            SubExpr := 'LOWER(' + WrapColumn(SubW.Column) + ') ' + OperatorSymbol(SubW.Op) + ' ' + ParamPlaceholder;
        end;
        else
        begin
          AddBinding(SubW.Value);
          SubExpr := WrapColumn(SubW.Column) + ' ' + OperatorSymbol(SubW.Op) + ' ' + ParamPlaceholder;
        end;
      end;

      if SubW.IsNot then
        SubExpr := 'NOT (' + SubExpr + ')';
      Sb.Append(SubExpr);
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
  Clause: TAbstractClause;
begin
  FBindings := TList<Variant>.Create;
  try
    Clauses := Query.Clauses;

    // Detect DML query kind first
    for Clause in Clauses do
    begin
      if Clause is TDeleteClause then
      begin
        Result.SQL      := CompileDelete(Clauses);
        Result.Bindings := FBindings.ToArray;
        Exit;
      end;
      if Clause is TUpdateSetClause then
      begin
        Result.SQL      := CompileUpdate(Clauses);
        Result.Bindings := FBindings.ToArray;
        Exit;
      end;
      if Clause is TInsertClause then
      begin
        Result.SQL      := CompileInsert(Clauses);
        Result.Bindings := FBindings.ToArray;
        Exit;
      end;
    end;

    // Default: SELECT
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
  JS: TJoinSubqueryClause;
  TableExpr: string;

  function JoinKeyword(JoinType: TJoinType): string;
  begin
    case JoinType of
      TJoinType.Inner:     Result := 'INNER JOIN ';
      TJoinType.Left:      Result := 'LEFT JOIN ';
      TJoinType.Right:     Result := 'RIGHT JOIN ';
      TJoinType.Cross:     Result := 'CROSS JOIN ';
      TJoinType.FullOuter: Result := 'FULL OUTER JOIN ';
    else
      Result := 'JOIN ';
    end;
  end;

begin
  Sb := TStringBuilder.Create;
  try
    for Clause in Clauses do
    begin
      // --- Subquery JOIN ---
      if Clause is TJoinSubqueryClause then
      begin
        JS := TJoinSubqueryClause(Clause);
        if Sb.Length > 0 then Sb.Append(' ');
        Sb.Append(JoinKeyword(JS.JoinType));
        TableExpr := '(' + CompileSubQuery(JS.SubQuery as IQuery) + ') AS ' + WrapTable(JS.Alias);
        Sb.Append(TableExpr);
        if Length(JS.ConditionClauses) > 0 then
          Sb.Append(' ON (' + CompileWhereList(JS.ConditionClauses) + ')');
        Continue;
      end;

      if not (Clause is TJoinClause) then Continue;
      J := TJoinClause(Clause);

      if Sb.Length > 0 then Sb.Append(' ');
      Sb.Append(JoinKeyword(J.JoinType));

      if J.Schema <> '' then
        TableExpr := WrapTable(J.Schema) + '.' + WrapTable(J.Table)
      else
        TableExpr := WrapTable(J.Table);
      if J.Alias <> '' then
        TableExpr := TableExpr + ' AS ' + WrapTable(J.Alias);

      Sb.Append(TableExpr);

      if Length(J.ConditionClauses) > 0 then
        Sb.Append(' ON (' + CompileWhereList(J.ConditionClauses) + ')')
      else if J.Col1 <> '' then
        Sb.Append(' ON ' + WrapColumn(J.Col1) + ' ' + J.Op + ' ' + WrapColumn(J.Col2))
      else if J.Condition <> '' then
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
        if W.IsRawSql then
        begin
          var B: Variant;
          for B in W.RawBindings do
            AddBinding(B);
          Parts.Add(W.Name + ' AS (' + W.RawSql + ')');
        end
        else
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
      if Sb.Length > 0 then Sb.Append(' ');
      if U.RawSql <> '' then
      begin
        Sb.Append(U.RawSql);
        Continue;
      end;
      SubSQL := CompileSubQuery(U.SubQuery as IQuery);
      case U.Kind of
        TUnionKind.Union:        Sb.Append('UNION ' + SubSQL);
        TUnionKind.UnionAll:     Sb.Append('UNION ALL ' + SubSQL);
        TUnionKind.Intersect:    Sb.Append('INTERSECT ' + SubSQL);
        TUnionKind.&Except:      Sb.Append('EXCEPT ' + SubSQL);
        TUnionKind.IntersectAll: Sb.Append('INTERSECT ALL ' + SubSQL);
        TUnionKind.ExceptAll:    Sb.Append('EXCEPT ALL ' + SubSQL);
      end;
    end;
    Result := Sb.ToString;
  finally
    Sb.Free;
  end;
end;

// ---------------------------------------------------------------------------
//  Phase-3 DML compile methods
// ---------------------------------------------------------------------------

function TAnsiSqlCompiler.CompileInsert(const Clauses: TArray<TAbstractClause>): string;
var
  IC: TInsertClause;
  Clause: TAbstractClause;
  TableName, ColsStr, RowStr: string;
  RowsParts: TList<string>;
  I, J: Integer;
begin
  TableName := '';
  IC        := nil;
  for Clause in Clauses do
  begin
    if (Clause is TFromClause) and (TableName = '') then
      TableName := WrapTable(TFromClause(Clause).Table);
    if Clause is TInsertClause then
      IC := TInsertClause(Clause);
  end;
  if IC = nil then Exit('');

  // Build column list
  ColsStr := '';
  for I := 0 to High(IC.Columns) do
  begin
    if I > 0 then ColsStr := ColsStr + ', ';
    ColsStr := ColsStr + WrapColumn(IC.Columns[I]);
  end;

  // INSERT … SELECT
  if IC.SubQuery <> nil then
  begin
    Result := 'INSERT INTO ' + TableName + ' (' + ColsStr + ') '
            + CompileSubQuery(IC.SubQuery as IQuery);
    Exit;
  end;

  // INSERT … VALUES
  RowsParts := TList<string>.Create;
  try
    for I := 0 to High(IC.Rows) do
    begin
      RowStr := '(';
      for J := 0 to High(IC.Rows[I]) do
      begin
        if J > 0 then RowStr := RowStr + ', ';
        AddBinding(IC.Rows[I][J]);
        RowStr := RowStr + ParamPlaceholder;
      end;
      RowsParts.Add(RowStr + ')');
    end;
    var RowsStr := '';
    for I := 0 to RowsParts.Count - 1 do
    begin
      if I > 0 then RowsStr := RowsStr + ', ';
      RowsStr := RowsStr + RowsParts[I];
    end;
    Result := 'INSERT INTO ' + TableName + ' (' + ColsStr + ') VALUES ' + RowsStr;
  finally
    RowsParts.Free;
  end;
end;

function TAnsiSqlCompiler.CompileUpdate(const Clauses: TArray<TAbstractClause>): string;
var
  Clause: TAbstractClause;
  TableName: string;
  SetParts: TList<string>;
  US: TUpdateSetClause;
  WherePart: string;
  SetStr: string;
  I: Integer;
begin
  TableName := '';
  for Clause in Clauses do
    if (Clause is TFromClause) and (TableName = '') then
      TableName := WrapTable(TFromClause(Clause).Table);

  SetParts := TList<string>.Create;
  try
    for Clause in Clauses do
      if Clause is TUpdateSetClause then
      begin
        US := TUpdateSetClause(Clause);
        if US.IsRaw then
          SetParts.Add(US.RawSql)
        else
        begin
          AddBinding(US.Value);
          SetParts.Add(WrapColumn(US.Column) + ' = ' + ParamPlaceholder);
        end;
      end;

    SetStr := '';
    for I := 0 to SetParts.Count - 1 do
    begin
      if I > 0 then SetStr := SetStr + ', ';
      SetStr := SetStr + SetParts[I];
    end;

    WherePart := CompileWhere(Clauses);
    Result := 'UPDATE ' + TableName + ' SET ' + SetStr;
    if WherePart <> '' then
      Result := Result + ' ' + WherePart;
  finally
    SetParts.Free;
  end;
end;

function TAnsiSqlCompiler.CompileDelete(const Clauses: TArray<TAbstractClause>): string;
var
  Clause: TAbstractClause;
  TableName, WherePart: string;
begin
  TableName := '';
  for Clause in Clauses do
    if (Clause is TFromClause) and (TableName = '') then
      TableName := WrapTable(TFromClause(Clause).Table);

  WherePart := CompileWhere(Clauses);
  Result := 'DELETE FROM ' + TableName;
  if WherePart <> '' then
    Result := Result + ' ' + WherePart;
end;

// ---------------------------------------------------------------------------
//  Date / time WHERE — ANSI fallback
// ---------------------------------------------------------------------------

function TAnsiSqlCompiler.CompileDateWhere(const Clause: TDateWhereClause): string;
begin
  AddBinding(Clause.Value);
  Result := WrapColumn(Clause.Column) + ' ' + Clause.Op + ' ' + ParamPlaceholder;
end;

end.
