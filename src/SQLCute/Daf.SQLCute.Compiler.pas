unit Daf.SQLCute.Compiler;

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils,
  System.Variants,
  System.Generics.Collections,
  Daf.SQLCute,
  Daf.SQLCute.Clauses,
  Daf.SQLCute.WhereTree;

type
  /// <summary>Raised when the compiler is handed a statement kind it has no
  /// path for — so a future kind fails loud instead of silently compiling a
  /// retrieval.</summary>
  ESQLCuteCompilerError = class(Exception);

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
    /// <summary>
    /// Emits one ordered column's whole contribution to the ORDER BY list —
    /// including the NULL-ordering option, if any. A dialect without
    /// <c>NULLS FIRST/LAST</c> overrides this to emulate the requested order,
    /// which is why the hook returns the entire term (and may return several
    /// comma-separated sort keys) rather than a suffix.
    /// </summary>
    function CompileOrderByColumn(const O: TOrderByClause): string; virtual;
    /// <summary>
    /// <c>ASC</c> / <c>DESC</c> for a sort direction. Shared by the ORDER BY hook
    /// and by the dialects that emulate NULL placement with an extra sort key.
    /// </summary>
    function SortDirKeyword(Dir: TSortDir): string;
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
    /// <summary>
    /// Emits the SQL for a single boolean leaf clause (comparison, IN/BETWEEN,
    /// IS NULL, LIKE, EXISTS, raw, column-column, value-IN, date, or HAVING),
    /// adding its bindings in order. NOT is NOT applied here — the tree's Neg
    /// node wraps it (see EmitNode).
    /// </summary>
    function EmitLeafClause(const Clause: TAbstractClause): string;
    /// <summary>
    /// Walks a normalized WHERE/HAVING tree emitting SQL with explicit precedence
    /// parentheses (OR-group inside AND, and AND-group inside OR, are wrapped).
    /// </summary>
    function EmitNode(const Node: TWhereNode): string;
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

function TAnsiSqlCompiler.EmitLeafClause(const Clause: TAbstractClause): string;
var
  W: TWhereClause;
  WI: TWhereInClause;
  H: THavingClause;
  Placeholders: string;
  I: Integer;
begin
  // --- WHERE IN / NOT IN (value-array variant) ---
  if Clause is TWhereInClause then
  begin
    WI := TWhereInClause(Clause);
    Placeholders := '';
    for I := 0 to High(WI.Values) do
    begin
      if I > 0 then Placeholders := Placeholders + ', ';
      AddBinding(WI.Values[I]);
      Placeholders := Placeholders + ParamPlaceholder;
    end;
    if WI.Negated then
      Result := WrapColumn(WI.Column) + ' NOT IN (' + Placeholders + ')'
    else
      Result := WrapColumn(WI.Column) + ' IN (' + Placeholders + ')';
    Exit;
  end;

  // --- date/time portion filter ---
  if Clause is TDateWhereClause then
    Exit(CompileDateWhere(TDateWhereClause(Clause)));

  // --- HAVING condition (raw, or col op ?; note: no WrapColumn) ---
  if Clause is THavingClause then
  begin
    H := THavingClause(Clause);
    case H.Op of
      TWhereOp.Raw:
        Result := H.RawSql;
    else
      begin
        AddBinding(H.Value);
        Result := H.Column + ' ' + OperatorSymbol(H.Op) + ' ' + ParamPlaceholder;
      end;
    end;
    Exit;
  end;

  // --- TWhereClause: op-dispatch ---
  W := TWhereClause(Clause);
  if W.IsColumnValue then
    Result := WrapColumn(W.Column) + ' ' + OperatorSymbol(W.Op) + ' ' + WrapColumn(VarToStr(W.Value))
  else
    case W.Op of
      TWhereOp.IsNull:
        Result := WrapColumn(W.Column) + ' IS NULL';
      TWhereOp.IsNotNull:
        Result := WrapColumn(W.Column) + ' IS NOT NULL';
      TWhereOp.&Exists:
        Result := 'EXISTS (' + CompileSubQuery(W.SubQuery as IQuery) + ')';
      TWhereOp.NotExists:
        Result := 'NOT EXISTS (' + CompileSubQuery(W.SubQuery as IQuery) + ')';
      TWhereOp.&Between:
      begin
        AddBinding(W.Value);
        AddBinding(W.Value2);
        Result := WrapColumn(W.Column) + ' BETWEEN ' + ParamPlaceholder + ' AND ' + ParamPlaceholder;
      end;
      TWhereOp.NotBetween:
      begin
        AddBinding(W.Value);
        AddBinding(W.Value2);
        Result := WrapColumn(W.Column) + ' NOT BETWEEN ' + ParamPlaceholder + ' AND ' + ParamPlaceholder;
      end;
      TWhereOp.&In:
      begin
        if W.SubQuery <> nil then
          Result := WrapColumn(W.Column) + ' IN (' + CompileSubQuery(W.SubQuery as IQuery) + ')'
        else
          Result := WrapColumn(W.Column) + ' IN (' + VarToStr(W.Value) + ')';
      end;
      TWhereOp.NotIn:
      begin
        if W.SubQuery <> nil then
          Result := WrapColumn(W.Column) + ' NOT IN (' + CompileSubQuery(W.SubQuery as IQuery) + ')'
        else
          Result := WrapColumn(W.Column) + ' NOT IN (' + VarToStr(W.Value) + ')';
      end;
      TWhereOp.Raw:
        Result := W.RawSql;
      TWhereOp.Like, TWhereOp.NotLike:
      begin
        AddBinding(W.Value);
        if W.CaseSensitive then
          Result := WrapColumn(W.Column) + ' ' + OperatorSymbol(W.Op) + ' ' + ParamPlaceholder
        else
          Result := 'LOWER(' + WrapColumn(W.Column) + ') ' + OperatorSymbol(W.Op) + ' ' + ParamPlaceholder;
      end;
    else
      begin
        AddBinding(W.Value);
        Result := WrapColumn(W.Column) + ' ' + OperatorSymbol(W.Op) + ' ' + ParamPlaceholder;
      end;
    end;
end;

function TAnsiSqlCompiler.EmitNode(const Node: TWhereNode): string;

  // Emit a child, parenthesizing it when its kind would otherwise lose precedence.
  function Wrapped(const Child: TWhereNode; ParenIf: TWhereNodeKind): string;
  begin
    Result := EmitNode(Child);
    if Child.Kind = ParenIf then
      Result := '(' + Result + ')';
  end;

var
  I: Integer;
begin
  case Node.Kind of
    TWhereNodeKind.Leaf:
      Result := EmitLeafClause(Node.Clause);
    TWhereNodeKind.Neg:
      Result := 'NOT (' + EmitNode(Node.Children[0]) + ')';
    TWhereNodeKind.Conjunction:
    begin
      Result := '';
      for I := 0 to High(Node.Children) do
      begin
        if I > 0 then
          Result := Result + ' AND ';
        // An OR-group inside an AND must be parenthesized.
        Result := Result + Wrapped(Node.Children[I], TWhereNodeKind.Disjunction);
      end;
    end;
    TWhereNodeKind.Disjunction:
    begin
      Result := '';
      for I := 0 to High(Node.Children) do
      begin
        if I > 0 then
          Result := Result + ' OR ';
        // AND-groups inside an OR get explicit parentheses (precedence-explicit).
        Result := Result + Wrapped(Node.Children[I], TWhereNodeKind.Conjunction);
      end;
    end;
  end;
end;

function TAnsiSqlCompiler.CompileWhere(const Clauses: TArray<TAbstractClause>): string;
begin
  var Root := TWhereTree.Build(Clauses);
  try
    if Root = nil then
      Result := ''
    else
      Result := 'WHERE ' + EmitNode(Root);
  finally
    Root.Free;
  end;
end;

function TAnsiSqlCompiler.CompileWhereList(const Clauses: TArray<TAbstractClause>): string;
begin
  // Used by JOIN ON callback conditions — same tree, without the WHERE keyword.
  var Root := TWhereTree.Build(Clauses);
  try
    if Root = nil then
      Result := ''
    else
      Result := EmitNode(Root);
  finally
    Root.Free;
  end;
end;

function TAnsiSqlCompiler.SortDirKeyword(Dir: TSortDir): string;
begin
  if Dir = TSortDir.Desc then
    Result := 'DESC'
  else
    Result := 'ASC';
end;

function TAnsiSqlCompiler.CompileOrderByColumn(const O: TOrderByClause): string;
begin
  Result := WrapColumn(O.Column) + ' ' + SortDirKeyword(O.Direction);
  // Default emits nothing: the database applies its own rule.
  case O.Nulls of
    TNullsOrder.First: Result := Result + ' NULLS FIRST';
    TNullsOrder.Last:  Result := Result + ' NULLS LAST';
  end;
end;

function TAnsiSqlCompiler.CompileOrderBy(const Clauses: TArray<TAbstractClause>): string;
var
  Parts: TList<string>;
  Clause: TAbstractClause;
  O: TOrderByClause;
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
          Parts.Add(O.Column)          // a raw expression is emitted verbatim
        else
          Parts.Add(CompileOrderByColumn(O));
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

    // Route by the declared statement kind, not by which clauses are present.
    // No mute branch: a kind with no path here raises rather than falling to
    // SELECT — a future kind is a loud failure, not a silent retrieval. Select
    // is the empty arm that falls through to the SELECT assembly below.
    case Query.Kind of
      TQueryKind.Delete:
        begin
          Result.SQL      := CompileDelete(Clauses);
          Result.Bindings := FBindings.ToArray;
          Exit;
        end;
      TQueryKind.Update:
        begin
          Result.SQL      := CompileUpdate(Clauses);
          Result.Bindings := FBindings.ToArray;
          Exit;
        end;
      TQueryKind.Insert:
        begin
          Result.SQL      := CompileInsert(Clauses);
          Result.Bindings := FBindings.ToArray;
          Exit;
        end;
      TQueryKind.Select: ;  // falls through to the SELECT assembly below
    else
      raise ESQLCuteCompilerError.CreateFmt(
        'No SQL path for statement kind %s.', [QueryKindName(Query.Kind)]);
    end;

    // SELECT
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
//  compile methods
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
begin
  // Built from the same shared normalizer as WHERE, so precedence (AND before
  // OR) and parenthesization are identical. HAVING leaves emit their column
  // verbatim (aggregate expressions like COUNT(*)) — see EmitLeafClause.
  var Root := TWhereTree.BuildHaving(Clauses);
  try
    if Root = nil then
      Result := ''
    else
      Result := 'HAVING ' + EmitNode(Root);
  finally
    Root.Free;
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
//  DML compile methods
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
