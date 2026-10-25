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
  /// The state of one compilation: the bindings, in the order their placeholders
  /// are written. <c>Compile</c> creates it and hands it to every method that
  /// emits SQL, so a compiler instance keeps nothing between compilations and
  /// serves successive, nested and concurrent ones.
  /// </summary>
  TSqlCompilation = class
  strict private
    FBindings: TList<Variant>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddBinding(const Value: Variant);
    function Bindings: TArray<Variant>;
  end;

  /// <summary>
  /// ANSI SQL compiler — baseline implementation of IQueryCompiler.
  /// Every method that emits SQL writes its placeholders as '?'; a final pass
  /// replaces each '?' not preceded by '\' with the placeholder of its position
  /// (<see cref="FormatPlaceholder"/>) and each '\?' with a literal '?'. So the
  /// '?' of a raw expression are numbered with the others.
  /// All dialect compilers extend this class and override virtual methods.
  /// </summary>
  TAnsiSqlCompiler = class(TInterfacedObject, IQueryCompiler)
  private
    function CompileSubQuery(const C: TSqlCompilation; const Query: IQuery): string;
    function CompileStatement(const C: TSqlCompilation; const Query: IQuery): string;
    function NumberPlaceholders(const SQL: string): string;
  protected
    /// <summary>
    /// Quotes a single name, doubling inside it the character that closes the
    /// quote. ANSI: the name as it is.
    /// </summary>
    function WrapIdentifier(const Name: string): string; virtual;
    /// <summary>
    /// A column: an alias written in it (<c>name as alias</c>) is split off, the
    /// name is split at each dot, a <c>*</c> part stays bare, and every other
    /// part and the alias go through <see cref="WrapIdentifier"/>.
    /// </summary>
    function WrapColumn(const Col: string): string; virtual;
    /// <summary>A table, quoted the way <see cref="WrapColumn"/> quotes a column.</summary>
    function WrapTable(const Table: string): string; virtual;
    /// <summary>
    /// What joins a column, a table or a subquery with its alias, spaces
    /// included. ANSI: <c> AS </c>.
    /// </summary>
    function AliasKeyword: string; virtual;
    /// <summary>
    /// The literal a logical comparison compares with. ANSI: <c>true</c> and
    /// <c>false</c>.
    /// </summary>
    function BooleanLiteral(const Value: Boolean): string; virtual;
    /// <summary>A name, the alias keyword and the quoted alias.</summary>
    function WithAlias(const Expr, Alias: string): string;
    /// <summary>A name split at each dot, each part quoted but a bare <c>*</c>.</summary>
    function WrapQualified(const Name: string): string;
    /// <summary>
    /// The placeholder for the binding at <c>Index</c>, counted from 0 in the
    /// order of the text. ANSI: '?'.
    /// </summary>
    function FormatPlaceholder(const Index: Integer): string; virtual;
    function OperatorSymbol(Op: TWhereOp): string; virtual;
    function CompileSelect(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string; virtual;
    function CompileFrom(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string; virtual;
    function CompileJoin(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string; virtual;
    function CompileWhere(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string; virtual;
    function CompileGroupBy(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string; virtual;
    function CompileHaving(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string; virtual;
    function CompileOrderBy(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string; virtual;
    /// <summary>
    /// Emits one ordered column's whole contribution to the ORDER BY list —
    /// including the NULL-ordering option, if any. A dialect without
    /// <c>NULLS FIRST/LAST</c> overrides this to emulate the requested order,
    /// which is why the hook returns the entire term (and may return several
    /// comma-separated sort keys) rather than a suffix.
    /// </summary>
    function CompileOrderByColumn(const C: TSqlCompilation; const O: TOrderByClause): string; virtual;
    /// <summary>
    /// <c>ASC</c> / <c>DESC</c> for a sort direction. Shared by the ORDER BY hook
    /// and by the dialects that emulate NULL placement with an extra sort key.
    /// </summary>
    function SortDirKeyword(Dir: TSortDir): string;
    function CompileLimit(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string; virtual;
    function CompileOffset(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string; virtual;
    function CompileWith(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string; virtual;
    function CompileUnion(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string; virtual;
    function CompileInsert(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string; virtual;
    function CompileUpdate(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string; virtual;
    function CompileDelete(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string; virtual;
    function AssembleQuery(const Parts: TArray<string>): string; virtual;
    /// <summary>
    /// Compile a flat list of TWhereClause items into "cond1 AND cond2 …"
    /// without a leading WHERE keyword. Used by CompileWhere nested groups
    /// and CompileJoin ON (…) callback conditions.
    /// </summary>
    function CompileWhereList(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string;
    /// <summary>
    /// Generates the SQL fragment for a date/time WHERE condition.
    /// Override in dialect subclasses for dialect-specific date functions.
    /// ANSI fallback: emits <c>col op ?</c>.
    /// </summary>
    function CompileDateWhere(const C: TSqlCompilation; const Clause: TDateWhereClause): string; virtual;
    /// <summary>
    /// Emits the SQL for a single boolean leaf clause (comparison, IN/BETWEEN,
    /// IS NULL, LIKE, EXISTS, raw, column-column, value-IN, date, or HAVING),
    /// adding its bindings in order. NOT is NOT applied here — the tree's Neg
    /// node wraps it (see EmitNode).
    /// </summary>
    function EmitLeafClause(const C: TSqlCompilation; const Clause: TAbstractClause): string;
    /// <summary>
    /// Walks a normalized WHERE/HAVING tree emitting SQL with explicit precedence
    /// parentheses (OR-group inside AND, and AND-group inside OR, are wrapped).
    /// </summary>
    function EmitNode(const C: TSqlCompilation; const Node: TWhereNode): string;
  public
    function Compile(const Query: IQuery): TSQLResult;
  end;

implementation

uses
  System.TypInfo;

{ TSqlCompilation }

constructor TSqlCompilation.Create;
begin
  inherited Create;
  FBindings := TList<Variant>.Create;
end;

destructor TSqlCompilation.Destroy;
begin
  FBindings.Free;
  inherited;
end;

procedure TSqlCompilation.AddBinding(const Value: Variant);
begin
  FBindings.Add(Value);
end;

function TSqlCompilation.Bindings: TArray<Variant>;
begin
  Result := FBindings.ToArray;
end;

{ TAnsiSqlCompiler }

function TAnsiSqlCompiler.WrapIdentifier(const Name: string): string;
begin
  Result := Name;
end;

function TAnsiSqlCompiler.WrapQualified(const Name: string): string;
begin
  var Parts := Name.Split(['.']);
  for var I := 0 to High(Parts) do
    if Parts[I] <> '*' then
      Parts[I] := WrapIdentifier(Parts[I]);
  Result := string.Join('.', Parts);
end;

function TAnsiSqlCompiler.WrapColumn(const Col: string): string;
var
  Name, Alias: string;
begin
  if SplitNameAlias(Col, Name, Alias) then
    Result := WithAlias(WrapQualified(Name), Alias)
  else
    Result := WrapQualified(Col);
end;

function TAnsiSqlCompiler.WrapTable(const Table: string): string;
begin
  Result := WrapColumn(Table);
end;

function TAnsiSqlCompiler.AliasKeyword: string;
begin
  Result := ' AS ';
end;

function TAnsiSqlCompiler.BooleanLiteral(const Value: Boolean): string;
begin
  if Value then
    Result := 'true'
  else
    Result := 'false';
end;

function TAnsiSqlCompiler.WithAlias(const Expr, Alias: string): string;
begin
  Result := Expr + AliasKeyword + WrapIdentifier(Alias);
end;

function TAnsiSqlCompiler.FormatPlaceholder(const Index: Integer): string;
begin
  Result := '?';
end;

function TAnsiSqlCompiler.NumberPlaceholders(const SQL: string): string;
var
  Sb: TStringBuilder;
  I, Index: Integer;
begin
  Sb := TStringBuilder.Create;
  try
    Index := 0;
    I := 1;
    while I <= Length(SQL) do
    begin
      if (SQL[I] = '\') and (I < Length(SQL)) and (SQL[I + 1] = '?') then
      begin
        Sb.Append('?');
        Inc(I, 2);
        Continue;
      end;
      if SQL[I] = '?' then
      begin
        Sb.Append(FormatPlaceholder(Index));
        Inc(Index);
      end
      else
        Sb.Append(SQL[I]);
      Inc(I);
    end;
    Result := Sb.ToString;
  finally
    Sb.Free;
  end;
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
    raise ESQLCuteCompilerError.CreateFmt('Operator %s has no SQL symbol.',
      [GetEnumName(TypeInfo(TWhereOp), Ord(Op))]);
  end;
end;

function TAnsiSqlCompiler.CompileSelect(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string;
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
            Expr := '(' + CompileSubQuery(C, Col.SubQuery as IQuery) + ')'
          else if Col.IsRaw then
            Expr := Col.Column
          else if Col.Column = '*' then
            Expr := '*'
          else
            Expr := WrapColumn(Col.Column);

          if Col.Alias <> '' then
            Expr := WithAlias(Expr, Col.Alias);

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

function TAnsiSqlCompiler.CompileFrom(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string;
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
      Result := 'FROM (' + CompileSubQuery(C, FSQ.SubQuery) + ') ' + WrapIdentifier(FSQ.Alias);
      Exit;
    end;

    if Clause is TFromRawClause then
    begin
      FRaw := TFromRawClause(Clause);
      TableExpr := FRaw.RawSql;
      if FRaw.Alias <> '' then
        TableExpr := WithAlias(TableExpr, FRaw.Alias);
      for B in FRaw.Bindings do
        C.AddBinding(B);
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
        TableExpr := WithAlias(TableExpr, From.Alias);
      Result := 'FROM ' + TableExpr;
      Exit;
    end;
  end;
end;

function TAnsiSqlCompiler.EmitLeafClause(const C: TSqlCompilation; const Clause: TAbstractClause): string;
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
      C.AddBinding(WI.Values[I]);
      Placeholders := Placeholders + '?';
    end;
    if WI.Negated then
      Result := WrapColumn(WI.Column) + ' NOT IN (' + Placeholders + ')'
    else
      Result := WrapColumn(WI.Column) + ' IN (' + Placeholders + ')';
    Exit;
  end;

  // --- date/time portion filter ---
  if Clause is TDateWhereClause then
    Exit(CompileDateWhere(C, TDateWhereClause(Clause)));

  // --- HAVING condition (raw, or col op ?, the column quoted like any other) ---
  if Clause is THavingClause then
  begin
    H := THavingClause(Clause);
    case H.Op of
      TWhereOp.Raw:
        Result := H.RawSql;
      TWhereOp.IsNull:
        Result := WrapColumn(H.Column) + ' IS NULL';
      TWhereOp.IsNotNull:
        Result := WrapColumn(H.Column) + ' IS NOT NULL';
    else
      begin
        C.AddBinding(H.Value);
        Result := WrapColumn(H.Column) + ' ' + OperatorSymbol(H.Op) + ' ?';
      end;
    end;
    Exit;
  end;

  // --- TWhereClause: op-dispatch ---
  W := TWhereClause(Clause);
  if W.IsBoolLiteral then
    Result := WrapColumn(W.Column) + ' = ' + BooleanLiteral(W.Value)
  else if W.IsColumnValue then
    Result := WrapColumn(W.Column) + ' ' + OperatorSymbol(W.Op) + ' ' + WrapColumn(VarToStr(W.Value))
  else
    case W.Op of
      TWhereOp.IsNull:
        Result := WrapColumn(W.Column) + ' IS NULL';
      TWhereOp.IsNotNull:
        Result := WrapColumn(W.Column) + ' IS NOT NULL';
      TWhereOp.&Exists:
        Result := 'EXISTS (' + CompileSubQuery(C, W.SubQuery as IQuery) + ')';
      TWhereOp.NotExists:
        Result := 'NOT EXISTS (' + CompileSubQuery(C, W.SubQuery as IQuery) + ')';
      TWhereOp.&Between:
      begin
        C.AddBinding(W.Value);
        C.AddBinding(W.Value2);
        Result := WrapColumn(W.Column) + ' BETWEEN ? AND ?';
      end;
      TWhereOp.NotBetween:
      begin
        C.AddBinding(W.Value);
        C.AddBinding(W.Value2);
        Result := WrapColumn(W.Column) + ' NOT BETWEEN ? AND ?';
      end;
      TWhereOp.&In:
      begin
        if W.SubQuery <> nil then
          Result := WrapColumn(W.Column) + ' IN (' + CompileSubQuery(C, W.SubQuery as IQuery) + ')'
        else
        begin
          C.AddBinding(W.Value);
          Result := WrapColumn(W.Column) + ' IN (?)';
        end;
      end;
      TWhereOp.NotIn:
      begin
        if W.SubQuery <> nil then
          Result := WrapColumn(W.Column) + ' NOT IN (' + CompileSubQuery(C, W.SubQuery as IQuery) + ')'
        else
        begin
          C.AddBinding(W.Value);
          Result := WrapColumn(W.Column) + ' NOT IN (?)';
        end;
      end;
      TWhereOp.Raw:
        Result := W.RawSql;
      TWhereOp.Like, TWhereOp.NotLike:
      begin
        C.AddBinding(W.Value);
        if W.CaseSensitive then
          Result := WrapColumn(W.Column) + ' ' + OperatorSymbol(W.Op) + ' ?'
        else
          Result := 'LOWER(' + WrapColumn(W.Column) + ') ' + OperatorSymbol(W.Op) + ' ?';
      end;
    else
      begin
        C.AddBinding(W.Value);
        Result := WrapColumn(W.Column) + ' ' + OperatorSymbol(W.Op) + ' ?';
      end;
    end;
end;

function TAnsiSqlCompiler.EmitNode(const C: TSqlCompilation; const Node: TWhereNode): string;

  // Emit a child, parenthesizing it when its kind would otherwise lose precedence.
  function Wrapped(const Child: TWhereNode; ParenIf: TWhereNodeKind): string;
  begin
    Result := EmitNode(C, Child);
    if Child.Kind = ParenIf then
      Result := '(' + Result + ')';
  end;

var
  I: Integer;
begin
  case Node.Kind of
    TWhereNodeKind.Leaf:
      Result := EmitLeafClause(C, Node.Clause);
    TWhereNodeKind.Neg:
      Result := 'NOT (' + EmitNode(C, Node.Children[0]) + ')';
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

function TAnsiSqlCompiler.CompileWhere(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string;
begin
  var Root := TWhereTree.Build(Clauses);
  try
    if Root = nil then
      Result := ''
    else
      Result := 'WHERE ' + EmitNode(C, Root);
  finally
    Root.Free;
  end;
end;

function TAnsiSqlCompiler.CompileWhereList(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string;
begin
  // Used by JOIN ON callback conditions — same tree, without the WHERE keyword.
  var Root := TWhereTree.Build(Clauses);
  try
    if Root = nil then
      Result := ''
    else
      Result := EmitNode(C, Root);
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

function TAnsiSqlCompiler.CompileOrderByColumn(const C: TSqlCompilation; const O: TOrderByClause): string;
begin
  Result := WrapColumn(O.Column) + ' ' + SortDirKeyword(O.Direction);
  // Default emits nothing: the database applies its own rule.
  case O.Nulls of
    TNullsOrder.First: Result := Result + ' NULLS FIRST';
    TNullsOrder.Last:  Result := Result + ' NULLS LAST';
  end;
end;

function TAnsiSqlCompiler.CompileOrderBy(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string;
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
          Parts.Add(CompileOrderByColumn(C, O));
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

function TAnsiSqlCompiler.CompileLimit(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string;
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

function TAnsiSqlCompiler.CompileOffset(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string;
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
begin
  var C := TSqlCompilation.Create;
  try
    Result.SQL := NumberPlaceholders(CompileStatement(C, Query));
    Result.Bindings := C.Bindings;
  finally
    C.Free;
  end;
end;

function TAnsiSqlCompiler.CompileStatement(const C: TSqlCompilation; const Query: IQuery): string;
begin
  // Route by the declared statement kind, not by which clauses are present.
  // No mute branch: a kind with no path here raises rather than falling to
  // SELECT — a future kind is a loud failure, not a silent retrieval.
  case Query.Kind of
    TQueryKind.Delete: Result := CompileDelete(C, Query.Clauses);
    TQueryKind.Update: Result := CompileUpdate(C, Query.Clauses);
    TQueryKind.Insert: Result := CompileInsert(C, Query.Clauses);
    TQueryKind.Select: Result := CompileSubQuery(C, Query);
  else
    raise ESQLCuteCompilerError.CreateFmt(
      'No SQL path for statement kind %s.', [QueryKindName(Query.Kind)]);
  end;
end;

// ---------------------------------------------------------------------------
//  CompileSubQuery — the SELECT assembly, shared by a whole query and by every
//  subquery, all adding their bindings to the same compilation
// ---------------------------------------------------------------------------

function TAnsiSqlCompiler.CompileSubQuery(const C: TSqlCompilation; const Query: IQuery): string;
var
  Clauses: TArray<TAbstractClause>;
  Parts: TArray<string>;
  WithPart, UnionPart: string;
begin
  Clauses := Query.Clauses;

  WithPart := CompileWith(C, Clauses);

  SetLength(Parts, 9);
  Parts[0] := CompileSelect(C, Clauses);
  Parts[1] := CompileFrom(C, Clauses);
  Parts[2] := CompileJoin(C, Clauses);
  Parts[3] := CompileWhere(C, Clauses);
  Parts[4] := CompileGroupBy(C, Clauses);
  Parts[5] := CompileHaving(C, Clauses);
  Parts[6] := CompileOrderBy(C, Clauses);
  Parts[7] := CompileLimit(C, Clauses);
  Parts[8] := CompileOffset(C, Clauses);

  UnionPart := CompileUnion(C, Clauses);
  Result    := AssembleQuery(Parts);

  if WithPart <> '' then
    Result := WithPart + ' ' + Result;
  if UnionPart <> '' then
    Result := Result + ' ' + UnionPart;
end;

// ---------------------------------------------------------------------------
//  compile methods
// ---------------------------------------------------------------------------

function TAnsiSqlCompiler.CompileJoin(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string;
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
        TableExpr := WithAlias('(' + CompileSubQuery(C, JS.SubQuery as IQuery) + ')', JS.Alias);
        Sb.Append(TableExpr);
        if Length(JS.ConditionClauses) > 0 then
          Sb.Append(' ON (' + CompileWhereList(C, JS.ConditionClauses) + ')');
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
        TableExpr := WithAlias(TableExpr, J.Alias);

      Sb.Append(TableExpr);

      if Length(J.ConditionClauses) > 0 then
        Sb.Append(' ON (' + CompileWhereList(C, J.ConditionClauses) + ')')
      else if J.Col1 <> '' then
        Sb.Append(' ON ' + WrapColumn(J.Col1) + ' ' + OperatorSymbol(J.Op) + ' ' + WrapColumn(J.Col2))
      else if J.Condition <> '' then
        Sb.Append(' ON ' + J.Condition);
    end;
    Result := Sb.ToString;
  finally
    Sb.Free;
  end;
end;

function TAnsiSqlCompiler.CompileGroupBy(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string;
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

function TAnsiSqlCompiler.CompileHaving(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string;
begin
  // Built from the same shared normalizer as WHERE, so precedence (AND before
  // OR) and parenthesization are identical. HAVING leaves emit their column
  // verbatim (aggregate expressions like COUNT(*)) — see EmitLeafClause.
  var Root := TWhereTree.BuildHaving(Clauses);
  try
    if Root = nil then
      Result := ''
    else
      Result := 'HAVING ' + EmitNode(C, Root);
  finally
    Root.Free;
  end;
end;

function TAnsiSqlCompiler.CompileWith(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string;
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
            C.AddBinding(B);
          Parts.Add(W.Name + ' AS (' + W.RawSql + ')');
        end
        else
          Parts.Add(W.Name + ' AS (' + CompileSubQuery(C, W.SubQuery as IQuery) + ')');
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

function TAnsiSqlCompiler.CompileUnion(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string;
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
      SubSQL := CompileSubQuery(C, U.SubQuery as IQuery);
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

function TAnsiSqlCompiler.CompileInsert(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string;
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
            + CompileSubQuery(C, IC.SubQuery as IQuery);
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
        C.AddBinding(IC.Rows[I][J]);
        RowStr := RowStr + '?';
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

function TAnsiSqlCompiler.CompileUpdate(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string;
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
          C.AddBinding(US.Value);
          SetParts.Add(WrapColumn(US.Column) + ' = ?');
        end;
      end;

    SetStr := '';
    for I := 0 to SetParts.Count - 1 do
    begin
      if I > 0 then SetStr := SetStr + ', ';
      SetStr := SetStr + SetParts[I];
    end;

    WherePart := CompileWhere(C, Clauses);
    Result := 'UPDATE ' + TableName + ' SET ' + SetStr;
    if WherePart <> '' then
      Result := Result + ' ' + WherePart;
  finally
    SetParts.Free;
  end;
end;

function TAnsiSqlCompiler.CompileDelete(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string;
var
  Clause: TAbstractClause;
  TableName, WherePart: string;
begin
  TableName := '';
  for Clause in Clauses do
    if (Clause is TFromClause) and (TableName = '') then
      TableName := WrapTable(TFromClause(Clause).Table);

  WherePart := CompileWhere(C, Clauses);
  Result := 'DELETE FROM ' + TableName;
  if WherePart <> '' then
    Result := Result + ' ' + WherePart;
end;

// ---------------------------------------------------------------------------
//  Date / time WHERE — ANSI fallback
// ---------------------------------------------------------------------------

function TAnsiSqlCompiler.CompileDateWhere(const C: TSqlCompilation; const Clause: TDateWhereClause): string;
begin
  C.AddBinding(Clause.Value);
  Result := WrapColumn(Clause.Column) + ' ' + OperatorSymbol(Clause.Op) + ' ?';
end;

end.
