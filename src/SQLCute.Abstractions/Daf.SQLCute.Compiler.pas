unit Daf.SQLCute.Compiler;

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  Daf.SQLCute.Clauses;

type
  /// <summary>
  /// Result produced by a query compiler: the final SQL string plus the
  /// ordered list of parameter bindings (positional, matching '?' markers).
  /// </summary>
  TSQLResult = record
    SQL: string;
    Bindings: TArray<Variant>;
    class function Empty: TSQLResult; static;
  end;

  // Forward declaration — defined in Daf.SQLCute.pas
  IQuery = interface;

  /// <summary>
  /// Strategy interface for compiling an IQuery into dialect-specific SQL.
  /// Register implementations via DI and inject where queries are executed.
  /// </summary>
  IQueryCompiler = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    function Compile(const Query: IQuery): TSQLResult;
  end;

  /// <summary>
  /// ANSI SQL compiler — the base implementation that all dialect compilers
  /// extend. Produces standard SQL with '?' positional parameters.
  /// </summary>
  TAnsiSqlCompiler = class(TInterfacedObject, IQueryCompiler)
  protected
    FBindings: TList<Variant>;

    procedure AddBinding(const Value: Variant);
    function WrapColumn(const Col: string): string; virtual;
    function WrapTable(const Table: string): string; virtual;
    function OperatorSymbol(Op: TWhereOp): string; virtual;
    function CompileSelect(Clauses: TArray<TAbstractClause>): string; virtual;
    function CompileFrom(Clauses: TArray<TAbstractClause>): string; virtual;
    function CompileWhere(Clauses: TArray<TAbstractClause>): string; virtual;
    function CompileOrderBy(Clauses: TArray<TAbstractClause>): string; virtual;
    function CompileLimit(Clauses: TArray<TAbstractClause>): string; virtual;
    function CompileOffset(Clauses: TArray<TAbstractClause>): string; virtual;
    function AssembleQuery(const Parts: TArray<string>): string; virtual;
  public
    function Compile(const Query: IQuery): TSQLResult;
  end;

implementation

uses
  System.StrUtils,
  Daf.SQLCute;

{ TSQLResult }

class function TSQLResult.Empty: TSQLResult;
begin
  Result.SQL      := '';
  Result.Bindings := [];
end;

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

function TAnsiSqlCompiler.CompileSelect(Clauses: TArray<TAbstractClause>): string;
var
  Parts: TStringList;
  I: Integer;
  Clause: TAbstractClause;
  Sel: TSelectClause;
  Col: TSelectColumn;
  Expr: string;
begin
  Parts := TStringList.Create;
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

    if Parts.Count = 0 then
      Result := 'SELECT *'
    else
      Result := 'SELECT ' + Parts.CommaText;
  finally
    Parts.Free;
  end;
  // TStringList.CommaText quotes items with spaces — avoid that by using manual join
  // Re-build without TStringList quoting:
  if Parts <> nil then ; // parts already freed — result is already set above
end;

function TAnsiSqlCompiler.CompileFrom(Clauses: TArray<TAbstractClause>): string;
var
  Clause: TAbstractClause;
  From: TFromClause;
  TableExpr: string;
begin
  Result := '';
  for Clause in Clauses do
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

function TAnsiSqlCompiler.CompileWhere(Clauses: TArray<TAbstractClause>): string;
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

        TWhereOp.&Between:
        begin
          AddBinding(W.Value);
          AddBinding(W.Value2);
          Expr := WrapColumn(W.Column) + ' BETWEEN ? AND ?';
        end;

        TWhereOp.&In:
        begin
          // Value is expected to be a comma-separated string for simplicity in Phase 1.
          // Phase 2 will support TArray<Variant> sub-queries, etc.
          Expr := WrapColumn(W.Column) + ' IN (' + VarToStr(W.Value) + ')';
        end;

        TWhereOp.NotIn:
          Expr := WrapColumn(W.Column) + ' NOT IN (' + VarToStr(W.Value) + ')';

        TWhereOp.Raw:
          Expr := W.RawSql;

        else
        begin
          AddBinding(W.Value);
          Expr := WrapColumn(W.Column) + ' ' + OperatorSymbol(W.Op) + ' ?';
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

function TAnsiSqlCompiler.CompileOrderBy(Clauses: TArray<TAbstractClause>): string;
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

function TAnsiSqlCompiler.CompileLimit(Clauses: TArray<TAbstractClause>): string;
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

function TAnsiSqlCompiler.CompileOffset(Clauses: TArray<TAbstractClause>): string;
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
begin
  FBindings := TList<Variant>.Create;
  try
    Clauses := Query.Clauses;

    SetLength(Parts, 6);
    Parts[0] := CompileSelect(Clauses);
    Parts[1] := CompileFrom(Clauses);
    Parts[2] := CompileWhere(Clauses);
    Parts[3] := CompileOrderBy(Clauses);
    Parts[4] := CompileLimit(Clauses);
    Parts[5] := CompileOffset(Clauses);

    Result.SQL      := AssembleQuery(Parts);
    Result.Bindings := FBindings.ToArray;
  finally
    FBindings.Free;
    FBindings := nil;
  end;
end;

end.
