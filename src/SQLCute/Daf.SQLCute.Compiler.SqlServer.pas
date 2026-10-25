unit Daf.SQLCute.Compiler.SqlServer;

interface

uses
  Daf.SQLCute,
  Daf.SQLCute.Clauses,
  Daf.SQLCute.Compiler;

type
  TSqlServerCompiler = class(TAnsiSqlCompiler)
  protected
    function WrapColumn(const Col: string): string; override;
    function WrapTable(const Table: string): string; override;
    function FormatPlaceholder(const Index: Integer): string; override;
    function CompileSelect(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string; override;
    function CompileLimit(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string; override;
    function CompileOffset(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string; override;
    function CompileDateWhere(const C: TSqlCompilation; const Clause: TDateWhereClause): string; override;
    /// <summary>
    /// T-SQL has no <c>NULLS FIRST/LAST</c>, and no boolean expression is
    /// allowed in <c>ORDER BY</c>, so a requested placement is emulated with a
    /// leading <c>CASE</c> key: 1 for NULL and 0 otherwise, sorted DESC for
    /// nulls-first and ASC for nulls-last. Adds no binding.
    /// </summary>
    function CompileOrderByColumn(const C: TSqlCompilation; const O: TOrderByClause): string; override;
  end;

implementation

uses
  SysUtils;

function TSqlServerCompiler.WrapColumn(const Col: string): string;
begin
  Result := '[' + Col + ']';
end;

function TSqlServerCompiler.WrapTable(const Table: string): string;
begin
  Result := '[' + Table + ']';
end;

function TSqlServerCompiler.FormatPlaceholder(const Index: Integer): string;
begin
  Result := '@p' + IntToStr(Index);
end;

function TSqlServerCompiler.CompileOrderByColumn(const C: TSqlCompilation; const O: TOrderByClause): string;
var
  Col: string;
begin
  if O.Nulls = TNullsOrder.Default then
    Exit(inherited CompileOrderByColumn(C, O));
  Col := WrapColumn(O.Column);
  Result := 'CASE WHEN ' + Col + ' IS NULL THEN 1 ELSE 0 END ';
  if O.Nulls = TNullsOrder.First then
    Result := Result + 'DESC'
  else
    Result := Result + 'ASC';
  Result := Result + ', ' + Col + ' ' + SortDirKeyword(O.Direction);
end;

function TSqlServerCompiler.CompileSelect(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string;
var
  Clause: TAbstractClause;
  HasLimit, HasOffset: Boolean;
  LimitVal: Integer;
begin
  HasLimit  := False;
  HasOffset := False;
  LimitVal  := 0;
  for Clause in Clauses do
  begin
    if Clause is TLimitClause then
    begin
      HasLimit := True;
      LimitVal := TLimitClause(Clause).Value;
    end;
    if Clause is TOffsetClause then
      HasOffset := True;
  end;
  Result := inherited CompileSelect(C, Clauses);
  // Inject TOP (N) right after SELECT or SELECT DISTINCT — only when no OFFSET
  if HasLimit and not HasOffset then
  begin
    if Copy(Result, 1, 15) = 'SELECT DISTINCT' then
      Insert('TOP (' + IntToStr(LimitVal) + ') ', Result, 17)
    else
      Insert('TOP (' + IntToStr(LimitVal) + ') ', Result, 8);
  end;
end;

function TSqlServerCompiler.CompileLimit(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string;
begin
  // SQL Server uses TOP (injected in SELECT) or OFFSET-FETCH — never LIMIT keyword
  Result := '';
end;

function TSqlServerCompiler.CompileOffset(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string;
var
  Clause: TAbstractClause;
  OffsetVal, LimitVal: Integer;
  HasOffset: Boolean;
begin
  HasOffset := False;
  OffsetVal := 0;
  LimitVal  := 2147483647;
  for Clause in Clauses do
  begin
    if Clause is TOffsetClause then
    begin
      HasOffset := True;
      OffsetVal := TOffsetClause(Clause).Value;
    end;
    if Clause is TLimitClause then
    begin
      LimitVal := TLimitClause(Clause).Value;
    end;
  end;
  if not HasOffset then
    Result := ''
  else
    Result := 'OFFSET ' + IntToStr(OffsetVal) + ' ROWS FETCH NEXT ' + IntToStr(LimitVal) + ' ROWS ONLY';
end;

function TSqlServerCompiler.CompileDateWhere(const C: TSqlCompilation; const Clause: TDateWhereClause): string;
const
  DatePartName: array[TDatePart] of string = (
    '', '', 'year', 'month', 'day', 'hour', 'minute');
var
  ColExpr: string;
begin
  C.AddBinding(Clause.Value);
  case Clause.DatePart of
    TDatePart.dpDate:
      ColExpr := 'CAST(' + WrapColumn(Clause.Column) + ' AS date)';
    TDatePart.dpTime:
      ColExpr := 'CAST(' + WrapColumn(Clause.Column) + ' AS time)';
    else
      ColExpr := 'DATEPART(' + DatePartName[Clause.DatePart] + ', ' + WrapColumn(Clause.Column) + ')';
  end;
  Result := ColExpr + ' ' + Clause.Op + ' ' + '?';
end;

end.
