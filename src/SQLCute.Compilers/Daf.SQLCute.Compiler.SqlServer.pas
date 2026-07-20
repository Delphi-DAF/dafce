unit Daf.SQLCute.Compiler.SqlServer;

interface

uses
  Daf.SQLCute,
  Daf.SQLCute.Clauses,
  Daf.SQLCute.Compiler;

type
  TSqlServerCompiler = class(TAnsiSqlCompiler)
  private
    FParamIndex: Integer;
  protected
    function WrapColumn(const Col: string): string; override;
    function WrapTable(const Table: string): string; override;
    function ParamPlaceholder: string; override;
    function CompileSelect(const Clauses: TArray<TAbstractClause>): string; override;
    function CompileLimit(const Clauses: TArray<TAbstractClause>): string; override;
    function CompileOffset(const Clauses: TArray<TAbstractClause>): string; override;
    function CompileDateWhere(const Clause: TDateWhereClause): string; override;
  public
    function Compile(const Query: IQuery): TSQLResult;
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

function TSqlServerCompiler.ParamPlaceholder: string;
begin
  Result := '@p' + IntToStr(FParamIndex);
  Inc(FParamIndex);
end;

function TSqlServerCompiler.CompileSelect(const Clauses: TArray<TAbstractClause>): string;
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
  Result := inherited CompileSelect(Clauses);
  // Inject TOP (N) right after SELECT or SELECT DISTINCT — only when no OFFSET
  if HasLimit and not HasOffset then
  begin
    if Copy(Result, 1, 15) = 'SELECT DISTINCT' then
      Insert('TOP (' + IntToStr(LimitVal) + ') ', Result, 17)
    else
      Insert('TOP (' + IntToStr(LimitVal) + ') ', Result, 8);
  end;
end;

function TSqlServerCompiler.CompileLimit(const Clauses: TArray<TAbstractClause>): string;
begin
  // SQL Server uses TOP (injected in SELECT) or OFFSET-FETCH — never LIMIT keyword
  Result := '';
end;

function TSqlServerCompiler.CompileOffset(const Clauses: TArray<TAbstractClause>): string;
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

function TSqlServerCompiler.Compile(const Query: IQuery): TSQLResult;
begin
  FParamIndex := 0;
  Result := inherited Compile(Query);
end;

function TSqlServerCompiler.CompileDateWhere(const Clause: TDateWhereClause): string;
const
  DatePartName: array[TDatePart] of string = (
    '', '', 'year', 'month', 'day', 'hour', 'minute');
var
  ColExpr: string;
begin
  AddBinding(Clause.Value);
  case Clause.DatePart of
    TDatePart.dpDate:
      ColExpr := 'CAST(' + WrapColumn(Clause.Column) + ' AS date)';
    TDatePart.dpTime:
      ColExpr := 'CAST(' + WrapColumn(Clause.Column) + ' AS time)';
    else
      ColExpr := 'DATEPART(' + DatePartName[Clause.DatePart] + ', ' + WrapColumn(Clause.Column) + ')';
  end;
  Result := ColExpr + ' ' + Clause.Op + ' ' + ParamPlaceholder;
end;

end.
