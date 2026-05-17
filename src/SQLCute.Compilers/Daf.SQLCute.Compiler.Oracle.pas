unit Daf.SQLCute.Compiler.Oracle;

interface

uses
  Daf.SQLCute,
  Daf.SQLCute.Clauses,
  Daf.SQLCute.Compiler;

type
  TOracleCompiler = class(TAnsiSqlCompiler)
  private
    FParamIndex: Integer;
  protected
    function WrapColumn(const Col: string): string; override;
    function WrapTable(const Table: string): string; override;
    function ParamPlaceholder: string; override;
    function CompileLimit(const Clauses: TArray<TAbstractClause>): string; override;
    function CompileOffset(const Clauses: TArray<TAbstractClause>): string; override;
    function CompileDateWhere(const Clause: TDateWhereClause): string; override;
  public
    function Compile(const Query: IQuery): TSQLResult;
  end;

implementation

uses
  SysUtils;

function TOracleCompiler.WrapColumn(const Col: string): string;
begin
  Result := '"' + UpperCase(Col) + '"';
end;

function TOracleCompiler.WrapTable(const Table: string): string;
begin
  Result := '"' + UpperCase(Table) + '"';
end;

function TOracleCompiler.ParamPlaceholder: string;
begin
  Inc(FParamIndex);
  Result := ':p' + IntToStr(FParamIndex);
end;

function TOracleCompiler.CompileLimit(const Clauses: TArray<TAbstractClause>): string;
var
  Clause: TAbstractClause;
  LimitVal: Integer;
  HasOffset: Boolean;
begin
  HasOffset := False;
  LimitVal  := 0;
  for Clause in Clauses do
  begin
    if Clause is TLimitClause then
      LimitVal := TLimitClause(Clause).Value;
    if Clause is TOffsetClause then
      HasOffset := True;
  end;
  // When there's also an offset, CompileOffset handles the combined clause
  if (LimitVal > 0) and not HasOffset then
    Result := 'FETCH FIRST ' + IntToStr(LimitVal) + ' ROWS ONLY'
  else
    Result := '';
end;

function TOracleCompiler.CompileOffset(const Clauses: TArray<TAbstractClause>): string;
var
  Clause: TAbstractClause;
  OffsetVal, LimitVal: Integer;
  HasOffset, HasLimit: Boolean;
begin
  HasOffset := False;
  HasLimit  := False;
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
      HasLimit := True;
      LimitVal := TLimitClause(Clause).Value;
    end;
  end;
  if not HasOffset then
    Result := ''
  else
    Result := 'OFFSET ' + IntToStr(OffsetVal) + ' ROWS FETCH NEXT ' + IntToStr(LimitVal) + ' ROWS ONLY';
end;

function TOracleCompiler.Compile(const Query: IQuery): TSQLResult;
begin
  FParamIndex := 0;
  Result := inherited Compile(Query);
end;

function TOracleCompiler.CompileDateWhere(const Clause: TDateWhereClause): string;
const
  ExtractPart: array[TDatePart] of string = (
    '', '', 'YEAR', 'MONTH', 'DAY', 'HOUR', 'MINUTE');
var
  ColExpr: string;
begin
  AddBinding(Clause.Value);
  case Clause.DatePart of
    TDatePart.dpDate:
      ColExpr := 'TRUNC(' + WrapColumn(Clause.Column) + ')';
    TDatePart.dpTime:
      // Oracle has no native time extraction function; fall back to plain column
      ColExpr := WrapColumn(Clause.Column);
    else
      ColExpr := 'EXTRACT(' + ExtractPart[Clause.DatePart] + ' FROM ' + WrapColumn(Clause.Column) + ')';
  end;
  Result := ColExpr + ' ' + Clause.Op + ' ' + ParamPlaceholder;
end;

end.
