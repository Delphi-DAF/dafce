unit Daf.SQLCute.Compiler.Firebird;

interface

uses
  Daf.SQLCute,
  Daf.SQLCute.Clauses,
  Daf.SQLCute.Compiler;

type
  TFirebirdCompiler = class(TAnsiSqlCompiler)
  protected
    function WrapColumn(const Col: string): string; override;
    function WrapTable(const Table: string): string; override;
    function CompileLimit(const Clauses: TArray<TAbstractClause>): string; override;
    function CompileOffset(const Clauses: TArray<TAbstractClause>): string; override;
    function CompileDateWhere(const Clause: TDateWhereClause): string; override;
  end;

implementation

uses
  SysUtils;

function TFirebirdCompiler.WrapColumn(const Col: string): string;
begin
  Result := Col;
end;

function TFirebirdCompiler.WrapTable(const Table: string): string;
begin
  Result := Table;
end;

function TFirebirdCompiler.CompileLimit(const Clauses: TArray<TAbstractClause>): string;
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
  // When offset is also present, CompileOffset emits the combined ROWS M TO N clause
  if (LimitVal > 0) and not HasOffset then
    Result := 'ROWS 1 TO ' + IntToStr(LimitVal)
  else
    Result := '';
end;

function TFirebirdCompiler.CompileOffset(const Clauses: TArray<TAbstractClause>): string;
var
  Clause: TAbstractClause;
  OffsetVal, LimitVal: Integer;
  HasOffset, HasLimit: Boolean;
begin
  HasOffset := False;
  HasLimit  := False;
  OffsetVal := 0;
  LimitVal  := 0;
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
  else if HasLimit then
    Result := 'ROWS ' + IntToStr(OffsetVal + 1) + ' TO ' + IntToStr(OffsetVal + LimitVal)
  else
    Result := 'ROWS ' + IntToStr(OffsetVal + 1) + ' TO 2147483647';
end;

function TFirebirdCompiler.CompileDateWhere(const Clause: TDateWhereClause): string;
const
  ExtractPart: array[TDatePart] of string = (
    '', '', 'YEAR', 'MONTH', 'DAY', 'HOUR', 'MINUTE');
var
  ColExpr: string;
begin
  AddBinding(Clause.Value);
  case Clause.DatePart of
    TDatePart.dpDate:
      ColExpr := 'CAST(' + WrapColumn(Clause.Column) + ' AS DATE)';
    TDatePart.dpTime:
      ColExpr := 'CAST(' + WrapColumn(Clause.Column) + ' AS TIME)';
    else
      ColExpr := 'EXTRACT(' + ExtractPart[Clause.DatePart] + ' FROM ' + WrapColumn(Clause.Column) + ')';
  end;
  Result := ColExpr + ' ' + Clause.Op + ' ' + ParamPlaceholder;
end;

end.
