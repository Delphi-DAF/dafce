unit Daf.SQLCute.Compiler.Oracle;

interface

uses
  Daf.SQLCute,
  Daf.SQLCute.Clauses,
  Daf.SQLCute.Compiler;

type
  TOracleCompiler = class(TAnsiSqlCompiler)
  protected
    function WrapIdentifier(const Name: string): string; override;
    function AliasKeyword: string; override;
    function FormatPlaceholder(const Index: Integer): string; override;
    function CompileLimit(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string; override;
    function CompileOffset(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string; override;
    function CompileDateWhere(const C: TSqlCompilation; const Clause: TDateWhereClause): string; override;
  end;

implementation

uses
  SysUtils;

function TOracleCompiler.WrapIdentifier(const Name: string): string;
begin
  Result := '"' + UpperCase(StringReplace(Name, '"', '""', [rfReplaceAll])) + '"';
end;

function TOracleCompiler.AliasKeyword: string;
begin
  Result := ' ';
end;

function TOracleCompiler.FormatPlaceholder(const Index: Integer): string;
begin
  Result := ':p' + IntToStr(Index + 1);
end;

function TOracleCompiler.CompileLimit(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string;
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

function TOracleCompiler.CompileOffset(const C: TSqlCompilation; const Clauses: TArray<TAbstractClause>): string;
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

function TOracleCompiler.CompileDateWhere(const C: TSqlCompilation; const Clause: TDateWhereClause): string;
const
  ExtractPart: array[TDatePart] of string = (
    '', '', 'YEAR', 'MONTH', 'DAY', 'HOUR', 'MINUTE');
var
  ColExpr: string;
begin
  C.AddBinding(Clause.Value);
  case Clause.DatePart of
    TDatePart.dpDate:
      ColExpr := 'TRUNC(' + WrapColumn(Clause.Column) + ')';
    TDatePart.dpTime:
      // Oracle has no native time extraction function; fall back to plain column
      ColExpr := WrapColumn(Clause.Column);
    else
      ColExpr := 'EXTRACT(' + ExtractPart[Clause.DatePart] + ' FROM ' + WrapColumn(Clause.Column) + ')';
  end;
  Result := ColExpr + ' ' + OperatorSymbol(Clause.Op) + ' ?';
end;

end.
