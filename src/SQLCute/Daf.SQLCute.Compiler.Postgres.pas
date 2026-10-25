unit Daf.SQLCute.Compiler.Postgres;

interface

uses
  Daf.SQLCute,
  Daf.SQLCute.Clauses,
  Daf.SQLCute.Compiler;

type
  TPostgresCompiler = class(TAnsiSqlCompiler)
  protected
    function WrapIdentifier(const Name: string): string; override;
    function FormatPlaceholder(const Index: Integer): string; override;
    function CompileDateWhere(const C: TSqlCompilation; const Clause: TDateWhereClause): string; override;
  end;

implementation

uses
  SysUtils;

function TPostgresCompiler.WrapIdentifier(const Name: string): string;
begin
  Result := '"' + StringReplace(Name, '"', '""', [rfReplaceAll]) + '"';
end;

function TPostgresCompiler.FormatPlaceholder(const Index: Integer): string;
begin
  Result := '$' + IntToStr(Index + 1);
end;

function TPostgresCompiler.CompileDateWhere(const C: TSqlCompilation; const Clause: TDateWhereClause): string;
const
  DatePartName: array[TDatePart] of string = (
    '', '', 'year', 'month', 'day', 'hour', 'minute');
var
  ColExpr: string;
begin
  C.AddBinding(Clause.Value);
  case Clause.DatePart of
    TDatePart.dpDate:
      ColExpr := WrapColumn(Clause.Column) + '::date';
    TDatePart.dpTime:
      ColExpr := WrapColumn(Clause.Column) + '::time';
    else
      ColExpr := 'DATE_PART(''' + DatePartName[Clause.DatePart] + ''', ' + WrapColumn(Clause.Column) + ')';
  end;
  Result := ColExpr + ' ' + Clause.Op + ' ' + '?';
end;

end.
