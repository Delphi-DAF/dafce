unit Daf.SQLCute.Compiler.SQLite;

interface

uses
  Daf.SQLCute,
  Daf.SQLCute.Clauses,
  Daf.SQLCute.Compiler;

type
  TSQLiteCompiler = class(TAnsiSqlCompiler)
  protected
    function WrapIdentifier(const Name: string): string; override;
    function CompileDateWhere(const C: TSqlCompilation; const Clause: TDateWhereClause): string; override;
  end;

implementation

uses
  System.SysUtils;

function TSQLiteCompiler.WrapIdentifier(const Name: string): string;
begin
  Result := '"' + StringReplace(Name, '"', '""', [rfReplaceAll]) + '"';
end;

function TSQLiteCompiler.CompileDateWhere(const C: TSqlCompilation; const Clause: TDateWhereClause): string;
const
  StrftimeFmt: array[TDatePart] of string = (
    '', '', '%Y', '%m', '%d', '%H', '%M');
var
  ColExpr: string;
begin
  C.AddBinding(Clause.Value);
  case Clause.DatePart of
    TDatePart.dpDate:
      ColExpr := 'date(' + WrapColumn(Clause.Column) + ')';
    TDatePart.dpTime:
      ColExpr := 'time(' + WrapColumn(Clause.Column) + ')';
    else
      ColExpr := 'strftime(''' + StrftimeFmt[Clause.DatePart] + ''', ' + WrapColumn(Clause.Column) + ')';
  end;
  Result := ColExpr + ' ' + OperatorSymbol(Clause.Op) + ' ?';
end;

end.
