unit Daf.SQLCute.Compiler.SQLite;

interface

uses
  Daf.SQLCute,
  Daf.SQLCute.Clauses,
  Daf.SQLCute.Compiler;

type
  TSQLiteCompiler = class(TAnsiSqlCompiler)
  protected
    function WrapColumn(const Col: string): string; override;
    function WrapTable(const Table: string): string; override;
    function CompileDateWhere(const Clause: TDateWhereClause): string; override;
  end;

implementation

function TSQLiteCompiler.WrapColumn(const Col: string): string;
begin
  Result := '"' + Col + '"';
end;

function TSQLiteCompiler.WrapTable(const Table: string): string;
begin
  Result := '"' + Table + '"';
end;

function TSQLiteCompiler.CompileDateWhere(const Clause: TDateWhereClause): string;
const
  StrftimeFmt: array[TDatePart] of string = (
    '', '', '%Y', '%m', '%d', '%H', '%M');
var
  ColExpr: string;
begin
  AddBinding(Clause.Value);
  case Clause.DatePart of
    TDatePart.dpDate:
      ColExpr := 'date(' + WrapColumn(Clause.Column) + ')';
    TDatePart.dpTime:
      ColExpr := 'time(' + WrapColumn(Clause.Column) + ')';
    else
      ColExpr := 'strftime(''' + StrftimeFmt[Clause.DatePart] + ''', ' + WrapColumn(Clause.Column) + ')';
  end;
  Result := ColExpr + ' ' + Clause.Op + ' ' + ParamPlaceholder;
end;

end.
