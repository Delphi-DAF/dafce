unit Daf.SQLCute.Compiler.MySql;

interface

uses
  Daf.SQLCute,
  Daf.SQLCute.Clauses,
  Daf.SQLCute.Compiler;

type
  TMySqlCompiler = class(TAnsiSqlCompiler)
  protected
    function WrapColumn(const Col: string): string; override;
    function WrapTable(const Table: string): string; override;
    function CompileDateWhere(const Clause: TDateWhereClause): string; override;
  end;

implementation

function TMySqlCompiler.WrapColumn(const Col: string): string;
begin
  Result := '`' + Col + '`';
end;

function TMySqlCompiler.WrapTable(const Table: string): string;
begin
  Result := '`' + Table + '`';
end;

function TMySqlCompiler.CompileDateWhere(const Clause: TDateWhereClause): string;
const
  PartFunc: array[TDatePart] of string = (
    'DATE', 'TIME', 'YEAR', 'MONTH', 'DAY', 'HOUR', 'MINUTE');
begin
  AddBinding(Clause.Value);
  Result := PartFunc[Clause.DatePart] + '(' + WrapColumn(Clause.Column) + ') ' +
            Clause.Op + ' ' + ParamPlaceholder;
end;

end.
