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
    /// <summary>
    /// MySQL has no <c>NULLS FIRST/LAST</c>, so a requested placement is
    /// emulated with a leading boolean sort key: <c>col IS NULL</c> is 1 for
    /// NULL and 0 otherwise, so sorting it DESC puts NULLs first and ASC puts
    /// them last. Adds no binding.
    /// </summary>
    function CompileOrderByColumn(const O: TOrderByClause): string; override;
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

function TMySqlCompiler.CompileOrderByColumn(const O: TOrderByClause): string;
var
  Col: string;
begin
  if O.Nulls = TNullsOrder.Default then
    Exit(inherited CompileOrderByColumn(O));
  Col := WrapColumn(O.Column);
  if O.Nulls = TNullsOrder.First then
    Result := '(' + Col + ' IS NULL) DESC'
  else
    Result := '(' + Col + ' IS NULL) ASC';
  Result := Result + ', ' + Col + ' ' + SortDirKeyword(O.Direction);
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
