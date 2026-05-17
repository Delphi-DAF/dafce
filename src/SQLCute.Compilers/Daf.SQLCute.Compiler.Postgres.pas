unit Daf.SQLCute.Compiler.Postgres;

interface

uses
  Daf.SQLCute,
  Daf.SQLCute.Clauses,
  Daf.SQLCute.Compiler;

type
  TPostgresCompiler = class(TAnsiSqlCompiler)
  private
    FParamIndex: Integer;
  protected
    function WrapColumn(const Col: string): string; override;
    function WrapTable(const Table: string): string; override;
    function ParamPlaceholder: string; override;
    function CompileDateWhere(const Clause: TDateWhereClause): string; override;
  public
    function Compile(const Query: IQuery): TSQLResult;
  end;

implementation

uses
  SysUtils;

function TPostgresCompiler.WrapColumn(const Col: string): string;
begin
  Result := '"' + Col + '"';
end;

function TPostgresCompiler.WrapTable(const Table: string): string;
begin
  Result := '"' + Table + '"';
end;

function TPostgresCompiler.ParamPlaceholder: string;
begin
  Inc(FParamIndex);
  Result := '$' + IntToStr(FParamIndex);
end;

function TPostgresCompiler.Compile(const Query: IQuery): TSQLResult;
begin
  FParamIndex := 0;
  Result := inherited Compile(Query);
end;

function TPostgresCompiler.CompileDateWhere(const Clause: TDateWhereClause): string;
const
  DatePartName: array[TDatePart] of string = (
    '', '', 'year', 'month', 'day', 'hour', 'minute');
var
  ColExpr: string;
begin
  AddBinding(Clause.Value);
  case Clause.DatePart of
    TDatePart.dpDate:
      ColExpr := WrapColumn(Clause.Column) + '::date';
    TDatePart.dpTime:
      ColExpr := WrapColumn(Clause.Column) + '::time';
    else
      ColExpr := 'DATE_PART(''' + DatePartName[Clause.DatePart] + ''', ' + WrapColumn(Clause.Column) + ')';
  end;
  Result := ColExpr + ' ' + Clause.Op + ' ' + ParamPlaceholder;
end;

end.
