unit SQLCute.SpecHelpers;

interface

uses
  Daf.SQLCute,
  Daf.SQLCute.Compiler;

type
  /// <summary>
  /// Shared world for SQLCute BDD scenarios.
  /// Holds the current query under construction and the last compiled result.
  /// </summary>
  TSQLCuteWorld = class
  public
    Query: IQuery;
    LastResult: TSQLResult;
    Compiler: IQueryCompiler;
    constructor Create;
  end;

implementation

constructor TSQLCuteWorld.Create;
begin
  inherited;
  Compiler := TAnsiSqlCompiler.Create;
  Query    := TQuery.New;
end;

end.
