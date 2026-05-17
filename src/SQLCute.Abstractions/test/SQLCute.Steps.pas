unit SQLCute.Steps;

{
  Facade unit — re-exports all themed step units.
  All step registration is done in the individual themed units via initialization.
}

interface

implementation

uses
  SQLCute.Steps.Core,
  SQLCute.Steps.Joins,
  SQLCute.Steps.Aggregation,
  SQLCute.Steps.SetOps,
  SQLCute.Steps.Subqueries,
  SQLCute.Steps.Dml,
  SQLCute.Steps.WhereIn,
  SQLCute.Steps.WhereAdvanced,
  SQLCute.Steps.StringOps;

end.
