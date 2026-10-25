program SQLCuteSpecs;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.MiniSpec,
  Daf.SQLCute.Core,
  SQLCute.SpecHelpers in 'SQLCute.SpecHelpers.pas',
  SQLCute.Select.Feat in 'SQLCute.Select.Feat.pas',
  SQLCute.Where.Feat in 'SQLCute.Where.Feat.pas',
  SQLCute.Operators.Feat in 'SQLCute.Operators.Feat.pas',
  SQLCute.WhereTree.Feat in 'SQLCute.WhereTree.Feat.pas',
  SQLCute.Kind.Feat in 'SQLCute.Kind.Feat.pas',
  SQLCute.Factory.Feat in 'SQLCute.Factory.Feat.pas',
  SQLCute.Join.Feat in 'SQLCute.Join.Feat.pas',
  SQLCute.SetOps.Feat in 'SQLCute.SetOps.Feat.pas',
  SQLCute.DML.Feat in 'SQLCute.DML.Feat.pas',
  SQLCute.Groups.Feat in 'SQLCute.Groups.Feat.pas',
  SQLCute.StringOps.Feat in 'SQLCute.StringOps.Feat.pas',
  SQLCute.OrderByNulls.Feat in 'SQLCute.OrderByNulls.Feat.pas',
  SQLCute.Steps.Core in 'SQLCute.Steps.Core.pas',
  SQLCute.Steps.Joins in 'SQLCute.Steps.Joins.pas',
  SQLCute.Steps.Aggregation in 'SQLCute.Steps.Aggregation.pas',
  SQLCute.Steps.SetOps in 'SQLCute.Steps.SetOps.pas',
  SQLCute.Steps.Subqueries in 'SQLCute.Steps.Subqueries.pas',
  SQLCute.Steps.Dml in 'SQLCute.Steps.Dml.pas',
  SQLCute.Steps.WhereIn in 'SQLCute.Steps.WhereIn.pas',
  SQLCute.Steps.WhereAdvanced in 'SQLCute.Steps.WhereAdvanced.pas',
  SQLCute.Steps.StringOps in 'SQLCute.Steps.StringOps.pas',
  // Del runner SQLCuteCompilersSpecs, fusionado aqui: un runner por modulo.
  SQLCute.Compilers.Feat in 'SQLCute.Compilers.Feat.pas',
  SQLCute.Compilers.Steps in 'SQLCute.Compilers.Steps.pas',
  SQLCute.Date.Feat in 'SQLCute.Date.Feat.pas',
  SQLCute.Reentrancy.Feat in 'SQLCute.Reentrancy.Feat.pas',
  SQLCute.Identifiers.Feat in 'SQLCute.Identifiers.Feat.pas',
  SQLCute.Operators.Validation.Feat in 'SQLCute.Operators.Validation.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('SQLCute')
    .Run;
end.
