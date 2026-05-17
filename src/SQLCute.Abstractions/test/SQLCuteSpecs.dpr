program SQLCuteSpecs;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.MiniSpec,
  Daf.SQLCute.Core in '..\..\SQLCute\Daf.SQLCute.Core.pas',
  SQLCute.SpecHelpers in 'SQLCute.SpecHelpers.pas',
  SQLCute.Select.Feat in 'SQLCute.Select.Feat.pas',
  SQLCute.Where.Feat in 'SQLCute.Where.Feat.pas',
  SQLCute.Join.Feat in 'SQLCute.Join.Feat.pas',
  SQLCute.DML.Feat in 'SQLCute.DML.Feat.pas',
  SQLCute.Phase1.Feat in 'SQLCute.Phase1.Feat.pas',
  SQLCute.Groups.Feat in 'SQLCute.Groups.Feat.pas',
  SQLCute.StringOps.Feat in 'SQLCute.StringOps.Feat.pas',
  SQLCute.Steps.Core in 'SQLCute.Steps.Core.pas',
  SQLCute.Steps.Joins in 'SQLCute.Steps.Joins.pas',
  SQLCute.Steps.Aggregation in 'SQLCute.Steps.Aggregation.pas',
  SQLCute.Steps.SetOps in 'SQLCute.Steps.SetOps.pas',
  SQLCute.Steps.Subqueries in 'SQLCute.Steps.Subqueries.pas',
  SQLCute.Steps.Dml in 'SQLCute.Steps.Dml.pas',
  SQLCute.Steps.WhereIn in 'SQLCute.Steps.WhereIn.pas',
  SQLCute.Steps.WhereAdvanced in 'SQLCute.Steps.WhereAdvanced.pas',
  SQLCute.Steps.StringOps in 'SQLCute.Steps.StringOps.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('SQLCute')
    .Run;
end.
