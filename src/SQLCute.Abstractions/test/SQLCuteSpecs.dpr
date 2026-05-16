program SQLCuteSpecs;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.MiniSpec,
  SQLCute.SpecHelpers in 'SQLCute.SpecHelpers.pas',
  SQLCute.Select.Feat in 'SQLCute.Select.Feat.pas',
  SQLCute.Where.Feat in 'SQLCute.Where.Feat.pas',
  SQLCute.Join.Feat in 'SQLCute.Join.Feat.pas',
  SQLCute.DML.Feat in 'SQLCute.DML.Feat.pas',
  SQLCute.Phase1.Feat in 'SQLCute.Phase1.Feat.pas',
  SQLCute.Groups.Feat in 'SQLCute.Groups.Feat.pas',
  SQLCute.Steps in 'SQLCute.Steps.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('SQLCute')
    .Run;
end.
