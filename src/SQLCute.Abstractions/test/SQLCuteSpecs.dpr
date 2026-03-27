program SQLCuteSpecs;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.MiniSpec,
  SQLCute.SpecHelpers in 'SQLCute.SpecHelpers.pas',
  SQLCute.Select.Feat in 'SQLCute.Select.Feat.pas',
  SQLCute.Where.Feat in 'SQLCute.Where.Feat.pas',
  SQLCute.Steps in 'SQLCute.Steps.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('SQLCute')
    .Run;
end.
