program SQLCuteCompilersSpecs;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.MiniSpec,
  Daf.SQLCute.Core in '..\..\SQLCute\Daf.SQLCute.Core.pas',
  SQLCute.SpecHelpers in '..\..\SQLCute.Abstractions\test\SQLCute.SpecHelpers.pas',
  SQLCute.Compilers.Feat in 'SQLCute.Compilers.Feat.pas',
  SQLCute.Compilers.Steps in 'SQLCute.Compilers.Steps.pas',
  SQLCute.Date.Feat in 'SQLCute.Date.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('SQLCute')
    .Run;
end.
