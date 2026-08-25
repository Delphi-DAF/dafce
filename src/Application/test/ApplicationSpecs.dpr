program ApplicationSpecs;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.MiniSpec,
  Doubles.Feat in 'Doubles.Feat.pas',
  Startup.Feat in 'Startup.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('Application')
    .Run;
end.
