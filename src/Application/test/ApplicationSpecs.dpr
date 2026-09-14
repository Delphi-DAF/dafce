program ApplicationSpecs;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.MiniSpec,
  Application.Doubles in 'Application.Doubles.pas',
  Startup.Feat in 'Startup.Feat.pas',
  Version.Feat in 'Version.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('Application')
    .Run;
end.
