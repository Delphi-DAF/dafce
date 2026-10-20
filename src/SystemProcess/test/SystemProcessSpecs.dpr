program SystemProcessSpecs;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.MiniSpec,
  Launch.Feat in 'Launch.Feat.pas',
  Process.Feat in 'Process.Feat.pas',
  StandardStreams.Feat in 'StandardStreams.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('SystemProcess')
    .Run;
end.
