program NNLogSpecs;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.MiniSpec,
  DAF.NNLog.Targets.Probe in 'DAF.NNLog.Targets.Probe.pas',
  RuleSelection.Feat in 'RuleSelection.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('NNLog')
    .Run;
end.
