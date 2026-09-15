program NNLogSpecs;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.MiniSpec,
  Daf.NNLog.Targets.Probe in 'Daf.NNLog.Targets.Probe.pas',
  RuleSelection.Feat in 'RuleSelection.Feat.pas',
  CodeConfig.Feat in 'CodeConfig.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('NNLog')
    .Run;
end.
