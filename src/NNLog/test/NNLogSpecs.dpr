program NNLogSpecs;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.MiniSpec,
  Daf.NNLog.Targets.Probe in 'Daf.NNLog.Targets.Probe.pas',
  RuleSelection.Feat in 'RuleSelection.Feat.pas',
  CodeConfig.Feat in 'CodeConfig.Feat.pas',
  TargetConcurrency.Feat in 'TargetConcurrency.Feat.pas',
  TargetRecovery.Feat in 'TargetRecovery.Feat.pas',
  FileRotation.Feat in 'FileRotation.Feat.pas',
  LayoutTokens.Feat in 'LayoutTokens.Feat.pas',
  TargetConfiguration.Feat in 'TargetConfiguration.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('NNLog')
    .Run;
end.
