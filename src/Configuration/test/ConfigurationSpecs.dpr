program ConfigurationSpecs;

{$APPTYPE CONSOLE}

uses
  Daf.MiniSpec,
  Configuration.Feat in 'Configuration.Feat.pas',
  Configuration.Steps in 'Configuration.Steps.pas',
  CommandLine.Feat in 'CommandLine.Feat.pas',
  CommandLine.Steps in 'CommandLine.Steps.pas',
  KeyPresence.Feat in 'KeyPresence.Feat.pas',
  BinderAbsence.Feat in 'BinderAbsence.Feat.pas',
  BinderKeyName.Feat in 'BinderKeyName.Feat.pas',
  ConfigManager.Feat in 'ConfigManager.Feat.pas',
  ChainedConfig.Feat in 'ChainedConfig.Feat.pas',
  OptionsBinding.Feat in 'OptionsBinding.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('Configuration')
    .Run;
end.
