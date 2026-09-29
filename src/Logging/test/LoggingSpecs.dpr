program LoggingSpecs;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.MiniSpec,
  BootstrapChannel.Feat in 'BootstrapChannel.Feat.pas',
  BootstrapSinks.Feat in 'BootstrapSinks.Feat.pas',
  PipelineComposition.Feat in 'PipelineComposition.Feat.pas',
  TypedLogger.Feat in 'TypedLogger.Feat.pas',
  EntryCost.Feat in 'EntryCost.Feat.pas',
  EntryScope.Feat in 'EntryScope.Feat.pas',
  TemplateAnalysis.Feat in 'TemplateAnalysis.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('Logging')
    .Run;
end.
