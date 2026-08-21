program LoggingSpecs;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.MiniSpec,
  BootstrapChannel.Feat in 'BootstrapChannel.Feat.pas',
  BootstrapSinks.Feat in 'BootstrapSinks.Feat.pas',
  PipelineComposition.Feat in 'PipelineComposition.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('Logging')
    .Run;
end.
