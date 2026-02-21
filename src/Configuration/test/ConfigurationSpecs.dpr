program ConfigurationSpecs;

{$APPTYPE CONSOLE}

uses
  Daf.MiniSpec,
  Configuration.Feat in 'Configuration.Feat.pas',
  Configuration.Steps in 'Configuration.Steps.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('Configuration')
    .Run;
end.
