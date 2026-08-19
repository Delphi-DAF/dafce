program LoggingSpecs;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.MiniSpec,
  DAF.Extensions.Logging.PlatformSink in '..\..\Logging.Abstractions\DAF.Extensions.Logging.PlatformSink.pas',
  BootstrapChannel.Feat in 'BootstrapChannel.Feat.pas',
  BootstrapSinks.Feat in 'BootstrapSinks.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('Logging')
    .Run;
end.
