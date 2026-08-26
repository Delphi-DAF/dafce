program FMXSpecs;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.MiniSpec,
  Daf.FMX.Application,
  FmxVariant.Feat in 'FmxVariant.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('FMX')
    .Run;
end.
