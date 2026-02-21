program DISpecs;

{$APPTYPE CONSOLE}

uses
  Daf.MiniSpec,
  DependencyInjection.Feat in 'DependencyInjection.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('DependencyInjection')
    .Run;
end.
