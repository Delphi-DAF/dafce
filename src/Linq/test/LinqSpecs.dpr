program LinqSpecs;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.MiniSpec,
  Queryable.Feat in 'Queryable.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('Linq')
    .Run;
end.
