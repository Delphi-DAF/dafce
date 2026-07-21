program LinqSpecs;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.MiniSpec,
  Queryable.Feat in 'Queryable.Feat.pas',
  Operators.Feat in 'Operators.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('Linq')
    .Run;
end.
