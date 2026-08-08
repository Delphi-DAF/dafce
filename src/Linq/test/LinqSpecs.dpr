program LinqSpecs;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.MiniSpec,
  Sequence.Feat in 'Sequence.Feat.pas',
  Operators.Feat in 'Operators.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('Linq')
    .Run;
end.
