program DISpecs;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.MiniSpec,
  DependencyInjection.Feat in 'DependencyInjection.Feat.pas',
  DependencyInjection.Steps in 'DependencyInjection.Steps.pas',
  Doubles.Feat in 'Doubles.Feat.pas',
  Composition.Feat in 'Composition.Feat.pas',
  Decoration.Feat in 'Decoration.Feat.pas',
  Modules.Feat in 'Modules.Feat.pas',
  Validation.Feat in 'Validation.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('DependencyInjection')
    .Run;
end.
