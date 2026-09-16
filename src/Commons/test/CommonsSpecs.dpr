program CommonsSpecs;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.MiniSpec,
  Activator.Feat in 'Activator.Feat.pas',
  Activator.Steps in 'Activator.Steps.pas',
  Nullable.Feat in 'Nullable.Feat.pas',
  Enumerable.Feat in 'Enumerable.Feat.pas',
  Comparers.Feat in 'Comparers.Feat.pas',
  Arrays.Feat in 'Arrays.Feat.pas',
  Macros.Feat in 'Macros.Feat.pas',
  Rtti.Feat in 'Rtti.Feat.pas',
  Version.Feat in 'Version.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('Commons')
    .Run;
end.
