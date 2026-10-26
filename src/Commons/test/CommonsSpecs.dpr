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
  MissingMacros.Feat in 'MissingMacros.Feat.pas',
  Rtti.Feat in 'Rtti.Feat.pas',
  PropertyAssign.Feat in 'PropertyAssign.Feat.pas',
  Version.Feat in 'Version.Feat.pas',
  EnvVars.Feat in 'EnvVars.Feat.pas',
  Cancellation.Feat in 'Cancellation.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('Commons')
    .Run;
end.
