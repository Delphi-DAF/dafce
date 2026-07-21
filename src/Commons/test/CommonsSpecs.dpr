program CommonsSpecs;

{$APPTYPE CONSOLE}

uses
  Daf.MiniSpec,
  Activator.Feat in 'Activator.Feat.pas',
  Activator.Steps in 'Activator.Steps.pas',
  CmdLnParser.Feat in 'CmdLnParser.Feat.pas',
  CmdLnParser.Steps in 'CmdLnParser.Steps.pas',
  Nullable.Feat in 'Nullable.Feat.pas',
  Enumerable.Feat in 'Enumerable.Feat.pas',
  Comparers.Feat in 'Comparers.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('Commons')
    .Run;
end.
