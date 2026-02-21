program CommonsSpecs;

{$APPTYPE CONSOLE}

uses
  Daf.MiniSpec,
  Activator.Feat in 'Activator.Feat.pas',
  Activator.Steps in 'Activator.Steps.pas',
  CmdLnParser.Feat in 'CmdLnParser.Feat.pas',
  CmdLnParser.Steps in 'CmdLnParser.Steps.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('Commons')
    .Run;
end.
