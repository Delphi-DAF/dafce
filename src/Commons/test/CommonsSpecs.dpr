program CommonsSpecs;

{$APPTYPE CONSOLE}

uses
  Daf.MiniSpec,
  Activator.Feat in 'Activator.Feat.pas',
  CmdLnParser.Feat in 'CmdLnParser.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('Commons')
    .Run;
end.
