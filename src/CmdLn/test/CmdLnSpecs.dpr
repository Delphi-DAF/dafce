program CmdLnSpecs;

{$APPTYPE CONSOLE}

uses
  Daf.MiniSpec,
  CmdLnParser.Feat in 'CmdLnParser.Feat.pas',
  CmdLnParser.Steps in 'CmdLnParser.Steps.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('CmdLn')
    .Run;
end.
