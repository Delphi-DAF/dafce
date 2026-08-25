program CommandLineSpecs;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.MiniSpec,
  Grammar.Feat in 'Grammar.Feat.pas',
  Help.Feat in 'Help.Feat.pas',
  Values.Feat in 'Values.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('CommandLine')
    .Run;
end.
