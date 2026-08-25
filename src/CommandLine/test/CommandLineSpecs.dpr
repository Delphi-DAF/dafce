program CommandLineSpecs;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.MiniSpec,
  Grammar.Feat in 'Grammar.Feat.pas',
  Syntax.Feat in 'Syntax.Feat.pas',
  Tokens.Feat in 'Tokens.Feat.pas',
  Help.Feat in 'Help.Feat.pas',
  Types.Feat in 'Types.Feat.pas',
  Values.Feat in 'Values.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('CommandLine')
    .Run;
end.
