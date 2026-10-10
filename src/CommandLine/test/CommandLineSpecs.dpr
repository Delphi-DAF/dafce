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
  Values.Feat in 'Values.Feat.pas',
  Invocation.Feat in 'Invocation.Feat.pas',
  FrozenGrammar.Feat in 'FrozenGrammar.Feat.pas',
  HelpActions.Feat in 'HelpActions.Feat.pas',
  ParseRules.Feat in 'ParseRules.Feat.pas',
  Results.Feat in 'Results.Feat.pas',
  Actions.Feat in 'Actions.Feat.pas',
  Validators.Feat in 'Validators.Feat.pas',
  CommandLineText.Feat in 'CommandLineText.Feat.pas',
  NameCase.Feat in 'NameCase.Feat.pas',
  Directives.Feat in 'Directives.Feat.pas',
  Texts.Feat in 'Texts.Feat.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  MiniSpec
    .Category('CommandLine')
    .Run;
end.
