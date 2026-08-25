unit Help.Feat;

interface

uses
  Daf.CommandLine;

type
  THelpWorld = class
  public
    Root: IRootCommand;
    Build: ICommand;
    Serve: ICommand;
    VerboseOpt: IOption<Boolean>;
    FileOpt: IOption<string>;
    PortOpt: IOption<Integer>;
    TargetOpt: IOption<string>;
    // A flat grammar that declares a --help of its own, so what wins can be asserted
    // without a missing-subcommand error in the way.
    OwnRoot: IRootCommand;
    OwnHelpOpt: IOption<Boolean>;
    // La versión vive en la raíz, así que necesita su propia gramática con subcomando
    // para poder afirmar que un subcomando NO la responde.
    VersionRoot: IRootCommand;
    VersionBuild: ICommand;
    DefaultedOpt: IOption<string>;
    OwnVersionRoot: IRootCommand;
    OwnVersionOpt: IOption<Boolean>;
    // A hidden option next to one that is not, which is the only way to tell "not
    // listed" from "nothing was listed".
    HiddenRoot: IRootCommand;
    DiagOpt: IOption<Integer>;
    PlainOpt: IOption<string>;
    Parsed: ICommandLineParseResult;
    Rendered: string;
    RenderedAgain: string;
    Listed: string;
    ReadDiag: Integer;
    procedure BuildGrammar;
    procedure BuildOwnHelpGrammar;
    procedure BuildVersionGrammar;
    procedure BuildOwnVersionGrammar;
    procedure BuildHiddenGrammar;
  end;

implementation

uses
  System.SysUtils,
  Daf.MiniSpec;

{ THelpWorld }

procedure THelpWorld.BuildGrammar;
begin
  VerboseOpt := CommandLine.Option<Boolean>('--verbose').Describe('Show more detail').Recursive;
  FileOpt := CommandLine.Option<string>('--file').Describe('File to compile');
  PortOpt := CommandLine.Option<Integer>('--port').Describe('Port to listen on');
  Build := CommandLine.Command('build').Describe('Build things').Add(FileOpt);
  Serve := CommandLine.Command('serve').Describe('Serve things').Add(PortOpt);
  Root := CommandLine.Root('mytool', 'A tool that builds things');
  Root.Add([VerboseOpt, Build, Serve]);
end;

procedure THelpWorld.BuildOwnHelpGrammar;
begin
  OwnHelpOpt := CommandLine.Option<Boolean>('--help').Describe('The help this application wants');
  OwnRoot := CommandLine.Root('owntool', 'Brings its own help');
  OwnRoot.Add(OwnHelpOpt);
end;

procedure THelpWorld.BuildVersionGrammar;
begin
  DefaultedOpt := CommandLine.Option<string>('--out', 'Where it goes').WithDefault('bin\out.exe');
  VersionBuild := CommandLine.Command('build', 'Build things').Add(DefaultedOpt);
  VersionRoot := CommandLine.Root('mytool', 'A tool with a version').WithVersion('1.2.3');
  VersionRoot.Add(VersionBuild);
end;

procedure THelpWorld.BuildOwnVersionGrammar;
begin
  OwnVersionOpt := CommandLine.Option<Boolean>('--version', 'The version this application wants');
  OwnVersionRoot := CommandLine.Root('owntool', 'Brings its own version').WithVersion('9.9.9');
  OwnVersionRoot.Add(OwnVersionOpt);
end;

procedure THelpWorld.BuildHiddenGrammar;
begin
  // Hidden touches the help and nothing else: it is declared, parsed and read the same.
  DiagOpt := CommandLine.Option<Integer>('--diag').Describe('Diagnostics level').Hidden;
  PlainOpt := CommandLine.Option<string>('--name').Describe('What to call it');
  HiddenRoot := CommandLine.Root('mytool', 'A tool with something not advertised');
  HiddenRoot.Add([DiagOpt, PlainOpt]);
end;

initialization

Feature('''
Feature Command line help @commandline @help

  As a developer writing a console application
  I want the help to be generated from the grammar I declared
  So it cannot contradict what the program actually accepts
''')

.UseWorld<THelpWorld>

.Rule('the help is generated from the declared grammar')

  .Scenario('The help of the tool')
    .Given('a root command with subcommands and options, each with its description',
      procedure(W: THelpWorld)
      begin
        W.BuildGrammar;
      end)
    .When('the help of the root command is asked for',
      procedure(W: THelpWorld)
      begin
        W.Rendered := W.Root.HelpText;
      end)
    .&Then('the text names the subcommands and the options with their descriptions',
      procedure(W: THelpWorld)
      begin
        Expect(W.Rendered).ToContain('build');
        Expect(W.Rendered).ToContain('Build things');
        Expect(W.Rendered).ToContain('serve');
        Expect(W.Rendered).ToContain('Serve things');
        Expect(W.Rendered).ToContain('--verbose');
        Expect(W.Rendered).ToContain('Show more detail');
      end)
    .&And('the only text the application wrote is those descriptions -- it composed no help of its own',
      procedure(W: THelpWorld)
      begin
        // Usage and the section headings come from the renderer walking the tree.
        // Nothing here was declared by the application.
        Expect(W.Rendered).ToContain('Usage:');
        Expect(W.Rendered).ToContain('mytool');
        Expect(W.Rendered).ToContain('Commands:');
        Expect(W.Rendered).ToContain('Options:');
      end)

  .Scenario('The help of a subcommand')
    .Given('a subcommand with options of its own',
      procedure(W: THelpWorld)
      begin
        W.BuildGrammar;
      end)
    .When('the help of that subcommand is asked for',
      procedure(W: THelpWorld)
      begin
        W.Rendered := W.Build.HelpText;
      end)
    .&Then('the text describes its options, and not its siblings',
      procedure(W: THelpWorld)
      begin
        Expect(W.Rendered).ToContain('--file');
        Expect(W.Rendered).ToContain('File to compile');
        Expect(W.Rendered).ToContain('mytool build');
      end)
    .&And('the option belonging to the sibling does not appear',
      procedure(W: THelpWorld)
      begin
        Expect(W.Rendered.Contains('--port')).ToBeFalse;
        Expect(W.Rendered.Contains('Port to listen on')).ToBeFalse;
      end)

  .Scenario('The default value appears in the help')
    .Given('an option declared with a default value',
      procedure(W: THelpWorld)
      begin
        W.BuildVersionGrammar;
      end)
    .When('the help of the command that declares it is asked for',
      procedure(W: THelpWorld)
      begin
        W.Rendered := W.VersionBuild.HelpText;
      end)
    .&Then('the text shows that default, without the application having written it',
      procedure(W: THelpWorld)
      begin
        Expect(W.Rendered).ToContain('default:');
        Expect(W.Rendered).ToContain('bin\out.exe');
      end)

  .Scenario('An option added shows up without touching the help')
    .Given('a grammar whose help has already been asked for once',
      procedure(W: THelpWorld)
      begin
        W.BuildGrammar;
        W.Rendered := W.Build.HelpText;
        Expect(W.Rendered.Contains('--target')).ToBeFalse;
      end)
    .When('a new option is declared and the help is asked for again',
      procedure(W: THelpWorld)
      begin
        W.TargetOpt := CommandLine.Option<string>('--target').Describe('Where it goes');
        W.Build.Add(W.TargetOpt);
        W.RenderedAgain := W.Build.HelpText;
      end)
    .&Then('the new option appears, with no text edited anywhere',
      procedure(W: THelpWorld)
      begin
        Expect(W.RenderedAgain).ToContain('--target');
        Expect(W.RenderedAgain).ToContain('Where it goes');
      end)

.Rule('--help comes as standard')

  .Scenario('Asking for the help of the root command')
    .Given('a grammar that declares no help option',
      procedure(W: THelpWorld)
      begin
        W.BuildGrammar;
      end)
    .When('an invocation passing --help is parsed',
      procedure(W: THelpWorld)
      begin
        W.Parsed := W.Root.Parse(['--help']);
      end)
    .&Then('the result says the help was asked for, and carries its text',
      procedure(W: THelpWorld)
      begin
        Expect(W.Parsed.HelpRequested).ToBeTrue;
        Expect(W.Parsed.HelpText).ToContain('Usage:');
        Expect(W.Parsed.HelpText).ToContain('build');
      end)
    .&And('no invocation error is reported -- asking is not getting it wrong',
      procedure(W: THelpWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.Parsed.Succeeded).ToBeTrue;
      end)

  .Scenario('Asking for the help of a leaf')
    .Given('a grammar with subcommands',
      procedure(W: THelpWorld)
      begin
        W.BuildGrammar;
      end)
    .When('an invocation names a subcommand and passes --help',
      procedure(W: THelpWorld)
      begin
        W.Parsed := W.Root.Parse(['build', '--help']);
      end)
    .&Then('the help obtained is that subcommand''s',
      procedure(W: THelpWorld)
      begin
        Expect(W.Parsed.HelpRequested).ToBeTrue;
        Expect(W.Parsed.Command.Name).ToEqual('build');
        Expect(W.Parsed.HelpText).ToContain('--file');
        Expect(W.Parsed.HelpText.Contains('--port')).ToBeFalse;
      end)

  .Scenario('A grammar that declares a help option of its own')
    .Given('a grammar that declares an option called --help on purpose',
      procedure(W: THelpWorld)
      begin
        W.BuildOwnHelpGrammar;
      end)
    .When('an invocation passing it is parsed',
      procedure(W: THelpWorld)
      begin
        W.Parsed := W.OwnRoot.Parse(['--help']);
      end)
    .&Then('the one the application declared wins, since it is the one that knows its case',
      procedure(W: THelpWorld)
      begin
        Expect(W.Parsed.HelpRequested).ToBeFalse;
        Expect(W.Parsed.WasProvided(W.OwnHelpOpt)).ToBeTrue;
        Expect(W.OwnHelpOpt.ValueIn(W.Parsed)).ToBeTrue;
        Expect(W.Parsed.Succeeded).ToBeTrue;
      end)

.Rule('--version comes as standard on the root command')

  .Scenario('Asking for the version')
    .Given('a grammar whose root declares its version and no version option',
      procedure(W: THelpWorld)
      begin
        W.BuildVersionGrammar;
      end)
    .When('an invocation passing --version is parsed',
      procedure(W: THelpWorld)
      begin
        W.Parsed := W.VersionRoot.Parse(['--version']);
      end)
    .&Then('the result says the version was asked for, with its text',
      procedure(W: THelpWorld)
      begin
        Expect(W.Parsed.VersionRequested).ToBeTrue;
        Expect(W.Parsed.VersionText).ToEqual('1.2.3');
      end)
    .&And('no invocation error is reported',
      procedure(W: THelpWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.Parsed.Succeeded).ToBeTrue;
      end)

  .Scenario('The version belongs to the tool, not to each subcommand')
    .Given('a grammar with subcommands',
      procedure(W: THelpWorld)
      begin
        W.BuildVersionGrammar;
      end)
    .When('an invocation names a subcommand and passes --version',
      procedure(W: THelpWorld)
      begin
        W.Parsed := W.VersionRoot.Parse(['build', '--version']);
      end)
    .&Then('the version is not answered, because --version is the root command''s',
      procedure(W: THelpWorld)
      begin
        Expect(W.Parsed.VersionRequested).ToBeFalse;
      end)

  .Scenario('A grammar that declares a version option of its own')
    .Given('a grammar that declares an option called --version on purpose',
      procedure(W: THelpWorld)
      begin
        W.BuildOwnVersionGrammar;
      end)
    .When('an invocation passing it is parsed',
      procedure(W: THelpWorld)
      begin
        W.Parsed := W.OwnVersionRoot.Parse(['--version']);
      end)
    .&Then('the one the application declared wins, as happens with --help',
      procedure(W: THelpWorld)
      begin
        Expect(W.Parsed.VersionRequested).ToBeFalse;
        Expect(W.Parsed.WasProvided(W.OwnVersionOpt)).ToBeTrue;
        Expect(W.OwnVersionOpt.ValueIn(W.Parsed)).ToBeTrue;
        Expect(W.Parsed.Succeeded).ToBeTrue;
      end)

.Rule('a hidden symbol does not appear in the help')

  .Scenario('A hidden option is not listed')
    .Given('a command with one option declared hidden and another that is not',
      procedure(W: THelpWorld)
      begin
        W.BuildHiddenGrammar;
      end)
    .When('the help of that command is asked for',
      procedure(W: THelpWorld)
      begin
        W.Rendered := W.HiddenRoot.HelpText;
        // Named rather than asserted as a boolean, so a failure says WHICH one showed up.
        W.Listed := 'neither';
        if W.Rendered.Contains('--diag') then
          W.Listed := '--diag';
      end)
    .&Then('the one that is not hidden appears and the hidden one does not',
      procedure(W: THelpWorld)
      begin
        Expect(W.Rendered).ToContain('--name');
        Expect(W.Listed).ToEqual('neither');
      end)

  .Scenario('A hidden option still works')
    .Given('a command with an option declared hidden',
      procedure(W: THelpWorld)
      begin
        W.BuildHiddenGrammar;
      end)
    .When('an invocation that passes it is parsed',
      procedure(W: THelpWorld)
      begin
        W.Parsed := W.HiddenRoot.Parse(['--diag', '3']);
        W.ReadDiag := W.DiagOpt.ValueIn(W.Parsed);
      end)
    .&Then('its value is read as usual',
      procedure(W: THelpWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.ReadDiag).ToEqual(3);
      end)

  .Scenario('A hidden option is named when it fails')
    .Given('a command with a hidden option of integer type',
      procedure(W: THelpWorld)
      begin
        W.BuildHiddenGrammar;
      end)
    .When('the invocation passes it something that is not an integer',
      procedure(W: THelpWorld)
      begin
        W.Parsed := W.HiddenRoot.Parse(['--diag', 'loud']);
      end)
    .&Then('the parse fails naming that option',
      procedure(W: THelpWorld)
      begin
        // Hiding it from the help is not hiding it from whoever already uses it: an
        // error that will not say what failed turns a mistake into a riddle (design D5).
        Expect(W.Parsed.Succeeded).ToBeFalse;
        // The message has to be the CONVERSION's, naming the option. Asserting only
        // that '--diag' appears would also be satisfied by "unknown option --diag",
        // which is what a hidden option would produce if hiding reached the parser --
        // a green that coincides with the failure it is meant to catch.
        Expect(W.Parsed.ErrorText).ToContain('not a valid integer');
        Expect(W.Parsed.ErrorText).ToContain('--diag');
        Expect(W.Parsed.ErrorText).ToContain('loud');
      end);

end.
