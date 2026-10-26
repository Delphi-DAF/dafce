unit Help.Feat;

interface

uses
  System.Classes,
  Daf.CommandLine,
  Daf.Application.Version;

type
  // Las clases con atributo van en la interface: lo que se ejerce es una lectura por RTTI, y el
  // enlazador deja fuera un tipo que no considere alcanzable.
  [Version('4.5.6', 'AnnotatedTool', 'DAF')]
  TAnnotatedTool = class
  end;

  TUnannotatedTool = class
  end;

  [Version('4.5.6', 'DebugTool', 'DAF', True)]
  TDebugAnnotatedTool = class
  end;

  {$SCOPEDENUMS ON}
  // Declared out of alphabetical order, so a help listing its values sorted can be told from one that does not.
  TLoudness = (Quiet, Normal, Loud);
  {$SCOPEDENUMS OFF}

  THelpWorld = class
  public
    Root: IRootCommand;
    Build: ICommand;
    Serve: ICommand;
    VerboseOpt: IOption<Boolean>;
    FileOpt: IOption<string>;
    PortOpt: IOption<Integer>;
    TargetOpt: IOption<string>;
    // A root, and an option of the application that would take a name of the help or of the version.
    OwnRoot: IRootCommand;
    OwnHelpOpt: IOption<Boolean>;
    OwnBuild: ICommand;
    // La versión vive en la raíz, así que necesita su propia gramática con subcomando
    // para poder afirmar que un subcomando NO la responde.
    VersionRoot: IRootCommand;
    VersionBuild: ICommand;
    DefaultedOpt: IOption<string>;
    QuietOpt: IOption<Boolean>;
    OwnVersionRoot: IRootCommand;
    OwnVersionOpt: IOption<Boolean>;
    /// <summary>What adding or parsing raised, as its class and its message; '&lt;nothing&gt;' when nothing did.</summary>
    Refusal: string;
    // Las tres vias de declarar la version, y la de no declararla.
    NoVersionRoot: IRootCommand;
    AnnotatedRoot: IRootCommand;
    DebugAnnotatedRoot: IRootCommand;
    UnannotatedRoot: IRootCommand;
    NamedRoot: IRootCommand;
    // A hidden option next to one that is not, which is the only way to tell "not
    // listed" from "nothing was listed".
    HiddenRoot: IRootCommand;
    DiagOpt: IOption<Integer>;
    PlainOpt: IOption<string>;
    // Las gramáticas del formato, cada una la de su texto de referencia.
    SectionsRoot: IRootCommand;
    NoDescriptionRoot: IRootCommand;
    Native: ICommand;
    RequiredRoot: IRootCommand;
    AcceptedRoot: IRootCommand;
    WidthRoot: IRootCommand;
    DefaultsRoot: IRootCommand;
    HiddenCommandRoot: IRootCommand;
    WrapperRoot: IRootCommand;
    Wrapper: ICommand;
    RecursiveRoot: IRootCommand;
    RecursiveBuild: ICommand;
    LongDescription: string;
    Parsed: IParseResult;
    Rendered: string;
    RenderedAgain: string;
    Listed: string;
    /// <summary>Each name tried, and whether it asked for the help.</summary>
    Answered: string;
    /// <summary>Whose the errors of the parse are: the version option's, or another symbol's.</summary>
    ErrorOwners: string;
    ReadDiag: Integer;
    Output: TStringWriter;
    Error: TStringWriter;
    /// <summary>
    ///   Starts at -1 and not at 0: "the invocation answered 0" is exactly what a field would
    ///   hold if the When never ran.
    /// </summary>
    ExitCode: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure BuildGrammar;
    procedure BuildVersionGrammar;
    procedure BuildVersionSourcesGrammar;
    procedure BuildHiddenGrammar;
    procedure BuildSectionsGrammar;
    procedure BuildRequiredGrammar;
    procedure BuildAcceptedGrammar;
    procedure BuildWidthGrammar;
    procedure BuildDefaultsGrammar;
    procedure BuildHiddenCommandGrammar;
    procedure BuildWrapperGrammar;
    procedure BuildRecursiveGrammar;
    /// <summary>Parses and invokes with the output and the error of the world.</summary>
    procedure Invoke(const Root: IRootCommand; const Args: TArray<string>);
  end;

implementation

uses
  System.SysUtils,
  System.Math,
  Daf.MiniSpec;

const
  {$IFDEF WIN64}
  CompiledPlatform = 'Win64';
  {$ELSE}
  CompiledPlatform = 'Win32';
  {$ENDIF}

  // The help of the grammars of this feature, written out as text: a reference computed by the code
  // under test would move with it. Each is what System.CommandLine writes for the same grammar, except
  // that a root declaring no version does not list --version.
  ToolHelp =
    'Description:' + sLineBreak +
    '  A tool that builds things' + sLineBreak +
    sLineBreak +
    'Usage:' + sLineBreak +
    '  mytool [command] [options]' + sLineBreak +
    sLineBreak +
    'Options:' + sLineBreak +
    '  --verbose       Show more detail' + sLineBreak +
    '  -?, -h, --help  Show help and usage information' + sLineBreak +
    sLineBreak +
    'Commands:' + sLineBreak +
    '  build  Build things' + sLineBreak +
    '  serve  Serve things' + sLineBreak;

  BuildHelp =
    'Description:' + sLineBreak +
    '  Build things' + sLineBreak +
    sLineBreak +
    'Usage:' + sLineBreak +
    '  mytool build [options]' + sLineBreak +
    sLineBreak +
    'Options:' + sLineBreak +
    '  --file <file>   File to compile' + sLineBreak +
    '  -?, -h, --help  Show help and usage information' + sLineBreak +
    '  --verbose       Show more detail' + sLineBreak;

  DefaultedBuildHelp =
    'Description:' + sLineBreak +
    '  Build things' + sLineBreak +
    sLineBreak +
    'Usage:' + sLineBreak +
    '  mytool build [options]' + sLineBreak +
    sLineBreak +
    'Options:' + sLineBreak +
    '  --out <out>     Where it goes [default: bin\out.exe]' + sLineBreak +
    '  -?, -h, --help  Show help and usage information' + sLineBreak;

  SectionsHelp =
    'Description:' + sLineBreak +
    '  A tool' + sLineBreak +
    sLineBreak +
    'Usage:' + sLineBreak +
    '  mytool <source> [command] [options]' + sLineBreak +
    sLineBreak +
    'Arguments:' + sLineBreak +
    '  <source>  What it reads' + sLineBreak +
    sLineBreak +
    'Options:' + sLineBreak +
    '  --out <out>     Where it goes' + sLineBreak +
    '  -?, -h, --help  Show help and usage information' + sLineBreak +
    '  --version       Show version information' + sLineBreak +
    sLineBreak +
    'Commands:' + sLineBreak +
    '  native <target>  Build natively' + sLineBreak;

  NativeHelp =
    'Description:' + sLineBreak +
    '  Build natively' + sLineBreak +
    sLineBreak +
    'Usage:' + sLineBreak +
    '  mytool <source> native <target> [options]' + sLineBreak +
    sLineBreak +
    'Arguments:' + sLineBreak +
    '  <source>  What it reads' + sLineBreak +
    '  <target>  What to build' + sLineBreak +
    sLineBreak +
    'Options:' + sLineBreak +
    '  -?, -h, --help  Show help and usage information' + sLineBreak;

  NoDescriptionHelp =
    'Description:' + sLineBreak +
    sLineBreak +
    'Usage:' + sLineBreak +
    '  mytool [options]' + sLineBreak +
    sLineBreak +
    'Options:' + sLineBreak +
    '  -?, -h, --help  Show help and usage information' + sLineBreak +
    '  --version       Show version information' + sLineBreak;

  RequiredHelp =
    'Description:' + sLineBreak +
    '  A tool' + sLineBreak +
    sLineBreak +
    'Usage:' + sLineBreak +
    '  mytool [options]' + sLineBreak +
    sLineBreak +
    'Options:' + sLineBreak +
    '  -v, --verbose (REQUIRED)  Show more detail' + sLineBreak +
    '  -?, -h, --help            Show help and usage information' + sLineBreak +
    '  --version                 Show version information' + sLineBreak;

  AcceptedHelp =
    'Description:' + sLineBreak +
    '  A tool' + sLineBreak +
    sLineBreak +
    'Usage:' + sLineBreak +
    '  mytool [options]' + sLineBreak +
    sLineBreak +
    'Options:' + sLineBreak +
    '  --mode <fast|safe>           How to run' + sLineBreak +
    '  --level <Loud|Normal|Quiet>  How loud' + sLineBreak +
    '  -?, -h, --help               Show help and usage information' + sLineBreak +
    '  --version                    Show version information' + sLineBreak;

  // A wrapped piece keeps the blank it was cut at.
  WidthHelp =
    'Description:' + sLineBreak +
    '  A tool' + sLineBreak +
    sLineBreak +
    'Usage:' + sLineBreak +
    '  mytool [options]' + sLineBreak +
    sLineBreak +
    'Options:' + sLineBreak +
    '  --out <out>     Where the result is ' + sLineBreak +
    '                  written, relative to ' + sLineBreak +
    '                  the current directory ' + sLineBreak +
    '                  unless it is absolute' + sLineBreak +
    '  -?, -h, --help  Show help and usage ' + sLineBreak +
    '                  information' + sLineBreak +
    '  --version       Show version ' + sLineBreak +
    '                  information' + sLineBreak;

  DefaultsHelp =
    'Description:' + sLineBreak +
    '  A tool' + sLineBreak +
    sLineBreak +
    'Usage:' + sLineBreak +
    '  mytool [options]' + sLineBreak +
    sLineBreak +
    'Options:' + sLineBreak +
    '  --fast           Go fast' + sLineBreak +
    '  --safe           Stay safe [default: true]' + sLineBreak +
    '  --count <count>  How many [default: 7]' + sLineBreak +
    '  -?, -h, --help   Show help and usage information' + sLineBreak +
    '  --version        Show version information' + sLineBreak;

  HiddenCommandHelp =
    'Description:' + sLineBreak +
    '  A tool' + sLineBreak +
    sLineBreak +
    'Usage:' + sLineBreak +
    '  mytool [command] [options]' + sLineBreak +
    sLineBreak +
    'Options:' + sLineBreak +
    '  -?, -h, --help  Show help and usage information' + sLineBreak +
    '  --version       Show version information' + sLineBreak +
    sLineBreak +
    'Commands:' + sLineBreak +
    '  serve  Serve things' + sLineBreak;

  // A declared divergence: System.CommandLine's text closes one bracket more than it opens.
  WrapperHelp =
    'Description:' + sLineBreak +
    '  Run another program' + sLineBreak +
    sLineBreak +
    'Usage:' + sLineBreak +
    '  mytool exec [options] [[--] <additional arguments>...]' + sLineBreak +
    sLineBreak +
    'Options:' + sLineBreak +
    '  -?, -h, --help  Show help and usage information' + sLineBreak +
    sLineBreak +
    'Additional Arguments:' + sLineBreak +
    '  Arguments passed to the application that is being run.' + sLineBreak;

  RecursiveBuildHelp =
    'Description:' + sLineBreak +
    '  Build things' + sLineBreak +
    sLineBreak +
    'Usage:' + sLineBreak +
    '  mytool build [options]' + sLineBreak +
    sLineBreak +
    'Options:' + sLineBreak +
    '  -?, -h, --help  Show help and usage information' + sLineBreak +
    '  --verbose       Show more detail' + sLineBreak;

function HelpActionOf(const Root: IRootCommand): IHelpAction;
var
  Option: IOption;
begin
  for Option in Root.Options do
    if Supports(Option, IHelpOption) and Supports(Option.Action, IHelpAction, Result) then
      Exit;
  Result := nil;
end;

function VersionActionOf(const Root: IRootCommand): IVersionAction;
var
  Option: IOption;
begin
  for Option in Root.Options do
    if Supports(Option, IVersionOption) and Supports(Option.Action, IVersionAction, Result) then
      Exit;
  Result := nil;
end;

function Raised(const Run: TProc): string;
begin
  try
    Run();
    Result := '<nothing>';
  except
    on E: Exception do
      Result := E.ClassName + ': ' + E.Message;
  end;
end;

function LongestLine(const Text: string): Integer;
var
  Line: string;
begin
  Result := 0;
  for Line in Text.Split([sLineBreak]) do
    Result := Max(Result, Line.Length);
end;

{ THelpWorld }

constructor THelpWorld.Create;
begin
  inherited;
  Output := TStringWriter.Create;
  Error := TStringWriter.Create;
  ExitCode := -1;
end;

destructor THelpWorld.Destroy;
begin
  Output.Free;
  Error.Free;
  inherited;
end;

procedure THelpWorld.Invoke(const Root: IRootCommand; const Args: TArray<string>);
var
  Configuration: TInvocationConfiguration;
begin
  Configuration.Output := Output;
  Configuration.Error := Error;
  ExitCode := Root.Parse(Args).Invoke(Configuration);
end;

procedure THelpWorld.BuildGrammar;
begin
  VerboseOpt := CommandLine.Option<Boolean>('--verbose').Description('Show more detail').Recursive;
  FileOpt := CommandLine.Option<string>('--file').Description('File to compile');
  PortOpt := CommandLine.Option<Integer>('--port').Description('Port to listen on');
  Build := CommandLine.Command('build').Description('Build things').Add(FileOpt);
  Serve := CommandLine.Command('serve').Description('Serve things').Add(PortOpt);
  Root := CommandLine.Root('mytool', 'A tool that builds things');
  Root.Add([VerboseOpt, Build, Serve]);
end;

procedure THelpWorld.BuildVersionGrammar;
begin
  DefaultedOpt := CommandLine.Option<string>('--out').Description('Where it goes').DefaultValue('bin\out.exe');
  VersionBuild := CommandLine.Command('build', 'Build things').Add(DefaultedOpt);
  QuietOpt := CommandLine.Option<Boolean>('--quiet').Description('Say less');
  VersionRoot := CommandLine.Root('mytool', 'A tool with a version').WithVersion('1.2.3');
  VersionRoot.Add([QuietOpt, VersionBuild]);
end;

procedure THelpWorld.BuildVersionSourcesGrammar;
begin
  NoVersionRoot := CommandLine.Root('bare', 'Declares no version at all');
  AnnotatedRoot := CommandLine.Root('annotated', 'Takes it from a class').WithVersion(TAnnotatedTool);
  DebugAnnotatedRoot := CommandLine.Root('debugtool', 'A class that declares a debug build').WithVersion(TDebugAnnotatedTool);
  UnannotatedRoot := CommandLine.Root('unannotated', 'Names a class without it').WithVersion(TUnannotatedTool);
  NamedRoot := CommandLine.Root('named', 'Declares text, name and company').WithVersion('7.8.9', 'NamedTool', 'DAF');
end;

procedure THelpWorld.BuildHiddenGrammar;
begin
  // Hidden touches the help and nothing else: it is declared, parsed and read the same.
  DiagOpt := CommandLine.Option<Integer>('--diag').Description('Diagnostics level').Hidden;
  PlainOpt := CommandLine.Option<string>('--name').Description('What to call it');
  HiddenRoot := CommandLine.Root('mytool', 'A tool with something not advertised');
  HiddenRoot.Add([DiagOpt, PlainOpt]);
end;

procedure THelpWorld.BuildSectionsGrammar;
begin
  Native := CommandLine.Command('native', 'Build natively')
    .Add(CommandLine.Argument<string>('target').Description('What to build'));
  SectionsRoot := CommandLine.Root('mytool', 'A tool').WithVersion('1.0.0');
  SectionsRoot.Add([CommandLine.Argument<string>('source').Description('What it reads'),
    CommandLine.Option<string>('--out').Description('Where it goes'), Native]);
end;

procedure THelpWorld.BuildRequiredGrammar;
begin
  RequiredRoot := CommandLine.Root('mytool', 'A tool').WithVersion('1.0.0');
  RequiredRoot.Add(CommandLine.Option<Boolean>('--verbose', ['-v']).Description('Show more detail').Required);
end;

procedure THelpWorld.BuildAcceptedGrammar;
begin
  AcceptedRoot := CommandLine.Root('mytool', 'A tool').WithVersion('1.0.0');
  AcceptedRoot.Add([CommandLine.Option<string>('--mode').Description('How to run').AcceptOnlyFromAmong(['fast', 'safe']),
    CommandLine.Option<TLoudness>('--level').Description('How loud')]);
end;

procedure THelpWorld.BuildWidthGrammar;
begin
  WidthRoot := CommandLine.Root('mytool', 'A tool').WithVersion('1.0.0');
  WidthRoot.Add(CommandLine.Option<string>('--out')
    .Description('Where the result is written, relative to the current directory unless it is absolute'));
end;

procedure THelpWorld.BuildDefaultsGrammar;
begin
  DefaultsRoot := CommandLine.Root('mytool', 'A tool').WithVersion('1.0.0');
  DefaultsRoot.Add([CommandLine.Option<Boolean>('--fast').Description('Go fast').DefaultValue(False),
    CommandLine.Option<Boolean>('--safe').Description('Stay safe').DefaultValue(True),
    CommandLine.Option<Integer>('--count').Description('How many').DefaultValue(7)]);
end;

procedure THelpWorld.BuildHiddenCommandGrammar;
begin
  HiddenCommandRoot := CommandLine.Root('mytool', 'A tool').WithVersion('1.0.0');
  HiddenCommandRoot.Add([CommandLine.Command('build', 'Build things').Hidden, CommandLine.Command('serve', 'Serve things')]);
end;

procedure THelpWorld.BuildWrapperGrammar;
begin
  Wrapper := CommandLine.Command('exec', 'Run another program').AllowUnmatchedTokens;
  WrapperRoot := CommandLine.Root('mytool', 'A tool');
  WrapperRoot.Add(Wrapper);
end;

procedure THelpWorld.BuildRecursiveGrammar;
begin
  RecursiveBuild := CommandLine.Command('build', 'Build things');
  RecursiveRoot := CommandLine.Root('mytool', 'A tool');
  RecursiveRoot.Add([CommandLine.Option<Boolean>('--verbose').Description('Show more detail').Recursive,
    CommandLine.Option<Boolean>('--quiet').Description('Say less'), RecursiveBuild]);
end;

initialization

Feature('''
Feature Command line help @commandline @help

  As a developer writing a console application
  I want the help to be generated from the grammar I declared
  So it cannot contradict what the program actually accepts
''')

.UseWorld<THelpWorld>

.Rule('the help is generated from the declared grammar, in the format of System.CommandLine')

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
        Expect(W.Rendered).ToEqual(ToolHelp);
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
        Expect(W.Rendered).ToEqual(BuildHelp);
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
        Expect(W.Rendered).ToEqual(DefaultedBuildHelp);
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
        W.TargetOpt := CommandLine.Option<string>('--target').Description('Where it goes');
        W.Build.Add(W.TargetOpt);
        W.RenderedAgain := W.Build.HelpText;
      end)
    .&Then('the new option appears, with no text edited anywhere',
      procedure(W: THelpWorld)
      begin
        Expect(W.RenderedAgain).ToContain('  --target <target>  Where it goes' + sLineBreak);
      end)

  .Scenario('The sections, in the order of System.CommandLine')
    .Given('a command with a description, an argument, an option and a subcommand',
      procedure(W: THelpWorld)
      begin
        W.BuildSectionsGrammar;
      end)
    .When('its help is asked for',
      procedure(W: THelpWorld)
      begin
        W.Rendered := W.SectionsRoot.HelpText;
      end)
    .&Then('Description, Usage, Arguments, Options and Commands appear, in that order',
      procedure(W: THelpWorld)
      begin
        Expect(W.Rendered).ToEqual(SectionsHelp);
      end)

  .Scenario('A command with no description still writes the heading')
    .Given('a root command declared with no description',
      procedure(W: THelpWorld)
      begin
        W.NoDescriptionRoot := CommandLine.Root('mytool').WithVersion('1.0.0');
      end)
    .When('its help is asked for',
      procedure(W: THelpWorld)
      begin
        W.Rendered := W.NoDescriptionRoot.HelpText;
      end)
    .&Then('Description: is written with nothing under it',
      procedure(W: THelpWorld)
      begin
        Expect(W.Rendered).ToEqual(NoDescriptionHelp);
      end)

  .Scenario('A required option and its names')
    .Given('a required option called --verbose, with the alias -v',
      procedure(W: THelpWorld)
      begin
        W.BuildRequiredGrammar;
      end)
    .When('the help of its command is asked for',
      procedure(W: THelpWorld)
      begin
        W.Rendered := W.RequiredRoot.HelpText;
      end)
    .&Then('its row starts with -v, --verbose and carries (REQUIRED)',
      procedure(W: THelpWorld)
      begin
        Expect(W.Rendered).ToContain('  -v, --verbose (REQUIRED)  Show more detail' + sLineBreak);
        Expect(W.Rendered).ToEqual(RequiredHelp);
      end)

  .Scenario('The accepted values in the placeholder')
    .Given('an option --mode restricted to fast and safe, with no help name',
      procedure(W: THelpWorld)
      begin
        W.BuildAcceptedGrammar;
      end)
    .When('the help of its command is asked for',
      procedure(W: THelpWorld)
      begin
        W.Rendered := W.AcceptedRoot.HelpText;
      end)
    .&Then('its row shows --mode <fast|safe>',
      procedure(W: THelpWorld)
      begin
        Expect(W.Rendered).ToContain('  --mode <fast|safe>  ');
      end)
    .&And('an enumerated type shows its values sorted, as the rest of the help does',
      procedure(W: THelpWorld)
      begin
        Expect(W.Rendered).ToEqual(AcceptedHelp);
      end)

  .Scenario('A width set wraps the lines')
    .Given('an option with a description longer than the width set in the help action',
      procedure(W: THelpWorld)
      begin
        W.BuildWidthGrammar;
        HelpActionOf(W.WidthRoot).MaxWidth := 40;
      end)
    .When('the help of its command is asked for',
      procedure(W: THelpWorld)
      begin
        W.Invoke(W.WidthRoot, ['--help']);
      end)
    .&Then('no line is longer than that width',
      procedure(W: THelpWorld)
      begin
        Expect(LongestLine(W.Output.ToString)).ToBeLessOrEqual(40);
      end)
    .&And('each entry is wrapped within its own column',
      procedure(W: THelpWorld)
      begin
        Expect(W.Output.ToString).ToEqual(WidthHelp + sLineBreak);
      end)

  .Scenario('No width set does not wrap')
    .Given('an option with a description of two hundred characters, and no width set',
      procedure(W: THelpWorld)
      begin
        W.LongDescription := StringOfChar('x', 100) + ' ' + StringOfChar('y', 99);
        W.WidthRoot := CommandLine.Root('mytool', 'A tool').WithVersion('1.0.0');
        W.WidthRoot.Add(CommandLine.Option<string>('--out').Description(W.LongDescription));
      end)
    .When('the help of its command is asked for',
      procedure(W: THelpWorld)
      begin
        W.Invoke(W.WidthRoot, ['--help']);
      end)
    .&Then('the description comes out on one line',
      procedure(W: THelpWorld)
      begin
        Expect(W.LongDescription.Length).ToEqual(200);
        Expect(W.Output.ToString).ToContain('  --out <out>     ' + W.LongDescription + sLineBreak);
      end)

  .Scenario('A boolean default that is false is not shown')
    .Given('a boolean option whose default is false, another whose default is true, and an integer one',
      procedure(W: THelpWorld)
      begin
        W.BuildDefaultsGrammar;
      end)
    .When('the help of their command is asked for',
      procedure(W: THelpWorld)
      begin
        W.Rendered := W.DefaultsRoot.HelpText;
      end)
    .&Then('only the default that is false is missing',
      procedure(W: THelpWorld)
      begin
        Expect(W.Rendered).ToEqual(DefaultsHelp);
      end)

  .Scenario('A hidden command writes no help')
    .Given('a root with a hidden subcommand and one that is not',
      procedure(W: THelpWorld)
      begin
        W.BuildHiddenCommandGrammar;
      end)
    .When('the help of the hidden subcommand is invoked',
      procedure(W: THelpWorld)
      begin
        W.Invoke(W.HiddenCommandRoot, ['build', '--help']);
      end)
    .&Then('nothing is written, and the invocation answers 0',
      procedure(W: THelpWorld)
      begin
        Expect(Format('exit=%d output=[%s]', [W.ExitCode, W.Output.ToString])).ToEqual('exit=0 output=[]');
      end)
    .&And('the help of the root does not list it',
      procedure(W: THelpWorld)
      begin
        Expect(W.HiddenCommandRoot.HelpText).ToEqual(HiddenCommandHelp);
      end)

  .Scenario('A command that collects what it does not recognise says so')
    .Given('a subcommand that allows unmatched tokens',
      procedure(W: THelpWorld)
      begin
        W.BuildWrapperGrammar;
      end)
    .When('its help is asked for',
      procedure(W: THelpWorld)
      begin
        W.Rendered := W.Wrapper.HelpText;
      end)
    .&Then('its usage ends with the additional arguments, and a section describes them',
      procedure(W: THelpWorld)
      begin
        Expect(W.Rendered).ToEqual(WrapperHelp);
      end)

.Rule('the help of a subcommand shows what it inherits')

  .Scenario('A recursive option of the root in the help of a subcommand')
    .Given('a root with a recursive option --verbose and another that is not, and a subcommand build',
      procedure(W: THelpWorld)
      begin
        W.BuildRecursiveGrammar;
      end)
    .When('the help of build is asked for',
      procedure(W: THelpWorld)
      begin
        W.Rendered := W.RecursiveBuild.HelpText;
      end)
    .&Then('--verbose appears, and not the other option of the root',
      procedure(W: THelpWorld)
      begin
        Expect(W.Rendered).ToEqual(RecursiveBuildHelp);
      end)

  .Scenario('The arguments of the ancestors in the help of a subcommand')
    .Given('a root with an argument, and a subcommand with an argument of its own',
      procedure(W: THelpWorld)
      begin
        W.BuildSectionsGrammar;
      end)
    .When('the help of the subcommand is asked for',
      procedure(W: THelpWorld)
      begin
        W.Rendered := W.Native.HelpText;
      end)
    .&Then('its usage and its arguments carry the root''s before its own',
      procedure(W: THelpWorld)
      begin
        Expect(W.Rendered).ToEqual(NativeHelp);
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
        Expect(W.Parsed.IsHelpRequested).ToBeTrue;
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
        Expect(W.Parsed.IsHelpRequested).ToBeTrue;
        Expect(W.Parsed.Command.Name).ToEqual('build');
        Expect(W.Parsed.HelpText).ToContain('--file');
        Expect(W.Parsed.HelpText.Contains('--port')).ToBeFalse;
      end)

  .Scenario('The help answers to its aliases')
    .Given('a grammar that declares no help option',
      procedure(W: THelpWorld)
      begin
        W.BuildGrammar;
      end)
    .When('invocations passing each of the other names of the help are parsed',
      procedure(W: THelpWorld)
      var
        Names: TArray<string>;
        Name: string;
      begin
        Names := ['-?', '-h', '/h', '/?'];
        for Name in Names do
          W.Answered := W.Answered + Format('%s=%s ', [Name, BoolToStr(W.Root.Parse([Name]).IsHelpRequested, True)]);
      end)
    .&Then('each of them asks for the help, as --help does',
      procedure(W: THelpWorld)
      begin
        Expect(W.Answered).ToEqual('-?=True -h=True /h=True /?=True ');
      end)

  .Scenario('The three names of the help answer the same on a subcommand')
    .Given('a grammar with subcommands',
      procedure(W: THelpWorld)
      begin
        W.BuildGrammar;
      end)
    .When('invocations naming a subcommand and passing --help, -h and -? are parsed',
      procedure(W: THelpWorld)
      var
        Names: TArray<string>;
        Name: string;
      begin
        Names := ['--help', '-h', '-?'];
        for Name in Names do
          W.Answered := W.Answered + Format('%s=%s ', [Name, BoolToStr(W.Root.Parse(['build', Name]).IsHelpRequested, True)]);
      end)
    .&Then('each of them asks for the help of that subcommand',
      procedure(W: THelpWorld)
      begin
        Expect(W.Answered).ToEqual('--help=True -h=True -?=True ');
      end)

.Rule('the names of the help and of the version are reserved')

  .Scenario('An option of the application that takes a name of the help is refused')
    .Given('a root, and an option of the application called --help',
      procedure(W: THelpWorld)
      begin
        W.OwnRoot := CommandLine.Root('owntool', 'Brings its own help');
        W.OwnHelpOpt := CommandLine.Option<Boolean>('--help').Description('The help this application wants');
      end)
    .When('the option is added to the root',
      procedure(W: THelpWorld)
      begin
        W.Refusal := Raised(
          procedure
          begin
            W.OwnRoot.Add(W.OwnHelpOpt);
          end);
      end)
    .&Then('adding it raises, naming the option and the name it takes',
      procedure(W: THelpWorld)
      begin
        Expect(W.Refusal).ToEqual('ECommandLine: Option "--help" of "owntool" takes "--help", which is reserved to ' +
          '"--help": that name always reaches it, so the option could never answer');
      end)
    .&And('the root is left as it was, and the help still answers',
      procedure(W: THelpWorld)
      begin
        Expect(Length(W.OwnRoot.Options)).ToEqual(2);
        Expect(W.OwnRoot.Parse(['--help']).IsHelpRequested).ToBeTrue;
      end)

  .Scenario('A subcommand whose option takes a name of the help is refused when it is added')
    .Given('a subcommand with an option of its own called -h, not yet added to any root',
      procedure(W: THelpWorld)
      begin
        W.OwnBuild := CommandLine.Command('build', 'Build things')
          .Add(CommandLine.Option<Boolean>('-h').Description('Human readable'));
        W.OwnRoot := CommandLine.Root('owntool', 'Brings a subcommand');
      end)
    .When('the subcommand is added to the root',
      procedure(W: THelpWorld)
      begin
        W.Refusal := Raised(
          procedure
          begin
            W.OwnRoot.Add(W.OwnBuild);
          end);
      end)
    .&Then('adding it raises, because the help of the root answers on the subcommand',
      procedure(W: THelpWorld)
      begin
        Expect(W.Refusal).ToEqual('ECommandLine: Option "-h" of "build" takes "-h", which is reserved to "--help": ' +
          'that name always reaches it, so the option could never answer');
      end)

  .Scenario('An alias added afterwards that takes a name of the help is refused by the parse')
    .Given('a root with an option of the application, to which the alias -? is added after adding it',
      procedure(W: THelpWorld)
      begin
        W.OwnHelpOpt := CommandLine.Option<Boolean>('--human').Description('Human readable');
        W.OwnRoot := CommandLine.Root('owntool', 'Brings its own help');
        W.OwnRoot.Add(W.OwnHelpOpt);
        W.OwnHelpOpt.Aliases.Add('-?');
      end)
    .When('an invocation is parsed',
      procedure(W: THelpWorld)
      begin
        W.Refusal := Raised(
          procedure
          begin
            W.OwnRoot.Parse([]);
          end);
      end)
    .&Then('the parse raises, naming the option and the alias',
      procedure(W: THelpWorld)
      begin
        Expect(W.Refusal).ToEqual('ECommandLine: Option "--human" of "owntool" takes "-?", which is reserved to ' +
          '"--help": that name always reaches it, so the option could never answer');
      end)

  .Scenario('With names compared without regard to case, a name of the help written otherwise is refused too')
    .Given('a root with an option of the application called -H',
      procedure(W: THelpWorld)
      begin
        W.OwnRoot := CommandLine.Root('owntool', 'Brings its own help');
        W.OwnRoot.Add(CommandLine.Option<Boolean>('-H').Description('Human readable'));
      end)
    .When('an invocation is parsed with names compared without regard to case',
      procedure(W: THelpWorld)
      begin
        W.Refusal := Raised(
          procedure
          var
            Configuration: TParserConfiguration;
          begin
            Configuration.EnableCaseInsensitiveNames := True;
            W.OwnRoot.Parse([], Configuration);
          end);
      end)
    .&Then('the parse raises, because -H would reach the help',
      procedure(W: THelpWorld)
      begin
        Expect(W.Refusal).ToEqual('ECommandLine: Option "-H" of "owntool" takes "-H", which is reserved to "--help": ' +
          'that name always reaches it, so the option could never answer');
      end)

  .Scenario('An option of the application that takes the name of the version is refused')
    .Given('a root with a declared version, and an option of the application called --version',
      procedure(W: THelpWorld)
      begin
        W.OwnVersionRoot := CommandLine.Root('owntool', 'Brings its own version').WithVersion('9.9.9');
        W.OwnVersionOpt := CommandLine.Option<Boolean>('--version').Description('The version this application wants');
      end)
    .When('the option is added to the root',
      procedure(W: THelpWorld)
      begin
        W.Refusal := Raised(
          procedure
          begin
            W.OwnVersionRoot.Add(W.OwnVersionOpt);
          end);
      end)
    .&Then('adding it raises, naming the option and the name it takes',
      procedure(W: THelpWorld)
      begin
        Expect(W.Refusal).ToEqual('ECommandLine: Option "--version" of "owntool" takes "--version", which is reserved ' +
          'to "--version": that name always reaches it, so the option could never answer');
      end)
    .&And('the version still answers',
      procedure(W: THelpWorld)
      begin
        Expect(W.OwnVersionRoot.Parse(['--version']).IsVersionRequested).ToBeTrue;
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
        Expect(W.Parsed.IsVersionRequested).ToBeTrue;
        Expect(W.Parsed.VersionInfo.Tag).ToEqual('1.2.3');
        Expect(W.Parsed.VersionInfo.Major).ToEqual(1);
      end)
    .&And('no invocation error is reported',
      procedure(W: THelpWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.Parsed.Succeeded).ToBeTrue;
      end)

  .Scenario('The version by default')
    .Given('a root with the version 1.2.3 declared',
      procedure(W: THelpWorld)
      begin
        W.BuildVersionGrammar;
      end)
    .When('the result of parsing --version is invoked',
      procedure(W: THelpWorld)
      begin
        W.Invoke(W.VersionRoot, ['--version']);
      end)
    .&Then('1.2.3 is written, without the build data',
      procedure(W: THelpWorld)
      begin
        Expect(W.Output.ToString).ToEqual('1.2.3' + sLineBreak);
        Expect(W.ExitCode).ToEqual(TCommandLineExit.Ok);
      end)

  .Scenario('The version with the build data')
    .Given('a root with the version 1.2.3 declared, and the version action set to include the build data',
      procedure(W: THelpWorld)
      begin
        W.BuildVersionGrammar;
        VersionActionOf(W.VersionRoot).IncludeBuildData := True;
      end)
    .When('the result of parsing --version is invoked',
      procedure(W: THelpWorld)
      begin
        W.Invoke(W.VersionRoot, ['--version']);
      end)
    .&Then('1.2.3 is written followed by the build data',
      procedure(W: THelpWorld)
      begin
        Expect(W.Output.ToString).ToEqual('1.2.3+' + CompiledPlatform + sLineBreak);
      end)

  .Scenario('A version not declared stays empty')
    .Given('a root that declares no version',
      procedure(W: THelpWorld)
      begin
        W.BuildVersionSourcesGrammar;
      end)
    .When('the result of parsing --version is invoked',
      procedure(W: THelpWorld)
      begin
        W.Invoke(W.NoVersionRoot, ['--version']);
      end)
    .&Then('an empty version is written, and nothing is taken from the executable',
      procedure(W: THelpWorld)
      begin
        Expect(W.Output.ToString).ToEqual(sLineBreak);
        Expect(W.ExitCode).ToEqual(TCommandLineExit.Ok);
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
        Expect(W.Parsed.IsVersionRequested).ToBeFalse;
      end)

  .Scenario('The version does not combine with other arguments')
    .Given('a grammar whose root declares its version, and an option',
      procedure(W: THelpWorld)
      begin
        W.BuildVersionGrammar;
      end)
    .When('an invocation passing --version and that option is parsed',
      procedure(W: THelpWorld)
      var
        Error: TParseError;
        AsOption: IOptionResult;
      begin
        W.Parsed := W.VersionRoot.Parse(['--version', '--quiet']);
        for Error in W.Parsed.Errors do
          if Supports(Error.SymbolResult, IOptionResult, AsOption) and Supports(AsOption.Option, IVersionOption) then
            W.ErrorOwners := W.ErrorOwners + 'version '
          else
            W.ErrorOwners := W.ErrorOwners + 'other ';
      end)
    .&Then('the parse fails with an error of the version option that says it does not combine with other arguments',
      procedure(W: THelpWorld)
      begin
        Expect(W.Parsed.Succeeded).ToBeFalse;
        Expect(W.ErrorOwners).ToEqual('version ');
        Expect(W.Parsed.ErrorText).ToEqual('--version option cannot be combined with other arguments.');
      end)

  .Scenario('A root command with no version does not advertise one')
    .Given('a grammar whose root declares no version',
      procedure(W: THelpWorld)
      begin
        W.BuildVersionSourcesGrammar;
      end)
    .When('its help is generated',
      procedure(W: THelpWorld)
      begin
        W.Rendered := W.NoVersionRoot.HelpText;
      end)
    .&Then('--version is not among the listed options',
      procedure(W: THelpWorld)
      begin
        // Que la version este fijada es lo que se pregunta aqui, y no puede deducirse de que su
        // texto compuesto sea o no vacio: sobre una version vacia el render nunca da cadena vacia.
        Expect(Pos('--version', W.Rendered)).ToEqual(0);
      end)

.Rule('the version can be declared from a class that carries it')

  .Scenario('Declaring the version by naming an annotated class')
    .Given('a grammar whose root takes its version from an annotated class',
      procedure(W: THelpWorld)
      begin
        W.BuildVersionSourcesGrammar;
      end)
    .When('an invocation passing --version is parsed',
      procedure(W: THelpWorld)
      begin
        W.Parsed := W.AnnotatedRoot.Parse(['--version']);
      end)
    .&Then('the annotation is what the result carries',
      procedure(W: THelpWorld)
      begin
        Expect(W.Parsed.IsVersionRequested).ToBeTrue;
        Expect(W.Parsed.VersionInfo.Tag).ToEqual('4.5.6');
        Expect(W.Parsed.VersionInfo.ApplicationName).ToEqual('AnnotatedTool');
      end)

  .Scenario('The build configuration the class declares travels with the version')
    .Given('a grammar whose root takes its version from a class that declares a debug build',
      procedure(W: THelpWorld)
      begin
        W.BuildVersionSourcesGrammar;
      end)
    .When('an invocation passing --version is parsed',
      procedure(W: THelpWorld)
      begin
        W.Parsed := W.DebugAnnotatedRoot.Parse(['--version']);
      end)
    .&Then('the version it hands over says it is a debug build',
      procedure(W: THelpWorld)
      begin
        Expect(W.Parsed.IsVersionRequested).ToBeTrue;
        Expect(W.Parsed.VersionInfo.Debug).ToBeTrue;
      end)

  .Scenario('A class without the annotation leaves the version empty')
    .Given('a grammar whose root names a class that carries no version',
      procedure(W: THelpWorld)
      begin
        W.BuildVersionSourcesGrammar;
      end)
    .When('its help is generated',
      procedure(W: THelpWorld)
      begin
        W.Rendered := W.UnannotatedRoot.HelpText;
      end)
    .&Then('the version is empty and is not advertised',
      procedure(W: THelpWorld)
      begin
        Expect(W.UnannotatedRoot.VersionInfo.IsEmpty).ToBeTrue;
        Expect(Pos('--version', W.Rendered)).ToEqual(0);
      end)

  .Scenario('Declaring the version as text with a name and a company')
    .Given('a grammar whose root declares the three',
      procedure(W: THelpWorld)
      begin
        W.BuildVersionSourcesGrammar;
      end)
    .When('an invocation passing --version is parsed',
      procedure(W: THelpWorld)
      begin
        W.Parsed := W.NamedRoot.Parse(['--version']);
      end)
    .&Then('the three travel together',
      procedure(W: THelpWorld)
      begin
        Expect(W.Parsed.VersionInfo.Tag).ToEqual('7.8.9');
        Expect(W.Parsed.VersionInfo.ApplicationName).ToEqual('NamedTool');
        Expect(W.Parsed.VersionInfo.Company).ToEqual('DAF');
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
        // error that will not say what failed turns a mistake into a riddle.
        Expect(W.Parsed.Succeeded).ToBeFalse;
        // The message has to be the CONVERSION's, naming the option. Asserting only
        // that '--diag' appears would also be satisfied by "unknown option --diag",
        // which is what a hidden option would produce if hiding reached the parser --
        // a green that coincides with the failure it is meant to catch.
        Expect(W.Parsed.ErrorText).ToEqual('Cannot parse argument ''loud'' for option ''--diag'' as expected type ''integer''.');
        Expect(W.Parsed.ErrorText).ToContain('--diag');
        Expect(W.Parsed.ErrorText).ToContain('loud');
      end);

end.
