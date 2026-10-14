unit HelpActions.Feat;

interface

uses
  System.SysUtils,
  System.Classes,
  Daf.CommandLine;

type
  EBannerFailed = class(Exception);

  THelpActionsWorld = class
  public
    Root: IRootCommand;
    Build: ICommand;
    DiagOpt: IOption<Integer>;
    NameOpt: IOption<string>;
    OutOpt: IOption<string>;
    Parsed: IParseResult;
    Output: TStringWriter;
    Error: TStringWriter;
    RootHelp: string;
    BuildHelp: string;
    Version: string;
    /// <summary>Which writer the replaced action was handed, or empty if it never ran.</summary>
    SeenOutput: string;
    /// <summary>The configuration the result holds, as text, or empty if it was never read.</summary>
    SeenConfiguration: string;
    /// <summary>A command with nothing but a help option declared by hand.</summary>
    Bare: ICommand;
    /// <summary>The actions of their own that ran, in order.</summary>
    Ran: string;
    Report: string;
    /// <summary>
    ///   Starts at -1 and not at 0: "the invocation answered 0" is exactly what a field would
    ///   hold if the When never ran.
    /// </summary>
    ExitCode: Integer;
    constructor Create;
    destructor Destroy; override;
    function Configuration: TInvocationConfiguration;
    function WriterName(const Writer: TTextWriter): string;
    /// <summary>A grammar with a subcommand, a hidden option and a declared version.</summary>
    procedure BuildGrammar;
    /// <summary>Parses and invokes with an output of its own, and answers what it wrote.</summary>
    function Captured(const Args: TArray<string>): string;
    function HelpOfRoot: IHelpOption;
    function VersionOfRoot: IVersionOption;
    procedure Track(const Name: string);
  end;

  /// <summary>An action of its own: terminating, clearing no errors, leaving its name in the world.</summary>
  TOwnAction = class(TSynchronousCommandLineAction)
  private
    // An object reference and not an interface: the world owns the grammar that holds this action.
    FWorld: THelpActionsWorld;
    FName: string;
    FCode: Integer;
  public
    constructor Create(World: THelpActionsWorld; const Name: string; Code: Integer);
    function Invoke(const Parsed: IParseResult): Integer; override;
  end;

implementation

uses
  Daf.MiniSpec;

const
  // The default help and version of BuildGrammar, written out as text: a reference computed by
  // the code under test would move with it.
  RootHelpReference =
    'Description:' + sLineBreak +
    '  A tool with a version' + sLineBreak +
    sLineBreak +
    'Usage:' + sLineBreak +
    '  tool [command] [options]' + sLineBreak +
    sLineBreak +
    'Options:' + sLineBreak +
    '  --name <name>   What to call it' + sLineBreak +
    '  -?, -h, --help  Show help and usage information' + sLineBreak +
    '  --version       Show version information' + sLineBreak +
    sLineBreak +
    'Commands:' + sLineBreak +
    '  build  Build things' + sLineBreak +
    sLineBreak;

  BuildHelpReference =
    'Description:' + sLineBreak +
    '  Build things' + sLineBreak +
    sLineBreak +
    'Usage:' + sLineBreak +
    '  tool build [options]' + sLineBreak +
    sLineBreak +
    'Options:' + sLineBreak +
    '  --out <out>     Where it goes' + sLineBreak +
    '  -?, -h, --help  Show help and usage information' + sLineBreak +
    sLineBreak;

  VersionReference = '3.1.4' + sLineBreak;

  Banner = 'ACME tool -- (c) ACME';

// Outside the world, so the closure captures the banner and the action it wraps and nothing
// else: an action holding the root it is installed on is a cycle between counted references.
function WithBanner(const Inner: TCommandAction): TCommandAction;
begin
  Result :=
    function(const Parsed: IParseResult): Integer
    begin
      Parsed.InvocationConfiguration.WriteOutput(Banner);
      Result := Inner(Parsed);
    end;
end;

{ TOwnAction }

constructor TOwnAction.Create(World: THelpActionsWorld; const Name: string; Code: Integer);
begin
  inherited Create;
  FWorld := World;
  FName := Name;
  FCode := Code;
end;

function TOwnAction.Invoke(const Parsed: IParseResult): Integer;
begin
  FWorld.Track(FName);
  Result := FCode;
end;

{ THelpActionsWorld }

constructor THelpActionsWorld.Create;
begin
  inherited;
  Output := TStringWriter.Create;
  Error := TStringWriter.Create;
  ExitCode := -1;
end;

destructor THelpActionsWorld.Destroy;
begin
  Output.Free;
  Error.Free;
  inherited;
end;

function THelpActionsWorld.Configuration: TInvocationConfiguration;
begin
  Result.Output := Output;
  Result.Error := Error;
end;

function THelpActionsWorld.WriterName(const Writer: TTextWriter): string;
begin
  if Writer = nil then
    Result := 'the console'
  else if Writer = Output then
    Result := 'the invocation output'
  else if Writer = Error then
    Result := 'the invocation error'
  else
    Result := 'another writer';
end;

procedure THelpActionsWorld.BuildGrammar;
begin
  DiagOpt := CommandLine.Option<Integer>('--diag').Description('Diagnostics level').Hidden;
  NameOpt := CommandLine.Option<string>('--name').Description('What to call it');
  OutOpt := CommandLine.Option<string>('--out').Description('Where it goes');
  Build := CommandLine.Command('build', 'Build things').Add(OutOpt);
  Root := CommandLine.Root('tool', 'A tool with a version').WithVersion('3.1.4');
  Root.Add([DiagOpt, NameOpt, Build]);
end;

function THelpActionsWorld.HelpOfRoot: IHelpOption;
var
  Option: IOption;
begin
  Result := nil;
  for Option in Root.Options do
    if Supports(Option, IHelpOption, Result) then
      Exit;
end;

function THelpActionsWorld.VersionOfRoot: IVersionOption;
var
  Option: IOption;
begin
  Result := nil;
  for Option in Root.Options do
    if Supports(Option, IVersionOption, Result) then
      Exit;
end;

procedure THelpActionsWorld.Track(const Name: string);
begin
  if Ran <> '' then
    Ran := Ran + ',';
  Ran := Ran + Name;
end;

function THelpActionsWorld.Captured(const Args: TArray<string>): string;
var
  Writer: TStringWriter;
  Config: TInvocationConfiguration;
begin
  Writer := TStringWriter.Create;
  try
    Config.Output := Writer;
    Config.Error := Error;
    Root.Parse(Args).Invoke(Config);
    Result := Writer.ToString;
  finally
    Writer.Free;
  end;
end;

initialization

Feature('''
Feature Replacing the help and the version actions @commandline @helpactions

  As a developer writing a console application
  I want to put something of my own above the help and the version
  So the tool can show its banner without composing a help of its own
''')

.UseWorld<THelpActionsWorld>

.Rule('without replacing anything, the help and the version are the default ones')

  .Scenario('Without replacing anything, the output does not change')
    .Given('a root command with a subcommand, a hidden option and a declared version',
      procedure(W: THelpActionsWorld)
      begin
        W.BuildGrammar;
      end)
    .When('the help of the root, the help of the subcommand and the version are invoked',
      procedure(W: THelpActionsWorld)
      begin
        W.RootHelp := W.Captured(['--help']);
        W.BuildHelp := W.Captured(['build', '--help']);
        W.Version := W.Captured(['--version']);
      end)
    .&Then('the help of the root is the one written before the actions could be replaced',
      procedure(W: THelpActionsWorld)
      begin
        Expect(W.RootHelp).ToEqual(RootHelpReference);
      end)
    .&And('so is the help of the subcommand',
      procedure(W: THelpActionsWorld)
      begin
        Expect(W.BuildHelp).ToEqual(BuildHelpReference);
      end)
    .&And('so is the version',
      procedure(W: THelpActionsWorld)
      begin
        Expect(W.Version).ToEqual(VersionReference);
        Expect(W.Error.ToString).ToEqual('');
      end)

.Rule('the help and the version actions of the root can be replaced, and the default delegated to')

  .Scenario('A banner above the help')
    .Given('a root command whose help action writes a banner and then delegates to the default one',
      procedure(W: THelpActionsWorld)
      begin
        W.BuildGrammar;
        W.Root.HelpAction := WithBanner(W.Root.HelpAction);
      end)
    .When('the result of parsing an invocation that passes --help is invoked',
      procedure(W: THelpActionsWorld)
      begin
        W.Parsed := W.Root.Parse(['--help']);
        W.ExitCode := W.Parsed.Invoke(W.Configuration);
      end)
    .&Then('the output is the banner followed by the usual help',
      procedure(W: THelpActionsWorld)
      begin
        Expect(W.Output.ToString).ToEqual(Banner + sLineBreak + RootHelpReference);
      end)
    .&And('the invocation answers 0',
      procedure(W: THelpActionsWorld)
      begin
        Expect(W.ExitCode).ToEqual(TCommandLineExit.Ok);
      end)

  .Scenario('A banner above the version')
    .Given('a root command with a declared version, whose version action writes a banner and then delegates to the default one',
      procedure(W: THelpActionsWorld)
      begin
        W.BuildGrammar;
        W.Root.VersionAction := WithBanner(W.Root.VersionAction);
      end)
    .When('the result of parsing an invocation that passes --version is invoked',
      procedure(W: THelpActionsWorld)
      begin
        W.Parsed := W.Root.Parse(['--version']);
        W.ExitCode := W.Parsed.Invoke(W.Configuration);
      end)
    .&Then('the output is the banner followed by the version',
      procedure(W: THelpActionsWorld)
      begin
        Expect(W.Output.ToString).ToEqual(Banner + sLineBreak + VersionReference);
        Expect(W.ExitCode).ToEqual(TCommandLineExit.Ok);
      end)

  .Scenario('The help of a subcommand is wrapped too')
    .Given('a root command with a subcommand, and a help action replaced on the root',
      procedure(W: THelpActionsWorld)
      begin
        W.BuildGrammar;
        W.Root.HelpAction := WithBanner(W.Root.HelpAction);
      end)
    .When('the result of parsing an invocation that names the subcommand and passes --help is invoked',
      procedure(W: THelpActionsWorld)
      begin
        W.Parsed := W.Root.Parse(['build', '--help']);
        W.ExitCode := W.Parsed.Invoke(W.Configuration);
      end)
    .&Then('the output is the banner followed by the help of the subcommand',
      procedure(W: THelpActionsWorld)
      begin
        Expect(W.Output.ToString).ToEqual(Banner + sLineBreak + BuildHelpReference);
      end)

  .Scenario('The banner goes where the help goes')
    .Given('a replaced help action that writes its banner to the output of the invocation',
      procedure(W: THelpActionsWorld)
      begin
        W.BuildGrammar;
        var Default := W.Root.HelpAction;
        W.Root.HelpAction :=
          function(const Parsed: IParseResult): Integer
          begin
            W.SeenOutput := W.WriterName(Parsed.InvocationConfiguration.Output);
            Parsed.InvocationConfiguration.WriteOutput(Banner);
            Result := Default(Parsed);
          end;
      end)
    .When('it is invoked with a configuration that gives its own output',
      procedure(W: THelpActionsWorld)
      begin
        W.Parsed := W.Root.Parse(['--help']);
        W.ExitCode := W.Parsed.Invoke(W.Configuration);
      end)
    .&Then('the action is handed that output, and not the console',
      procedure(W: THelpActionsWorld)
      begin
        Expect(W.SeenOutput).ToEqual('the invocation output');
      end)
    .&And('the banner and the help are written to it',
      procedure(W: THelpActionsWorld)
      begin
        Expect(W.Output.ToString).ToEqual(Banner + sLineBreak + RootHelpReference);
        Expect(W.Error.ToString).ToEqual('');
      end)

  .Scenario('Before any invocation, the result holds the default configuration')
    .Given('a root command',
      procedure(W: THelpActionsWorld)
      begin
        W.BuildGrammar;
      end)
    .When('an invocation is parsed and its result is not invoked',
      procedure(W: THelpActionsWorld)
      begin
        W.Parsed := W.Root.Parse(['--help']);
        var Seen := W.Parsed.InvocationConfiguration;
        W.SeenConfiguration := Format('output=%s error=%s handler=%s',
          [W.WriterName(Seen.Output), W.WriterName(Seen.Error), BoolToStr(Seen.EnableDefaultExceptionHandler, True)]);
      end)
    .&Then('its configuration writes to the console and has the default handler on',
      procedure(W: THelpActionsWorld)
      begin
        Expect(W.SeenConfiguration).ToEqual('output=the console error=the console handler=True');
      end)

  .Scenario('Assigning nil brings the default action back')
    .Given('a root command whose help action was replaced with one that writes a banner, and then given nil',
      procedure(W: THelpActionsWorld)
      begin
        W.BuildGrammar;
        W.Root.HelpAction := WithBanner(W.Root.HelpAction);
        W.Root.HelpAction := nil;
      end)
    .When('the help is asked for',
      procedure(W: THelpActionsWorld)
      begin
        W.Parsed := W.Root.Parse(['--help']);
        W.ExitCode := W.Parsed.Invoke(W.Configuration);
      end)
    .&Then('the output is the default help, without the banner',
      procedure(W: THelpActionsWorld)
      begin
        Expect(W.Output.ToString).ToEqual(RootHelpReference);
        Expect(W.ExitCode).ToEqual(TCommandLineExit.Ok);
      end)

  .Scenario('What the action answers is what the invocation answers')
    .Given('a replaced help action that answers a code other than 0',
      procedure(W: THelpActionsWorld)
      begin
        W.BuildGrammar;
        W.Root.HelpAction :=
          function(const Parsed: IParseResult): Integer
          begin
            Result := 7;
          end;
      end)
    .When('the help is asked for',
      procedure(W: THelpActionsWorld)
      begin
        W.Parsed := W.Root.Parse(['--help']);
        W.ExitCode := W.Parsed.Invoke(W.Configuration);
      end)
    .&Then('the invocation answers that code',
      procedure(W: THelpActionsWorld)
      begin
        Expect(W.ExitCode).ToEqual(7);
        Expect(W.Output.ToString).ToEqual('');
      end)

  .Scenario('Replacing it on the root is replacing it on the option')
    .Given('a root command whose help action was replaced',
      procedure(W: THelpActionsWorld)
      begin
        W.BuildGrammar;
        W.Root.HelpAction :=
          function(const Parsed: IParseResult): Integer
          begin
            W.SeenOutput := 'the replaced action';
            Result := 7;
          end;
      end)
    .When('the action of its help option is read, and run',
      procedure(W: THelpActionsWorld)
      var
        Option: IOption;
      begin
        for Option in W.Root.Options do
          if Supports(Option, IHelpOption) then
            W.ExitCode := Option.Action.Invoke(W.Root.Parse([]));
      end)
    .&Then('it is the replaced action',
      procedure(W: THelpActionsWorld)
      begin
        Expect(Format('ran=%s exit=%d', [W.SeenOutput, W.ExitCode])).ToEqual('ran=the replaced action exit=7');
      end)

  .Scenario('A replaced action that raises goes through the default handler')
    .Given('a replaced version action that raises an exception',
      procedure(W: THelpActionsWorld)
      begin
        W.BuildGrammar;
        W.Root.VersionAction :=
          function(const Parsed: IParseResult): Integer
          begin
            raise EBannerFailed.Create('no banner today');
          end;
      end)
    .When('the version is asked for with the default handler on',
      procedure(W: THelpActionsWorld)
      begin
        W.Parsed := W.Root.Parse(['--version']);
        W.ExitCode := W.Parsed.Invoke(W.Configuration);
      end)
    .&Then('the exception is written to the error, and the invocation answers 1',
      procedure(W: THelpActionsWorld)
      begin
        Expect(W.Error.ToString).ToEqual('Unhandled exception: EBannerFailed: no banner today' + sLineBreak);
        Expect(W.ExitCode).ToEqual(TCommandLineExit.Failure);
      end)

.Rule('a help option or a version option can change its action')

  .Scenario('A help option declared by hand with an action of its own')
    .Given('a help option declared by hand, with an action of its own that answers 7',
      procedure(W: THelpActionsWorld)
      begin
        W.Bare := CommandLine.Command('tool', 'A tool')
          .Add(CommandLine.HelpOption('--ayuda', []).Action(TOwnAction.Create(W, 'own help', 7)));
      end)
    .When('the result of parsing an invocation that names it is invoked',
      procedure(W: THelpActionsWorld)
      begin
        W.Parsed := W.Bare.Parse(['--ayuda']);
        W.ExitCode := W.Parsed.Invoke(W.Configuration);
      end)
    .&Then('that action runs, and the invocation answers 7',
      procedure(W: THelpActionsWorld)
      begin
        Expect(Format('ran=%s exit=%d output=[%s]', [W.Ran, W.ExitCode, W.Output.ToString]))
          .ToEqual('ran=own help exit=7 output=[]');
      end)

  .Scenario('A version option with an action of its own')
    .Given('the version option of a root, with an action of its own',
      procedure(W: THelpActionsWorld)
      begin
        W.BuildGrammar;
        W.VersionOfRoot.Action(TOwnAction.Create(W, 'own version', 0));
      end)
    .When('the result of parsing an invocation that names it is invoked',
      procedure(W: THelpActionsWorld)
      begin
        W.Parsed := W.Root.Parse(['--version']);
        W.ExitCode := W.Parsed.Invoke(W.Configuration);
      end)
    .&Then('that action runs, and not the default one',
      procedure(W: THelpActionsWorld)
      begin
        Expect(Format('ran=%s output=[%s]', [W.Ran, W.Output.ToString])).ToEqual('ran=own version output=[]');
      end)

  .Scenario('The action replaced runs even with parse errors')
    .Given('the help option of a root, with an action of its own',
      procedure(W: THelpActionsWorld)
      begin
        W.BuildGrammar;
        W.HelpOfRoot.Action(TOwnAction.Create(W, 'own help', 10));
      end)
    .When('the result of parsing an invocation that names it next to a token that matches nothing is invoked',
      procedure(W: THelpActionsWorld)
      begin
        W.Parsed := W.Root.Parse(['--help', '--bogus']);
        W.ExitCode := W.Parsed.Invoke(W.Configuration);
      end)
    .&Then('that action runs with its code, and the result still has the error',
      procedure(W: THelpActionsWorld)
      begin
        Expect(Format('ran=%s exit=%d errors=%s', [W.Ran, W.ExitCode, W.Parsed.ErrorText]))
          .ToEqual('ran=own help exit=10 errors=Unrecognized command or argument ''--bogus''.');
      end)

.Rule('a help option or a version option is never left with no action')

  .Scenario('No action on a help option')
    .Given('the help option of a root',
      procedure(W: THelpActionsWorld)
      begin
        W.BuildGrammar;
      end)
    .When('it is given no action',
      procedure(W: THelpActionsWorld)
      begin
        try
          W.HelpOfRoot.Action(nil);
          W.Report := '<accepted>';
        except
          on E: Exception do
            W.Report := E.ClassName + ': ' + E.Message;
        end;
      end)
    .&Then('it fails',
      procedure(W: THelpActionsWorld)
      begin
        Expect(W.Report).ToEqual('ECommandLine: The action of "--help" is not assigned');
      end)

  .Scenario('No action on a version option')
    .Given('the version option of a root',
      procedure(W: THelpActionsWorld)
      begin
        W.BuildGrammar;
      end)
    .When('it is given no action',
      procedure(W: THelpActionsWorld)
      begin
        try
          W.VersionOfRoot.Action(nil);
          W.Report := '<accepted>';
        except
          on E: Exception do
            W.Report := E.ClassName + ': ' + E.Message;
        end;
      end)
    .&Then('it fails',
      procedure(W: THelpActionsWorld)
      begin
        Expect(W.Report).ToEqual('ECommandLine: The action of "--version" is not assigned');
      end);

end.
