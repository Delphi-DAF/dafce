unit Syntax.Feat;

interface

uses
  Daf.CommandLine;

type
  TSyntaxWorld = class
  public
    // Every option here hangs off `build` and not off the root, on purpose: a token is
    // only splittable knowing what is declared, and what is declared depends on the
    // command reached. A rewriting done once over the whole invocation would see the
    // root's options and none of these.
    Root: IRootCommand;
    Build: ICommand;
    ForceOpt: IOption<Boolean>;
    DryOpt: IOption<Boolean>;
    CrossOpt: IOption<Boolean>;
    OutOpt: IOption<string>;
    VerbosityOpt: IOption<string>;
    TargetArg: IArgument<string>;
    // A flat grammar for the arity, so no subcommand requirement gets in the way of
    // what is being asserted.
    Many: IRootCommand;
    ItemsOpt: IOption<string>;
    LooseOpt: IOption<string>;
    NameArg: IArgument<string>;
    // A wrapper and a command that wraps nobody, so what changes and what does not can
    // be asserted side by side.
    Wrapper: IRootCommand;
    UserOpt: IOption<string>;
    Plain: IRootCommand;
    KeepOpt: IOption<string>;
    Parsed: ICommandLineParseResult;
    ReadVerbosity: string;
    ReadOut: string;
    ReadTarget: string;
    ReadItems: TArray<string>;
    ReadLoose: TArray<string>;
    ReadName: string;
    destructor Destroy; override;
    procedure BuildGrammar;
    procedure BuildMany;
    procedure BuildWrapper;
    /// <summary>Writes a response file and remembers it, so nothing is left behind.</summary>
    function WriteResponseFile(const FileName: string; const Lines: array of string): string;
    /// <summary>A path in the temp folder that is guaranteed not to exist.</summary>
    function MissingResponseFile: string;
  private
    FWritten: TArray<string>;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  Daf.MiniSpec;

{ TSyntaxWorld }

destructor TSyntaxWorld.Destroy;
var
  Path: string;
begin
  for Path in FWritten do
    if TFile.Exists(Path) then
      TFile.Delete(Path);
  inherited;
end;

function TSyntaxWorld.WriteResponseFile(const FileName: string; const Lines: array of string): string;
var
  Content: TArray<string>;
  I: Integer;
begin
  Result := TPath.Combine(TPath.GetTempPath, FileName);
  SetLength(Content, Length(Lines));
  for I := Low(Lines) to High(Lines) do
    Content[I - Low(Lines)] := Lines[I];
  TFile.WriteAllText(Result, string.Join(sLineBreak, Content));
  FWritten := FWritten + [Result];
end;

function TSyntaxWorld.MissingResponseFile: string;
begin
  Result := TPath.Combine(TPath.GetTempPath, 'daf-commandline-nothing-here.rsp');
  if TFile.Exists(Result) then
    TFile.Delete(Result);
end;

procedure TSyntaxWorld.BuildGrammar;
begin
  ForceOpt := CommandLine.Option<Boolean>('--force', 'Do it anyway').WithAlias('-f');
  DryOpt := CommandLine.Option<Boolean>('--dry', 'Say what would happen').WithAlias('-d');
  CrossOpt := CommandLine.Option<Boolean>('--cross', 'Cross compile').WithAlias('-x');
  OutOpt := CommandLine.Option<string>('--out', 'Where it goes').WithAlias('-o');
  VerbosityOpt := CommandLine.Option<string>('--verbosity', 'How much to say').WithAlias('-v');
  TargetArg := CommandLine.Argument<string>('target', 'What to build');
  Build := CommandLine.Command('build', 'Build things')
    .Add([ForceOpt, DryOpt, CrossOpt, OutOpt, VerbosityOpt, TargetArg]);
  Root := CommandLine.Root('mytool', 'A tool that builds things');
  Root.Add(Build);
end;

procedure TSyntaxWorld.BuildMany;
begin
  ItemsOpt := CommandLine.Option<string>('--items', 'Several at once')
    .WithArity(TArity.ZeroOrMore).AllowMultipleArgumentsPerToken;
  // The same arity WITHOUT the mark, which is the half of the requirement that says
  // what does NOT change.
  LooseOpt := CommandLine.Option<string>('--loose', 'Several, one mention each')
    .WithArity(TArity.ZeroOrMore);
  NameArg := CommandLine.Argument<string>('name', 'What it is called').WithArity(TArity.ZeroOrOne);
  Many := CommandLine.Root('tool', 'A tool');
  Many.Add([ItemsOpt, LooseOpt, NameArg]);
end;

procedure TSyntaxWorld.BuildWrapper;
begin
  UserOpt := CommandLine.Option<string>('--user', 'Who to run as').WithAlias('-u');
  Wrapper := CommandLine.Root('sudo', 'Runs something as somebody else');
  Wrapper.Add(UserOpt);
  // Called as a statement and not chained: it answers ICommand, and a root that went
  // through it would stop being an IRootCommand.
  Wrapper.AllowUnmatchedTokens;

  KeepOpt := CommandLine.Option<string>('--keep', 'What to keep');
  Plain := CommandLine.Root('tool', 'A tool that wraps nobody');
  Plain.Add(KeepOpt);
end;

initialization

Feature('''
Feature Command line syntax @commandline @syntax

  As someone who came from another tool
  I want to write an invocation the way I already write them
  So the tool does not refuse the form I have always used
''')

.UseWorld<TSyntaxWorld>

.Rule('an invocation can be written in the forms the convention admits')

  .Scenario('The value separated by a colon')
    .Given('a declared option that takes a value',
      procedure(W: TSyntaxWorld)
      begin
        W.BuildGrammar;
      end)
    .When('the invocation writes it as --option:value',
      procedure(W: TSyntaxWorld)
      begin
        W.Parsed := W.Root.Parse(['build', 'app.dpr', '--verbosity:quiet']);
        W.ReadVerbosity := W.VerbosityOpt.ValueIn(W.Parsed);
      end)
    .&Then('the same value is read as with a space between them',
      procedure(W: TSyntaxWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.ReadVerbosity).ToEqual('quiet');
      end)

  .Scenario('A one-character alias glued to its value')
    .Given('a declared option with a one-character alias',
      procedure(W: TSyntaxWorld)
      begin
        W.BuildGrammar;
      end)
    .When('the invocation writes the alias and the value with nothing between them',
      procedure(W: TSyntaxWorld)
      begin
        W.Parsed := W.Root.Parse(['build', 'app.dpr', '-vquiet']);
        W.ReadVerbosity := W.VerbosityOpt.ValueIn(W.Parsed);
      end)
    .&Then('the same value is read as with them separated',
      procedure(W: TSyntaxWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.ReadVerbosity).ToEqual('quiet');
      end)
    .&And('the positional it sits next to is untouched',
      procedure(W: TSyntaxWorld)
      begin
        W.ReadTarget := W.TargetArg.ValueIn(W.Parsed);
        Expect(W.ReadTarget).ToEqual('app.dpr');
      end)

.Rule('one-character options can be grouped')

  .Scenario('Three flags grouped')
    .Given('three declared boolean options with one-character aliases',
      procedure(W: TSyntaxWorld)
      begin
        W.BuildGrammar;
      end)
    .When('the invocation writes them together behind a single dash',
      procedure(W: TSyntaxWorld)
      begin
        W.Parsed := W.Root.Parse(['build', 'app.dpr', '-fdx']);
      end)
    .&Then('all three are passed, just as if they had been written apart',
      procedure(W: TSyntaxWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.ForceOpt.ValueIn(W.Parsed)).ToBeTrue;
        Expect(W.DryOpt.ValueIn(W.Parsed)).ToBeTrue;
        Expect(W.CrossOpt.ValueIn(W.Parsed)).ToBeTrue;
      end)

  .Scenario('A value behind a group')
    .Given('a group whose last option takes a value',
      procedure(W: TSyntaxWorld)
      begin
        W.BuildGrammar;
      end)
    .When('the invocation writes the group followed by a value',
      procedure(W: TSyntaxWorld)
      begin
        W.Parsed := W.Root.Parse(['build', 'app.dpr', '-fdo', 'out.exe']);
        W.ReadOut := W.OutOpt.ValueIn(W.Parsed);
      end)
    .&Then('the value goes to the last option of the group',
      procedure(W: TSyntaxWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.ReadOut).ToEqual('out.exe');
        Expect(W.ForceOpt.ValueIn(W.Parsed)).ToBeTrue;
        Expect(W.DryOpt.ValueIn(W.Parsed)).ToBeTrue;
      end)
    .&And('the positional keeps its own value',
      procedure(W: TSyntaxWorld)
      begin
        Expect(W.TargetArg.ValueIn(W.Parsed)).ToEqual('app.dpr');
      end)

  .Scenario('A letter of the group that does not exist')
    .Given('a group in which one letter matches no declared option',
      procedure(W: TSyntaxWorld)
      begin
        W.BuildGrammar;
      end)
    .When('it is parsed',
      procedure(W: TSyntaxWorld)
      begin
        W.Parsed := W.Root.Parse(['build', 'app.dpr', '-fdz']);
      end)
    .&Then('the parse fails naming that letter',
      procedure(W: TSyntaxWorld)
      begin
        Expect(W.Parsed.Succeeded).ToBeFalse;
        Expect(W.Parsed.ErrorText).ToContain('-z');
      end)
    .&And('none of the other letters of the group is taken as passed',
      procedure(W: TSyntaxWorld)
      begin
        Expect(W.Parsed.WasProvided(W.ForceOpt)).ToBeFalse;
        Expect(W.Parsed.WasProvided(W.DryOpt)).ToBeFalse;
      end)

.Rule('the Windows prefix is admitted alongside the POSIX one')

  .Scenario('An option named with a slash')
    .Given('a declared option',
      procedure(W: TSyntaxWorld)
      begin
        W.BuildGrammar;
      end)
    .When('the invocation names it with a slash instead of with dashes',
      procedure(W: TSyntaxWorld)
      begin
        W.Parsed := W.Root.Parse(['build', 'app.dpr', '/out', 'out.exe']);
        W.ReadOut := W.OutOpt.ValueIn(W.Parsed);
      end)
    .&Then('the same option is reached',
      procedure(W: TSyntaxWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.ReadOut).ToEqual('out.exe');
      end)

  .Scenario('A positional value that starts with a slash')
    .Given('a command with a positional argument and a value that starts with a slash',
      procedure(W: TSyntaxWorld)
      begin
        W.BuildGrammar;
      end)
    .When('that value matches no declared option',
      procedure(W: TSyntaxWorld)
      begin
        W.Parsed := W.Root.Parse(['build', '/tmp/app.dpr']);
        W.ReadTarget := W.TargetArg.ValueIn(W.Parsed);
      end)
    .&Then('it is a positional value and not an unknown option',
      procedure(W: TSyntaxWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.ReadTarget).ToEqual('/tmp/app.dpr');
      end)

.Rule('an option with room for many values takes them without repeating its name')

  .Scenario('Several values behind one name')
    .Given('an option with multiple arity, declared to take several values per token',
      procedure(W: TSyntaxWorld)
      begin
        W.BuildMany;
      end)
    .When('the invocation writes its name followed by three values',
      procedure(W: TSyntaxWorld)
      begin
        W.Parsed := W.Many.Parse(['--items', 'a', 'b', 'c']);
        W.ReadItems := W.ItemsOpt.ValuesIn(W.Parsed);
      end)
    .&Then('all three are read, in the order they came',
      procedure(W: TSyntaxWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(string.Join(',', W.ReadItems)).ToEqual('a,b,c');
      end)

  .Scenario('Without declaring it, a loose value is still a positional')
    .Given('an option with multiple arity NOT declared for several values per token, and a positional',
      procedure(W: TSyntaxWorld)
      begin
        W.BuildMany;
      end)
    .When('the invocation writes the option with a value and then another value on its own',
      procedure(W: TSyntaxWorld)
      begin
        W.Parsed := W.Many.Parse(['--loose', 'a', 'b']);
        W.ReadLoose := W.LooseOpt.ValuesIn(W.Parsed);
        W.ReadName := W.NameArg.ValueIn(W.Parsed);
      end)
    .&Then('the second value goes to the positional argument',
      procedure(W: TSyntaxWorld)
      begin
        // Making this greedy by default would move what invocations that already work
        // mean, which is why the mark exists at all.
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(string.Join(',', W.ReadLoose)).ToEqual('a');
        Expect(W.ReadName).ToEqual('b');
      end)

.Rule('a response file contributes tokens to the invocation')

  .Scenario('An invocation that comes out of a file')
    .Given('a response file with a subcommand and an option, one per line',
      procedure(W: TSyntaxWorld)
      begin
        W.BuildGrammar;
      end)
    .When('the invocation is that file behind an at sign',
      procedure(W: TSyntaxWorld)
      begin
        W.Parsed := W.Root.Parse(['@' + W.WriteResponseFile('daf-cl-whole.rsp',
          ['build', 'app.dpr', '--verbosity', 'quiet'])]);
        W.ReadVerbosity := W.VerbosityOpt.ValueIn(W.Parsed);
      end)
    .&Then('it parses as if its tokens had been written out',
      procedure(W: TSyntaxWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.Parsed.Command.FullPath).ToEqual('mytool build');
        Expect(W.ReadVerbosity).ToEqual('quiet');
        Expect(W.TargetArg.ValueIn(W.Parsed)).ToEqual('app.dpr');
      end)

  .Scenario('A file mixed with tokens written by hand')
    .Given('a response file with some options',
      procedure(W: TSyntaxWorld)
      begin
        W.BuildGrammar;
      end)
    .When('the invocation mixes the file with other tokens',
      procedure(W: TSyntaxWorld)
      begin
        W.Parsed := W.Root.Parse(['build', 'app.dpr',
          '@' + W.WriteResponseFile('daf-cl-mixed.rsp', ['--verbosity', 'quiet']), '-f']);
        W.ReadVerbosity := W.VerbosityOpt.ValueIn(W.Parsed);
      end)
    .&Then('all of them are parsed, in the order they appear',
      procedure(W: TSyntaxWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.ReadVerbosity).ToEqual('quiet');
        Expect(W.ForceOpt.ValueIn(W.Parsed)).ToBeTrue;
      end)

  .Scenario('Comments inside the file')
    .Given('a response file with a comment line',
      procedure(W: TSyntaxWorld)
      begin
        W.BuildGrammar;
      end)
    .When('it is parsed',
      procedure(W: TSyntaxWorld)
      begin
        W.Parsed := W.Root.Parse(['build', 'app.dpr',
          '@' + W.WriteResponseFile('daf-cl-comments.rsp',
          ['# what this invocation is for', '', '--verbosity', 'quiet'])]);
        W.ReadVerbosity := W.VerbosityOpt.ValueIn(W.Parsed);
      end)
    .&Then('the comment line contributes no token at all',
      procedure(W: TSyntaxWorld)
      begin
        // Were it a token it would arrive as an argument nobody expected, which is
        // exactly what the empty error text below rules out.
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.ReadVerbosity).ToEqual('quiet');
      end)

  .Scenario('A file that does not exist')
    .Given('an invocation naming a response file that is not there',
      procedure(W: TSyntaxWorld)
      begin
        W.BuildGrammar;
      end)
    .When('it is parsed',
      procedure(W: TSyntaxWorld)
      begin
        W.Parsed := W.Root.Parse(['build', 'app.dpr', '@' + W.MissingResponseFile]);
      end)
    .&Then('the parse fails naming the file',
      procedure(W: TSyntaxWorld)
      begin
        // Passing it through as a literal token would turn a typo into an argument
        // with an at sign on it, travelling all the way to wherever it gets used.
        //
        // The failure has to be the NAMED one. Before response files existed this same
        // token failed too -- as "Unexpected argument" -- and its text carried the path,
        // so an assertion on the path alone was already green while nothing had been
        // built. A spec whose green coincides with its own failure mode is not a spec.
        Expect(W.Parsed.Succeeded).ToBeFalse;
        Expect(W.Parsed.ErrorText).ToContain('Response file');
        Expect(W.Parsed.ErrorText).ToContain('daf-commandline-nothing-here.rsp');
      end)

  .Scenario('A file that names another file')
    .Given('a response file whose only line names a second one',
      procedure(W: TSyntaxWorld)
      begin
        W.BuildGrammar;
      end)
    .When('the invocation names the outer one',
      procedure(W: TSyntaxWorld)
      begin
        var Inner := W.WriteResponseFile('daf-cl-inner.rsp', ['--verbosity', 'quiet']);
        W.Parsed := W.Root.Parse(['build', 'app.dpr',
          '@' + W.WriteResponseFile('daf-cl-outer.rsp', ['@' + Inner])]);
        W.ReadVerbosity := W.VerbosityOpt.ValueIn(W.Parsed);
      end)
    .&Then('the tokens of the inner one arrive as well',
      procedure(W: TSyntaxWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.ReadVerbosity).ToEqual('quiet');
      end)

  .Scenario('A file that names itself')
    .Given('a response file whose only line is itself',
      procedure(W: TSyntaxWorld)
      begin
        W.BuildGrammar;
      end)
    .When('it is parsed',
      procedure(W: TSyntaxWorld)
      begin
        // That this step RETURNS is the assertion. Without a bound the expansion loops
        // forever, and a spec that hangs does not read as a failure -- it reads as a
        // suite still running, which is the worst of the two.
        var Cycle := TPath.Combine(TPath.GetTempPath, 'daf-cl-cycle.rsp');
        W.WriteResponseFile('daf-cl-cycle.rsp', ['@' + Cycle]);
        W.Parsed := W.Root.Parse(['build', 'app.dpr', '@' + Cycle]);
      end)
    .&Then('the parse fails saying the nesting went too deep',
      procedure(W: TSyntaxWorld)
      begin
        Expect(W.Parsed.Succeeded).ToBeFalse;
        Expect(W.Parsed.ErrorText).ToContain('nested deeper than');
        Expect(W.Parsed.ErrorText).ToContain('daf-cl-cycle.rsp');
      end)

.Rule('a command can collect what it does not recognise instead of failing')

  .Scenario('A wrapper command collects what is not its own')
    .Given('a command declared not to treat what it does not recognise as an error',
      procedure(W: TSyntaxWorld)
      begin
        W.BuildWrapper;
      end)
    .When('the invocation passes its own options and behind them tokens that are not its',
      procedure(W: TSyntaxWorld)
      begin
        W.Parsed := W.Wrapper.Parse(['-u', 'admin', 'apt', 'update']);
      end)
    .&Then('the parse succeeds and its own options are read',
      procedure(W: TSyntaxWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.UserOpt.ValueIn(W.Parsed)).ToEqual('admin');
      end)
    .&And('the tokens that are not its are available apart, in order',
      procedure(W: TSyntaxWorld)
      begin
        Expect(string.Join(' ', W.Parsed.UnmatchedTokens)).ToEqual('apt update');
      end)

  .Scenario('By default it is still an error')
    .Given('a command that has declared nothing about it',
      procedure(W: TSyntaxWorld)
      begin
        W.BuildWrapper;
      end)
    .When('the invocation passes a token that matches nothing',
      procedure(W: TSyntaxWorld)
      begin
        W.Parsed := W.Plain.Parse(['--keep', 'x', 'stray']);
      end)
    .&Then('the parse fails, as it did before',
      procedure(W: TSyntaxWorld)
      begin
        // For anything that wraps nobody an unrecognised token is a typo, and taking it
        // quietly is the worse of the two failures.
        Expect(W.Parsed.Succeeded).ToBeFalse;
        Expect(W.Parsed.ErrorText).ToContain('stray');
      end)
    .&And('nothing is collected apart',
      procedure(W: TSyntaxWorld)
      begin
        Expect('[' + string.Join(' ', W.Parsed.UnmatchedTokens) + ']').ToEqual('[]');
      end)

.Rule('what a new form takes away, the end of options gives back')

  .Scenario('A positional value that starts with an at sign')
    .Given('a command with a positional argument',
      procedure(W: TSyntaxWorld)
      begin
        W.BuildGrammar;
      end)
    .When('its value is passed behind the end of options',
      procedure(W: TSyntaxWorld)
      begin
        W.Parsed := W.Root.Parse(['build', '--', '@not-a-response-file']);
        W.ReadTarget := W.TargetArg.ValueIn(W.Parsed);
      end)
    .&Then('it arrives whole, and no file is looked for',
      procedure(W: TSyntaxWorld)
      begin
        // Admitting response files is what takes this value away by default, and this
        // is the whole of what gives it back. Documented in the README as such, so it
        // is measured here rather than promised.
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.ReadTarget).ToEqual('@not-a-response-file');
      end)

  .Scenario('A positional value that spells a declared option with a slash')
    .Given('a command with a positional argument and an option that the value would name',
      procedure(W: TSyntaxWorld)
      begin
        W.BuildGrammar;
      end)
    .When('its value is passed behind the end of options',
      procedure(W: TSyntaxWorld)
      begin
        W.Parsed := W.Root.Parse(['build', '--', '/out']);
        W.ReadTarget := W.TargetArg.ValueIn(W.Parsed);
      end)
    .&Then('it is the value and not the option it spells',
      procedure(W: TSyntaxWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.ReadTarget).ToEqual('/out');
        Expect(W.Parsed.WasProvided(W.OutOpt)).ToBeFalse;
      end);

end.
