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
    // A root that collects, over a subcommand that declares nothing about it.
    Outer: IRootCommand;
    // An argument that keeps the tail of the invocation, next to one that does not.
    Tail: IRootCommand;
    TailArg: IArgument<TArray<string>>;
    TailVerboseOpt: IOption<Boolean>;
    Loose: IRootCommand;
    LooseArg: IArgument<TArray<string>>;
    LooseVerboseOpt: IOption<Boolean>;
    ReadTail: TArray<string>;
    Configuration: TParserConfiguration;
    Parsed: IParseResult;
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
    procedure BuildTails;
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

procedure TSyntaxWorld.BuildTails;
begin
  TailVerboseOpt := CommandLine.Option<Boolean>('--verbose').Description('Say more');
  TailArg := CommandLine.Argument<TArray<string>>('command').Description('What to run, and its arguments')
    .CaptureRemainingTokens;
  Tail := CommandLine.Root('exec', 'Runs a command');
  Tail.Add([TailVerboseOpt, TailArg]);

  LooseVerboseOpt := CommandLine.Option<Boolean>('--verbose').Description('Say more');
  LooseArg := CommandLine.Argument<TArray<string>>('files').Description('Files to read');
  Loose := CommandLine.Root('cat', 'Reads files');
  Loose.Add([LooseVerboseOpt, LooseArg]);
end;

function TSyntaxWorld.MissingResponseFile: string;
begin
  Result := TPath.Combine(TPath.GetTempPath, 'daf-commandline-nothing-here.rsp');
  if TFile.Exists(Result) then
    TFile.Delete(Result);
end;

procedure TSyntaxWorld.BuildGrammar;
begin
  ForceOpt := CommandLine.Option<Boolean>('--force', ['-f']).Description('Do it anyway');
  DryOpt := CommandLine.Option<Boolean>('--dry', ['-d']).Description('Say what would happen');
  CrossOpt := CommandLine.Option<Boolean>('--cross', ['-x']).Description('Cross compile');
  OutOpt := CommandLine.Option<string>('--out', ['-o']).Description('Where it goes');
  VerbosityOpt := CommandLine.Option<string>('--verbosity', ['-v']).Description('How much to say');
  TargetArg := CommandLine.Argument<string>('target').Description('What to build');
  Build := CommandLine.Command('build', 'Build things')
    .Add([ForceOpt, DryOpt, CrossOpt, OutOpt, VerbosityOpt, TargetArg]);
  Root := CommandLine.Root('mytool', 'A tool that builds things');
  Root.Add(Build);
end;

procedure TSyntaxWorld.BuildMany;
begin
  ItemsOpt := CommandLine.Option<string>('--items').Description('Several at once')
    .Arity(TArgumentArity.ZeroOrMore).AllowMultipleArgumentsPerToken;
  // The same arity WITHOUT the mark, which is the half of the requirement that says
  // what does NOT change.
  LooseOpt := CommandLine.Option<string>('--loose').Description('Several, one mention each')
    .Arity(TArgumentArity.ZeroOrMore);
  NameArg := CommandLine.Argument<string>('name').Description('What it is called').Arity(TArgumentArity.ZeroOrOne);
  Many := CommandLine.Root('tool', 'A tool');
  Many.Add([ItemsOpt, LooseOpt, NameArg]);
end;

procedure TSyntaxWorld.BuildWrapper;
begin
  UserOpt := CommandLine.Option<string>('--user', ['-u']).Description('Who to run as');
  Wrapper := CommandLine.Root('sudo', 'Runs something as somebody else');
  Wrapper.Add(UserOpt);
  // Called as a statement and not chained: it answers ICommand, and a root that went
  // through it would stop being an IRootCommand.
  Wrapper.AllowUnmatchedTokens;

  KeepOpt := CommandLine.Option<string>('--keep').Description('What to keep');
  Plain := CommandLine.Root('tool', 'A tool that wraps nobody');
  Plain.Add(KeepOpt);

  Outer := CommandLine.Root('outer', 'Collects what nobody recognises');
  Outer.AllowUnmatchedTokens;
  Outer.Add(CommandLine.Command('inner', 'Declares nothing about it'));
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

  .Scenario('An option that takes a value keeps the rest of the group')
    .Given('the boolean options -f and -d, and the option -o, which takes a value',
      procedure(W: TSyntaxWorld)
      begin
        W.BuildGrammar;
      end)
    .When('the invocation writes -fdobin',
      procedure(W: TSyntaxWorld)
      begin
        W.Parsed := W.Root.Parse(['build', 'app.dpr', '-fdobin']);
        W.ReadOut := W.OutOpt.ValueIn(W.Parsed);
      end)
    .&Then('-f and -d are passed, and -o is bin',
      procedure(W: TSyntaxWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.ForceOpt.ValueIn(W.Parsed)).ToBeTrue;
        Expect(W.DryOpt.ValueIn(W.Parsed)).ToBeTrue;
        Expect(W.ReadOut).ToEqual('bin');
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

  .Scenario('Grouping turned off')
    .Given('three boolean options with one-character aliases, and the parser configured not to group',
      procedure(W: TSyntaxWorld)
      begin
        W.BuildGrammar;
        W.Configuration.EnablePosixBundling := False;
      end)
    .When('the invocation writes them together behind a single dash',
      procedure(W: TSyntaxWorld)
      begin
        W.Parsed := W.Root.Parse(['build', 'app.dpr', '-fdx'], W.Configuration);
      end)
    .&Then('the group is not read as those three options',
      procedure(W: TSyntaxWorld)
      begin
        Expect(W.Parsed.ErrorText).ToContain('''-fdx''');
        Expect(W.Parsed.WasProvided(W.ForceOpt)).ToBeFalse;
        Expect(W.Parsed.WasProvided(W.DryOpt)).ToBeFalse;
        Expect(W.Parsed.WasProvided(W.CrossOpt)).ToBeFalse;
      end)

.Scenario('The built-in help still answers inside a group')
    .Given('declared one-character aliases',
      procedure(W: TSyntaxWorld)
      begin
        W.BuildGrammar;
      end)
    .When('the invocation groups a declared letter with the help letter',
      procedure(W: TSyntaxWorld)
      begin
        W.Parsed := W.Root.Parse(['build', '-fh']);
      end)
    .&Then('help is asked for, as if both had been written apart',
      procedure(W: TSyntaxWorld)
      begin
        Expect(W.Parsed.IsHelpRequested).ToBeTrue;
      end)

.Rule('a slash starts a value, not an option')

  .Scenario('A declared name written with a slash')
    .Given('a command with a positional argument and an option --out',
      procedure(W: TSyntaxWorld)
      begin
        W.BuildGrammar;
      end)
    .When('the invocation writes /out',
      procedure(W: TSyntaxWorld)
      begin
        W.Parsed := W.Root.Parse(['build', '/out']);
        W.ReadTarget := W.TargetArg.ValueIn(W.Parsed);
      end)
    .&Then('it is the positional value, and the option is not given',
      procedure(W: TSyntaxWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.ReadTarget).ToEqual('/out');
        Expect(W.Parsed.WasProvided(W.OutOpt)).ToBeFalse;
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

  .Scenario('A line with several tokens')
    .Given('a response file with the line --out "bin\my app.exe"',
      procedure(W: TSyntaxWorld)
      begin
        W.BuildGrammar;
      end)
    .When('the invocation names it behind an at sign',
      procedure(W: TSyntaxWorld)
      begin
        W.Parsed := W.Root.Parse(['build', 'app.dpr',
          '@' + W.WriteResponseFile('daf-cl-quoted.rsp', ['--out "bin\my app.exe"'])]);
        W.ReadOut := W.OutOpt.ValueIn(W.Parsed);
      end)
    .&Then('--out is bin\my app.exe',
      procedure(W: TSyntaxWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.ReadOut).ToEqual('bin\my app.exe');
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
        // The failure has to be the NAMED one: as a token nothing matches, the same one
        // fails too and its text carries the path, so an assertion on the path alone is
        // green with no file read. A spec whose green coincides with its own failure mode
        // is not a spec.
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
        Expect(W.Parsed.ErrorText).ToContain('is nested more than 10 levels deep');
        Expect(W.Parsed.ErrorText).ToContain('daf-cl-cycle.rsp');
      end)

  .Scenario('Response files turned off')
    .Given('a command with a positional argument, and the parser configured to read no response file',
      procedure(W: TSyntaxWorld)
      begin
        W.BuildGrammar;
        W.Configuration.ResponseFileTokenReplacer := nil;
      end)
    .When('the invocation passes @value',
      procedure(W: TSyntaxWorld)
      begin
        W.Parsed := W.Root.Parse(['build', '@value'], W.Configuration);
        W.ReadTarget := W.TargetArg.ValueIn(W.Parsed);
      end)
    .&Then('the argument receives @value',
      procedure(W: TSyntaxWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.ReadTarget).ToEqual('@value');
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

  .Scenario('What matches nothing is offered apart even when it is an error')
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
    .&Then('the parse fails, and the token is available apart',
      procedure(W: TSyntaxWorld)
      begin
        Expect(W.Parsed.Succeeded).ToBeFalse;
        Expect(string.Join(' ', W.Parsed.UnmatchedTokens)).ToEqual('stray');
      end)

  .Scenario('A root that collects outranks a subcommand that does not')
    .Given('a root declared not to treat what it does not recognise as an error, with a subcommand that declares nothing',
      procedure(W: TSyntaxWorld)
      begin
        W.BuildWrapper;
      end)
    .When('the invocation names the subcommand and passes a token that matches nothing',
      procedure(W: TSyntaxWorld)
      begin
        W.Parsed := W.Outer.Parse(['inner', 'stray']);
      end)
    .&Then('the parse succeeds, and the token is available apart',
      procedure(W: TSyntaxWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(string.Join(' ', W.Parsed.UnmatchedTokens)).ToEqual('stray');
      end)

.Scenario('A single-dash token whose only known letter is the help one is handed on whole')
    .Given('a command declared not to treat what it does not recognise as an error',
      procedure(W: TSyntaxWorld)
      begin
        W.BuildWrapper;
      end)
    .When('the invocation passes a single-dash token none of whose letters the command declares',
      procedure(W: TSyntaxWorld)
      begin
        // "-Fecha" carries an h, and the help letter counts as known: it is what lets -fh
        // reach the help. A group starts with a known letter, and F is not one.
        W.Parsed := W.Wrapper.Parse(['-u', 'admin', '-Fecha', '2026-09-26']);
      end)
    .&Then('the parse succeeds and the token goes on whole, value and all',
      procedure(W: TSyntaxWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(string.Join(' ', W.Parsed.UnmatchedTokens)).ToEqual('-Fecha 2026-09-26');
      end)

  .Scenario('A command that collects nothing names that token whole')
    .Given('a command that has declared nothing about unrecognised tokens',
      procedure(W: TSyntaxWorld)
      begin
        W.BuildWrapper;
      end)
    .When('the invocation passes a single-dash token none of whose letters the command declares',
      procedure(W: TSyntaxWorld)
      begin
        W.Parsed := W.Plain.Parse(['--keep', 'x', '-Fecha']);
      end)
    .&Then('the parse fails naming the token, not one of its letters',
      procedure(W: TSyntaxWorld)
      begin
        Expect(W.Parsed.Succeeded).ToBeFalse;
        Expect(W.Parsed.ErrorText).ToContain('''-Fecha''');
        Expect(Pos('inside the group', W.Parsed.ErrorText)).ToEqual(0);
      end)

.Rule('an argument can keep what is left of the invocation')

  .Scenario('The whole tail for the argument')
    .Given('a command with an argument declared to keep what is left, and an option --verbose',
      procedure(W: TSyntaxWorld)
      begin
        W.BuildTails;
      end)
    .When('the invocation passes a value for the argument and behind it --verbose and -x',
      procedure(W: TSyntaxWorld)
      begin
        W.Parsed := W.Tail.Parse(['app', '--verbose', '-x']);
        W.ReadTail := W.TailArg.ValueIn(W.Parsed);
      end)
    .&Then('the argument takes the three tokens, and --verbose is not taken as given',
      procedure(W: TSyntaxWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(string.Join(' ', W.ReadTail)).ToEqual('app --verbose -x');
        Expect(W.Parsed.WasProvided(W.TailVerboseOpt)).ToBeFalse;
      end)

  .Scenario('Without declaring it, an option is still an option')
    .Given('a command with an argument of variable arity and an option --verbose',
      procedure(W: TSyntaxWorld)
      begin
        W.BuildTails;
      end)
    .When('the invocation passes a value for the argument and behind it --verbose',
      procedure(W: TSyntaxWorld)
      begin
        W.Parsed := W.Loose.Parse(['app', '--verbose']);
        W.ReadTail := W.LooseArg.ValueIn(W.Parsed);
      end)
    .&Then('--verbose is taken as given, and the argument takes only the value',
      procedure(W: TSyntaxWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.Parsed.WasProvided(W.LooseVerboseOpt)).ToBeTrue;
        Expect(string.Join(' ', W.ReadTail)).ToEqual('app');
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
