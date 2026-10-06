unit ParseRules.Feat;

interface

uses
  System.Classes,
  Daf.CommandLine;

type
  TParseRulesWorld = class
  public
    // tool --seed <int> [--port <int>] [--out <string>] [--verbose], with a version and an action.
    Flat: IRootCommand;
    SeedOpt: IOption<Integer>;
    PortOpt: IOption<Integer>;
    OutOpt: IOption<string>;
    VerboseOpt: IOption<Boolean>;
    // tool --seed <int> {build | serve}: a root that only groups.
    Group: IRootCommand;
    // tool [--count <int>] [--level <int>]: a validation that counts its runs, and a bounded set.
    Checked: IRootCommand;
    CountOpt: IOption<Integer>;
    LevelOpt: IOption<Integer>;
    // calc <count>
    Calc: IRootCommand;
    CountArg: IArgument<Integer>;
    // copy [--force] <source>
    Copier: IRootCommand;
    ForceOpt: IOption<Boolean>;
    SourceArg: IArgument<string>;
    // cp <sources>... <dest>
    Spread: IRootCommand;
    SourcesArg: IArgument<TArray<string>>;
    DestArg: IArgument<string>;
    // echo [<words>...]
    Echo: IRootCommand;
    WordsArg: IArgument<TArray<string>>;
    // tool [--port <int>] [--level <int>] --token <string> --profile <string> run, where only
    // --profile is recursive and --level has a default.
    Nested: IRootCommand;
    ParentPortOpt: IOption<Integer>;
    ParentLevelOpt: IOption<Integer>;
    TokenOpt: IOption<string>;
    ProfileOpt: IOption<string>;
    Run: ICommand;

    MissingResponseFile: string;
    ValidationRuns: Integer;
    Parsed: IParseResult;
    Output: TStringWriter;
    Error: TStringWriter;
    /// <summary>
    ///   Starts at -1 and not at 0: "the invocation answered 0" is exactly what a field would
    ///   hold if the When never ran.
    /// </summary>
    ExitCode: Integer;
    Ran: string;
    Report: string;
    constructor Create;
    destructor Destroy; override;
    function Configuration: TInvocationConfiguration;
    function RanText: string;
    /// <summary>What reading --port answers, or the exception it raises, as text.</summary>
    function ReadPort: string;
    procedure BuildFlat;
    procedure BuildGroup;
    procedure BuildChecked;
    procedure BuildCalc;
    procedure BuildCopier;
    procedure BuildSpread;
    procedure BuildEcho;
    procedure BuildNested;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  Daf.MiniSpec;

// What ErrorText is made of: the errors, one per line, in the order they were added.
function Lines(const Items: array of string): string;
begin
  Result := string.Join(sLineBreak, Items);
end;

{ TParseRulesWorld }

constructor TParseRulesWorld.Create;
begin
  inherited;
  Output := TStringWriter.Create;
  Error := TStringWriter.Create;
  ExitCode := -1;
end;

destructor TParseRulesWorld.Destroy;
begin
  Output.Free;
  Error.Free;
  inherited;
end;

function TParseRulesWorld.Configuration: TInvocationConfiguration;
begin
  Result.Output := Output;
  Result.Error := Error;
end;

function TParseRulesWorld.RanText: string;
begin
  if Ran = '' then
    Result := '<nothing>'
  else
    Result := Ran;
end;

function TParseRulesWorld.ReadPort: string;
begin
  try
    Result := Format('port=%d', [PortOpt.ValueIn(Parsed)]);
  except
    on E: Exception do
      Result := E.ClassName + ': ' + E.Message;
  end;
end;

procedure TParseRulesWorld.BuildFlat;
begin
  SeedOpt := CommandLine.Option<Integer>('--seed').Description('Starting point').Required;
  PortOpt := CommandLine.Option<Integer>('--port').Description('Port to listen on');
  OutOpt := CommandLine.Option<string>('--out').Description('Where to write');
  VerboseOpt := CommandLine.Option<Boolean>('--verbose').Description('Say more');
  Flat := CommandLine.Root('tool', 'A tool').WithVersion('1.0.0');
  Flat.Add([SeedOpt, PortOpt, OutOpt, VerboseOpt]);
  Flat.SetAction(
    function(const Parsed: IParseResult): Integer
    begin
      Ran := 'tool';
      Result := 5;
    end);
end;

procedure TParseRulesWorld.BuildGroup;
begin
  SeedOpt := CommandLine.Option<Integer>('--seed').Description('Starting point').Required;
  Group := CommandLine.Root('tool', 'A tool that only groups');
  Group.Add(SeedOpt);
  Group.Add(CommandLine.Command('build', 'Build things'));
  Group.Add(CommandLine.Command('serve', 'Serve things'));
end;

procedure TParseRulesWorld.BuildChecked;
begin
  ValidationRuns := 0;
  CountOpt := CommandLine.Option<Integer>('--count').Description('How many to make')
    .Validate(
    function(const Value: Integer; out Error: string): Boolean
    begin
      Inc(ValidationRuns);
      Error := '';
      Result := Value > 0;
    end);
  LevelOpt := CommandLine.Option<Integer>('--level').Description('How hard to try').AcceptOnlyFromAmong(['1', '2', '3']);
  Checked := CommandLine.Root('tool', 'A tool');
  Checked.Add([CountOpt, LevelOpt]);
end;

procedure TParseRulesWorld.BuildCalc;
begin
  CountArg := CommandLine.Argument<Integer>('count').Description('How many');
  Calc := CommandLine.Root('calc', 'Does sums');
  Calc.Add(CountArg);
end;

procedure TParseRulesWorld.BuildCopier;
begin
  ForceOpt := CommandLine.Option<Boolean>('--force').Description('Overwrite');
  SourceArg := CommandLine.Argument<string>('source').Description('What to copy');
  Copier := CommandLine.Root('copy', 'Copies one file');
  Copier.Add([ForceOpt, SourceArg]);
end;

procedure TParseRulesWorld.BuildSpread;
begin
  SourcesArg := CommandLine.Argument<TArray<string>>('sources').Description('What to copy');
  DestArg := CommandLine.Argument<string>('dest').Description('Where to');
  Spread := CommandLine.Root('cp', 'Copies many files');
  Spread.Add([SourcesArg, DestArg]);
end;

procedure TParseRulesWorld.BuildEcho;
begin
  WordsArg := CommandLine.Argument<TArray<string>>('words').Description('What to say');
  Echo := CommandLine.Root('echo', 'Says it back');
  Echo.Add(WordsArg);
end;

procedure TParseRulesWorld.BuildNested;
begin
  ParentPortOpt := CommandLine.Option<Integer>('--port').Description('Port to listen on');
  ParentLevelOpt := CommandLine.Option<Integer>('--level').Description('How hard to try').DefaultValue(3);
  TokenOpt := CommandLine.Option<string>('--token').Description('Who is asking').Required;
  ProfileOpt := CommandLine.Option<string>('--profile').Description('Which settings').Required.Recursive;
  Run := CommandLine.Command('run', 'Runs it');
  Nested := CommandLine.Root('tool', 'A tool with one subcommand');
  Nested.Add([ParentPortOpt, ParentLevelOpt, TokenOpt, ProfileOpt]);
  Nested.Add(Run);
end;

initialization

Feature('''
Feature Command line parse rules @commandline @parserules

  As a developer relying on what an invocation produces
  I want the order of its errors and the reach of each token fixed rule by rule
  So the same invocation keeps giving the same answer
''')

.UseWorld<TParseRulesWorld>

.Rule('the errors of an invocation come out in a fixed order')

  .Scenario('The errors of the tokens come first, then each option in the order it was declared')
    .Given('a command with an integer option and a required one, declared before it',
      procedure(W: TParseRulesWorld)
      begin
        W.BuildFlat;
      end)
    .When('an invocation with a bad value, an unknown option and a stray argument is parsed',
      procedure(W: TParseRulesWorld)
      begin
        W.Parsed := W.Flat.Parse(['--port', 'x', '--bogus', 'extra']);
      end)
    .&Then('the tokens nobody claimed come first, then the missing option, then the value that does not convert',
      procedure(W: TParseRulesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual(Lines([
          'Unknown option "--bogus" for tool',
          'Unexpected argument "extra" for tool',
          'Option "--seed" is required',
          '"x" is not a valid integer for --port']));
      end)

  .Scenario('A missing subcommand comes first, ahead of the errors of the tokens')
    .Given('a root that only groups, with a required option',
      procedure(W: TParseRulesWorld)
      begin
        W.BuildGroup;
      end)
    .When('an invocation that names no subcommand and carries an unknown option is parsed',
      procedure(W: TParseRulesWorld)
      begin
        W.Parsed := W.Group.Parse(['--bogus']);
      end)
    .&Then('the missing subcommand comes first, then the unknown option, then the missing option',
      procedure(W: TParseRulesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual(Lines([
          'Command "tool" requires a subcommand: build, serve',
          'Unknown option "--bogus" for tool',
          'Option "--seed" is required']));
      end)

  .Scenario('A response file that fails comes first, wherever it is named')
    .Given('a command, and a response file that does not exist',
      procedure(W: TParseRulesWorld)
      begin
        W.BuildFlat;
        W.MissingResponseFile := TPath.Combine(TPath.GetTempPath, 'daf-cl-rules-missing.rsp');
        if TFile.Exists(W.MissingResponseFile) then
          TFile.Delete(W.MissingResponseFile);
      end)
    .When('an invocation that names the file after an unknown option is parsed',
      procedure(W: TParseRulesWorld)
      begin
        W.Parsed := W.Flat.Parse(['--bogus', '--seed', '1', '@' + W.MissingResponseFile]);
      end)
    .&Then('the file comes out before the unknown option',
      procedure(W: TParseRulesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual(Lines([
          Format('Response file "%s" does not exist', [W.MissingResponseFile]),
          'Unknown option "--bogus" for tool']));
      end)

.Rule('a value is checked against its accepted set, converted and validated, and stops at the first refusal')

  .Scenario('The accepted set is checked before the conversion')
    .Given('an integer option bounded to a set of values',
      procedure(W: TParseRulesWorld)
      begin
        W.BuildChecked;
      end)
    .When('the invocation passes it something that is neither in the set nor an integer',
      procedure(W: TParseRulesWorld)
      begin
        W.Parsed := W.Checked.Parse(['--level', 'x']);
      end)
    .&Then('the error is the one of the set, not the one of the conversion',
      procedure(W: TParseRulesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('"x" is not a valid value for --level. Accepted: 1, 2, 3');
      end)

  .Scenario('A value that does not convert is not validated')
    .Given('an integer option with a validation of its own',
      procedure(W: TParseRulesWorld)
      begin
        W.BuildChecked;
      end)
    .When('the invocation passes it something that is not an integer',
      procedure(W: TParseRulesWorld)
      begin
        W.Parsed := W.Checked.Parse(['--count', 'abc']);
        W.Report := Format('runs=%d', [W.ValidationRuns]);
      end)
    .&Then('the only error is the one of the conversion, and the validation never ran',
      procedure(W: TParseRulesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('"abc" is not a valid integer for --count');
        Expect(W.Report).ToEqual('runs=0');
      end)

.Rule('a repeated option fails for the repetition, which is checked before its values convert')

  .Scenario('A repeated option whose second value is invalid fails for the repetition')
    .Given('an option that admits one value',
      procedure(W: TParseRulesWorld)
      begin
        W.BuildFlat;
      end)
    .When('the invocation names it twice, the second time with a value that does not convert',
      procedure(W: TParseRulesWorld)
      begin
        W.Parsed := W.Flat.Parse(['--seed', '1', '--port', '1', '--port', 'x']);
        W.Report := W.ReadPort;
      end)
    .&Then('the only error is the repetition, and reading the option raises it',
      procedure(W: TParseRulesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('Option "--port" was given more than once, and it takes exactly one value');
        Expect(W.Report).ToEqual('ECommandLine: Option "--port" was given more than once, and it takes exactly one value');
      end)

  .Scenario('A repeated option whose first value is invalid fails for the repetition too')
    .Given('an option that admits one value',
      procedure(W: TParseRulesWorld)
      begin
        W.BuildFlat;
      end)
    .When('the invocation names it twice, the first time with a value that does not convert',
      procedure(W: TParseRulesWorld)
      begin
        W.Parsed := W.Flat.Parse(['--seed', '1', '--port', 'x', '--port', '2']);
        W.Report := W.ReadPort;
      end)
    .&Then('the arity is checked before the conversion: the only error is the repetition, and reading raises it',
      procedure(W: TParseRulesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('Option "--port" was given more than once, and it takes exactly one value');
        Expect(W.Report).ToEqual('ECommandLine: Option "--port" was given more than once, and it takes exactly one value');
      end)

.Rule('a required symbol whose value does not convert has a result, so it is not reported as missing')

  .Scenario('A required option whose value does not convert fails once')
    .Given('a required integer option',
      procedure(W: TParseRulesWorld)
      begin
        W.BuildFlat;
      end)
    .When('the invocation passes it something that is not an integer',
      procedure(W: TParseRulesWorld)
      begin
        W.Parsed := W.Flat.Parse(['--seed', 'x']);
      end)
    .&Then('the only error is the conversion',
      procedure(W: TParseRulesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('"x" is not a valid integer for --seed');
      end)

  .Scenario('A required argument whose value does not convert fails once')
    .Given('an integer argument, which demands its value',
      procedure(W: TParseRulesWorld)
      begin
        W.BuildCalc;
      end)
    .When('the invocation passes it something that is not an integer',
      procedure(W: TParseRulesWorld)
      begin
        W.Parsed := W.Calc.Parse(['abc']);
      end)
    .&Then('the only error is the conversion',
      procedure(W: TParseRulesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('"abc" is not a valid integer for count');
      end)

.Rule('--help and --version end the parse where they appear')

  .Scenario('--help behind an unknown option asks for the help and keeps the error')
    .Given('a command with an action and a required option',
      procedure(W: TParseRulesWorld)
      begin
        W.BuildFlat;
      end)
    .When('the result of parsing an unknown option followed by --help is invoked',
      procedure(W: TParseRulesWorld)
      begin
        W.Parsed := W.Flat.Parse(['--bogus', '--help']);
        W.ExitCode := W.Parsed.Invoke(W.Configuration);
      end)
    .&Then('the help is asked for, and the error of the unknown option stays in the result',
      procedure(W: TParseRulesWorld)
      begin
        Expect(W.Parsed.IsHelpRequested).ToBeTrue;
        Expect(W.Parsed.ErrorText).ToEqual('Unknown option "--bogus" for tool');
      end)
    .&And('the invocation writes the help and no error, does not run the action, and answers 0',
      procedure(W: TParseRulesWorld)
      begin
        Expect(W.Output.ToString).ToEqual(W.Flat.HelpText + sLineBreak);
        Expect(W.Error.ToString).ToEqual('');
        Expect(W.RanText).ToEqual('<nothing>');
        Expect(W.ExitCode).ToEqual(TCommandLineExit.Ok);
      end)

  .Scenario('--help behind a value that does not convert asks for the help and keeps the error')
    .Given('a command with an integer option',
      procedure(W: TParseRulesWorld)
      begin
        W.BuildFlat;
      end)
    .When('an invocation passing it something that is not an integer, followed by --help, is parsed',
      procedure(W: TParseRulesWorld)
      begin
        W.Parsed := W.Flat.Parse(['--port', 'x', '--help']);
      end)
    .&Then('the help is asked for, and the error of the conversion stays in the result',
      procedure(W: TParseRulesWorld)
      begin
        Expect(W.Parsed.IsHelpRequested).ToBeTrue;
        Expect(W.Parsed.ErrorText).ToEqual('"x" is not a valid integer for --port');
      end)

  .Scenario('Nothing behind --help is read')
    .Given('a root that only groups',
      procedure(W: TParseRulesWorld)
      begin
        W.BuildGroup;
      end)
    .When('an invocation passing --help before a subcommand and an unknown option is parsed',
      procedure(W: TParseRulesWorld)
      begin
        W.Parsed := W.Group.Parse(['--help', 'build', '--bogus']);
      end)
    .&Then('the help is the one of the root, and neither the subcommand nor the unknown option counts',
      procedure(W: TParseRulesWorld)
      begin
        Expect(W.Parsed.IsHelpRequested).ToBeTrue;
        Expect(W.Parsed.Command.FullPath).ToEqual('tool');
        Expect(W.Parsed.ErrorText).ToEqual('');
      end)

  .Scenario('Nothing behind --version is read')
    .Given('a command with a version',
      procedure(W: TParseRulesWorld)
      begin
        W.BuildFlat;
      end)
    .When('an invocation passing --version followed by a stray argument is parsed',
      procedure(W: TParseRulesWorld)
      begin
        W.Parsed := W.Flat.Parse(['--version', 'extra']);
      end)
    .&Then('the version is asked for, and the stray argument is not reported',
      procedure(W: TParseRulesWorld)
      begin
        Expect(W.Parsed.IsVersionRequested).ToBeTrue;
        Expect(W.Parsed.ErrorText).ToEqual('');
      end)

.Rule('an option that expects a value takes the next token, whatever it is')

  .Scenario('A declared option behind an option that expects a value is that value')
    .Given('a string option and a flag',
      procedure(W: TParseRulesWorld)
      begin
        W.BuildFlat;
      end)
    .When('the invocation writes the flag right behind the string option',
      procedure(W: TParseRulesWorld)
      begin
        W.Parsed := W.Flat.Parse(['--seed', '1', '--out', '--verbose']);
        W.Report := Format('out=%s verbose=%s',
          [W.OutOpt.ValueIn(W.Parsed), BoolToStr(W.Parsed.WasProvided(W.VerboseOpt), True)]);
      end)
    .&Then('the flag is the value of the string option, and is not passed itself',
      procedure(W: TParseRulesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.Report).ToEqual('out=--verbose verbose=False');
      end)

  .Scenario('An option that expects a value at the end of the invocation')
    .Given('an integer option',
      procedure(W: TParseRulesWorld)
      begin
        W.BuildFlat;
      end)
    .When('the invocation ends right after its name',
      procedure(W: TParseRulesWorld)
      begin
        W.Parsed := W.Flat.Parse(['--seed', '1', '--port']);
      end)
    .&Then('the parse fails saying the option needs a value',
      procedure(W: TParseRulesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('Option "--port" requires a integer value');
      end)

.Rule('a flag takes the next token only when it is a boolean literal')

  .Scenario('A flag followed by something that is not a boolean leaves it as a positional')
    .Given('a flag and a positional argument',
      procedure(W: TParseRulesWorld)
      begin
        W.BuildCopier;
      end)
    .When('the invocation writes a file name right behind the flag',
      procedure(W: TParseRulesWorld)
      begin
        W.Parsed := W.Copier.Parse(['--force', 'a.txt']);
        W.Report := Format('force=%s source=%s',
          [BoolToStr(W.ForceOpt.ValueIn(W.Parsed), True), W.SourceArg.ValueIn(W.Parsed)]);
      end)
    .&Then('the flag is on, and the file name goes to the positional',
      procedure(W: TParseRulesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.Report).ToEqual('force=True source=a.txt');
      end)

  .Scenario('A flag followed by 0 leaves it as a positional, because 0 is not a boolean')
    .Given('a flag and a positional argument',
      procedure(W: TParseRulesWorld)
      begin
        W.BuildCopier;
      end)
    .When('the invocation writes 0 right behind the flag, and a file name after it',
      procedure(W: TParseRulesWorld)
      begin
        W.Parsed := W.Copier.Parse(['--force', '0', 'a.txt']);
        W.Report := Format('force=%s source=%s',
          [BoolToStr(W.ForceOpt.ValueIn(W.Parsed), True), W.SourceArg.ValueIn(W.Parsed)]);
      end)
    .&Then('the flag is on, 0 goes to the positional, and the file name behind it is unexpected',
      procedure(W: TParseRulesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('Unexpected argument "a.txt" for copy');
        Expect(W.Report).ToEqual('force=True source=0');
      end)

.Rule('positionals are handed out in order, and -- ends the options once')

  .Scenario('An argument that admits many takes every positional, and the next one is left missing')
    .Given('a collection argument followed by a single one',
      procedure(W: TParseRulesWorld)
      begin
        W.BuildSpread;
      end)
    .When('the invocation passes three positionals',
      procedure(W: TParseRulesWorld)
      begin
        W.Parsed := W.Spread.Parse(['a', 'b', 'c']);
        W.Report := 'sources=' + string.Join(' ', W.SourcesArg.ValueIn(W.Parsed));
      end)
    .&Then('the collection takes the three, and the single one is reported as missing',
      procedure(W: TParseRulesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('Argument "dest" is required');
        Expect(W.Report).ToEqual('sources=a b c');
      end)

  .Scenario('A second -- is a value')
    .Given('a collection argument',
      procedure(W: TParseRulesWorld)
      begin
        W.BuildEcho;
      end)
    .When('the invocation passes --, a word, -- again and another word',
      procedure(W: TParseRulesWorld)
      begin
        W.Parsed := W.Echo.Parse(['--', 'a', '--', 'b']);
        W.Report := 'words=' + string.Join(' ', W.WordsArg.ValueIn(W.Parsed));
      end)
    .&Then('the second -- is one of the values',
      procedure(W: TParseRulesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.Report).ToEqual('words=a -- b');
      end)

.Rule('a subcommand sees the recursive options of its parents, and no other')

  .Scenario('An option of the parent written before the subcommand is read')
    .Given('a root with an option that is not recursive, and a subcommand',
      procedure(W: TParseRulesWorld)
      begin
        W.BuildNested;
      end)
    .When('the invocation writes the option before naming the subcommand',
      procedure(W: TParseRulesWorld)
      begin
        W.Parsed := W.Nested.Parse(['--port', '1', '--profile', 'p', 'run']);
        W.Report := Format('command=%s port=%d', [W.Parsed.Command.FullPath, W.ParentPortOpt.ValueIn(W.Parsed)]);
      end)
    .&Then('the subcommand is reached and the option is read',
      procedure(W: TParseRulesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.Report).ToEqual('command=tool run port=1');
      end)

  .Scenario('An option of the parent that is not recursive is unknown behind the subcommand')
    .Given('a root with an option that is not recursive, and a subcommand',
      procedure(W: TParseRulesWorld)
      begin
        W.BuildNested;
      end)
    .When('the invocation writes the option after naming the subcommand',
      procedure(W: TParseRulesWorld)
      begin
        W.Parsed := W.Nested.Parse(['--profile', 'p', 'run', '--port', '1']);
        W.Report := 'command=' + W.Parsed.Command.FullPath;
      end)
    .&Then('the option is unknown to the subcommand, and its value is a stray argument',
      procedure(W: TParseRulesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual(Lines([
          'Unknown option "--port" for tool run',
          'Unexpected argument "1" for tool run']));
        Expect(W.Report).ToEqual('command=tool run');
      end)

  .Scenario('Of the required options of the parent, a subcommand demands only the recursive one')
    .Given('a root with a required option that is recursive, another that is not, and a subcommand',
      procedure(W: TParseRulesWorld)
      begin
        W.BuildNested;
      end)
    .When('an invocation that names only the subcommand is parsed',
      procedure(W: TParseRulesWorld)
      begin
        W.Parsed := W.Nested.Parse(['run']);
      end)
    .&Then('only the recursive one is reported as missing',
      procedure(W: TParseRulesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('Option "--profile" is required');
      end)

  .Scenario('The default of a parent option that is not recursive is read from a subcommand')
    .Given('a root with an option that is not recursive and has a default, and a subcommand',
      procedure(W: TParseRulesWorld)
      begin
        W.BuildNested;
      end)
    .When('an invocation that names the subcommand and not the option is parsed and read',
      procedure(W: TParseRulesWorld)
      begin
        W.Parsed := W.Nested.Parse(['--profile', 'p', 'run']);
        W.Report := Format('level=%d', [W.ParentLevelOpt.ValueIn(W.Parsed)]);
      end)
    .&Then('the value read is the default',
      procedure(W: TParseRulesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.Report).ToEqual('level=3');
      end);

end.
