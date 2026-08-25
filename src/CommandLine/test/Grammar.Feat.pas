unit Grammar.Feat;

interface

uses
  Daf.CommandLine;

type
  TGrammarWorld = class
  public
    // A two-level grammar: mytool build native <target> [--file] [--port], with a
    // recursive --verbose declared once at the root.
    Root: IRootCommand;
    Build: ICommand;
    Native: ICommand;
    FileOpt: IOption<string>;
    PortOpt: IOption<Integer>;
    VerboseOpt: IOption<Boolean>;
    TargetArg: IArgument<string>;
    // A flat grammar, so the type engine can be exercised without a subcommand
    // requirement getting in the way of what is being asserted.
    Calc: IRootCommand;
    AmountOpt: IOption<Integer>;
    CountArg: IArgument<Integer>;
    Parsed: ICommandLineParseResult;
    ReadFile: string;
    ReadPort: Integer;
    ReadTarget: string;
    ReadVerbose: Boolean;
    ReadAmount: Integer;
    ReadCount: Integer;
    procedure BuildGrammar;
    procedure BuildCalc;
  end;

implementation

uses
  Daf.MiniSpec;

{ TGrammarWorld }

procedure TGrammarWorld.BuildGrammar;
begin
  FileOpt := CommandLine.Option<string>('--file').Describe('File to compile').WithDefault('none');
  PortOpt := CommandLine.Option<Integer>('--port').Describe('Port to listen on');
  VerboseOpt := CommandLine.Option<Boolean>('--verbose').Describe('Show more detail').Recursive;
  TargetArg := CommandLine.Argument<string>('target').Describe('What to build');
  Native := CommandLine.Command('native').Describe('Build a native binary').Add([FileOpt, PortOpt, TargetArg]);
  Build := CommandLine.Command('build').WithAlias('b').Describe('Build things').Add(Native);
  Root := CommandLine.Root('mytool', 'A tool that builds things');
  Root.Add(VerboseOpt);
  Root.Add(Build);
end;

procedure TGrammarWorld.BuildCalc;
begin
  AmountOpt := CommandLine.Option<Integer>('--amount').Describe('How much');
  CountArg := CommandLine.Argument<Integer>('count').Describe('How many');
  Calc := CommandLine.Root('calc', 'Does sums');
  Calc.Add([AmountOpt, CountArg]);
end;

initialization

Feature('''
Feature Command line grammar @commandline @grammar

  As a developer writing a console application
  I want to declare commands, options and positional arguments
  So I can read their values back with the very objects I declared
''')

.UseWorld<TGrammarWorld>

.Rule('the declared object is the key its value is read with')

  .Scenario('Reading a declared option')
    .Given('a string option declared on a command',
      procedure(W: TGrammarWorld)
      begin
        W.BuildGrammar;
      end)
    .When('an invocation carrying it is parsed and read back with the declared object',
      procedure(W: TGrammarWorld)
      begin
        W.Parsed := W.Root.Parse(['build', 'native', 'app.dpr', '--file', 'main.pas']);
        W.ReadFile := W.FileOpt.ValueIn(W.Parsed);
      end)
    .&Then('the value comes back as a string, with no conversion and no name spelled out',
      procedure(W: TGrammarWorld)
      begin
        Expect(W.Parsed.Succeeded).ToBeTrue;
        Expect(W.ReadFile).ToEqual('main.pas');
      end)

  .Scenario('An option the invocation does not carry')
    .Given('an option declared with a default value',
      procedure(W: TGrammarWorld)
      begin
        W.BuildGrammar;
      end)
    .When('an invocation that does not name it is parsed',
      procedure(W: TGrammarWorld)
      begin
        W.Parsed := W.Root.Parse(['build', 'native', 'app.dpr']);
        W.ReadFile := W.FileOpt.ValueIn(W.Parsed);
      end)
    .&Then('the default is what comes back',
      procedure(W: TGrammarWorld)
      begin
        Expect(W.ReadFile).ToEqual('none');
      end)
    .&And('absent and present-but-empty are told apart, which the parked module could not do',
      procedure(W: TGrammarWorld)
      begin
        Expect(W.Parsed.WasProvided(W.FileOpt)).ToBeFalse;

        W.Parsed := W.Root.Parse(['build', 'native', 'app.dpr', '--file', '']);
        Expect(W.Parsed.WasProvided(W.FileOpt)).ToBeTrue;
        Expect(W.FileOpt.ValueIn(W.Parsed)).ToEqual('');
      end)

  .Scenario('The declared type is what comes back')
    .Given('options declared as string and as integer',
      procedure(W: TGrammarWorld)
      begin
        W.BuildGrammar;
      end)
    .When('both are read back through the declared objects',
      procedure(W: TGrammarWorld)
      begin
        W.Parsed := W.Root.Parse(['build', 'native', 'app.dpr', '--file', 'main.pas', '--port', '8080']);
        // These two assignments are the assertion. ReadPort is an Integer and
        // ReadFile a string, so they only compile because ValueIn returns the
        // declared type -- a TValue would not fit either of them. Reading the
        // integer option into a string is a COMPILE error, which is the half of
        // the requirement no runtime check can stand in for.
        W.ReadFile := W.FileOpt.ValueIn(W.Parsed);
        W.ReadPort := W.PortOpt.ValueIn(W.Parsed);
      end)
    .&Then('each value arrives with its declared type and needs no conversion',
      procedure(W: TGrammarWorld)
      begin
        Expect(W.ReadFile).ToEqual('main.pas');
        Expect(W.ReadPort).ToEqual(8080);
        Expect(W.ReadPort + 1).ToEqual(8081);
      end)

.Rule('the grammar takes nested commands, positionals and aliases')

  .Scenario('A two-level leaf')
    .Given('a root command with a subcommand that has another',
      procedure(W: TGrammarWorld)
      begin
        W.BuildGrammar;
      end)
    .When('an invocation naming the leaf with its options is parsed',
      procedure(W: TGrammarWorld)
      begin
        W.Parsed := W.Root.Parse(['build', 'native', 'app.dpr', '--file', 'main.pas']);
      end)
    .&Then('the result identifies the leaf reached and gives access to what it declared',
      procedure(W: TGrammarWorld)
      begin
        Expect(W.Parsed.Succeeded).ToBeTrue;
        Expect(W.Parsed.Command.Name).ToEqual('native');
        Expect(W.Parsed.Command.FullPath).ToEqual('mytool build native');
        Expect(W.FileOpt.ValueIn(W.Parsed)).ToEqual('main.pas');
      end)

  .Scenario('A command that only groups is not invocable')
    .Given('a command that only contains subcommands',
      procedure(W: TGrammarWorld)
      begin
        W.BuildGrammar;
      end)
    .When('an invocation names it and names no leaf',
      procedure(W: TGrammarWorld)
      begin
        W.Parsed := W.Root.Parse(['build']);
      end)
    .&Then('the parse fails saying the subcommand is missing',
      procedure(W: TGrammarWorld)
      begin
        Expect(W.Parsed.Succeeded).ToBeFalse;
        Expect(W.Parsed.ErrorText).ToContain('requires a subcommand');
        Expect(W.Parsed.ErrorText).ToContain('native');
      end)

  .Scenario('A positional argument')
    .Given('a command with a positional argument declared',
      procedure(W: TGrammarWorld)
      begin
        W.BuildGrammar;
      end)
    .When('an invocation passes its value without naming it',
      procedure(W: TGrammarWorld)
      begin
        W.Parsed := W.Root.Parse(['build', 'native', 'app.dpr']);
        W.ReadTarget := W.TargetArg.ValueIn(W.Parsed);
      end)
    .&Then('the value is bound to that argument',
      procedure(W: TGrammarWorld)
      begin
        Expect(W.Parsed.Succeeded).ToBeTrue;
        Expect(W.ReadTarget).ToEqual('app.dpr');
      end)

  .Scenario('A command alias')
    .Given('a command declared with an alias',
      procedure(W: TGrammarWorld)
      begin
        W.BuildGrammar;
      end)
    .When('an invocation uses the alias',
      procedure(W: TGrammarWorld)
      begin
        W.Parsed := W.Root.Parse(['b', 'native', 'app.dpr']);
      end)
    .&Then('the same command is reached as with its name',
      procedure(W: TGrammarWorld)
      begin
        Expect(W.Parsed.Succeeded).ToBeTrue;
        Expect(W.Parsed.Command.FullPath).ToEqual('mytool build native');
      end)

.Rule('a recursive option is inherited by the subcommands')

  .Scenario('Reading a recursive option from a leaf')
    .Given('a recursive option declared on the root command',
      procedure(W: TGrammarWorld)
      begin
        W.BuildGrammar;
      end)
    .When('an invocation passes it next to a second-level subcommand',
      procedure(W: TGrammarWorld)
      begin
        W.Parsed := W.Root.Parse(['build', 'native', 'app.dpr', '--verbose']);
        W.ReadVerbose := W.VerboseOpt.ValueIn(W.Parsed);
      end)
    .&Then('its value is read with the same object declared at the root',
      procedure(W: TGrammarWorld)
      begin
        Expect(W.Parsed.Succeeded).ToBeTrue;
        Expect(W.ReadVerbose).ToBeTrue;
      end)

.Rule('the value engine does not inherit the defects of the parked module')

  .Scenario('A negative integer')
    .Given('an integer argument declared',
      procedure(W: TGrammarWorld)
      begin
        W.BuildCalc;
      end)
    .When('the invocation passes it -5',
      procedure(W: TGrammarWorld)
      begin
        W.Parsed := W.Calc.Parse(['--amount', '-5', '-5']);
        W.ReadAmount := W.AmountOpt.ValueIn(W.Parsed);
        W.ReadCount := W.CountArg.ValueIn(W.Parsed);
      end)
    .&Then('the value read is -5, and not a parse failure',
      procedure(W: TGrammarWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.ReadAmount).ToEqual(-5);
        Expect(W.ReadCount).ToEqual(-5);
      end)

  .Scenario('A number with something left over')
    .Given('an integer argument declared',
      procedure(W: TGrammarWorld)
      begin
        W.BuildCalc;
      end)
    .When('the invocation passes it 80xyz',
      procedure(W: TGrammarWorld)
      begin
        W.Parsed := W.Calc.Parse(['--amount', '80xyz']);
        W.ReadAmount := W.AmountOpt.ValueIn(W.Parsed);
      end)
    .&Then('the parse fails naming the argument and the value received',
      procedure(W: TGrammarWorld)
      begin
        Expect(W.Parsed.Succeeded).ToBeFalse;
        Expect(W.Parsed.ErrorText).ToContain('80xyz');
        Expect(W.Parsed.ErrorText).ToContain('--amount');
      end)
    .&And('80 is not accepted -- the parked module read it as 80, a value invented from an invalid invocation',
      procedure(W: TGrammarWorld)
      begin
        Expect(W.ReadAmount).ToNotEqual(80);
        Expect(W.Parsed.WasProvided(W.AmountOpt)).ToBeFalse;
      end);

end.
