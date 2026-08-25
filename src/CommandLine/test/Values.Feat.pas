unit Values.Feat;

{$SCOPEDENUMS ON}

interface

uses
  Daf.CommandLine;

type
  /// <summary>An enumerated type is the one that already names its own values.</summary>
  TVerbosity = (Quiet, Normal, Detailed);

  TValuesWorld = class
  public
    // One small grammar per rule: a flat root, so nothing else interferes with what
    // is being asserted.
    Arity: IRootCommand;
    PortOpt: IOption<Integer>;
    FilesOpt: IOption<string>;
    FlagOpt: IOption<Boolean>;

    NeedOpts: IRootCommand;
    MustOpt: IOption<string>;
    FallbackOpt: IOption<string>;

    NeedArg: IRootCommand;
    TargetArg: IArgument<string>;

    Bounded: IRootCommand;
    LevelOpt: IOption<TVerbosity>;
    ModeOpt: IOption<string>;

    Escaped: IRootCommand;
    MsgArg: IArgument<string>;
    VerboseOpt: IOption<Boolean>;

    Parsed: ICommandLineParseResult;
    ReadPort: Integer;
    ReadFiles: TArray<string>;
    ReadFlag: Boolean;
    ReadFallback: string;
    ReadLevel: TVerbosity;
    ReadMsg: string;
    procedure BuildArity;
    procedure BuildRequiredOptions;
    procedure BuildRequiredArgument;
    procedure BuildBounded;
    procedure BuildEscaped;
  end;

implementation

uses
  System.SysUtils,
  Daf.MiniSpec;

{ TValuesWorld }

procedure TValuesWorld.BuildArity;
begin
  PortOpt := CommandLine.Option<Integer>('--port', 'Port to listen on');
  FilesOpt := CommandLine.Option<string>('--files', 'Files to compile').WithArity(TArity.ZeroOrMore);
  FlagOpt := CommandLine.Option<Boolean>('--flag', 'A flag');
  Arity := CommandLine.Root('tool', 'A tool');
  Arity.Add([PortOpt, FilesOpt, FlagOpt]);
end;

procedure TValuesWorld.BuildRequiredOptions;
begin
  MustOpt := CommandLine.Option<string>('--must', 'Has to be there').Required;
  FallbackOpt := CommandLine.Option<string>('--fallback', 'Required, but has a default').Required.WithDefault('D');
  NeedOpts := CommandLine.Root('tool', 'A tool');
  NeedOpts.Add([MustOpt, FallbackOpt]);
end;

procedure TValuesWorld.BuildRequiredArgument;
begin
  // A string argument gets ExactlyOne by default, so it is required without saying so.
  TargetArg := CommandLine.Argument<string>('target', 'What to build');
  NeedArg := CommandLine.Root('tool', 'A tool');
  NeedArg.Add(TargetArg);
end;

procedure TValuesWorld.BuildBounded;
begin
  LevelOpt := CommandLine.Option<TVerbosity>('--level', 'How much to say');
  ModeOpt := CommandLine.Option<string>('--mode', 'How to run').AcceptOnly(['fast', 'safe']);
  Bounded := CommandLine.Root('tool', 'A tool');
  Bounded.Add([LevelOpt, ModeOpt]);
end;

procedure TValuesWorld.BuildEscaped;
begin
  MsgArg := CommandLine.Argument<string>('msg', 'The message');
  VerboseOpt := CommandLine.Option<Boolean>('--verbose', 'Say more');
  Escaped := CommandLine.Root('tool', 'A tool');
  Escaped.Add([MsgArg, VerboseOpt]);
end;

initialization

Feature('''
Feature Command line values @commandline @values

  As a developer writing a console application
  I want to say how many values a symbol takes and which ones are valid
  So an ambiguous or incomplete invocation fails instead of being guessed at
''')

.UseWorld<TValuesWorld>

.Rule('the arity says how many values a symbol admits')

  .Scenario('A repeated option that admits one value')
    .Given('an option declared with a maximum arity of one',
      procedure(W: TValuesWorld)
      begin
        W.BuildArity;
      end)
    .When('the invocation names it twice with different values',
      procedure(W: TValuesWorld)
      begin
        W.Parsed := W.Arity.Parse(['--port', '1', '--port', '2']);
        W.ReadPort := W.PortOpt.ValueIn(W.Parsed);
      end)
    .&Then('the parse fails naming the option',
      procedure(W: TValuesWorld)
      begin
        Expect(W.Parsed.Succeeded).ToBeFalse;
        Expect(W.Parsed.ErrorText).ToContain('--port');
        Expect(W.Parsed.ErrorText).ToContain('more than once');
      end)
    .&And('it does not quietly settle on either of the two values',
      procedure(W: TValuesWorld)
      begin
        // The parked behaviour was to keep the last one and say nothing, which is the
        // same sin as reading 80xyz as 80.
        Expect(W.ReadPort).ToNotEqual(2);
      end)

  .Scenario('An option that admits several values')
    .Given('an option declared with a maximum arity above one',
      procedure(W: TValuesWorld)
      begin
        W.BuildArity;
      end)
    .When('the invocation repeats it with different values',
      procedure(W: TValuesWorld)
      begin
        W.Parsed := W.Arity.Parse(['--files', 'a.pas', '--files', 'b.pas']);
        W.ReadFiles := W.FilesOpt.ValuesIn(W.Parsed);
      end)
    .&Then('every value is read, in the order they came',
      procedure(W: TValuesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(Length(W.ReadFiles)).ToEqual(2);
        Expect(W.ReadFiles[0]).ToEqual('a.pas');
        Expect(W.ReadFiles[1]).ToEqual('b.pas');
      end)

  .Scenario('A flag with its explicit value')
    .Given('a boolean option, whose default arity admits none or one value',
      procedure(W: TValuesWorld)
      begin
        W.BuildArity;
      end)
    .When('the invocation passes it followed by false',
      procedure(W: TValuesWorld)
      begin
        W.Parsed := W.Arity.Parse(['--flag', 'false']);
        W.ReadFlag := W.FlagOpt.ValueIn(W.Parsed);
      end)
    .&Then('the value read is false, and false is not taken for a positional',
      procedure(W: TValuesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.ReadFlag).ToBeFalse;
        Expect(W.Parsed.WasProvided(W.FlagOpt)).ToBeTrue;
      end)

  .Scenario('The default arity comes from the type')
    .Given('an integer option and a boolean one, neither with a declared arity',
      procedure(W: TValuesWorld)
      begin
        W.BuildArity;
      end)
    .When('each arity is inspected').NoAction
    .&Then('the integer demands exactly one value and the boolean admits none or one',
      procedure(W: TValuesWorld)
      begin
        Expect(W.PortOpt.Arity.Min).ToEqual(1);
        Expect(W.PortOpt.Arity.Max).ToEqual(1);
        Expect(W.FlagOpt.Arity.Min).ToEqual(0);
        Expect(W.FlagOpt.Arity.Max).ToEqual(1);
      end)

.Rule('a required symbol that does not come makes the parse fail')

  .Scenario('A required option the invocation does not carry')
    .Given('an option declared required and with no default',
      procedure(W: TValuesWorld)
      begin
        W.BuildRequiredOptions;
      end)
    .When('an invocation that does not name it is parsed',
      procedure(W: TValuesWorld)
      begin
        W.Parsed := W.NeedOpts.Parse([]);
      end)
    .&Then('the parse fails naming the missing option',
      procedure(W: TValuesWorld)
      begin
        Expect(W.Parsed.Succeeded).ToBeFalse;
        Expect(W.Parsed.ErrorText).ToContain('--must');
        Expect(W.Parsed.ErrorText).ToContain('required');
      end)

  .Scenario('A required option that has a default')
    .Given('an option declared required and with a default value',
      procedure(W: TValuesWorld)
      begin
        W.BuildRequiredOptions;
      end)
    .When('an invocation that does not name it is parsed',
      procedure(W: TValuesWorld)
      begin
        W.Parsed := W.NeedOpts.Parse([]);
        W.ReadFallback := W.FallbackOpt.ValueIn(W.Parsed);
      end)
    .&Then('it is not reported as missing, and the default is what is read',
      procedure(W: TValuesWorld)
      begin
        Expect(W.Parsed.ErrorText.Contains('--fallback')).ToBeFalse;
        Expect(W.ReadFallback).ToEqual('D');
      end)

  .Scenario('A positional argument that does not come')
    .Given('a command with a positional argument whose arity demands a value',
      procedure(W: TValuesWorld)
      begin
        W.BuildRequiredArgument;
      end)
    .When('an invocation that does not pass it is parsed',
      procedure(W: TValuesWorld)
      begin
        W.Parsed := W.NeedArg.Parse([]);
      end)
    .&Then('the parse fails naming the missing argument',
      procedure(W: TValuesWorld)
      begin
        Expect(W.Parsed.Succeeded).ToBeFalse;
        Expect(W.Parsed.ErrorText).ToContain('target');
        Expect(W.Parsed.ErrorText).ToContain('required');
      end)

.Rule('a symbol can bound the values it accepts')

  .Scenario('A valid value of an enumerated type')
    .Given('an option declared with an enumerated type',
      procedure(W: TValuesWorld)
      begin
        W.BuildBounded;
      end)
    .When('the invocation passes the name of one of its values, in any casing',
      procedure(W: TValuesWorld)
      begin
        W.Parsed := W.Bounded.Parse(['--level', 'detailed']);
        W.ReadLevel := W.LevelOpt.ValueIn(W.Parsed);
      end)
    .&Then('that value is read, with the declared enumerated type',
      procedure(W: TValuesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(Ord(W.ReadLevel)).ToEqual(Ord(TVerbosity.Detailed));
      end)

  .Scenario('A value the enumerated type does not have')
    .Given('an option declared with an enumerated type',
      procedure(W: TValuesWorld)
      begin
        W.BuildBounded;
      end)
    .When('the invocation passes a name that is not one of its values',
      procedure(W: TValuesWorld)
      begin
        W.Parsed := W.Bounded.Parse(['--level', 'silent']);
      end)
    .&Then('the parse fails naming the option, the value and what is accepted',
      procedure(W: TValuesWorld)
      begin
        Expect(W.Parsed.Succeeded).ToBeFalse;
        Expect(W.Parsed.ErrorText).ToContain('--level');
        Expect(W.Parsed.ErrorText).ToContain('silent');
        Expect(W.Parsed.ErrorText).ToContain('Quiet');
        Expect(W.Parsed.ErrorText).ToContain('Detailed');
      end)

  .Scenario('An explicit set of accepted values')
    .Given('an option bounded to a declared set of values',
      procedure(W: TValuesWorld)
      begin
        W.BuildBounded;
      end)
    .When('the invocation passes one that is not in the set',
      procedure(W: TValuesWorld)
      begin
        W.Parsed := W.Bounded.Parse(['--mode', 'turbo']);
      end)
    .&Then('the parse fails listing the values of the set',
      procedure(W: TValuesWorld)
      begin
        Expect(W.Parsed.Succeeded).ToBeFalse;
        Expect(W.Parsed.ErrorText).ToContain('turbo');
        Expect(W.Parsed.ErrorText).ToContain('fast');
        Expect(W.Parsed.ErrorText).ToContain('safe');
      end)

.Rule('-- ends the options')

  .Scenario('A value that looks like an option')
    .Given('a command with a positional argument and a declared option',
      procedure(W: TValuesWorld)
      begin
        W.BuildEscaped;
      end)
    .When('the invocation passes -- and behind it the name of that option',
      procedure(W: TValuesWorld)
      begin
        W.Parsed := W.Escaped.Parse(['--', '--verbose']);
        W.ReadMsg := W.MsgArg.ValueIn(W.Parsed);
      end)
    .&Then('the value goes to the positional and the option is not considered passed',
      procedure(W: TValuesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.ReadMsg).ToEqual('--verbose');
        Expect(W.Parsed.WasProvided(W.VerboseOpt)).ToBeFalse;
      end)

  .Scenario('-- with nothing behind it')
    .Given('a valid invocation',
      procedure(W: TValuesWorld)
      begin
        W.BuildEscaped;
      end)
    .When('it ends in --',
      procedure(W: TValuesWorld)
      begin
        W.Parsed := W.Escaped.Parse(['hello', '--']);
      end)
    .&Then('the parse succeeds and -- is not reported as an unknown option',
      procedure(W: TValuesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.Parsed.Succeeded).ToBeTrue;
      end);

end.
