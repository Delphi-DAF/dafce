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
    SpeedOpt: IOption<string>;
    StyleOpt: IOption<string>;

    Checked: IRootCommand;
    CountOpt: IOption<Integer>;
    InputOpt: IOption<string>;
    NameOpt: IOption<string>;
    PathOpt: IOption<string>;

    Computed: IRootCommand;
    OutOpt: IOption<string>;
    // A default calculated from another option of the same invocation.
    FromAnother: IRootCommand;
    WhoOpt: IOption<string>;
    FileOpt: IOption<string>;
    SeedOpt: IOption<Integer>;

    Escaped: IRootCommand;
    MsgArg: IArgument<string>;
    VerboseOpt: IOption<Boolean>;

    Parsed: IParseResult;
    ReadPortMin: Integer;
    ReadPortMax: Integer;
    ReadFlagMin: Integer;
    ReadFlagMax: Integer;
    ReadPort: Integer;
    ReadFiles: TArray<string>;
    ReadFlag: Boolean;
    ReadFallback: string;
    ReadLevel: TVerbosity;
    ReadMode: string;
    ReadMsg: string;
    ReadCount: Integer;
    ReadInput: string;
    ReadName: string;
    ReadOut: string;
    HelpText: string;
    SeedDefault: string;
    Report: string;
    procedure BuildArity;
    procedure BuildRequiredOptions;
    procedure BuildRequiredArgument;
    procedure BuildBounded;
    procedure BuildChecked;
    procedure BuildComputed;
    procedure BuildComputedFromAnother;
    procedure BuildEscaped;
  end;

implementation

uses
  System.SysUtils,
  System.Generics.Defaults,
  Daf.MiniSpec;

var
  // How many times each computed default has run. Counters of the unit and not fields
  // of the world: what has to be observed is that a calculation did NOT run, and a
  // counter the closure reaches without capturing the world is the plainest way to.
  OutRuns: Integer;
  SeedRuns: Integer;

{ TValuesWorld }

procedure TValuesWorld.BuildArity;
begin
  PortOpt := CommandLine.Option<Integer>('--port').Description('Port to listen on');
  FilesOpt := CommandLine.Option<string>('--files').Description('Files to compile').Arity(TArgumentArity.ZeroOrMore);
  FlagOpt := CommandLine.Option<Boolean>('--flag').Description('A flag');
  Arity := CommandLine.Root('tool', 'A tool');
  Arity.Add([PortOpt, FilesOpt, FlagOpt]);
end;

procedure TValuesWorld.BuildRequiredOptions;
begin
  MustOpt := CommandLine.Option<string>('--must').Description('Has to be there').Required;
  FallbackOpt := CommandLine.Option<string>('--fallback').Description('Required, but has a default').Required.DefaultValue('D');
  NeedOpts := CommandLine.Root('tool', 'A tool');
  NeedOpts.Add([MustOpt, FallbackOpt]);
end;

procedure TValuesWorld.BuildRequiredArgument;
begin
  // A string argument gets ExactlyOne by default, so it is required without saying so.
  TargetArg := CommandLine.Argument<string>('target').Description('What to build');
  NeedArg := CommandLine.Root('tool', 'A tool');
  NeedArg.Add(TargetArg);
end;

procedure TValuesWorld.BuildBounded;
begin
  LevelOpt := CommandLine.Option<TVerbosity>('--level').Description('How much to say');
  ModeOpt := CommandLine.Option<string>('--mode').Description('How to run').AcceptOnlyFromAmong(['fast', 'safe']);
  SpeedOpt := CommandLine.Option<string>('--speed').Description('How fast')
    .AcceptOnlyFromAmong(TIStringComparer.Ordinal, ['fast', 'safe']);
  // A conversion of its own that writes the value in capitals: what the set is checked against is the
  // token as it came, before any conversion.
  StyleOpt := CommandLine.Option<string>('--style').Description('How it looks')
    .CustomParser(
    function(const Found: IArgumentResult): string
    begin
      Result := Found.Tokens[0].Value.ToUpper;
    end).AcceptOnlyFromAmong(['fast', 'safe']);
  Bounded := CommandLine.Root('tool', 'A tool');
  Bounded.Add([LevelOpt, ModeOpt, SpeedOpt, StyleOpt]);
end;

procedure TValuesWorld.BuildChecked;
begin
  // A validation runs INSIDE the parse, so a value the type admits and the application
  // does not is an invocation error like any other.
  CountOpt := CommandLine.Option<Integer>('--count').Description('How many to make')
    .Validate(
    function(const Value: Integer; out Error: string): Boolean
    begin
      Result := Value > 0;
      if not Result then
        Error := Format('--count has to be greater than zero, and %d is not', [Value]);
    end);
  // Written over that same hook, not over a mechanism of its own. It does touch disk,
  // which is the conscious exception to a validation being a pure question.
  InputOpt := CommandLine.Option<string>('--in').Description('Where to read from').AcceptExistingOnly;
  // A NAME and not a path: the separators are refused too, which is what tells this
  // validation from the one above it.
  NameOpt := CommandLine.Option<string>('--name').Description('What to call the report').AcceptLegalFileNamesOnly;
  // A PATH and not a name: the separators are what a path is made of.
  PathOpt := CommandLine.Option<string>('--path').Description('Where to put the report').AcceptLegalFilePathsOnly;
  Checked := CommandLine.Root('tool', 'A tool');
  Checked.Add([CountOpt, InputOpt, NameOpt, PathOpt]);
end;

procedure TValuesWorld.BuildComputed;
begin
  OutRuns := 0;
  SeedRuns := 0;
  // A default that depends on the surroundings cannot be written as a constant in the
  // grammar, which runs before there is anything to know. The text next to
  // it is what the help shows, because the help does NOT run the calculation.
  OutOpt := CommandLine.Option<string>('--out').Description('Where to write')
    .DefaultValueFactory(
    function: string
    begin
      Inc(OutRuns);
      Result := 'computed-place';
    end, 'the current directory');
  SeedOpt := CommandLine.Option<Integer>('--seed').Description('Starting point')
    .DefaultValueFactory(
    function: Integer
    begin
      Inc(SeedRuns);
      Result := 7;
    end);
  Computed := CommandLine.Root('tool', 'A tool');
  Computed.Add([OutOpt, SeedOpt]);
end;

procedure TValuesWorld.BuildComputedFromAnother;
var
  Who: IOption<string>;
begin
  // The calculation receives the result of the option it is the default of, and reads another value of the
  // same invocation through it.
  Who := CommandLine.Option<string>('--who').Description('Who it is for');
  WhoOpt := Who;
  FileOpt := CommandLine.Option<string>('--file').Description('Where to write').DefaultValueFactory(
    function(const Found: IArgumentResult): string
    begin
      Result := Who.ValueIn(Found) + '.txt';
    end, '<who>.txt');
  FromAnother := CommandLine.Root('tool', 'A tool');
  FromAnother.Add([WhoOpt, FileOpt]);
end;

procedure TValuesWorld.BuildEscaped;
begin
  MsgArg := CommandLine.Argument<string>('msg').Description('The message');
  VerboseOpt := CommandLine.Option<Boolean>('--verbose').Description('Say more');
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
        try
          W.ReadPort := W.PortOpt.ValueIn(W.Parsed);
          W.Report := 'read ' + IntToStr(W.ReadPort);
        except
          on E: Exception do
            W.Report := E.ClassName + ': ' + E.Message;
        end;
      end)
    .&Then('the parse fails naming the option',
      procedure(W: TValuesWorld)
      begin
        Expect(W.Parsed.Succeeded).ToBeFalse;
        Expect(W.Parsed.ErrorText).ToContain('--port');
        Expect(W.Parsed.ErrorText).ToContain('more than once');
      end)
    .&And('it does not quietly settle on either of the two values: reading it raises the error',
      procedure(W: TValuesWorld)
      begin
        Expect(W.Report).ToEqual('ECommandLine: Option "--port" was given more than once, and it takes exactly one value');
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
    .When('each arity is inspected',
      procedure(W: TValuesWorld)
      begin
        W.ReadPortMin := W.PortOpt.Arity.MinimumNumberOfValues;
        W.ReadPortMax := W.PortOpt.Arity.MaximumNumberOfValues;
        W.ReadFlagMin := W.FlagOpt.Arity.MinimumNumberOfValues;
        W.ReadFlagMax := W.FlagOpt.Arity.MaximumNumberOfValues;
      end)
    .&Then('the integer demands exactly one value and the boolean admits none or one',
      procedure(W: TValuesWorld)
      begin
        Expect(W.ReadPortMin).ToEqual(1);
        Expect(W.ReadPortMax).ToEqual(1);
        Expect(W.ReadFlagMin).ToEqual(0);
        Expect(W.ReadFlagMax).ToEqual(1);
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

  .Scenario('The explicit set tells cases apart')
    .Given('an option bounded to the set fast, safe, with no comparer',
      procedure(W: TValuesWorld)
      begin
        W.BuildBounded;
      end)
    .When('the invocation passes FAST',
      procedure(W: TValuesWorld)
      begin
        W.Parsed := W.Bounded.Parse(['--mode', 'FAST']);
      end)
    .&Then('the parse fails listing the values of the set',
      procedure(W: TValuesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('"FAST" is not a valid value for --mode. Accepted: fast, safe');
      end)

  .Scenario('An explicit set with a comparer that does not tell cases apart')
    .Given('an option bounded to the set fast, safe, with a comparer that does not tell cases apart',
      procedure(W: TValuesWorld)
      begin
        W.BuildBounded;
      end)
    .When('the invocation passes FAST',
      procedure(W: TValuesWorld)
      begin
        W.Parsed := W.Bounded.Parse(['--speed', 'FAST']);
        W.ReadMode := W.SpeedOpt.ValueIn(W.Parsed);
      end)
    .&Then('the value is read as it came',
      procedure(W: TValuesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.ReadMode).ToEqual('FAST');
      end)

  .Scenario('An explicit set over a conversion of its own')
    .Given('an option bounded to the set fast, safe, whose conversion of its own writes the value in capitals',
      procedure(W: TValuesWorld)
      begin
        W.BuildBounded;
      end)
    .When('the invocation passes a value outside the set',
      procedure(W: TValuesWorld)
      begin
        W.Parsed := W.Bounded.Parse(['--style', 'turbo']);
      end)
    .&Then('the parse fails listing the values of the set, as if there were no conversion',
      procedure(W: TValuesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('"turbo" is not a valid value for --style. Accepted: fast, safe');
      end)

  .Scenario('A value of the set reaches the conversion of its own')
    .Given('an option bounded to the set fast, safe, whose conversion of its own writes the value in capitals',
      procedure(W: TValuesWorld)
      begin
        W.BuildBounded;
      end)
    .When('the invocation passes a value of the set',
      procedure(W: TValuesWorld)
      begin
        // The control the refusing scenario needs: a set that refused everything would leave it green.
        W.Parsed := W.Bounded.Parse(['--style', 'fast']);
        W.ReadMode := W.StyleOpt.ValueIn(W.Parsed);
      end)
    .&Then('the set accepts it and the conversion runs on it',
      procedure(W: TValuesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.ReadMode).ToEqual('FAST');
      end)

  .Scenario('A validation of its own that the type does not express')
    .Given('an integer option with a validation demanding a value above zero',
      procedure(W: TValuesWorld)
      begin
        W.BuildChecked;
      end)
    .When('the invocation passes a value the type admits and the validation does not',
      procedure(W: TValuesWorld)
      begin
        // Two failures in one invocation, on purpose: one from the validation and one
        // from the grammar. If they came out by different routes, only one would show.
        W.Parsed := W.Checked.Parse(['--count', '-3', '--nope']);
      end)
    .&Then('the parse fails with the message the validation wrote',
      procedure(W: TValuesWorld)
      begin
        Expect(W.Parsed.Succeeded).ToBeFalse;
        Expect(W.Parsed.ErrorText).ToContain('greater than zero');
      end)
    .&And('it arrives by the same route as the rest of the invocation errors',
      procedure(W: TValuesWorld)
      begin
        Expect(Length(W.Parsed.Errors)).ToEqual(2);
        Expect(W.Parsed.ErrorText).ToContain('--nope');
      end)

  .Scenario('A validation of its own that accepts the value')
    .Given('an integer option with a validation demanding a value above zero',
      procedure(W: TValuesWorld)
      begin
        W.BuildChecked;
      end)
    .When('the invocation passes a value the validation accepts',
      procedure(W: TValuesWorld)
      begin
        W.Parsed := W.Checked.Parse(['--count', '3']);
        W.ReadCount := W.CountOpt.ValueIn(W.Parsed);
      end)
    .&Then('the parse succeeds and the value is read',
      procedure(W: TValuesWorld)
      begin
        // The control the refusing scenario needs: a validation that refused everything
        // would leave that one green too.
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.ReadCount).ToEqual(3);
      end)

  .Scenario('A path that has to exist')
    .Given('a path option declared to accept only what exists',
      procedure(W: TValuesWorld)
      begin
        W.BuildChecked;
      end)
    .When('the invocation passes a path that does not exist',
      procedure(W: TValuesWorld)
      begin
        W.Parsed := W.Checked.Parse(['--in', 'no-such-file-anywhere.txt']);
      end)
    .&Then('the parse fails naming the option and the path',
      procedure(W: TValuesWorld)
      begin
        Expect(W.Parsed.Succeeded).ToBeFalse;
        Expect(W.Parsed.ErrorText).ToContain('--in');
        Expect(W.Parsed.ErrorText).ToContain('no-such-file-anywhere.txt');
      end)

  .Scenario('A path that does exist')
    .Given('a path option declared to accept only what exists',
      procedure(W: TValuesWorld)
      begin
        W.BuildChecked;
      end)
    .When('the invocation passes a path that exists',
      procedure(W: TValuesWorld)
      begin
        // The running executable, which exists by definition.
        W.Parsed := W.Checked.Parse(['--in', ParamStr(0)]);
        W.ReadInput := W.InputOpt.ValueIn(W.Parsed);
      end)
    .&Then('the parse succeeds and the path is read',
      procedure(W: TValuesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.ReadInput).ToEqual(ParamStr(0));
      end)

  .Scenario('A file name that carries a separator')
    .Given('an option declared to accept only a legal file name',
      procedure(W: TValuesWorld)
      begin
        W.BuildChecked;
      end)
    .When('the invocation passes a value with a separator in it',
      procedure(W: TValuesWorld)
      begin
        W.Parsed := W.Checked.Parse(['--name', 'reports\march.txt']);
      end)
    .&Then('the parse fails naming the option and the value',
      procedure(W: TValuesWorld)
      begin
        // The message is asserted and not only the failure: an option that expects a
        // value and did not get one fails here too, and would leave this green.
        Expect(W.Parsed.Succeeded).ToBeFalse;
        Expect(W.Parsed.ErrorText).ToContain('is not a legal file name');
        Expect(W.Parsed.ErrorText).ToContain('--name');
        Expect(W.Parsed.ErrorText).ToContain('reports\march.txt');
      end)

  .Scenario('A file name with nothing illegal in it')
    .Given('an option declared to accept only a legal file name',
      procedure(W: TValuesWorld)
      begin
        W.BuildChecked;
      end)
    .When('the invocation passes a name the validation accepts',
      procedure(W: TValuesWorld)
      begin
        W.Parsed := W.Checked.Parse(['--name', 'march.txt']);
        W.ReadName := W.NameOpt.ValueIn(W.Parsed);
      end)
    .&Then('the parse succeeds and the name is read',
      procedure(W: TValuesWorld)
      begin
        // The control the two refusing scenarios need: a validation that refused
        // everything would leave both of them green.
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.ReadName).ToEqual('march.txt');
      end)

  .Scenario('A file name with nothing in it')
    .Given('an option declared to accept only a legal file name',
      procedure(W: TValuesWorld)
      begin
        W.BuildChecked;
      end)
    .When('the invocation passes an empty value',
      procedure(W: TValuesWorld)
      begin
        W.Parsed := W.Checked.Parse(['--name', '']);
      end)
    .&Then('the parse fails, because nothing is not a name either',
      procedure(W: TValuesWorld)
      begin
        Expect(W.Parsed.Succeeded).ToBeFalse;
        Expect(W.Parsed.ErrorText).ToContain('is not a legal file name');
        Expect(W.Parsed.ErrorText).ToContain('--name');
      end)

  .Scenario('A file name with a character the platform does not admit')
    .Given('an option declared to accept only a legal file name',
      procedure(W: TValuesWorld)
      begin
        W.BuildChecked;
      end)
    .When('the invocation passes a name with a slash in it',
      procedure(W: TValuesWorld)
      begin
        // The slash, because no platform admits it in a file name: the list is the platform's.
        W.Parsed := W.Checked.Parse(['--name', 'march/report.txt']);
      end)
    .&Then('the parse fails naming that character',
      procedure(W: TValuesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual(
          '"march/report.txt" is not a legal file name for --name: it carries "/", which a file name cannot');
      end)

  .Scenario('A path with a character the platform does not admit')
    .Given('an option declared to accept only a legal path',
      procedure(W: TValuesWorld)
      begin
        W.BuildChecked;
      end)
    .When('the invocation passes a path with a tab in it',
      procedure(W: TValuesWorld)
      begin
        // A control character, because no platform admits one in a path.
        W.Parsed := W.Checked.Parse(['--path', 'reports'#9'march.txt']);
      end)
    .&Then('the parse fails naming that character by its code',
      procedure(W: TValuesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual(
          '"reports'#9'march.txt" is not a legal path for --path: it carries #9, which a path cannot');
      end)

  .Scenario('A path with separators in it')
    .Given('an option declared to accept only a legal path',
      procedure(W: TValuesWorld)
      begin
        W.BuildChecked;
      end)
    .When('the invocation passes a path with separators',
      procedure(W: TValuesWorld)
      begin
        W.Parsed := W.Checked.Parse(['--path', 'reports/march.txt']);
        W.ReadName := W.PathOpt.ValueIn(W.Parsed);
      end)
    .&Then('the parse succeeds, because a separator is what a path is made of',
      procedure(W: TValuesWorld)
      begin
        // The control the refusing scenario needs, and what tells a path from a name.
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.ReadName).ToEqual('reports/march.txt');
      end)

.Rule('a validation that only fits a path is refused where it is declared')

  .Scenario('A file-name validation on an option that is not a path')
    // Nothing to build and nothing to assert yet: whether the validation fits its
    // symbol cannot be asked before a symbol is declared with it.
    .Given('an option whose type is not a path').NoAction
    .When('a file-name validation is declared on it',
      procedure(W: TValuesWorld)
      begin
        try
          CommandLine.Option<Integer>('--retries').Description('How many times to try').AcceptLegalFileNamesOnly;
          W.Report := '<the declaration was accepted>';
        except
          on E: Exception do
            W.Report := E.ClassName + ': ' + E.Message;
        end;
      end)
    .&Then('the declaration itself is refused, naming the validation, the symbol and its type',
      procedure(W: TValuesWorld)
      begin
        Expect(W.Report).ToStartWith('ECommandLine: ');
        Expect(W.Report).ToContain('AcceptLegalFileNamesOnly');
        Expect(W.Report).ToContain('--retries');
        Expect(W.Report).ToContain('integer');
      end)

.Rule('a default value can be a calculation instead of a constant')

  .Scenario('A computed default that reads another value of the invocation')
    .Given('an option whose default value is a calculation that reads another option',
      procedure(W: TValuesWorld)
      begin
        W.BuildComputedFromAnother;
      end)
    .When('an invocation that names the other option and not this one is parsed and read',
      procedure(W: TValuesWorld)
      begin
        W.Parsed := W.FromAnother.Parse(['--who', 'ana']);
        W.ReadOut := W.FileOpt.ValueIn(W.Parsed);
      end)
    .&Then('the value read is the one the calculation made from the other value',
      procedure(W: TValuesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.ReadOut).ToEqual('ana.txt');
      end)

  .Scenario('A computed default value')
    .Given('an option whose default value is declared as a calculation',
      procedure(W: TValuesWorld)
      begin
        W.BuildComputed;
      end)
    .When('an invocation that does not name it is parsed and read',
      procedure(W: TValuesWorld)
      begin
        W.Parsed := W.Computed.Parse([]);
        W.ReadOut := W.OutOpt.ValueIn(W.Parsed);
      end)
    .&Then('the value read is the one the calculation produced',
      procedure(W: TValuesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.ReadOut).ToEqual('computed-place');
        Expect(OutRuns).ToEqual(1);
      end)

  .Scenario('The calculation does not run when the invocation brings the value')
    .Given('an option whose default value is declared as a calculation',
      procedure(W: TValuesWorld)
      begin
        W.BuildComputed;
      end)
    .When('an invocation that does name it is parsed and read',
      procedure(W: TValuesWorld)
      begin
        W.Parsed := W.Computed.Parse(['--out', 'given']);
        W.ReadOut := W.OutOpt.ValueIn(W.Parsed);
      end)
    .&Then('the value read is the invocation''s and the calculation never ran',
      procedure(W: TValuesWorld)
      begin
        Expect(W.ReadOut).ToEqual('given');
        Expect(OutRuns).ToEqual(0);
      end)

  .Scenario('The calculation runs at parse, even when nobody reads the value')
    .Given('an option whose default value is declared as a calculation',
      procedure(W: TValuesWorld)
      begin
        W.BuildComputed;
      end)
    .When('an invocation that does not name it is parsed, and nothing is read',
      procedure(W: TValuesWorld)
      begin
        W.Parsed := W.Computed.Parse([]);
      end)
    .&Then('the calculation has run once, because the default is part of the parse result',
      procedure(W: TValuesWorld)
      begin
        Expect(OutRuns).ToEqual(1);
      end)

  .Scenario('The calculation runs on every parse')
    .Given('an option whose default value is declared as a calculation',
      procedure(W: TValuesWorld)
      begin
        W.BuildComputed;
      end)
    .When('two invocations that do not name it are parsed',
      procedure(W: TValuesWorld)
      begin
        W.Computed.Parse([]);
        W.Parsed := W.Computed.Parse([]);
      end)
    .&Then('the calculation has run once for each, and nothing was kept from the first',
      procedure(W: TValuesWorld)
      begin
        Expect(OutRuns).ToEqual(2);
      end)

  .Scenario('The help shows the text declared with the calculation, and does not run it')
    .Given('an option with a computed default and a text declared beside it',
      procedure(W: TValuesWorld)
      begin
        W.BuildComputed;
      end)
    .When('the help is asked for',
      procedure(W: TValuesWorld)
      begin
        W.HelpText := W.Computed.HelpText;
        // A calculation may have effects, and the help has no way of knowing whether
        // this one does. So it shows the declared text, and nothing at all when there is
        // none: no default line beats an invented one.
        W.SeedDefault := 'not shown';
        if W.HelpText.Contains('[default: 7]') then
          W.SeedDefault := 'shown';
      end)
    .&Then('the declared text appears, the one with no text shows none, and nothing ran',
      procedure(W: TValuesWorld)
      begin
        Expect(W.HelpText).ToContain('[default: the current directory]');
        Expect(W.SeedDefault).ToEqual('not shown');
        Expect(OutRuns + SeedRuns).ToEqual(0);
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
