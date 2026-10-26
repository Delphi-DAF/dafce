unit Actions.Feat;

interface

uses
  System.SysUtils,
  System.Classes,
  Daf.CommandLine;

type
  TActionsWorld = class;

  /// <summary>An action that leaves its name in the world and answers a code of its own.</summary>
  TTrackedAction = class(TSynchronousCommandLineAction)
  private
    // An object reference and not an interface: the world owns the grammar that holds this action.
    FWorld: TActionsWorld;
    FName: string;
    FCode: Integer;
    FTerminating: Boolean;
  public
    constructor Create(World: TActionsWorld; const Name: string; Code: Integer; Terminating: Boolean);
    function IsTerminating: Boolean; override;
    function Invoke(const Parsed: IParseResult): Integer; override;
  end;

  TActionsWorld = class
  public
    Root: IRootCommand;
    TraceOpt: IOption<Boolean>;
    LevelOpt: IOption<Integer>;
    CountOpt: IOption<Integer>;
    VerboseOpt: IOption<Boolean>;
    VerbOpt: IOption<Boolean>;
    SecretOpt: IOption<Boolean>;
    Parsed: IParseResult;
    Output: TStringWriter;
    Error: TStringWriter;
    /// <summary>The actions that ran, in order.</summary>
    Ran: string;
    /// <summary>
    ///   Starts at -1 and not at 0: "the invocation answered 0" is exactly what a field would
    ///   hold if the When never ran.
    /// </summary>
    ExitCode: Integer;
    constructor Create;
    destructor Destroy; override;
    function Configuration: TInvocationConfiguration;
    /// <summary>The output and the error in one writer, so the order between them can be read.</summary>
    function OneWriter: TInvocationConfiguration;
    function RanText: string;
    procedure Track(const Name: string);
    /// <summary>A root with an action that answers <c>RootCode</c>, and a <c>--trace</c> option.</summary>
    procedure BuildGrammar(RootCode: Integer);
    /// <summary>The same, adding <c>--level</c> with the default 3 and <c>--count</c> with none, both integers.</summary>
    procedure BuildDefaultedGrammar;
    /// <summary>A root with an action and <c>--verbose</c>, <c>--verb</c> and a hidden <c>--secret</c>.</summary>
    procedure BuildTypoGrammar;
  end;

implementation

uses
  Daf.MiniSpec;

{ TTrackedAction }

constructor TTrackedAction.Create(World: TActionsWorld; const Name: string; Code: Integer; Terminating: Boolean);
begin
  inherited Create;
  FWorld := World;
  FName := Name;
  FCode := Code;
  FTerminating := Terminating;
end;

function TTrackedAction.IsTerminating: Boolean;
begin
  Result := FTerminating;
end;

function TTrackedAction.Invoke(const Parsed: IParseResult): Integer;
begin
  FWorld.Track(FName);
  Result := FCode;
end;

{ TActionsWorld }

constructor TActionsWorld.Create;
begin
  inherited;
  Output := TStringWriter.Create;
  Error := TStringWriter.Create;
  ExitCode := -1;
end;

destructor TActionsWorld.Destroy;
begin
  Output.Free;
  Error.Free;
  inherited;
end;

function TActionsWorld.Configuration: TInvocationConfiguration;
begin
  Result.Output := Output;
  Result.Error := Error;
end;

function TActionsWorld.OneWriter: TInvocationConfiguration;
begin
  Result.Output := Output;
  Result.Error := Output;
end;

function TActionsWorld.RanText: string;
begin
  if Ran = '' then
    Result := '<nothing>'
  else
    Result := Ran;
end;

procedure TActionsWorld.Track(const Name: string);
begin
  if Ran <> '' then
    Ran := Ran + ',';
  Ran := Ran + Name;
end;

procedure TActionsWorld.BuildGrammar(RootCode: Integer);
begin
  TraceOpt := CommandLine.Option<Boolean>('--trace');
  Root := CommandLine.Root('tool');
  Root.Add(TraceOpt);
  Root.SetAction(
    function(const Parsed: IParseResult): Integer
    begin
      Track('command');
      Result := RootCode;
    end);
end;

procedure TActionsWorld.BuildDefaultedGrammar;
begin
  BuildGrammar(5);
  LevelOpt := CommandLine.Option<Integer>('--level').DefaultValue(3);
  CountOpt := CommandLine.Option<Integer>('--count');
  Root.Add([LevelOpt, CountOpt]);
end;

procedure TActionsWorld.BuildTypoGrammar;
begin
  VerboseOpt := CommandLine.Option<Boolean>('--verbose');
  VerbOpt := CommandLine.Option<Boolean>('--verb');
  SecretOpt := CommandLine.Option<Boolean>('--secret').Hidden;
  Root := CommandLine.Root('tool');
  Root.Add([VerboseOpt, VerbOpt, SecretOpt]);
  Root.SetAction(
    function(const Parsed: IParseResult): Integer
    begin
      Track('command');
      Result := 5;
    end);
end;

initialization

Feature('''
Feature Actions of options @commandline @actions

  As a developer writing a console application
  I want an option to carry an action of its own
  So something can run instead of the command, or before it, just by naming the option
''')

.UseWorld<TActionsWorld>

.Rule('an option can carry an action, terminating or not')

  .Scenario('An option with a terminating action')
    .Given('a command with an action and an option with a terminating action',
      procedure(W: TActionsWorld)
      begin
        W.BuildGrammar(5);
        W.TraceOpt.Action(TTrackedAction.Create(W, 'option', 7, True));
      end)
    .When('the result of parsing an invocation that names the command and the option is invoked',
      procedure(W: TActionsWorld)
      begin
        W.Parsed := W.Root.Parse(['--trace']);
        W.ExitCode := W.Parsed.Invoke(W.Configuration);
      end)
    .&Then('the action of the option runs, and the one of the command does not',
      procedure(W: TActionsWorld)
      begin
        Expect(W.RanText).ToEqual('option');
      end)
    .&And('the invocation answers what the action of the option answered',
      procedure(W: TActionsWorld)
      begin
        Expect(W.ExitCode).ToEqual(7);
      end)

  .Scenario('An option with an action that is not terminating')
    .Given('a command with an action and an option with an action that is not terminating',
      procedure(W: TActionsWorld)
      begin
        W.BuildGrammar(5);
        W.TraceOpt.Action(TTrackedAction.Create(W, 'option', 0, False));
      end)
    .When('the result of parsing an invocation that names the command and the option is invoked',
      procedure(W: TActionsWorld)
      begin
        W.Parsed := W.Root.Parse(['--trace']);
        W.ExitCode := W.Parsed.Invoke(W.Configuration);
      end)
    .&Then('the action of the option runs, and after it the one of the command',
      procedure(W: TActionsWorld)
      begin
        Expect(Format('ran=%s exit=%d', [W.RanText, W.ExitCode])).ToEqual('ran=option,command exit=5');
      end)

  .Scenario('The code of an action that is not terminating wins')
    .Given('a command whose action answers 0, and an option with an action that is not terminating and answers 3',
      procedure(W: TActionsWorld)
      begin
        W.BuildGrammar(0);
        W.TraceOpt.Action(TTrackedAction.Create(W, 'option', 3, False));
      end)
    .When('the result of parsing an invocation that names the command and the option is invoked',
      procedure(W: TActionsWorld)
      begin
        W.Parsed := W.Root.Parse(['--trace']);
        W.ExitCode := W.Parsed.Invoke(W.Configuration);
      end)
    .&Then('both actions run',
      procedure(W: TActionsWorld)
      begin
        Expect(W.RanText).ToEqual('option,command');
      end)
    .&And('the invocation answers 3',
      procedure(W: TActionsWorld)
      begin
        Expect(W.ExitCode).ToEqual(3);
      end)

.Rule('the action that is not terminating of an option that takes its default runs too')

  .Scenario('An option with a default value that is not named')
    .Given('a command with an action, and an integer option with a default value and an action that is not terminating',
      procedure(W: TActionsWorld)
      begin
        W.BuildDefaultedGrammar;
        W.LevelOpt.Action(TTrackedAction.Create(W, 'option', 0, False));
      end)
    .When('the result of parsing an invocation that names the command and not the option is invoked',
      procedure(W: TActionsWorld)
      begin
        W.Parsed := W.Root.Parse([]);
        W.ExitCode := W.Parsed.Invoke(W.Configuration);
      end)
    .&Then('the action of the option runs, and after it the one of the command',
      procedure(W: TActionsWorld)
      begin
        Expect(Format('ran=%s exit=%d', [W.RanText, W.ExitCode])).ToEqual('ran=option,command exit=5');
      end)

  .Scenario('A boolean option that is not named')
    .Given('a command with an action, and a boolean option with no declared default and an action that is not terminating',
      procedure(W: TActionsWorld)
      begin
        W.BuildDefaultedGrammar;
        W.TraceOpt.Action(TTrackedAction.Create(W, 'option', 0, False));
      end)
    .When('the result of parsing an invocation that names the command and not the option is invoked',
      procedure(W: TActionsWorld)
      begin
        W.Parsed := W.Root.Parse([]);
        W.ExitCode := W.Parsed.Invoke(W.Configuration);
      end)
    .&Then('the action of the option runs, and after it the one of the command',
      procedure(W: TActionsWorld)
      begin
        Expect(Format('ran=%s exit=%d', [W.RanText, W.ExitCode])).ToEqual('ran=option,command exit=5');
      end)

  .Scenario('An option with no default value that is not named')
    .Given('a command with an action, and an integer option with no default value and an action that is not terminating',
      procedure(W: TActionsWorld)
      begin
        W.BuildDefaultedGrammar;
        W.CountOpt.Action(TTrackedAction.Create(W, 'option', 0, False));
      end)
    .When('the result of parsing an invocation that names the command and not the option is invoked',
      procedure(W: TActionsWorld)
      begin
        W.Parsed := W.Root.Parse([]);
        W.ExitCode := W.Parsed.Invoke(W.Configuration);
      end)
    .&Then('only the action of the command runs',
      procedure(W: TActionsWorld)
      begin
        Expect(Format('ran=%s exit=%d', [W.RanText, W.ExitCode])).ToEqual('ran=command exit=5');
      end)

.Rule('a parse error suggests what was meant')

  .Scenario('A typo of one letter')
    .Given('a command with the option --verbose',
      procedure(W: TActionsWorld)
      begin
        W.BuildTypoGrammar;
      end)
    .When('the result of parsing an invocation that passes --verbse is invoked',
      procedure(W: TActionsWorld)
      begin
        W.Parsed := W.Root.Parse(['--verbse']);
        W.ExitCode := W.Parsed.Invoke(W.OneWriter);
      end)
    .&Then('--verbose is suggested, then the error is written, then the help',
      procedure(W: TActionsWorld)
      begin
        Expect(W.Output.ToString).ToEqual(
          '''--verbse'' was not matched. Did you mean one of the following?' + sLineBreak +
          '--verbose' + sLineBreak +
          sLineBreak +
          'Unrecognized command or argument ''--verbse''.' + sLineBreak +
          sLineBreak +
          W.Root.HelpText + sLineBreak);
      end)
    .&And('the action does not run, and the invocation answers 2',
      procedure(W: TActionsWorld)
      begin
        Expect(Format('ran=%s exit=%d', [W.RanText, W.ExitCode])).ToEqual('ran=<nothing> exit=2');
      end)

  .Scenario('Nothing is close enough')
    .Given('a command with the option --verbose',
      procedure(W: TActionsWorld)
      begin
        W.BuildTypoGrammar;
      end)
    .When('the result of parsing an invocation that passes a token more than 3 edits from every declared name is invoked',
      procedure(W: TActionsWorld)
      begin
        W.Parsed := W.Root.Parse(['--zzzzzz']);
        W.ExitCode := W.Parsed.Invoke(W.Configuration);
      end)
    .&Then('nothing is suggested, and the errors are written all the same',
      procedure(W: TActionsWorld)
      begin
        Expect(Pos('was not matched', W.Output.ToString)).ToEqual(0);
        Expect(W.Error.ToString).ToEqual('Unrecognized command or argument ''--zzzzzz''.' + sLineBreak + sLineBreak);
      end)

  .Scenario('Only the closest')
    .Given('a command with two options, one at one edit from the token and the other at two',
      procedure(W: TActionsWorld)
      begin
        W.BuildTypoGrammar;
      end)
    .When('the result of parsing an invocation that passes that token is invoked',
      procedure(W: TActionsWorld)
      begin
        // --verbose is one edit away, --verb two.
        W.Parsed := W.Root.Parse(['--verbse']);
        W.ExitCode := W.Parsed.Invoke(W.Configuration);
      end)
    .&Then('only the one at one edit is suggested',
      procedure(W: TActionsWorld)
      begin
        Expect(W.Output.ToString).ToStartWith(
          '''--verbse'' was not matched. Did you mean one of the following?' + sLineBreak +
          '--verbose' + sLineBreak +
          sLineBreak);
      end)

  .Scenario('A hidden symbol is not suggested')
    .Given('a command with a hidden option at one edit from the token',
      procedure(W: TActionsWorld)
      begin
        W.BuildTypoGrammar;
      end)
    .When('the result of parsing an invocation that passes that token is invoked',
      procedure(W: TActionsWorld)
      begin
        // One edit from --secret, and more than 3 from every name that is not hidden.
        W.Parsed := W.Root.Parse(['--secrets']);
        W.ExitCode := W.Parsed.Invoke(W.Configuration);
      end)
    .&Then('the hidden option is not suggested',
      procedure(W: TActionsWorld)
      begin
        Expect(Pos('--secret', W.Output.ToString)).ToEqual(0);
        Expect(Pos('was not matched', W.Output.ToString)).ToEqual(0);
      end);

end.
