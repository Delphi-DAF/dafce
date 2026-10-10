unit Invocation.Feat;

interface

uses
  System.SysUtils,
  System.Classes,
  Daf.CommandLine;

type
  EActionFailed = class(Exception);

  TInvocationWorld = class
  public
    Root: IRootCommand;
    First: ICommand;
    Second: ICommand;
    CountOpt: IOption<Integer>;
    QuietOpt: IOption<Boolean>;
    Parsed: IParseResult;
    Output: TStringWriter;
    Error: TStringWriter;
    /// <summary>The actions that ran, in order.</summary>
    Ran: string;
    ReadCount: Integer;
    /// <summary>
    ///   Starts at -1 and not at 0: "the invocation answered 0" is exactly what a field would
    ///   hold if the When never ran.
    /// </summary>
    ExitCode: Integer;
    Raised: string;
    constructor Create;
    destructor Destroy; override;
    function Configuration: TInvocationConfiguration;
    function RanText: string;
    procedure Track(const Name: string);
  end;

implementation

uses
  Daf.MiniSpec;

{ TInvocationWorld }

constructor TInvocationWorld.Create;
begin
  inherited;
  Output := TStringWriter.Create;
  Error := TStringWriter.Create;
  ExitCode := -1;
end;

destructor TInvocationWorld.Destroy;
begin
  Output.Free;
  Error.Free;
  inherited;
end;

function TInvocationWorld.Configuration: TInvocationConfiguration;
begin
  Result.Output := Output;
  Result.Error := Error;
end;

function TInvocationWorld.RanText: string;
begin
  if Ran = '' then
    Result := '<nothing>'
  else
    Result := Ran;
end;

procedure TInvocationWorld.Track(const Name: string);
begin
  if Ran <> '' then
    Ran := Ran + ',';
  Ran := Ran + Name;
end;

initialization

Feature('''
Feature Invoking a parse result @commandline @invocation

  As a developer writing a console application with several commands
  I want each command to declare its action and the parse result to run it
  So I do not have to look at the command reached and dispatch by hand
''')

.UseWorld<TInvocationWorld>

.Rule('invoking a parse result runs the action of the command reached')

  .Scenario('Only the action of the named subcommand runs')
    .Given('a root command with two subcommands, each with its action',
      procedure(W: TInvocationWorld)
      begin
        W.First := CommandLine.Command('first').SetAction(
          function(const Parsed: IParseResult): Integer
          begin
            W.Track('first');
            Result := 11;
          end);
        W.Second := CommandLine.Command('second').SetAction(
          function(const Parsed: IParseResult): Integer
          begin
            W.Track('second');
            Result := 22;
          end);
        W.Root := CommandLine.Root('tool');
        W.Root.Add([W.First, W.Second]);
      end)
    .When('the result of parsing an invocation that names one of them is invoked',
      procedure(W: TInvocationWorld)
      begin
        W.Parsed := W.Root.Parse(['second']);
        W.ExitCode := W.Parsed.Invoke;
      end)
    .&Then('only that action runs, and the invocation answers its code',
      procedure(W: TInvocationWorld)
      begin
        Expect(Format('ran=%s exit=%d', [W.RanText, W.ExitCode])).ToEqual('ran=second exit=22');
      end)

  .Scenario('The action reads the values of the invocation')
    .Given('a command with an option and an action that reads its value',
      procedure(W: TInvocationWorld)
      begin
        W.CountOpt := CommandLine.Option<Integer>('--count');
        W.Root := CommandLine.Root('tool');
        W.Root.Add(W.CountOpt);
        W.Root.SetAction(
          function(const Parsed: IParseResult): Integer
          begin
            W.ReadCount := W.CountOpt.ValueIn(Parsed);
            Result := TCommandLineExit.Ok;
          end);
      end)
    .When('the result of parsing an invocation that passes the option is invoked',
      procedure(W: TInvocationWorld)
      begin
        W.Parsed := W.Root.Parse(['--count', '5']);
        W.ExitCode := W.Parsed.Invoke;
      end)
    .&Then('the action receives the value that was passed',
      procedure(W: TInvocationWorld)
      begin
        Expect(Format('count=%d exit=%d', [W.ReadCount, W.ExitCode])).ToEqual('count=5 exit=0');
      end)

  .Scenario('An action with no code of its own')
    .Given('a command whose action is a procedure',
      procedure(W: TInvocationWorld)
      begin
        W.Root := CommandLine.Root('tool');
        W.Root.SetAction(
          procedure(const Parsed: IParseResult)
          begin
            W.Track('root');
          end);
      end)
    .When('the result of parsing an invocation that names it is invoked',
      procedure(W: TInvocationWorld)
      begin
        W.Parsed := W.Root.Parse([]);
        W.ExitCode := W.Parsed.Invoke;
      end)
    .&Then('the action runs, and the invocation answers 0',
      procedure(W: TInvocationWorld)
      begin
        Expect(Format('ran=%s exit=%d', [W.RanText, W.ExitCode])).ToEqual('ran=root exit=0');
      end)

  .Scenario('A command without an action')
    .Given('a leaf command that declares no action, next to one that does',
      procedure(W: TInvocationWorld)
      begin
        W.First := CommandLine.Command('first');
        W.Second := CommandLine.Command('second').SetAction(
          function(const Parsed: IParseResult): Integer
          begin
            W.Track('second');
            Result := 22;
          end);
        W.Root := CommandLine.Root('tool');
        W.Root.Add([W.First, W.Second]);
      end)
    .When('the result of parsing an invocation that names it is invoked',
      procedure(W: TInvocationWorld)
      begin
        W.Parsed := W.Root.Parse(['first']);
        W.ExitCode := W.Parsed.Invoke;
      end)
    .&Then('the invocation answers 0 without running anything',
      procedure(W: TInvocationWorld)
      begin
        Expect(Format('ran=%s exit=%d', [W.RanText, W.ExitCode])).ToEqual('ran=<nothing> exit=0');
      end)

  .Scenario('Declaring the action again replaces it')
    .Given('a command given one action and then another',
      procedure(W: TInvocationWorld)
      begin
        W.Root := CommandLine.Root('tool');
        W.Root.SetAction(
          function(const Parsed: IParseResult): Integer
          begin
            W.Track('one');
            Result := 1;
          end);
        W.Root.SetAction(
          function(const Parsed: IParseResult): Integer
          begin
            W.Track('two');
            Result := 2;
          end);
      end)
    .When('the result of parsing an invocation that names it is invoked',
      procedure(W: TInvocationWorld)
      begin
        W.Parsed := W.Root.Parse([]);
        W.ExitCode := W.Parsed.Invoke;
      end)
    .&Then('the second one runs',
      procedure(W: TInvocationWorld)
      begin
        Expect(Format('ran=%s exit=%d', [W.RanText, W.ExitCode])).ToEqual('ran=two exit=2');
      end)

.Rule('help, version and parse errors are settled by the invocation')

  .Scenario('Asking for help does not run the action')
    .Given('a command with an action',
      procedure(W: TInvocationWorld)
      begin
        W.Root := CommandLine.Root('tool', 'Does things');
        W.Root.SetAction(
          function(const Parsed: IParseResult): Integer
          begin
            W.Track('root');
            Result := 5;
          end);
      end)
    .When('the result of parsing an invocation that passes --help is invoked',
      procedure(W: TInvocationWorld)
      begin
        W.Parsed := W.Root.Parse(['--help']);
        W.ExitCode := W.Parsed.Invoke(W.Configuration);
      end)
    .&Then('the help of the command is written to the output, and the action does not run',
      procedure(W: TInvocationWorld)
      begin
        Expect(W.Output.ToString).ToEqual(W.Root.HelpText + sLineBreak);
        Expect(W.RanText).ToEqual('<nothing>');
      end)
    .&And('the invocation answers 0',
      procedure(W: TInvocationWorld)
      begin
        Expect(W.ExitCode).ToEqual(TCommandLineExit.Ok);
      end)

  .Scenario('Asking for the version does not run the action')
    .Given('a root command with a declared version and an action',
      procedure(W: TInvocationWorld)
      begin
        W.Root := CommandLine.Root('tool').WithVersion('2.0.0');
        W.Root.SetAction(
          function(const Parsed: IParseResult): Integer
          begin
            W.Track('root');
            Result := 5;
          end);
      end)
    .When('the result of parsing an invocation that passes --version is invoked',
      procedure(W: TInvocationWorld)
      begin
        W.Parsed := W.Root.Parse(['--version']);
        W.ExitCode := W.Parsed.Invoke(W.Configuration);
      end)
    .&Then('the version is written to the output, and the action does not run',
      procedure(W: TInvocationWorld)
      begin
        Expect(W.Output.ToString).ToEqual('2.0.0' + sLineBreak);
        Expect(W.RanText).ToEqual('<nothing>');
      end)
    .&And('the invocation answers 0',
      procedure(W: TInvocationWorld)
      begin
        Expect(W.ExitCode).ToEqual(TCommandLineExit.Ok);
      end)

  .Scenario('An invocation that does not fit')
    .Given('a command with an action and an integer option',
      procedure(W: TInvocationWorld)
      begin
        W.CountOpt := CommandLine.Option<Integer>('--count');
        W.Root := CommandLine.Root('tool');
        W.Root.Add(W.CountOpt);
        W.Root.SetAction(
          function(const Parsed: IParseResult): Integer
          begin
            W.Track('root');
            Result := 5;
          end);
      end)
    .When('the result of parsing an invocation that passes it something that is not an integer is invoked',
      procedure(W: TInvocationWorld)
      begin
        W.Parsed := W.Root.Parse(['--count', 'abc']);
        W.ExitCode := W.Parsed.Invoke(W.Configuration);
      end)
    .&Then('the error is written to the error, the help of the command to the output, and the action does not run',
      procedure(W: TInvocationWorld)
      begin
        Expect(W.Error.ToString).ToEqual('"abc" is not a valid integer for --count' + sLineBreak + sLineBreak);
        Expect(W.Output.ToString).ToEqual(W.Root.HelpText + sLineBreak);
        Expect(W.RanText).ToEqual('<nothing>');
      end)
    .&And('the invocation answers 2',
      procedure(W: TInvocationWorld)
      begin
        Expect(W.ExitCode).ToEqual(2);
      end)

  .Scenario('Asking for the help clears the errors of the rest')
    .Given('a command with an integer option',
      procedure(W: TInvocationWorld)
      begin
        W.CountOpt := CommandLine.Option<Integer>('--count');
        W.Root := CommandLine.Root('tool', 'Does things');
        W.Root.Add(W.CountOpt);
        W.Root.SetAction(
          function(const Parsed: IParseResult): Integer
          begin
            W.Track('root');
            Result := 5;
          end);
      end)
    .When('the result of parsing an invocation that passes it something that is not an integer, and --help, is invoked',
      procedure(W: TInvocationWorld)
      begin
        W.Parsed := W.Root.Parse(['--count', 'abc', '--help']);
        W.ExitCode := W.Parsed.Invoke(W.Configuration);
      end)
    .&Then('the help is written to the output, and no error is written',
      procedure(W: TInvocationWorld)
      begin
        Expect(W.Output.ToString).ToEqual(W.Root.HelpText + sLineBreak);
        Expect(W.Error.ToString).ToEqual('');
        Expect(W.RanText).ToEqual('<nothing>');
      end)
    .&And('the invocation answers 0',
      procedure(W: TInvocationWorld)
      begin
        Expect(W.ExitCode).ToEqual(TCommandLineExit.Ok);
      end)

  .Scenario('The version together with another argument is written all the same')
    .Given('a root command with a declared version, and an option',
      procedure(W: TInvocationWorld)
      begin
        W.QuietOpt := CommandLine.Option<Boolean>('--quiet');
        W.Root := CommandLine.Root('tool').WithVersion('2.0.0');
        W.Root.Add(W.QuietOpt);
        W.Root.SetAction(
          function(const Parsed: IParseResult): Integer
          begin
            W.Track('root');
            Result := 5;
          end);
      end)
    .When('the result of parsing an invocation that passes --version and that option is invoked',
      procedure(W: TInvocationWorld)
      begin
        W.Parsed := W.Root.Parse(['--version', '--quiet']);
        W.ExitCode := W.Parsed.Invoke(W.Configuration);
      end)
    .&Then('the version is written to the output, and the result carries the error that says it does not combine',
      procedure(W: TInvocationWorld)
      begin
        Expect(W.Output.ToString).ToEqual('2.0.0' + sLineBreak);
        Expect(W.Error.ToString).ToEqual('');
        Expect(W.Parsed.ErrorText).ToEqual('--version option cannot be combined with other arguments.');
        Expect(W.RanText).ToEqual('<nothing>');
      end)
    .&And('the invocation answers 0',
      procedure(W: TInvocationWorld)
      begin
        Expect(W.ExitCode).ToEqual(TCommandLineExit.Ok);
      end)

.Rule('the invocation takes where to write and what to do with what escapes')

  .Scenario('The output goes where it is told')
    .Given('a command, and an invocation configuration that gives its own output',
      procedure(W: TInvocationWorld)
      begin
        W.First := CommandLine.Command('first', 'The first one');
        W.Root := CommandLine.Root('tool');
        W.Root.Add(W.First);
      end)
    .When('the result of parsing an invocation that asks for help is invoked',
      procedure(W: TInvocationWorld)
      begin
        W.Parsed := W.Root.Parse(['first', '--help']);
        W.ExitCode := W.Parsed.Invoke(W.Configuration);
      end)
    .&Then('the help is written to that output',
      procedure(W: TInvocationWorld)
      begin
        Expect(W.Output.ToString).ToEqual(W.First.HelpText + sLineBreak);
        Expect(W.Error.ToString).ToEqual('');
      end)

  .Scenario('An exception with the default handler')
    .Given('a command whose action raises an exception',
      procedure(W: TInvocationWorld)
      begin
        W.Root := CommandLine.Root('tool');
        W.Root.SetAction(
          function(const Parsed: IParseResult): Integer
          begin
            raise EActionFailed.Create('the work failed');
          end);
      end)
    .When('the result of parsing it is invoked with a configuration that only says where to write',
      procedure(W: TInvocationWorld)
      begin
        W.Parsed := W.Root.Parse([]);
        W.ExitCode := W.Parsed.Invoke(W.Configuration);
      end)
    .&Then('the exception is written to the error as unhandled, with its class and its message',
      procedure(W: TInvocationWorld)
      begin
        // No provider of stack traces is installed here, so nothing follows the message.
        Expect(W.Error.ToString).ToEqual('Unhandled exception: EActionFailed: the work failed' + sLineBreak);
      end)
    .&And('the invocation answers 1',
      procedure(W: TInvocationWorld)
      begin
        Expect(W.ExitCode).ToEqual(TCommandLineExit.Failure);
      end)

  .Scenario('Without the handler, the exception leaves the invocation')
    .Given('a command whose action raises an exception',
      procedure(W: TInvocationWorld)
      begin
        W.Root := CommandLine.Root('tool');
        W.Root.SetAction(
          function(const Parsed: IParseResult): Integer
          begin
            raise EActionFailed.Create('the work failed');
          end);
      end)
    .When('the result of parsing it is invoked with the default handler turned off',
      procedure(W: TInvocationWorld)
      begin
        W.Parsed := W.Root.Parse([]);
        var Configuration := W.Configuration;
        Configuration.EnableDefaultExceptionHandler := False;
        try
          W.ExitCode := W.Parsed.Invoke(Configuration);
          W.Raised := '<nothing raised>';
        except
          on E: Exception do
            W.Raised := E.ClassName + ': ' + E.Message;
        end;
      end)
    .&Then('the invocation raises that same exception, and writes nothing',
      procedure(W: TInvocationWorld)
      begin
        Expect(W.Raised).ToEqual('EActionFailed: the work failed');
        Expect(W.Error.ToString).ToEqual('');
      end);

end.
