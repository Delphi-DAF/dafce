unit Validators.Feat;

interface

uses
  System.SysUtils,
  Daf.CommandLine;

type
  TValidatorsWorld = class
  public
    Root: IRootCommand;
    Sub: ICommand;
    JsonOpt: IOption<Boolean>;
    XmlOpt: IOption<Boolean>;
    CountOpt: IOption<Integer>;
    Parsed: IParseResult;
    /// <summary>A root with --json and --xml and a validation that refuses the two together.</summary>
    procedure BuildExclusive;
    /// <summary>A root with an Integer --count and a validation of the root that always fails.</summary>
    procedure BuildFailingRoot;
  end;

implementation

uses
  Daf.MiniSpec;

const
  BothFormats = '--json and --xml cannot be used together';
  RootRefuses = 'the root refuses every invocation';
  OptionRefuses = 'the option refuses every value';

// True when the invocation named the option: a result the parse created for a default does not count.
function Named(const Command: ICommandResult; const Option: IOption): Boolean;
var
  Found: IOptionResult;
begin
  Found := Command.GetResult(Option);
  Result := (Found <> nil) and not Found.IsImplicit;
end;

{ TValidatorsWorld }

procedure TValidatorsWorld.BuildExclusive;
begin
  JsonOpt := CommandLine.Option<Boolean>('--json');
  XmlOpt := CommandLine.Option<Boolean>('--xml');
  Root := CommandLine.Root('tool');
  Root.Add([JsonOpt, XmlOpt]);
  Root.Validators.Add(
    procedure(const Result: ICommandResult)
    begin
      if Named(Result, JsonOpt) and Named(Result, XmlOpt) then
        Result.AddError(BothFormats);
    end);
end;

procedure TValidatorsWorld.BuildFailingRoot;
begin
  CountOpt := CommandLine.Option<Integer>('--count');
  Sub := CommandLine.Command('build', 'Build things');
  Root := CommandLine.Root('tool');
  Root.Add([CountOpt, Sub]);
  Root.SetAction(
    function(const Parsed: IParseResult): Integer
    begin
      Result := TCommandLineExit.Ok;
    end);
  Root.Validators.Add(
    procedure(const Result: ICommandResult)
    begin
      Result.AddError(RootRefuses);
    end);
end;

initialization

Feature('''
Feature Validations over the result of a symbol @commandline @validators

  As a developer writing a console application
  I want to validate a command's invocation as a whole, and an option or an argument before its value
  So the rules no single symbol expresses are parse errors like any other
''')

.UseWorld<TValidatorsWorld>

.Rule('a command validates its whole invocation')

  .Scenario('Two options that exclude each other')
    .Given('a command with the options --json and --xml and a validation that refuses the two together',
      procedure(W: TValidatorsWorld)
      begin
        W.BuildExclusive;
      end)
    .When('an invocation that passes both is parsed',
      procedure(W: TValidatorsWorld)
      begin
        W.Parsed := W.Root.Parse(['--json', '--xml']);
      end)
    .&Then('the parse fails with the message of that validation',
      procedure(W: TValidatorsWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual(BothFormats);
      end)

  .Scenario('The error of the command cuts the validation of its parts')
    .Given('a command with a validation that always fails and an Integer option',
      procedure(W: TValidatorsWorld)
      begin
        W.BuildFailingRoot;
      end)
    .When('an invocation that gives the option a value that is not an integer is parsed',
      procedure(W: TValidatorsWorld)
      begin
        W.Parsed := W.Root.Parse(['--count', 'abc']);
      end)
    .&Then('the only error is the one of the validation of the command',
      procedure(W: TValidatorsWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual(RootRefuses);
      end)

  .Scenario('The validation of a parent does not run')
    .Given('a root with a validation that always fails, and a subcommand',
      procedure(W: TValidatorsWorld)
      begin
        W.BuildFailingRoot;
      end)
    .When('an invocation that names the subcommand is parsed',
      procedure(W: TValidatorsWorld)
      begin
        W.Parsed := W.Root.Parse(['build']);
      end)
    .&Then('the parse succeeds, because the root is not the innermost command',
      procedure(W: TValidatorsWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.Parsed.Command.Name).ToEqual('build');
      end)

  .Scenario('A validation of an option that fails keeps it from converting')
    .Given('an Integer option with a validation that always fails',
      procedure(W: TValidatorsWorld)
      begin
        W.CountOpt := CommandLine.Option<Integer>('--count');
        W.CountOpt.Validators.Add(
          procedure(const Result: IOptionResult)
          begin
            Result.AddError(OptionRefuses);
          end);
        W.Root := CommandLine.Root('tool');
        W.Root.Add(W.CountOpt);
      end)
    .When('an invocation that names it with a value that is not an integer is parsed',
      procedure(W: TValidatorsWorld)
      begin
        W.Parsed := W.Root.Parse(['--count', 'abc']);
      end)
    .&Then('the only error is the one of the validation, and not the one of the conversion',
      procedure(W: TValidatorsWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual(OptionRefuses);
      end);

end.
