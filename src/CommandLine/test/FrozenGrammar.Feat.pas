unit FrozenGrammar.Feat;

interface

uses
  System.SysUtils,
  Daf.CommandLine;

type
  TFrozenGrammarWorld = class
  public
    Root: IRootCommand;
    Sub: ICommand;
    NameOpt: IOption<string>;
    Report: string;
    /// <summary>What the action read from the parse result.</summary>
    Seen: string;
    /// <summary>
    ///   Starts at -1 and not at 0: "the invocation answered 0" is exactly what a field would
    ///   hold if the When never ran.
    /// </summary>
    ExitCode: Integer;
    constructor Create;
    /// <summary>A root with an option and a subcommand, frozen.</summary>
    procedure FreezeGrammar;
    /// <summary>Runs a change and answers what it raised, or a sentinel if it raised nothing.</summary>
    function Attempt(const Change: TProc): string;
  end;

implementation

uses
  Daf.MiniSpec;

const
  NoRaise = '<changed without complaint>';

constructor TFrozenGrammarWorld.Create;
begin
  inherited;
  ExitCode := -1;
end;

procedure TFrozenGrammarWorld.FreezeGrammar;
begin
  NameOpt := CommandLine.Option<string>('--name', 'Who to greet');
  Sub := CommandLine.Command('build', 'Build things');
  Root := CommandLine.Root('tool', 'A tool');
  Root.Add(NameOpt).Add(Sub);
  Root.Freeze;
end;

function TFrozenGrammarWorld.Attempt(const Change: TProc): string;
begin
  try
    Change();
    Result := NoRaise;
  except
    on E: Exception do
      Result := E.ClassName + ': ' + E.Message;
  end;
end;

initialization

Feature('''
Feature A frozen grammar takes no more changes, and still takes actions @commandline @freeze

  As the author of a tool whose composition reads the parse result
  I want the grammar to refuse changes once it is frozen
  So that what was parsed is never judged against a grammar that later changed
''')

.UseWorld<TFrozenGrammarWorld>

.Rule('Once frozen, the grammar cannot change')

  .Scenario('Adding an option to a frozen root')
    .Given('a frozen grammar',
      procedure(W: TFrozenGrammarWorld)
      begin
        W.FreezeGrammar;
      end)
    .When('an option is added to the root',
      procedure(W: TFrozenGrammarWorld)
      begin
        W.Report := W.Attempt(
          procedure
          begin
            W.Root.Add(CommandLine.Option<Boolean>('--loud'));
          end);
      end)
    .&Then('it fails with an ECommandLine that says the grammar is frozen',
      procedure(W: TFrozenGrammarWorld)
      begin
        Expect(W.Report).ToStartWith('ECommandLine: ');
        Expect(W.Report).ToContain('frozen');
      end)

  .Scenario('Changing an option declared before freezing')
    .Given('a frozen grammar',
      procedure(W: TFrozenGrammarWorld)
      begin
        W.FreezeGrammar;
      end)
    .When('one of its options gets a validation',
      procedure(W: TFrozenGrammarWorld)
      begin
        W.Report := W.Attempt(
          procedure
          begin
            W.NameOpt.Validate(
              function(const Value: string; out Error: string): Boolean
              begin
                Result := True;
              end);
          end);
      end)
    .&Then('it fails with an ECommandLine',
      procedure(W: TFrozenGrammarWorld)
      begin
        Expect(W.Report).ToStartWith('ECommandLine: ');
      end)

  .Scenario('Changing a subcommand of a frozen root')
    .Given('a frozen grammar',
      procedure(W: TFrozenGrammarWorld)
      begin
        W.FreezeGrammar;
      end)
    .When('one of its subcommands gets an option',
      procedure(W: TFrozenGrammarWorld)
      begin
        W.Report := W.Attempt(
          procedure
          begin
            W.Sub.Add(CommandLine.Option<Boolean>('--release'));
          end);
      end)
    .&Then('it fails with an ECommandLine',
      procedure(W: TFrozenGrammarWorld)
      begin
        Expect(W.Report).ToStartWith('ECommandLine: ');
      end)

  .Scenario('Declaring the version of a frozen root')
    .Given('a frozen grammar',
      procedure(W: TFrozenGrammarWorld)
      begin
        W.FreezeGrammar;
      end)
    .When('the root declares its version',
      procedure(W: TFrozenGrammarWorld)
      begin
        W.Report := W.Attempt(
          procedure
          begin
            W.Root.WithVersion('1.0.0');
          end);
      end)
    .&Then('it fails with an ECommandLine',
      procedure(W: TFrozenGrammarWorld)
      begin
        Expect(W.Report).ToStartWith('ECommandLine: ');
      end)

.Rule('A frozen grammar still takes actions, and parses as before')

  .Scenario('Setting the actions after freezing')
    .Given('a frozen grammar',
      procedure(W: TFrozenGrammarWorld)
      begin
        W.FreezeGrammar;
      end)
    .When('the root and the help get an action, and an invocation that names the option runs',
      procedure(W: TFrozenGrammarWorld)
      begin
        W.Report := W.Attempt(
          procedure
          begin
            var NameOpt := W.NameOpt;
            W.Root.SetAction(
              function(const Parsed: ICommandLineParseResult): Integer
              begin
                W.Seen := NameOpt.ValueIn(Parsed);
                Result := 7;
              end);
            W.Root.HelpAction :=
              function(const Parsed: ICommandLineParseResult): Integer
              begin
                Result := 0;
              end;
            W.ExitCode := W.Root.Parse(['--name', 'Jorge']).Invoke;
          end);
      end)
    .&Then('nothing fails, and the action reads what was parsed',
      procedure(W: TFrozenGrammarWorld)
      begin
        Expect(Format('%s seen=%s exit=%d', [W.Report, W.Seen, W.ExitCode]))
          .ToEqual(NoRaise + ' seen=Jorge exit=7');
      end)

end.
