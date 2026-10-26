unit NameCase.Feat;

interface

uses
  Daf.CommandLine;

type
  TNameCaseWorld = class
  public
    Root: IRootCommand;
    LowerOpt: IOption<Boolean>;
    UpperOpt: IOption<Boolean>;
    Build: ICommand;
    Configuration: TParserConfiguration;
    Parsed: IParseResult;
    Report: string;
    procedure BuildGrammar(DeclareUpper: Boolean);
  end;

implementation

uses
  System.SysUtils,
  Daf.MiniSpec;

{ TNameCaseWorld }

procedure TNameCaseWorld.BuildGrammar(DeclareUpper: Boolean);
begin
  LowerOpt := CommandLine.Option<Boolean>('-v').Description('Say more');
  Build := CommandLine.Command('build', 'Build things');
  Root := CommandLine.Root('mytool', 'A tool');
  Root.Add([LowerOpt, Build]);
  // An action of its own, so the root is an invocation without naming build.
  Root.SetAction(
    procedure(const Parsed: IParseResult)
    begin
    end);
  if DeclareUpper then
  begin
    UpperOpt := CommandLine.Option<Boolean>('-V').Description('Say even more');
    Root.Add(UpperOpt);
  end;
end;

initialization

Feature('''
Feature Whether names tell cases apart @commandline @case

  As the author of a tool
  I want to decide whether -v and -V are one option or two
  So the grammar follows the convention my users already have
''')

.UseWorld<TNameCaseWorld>

.Rule('names tell cases apart unless the application asks otherwise')

  .Scenario('By default they are told apart')
    .Given('a command with the option -v',
      procedure(W: TNameCaseWorld)
      begin
        W.BuildGrammar(False);
      end)
    .When('an invocation passing -V is parsed',
      procedure(W: TNameCaseWorld)
      begin
        W.Parsed := W.Root.Parse(['-V']);
      end)
    .&Then('the parse fails, because -V is no declared name',
      procedure(W: TNameCaseWorld)
      begin
        Expect(W.Parsed.Succeeded).ToBeFalse;
        Expect(W.Parsed.ErrorText).ToContain('''-V''');
        Expect(W.Parsed.WasProvided(W.LowerOpt)).ToBeFalse;
      end)

  .Scenario('The application asks not to tell them apart')
    .Given('a command with the option -v, and the parser configured not to tell cases apart',
      procedure(W: TNameCaseWorld)
      begin
        W.BuildGrammar(False);
        W.Configuration.EnableCaseInsensitiveNames := True;
      end)
    .When('an invocation passing -V is parsed',
      procedure(W: TNameCaseWorld)
      begin
        W.Parsed := W.Root.Parse(['-V'], W.Configuration);
      end)
    .&Then('the option -v is reached',
      procedure(W: TNameCaseWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.Parsed.WasProvided(W.LowerOpt)).ToBeTrue;
      end)
    .&And('the result carries the configuration it was read with',
      procedure(W: TNameCaseWorld)
      begin
        Expect(W.Parsed.Configuration.EnableCaseInsensitiveNames).ToBeTrue;
      end)

  .Scenario('A command written with another case')
    .Given('a command with a subcommand build, and the parser configured not to tell cases apart',
      procedure(W: TNameCaseWorld)
      begin
        W.BuildGrammar(False);
        W.Configuration.EnableCaseInsensitiveNames := True;
      end)
    .When('an invocation passing BUILD is parsed',
      procedure(W: TNameCaseWorld)
      begin
        W.Parsed := W.Root.Parse(['BUILD'], W.Configuration);
      end)
    .&Then('build is reached',
      procedure(W: TNameCaseWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.Parsed.Command.Name).ToEqual('build');
      end)

  .Scenario('Two names that differ only in case')
    .Given('a command with the options -v and -V, and the parser configured not to tell cases apart',
      procedure(W: TNameCaseWorld)
      begin
        W.BuildGrammar(True);
        W.Configuration.EnableCaseInsensitiveNames := True;
      end)
    .When('any invocation is parsed',
      procedure(W: TNameCaseWorld)
      begin
        W.Report := '<no exception>';
        try
          W.Parsed := W.Root.Parse(['build'], W.Configuration);
        except
          on E: ECommandLine do
            W.Report := E.Message;
        end;
      end)
    .&Then('the grammar is refused, naming the two',
      procedure(W: TNameCaseWorld)
      begin
        Expect(W.Report).ToContain('"-v"');
        Expect(W.Report).ToContain('"-V"');
      end)

  .Scenario('Two names that differ only in case, told apart')
    .Given('a command with the options -v and -V, and the parser left to tell cases apart',
      procedure(W: TNameCaseWorld)
      begin
        W.BuildGrammar(True);
      end)
    .When('an invocation passing -V is parsed',
      procedure(W: TNameCaseWorld)
      begin
        W.Parsed := W.Root.Parse(['-V']);
      end)
    .&Then('-V is reached, and -v is not',
      procedure(W: TNameCaseWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.Parsed.WasProvided(W.UpperOpt)).ToBeTrue;
        Expect(W.Parsed.WasProvided(W.LowerOpt)).ToBeFalse;
      end);

end.
