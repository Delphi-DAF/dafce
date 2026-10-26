unit Directives.Feat;

interface

uses
  System.SysUtils,
  System.Classes,
  Daf.CommandLine;

type
  TDirectivesWorld = class;

  /// <summary>An action that is not terminating and leaves its name in the world.</summary>
  TRecordingAction = class(TSynchronousCommandLineAction)
  private
    // An object reference and not an interface: the world owns the grammar that holds this action.
    FWorld: TDirectivesWorld;
    FName: string;
  public
    constructor Create(World: TDirectivesWorld; const Name: string);
    function IsTerminating: Boolean; override;
    function Invoke(const Parsed: IParseResult): Integer; override;
  end;

  /// <summary>The same, terminating: it runs instead of the action that would win.</summary>
  TTerminatingAction = class(TRecordingAction)
  public
    function IsTerminating: Boolean; override;
  end;

  TDirectivesWorld = class
  public
    Root: IRootCommand;
    Build: ICommand;
    ValueArg: IArgument<string>;
    FileArg: IArgument<string>;
    TraceOpt: IOption<Boolean>;
    LevelOpt: IOption<Integer>;
    ForceOpt: IOption<Boolean>;
    CopyCommand: ICommand;
    Diagram: IDiagramDirective;
    Trace: IDirective;
    /// <summary>A directive named Diagram, which differs from diagram only in case.</summary>
    UpperDiagram: IDirective;
    /// <summary>The message of the exception the When caught, or <c>&lt;no exception&gt;</c>.</summary>
    Report: string;
    Configuration: TParserConfiguration;
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
    function Invocation: TInvocationConfiguration;
    procedure Track(const Name: string);
    function RanText: string;
    /// <summary>The token a directive was written with and its values, or <c>&lt;not written&gt;</c>.</summary>
    function Written(const Directive: IDirective): string;
    /// <summary>The same, asked of a node of the tree instead of the parse result.</summary>
    function WrittenFrom(const Node: ISymbolResult; const Directive: IDirective): string;
    function Describe(const Found: IDirectiveResult): string;
    /// <summary>
    ///   A root mitool with an optional positional, a --trace option and an action, and a subcommand build with a
    ///   positional file and an action. It declares no directive: each scenario adds the ones it needs.
    /// </summary>
    procedure BuildGrammar;
    /// <summary>Adds to build an option --level, -l of type Integer with the default 3, and a flag --force, -f.</summary>
    procedure AddLevel;
    /// <summary>Adds to the root a subcommand copy with the arguments first, second and third, the last one a collection.</summary>
    procedure AddCopy;
    /// <summary>Writes a response file in the temp folder, removed when the world is, and answers its path.</summary>
    function WriteResponseFile(const FileName: string; const Lines: array of string): string;
  private
    FWritten: TArray<string>;
  end;

implementation

uses
  System.IOUtils,
  Daf.MiniSpec;

{ TRecordingAction }

constructor TRecordingAction.Create(World: TDirectivesWorld; const Name: string);
begin
  inherited Create;
  FWorld := World;
  FName := Name;
end;

function TRecordingAction.IsTerminating: Boolean;
begin
  Result := False;
end;

function TRecordingAction.Invoke(const Parsed: IParseResult): Integer;
begin
  FWorld.Track(FName);
  Result := 0;
end;

{ TTerminatingAction }

function TTerminatingAction.IsTerminating: Boolean;
begin
  Result := True;
end;

{ TDirectivesWorld }

constructor TDirectivesWorld.Create;
begin
  inherited;
  Output := TStringWriter.Create;
  Error := TStringWriter.Create;
  ExitCode := -1;
end;

destructor TDirectivesWorld.Destroy;
var
  Path: string;
begin
  for Path in FWritten do
    if TFile.Exists(Path) then
      TFile.Delete(Path);
  Output.Free;
  Error.Free;
  inherited;
end;

function TDirectivesWorld.Invocation: TInvocationConfiguration;
begin
  Result.Output := Output;
  Result.Error := Error;
end;

procedure TDirectivesWorld.Track(const Name: string);
begin
  if Ran <> '' then
    Ran := Ran + ',';
  Ran := Ran + Name;
end;

function TDirectivesWorld.RanText: string;
begin
  if Ran = '' then
    Result := '<nothing>'
  else
    Result := Ran;
end;

function TDirectivesWorld.Written(const Directive: IDirective): string;
begin
  Result := Describe(Parsed.GetResult(Directive));
end;

function TDirectivesWorld.WrittenFrom(const Node: ISymbolResult; const Directive: IDirective): string;
begin
  Result := Describe(Node.GetResult(Directive));
end;

function TDirectivesWorld.Describe(const Found: IDirectiveResult): string;
begin
  if Found = nil then
    Exit('<not written>');
  Result := Found.Token.Value + ' values=' + string.Join(',', Found.Values);
end;

procedure TDirectivesWorld.BuildGrammar;
begin
  ValueArg := CommandLine.Argument<string>('value').Arity(TArgumentArity.ZeroOrOne);
  TraceOpt := CommandLine.Option<Boolean>('--trace');
  FileArg := CommandLine.Argument<string>('file');
  Build := CommandLine.Command('build', 'Build a file');
  Build.Add(FileArg);
  Build.SetAction(
    procedure(const Parsed: IParseResult)
    begin
      Track('build');
    end);
  Root := CommandLine.Root('mitool', 'A tool');
  Root.Add([ValueArg, TraceOpt, Build]);
  Root.SetAction(
    procedure(const Parsed: IParseResult)
    begin
      Track('mitool');
    end);
  Diagram := CommandLine.DiagramDirective;
  Trace := CommandLine.Directive('trace');
end;

procedure TDirectivesWorld.AddLevel;
begin
  LevelOpt := CommandLine.Option<Integer>('--level', ['-l']).DefaultValue(3);
  ForceOpt := CommandLine.Option<Boolean>('--force', ['-f']);
  Build.Add([LevelOpt, ForceOpt]);
end;

procedure TDirectivesWorld.AddCopy;
begin
  CopyCommand := CommandLine.Command('copy', 'Copy things');
  CopyCommand.Add([CommandLine.Argument<string>('first'), CommandLine.Argument<string>('second'),
    CommandLine.Argument<TArray<string>>('third')]);
  CopyCommand.SetAction(
    procedure(const Parsed: IParseResult)
    begin
      Track('copy');
    end);
  Root.Add(CopyCommand);
end;

function TDirectivesWorld.WriteResponseFile(const FileName: string; const Lines: array of string): string;
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

initialization

Feature('''
Feature Directives @commandline @directives

  As the author of a tool
  I want an invocation to carry instructions for the parser itself
  So I can see how an invocation was understood without mixing that into my own grammar
''')

.UseWorld<TDirectivesWorld>

.Rule('a directive is written in square brackets before anything else')

  .Scenario('A declared directive')
    .Given('a root that declares the directive diagram',
      procedure(W: TDirectivesWorld)
      begin
        W.BuildGrammar;
        W.Root.Add(W.Diagram);
      end)
    .When('an invocation starting with [diagram] is parsed',
      procedure(W: TDirectivesWorld)
      begin
        W.Parsed := W.Root.Parse(['[diagram]', 'build', 'fichero.txt']);
      end)
    .&Then('the directive is written',
      procedure(W: TDirectivesWorld)
      begin
        Expect(W.Written(W.Diagram)).ToEqual('[diagram] values=');
      end)
    .&And('the rest of the invocation is parsed as always',
      procedure(W: TDirectivesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.Parsed.Command.Name + ' ' + W.FileArg.ValueIn(W.Parsed)).ToEqual('build fichero.txt');
      end)

  .Scenario('A directive with a value')
    .Given('a root that declares the directive trace',
      procedure(W: TDirectivesWorld)
      begin
        W.BuildGrammar;
        W.Root.Add(W.Trace);
      end)
    .When('an invocation starting with [trace:all] is parsed',
      procedure(W: TDirectivesWorld)
      begin
        W.Parsed := W.Root.Parse(['[trace:all]', 'build', 'fichero.txt']);
      end)
    .&Then('the directive is written with the value all',
      procedure(W: TDirectivesWorld)
      begin
        Expect(W.Written(W.Trace)).ToEqual('[trace:all] values=all');
      end)

  .Scenario('The result of a directive is found from any node of the tree')
    .Given('a root that declares the directive trace, with a subcommand build',
      procedure(W: TDirectivesWorld)
      begin
        W.BuildGrammar;
        W.Root.Add(W.Trace);
      end)
    .When('an invocation starting with [trace:all] and reaching build is parsed',
      procedure(W: TDirectivesWorld)
      begin
        W.Parsed := W.Root.Parse(['[trace:all]', 'build', 'fichero.txt']);
      end)
    .&Then('the result of build finds the directive, the same result the parse result finds',
      procedure(W: TDirectivesWorld)
      begin
        Expect(W.Parsed.Command.Name).ToEqual('build');
        Expect(W.WrittenFrom(W.Parsed.CommandResult, W.Trace)).ToEqual('[trace:all] values=all');
        Expect((W.Parsed.CommandResult.GetResult(W.Trace) as TObject) =
          (W.Parsed.GetResult(W.Trace) as TObject)).ToBeTrue;
      end)
    .&And('so does the result of the argument of build',
      procedure(W: TDirectivesWorld)
      begin
        Expect(W.WrittenFrom(W.Parsed.GetResult(W.FileArg), W.Trace)).ToEqual('[trace:all] values=all');
      end)

  .Scenario('A directive behind the name of the root')
    .Given('a root mitool that declares the directive diagram',
      procedure(W: TDirectivesWorld)
      begin
        W.BuildGrammar;
        W.Root.Add(W.Diagram);
      end)
    .When('an invocation starting with mitool [diagram] is parsed',
      procedure(W: TDirectivesWorld)
      begin
        W.Parsed := W.Root.Parse(['mitool', '[diagram]', 'build', 'fichero.txt']);
      end)
    .&Then('the directive is written, and build is reached',
      procedure(W: TDirectivesWorld)
      begin
        Expect(W.Written(W.Diagram)).ToEqual('[diagram] values=');
        Expect(W.Parsed.Command.Name).ToEqual('build');
      end)

  .Scenario('Without directives declared, square brackets are a value')
    .Given('a root that declares no directive and has a positional argument',
      procedure(W: TDirectivesWorld)
      begin
        W.BuildGrammar;
      end)
    .When('an invocation passing [1,2] is parsed',
      procedure(W: TDirectivesWorld)
      begin
        W.Parsed := W.Root.Parse(['[1,2]']);
      end)
    .&Then('the argument receives [1,2]',
      procedure(W: TDirectivesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.ValueArg.ValueIn(W.Parsed)).ToEqual('[1,2]');
      end)

  .Scenario('With directives declared, square brackets that name none of them')
    .Given('a root that declares the directive diagram and has a positional argument',
      procedure(W: TDirectivesWorld)
      begin
        W.BuildGrammar;
        W.Root.Add(W.Diagram);
      end)
    .When('an invocation starting with [1,2] is parsed',
      procedure(W: TDirectivesWorld)
      begin
        W.Parsed := W.Root.Parse(['[1,2]']);
      end)
    .&Then('[1,2] is a token that matches nothing, and the argument does not receive it',
      procedure(W: TDirectivesWorld)
      begin
        Expect(string.Join(',', W.Parsed.UnmatchedTokens)).ToEqual('[1,2]');
        Expect(W.Parsed.ErrorText).ToContain('[1,2]');
        Expect(W.Parsed.WasProvided(W.ValueArg)).ToBeFalse;
      end)

  .Scenario('Behind --, nothing is a directive')
    .Given('a root that declares the directive diagram and has a positional argument',
      procedure(W: TDirectivesWorld)
      begin
        W.BuildGrammar;
        W.Root.Add(W.Diagram);
      end)
    .When('an invocation passing -- [diagram] is parsed',
      procedure(W: TDirectivesWorld)
      begin
        W.Parsed := W.Root.Parse(['--', '[diagram]']);
      end)
    .&Then('the argument receives [diagram], and no directive is written',
      procedure(W: TDirectivesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.ValueArg.ValueIn(W.Parsed)).ToEqual('[diagram]');
        Expect(W.Written(W.Diagram)).ToEqual('<not written>');
      end)

  .Scenario('After the first token, nothing is a directive')
    .Given('a root that declares the directive diagram, and a subcommand build with a positional argument',
      procedure(W: TDirectivesWorld)
      begin
        W.BuildGrammar;
        W.Root.Add(W.Diagram);
      end)
    .When('an invocation passing build [diagram] is parsed',
      procedure(W: TDirectivesWorld)
      begin
        W.Parsed := W.Root.Parse(['build', '[diagram]']);
      end)
    .&Then('the argument of build receives [diagram], and no directive is written',
      procedure(W: TDirectivesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.FileArg.ValueIn(W.Parsed)).ToEqual('[diagram]');
        Expect(W.Written(W.Diagram)).ToEqual('<not written>');
      end)

.Rule('a directive wins over the help and the version')

  .Scenario('The diagram of an invocation that asks for help')
    .Given('a root that declares the directive diagram',
      procedure(W: TDirectivesWorld)
      begin
        W.BuildGrammar;
        W.Root.Add(W.Diagram);
      end)
    .When('the result of parsing [diagram] --help is invoked',
      procedure(W: TDirectivesWorld)
      begin
        W.Parsed := W.Root.Parse(['[diagram]', '--help']);
        W.ExitCode := W.Parsed.Invoke(W.Invocation);
      end)
    .&Then('the diagram is written, and not the help',
      procedure(W: TDirectivesWorld)
      begin
        Expect(W.Output.ToString).ToEqual('[ mitool [ --help ] *[ --trace ] ]' + sLineBreak);
        Expect(W.Parsed.IsHelpRequested).ToBeFalse;
      end)
    .&And('the invocation answers 0',
      procedure(W: TDirectivesWorld)
      begin
        Expect(W.ExitCode).ToEqual(0);
      end)

  .Scenario('The diagram of an invocation that asks for the version')
    .Given('a root with a version that declares the directive diagram',
      procedure(W: TDirectivesWorld)
      begin
        W.BuildGrammar;
        W.Root.WithVersion('1.2.3');
        W.Root.Add(W.Diagram);
      end)
    .When('the result of parsing [diagram] --version is invoked',
      procedure(W: TDirectivesWorld)
      begin
        W.Parsed := W.Root.Parse(['[diagram]', '--version']);
        W.ExitCode := W.Parsed.Invoke(W.Invocation);
      end)
    .&Then('the diagram is written, and not the version',
      procedure(W: TDirectivesWorld)
      begin
        Expect(W.Output.ToString).ToEqual('[ mitool [ --version ] *[ --trace ] ]' + sLineBreak);
        Expect(W.Parsed.IsVersionRequested).ToBeFalse;
      end)

  .Scenario('A named option does not run its action either')
    .Given('a root that declares the directive diagram, and an option with an action that is not terminating',
      procedure(W: TDirectivesWorld)
      begin
        W.BuildGrammar;
        W.TraceOpt.Action(TRecordingAction.Create(W, 'trace'));
        W.Root.Add(W.Diagram);
      end)
    .When('the result of parsing [diagram] --trace is invoked',
      procedure(W: TDirectivesWorld)
      begin
        W.Parsed := W.Root.Parse(['[diagram]', '--trace']);
        W.ExitCode := W.Parsed.Invoke(W.Invocation);
      end)
    .&Then('neither the action of the option nor the one of the command runs',
      procedure(W: TDirectivesWorld)
      begin
        Expect(W.RanText).ToEqual('<nothing>');
      end)

.Rule('the name of a directive tells cases apart as the names of options do')

  .Scenario('A directive written with another case')
    .Given('a root that declares the directive diagram',
      procedure(W: TDirectivesWorld)
      begin
        W.BuildGrammar;
        W.Root.Add(W.Diagram);
      end)
    .When('an invocation starting with [Diagram] is parsed',
      procedure(W: TDirectivesWorld)
      begin
        W.Parsed := W.Root.Parse(['[Diagram]']);
      end)
    .&Then('[Diagram] is a token that matches nothing',
      procedure(W: TDirectivesWorld)
      begin
        Expect(W.Written(W.Diagram)).ToEqual('<not written>');
        Expect(string.Join(',', W.Parsed.UnmatchedTokens)).ToEqual('[Diagram]');
      end)

  .Scenario('A directive written with another case, when names do not tell cases apart')
    .Given('a root that declares the directive diagram, and the parser configured not to tell cases apart',
      procedure(W: TDirectivesWorld)
      begin
        W.BuildGrammar;
        W.Root.Add(W.Diagram);
        W.Configuration.EnableCaseInsensitiveNames := True;
      end)
    .When('an invocation starting with [Diagram] is parsed',
      procedure(W: TDirectivesWorld)
      begin
        W.Parsed := W.Root.Parse(['[Diagram]'], W.Configuration);
      end)
    .&Then('the directive diagram is written',
      procedure(W: TDirectivesWorld)
      begin
        Expect(W.Written(W.Diagram)).ToEqual('[Diagram] values=');
        Expect(string.Join(',', W.Parsed.UnmatchedTokens)).ToEqual('');
      end)

  .Scenario('Two directives that differ only in case')
    .Given('a root that declares the directives diagram and Diagram, and the parser configured not to tell cases apart',
      procedure(W: TDirectivesWorld)
      begin
        W.BuildGrammar;
        W.UpperDiagram := CommandLine.Directive('Diagram');
        W.Root.Add(W.Diagram);
        W.Root.Add(W.UpperDiagram);
        W.Configuration.EnableCaseInsensitiveNames := True;
      end)
    .When('any invocation is parsed',
      procedure(W: TDirectivesWorld)
      begin
        W.Report := '<no exception>';
        try
          W.Parsed := W.Root.Parse(['build', 'fichero.txt'], W.Configuration);
        except
          on E: ECommandLine do
            W.Report := E.Message;
        end;
      end)
    .&Then('the grammar is refused, naming the two',
      procedure(W: TDirectivesWorld)
      begin
        Expect(W.Report).ToContain('"[diagram]"');
        Expect(W.Report).ToContain('"[Diagram]"');
      end)

  .Scenario('Two directives that differ only in case, told apart')
    .Given('a root that declares the directives diagram and Diagram, and the parser left to tell cases apart',
      procedure(W: TDirectivesWorld)
      begin
        W.BuildGrammar;
        W.UpperDiagram := CommandLine.Directive('Diagram');
        W.Root.Add(W.Diagram);
        W.Root.Add(W.UpperDiagram);
      end)
    .When('an invocation starting with [Diagram] is parsed',
      procedure(W: TDirectivesWorld)
      begin
        W.Parsed := W.Root.Parse(['[Diagram]', 'build', 'fichero.txt']);
      end)
    .&Then('Diagram is written, and diagram is not',
      procedure(W: TDirectivesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.Written(W.UpperDiagram)).ToEqual('[Diagram] values=');
        Expect(W.Written(W.Diagram)).ToEqual('<not written>');
      end)

  .Scenario('A directive and a subcommand that differ only in case')
    .Given('a root with the directive diagram and a subcommand Diagram, and the parser configured not to tell cases apart',
      procedure(W: TDirectivesWorld)
      begin
        W.BuildGrammar;
        W.Root.Add(W.Diagram);
        W.Root.Add(CommandLine.Command('Diagram', 'Draw things'));
        W.Configuration.EnableCaseInsensitiveNames := True;
      end)
    .When('an invocation passing [diagram] and then diagram is parsed',
      procedure(W: TDirectivesWorld)
      begin
        W.Report := '<no exception>';
        try
          W.Parsed := W.Root.Parse(['[diagram]', 'diagram'], W.Configuration);
        except
          on E: ECommandLine do
            W.Report := E.Message;
        end;
      end)
    .&Then('the grammar is not refused: the brackets are the directive, and the bare name the subcommand',
      procedure(W: TDirectivesWorld)
      begin
        Expect(W.Report).ToEqual('<no exception>');
        Expect(W.Written(W.Diagram)).ToEqual('[diagram] values=');
        Expect(W.Parsed.Command.Name).ToEqual('Diagram');
      end)

.Rule('[diagram] writes how the invocation was understood instead of running it')

  .Scenario('The diagram of a correct invocation')
    .Given('a root mitool that declares the directive diagram, with a subcommand build that has a positional argument',
      procedure(W: TDirectivesWorld)
      begin
        W.BuildGrammar;
        W.Root.Add(W.Diagram);
      end)
    .When('the result of parsing [diagram] build fichero.txt is invoked',
      procedure(W: TDirectivesWorld)
      begin
        W.Parsed := W.Root.Parse(['[diagram]', 'build', 'fichero.txt']);
        W.ExitCode := W.Parsed.Invoke(W.Invocation);
      end)
    .&Then('a diagram is written in which build goes inside mitool and carries fichero.txt',
      procedure(W: TDirectivesWorld)
      begin
        Expect(W.Output.ToString).ToEqual('[ mitool [ build <fichero.txt> ] *[ --trace ] ]' + sLineBreak);
      end)
    .&And('no action runs, and the invocation answers 0',
      procedure(W: TDirectivesWorld)
      begin
        Expect(Format('ran=%s exit=%d', [W.RanText, W.ExitCode])).ToEqual('ran=<nothing> exit=0');
      end)

  .Scenario('A default value in the diagram')
    .Given('a command with an option with a default value',
      procedure(W: TDirectivesWorld)
      begin
        W.BuildGrammar;
        W.AddLevel;
        W.Root.Add(W.Diagram);
      end)
    .When('the result of parsing [diagram] and the command, without the option, is invoked',
      procedure(W: TDirectivesWorld)
      begin
        W.Parsed := W.Root.Parse(['[diagram]', 'build', 'fichero.txt']);
        W.ExitCode := W.Parsed.Invoke(W.Invocation);
      end)
    .&Then('the option is in the diagram, marked with *',
      procedure(W: TDirectivesWorld)
      begin
        Expect(W.Output.ToString).ToEqual('[ mitool [ build <fichero.txt> *[ --level <3> ] *[ --force ] ] *[ --trace ] ]' + sLineBreak);
      end)

  .Scenario('The diagram of an invocation with errors')
    .Given('a command with an option of type Integer',
      procedure(W: TDirectivesWorld)
      begin
        W.BuildGrammar;
        W.AddLevel;
        W.Root.Add(W.Diagram);
      end)
    .When('the result of parsing [diagram] with a value that is not an integer is invoked',
      procedure(W: TDirectivesWorld)
      begin
        W.Parsed := W.Root.Parse(['[diagram]', 'build', 'fichero.txt', '--level', 'abc']);
        W.ExitCode := W.Parsed.Invoke(W.Invocation);
      end)
    .&Then('the option is in the diagram, marked with !',
      procedure(W: TDirectivesWorld)
      begin
        Expect(W.Output.ToString).ToEqual('[ mitool [ build <fichero.txt> ![ --level <abc> ] *[ --force ] ] *[ --trace ] ]' + sLineBreak);
      end)
    .&And('the invocation answers 2, and writes no error',
      procedure(W: TDirectivesWorld)
      begin
        Expect(W.ExitCode).ToEqual(2);
        Expect(W.Error.ToString).ToEqual('');
      end)

  .Scenario('Another code for a diagram with errors')
    .Given('a command with an option of type Integer, and the directive diagram answering 7 to errors',
      procedure(W: TDirectivesWorld)
      begin
        W.BuildGrammar;
        W.AddLevel;
        W.Diagram.ParseErrorReturnValue := 7;
        W.Root.Add(W.Diagram);
      end)
    .When('the result of parsing [diagram] with a value that is not an integer is invoked',
      procedure(W: TDirectivesWorld)
      begin
        W.Parsed := W.Root.Parse(['[diagram]', 'build', 'fichero.txt', '--level', 'abc']);
        W.ExitCode := W.Parsed.Invoke(W.Invocation);
      end)
    .&Then('the invocation answers 7',
      procedure(W: TDirectivesWorld)
      begin
        Expect(W.ExitCode).ToEqual(7);
      end)

  .Scenario('Each argument is named when the command declares several')
    .Given('a subcommand copy with the arguments first, second and third, the last one a collection',
      procedure(W: TDirectivesWorld)
      begin
        W.BuildGrammar;
        W.AddCopy;
        W.Root.Add(W.Diagram);
      end)
    .When('the result of parsing [diagram] copy one two three four five is invoked',
      procedure(W: TDirectivesWorld)
      begin
        W.Parsed := W.Root.Parse(['[diagram]', 'copy', 'one', 'two', 'three', 'four', 'five']);
        W.ExitCode := W.Parsed.Invoke(W.Invocation);
      end)
    .&Then('each argument shows its name and the tokens it took',
      procedure(W: TDirectivesWorld)
      begin
        Expect(W.Output.ToString).ToEqual(
          '[ mitool [ copy [ first <one> ] [ second <two> ] [ third <three> <four> <five> ] ] *[ --trace ] ]' + sLineBreak);
      end)

  .Scenario('What matched nothing goes at the end')
    .Given('a root mitool that declares the directive diagram, with a subcommand build that has one argument',
      procedure(W: TDirectivesWorld)
      begin
        W.BuildGrammar;
        W.Root.Add(W.Diagram);
      end)
    .When('the result of parsing [diagram] build fichero.txt extra is invoked',
      procedure(W: TDirectivesWorld)
      begin
        W.Parsed := W.Root.Parse(['[diagram]', 'build', 'fichero.txt', 'extra']);
        W.ExitCode := W.Parsed.Invoke(W.Invocation);
      end)
    .&Then('build carries the error, and extra follows the diagram',
      procedure(W: TDirectivesWorld)
      begin
        Expect(W.Output.ToString).ToEqual('[ mitool ![ build <fichero.txt> ] *[ --trace ] ]   ???--> extra' + sLineBreak);
        Expect(W.ExitCode).ToEqual(2);
      end)

.Rule('the diagram directive is never left with no action')

  .Scenario('No action on the diagram directive')
    .Given('a root that declares the directive diagram',
      procedure(W: TDirectivesWorld)
      begin
        W.BuildGrammar;
        W.Root.Add(W.Diagram);
      end)
    .When('the directive is given no action',
      procedure(W: TDirectivesWorld)
      begin
        try
          W.Diagram.Action(nil);
          W.Report := '<no exception>';
        except
          on E: Exception do
            W.Report := E.ClassName + ': ' + E.Message;
        end;
      end)
    .&Then('it fails',
      procedure(W: TDirectivesWorld)
      begin
        Expect(W.Report).ToEqual('ECommandLine: The action of "diagram" is not assigned');
      end)

  .Scenario('An action of its own on the diagram directive')
    .Given('a root that declares the directive diagram, with an action of its own',
      procedure(W: TDirectivesWorld)
      begin
        W.BuildGrammar;
        W.Diagram.Action(TTerminatingAction.Create(W, 'own diagram'));
        W.Root.Add(W.Diagram);
      end)
    .When('the result of parsing [diagram] build fichero.txt is invoked',
      procedure(W: TDirectivesWorld)
      begin
        W.Parsed := W.Root.Parse(['[diagram]', 'build', 'fichero.txt']);
        W.ExitCode := W.Parsed.Invoke(W.Invocation);
      end)
    .&Then('that action runs, and the diagram is not written',
      procedure(W: TDirectivesWorld)
      begin
        Expect(Format('ran=%s output=[%s]', [W.RanText, W.Output.ToString])).ToEqual('ran=own diagram output=[]');
      end)

.Rule('the diagram shows the tokens the way the parse rewrote them')

  .Scenario('A group of one-letter options')
    .Given('a command with a flag -f and an option -l that takes a value',
      procedure(W: TDirectivesWorld)
      begin
        W.BuildGrammar;
        W.AddLevel;
        W.Root.Add(W.Diagram);
      end)
    .When('the result of parsing [diagram] build fichero.txt -fl5 is invoked',
      procedure(W: TDirectivesWorld)
      begin
        W.Parsed := W.Root.Parse(['[diagram]', 'build', 'fichero.txt', '-fl5']);
        W.ExitCode := W.Parsed.Invoke(W.Invocation);
      end)
    .&Then('the group is written as the options it was split into',
      procedure(W: TDirectivesWorld)
      begin
        Expect(W.Output.ToString).ToEqual('[ mitool [ build <fichero.txt> [ -f ] [ -l <5> ] ] *[ --trace ] ]' + sLineBreak);
      end)

  .Scenario('A value behind a colon')
    .Given('a command with an option --level that takes a value',
      procedure(W: TDirectivesWorld)
      begin
        W.BuildGrammar;
        W.AddLevel;
        W.Root.Add(W.Diagram);
      end)
    .When('the result of parsing [diagram] build fichero.txt --level:5 is invoked',
      procedure(W: TDirectivesWorld)
      begin
        W.Parsed := W.Root.Parse(['[diagram]', 'build', 'fichero.txt', '--level:5']);
        W.ExitCode := W.Parsed.Invoke(W.Invocation);
      end)
    .&Then('the option is written apart from its value',
      procedure(W: TDirectivesWorld)
      begin
        Expect(W.Output.ToString).ToEqual('[ mitool [ build <fichero.txt> [ --level <5> ] *[ --force ] ] *[ --trace ] ]' + sLineBreak);
      end)

  .Scenario('A response file')
    .Given('a command with an option --level, and a response file that holds the rest of an invocation',
      procedure(W: TDirectivesWorld)
      begin
        W.BuildGrammar;
        W.AddLevel;
        W.Root.Add(W.Diagram);
        W.WriteResponseFile('daf-cl-diagram.rsp', ['fichero.txt', '--level 5']);
      end)
    .When('the result of parsing [diagram] build @file is invoked',
      procedure(W: TDirectivesWorld)
      begin
        W.Parsed := W.Root.Parse(['[diagram]', 'build',
          '@' + TPath.Combine(TPath.GetTempPath, 'daf-cl-diagram.rsp')]);
        W.ExitCode := W.Parsed.Invoke(W.Invocation);
      end)
    .&Then('the tokens the file contributed are written in its place',
      procedure(W: TDirectivesWorld)
      begin
        Expect(W.Output.ToString).ToEqual('[ mitool [ build <fichero.txt> [ --level <5> ] *[ --force ] ] *[ --trace ] ]' + sLineBreak);
      end);

end.
