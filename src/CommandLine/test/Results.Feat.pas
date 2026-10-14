unit Results.Feat;

interface

uses
  System.SysUtils,
  Daf.Nullable,
  Daf.CommandLine;

type
  TResultsWorld = class
  public
    // mytool build native <target>, with a recursive --verbose at the root.
    Tree: IRootCommand;
    Build: ICommand;
    Native: ICommand;
    // tool [--port <int>] [--level <int>, default 3] [--tag <string>] [--files <string>...], with an action.
    Flat: IRootCommand;
    PortOpt: IOption<Integer>;
    LevelOpt: IOption<Integer>;
    TagOpt: IOption<string>;
    FilesOpt: IOption<string>;
    // tool --seed <int>, required.
    Needy: IRootCommand;
    SeedOpt: IOption<Integer>;
    // tool [--port <int>] {run | serve [--workers <int>, default 5]}, both with an action.
    Runner: IRootCommand;
    ParentPortOpt: IOption<Integer>;
    Run: ICommand;
    Serve: ICommand;
    WorkersOpt: IOption<Integer>;
    // A directive of the runner, and an option that belongs to no grammar.
    ProbeDirective: IDirective;
    StrangerOpt: IOption<Integer>;
    // tool [<name>] [<count>] [<limit>] [<items>...], none of them demanded.
    Loose: IRootCommand;
    NameArg: IArgument<string>;
    CountArg: IArgument<Integer>;
    LimitArg: IArgument<TNullable<Integer>>;
    ItemsArg: IArgument<TArray<string>>;

    Parsed: IParseResult;
    Report: string;
    // The columns of the outline.
    Kind: string;
    Expected: string;
    procedure BuildTree;
    procedure BuildFlat;
    procedure BuildNeedy;
    procedure BuildRunner;
    /// <summary>The runner, with the directive probe, and an option added to nothing.</summary>
    procedure BuildDirectedRunner;
    procedure BuildLoose;
    /// <summary>The result of <c>Symbol</c>, asked as a symbol and not as its kind.</summary>
    function ResultAsSymbol(const Symbol: ISymbol): ISymbolResult;
    function MissingResponseFile: string;
    /// <summary>What a read answers, or the exception it raises, as text.</summary>
    function Attempt(const Read: TFunc<string>): string;
  end;

implementation

uses
  System.IOUtils,
  Daf.MiniSpec;

function Same(const A, B: IInterface): Boolean;
begin
  Result := (A <> nil) and (B <> nil) and ((A as TObject) = (B as TObject));
end;

{ TResultsWorld }

procedure TResultsWorld.BuildTree;
begin
  Native := CommandLine.Command('native', 'Build a native binary')
    .Add(CommandLine.Argument<string>('target').Description('What to build'));
  Build := CommandLine.Command('build', 'Build things').Add(Native);
  Tree := CommandLine.Root('mytool', 'A tool that builds things');
  Tree.Add(CommandLine.Option<Boolean>('--verbose').Description('Say more').Recursive);
  Tree.Add(Build);
end;

procedure TResultsWorld.BuildFlat;
begin
  PortOpt := CommandLine.Option<Integer>('--port').Description('Port to listen on');
  LevelOpt := CommandLine.Option<Integer>('--level').Description('How hard to try').DefaultValue(3);
  TagOpt := CommandLine.Option<string>('--tag').Description('What to call it');
  FilesOpt := CommandLine.Option<string>('--files').Description('What to read').Arity(TArgumentArity.ZeroOrMore);
  Flat := CommandLine.Root('tool', 'A tool');
  Flat.Add([PortOpt, LevelOpt, TagOpt, FilesOpt]);
  Flat.SetAction(
    function(const Parsed: IParseResult): Integer
    begin
      Result := 0;
    end);
end;

procedure TResultsWorld.BuildNeedy;
begin
  SeedOpt := CommandLine.Option<Integer>('--seed').Description('Starting point').Required;
  Needy := CommandLine.Root('tool', 'A tool');
  Needy.Add(SeedOpt);
end;

procedure TResultsWorld.BuildRunner;
begin
  ParentPortOpt := CommandLine.Option<Integer>('--port').Description('Port to listen on');
  WorkersOpt := CommandLine.Option<Integer>('--workers').Description('How many at once').DefaultValue(5);
  Run := CommandLine.Command('run', 'Runs it').SetAction(
    function(const Parsed: IParseResult): Integer
    begin
      Result := 0;
    end);
  Serve := CommandLine.Command('serve', 'Serves it').Add(WorkersOpt).SetAction(
    function(const Parsed: IParseResult): Integer
    begin
      Result := 0;
    end);
  Runner := CommandLine.Root('tool', 'A tool with two subcommands');
  Runner.Add(ParentPortOpt);
  Runner.Add(Run);
  Runner.Add(Serve);
end;

procedure TResultsWorld.BuildDirectedRunner;
begin
  BuildRunner;
  ProbeDirective := CommandLine.Directive('probe');
  Runner.Add(ProbeDirective);
  StrangerOpt := CommandLine.Option<Integer>('--stranger').Description('Belongs to nothing');
end;

function TResultsWorld.ResultAsSymbol(const Symbol: ISymbol): ISymbolResult;
begin
  Result := Parsed.GetResult(Symbol);
end;

procedure TResultsWorld.BuildLoose;
begin
  NameArg := CommandLine.Argument<string>('name').Description('What it is called').Arity(TArgumentArity.ZeroOrOne);
  CountArg := CommandLine.Argument<Integer>('count').Description('How many').Arity(TArgumentArity.ZeroOrOne);
  LimitArg := CommandLine.Argument<TNullable<Integer>>('limit').Description('How far').Arity(TArgumentArity.ZeroOrOne);
  ItemsArg := CommandLine.Argument<TArray<string>>('items').Description('What else');
  Loose := CommandLine.Root('tool', 'A tool');
  Loose.Add([NameArg, CountArg, LimitArg, ItemsArg]);
end;

function TResultsWorld.MissingResponseFile: string;
begin
  Result := TPath.Combine(TPath.GetTempPath, 'daf-cl-results-missing.rsp');
  if TFile.Exists(Result) then
    TFile.Delete(Result);
end;

function TResultsWorld.Attempt(const Read: TFunc<string>): string;
begin
  try
    Result := Read();
  except
    on E: Exception do
      Result := E.ClassName + ': ' + E.Message;
  end;
end;

initialization

Feature('''
Feature Command line parse results @commandline @results

  As a developer reading what an invocation produced
  I want one result per symbol, with its tokens, its errors and its parent
  So whatever needs a piece of the invocation reads it from the tree instead of rebuilding it
''')

.UseWorld<TResultsWorld>

.Rule('the result of the parse is a tree with one result per symbol')

  .Scenario('The tree follows the invocation')
    .Given('a root command with a subcommand that has another',
      procedure(W: TResultsWorld)
      begin
        W.BuildTree;
      end)
    .When('an invocation that names the leaf is parsed',
      procedure(W: TResultsWorld)
      var
        Leaf: ICommandResult;
        Middle: ICommandResult;
      begin
        W.Parsed := W.Tree.Parse(['build', 'native', 'app.dpr']);
        Leaf := W.Parsed.GetResult(W.Native);
        Middle := Leaf.Parent as ICommandResult;
        W.Report := Format('root=%s children=%d leaf=%s parent=%s same=%s', [
          W.Parsed.RootCommandResult.Command.Name, Length(W.Parsed.RootCommandResult.Children),
          W.Parsed.CommandResult.Command.Name, Middle.Command.Name,
          BoolToStr(Same(Middle, W.Parsed.GetResult(W.Build)), True)]);
      end)
    .&Then('there is a command result per level, and the parent of the leaf is the middle one',
      procedure(W: TResultsWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.Report).ToEqual('root=mytool children=2 leaf=native parent=build same=True');
      end)

  .Scenario('A default creates a result the invocation did not name')
    .Given('a command with an option that has a default and another that has none',
      procedure(W: TResultsWorld)
      begin
        W.BuildFlat;
      end)
    .When('an invocation that names neither is parsed',
      procedure(W: TResultsWorld)
      var
        Level: IOptionResult;
      begin
        W.Parsed := W.Flat.Parse([]);
        Level := W.Parsed.GetResult(W.LevelOpt);
        W.Report := Format('level implicit=%s named=%d token=%s; tag result=%s', [
          BoolToStr(Level.IsImplicit, True), Level.IdentifierTokenCount,
          BoolToStr(Level.IdentifierToken.HasValue, True),
          BoolToStr(W.Parsed.GetResult(W.TagOpt) <> nil, True)]);
      end)
    .&Then('the option with a default has a result the invocation did not name, and the other has none',
      procedure(W: TResultsWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.Report).ToEqual('level implicit=True named=0 token=False; tag result=False');
      end)

  .Scenario('A required option that is missing has a result with its error')
    .Given('a command with a required option and no default',
      procedure(W: TResultsWorld)
      begin
        W.BuildNeedy;
      end)
    .When('an invocation that does not name it is parsed',
      procedure(W: TResultsWorld)
      var
        Seed: IOptionResult;
      begin
        W.Parsed := W.Needy.Parse([]);
        Seed := W.Parsed.GetResult(W.SeedOpt);
        W.Report := Format('implicit=%s errors=%d message=%s owner=%s', [
          BoolToStr(Seed.IsImplicit, True), Length(Seed.Errors), Seed.Errors[0].Message,
          BoolToStr(Same(W.Parsed.Errors[0].SymbolResult, Seed), True)]);
      end)
    .&Then('the option has a result the invocation did not name, and the missing error is that result''s',
      procedure(W: TResultsWorld)
      begin
        Expect(W.Report).ToEqual('implicit=True errors=1 message=Option ''--seed'' is required. owner=True');
      end)

  .Scenario('The tokens of a repeated option')
    .Given('an option that admits several values',
      procedure(W: TResultsWorld)
      begin
        W.BuildFlat;
      end)
    .When('an invocation that names it twice, with a value each time, is parsed',
      procedure(W: TResultsWorld)
      var
        Files: IOptionResult;
        Token: TToken;
      begin
        W.Parsed := W.Flat.Parse(['--files', 'a.pas', '--files', 'b.pas']);
        Files := W.Parsed.GetResult(W.FilesOpt);
        W.Report := Format('named=%d first=%s tokens=', [Files.IdentifierTokenCount, Files.IdentifierToken.Value.Value]);
        for Token in Files.Tokens do
          W.Report := W.Report + Token.Value + ' ';
      end)
    .&Then('its result holds both values in order, and counts that it was named twice',
      procedure(W: TResultsWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.Report).ToEqual('named=2 first=--files tokens=a.pas b.pas ');
      end)

  .Scenario('An error says which symbol it belongs to')
    .Given('a command with an integer option',
      procedure(W: TResultsWorld)
      begin
        W.BuildFlat;
      end)
    .When('an invocation that gives it a value that is not an integer is parsed',
      procedure(W: TResultsWorld)
      begin
        W.Parsed := W.Flat.Parse(['--port', 'x']);
        W.Report := Format('errors=%d owner=%s', [Length(W.Parsed.Errors),
          BoolToStr(Same(W.Parsed.Errors[0].SymbolResult, W.Parsed.GetResult(W.PortOpt)), True)]);
      end)
    .&Then('the error is the result of that option''s, with the text it always had',
      procedure(W: TResultsWorld)
      begin
        Expect(W.Report).ToEqual('errors=1 owner=True');
        Expect(W.Parsed.ErrorText).ToEqual('Cannot parse argument ''x'' for option ''--port'' as expected type ''integer''.');
      end)

  .Scenario('A token that matches nothing is an error of the command reached')
    .Given('a subcommand that does not collect what it does not recognise',
      procedure(W: TResultsWorld)
      begin
        W.BuildRunner;
      end)
    .When('an invocation that names it with a token that matches nothing is parsed',
      procedure(W: TResultsWorld)
      begin
        W.Parsed := W.Runner.Parse(['run', 'stray']);
        W.Report := Format('owner=%s', [
          BoolToStr(Same(W.Parsed.Errors[0].SymbolResult, W.Parsed.GetResult(W.Run)), True)]);
      end)
    .&Then('the error is the result of that subcommand''s',
      procedure(W: TResultsWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('Unrecognized command or argument ''stray''.');
        Expect(W.Report).ToEqual('owner=True');
      end)

  .Scenario('An error of reading the invocation belongs to no symbol')
    .Given('a command, and a response file that does not exist',
      procedure(W: TResultsWorld)
      begin
        W.BuildFlat;
      end)
    .When('an invocation that names that file is parsed',
      procedure(W: TResultsWorld)
      begin
        W.Parsed := W.Flat.Parse(['@' + W.MissingResponseFile]);
        W.Report := Format('errors=%d owner=%s', [Length(W.Parsed.Errors),
          BoolToStr(W.Parsed.Errors[0].SymbolResult <> nil, True)]);
      end)
    .&Then('the error belongs to no result, because no symbol had been reached',
      procedure(W: TResultsWorld)
      begin
        Expect(W.Report).ToEqual('errors=1 owner=False');
        Expect(W.Parsed.ErrorText).ToContain('Response file');
      end)

  .Scenario('A symbol the invocation did not reach has no result')
    .Given('a root with two subcommands, the second with an option that has a default',
      procedure(W: TResultsWorld)
      begin
        W.BuildRunner;
      end)
    .When('an invocation that names the first one is parsed',
      procedure(W: TResultsWorld)
      begin
        W.Parsed := W.Runner.Parse(['run']);
        W.Report := Format('serve=%s workers=%s', [BoolToStr(W.Parsed.GetResult(W.Serve) <> nil, True),
          BoolToStr(W.Parsed.GetResult(W.WorkersOpt) <> nil, True)]);
      end)
    .&Then('neither the second subcommand nor its option has a result',
      procedure(W: TResultsWorld)
      begin
        Expect(W.Report).ToEqual('serve=False workers=False');
      end)

.Rule('the result of a symbol is asked for without saying what kind of symbol it is')

  .Scenario('The same result the query of its kind gives')
    .Given('an option the invocation carries',
      procedure(W: TResultsWorld)
      begin
        W.BuildFlat;
      end)
    .When('its result is asked for as a symbol',
      procedure(W: TResultsWorld)
      begin
        W.Parsed := W.Flat.Parse(['--port', '80']);
        W.Report := 'same=' + BoolToStr(Same(W.ResultAsSymbol(W.PortOpt), W.Parsed.GetResult(W.PortOpt)), True);
      end)
    .&Then('it is the one the query of an option gives',
      procedure(W: TResultsWorld)
      begin
        Expect(W.Report).ToEqual('same=True');
      end)

  .Scenario('A directive the invocation writes')
    .Given('a root with a directive the invocation writes',
      procedure(W: TResultsWorld)
      begin
        W.BuildDirectedRunner;
      end)
    .When('its result is asked for as a symbol',
      procedure(W: TResultsWorld)
      var
        Found: ISymbolResult;
      begin
        W.Parsed := W.Runner.Parse(['[probe:x]', 'run']);
        Found := W.ResultAsSymbol(W.ProbeDirective);
        W.Report := Format('directive=%s same=%s', [BoolToStr(Supports(Found, IDirectiveResult), True),
          BoolToStr(Same(Found, W.Parsed.GetResult(W.ProbeDirective)), True)]);
      end)
    .&Then('it is the result of the directive',
      procedure(W: TResultsWorld)
      begin
        Expect(W.Report).ToEqual('directive=True same=True');
      end)

  .Scenario('A command the invocation did not reach')
    .Given('a root with two subcommands',
      procedure(W: TResultsWorld)
      begin
        W.BuildDirectedRunner;
      end)
    .When('an invocation that names one is parsed, and the result of each is asked for as a symbol',
      procedure(W: TResultsWorld)
      begin
        W.Parsed := W.Runner.Parse(['run']);
        W.Report := Format('run=%s serve=%s', [BoolToStr(W.ResultAsSymbol(W.Run) <> nil, True),
          BoolToStr(W.ResultAsSymbol(W.Serve) <> nil, True)]);
      end)
    .&Then('the one it named has a result, and the other has none',
      procedure(W: TResultsWorld)
      begin
        Expect(W.Report).ToEqual('run=True serve=False');
      end)

  .Scenario('A symbol foreign to the grammar')
    .Given('an option that was not added to the grammar parsed',
      procedure(W: TResultsWorld)
      begin
        W.BuildDirectedRunner;
      end)
    .When('its result is asked for as a symbol',
      procedure(W: TResultsWorld)
      begin
        W.Parsed := W.Runner.Parse(['run']);
        W.Report := 'result=' + BoolToStr(W.ResultAsSymbol(W.StrangerOpt) <> nil, True);
      end)
    .&Then('there is none',
      procedure(W: TResultsWorld)
      begin
        Expect(W.Report).ToEqual('result=False');
      end)

.Rule('a value is read with the declared object on any result of the tree')

  .Scenario('Reading a value on the result of a command')
    .Given('a subcommand with an integer option',
      procedure(W: TResultsWorld)
      begin
        W.BuildRunner;
      end)
    .When('an invocation that names the subcommand and gives the option 8080 is parsed and read',
      procedure(W: TResultsWorld)
      begin
        W.Parsed := W.Runner.Parse(['serve', '--workers', '8080']);
        W.Report := Format('on the command=%d on the parse=%d', [W.WorkersOpt.ValueIn(W.Parsed.CommandResult),
          W.WorkersOpt.ValueIn(W.Parsed)]);
      end)
    .&Then('the value read on the command is 8080, the same as on the whole result',
      procedure(W: TResultsWorld)
      begin
        Expect(W.Report).ToEqual('on the command=8080 on the parse=8080');
      end)

  .Scenario('Reading a symbol of a command that was not invoked')
    .Given('a root with two subcommands, the second with an integer option whose default is 5',
      procedure(W: TResultsWorld)
      begin
        W.BuildRunner;
      end)
    .When('an invocation that names the first one is parsed, and the option of the second is read',
      procedure(W: TResultsWorld)
      begin
        W.Parsed := W.Runner.Parse(['run']);
        W.Report := 'workers=' + IntToStr(W.WorkersOpt.ValueIn(W.Parsed));
      end)
    .&Then('the value is 0, the default of the type and not the declared one',
      procedure(W: TResultsWorld)
      begin
        Expect(W.Report).ToEqual('workers=0');
      end)

.Rule('reading a value the parse refused raises its error')

  .Scenario('Reading a value that does not convert')
    .Given('a command with an integer option',
      procedure(W: TResultsWorld)
      begin
        W.BuildFlat;
      end)
    .When('an invocation that gives it x is parsed, and the option is read',
      procedure(W: TResultsWorld)
      begin
        W.Parsed := W.Flat.Parse(['--port', 'x']);
        W.Report := W.Attempt(
          function: string
          begin
            Result := IntToStr(W.PortOpt.ValueIn(W.Parsed));
          end);
      end)
    .&Then('the read raises the error of the conversion',
      procedure(W: TResultsWorld)
      begin
        Expect(W.Report).ToEqual('ECommandLine: Cannot parse argument ''x'' for option ''--port'' as expected type ''integer''.');
      end)

  .Scenario('Reading a repeated option')
    .Given('an option that admits one value',
      procedure(W: TResultsWorld)
      begin
        W.BuildFlat;
      end)
    .When('an invocation that names it twice is parsed, and the option is read',
      procedure(W: TResultsWorld)
      begin
        W.Parsed := W.Flat.Parse(['--port', '1', '--port', '2']);
        W.Report := W.Attempt(
          function: string
          begin
            Result := IntToStr(W.PortOpt.ValueIn(W.Parsed));
          end);
      end)
    .&Then('the read raises the error of the repetition, and picks neither value',
      procedure(W: TResultsWorld)
      begin
        Expect(W.Report).ToEqual('ECommandLine: Option ''--port'' expects a single argument but 2 were provided.');
      end)

  .Scenario('Reading a required option that is missing')
    .Given('a required option with no default',
      procedure(W: TResultsWorld)
      begin
        W.BuildNeedy;
      end)
    .When('an invocation that does not name it is parsed, and the option is read',
      procedure(W: TResultsWorld)
      begin
        W.Parsed := W.Needy.Parse([]);
        W.Report := W.Attempt(
          function: string
          begin
            Result := IntToStr(W.SeedOpt.ValueIn(W.Parsed));
          end);
      end)
    .&Then('the read raises the error that it is missing',
      procedure(W: TResultsWorld)
      begin
        Expect(W.Report).ToEqual('ECommandLine: Option ''--seed'' is required.');
      end)

  .Scenario('Reading an option of a parent that the subcommand does not validate')
    .Given('a root with an integer option that is neither recursive nor defaulted, and a subcommand',
      procedure(W: TResultsWorld)
      begin
        W.BuildRunner;
      end)
    .When('an invocation that gives the option x before naming the subcommand is parsed, and the option is read',
      procedure(W: TResultsWorld)
      begin
        W.Parsed := W.Runner.Parse(['--port', 'x', 'run']);
        W.Report := W.Attempt(
          function: string
          begin
            Result := IntToStr(W.ParentPortOpt.ValueIn(W.Parsed));
          end);
      end)
    .&Then('the parse reports no error, and the read raises the error of the conversion',
      procedure(W: TResultsWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.Report).ToEqual('ECommandLine: Cannot parse argument ''x'' for option ''--port'' as expected type ''integer''.');
      end)
    .&And('the error joins the result of the option, and the parse result stays clean',
      procedure(W: TResultsWorld)
      begin
        Expect(Length(W.Parsed.GetResult(W.ParentPortOpt).Errors)).ToEqual(1);
        Expect(W.Parsed.Succeeded).ToBeTrue;
      end)

.Rule('a required value of a symbol with no result answers what .NET answers for its type')

  .Scenario('An option with no result')
    .Given('an option that is neither required nor defaulted',
      procedure(W: TResultsWorld)
      begin
        W.BuildFlat;
      end)
    .When('an invocation that does not name it is parsed, and its required value is read',
      procedure(W: TResultsWorld)
      begin
        W.Parsed := W.Flat.Parse([]);
        W.Report := W.Attempt(
          function: string
          begin
            Result := W.TagOpt.RequiredValueIn(W.Parsed);
          end);
      end)
    .&Then('the read raises, naming the option',
      procedure(W: TResultsWorld)
      begin
        Expect(W.Report).ToEqual('ECommandLine: --tag is required but was not provided');
      end)

  .ScenarioOutline('A <Kind> argument with no result answers <Expected>')
    .Given('a command with four arguments that do not demand a value',
      procedure(W: TResultsWorld)
      begin
        W.BuildLoose;
      end)
    .When('an invocation with no positional is parsed, and the required value of the <Kind> argument is read',
      procedure(W: TResultsWorld)
      begin
        W.Parsed := W.Loose.Parse([]);
        W.Report := W.Attempt(
          function: string
          begin
            if W.Kind = 'string' then
              Result := '"' + W.NameArg.RequiredValueIn(W.Parsed) + '"'
            else if W.Kind = 'integer' then
              Result := IntToStr(W.CountArg.RequiredValueIn(W.Parsed))
            else if W.Kind = 'nullable' then
              Result := IntToStr(W.LimitArg.RequiredValueIn(W.Parsed).Value)
            else if W.Kind = 'array' then
              Result := Format('%d items', [Length(W.ItemsArg.RequiredValueIn(W.Parsed))])
            else
              Result := 'unknown kind ' + W.Kind;
          end);
      end)
    .&Then('the answer is <Expected>',
      procedure(W: TResultsWorld)
      begin
        Expect(W.Report).ToEqual(W.Expected);
      end)
    .Examples(
      [['Kind',    'Expected'],
       ['string',   'ECommandLine: name is required but was not provided'],
       ['integer',  '0'],
       ['nullable', 'ECommandLine: limit is required but was not provided'],
       ['array',    '0 items']]);

end.
