unit CommandLineText.Feat;

interface

uses
  Daf.CommandLine;

type
  TTextWorld = class
  public
    // Filled in by the examples of an outline, which inject by instance field.
    Shape: string;
    Line: string;
    Tokens: string;
    Report: string;
    Tool: IRootCommand;
    OutOpt: IOption<string>;
    PropertyOpt: IOption<string>;
    FileArg: IArgument<string>;
    // A root with a recursive option and a subcommand, to parse the subcommand on its own.
    Outer: IRootCommand;
    Build: ICommand;
    VerboseOpt: IOption<Boolean>;
    XOpt: IOption<string>;
    Parsed: IParseResult;
    procedure BuildTool;
    procedure BuildOuter;
  end;

implementation

uses
  System.SysUtils,
  Daf.MiniSpec;

{ TTextWorld }

procedure TTextWorld.BuildTool;
begin
  OutOpt := CommandLine.Option<string>('--out').Description('Where it goes');
  PropertyOpt := CommandLine.Option<string>('--property', ['/p']).Description('A property to set');
  FileArg := CommandLine.Argument<string>('file').Description('What to read')
    .Arity(TArgumentArity.ZeroOrOne);
  Tool := CommandLine.Root('mytool', 'A tool');
  Tool.Add([OutOpt, PropertyOpt, FileArg]);
end;

procedure TTextWorld.BuildOuter;
begin
  VerboseOpt := CommandLine.Option<Boolean>('--verbose').Description('Say more').Recursive;
  XOpt := CommandLine.Option<string>('-x').Description('A value');
  Build := CommandLine.Command('build', 'Build things').Add(XOpt);
  Outer := CommandLine.Root('outer', 'A tool with a subcommand');
  Outer.Add([VerboseOpt, Build]);
end;

initialization

Feature('''
Feature Command line text @commandline @text

  As someone who receives the whole invocation in one string
  I want it split as the command line splits it
  So I do not have to repeat the rules of quotes, and repeat them wrong
''')

.UseWorld<TTextWorld>

.Rule('a line is split as the command line splits it')

  .ScenarioOutline('A line with <Shape> splits into <Tokens>')
    .Given('a line with <Shape>',
      procedure(W: TTextWorld)
      begin
        Expect(W.Shape).ToNotEqual('');
      end)
    .When('it is split',
      procedure(W: TTextWorld)
      begin
        W.Report := '[' + string.Join('|', CommandLine.SplitCommandLine(W.Line)) + ']';
      end)
    .&Then('the tokens are <Tokens>',
      procedure(W: TTextWorld)
      begin
        Expect(W.Report).ToEqual(W.Tokens);
      end)
    .Examples(
      [['Shape', 'Line', 'Tokens'],
       ['spaces and a tab', 'one two'#9'three   four ', '[one|two|three|four]'],
       ['blanks in front', ' one two three   four', '[one|two|three|four]'],
       ['line feeds', ' one'#10'two'#10'three'#10'four'#10, '[one|two|three|four]'],
       ['carriage returns', ' one'#13#10'two'#13#10'three'#13#10'four'#13#10, '[one|two|three|four]'],
       ['a quoted path', 'rm -r "c:\temp files\"', '[rm|-r|c:\temp files\]'],
       ['a quoted value after a colon', 'the-command --the-option:"c:\temp files\"',
        '[the-command|--the-option:c:\temp files\]'],
       ['a quoted value after an equals sign', 'the-command /the-option="c:\temp files\"',
        '[the-command|/the-option=c:\temp files\]'],
       ['quotes inside a word', 'POST --raw=''{"Id":1,"Name":"Alice"}''', '[POST|--raw=''{Id:1,Name:Alice}'']'],
       ['quoted spaces inside a word', 'command --raw=''{"Id":1,"Movie Name":"The Three Musketeers"}''',
        '[command|--raw=''{Id:1,Movie Name:The Three Musketeers}'']'],
       ['an empty quoted token', 'x "" y', '[x||y]'],
       ['a quote opened inside a word', 'a"b c"d e', '[ab cd|e]'],
       ['two quoted tokens glued', '"a""b"', '[a|b]'],
       ['a quote never closed', 'a "b c', '[a]'],
       ['only blanks', '   ', '[]']])

.Rule('an invocation can be given as one line of text')

  .Scenario('A line with quotes')
    .Given('a command with an option --out and a positional argument',
      procedure(W: TTextWorld)
      begin
        W.BuildTool;
      end)
    .When('the line --out "my folder" file.txt is parsed',
      procedure(W: TTextWorld)
      begin
        W.Parsed := W.Tool.Parse('--out "my folder" file.txt');
      end)
    .&Then('--out is my folder and the argument is file.txt',
      procedure(W: TTextWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.OutOpt.ValueIn(W.Parsed)).ToEqual('my folder');
        Expect(W.FileArg.ValueIn(W.Parsed)).ToEqual('file.txt');
      end)

  .Scenario('The first token is the name of the root')
    .Given('a root command called mytool with a positional argument',
      procedure(W: TTextWorld)
      begin
        W.BuildTool;
      end)
    .When('the line mytool file.txt is parsed',
      procedure(W: TTextWorld)
      begin
        W.Parsed := W.Tool.Parse('mytool file.txt');
      end)
    .&Then('the argument is file.txt, and mytool is not taken for a value',
      procedure(W: TTextWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.FileArg.ValueIn(W.Parsed)).ToEqual('file.txt');
        Expect(W.Parsed.RootCommandResult.IdentifierToken.Value).ToEqual('mytool');
      end)

  .Scenario('The first token is the path of the executable')
    .Given('a root command with a positional argument',
      procedure(W: TTextWorld)
      begin
        W.BuildTool;
      end)
    .When('a line starting with the quoted path of the running executable is parsed',
      procedure(W: TTextWorld)
      begin
        W.Parsed := W.Tool.Parse('"' + CommandLine.ExecutablePath + '" file.txt');
      end)
    .&Then('the path names the root, and the argument is file.txt',
      procedure(W: TTextWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.FileArg.ValueIn(W.Parsed)).ToEqual('file.txt');
        Expect(W.Parsed.RootCommandResult.IdentifierToken.Value).ToEqual(CommandLine.ExecutablePath);
      end)

  .Scenario('The name of the executable')
    .Given('the running executable',
      procedure(W: TTextWorld)
      begin
        Expect(CommandLine.ExecutablePath).ToContain('CommandLineSpecs');
      end)
    .When('its name is asked for',
      procedure(W: TTextWorld)
      begin
        W.Report := CommandLine.ExecutableName;
      end)
    .&Then('it is the file name of its path, without the extension',
      procedure(W: TTextWorld)
      begin
        Expect(W.Report).ToEqual('CommandLineSpecs');
      end)

.Rule('the first token of a list of tokens may name the root too')

  .Scenario('The name of the root as the first token')
    .Given('a root command called mytool with a positional argument',
      procedure(W: TTextWorld)
      begin
        W.BuildTool;
      end)
    .When('the tokens mytool and file.txt are parsed',
      procedure(W: TTextWorld)
      begin
        W.Parsed := W.Tool.Parse(['mytool', 'file.txt']);
      end)
    .&Then('the argument is file.txt',
      procedure(W: TTextWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.FileArg.ValueIn(W.Parsed)).ToEqual('file.txt');
      end)

  .Scenario('A path whose file name is the root')
    .Given('a root command called mytool with a positional argument',
      procedure(W: TTextWorld)
      begin
        W.BuildTool;
      end)
    .When('the tokens tools/mytool and file.txt are parsed',
      procedure(W: TTextWorld)
      begin
        W.Parsed := W.Tool.Parse(['tools/mytool', 'file.txt']);
      end)
    .&Then('the path names the root, as written, and the argument is file.txt',
      procedure(W: TTextWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.Parsed.RootCommandResult.IdentifierToken.Value).ToEqual('tools/mytool');
        Expect(W.FileArg.ValueIn(W.Parsed)).ToEqual('file.txt');
      end)

  .Scenario('An option whose value ends in the name of the root')
    .Given('a root command called mytool with an option declared with the alias /p',
      procedure(W: TTextWorld)
      begin
        W.BuildTool;
      end)
    .When('the only token is /p:Key=something/mytool',
      procedure(W: TTextWorld)
      begin
        W.Parsed := W.Tool.Parse(['/p:Key=something/mytool']);
      end)
    .&Then('it is the option with its value, not the root',
      procedure(W: TTextWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.PropertyOpt.ValueIn(W.Parsed)).ToEqual('Key=something/mytool');
      end)

.Rule('any command can be parsed as the root of the parse')

  .Scenario('A subcommand parsed on its own')
    .Given('a root with a recursive option --verbose, and a subcommand build with an option -x',
      procedure(W: TTextWorld)
      begin
        W.BuildOuter;
      end)
    .When('build parses --verbose -x 1',
      procedure(W: TTextWorld)
      begin
        W.Parsed := W.Build.Parse(['--verbose', '-x', '1']);
      end)
    .&Then('build is the root of the result, and the recursive option of its parent is reached',
      procedure(W: TTextWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.Parsed.RootCommandResult.Command.Name).ToEqual('build');
        Expect(W.VerboseOpt.ValueIn(W.Parsed)).ToBeTrue;
        Expect(W.XOpt.ValueIn(W.Parsed)).ToEqual('1');
      end);

end.
