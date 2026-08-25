program BuildTool;

{
  A two-level command-line grammar, declared once and read back through the very
  objects that declared it.

  The grammar this builds:

      buildtool                        --verbose    recursive: every leaf inherits it
      |
      +-- build   (alias: b)                        only groups: not invocable itself
      |   +-- native <target>          --out        default bin\out.exe
      |   +-- web    <target>          --minify
      |
      +-- serve                        --port       default 8080
                                       --level      Quiet|Normal|Detailed

  `native`, `web` and `serve` are the leaves. `build` only groups, so `buildtool build`
  is an INCOMPLETE invocation rather than an invocation of `build`.

  <target> is REQUIRED without anybody saying so: a string argument gets an arity of
  exactly one, and exactly one is what "required" means at the argument end.

  Run it with no arguments and it walks a set of invocations, two of which fail on
  purpose. Run it with arguments and it parses those instead:

      BuildTool --help
      BuildTool build native app.dpr --out bin\app.exe --verbose
      BuildTool b web site.dpr --minify
      BuildTool serve --port 8080
}

{$APPTYPE CONSOLE}
{$SCOPEDENUMS ON}

uses
  System.SysUtils,
  System.TypInfo,
  Daf.CommandLine;

type
  /// <summary>An enumerated type already names its own values, so nobody repeats them.</summary>
  TVerbosity = (Quiet, Normal, Detailed);

  /// <summary>What to do once an invocation has reached a leaf.</summary>
  TLeafHandler = reference to procedure(const Parsed: ICommandLineParseResult);

  /// <summary>One leaf and the handler that answers for it.</summary>
  TLeafAction = record
    Leaf: ICommand;
    Handler: TLeafHandler;
  end;

  /// <summary>The tool: its grammar, and what it does with a parsed invocation.</summary>
  TBuildTool = class
  private
    // The grammar is held in fields, and that is not tidiness: these objects ARE the
    // keys the values are read back with. Whoever declares an option has to keep it.
    // No name is written twice anywhere in this program.
    FRoot: IRootCommand;
    FBuild: ICommand;
    FNative: ICommand;
    FWeb: ICommand;
    FServe: ICommand;
    FVerbose: IOption<Boolean>;
    FOut: IOption<string>;
    FMinify: IOption<Boolean>;
    FPort: IOption<Integer>;
    FLevel: IOption<TVerbosity>;
    FTarget: IArgument<string>;
    // The module parses and hands back the leaf; DISPATCHING is the application's
    // job, on purpose -- invocation is an explicit non-goal, because a library that
    // runs your handlers ends up owning exit codes, cancellation and middleware too.
    // So the table below is app code, and this is where the frontier sits.
    FActions: TArray<TLeafAction>;
    procedure DeclareGrammar;
    procedure BindActions;
    procedure ShowCommon(const Parsed: ICommandLineParseResult);
    procedure Dispatch(const Parsed: ICommandLineParseResult);
    procedure ReportNative(const Parsed: ICommandLineParseResult);
    procedure ReportWeb(const Parsed: ICommandLineParseResult);
    procedure ReportServe(const Parsed: ICommandLineParseResult);
  public
    constructor Create;
    /// <summary>Parses one invocation and reports it.</summary>
    procedure Run(const Args: TArray<string>);
    /// <summary>Walks a fixed set of invocations, including the ones that fail.</summary>
    procedure Demonstrate;
  end;

constructor TBuildTool.Create;
begin
  inherited;
  DeclareGrammar;
  BindActions;
end;

procedure TBuildTool.DeclareGrammar;
begin
  // Each symbol carries its description here, and nowhere else: the help is rendered
  // from these, so there is no second place that could disagree with them.
  // The one-character aliases are what make `-vm` a bundle: a group is only split when
  // every letter of it names a declared option, so declaring them is what admits the
  // form. Nothing else changes -- `--verbose` still works spelled out.
  FVerbose := CommandLine.Option<Boolean>('--verbose', 'Say more about what is happening')
    .WithAlias('-v').Recursive;
  FOut := CommandLine.Option<string>('--out', 'Where the binary goes').WithDefault('bin\out.exe');
  FMinify := CommandLine.Option<Boolean>('--minify', 'Shrink the produced assets').WithAlias('-m');
  FPort := CommandLine.Option<Integer>('--port', 'Port to listen on').WithDefault(8080);
  // Nothing here lists the accepted values: the enumerated type does it, both in the
  // help and in the error when one is refused.
  FLevel := CommandLine.Option<TVerbosity>('--level', 'How much to say').WithDefault(TVerbosity.Normal);
  FTarget := CommandLine.Argument<string>('target', 'The project to build');

  // Everything below is Add, and no two calls add the same KIND of thing. Worth
  // reading one by one, because the shape of the grammar comes entirely from which
  // kind goes where.

  // A LEAF: a positional argument plus its own options. The same FTarget object
  // serves both leaves -- only the leaf actually reached ever binds a value to it.
  FNative := CommandLine.Command('native', 'Build a native binary').Add([FTarget, FOut]);
  FWeb := CommandLine.Command('web', 'Build for the browser').Add([FTarget, FMinify]);
  FServe := CommandLine.Command('serve', 'Serve what was built').Add([FPort, FLevel]);

  // A GROUP: here Add takes SUBCOMMANDS, and that is what makes `build` a group.
  // From this line on, `buildtool build` is incomplete and must name native or web.
  FBuild := CommandLine.Command('build', 'Build things').WithAlias('b').Add([FNative, FWeb]);

  // THE ROOT, mixing both kinds: one recursive option and two subcommands. Because
  // FVerbose is recursive, adding it once here is what puts it on every leaf.
  // The version lives on the ROOT, as in .NET: it is the tool's, not each order's.
  FRoot := CommandLine.Root('buildtool', 'A small tool that builds and serves things').WithVersion('1.0.0');
  FRoot.Add([FVerbose, FBuild, FServe]);
end;

// The leaves are keyed by the command OBJECT, not by its name. Comparing
// Parsed.Command.Name against 'native' would put a quoted name back in the middle of
// the program -- the very mistake this module exists to remove.
procedure TBuildTool.BindActions;

  function OnLeaf(const Leaf: ICommand; const Handler: TLeafHandler): TLeafAction;
  begin
    Result.Leaf := Leaf;
    Result.Handler := Handler;
  end;

begin
  FActions := [
    OnLeaf(FNative, ReportNative),
    OnLeaf(FWeb, ReportWeb),
    OnLeaf(FServe, ReportServe)];
end;

procedure TBuildTool.Dispatch(const Parsed: ICommandLineParseResult);
var
  Action: TLeafAction;
begin
  for Action in FActions do
    if Parsed.Command = Action.Leaf then
    begin
      Action.Handler(Parsed);
      Exit;
    end;
end;

// What every leaf has in common, including the option nobody declared on it.
procedure TBuildTool.ShowCommon(const Parsed: ICommandLineParseResult);
begin
  Writeln('  command : ', Parsed.Command.FullPath);

  // FVerbose was declared once, on the root, and is read here from whatever leaf the
  // invocation reached. That is what Recursive buys.
  Writeln('  verbose : ', BoolToStr(FVerbose.ValueIn(Parsed), True));

  // WasProvided tells "the invocation did not carry it" from "it carried an empty
  // value", which are different answers.
  if Parsed.WasProvided(FTarget) then
    Writeln('  target  : ', FTarget.ValueIn(Parsed));
end;

procedure TBuildTool.ReportNative(const Parsed: ICommandLineParseResult);
begin
  // A string, statically. Reading it into an Integer would not compile.
  Writeln('  out     : ', FOut.ValueIn(Parsed));
end;

procedure TBuildTool.ReportWeb(const Parsed: ICommandLineParseResult);
begin
  Writeln('  minify  : ', BoolToStr(FMinify.ValueIn(Parsed), True));
end;

procedure TBuildTool.ReportServe(const Parsed: ICommandLineParseResult);
begin
  // An Integer, statically: it goes straight into arithmetic, with no conversion.
  Writeln('  port    : ', FPort.ValueIn(Parsed) + 0);
  // And an enumerated value, also statically typed.
  Writeln('  level   : ', GetEnumName(TypeInfo(TVerbosity), Ord(FLevel.ValueIn(Parsed))));
end;

procedure TBuildTool.Run(const Args: TArray<string>);
var
  Parsed: ICommandLineParseResult;
begin
  Writeln('$ buildtool ', string.Join(' ', Args));

  Parsed := FRoot.Parse(Args);

  // Help is answered before anything is judged: asking how a tool works is an
  // outcome, not a mistake, so it is not reported as a parse error.
  if Parsed.HelpRequested then
  begin
    Writeln(Parsed.HelpText);
    Exit;
  end;

  if Parsed.VersionRequested then
  begin
    Writeln('  ', Parsed.VersionText);
    Writeln;
    Exit;
  end;

  if not Parsed.Succeeded then
  begin
    Writeln('  ! ', Parsed.ErrorText);
    Writeln;
    Exit;
  end;

  ShowCommon(Parsed);
  Dispatch(Parsed);
  Writeln;
end;

procedure TBuildTool.Demonstrate;
begin
  Run(['build', 'native', 'app.dpr', '--out', 'bin\app.exe']);
  // The alias reaches the same command, and the recursive option is read from the leaf.
  Run(['b', 'web', 'site.dpr', '--minify', '--verbose']);
  Run(['serve', '--port', '9090']);
  // A command that only groups is not an invocation of its own.
  Run(['build']);
  // The value engine refuses a number with something left over instead of inventing
  // one out of it: this is NOT read as 80.
  Run(['serve', '--port', '80xyz']);
  // <target> is required by its arity: leaving it out is an incomplete invocation.
  Run(['build', 'native']);
  // The enumerated type refuses what is not one of its values, and says which are.
  Run(['serve', '--level', 'shouting']);
  // The same invocation, written the three other ways the grammar admits. None of them
  // says anything the forms above do not: that is the point of admitting them.
  Run(['serve', '--port:9090']);
  Run(['serve', '/port', '80']);
  // A bundle: -v and -m are declared one-character aliases, so this is `-v -m`.
  Run(['b', 'web', 'site.dpr', '-vm']);
  // Neither --help nor --version was declared anywhere. Both answer.
  Run(['--version']);
  Run(['--help']);
end;

function ArgsFromCommandLine: TArray<string>;
var
  I: Integer;
begin
  SetLength(Result, ParamCount);
  for I := 1 to ParamCount do
    Result[I - 1] := ParamStr(I);
end;

var
  Tool: TBuildTool;

begin
  ReportMemoryLeaksOnShutdown := True;
  try
    Tool := TBuildTool.Create;
    try
      if ParamCount = 0 then
        Tool.Demonstrate
      else
        Tool.Run(ArgsFromCommandLine);
    finally
      Tool.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
