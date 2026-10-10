program BuildTool;

{
  A two-level command-line grammar, declared once and read back through the very
  objects that declared it.

  The grammar this builds:

      buildtool                        --verbose    recursive: every leaf inherits it
      |                                [diagram]    writes how an invocation was understood
      +-- build   (alias: b)                        only groups: not invocable itself
      |   +-- native <target>          --out        default bin\out.exe
      |   |                            --min-sdk    a type of this program's own
      |   +-- web    <target>          --minify     never together with --pretty
      |                                --pretty
      |
      +-- serve                        --port       default 8080, from 1 to 65535
                                       --level      Quiet|Normal|Detailed

  `native`, `web` and `serve` are the leaves. `build` only groups, so `buildtool build`
  is an INCOMPLETE invocation rather than an invocation of `build`.

  Each leaf declares its action, and Invoke runs the one of the leaf reached and answers
  the code the process ends with. Nothing in this program asks which command it got.

  The help writes Synopsis: where System.CommandLine writes Usage:, a text this program
  assigns as its own.

  <target> is REQUIRED without anybody saying so: a string argument gets an arity of
  exactly one, and exactly one is what "required" means at the argument end.

  Run it with no arguments and it walks a set of invocations, several of which fail on
  purpose. Run it with arguments and it parses those instead:

      BuildTool --help
      BuildTool build native app.dpr --out bin\app.exe --verbose
      BuildTool b web site.dpr --minify
      BuildTool serve --port 8080
      BuildTool [diagram] b native app.dpr -v
}

{$APPTYPE CONSOLE}
{$SCOPEDENUMS ON}

uses
  System.SysUtils,
  System.Classes,
  System.TypInfo,
  Daf.CommandLine;

type
  /// <summary>An enumerated type already names its own values, so nobody repeats them.</summary>
  TVerbosity = (Quiet, Normal, Detailed);

  /// <summary>
  ///   A type the engine knows nothing about. It is here to show the way out: the
  ///   conversion travels with the option, so nothing in DAF had to change to admit it.
  /// </summary>
  TSemVer = record
    Major: Integer;
    Minor: Integer;
    Patch: Integer;
    class function TryParse(const Raw: string; out Value: TSemVer): Boolean; static;
    function ToString: string;
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
    FPretty: IOption<Boolean>;
    FPort: IOption<Integer>;
    FLevel: IOption<TVerbosity>;
    FMinSdk: IOption<TSemVer>;
    FTarget: IArgument<string>;
    procedure DeclareGrammar;
    procedure DeclareActions;
    procedure RefuseMinifyWithPretty(const Result: ICommandResult);
    procedure ShowCommon(const Parsed: IParseResult);
    procedure ReportNative(const Parsed: IParseResult);
    procedure ReportWeb(const Parsed: IParseResult);
    procedure ReportServe(const Parsed: IParseResult);
    /// <summary>One step of the walkthrough: the invocation, what it wrote, and its exit code.</summary>
    function Walk(const Args: TArray<string>): Integer;
  public
    constructor Create;
    /// <summary>Parses one invocation, invokes it, and answers its exit code.</summary>
    function Run(const Args: TArray<string>): Integer;
    /// <summary>Walks a fixed set of invocations, including the ones that fail.</summary>
    procedure Demonstrate;
  end;

class function TSemVer.TryParse(const Raw: string; out Value: TSemVer): Boolean;
var
  Parts: TArray<string>;
begin
  Parts := Raw.Split(['.']);
  Result := (Length(Parts) = 3) and TryStrToInt(Parts[0], Value.Major) and
    TryStrToInt(Parts[1], Value.Minor) and TryStrToInt(Parts[2], Value.Patch);
end;

function TSemVer.ToString: string;
begin
  Result := Format('%d.%d.%d', [Major, Minor, Patch]);
end;

constructor TBuildTool.Create;
begin
  inherited;
  DeclareGrammar;
  DeclareActions;
end;

procedure TBuildTool.DeclareGrammar;
begin
  // Each symbol carries its description here, and nowhere else: the help is rendered
  // from these, so there is no second place that could disagree with them.
  // The one-character aliases are what make `-vm` a bundle: each letter of it names a
  // declared option, so it reads as `-v -m`. Nothing else changes -- `--verbose` still
  // works spelled out.
  FVerbose := CommandLine.Option<Boolean>('--verbose', ['-v']).Description('Say more about what is happening').Recursive;
  FOut := CommandLine.Option<string>('--out').Description('Where the binary goes').DefaultValue('bin\out.exe');
  FMinify := CommandLine.Option<Boolean>('--minify', ['-m']).Description('Shrink the produced assets');
  FPretty := CommandLine.Option<Boolean>('--pretty').Description('Keep the produced assets readable');
  // A range is a validation and not a mechanism of its own, which is also how
  // System.CommandLine answers one. It receives the value already converted.
  FPort := CommandLine.Option<Integer>('--port').Description('Port to listen on').DefaultValue(8080)
    .Validate(
    function(const Value: Integer; out Error: string): Boolean
    begin
      Error := '';
      Result := (Value >= 1) and (Value <= 65535);
      if not Result then
        Error := Format('a port goes from 1 to 65535, and %d does not', [Value]);
    end);
  // Nothing here lists the accepted values: the enumerated type does it, both in the
  // help and in the error when one is refused.
  FLevel := CommandLine.Option<TVerbosity>('--level').Description('How much to say').DefaultValue(TVerbosity.Normal);
  // A type the engine does not know, made usable in ONE place: the conversion is passed
  // where the option is declared. Without one, adding the option to a command is refused.
  FMinSdk := CommandLine.Option<TSemVer>('--min-sdk',
    function(const Raw: string; out Value: TSemVer; out Error: string): Boolean
    begin
      Error := '';
      Result := TSemVer.TryParse(Raw, Value);
      if not Result then
        Error := Format('"%s" is not a version: write it as major.minor.patch', [Raw]);
    end).Description('Lowest SDK this build targets');
  FTarget := CommandLine.Argument<string>('target').Description('The project to build');

  // Everything below is Add, and no two calls add the same KIND of thing. Worth
  // reading one by one, because the shape of the grammar comes entirely from which
  // kind goes where.

  // A LEAF: a positional argument plus its own options. The same FTarget object
  // serves both leaves -- only the leaf actually reached ever binds a value to it.
  FNative := CommandLine.Command('native', 'Build a native binary').Add([FTarget, FOut, FMinSdk]);
  FWeb := CommandLine.Command('web', 'Build for the browser').Add([FTarget, FMinify, FPretty]);
  // A rule about the invocation as a whole, which no option states on its own. It runs
  // when `web` is the command reached, before its options are validated.
  FWeb.Validators.Add(RefuseMinifyWithPretty);
  FServe := CommandLine.Command('serve', 'Serve what was built').Add([FPort, FLevel]);

  // A GROUP: here Add takes SUBCOMMANDS, and that is what makes `build` a group.
  // From this line on, `buildtool build` is incomplete and must name native or web.
  FBuild := CommandLine.Command('build', 'Build things').Add([FNative, FWeb]);
  FBuild.Aliases.Add('b');

  // THE ROOT, mixing both kinds: one recursive option and two subcommands. Because
  // FVerbose is recursive, adding it once here is what puts it on every leaf.
  // The version lives on the ROOT, as in .NET: it is the tool's, not each order's.
  FRoot := CommandLine.Root('buildtool', 'A small tool that builds and serves things').WithVersion('1.0.0');
  FRoot.Add([FVerbose, FBuild, FServe]);
  // [diagram] writes how an invocation was understood, and runs nothing. Only a root
  // declares directives.
  FRoot.Add(CommandLine.DiagramDirective);
end;

// Each action is a method of this class, with no code of its own: the invocation answers 0
// for it. `build` gets none, and that is what keeps it a group.
procedure TBuildTool.DeclareActions;
begin
  FNative.SetAction(ReportNative);
  FWeb.SetAction(ReportWeb);
  FServe.SetAction(ReportServe);
end;

procedure TBuildTool.RefuseMinifyWithPretty(const Result: ICommandResult);
begin
  if (Result.GetResult(FMinify) <> nil) and (Result.GetResult(FPretty) <> nil) then
    Result.AddError('--minify and --pretty cannot be used together');
end;

// What every leaf has in common, including the option nobody declared on it.
procedure TBuildTool.ShowCommon(const Parsed: IParseResult);
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

procedure TBuildTool.ReportNative(const Parsed: IParseResult);
begin
  ShowCommon(Parsed);
  // A string, statically. Reading it into an Integer would not compile.
  Writeln('  out     : ', FOut.ValueIn(Parsed));
  // And a type of this program's own, read as that type: no TValue, no cast, no name
  // written twice. What the declared conversion produced is what comes back.
  if Parsed.WasProvided(FMinSdk) then
    Writeln('  min-sdk : ', FMinSdk.ValueIn(Parsed).ToString);
end;

procedure TBuildTool.ReportWeb(const Parsed: IParseResult);
begin
  ShowCommon(Parsed);
  Writeln('  minify  : ', BoolToStr(FMinify.ValueIn(Parsed), True));
end;

procedure TBuildTool.ReportServe(const Parsed: IParseResult);
begin
  ShowCommon(Parsed);
  // An Integer, statically: it goes straight into arithmetic, with no conversion.
  Writeln('  port    : ', FPort.ValueIn(Parsed) + 0);
  // And an enumerated value, also statically typed.
  Writeln('  level   : ', GetEnumName(TypeInfo(TVerbosity), Ord(FLevel.ValueIn(Parsed))));
end;

function TBuildTool.Run(const Args: TArray<string>): Integer;
begin
  // Help and version are written and answer 0, and otherwise the action of the leaf
  // reached runs. A parse error writes what was probably meant, the errors to the error
  // output and the help of the command reached, and answers 2. Asking for help clears
  // the errors of the rest: asking how a tool works is an outcome, not a mistake.
  Result := FRoot.Parse(Args).Invoke;
end;

function TBuildTool.Walk(const Args: TArray<string>): Integer;
var
  Configuration: TInvocationConfiguration;
  Errors: TStringWriter;
  Parsed: IParseResult;
  OnError: IParseErrorAction;
begin
  Writeln('$ buildtool ', string.Join(' ', Args));
  // The walkthrough reads top to bottom, so a parse error is written in its place, after
  // the invocation that caused it, and not on the console's error output. It is about the
  // errors, so the help that follows each one is left out.
  Errors := TStringWriter.Create;
  try
    Configuration.Error := Errors;
    Parsed := FRoot.Parse(Args);
    if Supports(Parsed.Action, IParseErrorAction, OnError) then
      OnError.ShowHelp := False;
    Result := Parsed.Invoke(Configuration);
    if Errors.ToString <> '' then
      Writeln('  ! ', Errors.ToString.TrimRight);
  finally
    Errors.Free;
  end;
  Writeln('  exit    : ', Result);
  Writeln;
end;

procedure TBuildTool.Demonstrate;
begin
  Walk(['build', 'native', 'app.dpr', '--out', 'bin\app.exe']);
  // The alias reaches the same command, and the recursive option is read from the leaf.
  Walk(['b', 'web', 'site.dpr', '--minify', '--verbose']);
  Walk(['serve', '--port', '9090']);
  // A command that only groups is not an invocation of its own.
  Walk(['build']);
  // The value engine refuses a number with something left over instead of inventing
  // one out of it: this is NOT read as 80.
  Walk(['serve', '--port', '80xyz']);
  // A number the type admits and the application does not. The validation's refusal
  // comes out through Errors, like every other invalid invocation.
  Walk(['serve', '--port', '70000']);
  // <target> is required by its arity: leaving it out is an incomplete invocation.
  Walk(['build', 'native']);
  // The enumerated type refuses what is not one of its values, and says which are.
  Walk(['serve', '--level', 'shouting']);
  // A type of this program's own, read through the conversion declared beside it.
  Walk(['build', 'native', 'app.dpr', '--min-sdk', '11.4.2']);
  // And refused by that same conversion, with the message the conversion wrote. The
  // refusal comes out through Errors, like every other invalid invocation.
  Walk(['build', 'native', 'app.dpr', '--min-sdk', '11.4']);
  // Each option is valid on its own, and the validation of the command refuses the two.
  Walk(['b', 'web', 'site.dpr', '--minify', '--pretty']);
  // The same invocations, written the other ways the grammar admits. None of them says
  // anything the forms above do not.
  Walk(['serve', '--port:9090']);
  Walk(['serve', '--port=9090']);
  // A bundle: -v and -m are declared one-character aliases, so this is `-v -m`.
  Walk(['b', 'web', 'site.dpr', '-vm']);
  // How an invocation was understood, and no action runs: the alias, the recursive option
  // and the default --out took, marked with *.
  Walk(['[diagram]', 'b', 'native', 'app.dpr', '-v']);
  // Neither --help nor --version was declared anywhere. Both answer.
  Walk(['--version']);
  Walk(['--help']);
end;

// The texts are a setting of the process, assigned before the grammar is declared. This
// program writes its own title for the usage; every other text stays .NET's.
procedure UseOwnTexts;
var
  Texts: TCommandLineTexts;
begin
  Texts[TCommandLineText.HelpUsageTitle] := 'Synopsis:';
  CommandLine.Texts := Texts;
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
    UseOwnTexts;
    Tool := TBuildTool.Create;
    try
      if ParamCount = 0 then
        Tool.Demonstrate
      else
        ExitCode := Tool.Run(ArgsFromCommandLine);
    finally
      Tool.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
