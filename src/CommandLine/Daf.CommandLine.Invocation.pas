unit Daf.CommandLine.Invocation;

{
  What settles an invocation, as System.CommandLine settles it: the actions that do not terminate run
  first, all of them, and then the action that won. The code is the last one other than 0 that one of
  the first answered, or else the code of the winner.

  Which action wins is decided by the parse, on the tree it builds, so it lives in
  Daf.CommandLine.Parsing. What lives here is what runs.
}

interface

uses
  Daf.CommandLine;

/// <summary>The action that runs a function: what <c>SetAction</c> declares when it is given one.</summary>
/// <remarks>
///   Terminating, and clearing the parse errors only when asked to, which is what a function in the place
///   of the help or the version needs. <c>nil</c> for no function.
/// </remarks>
function FunctionAction(const Action: TCommandAction; ClearsParseErrors: Boolean = False): ICommandLineAction;

/// <summary>The action of serie of a help option: it writes the help of the command reached, and answers 0.</summary>
function NewHelpAction: IHelpAction;

/// <summary>The action of serie of a version option: it writes the version the root declared, and answers 0.</summary>
function NewVersionAction: IVersionAction;

/// <summary>What the parse picks when it ends with errors and no action takes them.</summary>
function NewParseErrorAction: IParseErrorAction;

/// <summary>
///   The action of serie of a diagram directive: it writes the diagram of the parse, and answers <c>Ok</c>, or the
///   <c>ParseErrorReturnValue</c> of <c>Directive</c> when the parse has errors.
/// </summary>
/// <remarks>It holds the directive weakly, because the directive holds it.</remarks>
function NewDiagramAction(const Directive: IDiagramDirective): ICommandLineAction;

/// <summary>
///   Runs <c>PreActions</c> and then the action of <c>Parsed</c>, and answers the code of the invocation.
/// </summary>
/// <remarks>
///   With the default exception handler on, an exception leaving any of them is written to the error
///   of the invocation, and the code is <c>TCommandLineExit.Failure</c>.
/// </remarks>
function InvokeActions(const Parsed: IParseResult; const PreActions: TArray<ICommandLineAction>): Integer;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  Daf.Application.Version,
  Daf.CommandLine.Help,
  Daf.CommandLine.Resources;

const
  // The furthest a declared name may be from a token for it to be suggested.
  MaxLevenshteinDistance = 3;

type
  TAnonymousSynchronousCommandLineAction = class(TSynchronousCommandLineAction)
  private
    FAction: TCommandAction;
    FClearsParseErrors: Boolean;
  public
    constructor Create(const Action: TCommandAction; AClearsParseErrors: Boolean);
    function ClearsParseErrors: Boolean; override;
    function Invoke(const Parsed: IParseResult): Integer; override;
  end;

  THelpAction = class(TSynchronousCommandLineAction, IHelpAction)
  private
    FMaxWidth: Integer;
  public
    constructor Create;
    function GetMaxWidth: Integer;
    procedure SetMaxWidth(Value: Integer);
    function ClearsParseErrors: Boolean; override;
    function Invoke(const Parsed: IParseResult): Integer; override;
  end;

  TVersionAction = class(TSynchronousCommandLineAction, IVersionAction)
  private
    FIncludeBuildData: Boolean;
  public
    function GetIncludeBuildData: Boolean;
    procedure SetIncludeBuildData(Value: Boolean);
    function ClearsParseErrors: Boolean; override;
    function Invoke(const Parsed: IParseResult): Integer; override;
  end;

  TParseErrorAction = class(TSynchronousCommandLineAction, IParseErrorAction)
  private
    FShowHelp: Boolean;
    FShowTypoCorrections: Boolean;
    procedure WriteTypoCorrectionSuggestions(const Parsed: IParseResult);
    procedure WriteErrorDetails(const Parsed: IParseResult);
    procedure WriteHelp(const Parsed: IParseResult);
  public
    constructor Create;
    function GetShowHelp: Boolean;
    procedure SetShowHelp(Value: Boolean);
    function GetShowTypoCorrections: Boolean;
    procedure SetShowTypoCorrections(Value: Boolean);
    function Invoke(const Parsed: IParseResult): Integer; override;
  end;

  TDiagramAction = class(TSynchronousCommandLineAction)
  private
    [weak] FDirective: IDiagramDirective;
  public
    constructor Create(const Directive: IDiagramDirective);
    function Invoke(const Parsed: IParseResult): Integer; override;
  end;

procedure WriteError(const Configuration: TInvocationConfiguration; const Text: string);
begin
  if Assigned(Configuration.Error) then
    Configuration.Error.WriteLine(Text)
  else
    Writeln(System.ErrOutput, Text);
end;

{ TAnonymousSynchronousCommandLineAction }

constructor TAnonymousSynchronousCommandLineAction.Create(const Action: TCommandAction; AClearsParseErrors: Boolean);
begin
  inherited Create;
  FAction := Action;
  FClearsParseErrors := AClearsParseErrors;
end;

function TAnonymousSynchronousCommandLineAction.ClearsParseErrors: Boolean;
begin
  Result := FClearsParseErrors;
end;

function TAnonymousSynchronousCommandLineAction.Invoke(const Parsed: IParseResult): Integer;
begin
  Result := FAction(Parsed);
end;

{ THelpAction }

constructor THelpAction.Create;
begin
  inherited Create;
  FMaxWidth := MaxInt;
end;

function THelpAction.GetMaxWidth: Integer;
begin
  Result := FMaxWidth;
end;

procedure THelpAction.SetMaxWidth(Value: Integer);
begin
  if Value < 1 then
    raise ECommandLine.CreateFmt('The width the help is wrapped at has to be at least 1, and was given %d', [Value]);
  FMaxWidth := Value;
end;

function THelpAction.ClearsParseErrors: Boolean;
begin
  Result := True;
end;

function THelpAction.Invoke(const Parsed: IParseResult): Integer;
var
  Text: string;
begin
  Text := TCommandLineHelp.Render(Parsed.CommandResult.Command, FMaxWidth);
  // A hidden command has no help: not even an empty line is written for it.
  if Text <> '' then
    Parsed.InvocationConfiguration.WriteOutput(Text);
  Result := TCommandLineExit.Ok;
end;

{ TVersionAction }

function TVersionAction.GetIncludeBuildData: Boolean;
begin
  Result := FIncludeBuildData;
end;

procedure TVersionAction.SetIncludeBuildData(Value: Boolean);
begin
  FIncludeBuildData := Value;
end;

function TVersionAction.ClearsParseErrors: Boolean;
begin
  Result := True;
end;

function TVersionAction.Invoke(const Parsed: IParseResult): Integer;
var
  Root: IRootCommand;
  Version: TVersionInfo;
begin
  // The root's, whichever command was reached: the version belongs to the tool.
  if Supports(Parsed.RootCommandResult.Command, IRootCommand, Root) then
    Version := Root.VersionInfo
  else
    Version := Default(TVersionInfo);
  if FIncludeBuildData then
    Parsed.InvocationConfiguration.WriteOutput(Version.VersionTag(VersionTemplateWithBuild))
  else
    Parsed.InvocationConfiguration.WriteOutput(Version.VersionTag(VersionTemplate));
  Result := TCommandLineExit.Ok;
end;

{ Typo corrections }

// The edit distance between two names, with two rows rather than the whole matrix.
function GetDistance(const First, Second: string): Integer;
var
  N, M, I, J, Current, Next: Integer;
  Rows: array [0 .. 1] of TArray<Integer>;
begin
  N := Length(First);
  M := Length(Second);
  if N = 0 then
    Exit(M);
  if M = 0 then
    Exit(N);
  SetLength(Rows[0], M + 1);
  SetLength(Rows[1], M + 1);
  Current := 0;
  Next := 1;
  for J := 0 to M do
    Rows[Current][J] := J;
  for I := 1 to N do
  begin
    Rows[Next][0] := I;
    for J := 1 to M do
      Rows[Next][J] := Min(Rows[Current][J] + 1, Min(Rows[Next][J - 1] + 1,
        Rows[Current][J - 1] + IfThen(First[I] = Second[J], 0, 1)));
    Current := 1 - Current;
    Next := 1 - Next;
  end;
  Result := Rows[Current][M];
end;

// How many characters the two names share from the start.
function GetStartsWithDistance(const First, Second: string): Integer;
begin
  Result := 0;
  while (Result < Length(First)) and (Result < Length(Second)) and (First[Result + 1] = Second[Result + 1]) do
    Inc(Result);
end;

// Of a symbol's name and aliases, the one closest to the token; the first of them on a tie.
function ClosestName(const Symbol: ISymbol; const Token: string): string;
var
  Alias: string;
  Distance, Best, Prefix, BestPrefix: Integer;
begin
  Result := Symbol.Name;
  Best := GetDistance(Token, Result);
  BestPrefix := GetStartsWithDistance(Token, Result);
  for Alias in Symbol.Aliases do
  begin
    Distance := GetDistance(Token, Alias);
    Prefix := GetStartsWithDistance(Token, Alias);
    if (Distance < Best) or ((Distance = Best) and (Prefix > BestPrefix)) then
    begin
      Result := Alias;
      Best := Distance;
      BestPrefix := Prefix;
    end;
  end;
end;

// The names to suggest for a token: those of the options and subcommands visible on the command reached,
// and of the recursive options visible on its ancestors, at the best distance there is, within the
// maximum -- the longest common prefix first, and the order of declaration on a tie.
function GetPossibleTokens(const CommandResult: ICommandResult; const Token: string): TArray<string>;
var
  Target: ICommand;
  Candidates: TArray<ISymbol>;
  Child, Candidate: ISymbol;
  Option: IOption;
  Node: ISymbolResult;
  Parent: ICommandResult;
  Names: TArray<string>;
  Name: string;
  Best, I, J: Integer;
begin
  Target := CommandResult.Command;
  Candidates := nil;
  if (Length(Target.Options) > 0) or (Length(Target.Subcommands) > 0) then
    for Child in Target.Children do
      if not Child.IsHidden and (Supports(Child, IOption) or Supports(Child, ICommand)) then
        Candidates := Candidates + [Child];
  Node := CommandResult.Parent;
  while Supports(Node, ICommandResult, Parent) do
  begin
    for Option in Parent.Command.Options do
      if Option.IsRecursive and not Option.IsHidden then
        Candidates := Candidates + [ISymbol(Option)];
    Node := Parent.Parent;
  end;

  Best := MaxInt;
  Names := nil;
  for Candidate in Candidates do
  begin
    Name := ClosestName(Candidate, Token);
    I := GetDistance(Token, Name);
    if I > MaxLevenshteinDistance then
      Continue;
    if I < Best then
    begin
      Best := I;
      Names := nil;
    end;
    if I = Best then
      Names := Names + [Name];
  end;

  // Stable, by common prefix, longest first.
  for I := 1 to High(Names) do
  begin
    Name := Names[I];
    J := I - 1;
    while (J >= 0) and (GetStartsWithDistance(Token, Names[J]) < GetStartsWithDistance(Token, Name)) do
    begin
      Names[J + 1] := Names[J];
      Dec(J);
    end;
    Names[J + 1] := Name;
  end;
  Result := Names;
end;

{ TParseErrorAction }

constructor TParseErrorAction.Create;
begin
  inherited Create;
  FShowHelp := True;
  FShowTypoCorrections := True;
end;

function TParseErrorAction.GetShowHelp: Boolean;
begin
  Result := FShowHelp;
end;

procedure TParseErrorAction.SetShowHelp(Value: Boolean);
begin
  FShowHelp := Value;
end;

function TParseErrorAction.GetShowTypoCorrections: Boolean;
begin
  Result := FShowTypoCorrections;
end;

procedure TParseErrorAction.SetShowTypoCorrections(Value: Boolean);
begin
  FShowTypoCorrections := Value;
end;

procedure TParseErrorAction.WriteTypoCorrectionSuggestions(const Parsed: IParseResult);
var
  Unmatched: TArray<string>;
  Token, Suggestion: string;
  First: Boolean;
begin
  Unmatched := Parsed.UnmatchedTokens;
  for Token in Unmatched do
  begin
    First := True;
    for Suggestion in GetPossibleTokens(Parsed.CommandResult, Token) do
    begin
      if First then
      begin
        Parsed.InvocationConfiguration.WriteOutput(Format(STokenNotMatched, [Token]));
        First := False;
      end;
      Parsed.InvocationConfiguration.WriteOutput(Suggestion);
    end;
  end;
  // A blank line closes them, also when no token had anything to suggest.
  if Length(Unmatched) > 0 then
    Parsed.InvocationConfiguration.WriteOutput('');
end;

procedure TParseErrorAction.WriteErrorDetails(const Parsed: IParseResult);
var
  Error: TParseError;
begin
  for Error in Parsed.Errors do
    WriteError(Parsed.InvocationConfiguration, Error.Message);
  WriteError(Parsed.InvocationConfiguration, '');
end;

procedure TParseErrorAction.WriteHelp(const Parsed: IParseResult);
var
  Node: ISymbolResult;
  AtCommand: ICommandResult;
  Option: IOption;
begin
  // The help option nearest to the command reached, walking up: its action writes the help.
  Node := Parsed.CommandResult;
  while Supports(Node, ICommandResult, AtCommand) do
  begin
    for Option in AtCommand.Command.Options do
      if Supports(Option, IHelpOption) then
      begin
        if Option.Action <> nil then
          Option.Action.Invoke(Parsed);
        Exit;
      end;
    Node := AtCommand.Parent;
  end;
end;

function TParseErrorAction.Invoke(const Parsed: IParseResult): Integer;
begin
  if FShowTypoCorrections then
    WriteTypoCorrectionSuggestions(Parsed);
  WriteErrorDetails(Parsed);
  if FShowHelp then
    WriteHelp(Parsed);
  // A parse error answers 2, not the 1 of .NET: 1 is for work that failed.
  Result := TCommandLineExit.InvocationError;
end;

{ TDiagramAction }

constructor TDiagramAction.Create(const Directive: IDiagramDirective);
begin
  inherited Create;
  FDirective := Directive;
end;

function TDiagramAction.Invoke(const Parsed: IParseResult): Integer;
var
  Directive: IDiagramDirective;
begin
  Parsed.InvocationConfiguration.WriteOutput(Parsed.Diagram);
  if Length(Parsed.Errors) = 0 then
    Exit(TCommandLineExit.Ok);
  Directive := FDirective;
  if Directive <> nil then
    Result := Directive.ParseErrorReturnValue
  else
    Result := TCommandLineExit.InvocationError;
end;

function FunctionAction(const Action: TCommandAction; ClearsParseErrors: Boolean): ICommandLineAction;
begin
  if not Assigned(Action) then
    Exit(nil);
  Result := TAnonymousSynchronousCommandLineAction.Create(Action, ClearsParseErrors);
end;

function NewHelpAction: IHelpAction;
begin
  Result := THelpAction.Create;
end;

function NewVersionAction: IVersionAction;
begin
  Result := TVersionAction.Create;
end;

function NewParseErrorAction: IParseErrorAction;
begin
  Result := TParseErrorAction.Create;
end;

function NewDiagramAction(const Directive: IDiagramDirective): ICommandLineAction;
begin
  Result := TDiagramAction.Create(Directive);
end;

function RunActions(const Parsed: IParseResult; const PreActions: TArray<ICommandLineAction>): Integer;
var
  PreAction, Action: ICommandLineAction;
  PreActionCode, Code: Integer;
begin
  PreActionCode := TCommandLineExit.Ok;
  for PreAction in PreActions do
  begin
    Code := PreAction.Invoke(Parsed);
    if Code <> TCommandLineExit.Ok then
      PreActionCode := Code;
  end;

  Action := Parsed.Action;
  if Action = nil then
  begin
    if PreActionCode <> TCommandLineExit.Ok then
      Exit(PreActionCode);
    // Errors with nothing to settle them answer what a parse error answers.
    if Length(Parsed.Errors) > 0 then
      Exit(TCommandLineExit.InvocationError);
    Exit(TCommandLineExit.Ok);
  end;

  Code := Action.Invoke(Parsed);
  if PreActionCode <> TCommandLineExit.Ok then
    Result := PreActionCode
  else
    Result := Code;
end;

function InvokeActions(const Parsed: IParseResult; const PreActions: TArray<ICommandLineAction>): Integer;
var
  Configuration: TInvocationConfiguration;
  Text: string;
begin
  Configuration := Parsed.InvocationConfiguration;
  if not Configuration.EnableDefaultExceptionHandler then
    Exit(RunActions(Parsed, PreActions));

  try
    Result := RunActions(Parsed, PreActions);
  except
    on E: Exception do
    begin
      // The class and the message, and the stack when a provider of stack traces gives one: the
      // nearest Delphi has to the text .NET writes, which always carries it.
      Text := SUnhandledException + E.ClassName + ': ' + E.Message;
      if E.StackTrace <> '' then
        Text := Text + sLineBreak + TrimRight(E.StackTrace);
      WriteError(Configuration, Text);
      Result := TCommandLineExit.Failure;
    end;
  end;
end;

end.
