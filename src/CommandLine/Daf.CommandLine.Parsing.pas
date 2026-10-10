unit Daf.CommandLine.Parsing;

{
  The parse, in the three steps System.CommandLine takes. The invocation is read into typed tokens;
  the tokens build a tree with one result per symbol the invocation reached; and the tree is
  validated -- the innermost command whole, its parents only in their recursive options, in the ones
  with a default and in the version. Building and validating the tree is also where the action the
  invocation runs is picked, and the errors it clears are dropped.

  Reading follows the rules of DAF: a group of one-letter options, a value glued to
  its option, and an option that takes the next token whatever it is.

  Published here are the parse, the reading of values, and the refusal of a name that the help or the
  version keeps. The contract the results answer to lives in Daf.CommandLine, because interfaces that
  name each other have to share one interface section.
}

interface

uses
  System.Rtti,
  Daf.CommandLine;

/// <summary>Parses an invocation against the grammar of <c>Root</c>, the command the parse starts from.</summary>
/// <remarks><c>FromCommandLine</c> says the invocation was one line, where the path of the executable also names the root.</remarks>
function ParseInvocation(const Root: ICommand; const Args: TArray<string>; const Configuration: TParserConfiguration;
  FromCommandLine: Boolean): IParseResult;

/// <summary>The values of an option's or an argument's result.</summary>
/// <remarks>
///   The ones the validation converted, or the default it took: then <c>FromDefault</c> is True and the
///   one value is the whole default. <c>Whole</c> is True for that one value, and for the one a conversion of
///   its own answered. A result the validation never reached -- an option of a parent
///   that is neither recursive nor defaulted -- is converted now, once, without the validations, and its
///   error joins the tree as the validation's would.
///
///   Raises <c>ECommandLine</c> with the error of a result that has one.
/// </remarks>
function ResultValues(const Found: ISymbolResult; out FromDefault, Whole: Boolean): TArray<TValue>;

/// <summary>
///   Raises <c>ECommandLine</c> when an option of the application takes a name of the help or of the version on
///   a command where they answer, from <c>Root</c> down.
/// </summary>
/// <remarks>
///   Those names always reach the help or the version, so such an option could never answer. With
///   <c>IgnoreCase</c>, a name that differs from theirs only in case takes it too.
/// </remarks>
procedure DemandReservedNamesFree(const Root: ICommand; IgnoreCase: Boolean);

/// <summary>The same refusal, for <c>Option</c> before it is added to <c>Command</c>.</summary>
procedure DemandOptionAddable(const Command: ICommand; const Option: IOption);

/// <summary>The same refusal, for <c>Subcommand</c> and what hangs from it before it is added to <c>Command</c>.</summary>
procedure DemandSubcommandAddable(const Command, Subcommand: ICommand);

implementation

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Generics.Defaults,
  Daf.Nullable,
  Daf.Application.Version,
  Daf.CommandLine.Tokens,
  Daf.CommandLine.Values,
  Daf.CommandLine.Invocation,
  Daf.CommandLine.Resources;

type
  /// <summary>Every result of one parse, by symbol, and the errors and leftovers of that parse.</summary>
  ISymbolResultTree = interface
    ['{7C4B1A20-0010-4F31-9C10-4B0E2D5A0010}']
    function Find(const Symbol: TObject): ISymbolResult;
    procedure Add(const Symbol: TObject; const Node: ISymbolResult);
    function Results: TArray<ISymbolResult>;
    procedure AddError(const Message: string; const Node: ISymbolResult);
    procedure InsertFirstError(const Message: string; const Node: ISymbolResult);
    function Errors: TArray<TParseError>;
    /// <summary>Replaces the errors, which is how the action that wins discards the ones it clears.</summary>
    procedure SetErrors(const Value: TArray<TParseError>);
    function ErrorCount: Integer;
    procedure AddUnmatched(const Token: string);
    function Unmatched: TArray<string>;
  end;

  TSymbolResultTree = class(TInterfacedObject, ISymbolResultTree)
  private
    FSymbols: TArray<TObject>;
    FResults: TArray<ISymbolResult>;
    FErrors: TArray<TParseError>;
    FUnmatched: TArray<string>;
  public
    function Find(const Symbol: TObject): ISymbolResult;
    procedure Add(const Symbol: TObject; const Node: ISymbolResult);
    function Results: TArray<ISymbolResult>;
    procedure AddError(const Message: string; const Node: ISymbolResult);
    procedure InsertFirstError(const Message: string; const Node: ISymbolResult);
    function Errors: TArray<TParseError>;
    procedure SetErrors(const Value: TArray<TParseError>);
    function ErrorCount: Integer;
    procedure AddUnmatched(const Token: string);
    function Unmatched: TArray<string>;
  end;

  TSymbolResult = class(TInterfacedObject, ISymbolResult)
  private
    // Both weak: the tree holds its results, and a result holding the tree or its parent would be
    // a cycle between counted references.
    [weak] FTree: ISymbolResultTree;
    [weak] FParent: ISymbolResult;
    FTokens: TArray<TToken>;
  public
    constructor Create(const ATree: ISymbolResultTree; const AParent: ISymbolResult);
    function Parent: ISymbolResult;
    function Tokens: TArray<TToken>;
    function Errors: TArray<TParseError>;
    procedure AddError(const Message: string); virtual;
    function GetResult(const Option: IOption): IOptionResult; overload;
    function GetResult(const Argument: IArgument): IArgumentResult; overload;
    function GetResult(const Command: ICommand): ICommandResult; overload;
    function GetResult(const Directive: IDirective): IDirectiveResult; overload;
    procedure AddToken(const Token: TToken);
  end;

  TCommandResult = class(TSymbolResult, ICommandResult)
  private
    FCommand: ICommand;
    FIdentifierToken: TToken;
  public
    constructor Create(const ATree: ISymbolResultTree; const AParent: ISymbolResult; const ACommand: ICommand;
      const AIdentifierToken: TToken);
    function Command: ICommand;
    function IdentifierToken: TToken;
    function Children: TArray<ISymbolResult>;
  end;

  {$SCOPEDENUMS ON}
  TConversionState = (Pending, Converted, Failed);
  {$SCOPEDENUMS OFF}

  /// <summary>What an option's result and an argument's share: the values, converted once.</summary>
  TValuedResult = class(TSymbolResult)
  private
    FValued: TValuedSymbol;
    FValues: TArray<TValue>;
    FFromDefault: Boolean;
    FWhole: Boolean;
    FState: TConversionState;
    FFailure: string;
    FValidated: Boolean;
    procedure ConvertTokens;
  protected
    /// <summary>Why its tokens are not the number it admits; empty when they are.</summary>
    function ArityError: string; virtual; abstract;
    function IsImplicitResult: Boolean; virtual; abstract;
  public
    /// <summary>The result of the argument its value is carried in: itself, or the one of an option.</summary>
    function ValueResult: IArgumentResult; virtual; abstract;
    constructor Create(const ATree: ISymbolResultTree; const AParent: ISymbolResult; AValued: TValuedSymbol);
    /// <summary>An error of the parse that also stands for its value: reading it raises this.</summary>
    procedure Fail(const Message: string);
    /// <summary>
    ///   Its arity, the first step of its validation: False, with the error added, when its tokens are not the
    ///   number it admits. A result that took its default has nothing to count.
    /// </summary>
    function CheckArity: Boolean;
    /// <summary>
    ///   Its turn in the validation: the default when it is implicit; otherwise its arity, then each
    ///   token through the accepted values, the conversion and the validations. Runs once.
    /// </summary>
    procedure Validate;
    /// <summary>
    ///   Converts it now when nothing has yet: the default when it is implicit, else its tokens without the
    ///   validations. A failure joins the tree as an error, and nothing is raised.
    /// </summary>
    procedure EnsureConverted;
    function ReadValues(out FromDefault, Whole: Boolean): TArray<TValue>;
    /// <summary>
    ///   Its values as the diagram writes them, each between <c>&lt;</c> and <c>&gt;</c>: the tokens as written
    ///   when the conversion failed, and nothing for a symbol of one value that received none.
    /// </summary>
    function DiagramValues: string;
  end;

  TOptionResult = class(TValuedResult, IOptionResult)
  private
    FOption: IOption;
    FIdentifierToken: TNullable<TToken>;
    FIdentifierTokenCount: Integer;
  protected
    function ArityError: string; override;
    function IsImplicitResult: Boolean; override;
  public
    constructor Create(const ATree: ISymbolResultTree; const AParent: ISymbolResult; const AOption: IOption);
    function ValueResult: IArgumentResult; override;
    /// <summary>The invocation named it once more.</summary>
    procedure Named(const Token: TToken);
    function Option: IOption;
    function IsImplicit: Boolean;
    function IdentifierToken: TNullable<TToken>;
    function IdentifierTokenCount: Integer;
  end;

  TArgumentResult = class(TValuedResult, IArgumentResult)
  private
    FArgument: IArgument;
    FImplicit: Boolean;
    FOnlyTaken: Boolean;
    function ArgumentLimitReached: Boolean;
  protected
    function ArityError: string; override;
    function IsImplicitResult: Boolean; override;
  public
    constructor Create(const ATree: ISymbolResultTree; const AParent: ISymbolResult; const AArgument: IArgument);
    function ValueResult: IArgumentResult; override;
    procedure OnlyTake(NumberOfTokens: Integer);
    /// <summary>The validation created it for its default.</summary>
    procedure TakeDefault;
    /// <summary>An error of an argument is an error of its value too, as in .NET.</summary>
    procedure AddError(const Message: string); override;
    function Argument: IArgument;
    function IsImplicit: Boolean;
  end;

  /// <summary>
  ///   The result of the argument an option carries its value in, as .NET keeps under each option result:
  ///   what the validations of the value receive. It is the option's result seen as an argument's -- its
  ///   tokens, its parent -- and an error added to it fails the option's conversion.
  /// </summary>
  /// <remarks>
  ///   Built when it is needed and not kept in the tree: <c>GetResult</c> does not find it, as .NET does not
  ///   find an option's internal argument.
  /// </remarks>
  TOptionArgumentResult = class(TInterfacedObject, ISymbolResult, IArgumentResult)
  private
    FOption: TOptionResult;
    // Holds the option's result alive while the view is used.
    FOptionRef: IOptionResult;
  public
    constructor Create(AOption: TOptionResult);
    function Parent: ISymbolResult;
    function Tokens: TArray<TToken>;
    function Errors: TArray<TParseError>;
    procedure AddError(const Message: string);
    function GetResult(const Option: IOption): IOptionResult; overload;
    function GetResult(const Argument: IArgument): IArgumentResult; overload;
    function GetResult(const Command: ICommand): ICommandResult; overload;
    function GetResult(const Directive: IDirective): IDirectiveResult; overload;
    function Argument: IArgument;
    function IsImplicit: Boolean;
    procedure OnlyTake(NumberOfTokens: Integer);
  end;

  TDirectiveResult = class(TSymbolResult, IDirectiveResult)
  private
    FDirective: IDirective;
    FToken: TToken;
    FValues: TArray<string>;
  public
    constructor Create(const ATree: ISymbolResultTree; const ADirective: IDirective; const AToken: TToken);
    procedure AddValue(const Value: string);
    function Directive: IDirective;
    function Token: TToken;
    function Values: TArray<string>;
  end;

  TParseResult = class(TInterfacedObject, IParseResult)
  private
    FRoot: ICommand;
    FConfiguration: TParserConfiguration;
    FTree: ISymbolResultTree;
    FRootResult: ICommandResult;
    FCommandResult: ICommandResult;
    FTokens: TArray<TToken>;
    FErrorsShared: Boolean;
    FAction: ICommandLineAction;
    FActionOwner: IOption;
    FPreActions: TArray<ICommandLineAction>;
    FInvocationConfiguration: TInvocationConfiguration;
    function ResultOf(const Symbol: ISymbol): ISymbolResult;
  public
    constructor Create(const ARoot: ICommand; const AConfiguration: TParserConfiguration;
      const ATree: ISymbolResultTree; const ARootResult: ICommandResult);
    procedure Close(const Innermost: ICommandResult; const Tokens: TArray<TToken>; const Action: ICommandLineAction;
      const ActionOwner: IOption; const PreActions: TArray<ICommandLineAction>);
    function Command: ICommand;
    function CommandResult: ICommandResult;
    function RootCommandResult: ICommandResult;
    function Tokens: TArray<TToken>;
    function GetResult(const Option: IOption): IOptionResult; overload;
    function GetResult(const Argument: IArgument): IArgumentResult; overload;
    function GetResult(const Command: ICommand): ICommandResult; overload;
    function GetResult(const Directive: IDirective): IDirectiveResult; overload;
    function Succeeded: Boolean;
    function Errors: TArray<TParseError>;
    function ErrorText: string;
    function IsHelpRequested: Boolean;
    function HelpText: string;
    function IsVersionRequested: Boolean;
    function VersionInfo: TVersionInfo;
    function WasProvided(const Symbol: ISymbol): Boolean;
    function RawOf(const Symbol: ISymbol): TNullable<string>;
    function RawValuesOf(const Symbol: ISymbol): TArray<string>;
    function UnmatchedTokens: TArray<string>;
    function Action: ICommandLineAction;
    function Diagram: string;
    function Configuration: TParserConfiguration;
    function InvocationConfiguration: TInvocationConfiguration;
    function Invoke: Integer; overload;
    function Invoke(const Configuration: TInvocationConfiguration): Integer; overload;
  end;

  /// <summary>The grammar in scope, answering the two questions the rewriting asks.</summary>
  /// <remarks>
  ///   It is mutable and reused as the reading descends, because which options are
  ///   declared depends on the command reached: `mytool build native -fdx` can only be
  ///   split once `native` is where we are.
  /// </remarks>
  TScopeKnowledge = class(TInterfacedObject, IKnownTokens)
  private
    FCurrent: ICommand;
    FAvailable: TArray<IOption>;
    FIgnoreCase: Boolean;
  public
    constructor Create(IgnoreCase: Boolean);
    procedure Enter(const Command: ICommand; const Available: TArray<IOption>);
    function Knows(const Token: string): Boolean;
    function ShortTakesValue(const Letter: Char): Boolean;
  end;

  {$SCOPEDENUMS ON}
  /// <summary>What reading made of a token, finer than its token type.</summary>
  TItemKind = (Positional, CommandName, OptionName, OptionValue, DoubleDash, UnknownOption, BadGroup, Directive);
  {$SCOPEDENUMS OFF}

  TItem = record
    Token: TToken;
    Kind: TItemKind;
    /// <summary>The name of an unknown option, without the value glued to it.</summary>
    Name: string;
    /// <summary>Why a group of letters could not be split.</summary>
    Error: string;
  end;

  TParseOperation = class
  private
    FRoot: ICommand;
    FConfiguration: TParserConfiguration;
    FItems: TArray<TItem>;
    FIndex: Integer;
    FTree: ISymbolResultTree;
    FRootResult: ICommandResult;
    FInnermost: ICommandResult;
    FArgumentIndex: Integer;
    FArgumentCount: Integer;
    FAction: ICommandLineAction;
    FActionOwner: IOption;
    FPreActions: TArray<ICommandLineAction>;
    FCapturing: Boolean;
    /// <summary>A directive with a terminating action was written: no option's action joins the invocation.</summary>
    FDirectiveWins: Boolean;
    procedure Unmatched(const Token, Message: string);
    function IsCapturingRemainingTokens(Kind: TItemKind): Boolean;
    procedure ParseDirective;
    procedure ParseSubcommand;
    procedure ParseOption;
    procedure ParseArgument;
    procedure ValidateCommand(const Node: ICommandResult; Complete: Boolean);
    procedure ValidateAndAddDefaultResults;
    procedure KeepErrorsOf(const Owner: IOption);
    procedure DropErrorsOf(const Node: ICommandResult);
    procedure AddPreActionsForImplicitOptions;
  public
    constructor Create(const Root: ICommand; const Configuration: TParserConfiguration; const RootTokenValue: string;
      const Items: TArray<TItem>; const ReadErrors: TArray<string>);
    function Run: IParseResult;
  end;

// True for the help and the version options a root brings with it.
function IsStandardOption(const Option: IOption): Boolean;
begin
  Result := Supports(Option, IHelpOption) or Supports(Option, IVersionOption);
end;

function SameNameIgnoringCase(const A, B: string): Boolean;
var
  Comparer: IEqualityComparer<string>;
begin
  Comparer := TIStringComparer.Ordinal;
  Result := Comparer.Equals(A, B);
end;

// True when the token is the name or an alias of the symbol, with or without regard to case as the
// configuration of the parse says.
function NameMatches(const Symbol: ISymbol; const Token: string; IgnoreCase: Boolean): Boolean;
var
  Alias: string;
begin
  if not IgnoreCase then
    Exit(Symbol.Matches(Token));
  if SameNameIgnoringCase(Token, Symbol.Name) then
    Exit(True);
  for Alias in Symbol.Aliases do
    if SameNameIgnoringCase(Token, Alias) then
      Exit(True);
  Result := False;
end;

// Finds an option by token among the command's own and those inherited from ancestors that declared
// them recursive.
function FindOption(const Current: ICommand; const Inherited_: TArray<IOption>; const Token: string;
  IgnoreCase: Boolean): IOption;
var
  Candidate: IOption;
begin
  for Candidate in Current.Options do
    if NameMatches(Candidate, Token, IgnoreCase) then
      Exit(Candidate);
  for Candidate in Inherited_ do
    if NameMatches(Candidate, Token, IgnoreCase) then
      Exit(Candidate);
  Result := nil;
end;

function FindSubcommand(const Current: ICommand; const Token: string; IgnoreCase: Boolean): ICommand;
var
  Candidate: ICommand;
begin
  for Candidate in Current.Subcommands do
    if NameMatches(Candidate, Token, IgnoreCase) then
      Exit(Candidate);
  Result := nil;
end;

function FindDirective(const Root: IRootCommand; const Name: string; IgnoreCase: Boolean): IDirective;
var
  Candidate: IDirective;
begin
  for Candidate in Root.Directives do
    if NameMatches(Candidate, Name, IgnoreCase) then
      Exit(Candidate);
  Result := nil;
end;

function RecursiveOptionsOf(const Current: ICommand): TArray<IOption>;
var
  Candidate: IOption;
begin
  Result := nil;
  for Candidate in Current.Options do
    if Candidate.IsRecursive then
      Result := Result + [Candidate];
end;

// The options the ancestors of a command declared recursive, the farthest first, as they are in scope
// when the parse descends to it from the root. Each ancestor is the first command the one below was
// added to, as in .NET.
function InheritedOptionsOf(const Command: ICommand): TArray<IOption>;
var
  Parents: TArray<ISymbol>;
  Parent: ICommand;
begin
  Parents := Command.Parents;
  if (Length(Parents) = 0) or not Supports(Parents[0], ICommand, Parent) then
    Exit(nil);
  Result := InheritedOptionsOf(Parent) + RecursiveOptionsOf(Parent);
end;

// The last segment of a path, as .NET's Path.GetFileName: on Windows both slashes separate.
function FileNameOf(const Path: string): string;
const
  Separators = {$IFDEF MSWINDOWS}'\/'{$ELSE}'/'{$ENDIF};
begin
  Result := Copy(Path, LastDelimiter(Separators, Path) + 1, MaxInt);
end;

// As .NET's FirstArgumentIsRootCommand: the first token is the root when it is its name or an alias, or a
// path whose file name is -- unless what precedes its first `:` or `=` is one of its options, which makes it
// that option with a value. In a line, the path of the executable is the root too.
function FirstTokenIsRoot(const Root: ICommand; const Available: TArray<IOption>; const Args: TArray<string>;
  FromCommandLine, IgnoreCase: Boolean): Boolean;
var
  First: string;
  Cut: Integer;
begin
  if Length(Args) = 0 then
    Exit(False);
  First := Args[0];
  if FromCommandLine and (First = CommandLine.ExecutablePath) then
    Exit(True);
  Cut := First.IndexOfAny([CanonicalDelimiter, ColonDelimiter]);
  if (Cut >= 0) and (FindOption(Root, Available, First.Substring(0, Cut), IgnoreCase) <> nil) then
    Exit(False);
  Result := NameMatches(Root, FileNameOf(First), IgnoreCase);
end;

// With names compared without regard to case, two names one command answers to that differ only in case
// would reach the same symbol whichever was written: the grammar is refused, as an error of whoever wrote it.
// A name repeated exactly is not refused here: one that the help or the version answers to is refused by
// DemandReservedNamesFree.
procedure DemandNamesDistinctIgnoringCase(const Command: ICommand; const Inherited_: TArray<IOption>);
var
  Names: TArray<string>;
  Owners: TArray<TObject>;
  Option: IOption;
  Subcommand: ICommand;
  Root: IRootCommand;
  Directive: IDirective;
  First, Second: string;

  procedure Collect(const Symbol: ISymbol);
  var
    Alias: string;
  begin
    Names := Names + [Symbol.Name];
    Owners := Owners + [Symbol.AsObject];
    for Alias in Symbol.Aliases do
    begin
      Names := Names + [Alias];
      Owners := Owners + [Symbol.AsObject];
    end;
  end;

  // The first two names collected, of two different symbols, that differ only in case.
  function FindClash: Boolean;
  var
    I, J: Integer;
  begin
    for I := 0 to High(Names) do
      for J := I + 1 to High(Names) do
        if (Owners[I] <> Owners[J]) and (Names[I] <> Names[J]) and SameNameIgnoringCase(Names[I], Names[J]) then
        begin
          First := Names[I];
          Second := Names[J];
          Exit(True);
        end;
    Result := False;
  end;

begin
  Names := nil;
  Owners := nil;
  for Option in Command.Options do
    Collect(Option);
  for Option in Inherited_ do
    Collect(Option);
  for Subcommand in Command.Subcommands do
    Collect(Subcommand);
  if FindClash then
    raise ECommandLine.CreateFmt('Command "%s" answers to "%s" and to "%s", which differ only in case, ' +
      'and the parse was configured not to tell cases apart', [Command.FullPath, First, Second]);
  // The directives of a root are compared among themselves: written between brackets, a directive never
  // meets the name of an option or a command.
  if Supports(Command, IRootCommand, Root) then
  begin
    Names := nil;
    Owners := nil;
    for Directive in Root.Directives do
      Collect(Directive);
    if FindClash then
      raise ECommandLine.CreateFmt('Command "%s" answers to "[%s]" and to "[%s]", which differ only in case, ' +
        'and the parse was configured not to tell cases apart', [Command.FullPath, First, Second]);
  end;
  for Subcommand in Command.Subcommands do
    DemandNamesDistinctIgnoringCase(Subcommand, Inherited_ + RecursiveOptionsOf(Command));
end;

{ Reserved names }

function AllNamesOf(const Symbol: ISymbol): TArray<string>;
var
  Alias: string;
begin
  Result := [Symbol.Name];
  for Alias in Symbol.Aliases do
    Result := Result + [Alias];
end;

procedure RefuseTakenName(const Command: ICommand; const Option, Standard: IOption; IgnoreCase: Boolean);
var
  Name: string;
begin
  for Name in AllNamesOf(Option) do
    if NameMatches(Standard, Name, IgnoreCase) then
      raise ECommandLine.CreateFmt('Option "%s" of "%s" takes "%s", which is reserved to "%s": that name always ' +
        'reaches it, so the option could never answer', [Option.Name, Command.FullPath, Name, Standard.Name]);
end;

// Every option of the application against every help and version option, among the ones answering on Command.
procedure DemandFreeOn(const Command: ICommand; const Answering: TArray<IOption>; IgnoreCase: Boolean);
var
  Option, Standard: IOption;
begin
  for Option in Answering do
    if not IsStandardOption(Option) then
      for Standard in Answering do
        if IsStandardOption(Standard) then
          RefuseTakenName(Command, Option, Standard, IgnoreCase);
end;

procedure DemandFreeBelow(const Command: ICommand; const Inherited_: TArray<IOption>; IgnoreCase: Boolean);
var
  Subcommand: ICommand;
begin
  DemandFreeOn(Command, Command.Options + Inherited_, IgnoreCase);
  for Subcommand in Command.Subcommands do
    DemandFreeBelow(Subcommand, Inherited_ + RecursiveOptionsOf(Command), IgnoreCase);
end;

procedure DemandReservedNamesFree(const Root: ICommand; IgnoreCase: Boolean);
begin
  DemandFreeBelow(Root, InheritedOptionsOf(Root), IgnoreCase);
end;

procedure DemandOptionAddable(const Command: ICommand; const Option: IOption);
var
  Inherited_: TArray<IOption>;
  Subcommand: ICommand;
begin
  Inherited_ := InheritedOptionsOf(Command);
  DemandFreeOn(Command, Command.Options + Inherited_ + [Option], False);
  // A recursive option answers on every command below too.
  if Option.IsRecursive then
    for Subcommand in Command.Subcommands do
      DemandFreeBelow(Subcommand, Inherited_ + RecursiveOptionsOf(Command) + [Option], False);
end;

procedure DemandSubcommandAddable(const Command, Subcommand: ICommand);
begin
  DemandFreeBelow(Subcommand, InheritedOptionsOf(Command) + RecursiveOptionsOf(Command), False);
end;

{ TSymbolResultTree }

function TSymbolResultTree.Find(const Symbol: TObject): ISymbolResult;
var
  I: Integer;
begin
  for I := 0 to High(FSymbols) do
    if FSymbols[I] = Symbol then
      Exit(FResults[I]);
  Result := nil;
end;

procedure TSymbolResultTree.Add(const Symbol: TObject; const Node: ISymbolResult);
begin
  FSymbols := FSymbols + [Symbol];
  FResults := FResults + [Node];
end;

function TSymbolResultTree.Results: TArray<ISymbolResult>;
begin
  Result := FResults;
end;

procedure TSymbolResultTree.AddError(const Message: string; const Node: ISymbolResult);
var
  Error: TParseError;
begin
  Error.Message := Message;
  Error.SymbolResult := Node;
  FErrors := FErrors + [Error];
end;

procedure TSymbolResultTree.InsertFirstError(const Message: string; const Node: ISymbolResult);
var
  Error: TParseError;
begin
  Error.Message := Message;
  Error.SymbolResult := Node;
  FErrors := [Error] + FErrors;
end;

function TSymbolResultTree.Errors: TArray<TParseError>;
begin
  Result := FErrors;
end;

procedure TSymbolResultTree.SetErrors(const Value: TArray<TParseError>);
begin
  FErrors := Value;
end;

function TSymbolResultTree.ErrorCount: Integer;
begin
  Result := Length(FErrors);
end;

procedure TSymbolResultTree.AddUnmatched(const Token: string);
begin
  FUnmatched := FUnmatched + [Token];
end;

function TSymbolResultTree.Unmatched: TArray<string>;
begin
  Result := FUnmatched;
end;

{ TSymbolResult }

constructor TSymbolResult.Create(const ATree: ISymbolResultTree; const AParent: ISymbolResult);
begin
  inherited Create;
  FTree := ATree;
  FParent := AParent;
end;

function TSymbolResult.Parent: ISymbolResult;
begin
  Result := FParent;
end;

function TSymbolResult.Tokens: TArray<TToken>;
begin
  Result := FTokens;
end;

procedure TSymbolResult.AddToken(const Token: TToken);
begin
  FTokens := FTokens + [Token];
end;

function TSymbolResult.Errors: TArray<TParseError>;
var
  Owner: ISymbolResultTree;
  Error: TParseError;
begin
  Result := nil;
  Owner := FTree;
  if Owner = nil then
    Exit;
  for Error in Owner.Errors do
    if (Error.SymbolResult <> nil) and ((Error.SymbolResult as TObject) = Self) then
      Result := Result + [Error];
end;

procedure TSymbolResult.AddError(const Message: string);
var
  Owner: ISymbolResultTree;
begin
  Owner := FTree;
  if Owner <> nil then
    Owner.AddError(Message, Self);
end;

function TSymbolResult.GetResult(const Option: IOption): IOptionResult;
var
  Owner: ISymbolResultTree;
  Found: ISymbolResult;
begin
  Result := nil;
  Owner := FTree;
  if (Owner = nil) or (Option = nil) then
    Exit;
  Found := Owner.Find(Option.AsObject);
  if Found <> nil then
    Supports(Found, IOptionResult, Result);
end;

function TSymbolResult.GetResult(const Argument: IArgument): IArgumentResult;
var
  Owner: ISymbolResultTree;
  Found: ISymbolResult;
begin
  Result := nil;
  Owner := FTree;
  if (Owner = nil) or (Argument = nil) then
    Exit;
  Found := Owner.Find(Argument.AsObject);
  if Found <> nil then
    Supports(Found, IArgumentResult, Result);
end;

function TSymbolResult.GetResult(const Command: ICommand): ICommandResult;
var
  Owner: ISymbolResultTree;
  Found: ISymbolResult;
begin
  Result := nil;
  Owner := FTree;
  if (Owner = nil) or (Command = nil) then
    Exit;
  Found := Owner.Find(Command.AsObject);
  if Found <> nil then
    Supports(Found, ICommandResult, Result);
end;

function TSymbolResult.GetResult(const Directive: IDirective): IDirectiveResult;
var
  Owner: ISymbolResultTree;
  Found: ISymbolResult;
begin
  Result := nil;
  Owner := FTree;
  if (Owner = nil) or (Directive = nil) then
    Exit;
  Found := Owner.Find(Directive.AsObject);
  if Found <> nil then
    Supports(Found, IDirectiveResult, Result);
end;

{ TCommandResult }

constructor TCommandResult.Create(const ATree: ISymbolResultTree; const AParent: ISymbolResult;
  const ACommand: ICommand; const AIdentifierToken: TToken);
begin
  inherited Create(ATree, AParent);
  FCommand := ACommand;
  FIdentifierToken := AIdentifierToken;
end;

function TCommandResult.Command: ICommand;
begin
  Result := FCommand;
end;

function TCommandResult.IdentifierToken: TToken;
begin
  Result := FIdentifierToken;
end;

function TCommandResult.Children: TArray<ISymbolResult>;
var
  Owner: ISymbolResultTree;
  Candidate, CandidateParent: ISymbolResult;
begin
  Result := nil;
  Owner := FTree;
  if Owner = nil then
    Exit;
  for Candidate in Owner.Results do
  begin
    CandidateParent := Candidate.Parent;
    if (CandidateParent <> nil) and ((CandidateParent as TObject) = Self) then
      Result := Result + [Candidate];
  end;
end;

{ TValuedResult }

constructor TValuedResult.Create(const ATree: ISymbolResultTree; const AParent: ISymbolResult;
  AValued: TValuedSymbol);
begin
  inherited Create(ATree, AParent);
  FValued := AValued;
  FState := TConversionState.Pending;
end;

procedure TValuedResult.Fail(const Message: string);
begin
  FState := TConversionState.Failed;
  FFailure := Message;
  inherited AddError(Message);
end;

function TValuedResult.CheckArity: Boolean;
var
  Error: string;
begin
  if (FState <> TConversionState.Pending) or IsImplicitResult then
    Exit(FState <> TConversionState.Failed);
  Error := ArityError;
  Result := Error = '';
  if not Result then
    Fail(Error);
end;

procedure TValuedResult.ConvertTokens;
var
  Error: string;
  Token: TToken;
  Value: TValue;
  Values: TArray<TValue>;
  Parser: TUntypedWholeParser;
  Owner: ISymbolResultTree;
  ErrorsBefore: Integer;
  Command: ICommandResult;
  CommandName: string;
begin
  // When the arity fails nothing else is reported for it, as in .NET.
  Error := ArityError;
  if Error <> '' then
  begin
    Fail(Error);
    Exit;
  end;

  // A conversion of its own takes every token and answers the value whole. It refuses by adding an error,
  // and an exception it raises leaves the parse as it is.
  Parser := FValued.WholeParser();
  if Assigned(Parser) then
  begin
    Owner := FTree;
    ErrorsBefore := 0;
    if Owner <> nil then
      ErrorsBefore := Owner.ErrorCount;
    Value := Parser(ValueResult);
    if FState = TConversionState.Failed then
      Exit;
    if (Owner <> nil) and (Owner.ErrorCount <> ErrorsBefore) then
    begin
      FState := TConversionState.Failed;
      FFailure := Owner.Errors[Owner.ErrorCount - 1].Message;
      Exit;
    end;
    FValues := [Value];
    FFromDefault := False;
    FWhole := True;
    FState := TConversionState.Converted;
    Exit;
  end;
  if Supports(FParent, ICommandResult, Command) then
    CommandName := Command.Command.Name
  else
    CommandName := '';
  Values := nil;
  for Token in FTokens do
  begin
    if not FValued.Convert(Token.Value, CommandName, Value, Error) then
    begin
      Fail(Error);
      Exit;
    end;
    Values := Values + [Value];
  end;
  // A flag named with no value is the whole statement: --verbose says true.
  if (Length(Values) = 0) and FValued.IsFlag then
    Values := [TCommandLineValue.FlagValue(FValued.ValueType)];
  FValues := Values;
  FFromDefault := False;
  FState := TConversionState.Converted;
end;

procedure TValuedResult.Validate;
var
  Boxed: TValue;
begin
  if FValidated then
    Exit;
  FValidated := True;
  if FState <> TConversionState.Pending then
    Exit;
  if IsImplicitResult then
  begin
    // The default is part of the result, so it is calculated here, read or not. A calculation that adds an
    // error fails the value, as a conversion would.
    Boxed := FValued.BoxedDefault(ValueResult);
    if FState = TConversionState.Failed then
      Exit;
    FValues := [Boxed];
    FFromDefault := True;
    FState := TConversionState.Converted;
    Exit;
  end;
  ConvertTokens;
end;

procedure TValuedResult.EnsureConverted;
begin
  if FState <> TConversionState.Pending then
    Exit;
  if IsImplicitResult then
    Validate
  else
    // Read without the validations, the declared set of accepted values among them, which are the
    // validation's to run: the useValidators: false of .NET.
    ConvertTokens;
end;

function TValuedResult.ReadValues(out FromDefault, Whole: Boolean): TArray<TValue>;
begin
  EnsureConverted;
  if FState = TConversionState.Failed then
    raise ECommandLine.Create(FFailure);
  FromDefault := FFromDefault;
  Whole := FWhole or FFromDefault;
  Result := FValues;
end;

function TValuedResult.DiagramValues: string;
var
  Texts: TArray<string>;
  Token: TToken;
  Value: TValue;
  I: Integer;
begin
  EnsureConverted;
  Texts := nil;
  if FState = TConversionState.Failed then
  begin
    for Token in Tokens do
      Texts := Texts + [Token.Value];
    Exit('<' + string.Join('> <', Texts) + '>');
  end;
  // One value that is the whole value -- a default, or what a conversion of its own answered -- shows each
  // element when it is a collection.
  if (FWhole or FFromDefault) and FValues[0].IsArray then
    for I := 0 to FValues[0].GetArrayLength - 1 do
      Texts := Texts + [TCommandLineValue.ToText(FValues[0].GetArrayElement(I))]
  else
    for Value in FValues do
      Texts := Texts + [TCommandLineValue.ToText(Value)];
  if (Length(Texts) = 0) and (FValued.Arity.MaximumNumberOfValues <= 1) then
    Exit('');
  Result := '<' + string.Join('> <', Texts) + '>';
end;

{ TOptionResult }

constructor TOptionResult.Create(const ATree: ISymbolResultTree; const AParent: ISymbolResult;
  const AOption: IOption);
begin
  inherited Create(ATree, AParent, AOption.AsObject as TValuedSymbol);
  FOption := AOption;
end;

function TOptionResult.ValueResult: IArgumentResult;
begin
  Result := TOptionArgumentResult.Create(Self);
end;

procedure TOptionResult.Named(const Token: TToken);
begin
  if FIdentifierToken.IsNull then
    FIdentifierToken := Token;
  Inc(FIdentifierTokenCount);
end;

function TOptionResult.ArityError: string;
var
  Written: string;
begin
  // Named as the invocation first wrote it, as .NET names it.
  if FIdentifierToken.HasValue then
    Written := FIdentifierToken.Value.Value
  else
    Written := FOption.Name;
  // Repeating an option that admits one value is an ambiguous invocation, and an ambiguous
  // invocation has to fail. Quietly keeping one of them is the same sin as reading 80xyz as 80:
  // a value the parser chose on its own.
  if not FOption.Arity.AllowsMany and (FIdentifierTokenCount > 1) then
    Exit(FormatText(TCommandLineText.OptionExpectsOneArgument,
      [Written, IntToStr(Max(Length(FTokens), FIdentifierTokenCount))]));
  if not FOption.IsFlag and (Length(FTokens) = 0) then
    Exit(FormatText(TCommandLineText.OptionRequiredArgumentMissing, [Written]));
  Result := '';
end;

function TOptionResult.IsImplicitResult: Boolean;
begin
  Result := IsImplicit;
end;

function TOptionResult.Option: IOption;
begin
  Result := FOption;
end;

function TOptionResult.IsImplicit: Boolean;
begin
  Result := FIdentifierToken.IsNull;
end;

function TOptionResult.IdentifierToken: TNullable<TToken>;
begin
  Result := FIdentifierToken;
end;

function TOptionResult.IdentifierTokenCount: Integer;
begin
  Result := FIdentifierTokenCount;
end;

{ TArgumentResult }

constructor TArgumentResult.Create(const ATree: ISymbolResultTree; const AParent: ISymbolResult;
  const AArgument: IArgument);
begin
  inherited Create(ATree, AParent, AArgument.AsObject as TValuedSymbol);
  FArgument := AArgument;
end;

function TArgumentResult.ValueResult: IArgumentResult;
begin
  Result := Self;
end;

function TArgumentResult.ArgumentLimitReached: Boolean;
begin
  Result := FArgument.Arity.MaximumNumberOfValues = Length(FTokens);
end;

procedure TArgumentResult.OnlyTake(NumberOfTokens: Integer);
var
  Owner: ISymbolResultTree;
  Command: ICommandResult;
  Arguments: TArray<IArgument>;
  Index, NextIndex, ToPass: Integer;
  Found: ISymbolResult;
  Next: TArgumentResult;
  Token: TToken;
begin
  if NumberOfTokens < 0 then
    raise ECommandLine.CreateFmt('OnlyTake takes a number of tokens that is not negative, and got %d',
      [NumberOfTokens]);
  if FOnlyTaken then
    raise ECommandLine.Create('OnlyTake can only be called once');
  FOnlyTaken := True;
  if NumberOfTokens >= Length(FTokens) then
    Exit;

  Owner := FTree;
  Command := FParent as ICommandResult;
  Arguments := Command.Command.Arguments;
  Index := 0;
  while (Index < Length(Arguments)) and (Arguments[Index].AsObject <> FArgument.AsObject) do
    Inc(Index);
  NextIndex := Index + 1;
  ToPass := Length(FTokens) - NumberOfTokens;

  // The tokens beyond the ones kept go to the arguments that follow, each up to what its arity admits.
  while (ToPass > 0) and (NextIndex < Length(Arguments)) do
  begin
    Found := Owner.Find(Arguments[NextIndex].AsObject);
    if Found = nil then
    begin
      Found := TArgumentResult.Create(Owner, Command, Arguments[NextIndex]);
      Owner.Add(Arguments[NextIndex].AsObject, Found);
    end;
    Next := (Found as TObject) as TArgumentResult;
    while not Next.ArgumentLimitReached and (ToPass > 0) do
    begin
      Token := FTokens[NumberOfTokens];
      Delete(FTokens, NumberOfTokens, 1);
      Next.AddToken(Token);
      Dec(ToPass);
    end;
    Inc(NextIndex);
  end;

  // What no argument takes is a token nothing claimed, of the command this argument belongs to.
  while ToPass > 0 do
  begin
    Token := FTokens[NumberOfTokens];
    Delete(FTokens, NumberOfTokens, 1);
    Owner.AddUnmatched(Token.Value);
    if Command.Command.TreatsUnmatchedTokensAsErrors then
      Command.AddError(FormatText(TCommandLineText.UnrecognizedCommandOrArgument, [Token.Value]));
    Dec(ToPass);
  end;
end;

procedure TArgumentResult.TakeDefault;
begin
  FImplicit := True;
end;

procedure TArgumentResult.AddError(const Message: string);
begin
  Fail(Message);
end;

function TArgumentResult.ArityError: string;
begin
  if Length(FTokens) < FArgument.Arity.MinimumNumberOfValues then
    Exit(FormatText(TCommandLineText.CommandRequiredArgumentMissing,
      [FArgument.Name, (FParent as ICommandResult).Command.Name]));
  Result := '';
end;

function TArgumentResult.IsImplicitResult: Boolean;
begin
  Result := FImplicit;
end;

function TArgumentResult.Argument: IArgument;
begin
  Result := FArgument;
end;

function TArgumentResult.IsImplicit: Boolean;
begin
  Result := FImplicit;
end;

{ TOptionArgumentResult }

constructor TOptionArgumentResult.Create(AOption: TOptionResult);
begin
  inherited Create;
  FOption := AOption;
  FOptionRef := AOption;
end;

function TOptionArgumentResult.Parent: ISymbolResult;
begin
  Result := FOptionRef;
end;

function TOptionArgumentResult.Tokens: TArray<TToken>;
begin
  Result := FOption.Tokens;
end;

function TOptionArgumentResult.Errors: TArray<TParseError>;
begin
  Result := FOption.Errors;
end;

procedure TOptionArgumentResult.AddError(const Message: string);
begin
  FOption.Fail(Message);
end;

function TOptionArgumentResult.GetResult(const Option: IOption): IOptionResult;
begin
  Result := FOption.GetResult(Option);
end;

function TOptionArgumentResult.GetResult(const Argument: IArgument): IArgumentResult;
begin
  Result := FOption.GetResult(Argument);
end;

function TOptionArgumentResult.GetResult(const Command: ICommand): ICommandResult;
begin
  Result := FOption.GetResult(Command);
end;

function TOptionArgumentResult.GetResult(const Directive: IDirective): IDirectiveResult;
begin
  Result := FOption.GetResult(Directive);
end;

function TOptionArgumentResult.Argument: IArgument;
begin
  Result := FOption.FValued.ValueArgument;
end;

function TOptionArgumentResult.IsImplicit: Boolean;
begin
  Result := FOption.IsImplicit;
end;

procedure TOptionArgumentResult.OnlyTake(NumberOfTokens: Integer);
begin
  raise ECommandLine.CreateFmt('OnlyTake is supported only for an argument of a command, and "%s" is an option',
    [FOption.Option.Name]);
end;

{ TDirectiveResult }

constructor TDirectiveResult.Create(const ATree: ISymbolResultTree; const ADirective: IDirective;
  const AToken: TToken);
begin
  inherited Create(ATree, nil);
  FDirective := ADirective;
  FToken := AToken;
end;

procedure TDirectiveResult.AddValue(const Value: string);
begin
  FValues := FValues + [Value];
end;

function TDirectiveResult.Directive: IDirective;
begin
  Result := FDirective;
end;

function TDirectiveResult.Token: TToken;
begin
  Result := FToken;
end;

function TDirectiveResult.Values: TArray<string>;
begin
  Result := FValues;
end;

{ Diagram }

// A boolean value, or a symbol that takes none, is not shown: naming it already says it.
function ShowsValue(const Valued: IValued): Boolean;
begin
  Result := (Valued.ValueType <> TypeInfo(Boolean)) and (Valued.Arity.MaximumNumberOfValues > 0);
end;

// One result and what hangs from it, as .NET's ParseDiagramAction writes them.
procedure WriteDiagram(const Builder: TStringBuilder; const Parsed: IParseResult; const Node: ISymbolResult);
var
  Error: TParseError;
  AsCommand: ICommandResult;
  AsOption: IOptionResult;
  AsArgument: IArgumentResult;
  Child: ISymbolResult;
  WithName: Boolean;
begin
  for Error in Parsed.Errors do
    if (Error.SymbolResult <> nil) and ((Error.SymbolResult as TObject) = (Node as TObject)) then
    begin
      Builder.Append('!');
      Break;
    end;

  if Supports(Node, IArgumentResult, AsArgument) then
  begin
    // Its name only when its command has more than one argument: otherwise the place says which.
    WithName := Supports(Node.Parent, ICommandResult, AsCommand) and (Length(AsCommand.Command.Arguments) > 1);
    if WithName then
      Builder.Append('[ ').Append(AsArgument.Argument.Name).Append(' ');
    if AsArgument.Argument.Arity.MaximumNumberOfValues > 0 then
      Builder.Append(((Node as TObject) as TValuedResult).DiagramValues);
    if WithName then
      Builder.Append(' ]');
    Exit;
  end;

  if Supports(Node, IOptionResult, AsOption) then
  begin
    if AsOption.IsImplicit then
      Builder.Append('*');
    Builder.Append('[ ');
    if AsOption.IdentifierToken.HasValue then
      Builder.Append(AsOption.IdentifierToken.Value.Value)
    else
      Builder.Append(AsOption.Option.Name);
    // The value is the one of the argument it is carried in, which .NET writes as a child of the option.
    if ShowsValue(AsOption.Option) then
      Builder.Append(' ').Append(((Node as TObject) as TValuedResult).DiagramValues);
    Builder.Append(' ]');
    Exit;
  end;

  AsCommand := Node as ICommandResult;
  Builder.Append('[ ').Append(AsCommand.IdentifierToken.Value);
  for Child in AsCommand.Children do
    if not Supports(Child, IArgumentResult, AsArgument) or ShowsValue(AsArgument.Argument) then
    begin
      Builder.Append(' ');
      WriteDiagram(Builder, Parsed, Child);
    end;
  Builder.Append(' ]');
end;

{ TParseResult }

constructor TParseResult.Create(const ARoot: ICommand; const AConfiguration: TParserConfiguration;
  const ATree: ISymbolResultTree; const ARootResult: ICommandResult);
begin
  inherited Create;
  FRoot := ARoot;
  FConfiguration := AConfiguration;
  FTree := ATree;
  FRootResult := ARootResult;
  FCommandResult := ARootResult;
end;

procedure TParseResult.Close(const Innermost: ICommandResult; const Tokens: TArray<TToken>;
  const Action: ICommandLineAction; const ActionOwner: IOption; const PreActions: TArray<ICommandLineAction>);
begin
  FCommandResult := Innermost;
  FTokens := Tokens;
  FAction := Action;
  FActionOwner := ActionOwner;
  FPreActions := PreActions;
  FErrorsShared := FTree.ErrorCount > 0;
end;

function TParseResult.ResultOf(const Symbol: ISymbol): ISymbolResult;
begin
  if Symbol = nil then
    Exit(nil);
  Result := FTree.Find(Symbol.AsObject);
end;

function TParseResult.Command: ICommand;
begin
  Result := FCommandResult.Command;
end;

function TParseResult.CommandResult: ICommandResult;
begin
  Result := FCommandResult;
end;

function TParseResult.RootCommandResult: ICommandResult;
begin
  Result := FRootResult;
end;

function TParseResult.Tokens: TArray<TToken>;
begin
  Result := FTokens;
end;

function TParseResult.GetResult(const Option: IOption): IOptionResult;
begin
  Result := FRootResult.GetResult(Option);
end;

function TParseResult.GetResult(const Argument: IArgument): IArgumentResult;
begin
  Result := FRootResult.GetResult(Argument);
end;

function TParseResult.GetResult(const Command: ICommand): ICommandResult;
begin
  Result := FRootResult.GetResult(Command);
end;

function TParseResult.GetResult(const Directive: IDirective): IDirectiveResult;
begin
  Result := FRootResult.GetResult(Directive);
end;

function TParseResult.Succeeded: Boolean;
begin
  Result := Length(Errors) = 0;
end;

function TParseResult.Errors: TArray<TParseError>;
begin
  // As in .NET, a parse result shares the tree's list only when the parse found errors. An error
  // that a later read adds to a clean parse is in the Errors of the result it belongs to, not here.
  if FErrorsShared then
    Result := FTree.Errors
  else
    Result := nil;
end;

function TParseResult.ErrorText: string;
var
  Messages: TArray<string>;
  Error: TParseError;
begin
  Messages := nil;
  for Error in Errors do
    Messages := Messages + [Error.Message];
  Result := string.Join(sLineBreak, Messages);
end;

function TParseResult.IsHelpRequested: Boolean;
begin
  // The action that wins is the one of a help option.
  Result := (FActionOwner <> nil) and Supports(FActionOwner, IHelpOption);
end;

function TParseResult.HelpText: string;
begin
  if IsHelpRequested then
    Result := FCommandResult.Command.HelpText
  else
    Result := '';
end;

function TParseResult.IsVersionRequested: Boolean;
begin
  Result := (FActionOwner <> nil) and Supports(FActionOwner, IVersionOption);
end;

function TParseResult.VersionInfo: TVersionInfo;
var
  Root: IRootCommand;
begin
  // No copy is kept: the result already has the command that answered, and the version is its own.
  if Supports(Command, IRootCommand, Root) then
    Result := Root.VersionInfo
  else
    Result := Default(TVersionInfo);
end;

function TParseResult.WasProvided(const Symbol: ISymbol): Boolean;
var
  Found: ISymbolResult;
  AsOption: IOptionResult;
  AsArgument: IArgumentResult;
begin
  // The invocation named it. An option the parse created for its default or for its absence was not
  // named, and an argument was named when it holds a token: a missing required one has a result
  // too, with the error that it is missing.
  Found := ResultOf(Symbol);
  if Found = nil then
    Exit(False);
  if Supports(Found, IOptionResult, AsOption) then
    Exit(not AsOption.IsImplicit);
  if Supports(Found, IArgumentResult, AsArgument) then
    Exit(Length(AsArgument.Tokens) > 0);
  Result := (Found as TObject) <> (FRootResult as TObject);
end;

function TParseResult.RawOf(const Symbol: ISymbol): TNullable<string>;
var
  Found: ISymbolResult;
begin
  Found := ResultOf(Symbol);
  if (Found = nil) or (Length(Found.Tokens) = 0) then
    Exit(TNullable<string>.Null);
  Result := TNullable<string>.From(Found.Tokens[0].Value);
end;

function TParseResult.RawValuesOf(const Symbol: ISymbol): TArray<string>;
var
  Found: ISymbolResult;
  Token: TToken;
begin
  Result := nil;
  Found := ResultOf(Symbol);
  if Found = nil then
    Exit;
  for Token in Found.Tokens do
    Result := Result + [Token.Value];
end;

function TParseResult.UnmatchedTokens: TArray<string>;
begin
  Result := FTree.Unmatched;
end;

function TParseResult.Action: ICommandLineAction;
begin
  if FAction <> nil then
    Result := FAction
  else
    Result := FCommandResult.Command.Action;
end;

function TParseResult.Diagram: string;
var
  Builder: TStringBuilder;
  Token: string;
  Unmatched: TArray<string>;
begin
  Builder := TStringBuilder.Create;
  try
    WriteDiagram(Builder, Self, FRootResult);
    Unmatched := FTree.Unmatched;
    if Length(Unmatched) > 0 then
    begin
      Builder.Append('   ???-->');
      for Token in Unmatched do
        Builder.Append(' ').Append(Token);
    end;
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

function TParseResult.Configuration: TParserConfiguration;
begin
  Result := FConfiguration;
end;

function TParseResult.InvocationConfiguration: TInvocationConfiguration;
begin
  Result := FInvocationConfiguration;
end;

function TParseResult.Invoke: Integer;
var
  Configuration: TInvocationConfiguration;
begin
  Result := Invoke(Configuration);
end;

function TParseResult.Invoke(const Configuration: TInvocationConfiguration): Integer;
begin
  FInvocationConfiguration := Configuration;
  Result := InvokeActions(Self, FPreActions);
end;

{ TScopeKnowledge }

constructor TScopeKnowledge.Create(IgnoreCase: Boolean);
begin
  inherited Create;
  FIgnoreCase := IgnoreCase;
end;

procedure TScopeKnowledge.Enter(const Command: ICommand; const Available: TArray<IOption>);
begin
  FCurrent := Command;
  FAvailable := Available;
end;

function TScopeKnowledge.Knows(const Token: string): Boolean;
begin
  // The help and the version count as known. Otherwise `-h` could be swallowed into a bundle.
  Result := FindOption(FCurrent, FAvailable, Token, FIgnoreCase) <> nil;
end;

function TScopeKnowledge.ShortTakesValue(const Letter: Char): Boolean;
var
  Option: IOption;
begin
  Option := FindOption(FCurrent, FAvailable, ShortPrefix + Letter, FIgnoreCase);
  // A built-in answers nil and is a flag: asking for help takes no value.
  Result := (Option <> nil) and not Option.IsFlag;
end;

{ Reading }

// The first step: every token typed, in the scope of the command reached when it is read. Which
// tokens an option takes is decided here, because it depends on the token and not on the tree: a
// value glued to it, a boolean literal behind a flag, and anything at all behind any other option.
function Tokenize(const Root: ICommand; const Args: TArray<string>; const Configuration: TParserConfiguration;
  FromCommandLine: Boolean; out RootTokenValue: string; out ReadErrors: TArray<string>): TArray<TItem>;
var
  Items: TArray<TItem>;
  Tokens: TArray<string>;
  Current: ICommand;
  Available: TArray<IOption>;
  Scope: TScopeKnowledge;
  Known: IKnownTokens;
  Index: Integer;
  Token, OptionName, InlineValue, Next, DirectiveName, DirectiveValue: string;
  HasInline, Ended, Took, IgnoreCase, HasDirectiveValue: Boolean;
  Step: TTokenRewrite;
  Option: IOption;
  Subcommand: ICommand;
  AsRoot: IRootCommand;
  RootNameTokens: TArray<string>;
  TakenOwners: TArray<TObject>;
  TakenCounts: TArray<Integer>;

  procedure Emit(const Value: string; TokenType: TTokenType; const Symbol: ISymbol; Kind: TItemKind;
    const Name: string = ''; const Error: string = '');
  var
    Item: TItem;
  begin
    Item.Token.Value := Value;
    Item.Token.TokenType := TokenType;
    Item.Token.Symbol := Symbol;
    Item.Kind := Kind;
    Item.Name := Name;
    Item.Error := Error;
    Items := Items + [Item];
  end;

  // The values an option took, over every time the invocation named it.
  function TakenBy(const Owner: IOption): Integer;
  var
    At: Integer;
  begin
    for At := 0 to High(TakenOwners) do
      if TakenOwners[At] = Owner.AsObject then
        Exit(TakenCounts[At]);
    Result := 0;
  end;

  procedure TakeValue(const Owner: IOption; const Value: string);
  var
    At: Integer;
  begin
    Emit(Value, TTokenType.Argument, nil, TItemKind.OptionValue);
    for At := 0 to High(TakenOwners) do
      if TakenOwners[At] = Owner.AsObject then
      begin
        Inc(TakenCounts[At]);
        Exit;
      end;
    TakenOwners := TakenOwners + [Owner.AsObject];
    TakenCounts := TakenCounts + [1];
  end;

begin
  Items := nil;
  TakenOwners := nil;
  TakenCounts := nil;
  IgnoreCase := Configuration.EnableCaseInsensitiveNames;
  Current := Root;
  Available := InheritedOptionsOf(Current) + RecursiveOptionsOf(Current);
  Tokens := Args;
  RootTokenValue := Root.Name;
  if FirstTokenIsRoot(Root, Available, Args, FromCommandLine, IgnoreCase) then
  begin
    RootTokenValue := Args[0];
    Tokens := Copy(Args, 1, MaxInt);
  end;
  // The directives, only when the root declares any, and only among the tokens written ahead of every other
  // but the root's own name. They are read before the response files are, so none comes out of one.
  if Supports(Root, IRootCommand, AsRoot) and (Length(AsRoot.Directives) > 0) then
  begin
    RootNameTokens := nil;
    Index := 0;
    while Index < Length(Tokens) do
    begin
      Token := Tokens[Index];
      if TTokenNormalizer.LooksLikeDirective(Token) then
      begin
        TTokenNormalizer.SplitDirective(Token, DirectiveName, DirectiveValue, HasDirectiveValue);
        Emit(Token, TTokenType.Directive, FindDirective(AsRoot, DirectiveName, IgnoreCase), TItemKind.Directive);
      end
      else if NameMatches(Root, Token, IgnoreCase) then
        RootNameTokens := RootNameTokens + [Token]
      else
        Break;
      Inc(Index);
    end;
    Tokens := RootNameTokens + Copy(Tokens, Index, MaxInt);
  end;
  // Response files are resolved before anything else is decided about any token,
  // because all they do is contribute tokens and nothing else.
  Tokens := TResponseFiles.Expand(Tokens, Configuration.ResponseFileTokenReplacer, ReadErrors);
  Scope := TScopeKnowledge.Create(IgnoreCase);
  Known := Scope;
  Scope.Enter(Current, Available);
  Ended := False;
  Index := 0;

  while Index < Length(Tokens) do
  begin
    Token := Tokens[Index];

    // POSIX: `--` ends the options. Everything after it is a value, dashes and all,
    // which is the only way to pass a positional whose value looks like an option.
    if Ended then
    begin
      Emit(Token, TTokenType.Argument, nil, TItemKind.Positional);
      Inc(Index);
      Continue;
    end;
    if Token = EndOfOptions then
    begin
      Ended := True;
      Emit(Token, TTokenType.DoubleDash, nil, TItemKind.DoubleDash);
      Inc(Index);
      Continue;
    end;

    // Before anything else is decided about this token, it is put in the canonical form
    // the rest of this loop already understands. The rewriting is asked here, and not once
    // over the whole array, because it needs to know what is declared, and that depends on
    // the command reached.
    Step := TTokenNormalizer.Rewrite(Token, Known, Configuration.EnablePosixBundling);
    if Step.Error <> '' then
    begin
      Emit(Token, TTokenType.Option, nil, TItemKind.BadGroup, '', Step.Error);
      Inc(Index);
      Continue;
    end;
    if Step.Changed then
    begin
      Tokens := TTokenNormalizer.Splice(Tokens, Index, Step.Tokens);
      Continue;
    end;

    TTokenNormalizer.SplitToken(Token, OptionName, InlineValue, HasInline);

    // A declared name is an option whatever it looks like: the help answers to `/h` and `/?`.
    Option := FindOption(Current, Available, OptionName, IgnoreCase);
    if TTokenNormalizer.LooksLikeOption(Token) or (Option <> nil) then
    begin
      if Option = nil then
      begin
        // The token stays WHOLE, value and all, so a wrapper hands on what was written.
        Emit(Token, TTokenType.Option, nil, TItemKind.UnknownOption, OptionName);
        Inc(Index);
        Continue;
      end;
      Emit(OptionName, TTokenType.Option, Option, TItemKind.OptionName);
      if HasInline then
      begin
        TakeValue(Option, InlineValue);
        Took := True;
      end
      else if Option.IsFlag then
      begin
        // A flag may carry its value explicitly -- `--minify false` -- when its arity admits one.
        // Only a boolean literal counts as that value; anything else is somebody else's token.
        Took := (Option.Arity.MaximumNumberOfValues > 0) and (Index + 1 < Length(Tokens)) and
          TCommandLineValue.IsBooleanLiteral(Tokens[Index + 1]);
        if Took then
        begin
          Inc(Index);
          TakeValue(Option, Tokens[Index]);
        end;
      end
      else
      begin
        // Taken unconditionally, dash or no dash: this is what lets --port -5 through.
        Took := Index + 1 < Length(Tokens);
        if Took then
        begin
          Inc(Index);
          TakeValue(Option, Tokens[Index]);
        end;
      end;

      // One mention of the name, several values -- but only for an option that asked for it.
      // Taking them stops at the next token that looks like an option, at `--`, and at the
      // maximum the arity declared, so what is swallowed is bounded by what was declared rather
      // than by where the invocation happens to end.
      if Took and Option.AllowsMultipleArgumentsPerToken and Option.Arity.AllowsMany then
        while (Index + 1 < Length(Tokens)) and (TakenBy(Option) < Option.Arity.MaximumNumberOfValues) do
        begin
          Next := Tokens[Index + 1];
          if (Next = EndOfOptions) or TTokenNormalizer.LooksLikeOption(Next) then
            Break;
          Inc(Index);
          TakeValue(Option, Next);
        end;

      Inc(Index);
      Continue;
    end;

    Subcommand := FindSubcommand(Current, Token, IgnoreCase);
    if Subcommand <> nil then
    begin
      Emit(Token, TTokenType.Command, Subcommand, TItemKind.CommandName);
      Current := Subcommand;
      Available := Available + RecursiveOptionsOf(Current);
      Scope.Enter(Current, Available);
      Inc(Index);
      Continue;
    end;

    Emit(Token, TTokenType.Argument, nil, TItemKind.Positional);
    Inc(Index);
  end;
  Result := Items;
end;

{ TParseOperation }

constructor TParseOperation.Create(const Root: ICommand; const Configuration: TParserConfiguration;
  const RootTokenValue: string; const Items: TArray<TItem>; const ReadErrors: TArray<string>);
var
  RootToken: TToken;
  Error: string;
begin
  inherited Create;
  FRoot := Root;
  FConfiguration := Configuration;
  FItems := Items;
  FTree := TSymbolResultTree.Create;
  // An error of reading the invocation belongs to no symbol: none had been reached.
  for Error in ReadErrors do
    FTree.AddError(Error, nil);
  RootToken.Value := RootTokenValue;
  RootToken.TokenType := TTokenType.Command;
  RootToken.Symbol := Root;
  FRootResult := TCommandResult.Create(FTree, nil, Root, RootToken);
  FTree.Add(Root.AsObject, FRootResult);
  FInnermost := FRootResult;
end;

procedure TParseOperation.Unmatched(const Token, Message: string);
begin
  // Offered apart always, error or not: it is what a wrapper hands on, and what a typo is compared
  // with. It is an error when the command reached refuses it, which is the default -- unless the root
  // collects it, which outranks the subcommand.
  FTree.AddUnmatched(Token);
  if not FInnermost.Command.TreatsUnmatchedTokensAsErrors then
    Exit;
  if ((FInnermost as TObject) <> (FRootResult as TObject)) and not FRoot.TreatsUnmatchedTokensAsErrors then
    Exit;
  FInnermost.AddError(Message);
end;

procedure TParseOperation.ParseDirective;
var
  Item: TItem;
  Directive: IDirective;
  Found: ISymbolResult;
  Node: TDirectiveResult;
  Name, Value: string;
  HasValue: Boolean;
  Action: ICommandLineAction;
begin
  Item := FItems[FIndex];
  Inc(FIndex);
  if not Supports(Item.Token.Symbol, IDirective, Directive) then
  begin
    Unmatched(Item.Token.Value, FormatText(TCommandLineText.UnrecognizedCommandOrArgument, [Item.Token.Value]));
    Exit;
  end;
  Found := FTree.Find(Directive.AsObject);
  if Found = nil then
  begin
    Found := TDirectiveResult.Create(FTree, Directive, Item.Token);
    FTree.Add(Directive.AsObject, Found);
  end;
  Node := (Found as TObject) as TDirectiveResult;
  Node.AddToken(Item.Token);
  TTokenNormalizer.SplitDirective(Item.Token.Value, Name, Value, HasValue);
  if HasValue then
    Node.AddValue(Value);
  // Every time it is written, as in .NET: a terminating action wins the invocation, and one that is not runs
  // before the winner.
  Action := Directive.Action;
  if Action <> nil then
    if Action.IsTerminating then
    begin
      FAction := Action;
      FActionOwner := nil;
      FDirectiveWins := True;
    end
    else
      FPreActions := FPreActions + [Action];
end;

procedure TParseOperation.ParseSubcommand;
var
  Command: ICommand;
  Node: ICommandResult;
begin
  Command := FItems[FIndex].Token.Symbol as ICommand;
  Node := TCommandResult.Create(FTree, FInnermost, Command, FItems[FIndex].Token);
  FTree.Add(Command.AsObject, Node);
  FInnermost := Node;
  FArgumentIndex := 0;
  FArgumentCount := 0;
  Inc(FIndex);
end;

procedure TParseOperation.ParseOption;
var
  Option: IOption;
  Action: ICommandLineAction;
  Found: ISymbolResult;
  Node: TOptionResult;
begin
  Option := FItems[FIndex].Token.Symbol as IOption;
  Found := FTree.Find(Option.AsObject);
  if Found = nil then
  begin
    // The first time it is named is when its action joins the invocation: a terminating one wins it,
    // the last one named if there are several, and one that is not runs before the winner. Unless a
    // directive already won: then no option's action joins, the help and the version included.
    Action := Option.Action;
    if (Action <> nil) and not FDirectiveWins then
      if Action.IsTerminating then
      begin
        FAction := Action;
        FActionOwner := Option;
      end
      else
        FPreActions := FPreActions + [Action];
    Found := TOptionResult.Create(FTree, FInnermost, Option);
    FTree.Add(Option.AsObject, Found);
  end;
  Node := Found as TOptionResult;
  Node.Named(FItems[FIndex].Token);
  Inc(FIndex);
  while (FIndex < Length(FItems)) and (FItems[FIndex].Kind = TItemKind.OptionValue) do
  begin
    Node.AddToken(FItems[FIndex].Token);
    Inc(FIndex);
  end;
end;

procedure TParseOperation.ParseArgument;
var
  Arguments: TArray<IArgument>;
  Argument: IArgument;
  Found: ISymbolResult;
  Placed: Boolean;
begin
  // Positionals are handed out in order, each up to the maximum its arity admits.
  Arguments := FInnermost.Command.Arguments;
  Placed := False;
  while FArgumentIndex < Length(Arguments) do
  begin
    Argument := Arguments[FArgumentIndex];
    if FArgumentCount < Argument.Arity.MaximumNumberOfValues then
    begin
      FItems[FIndex].Token.Symbol := Argument;
      Found := FTree.Find(Argument.AsObject);
      if Found = nil then
      begin
        Found := TArgumentResult.Create(FTree, FInnermost, Argument);
        FTree.Add(Argument.AsObject, Found);
      end;
      (Found as TSymbolResult).AddToken(FItems[FIndex].Token);
      (FInnermost as TSymbolResult).AddToken(FItems[FIndex].Token);
      Inc(FArgumentCount);
      Placed := True;
      Break;
    end;
    FArgumentCount := 0;
    Inc(FArgumentIndex);
  end;
  if not Placed then
    Unmatched(FItems[FIndex].Token.Value, FormatText(TCommandLineText.UnrecognizedCommandOrArgument,
      [FItems[FIndex].Token.Value]));
  Inc(FIndex);
end;

procedure TParseOperation.ValidateCommand(const Node: ICommandResult; Complete: Boolean);
var
  Option: IOption;
  Argument: IArgument;
  Found: ISymbolResult;
  CommandValidator: TCommandValidator;
  OptionValidator: TOptionValidator;
  ArgumentValidator: TArgumentValidator;
  ValueResult: IArgumentResult;
  ErrorsBefore: Integer;
begin
  // The validations of the command run on the innermost one only, and first: an error of theirs says the
  // invocation makes no sense, and the errors of its parts would only repeat it.
  if Complete then
  begin
    ErrorsBefore := FTree.ErrorCount;
    for CommandValidator in Node.Command.Validators do
      CommandValidator(Node);
    if FTree.ErrorCount <> ErrorsBefore then
      Exit;
  end;

  // Each option and each argument: its arity, then its validations, then its conversion. An error at a
  // step stops the steps behind it.
  for Option in Node.Command.Options do
  begin
    if not Complete and not (Option.IsRecursive or Option.HasDefaultValue or Supports(Option, IVersionOption)) then
      Continue;
    Found := FTree.Find(Option.AsObject);
    if Found = nil then
    begin
      // What the invocation had to carry and did not. A declared default satisfies the demand,
      // because the value exists -- which is what "required" asks for.
      if not (Option.IsRequired or Option.HasDefaultValue) then
        Continue;
      Found := TOptionResult.Create(FTree, Node, Option);
      FTree.Add(Option.AsObject, Found);
      if not Option.HasDefaultValue then
      begin
        (Found as TValuedResult).Fail(FormatText(TCommandLineText.RequiredOptionWasNotProvided, [Option.Name]));
        Continue;
      end;
    end;
    if not (Found as TValuedResult).CheckArity then
      Continue;
    ErrorsBefore := FTree.ErrorCount;
    for OptionValidator in Option.Validators do
      OptionValidator(Found as IOptionResult);
    if FTree.ErrorCount <> ErrorsBefore then
      Continue;
    // Then the validations of the value, on the argument the option carries it in.
    ValueResult := (Found as TValuedResult).ValueResult;
    for ArgumentValidator in (Found as TValuedResult).FValued.ValueValidators do
      ArgumentValidator(ValueResult);
    if FTree.ErrorCount <> ErrorsBefore then
      Continue;
    (Found as TValuedResult).Validate;
  end;

  for Argument in Node.Command.Arguments do
  begin
    if not Complete and not Argument.HasDefaultValue then
      Continue;
    Found := FTree.Find(Argument.AsObject);
    if Found = nil then
    begin
      if not (Argument.HasDefaultValue or Argument.Arity.DemandsValue) then
        Continue;
      Found := TArgumentResult.Create(FTree, Node, Argument);
      FTree.Add(Argument.AsObject, Found);
      if not Argument.HasDefaultValue then
      begin
        (Found as TValuedResult).Fail(FormatText(TCommandLineText.CommandRequiredArgumentMissing,
          [Argument.Name, Node.Command.Name]));
        Continue;
      end;
      (Found as TArgumentResult).TakeDefault;
    end;
    if not (Found as TValuedResult).CheckArity then
      Continue;
    ErrorsBefore := FTree.ErrorCount;
    for ArgumentValidator in Argument.Validators do
      ArgumentValidator(Found as IArgumentResult);
    if FTree.ErrorCount <> ErrorsBefore then
      Continue;
    (Found as TValuedResult).Validate;
  end;
end;

procedure TParseOperation.KeepErrorsOf(const Owner: IOption);
var
  Kept: TArray<TParseError>;
  Error: TParseError;
  AsOption: IOptionResult;
begin
  Kept := nil;
  // A winner no option owns keeps none.
  if Owner <> nil then
    for Error in FTree.Errors do
      if Supports(Error.SymbolResult, IOptionResult, AsOption) and (AsOption.Option.AsObject = Owner.AsObject) then
        Kept := Kept + [Error];
  FTree.SetErrors(Kept);
end;

procedure TParseOperation.DropErrorsOf(const Node: ICommandResult);
var
  Kept: TArray<TParseError>;
  Error: TParseError;
begin
  Kept := nil;
  for Error in FTree.Errors do
    if (Error.SymbolResult = nil) or ((Error.SymbolResult as TObject) <> (Node as TObject)) then
      Kept := Kept + [Error];
  FTree.SetErrors(Kept);
end;

procedure TParseOperation.AddPreActionsForImplicitOptions;
var
  Node: ISymbolResult;
  AsOption: IOptionResult;
  Action: ICommandLineAction;
begin
  // An option that took its default runs its action too, unless it terminates: a default is not
  // a reason to replace the action of the command.
  for Node in FTree.Results do
    if Supports(Node, IOptionResult, AsOption) and AsOption.IsImplicit then
    begin
      Action := AsOption.Option.Action;
      if (Action <> nil) and not Action.IsTerminating and (Action <> FAction) then
        FPreActions := FPreActions + [Action];
    end;
end;

procedure TParseOperation.ValidateAndAddDefaultResults;
var
  Node: ISymbolResult;
  Command: ICommand;
  CommandAction: ICommandLineAction;
begin
  ValidateCommand(FInnermost, True);
  Node := FInnermost.Parent;
  while Node <> nil do
  begin
    ValidateCommand(Node as ICommandResult, False);
    Node := Node.Parent;
  end;

  if FAction = nil then
  begin
    // A command that only groups is not an invocation of its own; one with an action is.
    Command := FInnermost.Command;
    CommandAction := Command.Action;
    if (CommandAction = nil) and (Length(Command.Subcommands) > 0) then
      FTree.InsertFirstError(FormatText(TCommandLineText.RequiredCommandWasNotProvided, []), FInnermost);
    if (CommandAction <> nil) and CommandAction.ClearsParseErrors then
      DropErrorsOf(FInnermost)
    else if FTree.ErrorCount > 0 then
      FAction := NewParseErrorAction;
  end
  else if (FTree.ErrorCount > 0) and FAction.ClearsParseErrors then
  begin
    // The winner keeps only the errors of the option it belongs to, and the actions of the options
    // that took their default do not join it.
    KeepErrorsOf(FActionOwner);
    Exit;
  end;

  AddPreActionsForImplicitOptions;
end;

function TParseOperation.IsCapturingRemainingTokens(Kind: TItemKind): Boolean;
var
  Arguments: TArray<IArgument>;
  Index, Count: Integer;
begin
  // The next argument to fill, past the ones whose arity is full.
  Arguments := FInnermost.Command.Arguments;
  Index := FArgumentIndex;
  Count := FArgumentCount;
  while (Index < Length(Arguments)) and (Count >= Arguments[Index].Arity.MaximumNumberOfValues) do
  begin
    Count := 0;
    Inc(Index);
  end;
  // It takes a token of any kind once it has started, and a positional to start with: an option before the
  // first value is still an option.
  Result := (Index < Length(Arguments)) and Arguments[Index].CapturesRemainingTokens and
    ((Kind = TItemKind.Positional) or (Index > 0) or (Count > 0));
end;

function TParseOperation.Run: IParseResult;
var
  Outcome: TParseResult;
  Tokens: TArray<TToken>;
  Item: TItem;
begin
  Outcome := TParseResult.Create(FRoot, FConfiguration, FTree, FRootResult);
  Result := Outcome;
  // The directives first, all of them: the action one of them wins has to be known before any option is read.
  while (FIndex < Length(FItems)) and (FItems[FIndex].Kind = TItemKind.Directive) do
    ParseDirective;
  while FIndex < Length(FItems) do
  begin
    // An argument that keeps the tail takes every token as one more value; `--` too, once it has started.
    if IsCapturingRemainingTokens(FItems[FIndex].Kind) and
      ((FItems[FIndex].Kind <> TItemKind.DoubleDash) or FCapturing) then
    begin
      FCapturing := True;
      ParseArgument;
      Continue;
    end;
    FCapturing := False;
    case FItems[FIndex].Kind of
      TItemKind.BadGroup:
        begin
          FInnermost.AddError(FItems[FIndex].Error);
          Inc(FIndex);
        end;
      TItemKind.UnknownOption:
        begin
          Unmatched(FItems[FIndex].Token.Value, FormatText(TCommandLineText.UnrecognizedCommandOrArgument,
            [FItems[FIndex].Token.Value]));
          Inc(FIndex);
        end;
      TItemKind.CommandName:
        ParseSubcommand;
      TItemKind.OptionName:
        ParseOption;
      TItemKind.DoubleDash:
        Inc(FIndex);
    else
      ParseArgument;
    end;
  end;
  ValidateAndAddDefaultResults;

  Tokens := nil;
  for Item in FItems do
    Tokens := Tokens + [Item.Token];
  Outcome.Close(FInnermost, Tokens, FAction, FActionOwner, FPreActions);
end;

function ParseInvocation(const Root: ICommand; const Args: TArray<string>; const Configuration: TParserConfiguration;
  FromCommandLine: Boolean): IParseResult;
var
  Items: TArray<TItem>;
  ReadErrors: TArray<string>;
  RootTokenValue: string;
  Operation: TParseOperation;
begin
  // At every parse, because a grammar is only whole here: an alias can be added to an option after the option
  // was added to its command.
  DemandReservedNamesFree(Root, Configuration.EnableCaseInsensitiveNames);
  if Configuration.EnableCaseInsensitiveNames then
    DemandNamesDistinctIgnoringCase(Root, InheritedOptionsOf(Root));
  Items := Tokenize(Root, Args, Configuration, FromCommandLine, RootTokenValue, ReadErrors);
  Operation := TParseOperation.Create(Root, Configuration, RootTokenValue, Items, ReadErrors);
  try
    Result := Operation.Run;
  finally
    Operation.Free;
  end;
end;

function ResultValues(const Found: ISymbolResult; out FromDefault, Whole: Boolean): TArray<TValue>;
begin
  Result := ((Found as TObject) as TValuedResult).ReadValues(FromDefault, Whole);
end;

end.
