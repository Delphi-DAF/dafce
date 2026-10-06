unit Daf.CommandLine.Parsing;

{
  The parse, in the three steps System.CommandLine takes. The invocation is read into typed tokens;
  the tokens build a tree with one result per symbol the invocation reached; and the tree is
  validated -- the innermost command whole, its parents only in their recursive options and in the
  ones with a default.

  Reading follows the rules of DAF: the Windows prefix, a group of one-letter options, a value glued to
  its option, and an option that takes the next token whatever it is.

  Only two routines are published here. The contract the results answer to lives in Daf.CommandLine,
  because interfaces that name each other have to share one interface section.
}

interface

uses
  System.Rtti,
  Daf.CommandLine;

/// <summary>Parses an invocation against the grammar of <c>Root</c>.</summary>
function ParseInvocation(const Root: IRootCommand; const Args: TArray<string>): IParseResult;

/// <summary>The values of an option's or an argument's result.</summary>
/// <remarks>
///   The ones the validation converted, or the default it took: then <c>FromDefault</c> is True and the
///   one value is the whole default. A result the validation never reached -- an option of a parent
///   that is neither recursive nor defaulted -- is converted now, once, without the accepted values or
///   the validations, and its error joins the tree as the validation's would.
///
///   Raises <c>ECommandLine</c> with the error of a result that has one.
/// </remarks>
function ResultValues(const Found: ISymbolResult; out FromDefault: Boolean): TArray<TValue>;

implementation

uses
  System.SysUtils,
  System.Classes,
  Daf.Nullable,
  Daf.Application.Version,
  Daf.CommandLine.Tokens,
  Daf.CommandLine.Values,
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
    FState: TConversionState;
    FFailure: string;
    FValidated: Boolean;
    procedure ConvertTokens(Accept: Boolean);
  protected
    /// <summary>Why its tokens are not the number it admits; empty when they are.</summary>
    function ArityError: string; virtual; abstract;
    function IsImplicitResult: Boolean; virtual; abstract;
  public
    constructor Create(const ATree: ISymbolResultTree; const AParent: ISymbolResult; AValued: TValuedSymbol);
    /// <summary>An error of the parse that also stands for its value: reading it raises this.</summary>
    procedure Fail(const Message: string);
    /// <summary>
    ///   Its turn in the validation: the default when it is implicit; otherwise its arity, then each
    ///   token through the accepted values, the conversion and the validations. Runs once.
    /// </summary>
    procedure Validate;
    function ReadValues(out FromDefault: Boolean): TArray<TValue>;
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
  protected
    function ArityError: string; override;
    function IsImplicitResult: Boolean; override;
  public
    constructor Create(const ATree: ISymbolResultTree; const AParent: ISymbolResult; const AArgument: IArgument);
    /// <summary>The validation created it for its default.</summary>
    procedure TakeDefault;
    /// <summary>An error of an argument is an error of its value too, as in .NET.</summary>
    procedure AddError(const Message: string); override;
    function Argument: IArgument;
    function IsImplicit: Boolean;
  end;

  TParseResult = class(TInterfacedObject, IParseResult)
  private
    FRoot: IRootCommand;
    FTree: ISymbolResultTree;
    FRootResult: ICommandResult;
    FCommandResult: ICommandResult;
    FTokens: TArray<TToken>;
    FErrorsShared: Boolean;
    FHelpRequested: Boolean;
    FHelpText: string;
    FVersionRequested: Boolean;
    FInvocationConfiguration: TInvocationConfiguration;
    function ResultOf(const Symbol: ISymbol): ISymbolResult;
  public
    constructor Create(const ARoot: IRootCommand; const ATree: ISymbolResultTree; const ARootResult: ICommandResult);
    procedure Close(const Innermost: ICommandResult; const Tokens: TArray<TToken>);
    procedure RequestHelp(const ForCommand: ICommand);
    procedure RequestVersion;
    function Command: ICommand;
    function CommandResult: ICommandResult;
    function RootCommandResult: ICommandResult;
    function Tokens: TArray<TToken>;
    function GetResult(const Option: IOption): IOptionResult; overload;
    function GetResult(const Argument: IArgument): IArgumentResult; overload;
    function GetResult(const Command: ICommand): ICommandResult; overload;
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
    function InvocationConfiguration: TInvocationConfiguration;
    function Invoke: Integer; overload;
    function Invoke(const Configuration: TInvocationConfiguration): Integer; overload;
  end;

  /// <summary>The grammar in scope, answering the three questions the rewriting asks.</summary>
  /// <remarks>
  ///   It is mutable and reused as the reading descends, because which options are
  ///   declared depends on the command reached: `mytool build native -fdx` can only be
  ///   split once `native` is where we are.
  /// </remarks>
  TScopeKnowledge = class(TInterfacedObject, IKnownTokens)
  private
    FCurrent: ICommand;
    FAvailable: TArray<IOption>;
    FAtRoot: Boolean;
  public
    procedure Enter(const Command: ICommand; const Available: TArray<IOption>; AtRoot: Boolean);
    function Knows(const Token: string): Boolean;
    function KnowsShort(const Letter: Char): Boolean;
    function ShortTakesValue(const Letter: Char): Boolean;
  end;

  {$SCOPEDENUMS ON}
  /// <summary>What reading made of a token, finer than its token type.</summary>
  TItemKind = (Positional, CommandName, OptionName, OptionValue, DoubleDash, UnknownOption, BadGroup, Help,
    Version);
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
    FRoot: IRootCommand;
    FItems: TArray<TItem>;
    FIndex: Integer;
    FTree: ISymbolResultTree;
    FRootResult: ICommandResult;
    FInnermost: ICommandResult;
    FArgumentIndex: Integer;
    FArgumentCount: Integer;
    procedure Unmatched(const Token, Message: string);
    procedure ParseSubcommand;
    procedure ParseOption;
    procedure ParseArgument;
    procedure ValidateWhatWasRead;
    procedure ValidateCommand(const Node: ICommandResult; Complete: Boolean);
    procedure ValidateAndAddDefaultResults;
  public
    constructor Create(const Root: IRootCommand; const Items: TArray<TItem>; const ReadErrors: TArray<string>);
    function Run: IParseResult;
  end;

procedure WriteError(const Configuration: TInvocationConfiguration; const Text: string);
begin
  if Assigned(Configuration.Error) then
    Configuration.Error.WriteLine(Text)
  else
    Writeln(System.ErrOutput, Text);
end;

// Finds an option by token among the command's own and those inherited from ancestors
// that declared them recursive.
function FindOption(const Current: ICommand; const Inherited_: TArray<IOption>; const Token: string): IOption;
var
  Candidate: IOption;
begin
  for Candidate in Current.Options do
    if Candidate.Matches(Token) then
      Exit(Candidate);
  for Candidate in Inherited_ do
    if Candidate.Matches(Token) then
      Exit(Candidate);
  Result := nil;
end;

function FindSubcommand(const Current: ICommand; const Token: string): ICommand;
var
  Candidate: ICommand;
begin
  for Candidate in Current.Subcommands do
    if Candidate.Matches(Token) then
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

function NamesOf(const Commands: TArray<ICommand>): string;
var
  Names: TArray<string>;
  Candidate: ICommand;
begin
  Names := nil;
  for Candidate in Commands do
    Names := Names + [Candidate.Name];
  Result := string.Join(', ', Names);
end;

function IsHelpToken(const Token: string): Boolean;
var
  Known: string;
begin
  for Known in HelpTokens do
    if SameText(Token, Known) then
      Exit(True);
  Result := False;
end;

function IsVersionToken(const Token: string): Boolean;
begin
  Result := SameText(Token, VersionToken);
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

procedure TValuedResult.ConvertTokens(Accept: Boolean);
var
  Error: string;
  Token: TToken;
  Value: TValue;
  Values: TArray<TValue>;
  Converted: Boolean;
begin
  // When the arity fails nothing else is reported for it, as in .NET.
  Error := ArityError;
  if Error <> '' then
  begin
    Fail(Error);
    Exit;
  end;
  Values := nil;
  for Token in FTokens do
  begin
    if Accept then
      Converted := FValued.TryAcceptValue(Token.Value, Value, Error)
    else
      Converted := FValued.Convert(Token.Value, Value, Error);
    if not Converted then
    begin
      Fail(Error);
      Exit;
    end;
    Values := Values + [Value];
  end;
  // A flag named with no value is the whole statement: --verbose says true.
  if (Length(Values) = 0) and FValued.IsFlag then
  begin
    Value := TValue.From<Boolean>(True);
    Values := [Value];
  end;
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
    // The default is part of the result, so it is calculated here, read or not.
    Boxed := FValued.BoxedDefault;
    FValues := [Boxed];
    FFromDefault := True;
    FState := TConversionState.Converted;
    Exit;
  end;
  ConvertTokens(True);
end;

function TValuedResult.ReadValues(out FromDefault: Boolean): TArray<TValue>;
begin
  if FState = TConversionState.Pending then
  begin
    if IsImplicitResult then
      Validate
    else
      // Read without the accepted values or the validations, which are the validation's to run:
      // the useValidators: false of .NET.
      ConvertTokens(False);
  end;
  if FState = TConversionState.Failed then
    raise ECommandLine.Create(FFailure);
  FromDefault := FFromDefault;
  Result := FValues;
end;

{ TOptionResult }

constructor TOptionResult.Create(const ATree: ISymbolResultTree; const AParent: ISymbolResult;
  const AOption: IOption);
begin
  inherited Create(ATree, AParent, AOption.AsObject as TValuedSymbol);
  FOption := AOption;
end;

procedure TOptionResult.Named(const Token: TToken);
begin
  if FIdentifierToken.IsNull then
    FIdentifierToken := Token;
  Inc(FIdentifierTokenCount);
end;

function TOptionResult.ArityError: string;
begin
  // Repeating an option that admits one value is an ambiguous invocation, and an ambiguous
  // invocation has to fail. Quietly keeping one of them is the same sin as reading 80xyz as 80:
  // a value the parser chose on its own.
  if not FOption.Arity.AllowsMany and (FIdentifierTokenCount > 1) then
    Exit(Format(SOptionRepeated, [FOption.Name, FOption.Arity.Describe]));
  if not FOption.IsFlag and (Length(FTokens) = 0) then
    Exit(Format(SOptionRequiresValue, [FOption.Name, FOption.TypeName]));
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
    Exit(Format(SArgumentRequired, [FArgument.Name]));
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

{ TParseResult }

constructor TParseResult.Create(const ARoot: IRootCommand; const ATree: ISymbolResultTree;
  const ARootResult: ICommandResult);
begin
  inherited Create;
  FRoot := ARoot;
  FTree := ATree;
  FRootResult := ARootResult;
  FCommandResult := ARootResult;
end;

procedure TParseResult.Close(const Innermost: ICommandResult; const Tokens: TArray<TToken>);
begin
  FCommandResult := Innermost;
  FTokens := Tokens;
  FErrorsShared := FTree.ErrorCount > 0;
end;

procedure TParseResult.RequestHelp(const ForCommand: ICommand);
begin
  FHelpRequested := True;
  FHelpText := ForCommand.HelpText;
end;

procedure TParseResult.RequestVersion;
begin
  FVersionRequested := True;
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
  Result := FHelpRequested;
end;

function TParseResult.HelpText: string;
begin
  Result := FHelpText;
end;

function TParseResult.IsVersionRequested: Boolean;
begin
  Result := FVersionRequested;
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
var
  Action: TCommandAction;
begin
  FInvocationConfiguration := Configuration;

  if FVersionRequested then
    Action := FRoot.VersionAction
  else if FHelpRequested then
    Action := FRoot.HelpAction
  else if not Succeeded then
  begin
    WriteError(Configuration, ErrorText);
    Exit(TCommandLineExit.InvocationError);
  end
  else
    Action := Command.Action();

  if not Assigned(Action) then
    Exit(TCommandLineExit.Ok);

  if not Configuration.EnableDefaultExceptionHandler then
    Exit(Action(Self));

  try
    Result := Action(Self);
  except
    on E: Exception do
    begin
      WriteError(Configuration, E.ClassName + ': ' + E.Message);
      Result := TCommandLineExit.Failure;
    end;
  end;
end;

{ TScopeKnowledge }

procedure TScopeKnowledge.Enter(const Command: ICommand; const Available: TArray<IOption>; AtRoot: Boolean);
begin
  FCurrent := Command;
  FAvailable := Available;
  FAtRoot := AtRoot;
end;

function TScopeKnowledge.Knows(const Token: string): Boolean;
begin
  // The two built-in tokens count as known. Otherwise `/help` would not reach the help and
  // `-h` could be swallowed into a bundle, which is the kind of hole a rewriting opens by only
  // ever looking at what the application declared.
  Result := (FindOption(FCurrent, FAvailable, Token) <> nil) or IsHelpToken(Token) or
    (FAtRoot and IsVersionToken(Token));
end;

function TScopeKnowledge.KnowsShort(const Letter: Char): Boolean;
begin
  // Declared only, built-ins out: this is what decides whether a token is a group, and a help
  // letter the application never declared does not make `-Fecha` one.
  Result := FindOption(FCurrent, FAvailable, ShortPrefix + Letter) <> nil;
end;

function TScopeKnowledge.ShortTakesValue(const Letter: Char): Boolean;
var
  Option: IOption;
begin
  Option := FindOption(FCurrent, FAvailable, ShortPrefix + Letter);
  // A built-in answers nil and is a flag: asking for help takes no value.
  Result := (Option <> nil) and not Option.IsFlag;
end;

{ Reading }

// The first step: every token typed, in the scope of the command reached when it is read. Which
// tokens an option takes is decided here, because it depends on the token and not on the tree: a
// value glued to it, a boolean literal behind a flag, and anything at all behind any other option.
function Tokenize(const Root: IRootCommand; const Args: TArray<string>; out ReadErrors: TArray<string>): TArray<TItem>;
var
  Items: TArray<TItem>;
  Tokens: TArray<string>;
  Current: ICommand;
  Available: TArray<IOption>;
  Scope: TScopeKnowledge;
  Known: IKnownTokens;
  Index: Integer;
  Token, OptionName, InlineValue, Next: string;
  HasInline, Ended, Took: Boolean;
  Step: TTokenRewrite;
  Option: IOption;
  Subcommand: ICommand;
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
  // Response files are resolved before anything else is decided about any token,
  // because all they do is contribute tokens and nothing else.
  Tokens := TResponseFiles.Expand(Args, ReadErrors);
  Current := Root;
  Available := RecursiveOptionsOf(Current);
  Scope := TScopeKnowledge.Create;
  Known := Scope;
  Scope.Enter(Current, Available, True);
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
    Step := TTokenNormalizer.Rewrite(Token, Known);
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

    // An application that declared its own --help wins, so the built-in only answers when
    // nothing declared does. --version belongs to the ROOT, as in .NET: a subcommand does not
    // answer it, because a version is the tool's and not each order's.
    if IsHelpToken(OptionName) and (FindOption(Current, Available, OptionName) = nil) then
    begin
      Emit(Token, TTokenType.Option, nil, TItemKind.Help);
      Inc(Index);
      Continue;
    end;
    if IsVersionToken(OptionName) and (Current.AsObject = Root.AsObject) and
      (FindOption(Current, Available, OptionName) = nil) then
    begin
      Emit(Token, TTokenType.Option, nil, TItemKind.Version);
      Inc(Index);
      Continue;
    end;

    if TTokenNormalizer.LooksLikeOption(Token) then
    begin
      Option := FindOption(Current, Available, OptionName);
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
        // A flag may carry its value explicitly -- `--minify false` -- because its arity admits
        // none or one. Only a boolean literal counts as that value; anything else is somebody
        // else's token.
        Took := (Index + 1 < Length(Tokens)) and TCommandLineValue.IsBooleanLiteral(Tokens[Index + 1]);
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

    Subcommand := FindSubcommand(Current, Token);
    if Subcommand <> nil then
    begin
      Emit(Token, TTokenType.Command, Subcommand, TItemKind.CommandName);
      Current := Subcommand;
      Available := Available + RecursiveOptionsOf(Current);
      Scope.Enter(Current, Available, False);
      Inc(Index);
      Continue;
    end;

    Emit(Token, TTokenType.Argument, nil, TItemKind.Positional);
    Inc(Index);
  end;
  Result := Items;
end;

{ TParseOperation }

constructor TParseOperation.Create(const Root: IRootCommand; const Items: TArray<TItem>;
  const ReadErrors: TArray<string>);
var
  RootToken: TToken;
  Error: string;
begin
  inherited Create;
  FRoot := Root;
  FItems := Items;
  FTree := TSymbolResultTree.Create;
  // An error of reading the invocation belongs to no symbol: none had been reached.
  for Error in ReadErrors do
    FTree.AddError(Error, nil);
  RootToken.Value := Root.Name;
  RootToken.TokenType := TTokenType.Command;
  RootToken.Symbol := Root;
  FRootResult := TCommandResult.Create(FTree, nil, Root, RootToken);
  FTree.Add(Root.AsObject, FRootResult);
  FInnermost := FRootResult;
end;

procedure TParseOperation.Unmatched(const Token, Message: string);
begin
  // A wrapper hands on what is not its own; anything else refuses it, which is the default.
  if FInnermost.Command.TreatsUnmatchedTokensAsErrors then
    FInnermost.AddError(Message)
  else
    FTree.AddUnmatched(Token);
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
  Found: ISymbolResult;
  Node: TOptionResult;
begin
  Option := FItems[FIndex].Token.Symbol as IOption;
  Found := FTree.Find(Option.AsObject);
  if Found = nil then
  begin
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
    Unmatched(FItems[FIndex].Token.Value, Format(SUnexpectedArgument,
      [FItems[FIndex].Token.Value, FInnermost.Command.FullPath]));
  Inc(FIndex);
end;

procedure TParseOperation.ValidateWhatWasRead;
var
  Node: ISymbolResult;
begin
  // Help and version end the parse where they appear, before the grammar is validated: asking how
  // the syntax works is not an invalid invocation. What was read up to there is still checked, and
  // nothing is reported missing.
  for Node in FTree.Results do
    if (Node as TObject) is TValuedResult then
      ((Node as TObject) as TValuedResult).Validate;
end;

procedure TParseOperation.ValidateCommand(const Node: ICommandResult; Complete: Boolean);
var
  Option: IOption;
  Argument: IArgument;
  Found: ISymbolResult;
begin
  for Option in Node.Command.Options do
  begin
    if not Complete and not (Option.IsRecursive or Option.HasDefaultValue) then
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
        (Found as TValuedResult).Fail(Format(SOptionRequired, [Option.Name]));
        Continue;
      end;
    end;
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
        (Found as TValuedResult).Fail(Format(SArgumentRequired, [Argument.Name]));
        Continue;
      end;
      (Found as TArgumentResult).TakeDefault;
    end;
    (Found as TValuedResult).Validate;
  end;
end;

procedure TParseOperation.ValidateAndAddDefaultResults;
var
  Node: ISymbolResult;
  Command: ICommand;
  Action: TCommandAction;
begin
  ValidateCommand(FInnermost, True);
  Node := FInnermost.Parent;
  while Node <> nil do
  begin
    ValidateCommand(Node as ICommandResult, False);
    Node := Node.Parent;
  end;

  // A command that only groups is not an invocation of its own; one with an action is.
  Command := FInnermost.Command;
  Action := Command.Action();
  if (Length(Command.Subcommands) > 0) and not Assigned(Action) then
    FTree.InsertFirstError(Format(SCommandRequiresSubcommand, [Command.FullPath, NamesOf(Command.Subcommands)]),
      FInnermost);
end;

function TParseOperation.Run: IParseResult;
var
  Outcome: TParseResult;
  Tokens: TArray<TToken>;
  Item: TItem;
  Ended: Boolean;
begin
  Outcome := TParseResult.Create(FRoot, FTree, FRootResult);
  Result := Outcome;
  Ended := False;
  while not Ended and (FIndex < Length(FItems)) do
    case FItems[FIndex].Kind of
      TItemKind.Help:
        begin
          ValidateWhatWasRead;
          Outcome.RequestHelp(FInnermost.Command);
          Ended := True;
        end;
      TItemKind.Version:
        begin
          ValidateWhatWasRead;
          Outcome.RequestVersion;
          Ended := True;
        end;
      TItemKind.BadGroup:
        begin
          FInnermost.AddError(FItems[FIndex].Error);
          Inc(FIndex);
        end;
      TItemKind.UnknownOption:
        begin
          Unmatched(FItems[FIndex].Token.Value, Format(SUnknownOption,
            [FItems[FIndex].Name, FInnermost.Command.FullPath]));
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
  if not Ended then
    ValidateAndAddDefaultResults;

  Tokens := nil;
  for Item in FItems do
    Tokens := Tokens + [Item.Token];
  Outcome.Close(FInnermost, Tokens);
end;

function ParseInvocation(const Root: IRootCommand; const Args: TArray<string>): IParseResult;
var
  Items: TArray<TItem>;
  ReadErrors: TArray<string>;
  Operation: TParseOperation;
begin
  Items := Tokenize(Root, Args, ReadErrors);
  Operation := TParseOperation.Create(Root, Items, ReadErrors);
  try
    Result := Operation.Run;
  finally
    Operation.Free;
  end;
end;

function ResultValues(const Found: ISymbolResult; out FromDefault: Boolean): TArray<TValue>;
begin
  Result := ((Found as TObject) as TValuedResult).ReadValues(FromDefault);
end;

end.
