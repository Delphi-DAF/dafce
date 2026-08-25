unit Daf.CommandLine;

{
  A port of System.CommandLine -- the library the dotnet CLI itself is built on --
  as close to the original as Delphi allows, with every divergence declared.

  The one divergence that shows in every call is how a value is read. In .NET you
  write result.GetValue(fileOption). Here you write FileOption.ValueIn(Parsed): the
  object asks the result, not the result the object. It was measured, not assumed
  (design D2, 2026-08-24): an interface cannot declare a generic method at all --
  E2535 -- and even on a class Delphi will not infer T through an IOption<T>
  parameter -- E2010. Writing it the .NET way would mean giving up counted
  interfaces AND spelling the type out by hand, which is the repetition this module
  exists to remove.

  What does NOT change is the point: the object you declared is the key you read
  with. No name is written twice, so no name can disagree with itself.
}

interface

uses
  System.SysUtils,
  System.TypInfo,
  System.Rtti,
  Daf.Nullable;

type
  ECommandLine = class(Exception);

  ICommandLineParseResult = interface;

  /// <summary>How many values a symbol admits: a minimum and a maximum.</summary>
  /// <remarks>
  ///   The five named values are .NET's. It is rarely written by hand because it is
  ///   derived from the declared type -- a boolean admits none or one, anything else
  ///   demands exactly one -- and it is what makes "required" and "many values" the
  ///   same idea seen from its two ends (design D6).
  /// </remarks>
  TArity = record
  strict private
    FMin: Integer;
    FMax: Integer;
  public
    const Unbounded = MaxInt;
    class function Create(AMin, AMax: Integer): TArity; static;
    class function Zero: TArity; static;
    class function ZeroOrOne: TArity; static;
    class function ExactlyOne: TArity; static;
    class function ZeroOrMore: TArity; static;
    class function OneOrMore: TArity; static;
    function Min: Integer;
    function Max: Integer;
    /// <summary>True when more than one value fits, so repeating is not an error.</summary>
    function AllowsMany: Boolean;
    /// <summary>True when at least one value has to be there.</summary>
    function DemandsValue: Boolean;
    /// <summary>Says the arity in words, for an error message that has to name it.</summary>
    function Describe: string;
  end;

  /// <summary>Anything the grammar is made of: a command, an option, an argument.</summary>
  ISymbol = interface
    ['{7C4B1A20-0001-4F31-9C10-4B0E2D5A0001}']
    /// <summary>
    ///   The implementing object. It is the identity used to look a symbol up in a
    ///   parse result: two interface references on the same object answer the same
    ///   TObject, which no comparison of interface pointers can promise.
    /// </summary>
    function AsObject: TObject;
    function Name: string;
    function Aliases: TArray<string>;
    function Description: string;
    /// <summary>
    ///   True when <c>Token</c> is this symbol's name or one of its aliases.
    ///   Compared WITHOUT regard to case -- a declared divergence from .NET, which is
    ///   case-sensitive by POSIX convention. See design D7 for the reason.
    /// </summary>
    function Matches(const Token: string): Boolean;
  end;

  /// <summary>A symbol that carries a value, and therefore a declared type.</summary>
  /// <remarks>
  ///   <c>TryAccept</c> is where the type erasure is undone. The parser holds
  ///   <c>ISymbol</c> and does not know <c>T</c>; the generic descendant does. That is
  ///   what lets a bad value be a <b>parse</b> error -- reported once, naming the
  ///   symbol and the text -- instead of a surprise at the moment somebody reads it.
  /// </remarks>
  IValued = interface(ISymbol)
    ['{7C4B1A20-0002-4F31-9C10-4B0E2D5A0002}']
    function TryAccept(const Raw: string; out Error: string): Boolean;
    /// <summary>True when the token is the whole statement: <c>--verbose</c>.</summary>
    function IsFlag: Boolean;
    function TypeName: string;
    function Arity: TArity;
    /// <summary>
    ///   The values this symbol accepts, or empty when it does not restrict them. An
    ///   enumerated type fills this in by itself.
    /// </summary>
    function AcceptedValues: TArray<string>;
    /// <summary>True when a default was declared, which is what satisfies "required".</summary>
    function HasDefault: Boolean;
    /// <summary>The declared default rendered as text, for the help.</summary>
    function DefaultText: string;
  end;

  IOption = interface(IValued)
    ['{7C4B1A20-0003-4F31-9C10-4B0E2D5A0003}']
    /// <summary>True when every descendant of the declaring command also offers it.</summary>
    function IsRecursive: Boolean;
    /// <summary>
    ///   True when the invocation has to carry it. Separate from arity, as in .NET: an
    ///   option that takes one value may be obligatory or not, and arity cannot say.
    /// </summary>
    function IsRequired: Boolean;
  end;

  IArgument = interface(IValued)
    ['{7C4B1A20-0004-4F31-9C10-4B0E2D5A0004}']
  end;

  ICommand = interface(ISymbol)
    ['{7C4B1A20-0005-4F31-9C10-4B0E2D5A0005}']
    // Fluency lives in the interface, never in a class helper: only one helper per
    // type is in scope and helpers never apply to interfaces, so a fluent API spread
    // over units breaks -- silently, last uses wins -- the moment someone reorders a
    // uses clause (design D3).
    function Add(const Option: IOption): ICommand; overload;
    function Add(const Argument: IArgument): ICommand; overload;
    function Add(const Subcommand: ICommand): ICommand; overload;
    /// <summary>Convenience for several at once, the nearest thing to a C# collection initializer.</summary>
    function Add(const Symbols: array of ISymbol): ICommand; overload;
    function WithAlias(const Alias: string): ICommand;
    function Describe(const Text: string): ICommand;
    function Options: TArray<IOption>;
    function Arguments: TArray<IArgument>;
    function Subcommands: TArray<ICommand>;
    /// <summary>How this command is invoked, e.g. <c>mytool build</c>.</summary>
    function FullPath: string;
    /// <summary>
    ///   The help text, rendered from the grammar every time it is asked for. Nothing
    ///   composed is stored anywhere, so it cannot drift from what the program accepts
    ///   (design D4).
    /// </summary>
    function HelpText: string;
  end;

  IRootCommand = interface(ICommand)
    ['{7C4B1A20-0006-4F31-9C10-4B0E2D5A0006}']
    function Parse(const Args: TArray<string>): ICommandLineParseResult; overload;
    /// <summary>Parses the process command line, <c>ParamStr(1)</c> onwards.</summary>
    function Parse: ICommandLineParseResult; overload;
    /// <summary>
    ///   Declares the version this tool answers with. It lives on the ROOT and not on
    ///   each command, as in .NET: a version belongs to the tool, not to its orders.
    /// </summary>
    function WithVersion(const Version: string): IRootCommand;
    function VersionText: string;
  end;

  ICommandLineParseResult = interface
    ['{7C4B1A20-0007-4F31-9C10-4B0E2D5A0007}']
    /// <summary>The leaf command the invocation reached.</summary>
    function Command: ICommand;
    function Succeeded: Boolean;
    function Errors: TArray<string>;
    function ErrorText: string;
    /// <summary>True when help was asked for. Asking is a valid outcome, not a failure.</summary>
    function HelpRequested: Boolean;
    function HelpText: string;
    /// <summary>True when the version was asked for. Also an outcome, not a failure.</summary>
    function VersionRequested: Boolean;
    function VersionText: string;
    /// <summary>True when the invocation carried this symbol at all.</summary>
    function WasProvided(const Symbol: ISymbol): Boolean;
    /// <summary>
    ///   The raw text bound to a symbol; absent when the invocation did not carry it.
    ///   Absent and present-but-empty are different answers, which the parked module
    ///   could not tell apart.
    /// </summary>
    function RawOf(const Symbol: ISymbol): TNullable<string>;
    /// <summary>Every raw value bound to a symbol, for an arity that admits many.</summary>
    function RawValuesOf(const Symbol: ISymbol): TArray<string>;
  end;

  /// <summary>An option whose value is read back with the very object declared here.</summary>
  IOption<T> = interface(IOption)
    /// <summary>
    ///   The declared default, or the parsed value when the invocation carried one.
    /// </summary>
    /// <remarks>
    ///   Reads as <c>FileOption.ValueIn(Parsed)</c>. The order is inverted with respect
    ///   to .NET's <c>result.GetValue(fileOption)</c> on purpose -- see the unit header.
    /// </remarks>
    function ValueIn(const Parsed: ICommandLineParseResult): T;
    /// <summary>Every value the invocation carried, for an arity that admits many.</summary>
    function ValuesIn(const Parsed: ICommandLineParseResult): TArray<T>;
    function WithDefault(const Value: T): IOption<T>;
    function WithAlias(const Alias: string): IOption<T>;
    function WithArity(const Value: TArity): IOption<T>;
    function Describe(const Text: string): IOption<T>;
    /// <summary>Makes it available to every descendant of the declaring command.</summary>
    function Recursive: IOption<T>;
    /// <summary>Makes the invocation have to carry it, unless it has a default.</summary>
    function Required: IOption<T>;
    /// <summary>Restricts it to a declared set of values, listed when one is refused.</summary>
    function AcceptOnly(const Values: array of string): IOption<T>;
  end;

  /// <summary>An argument identified by its place, not by a name behind dashes.</summary>
  IArgument<T> = interface(IArgument)
    function ValueIn(const Parsed: ICommandLineParseResult): T;
    function ValuesIn(const Parsed: ICommandLineParseResult): TArray<T>;
    function WithDefault(const Value: T): IArgument<T>;
    function WithArity(const Value: TArity): IArgument<T>;
    function Describe(const Text: string): IArgument<T>;
    function AcceptOnly(const Values: array of string): IArgument<T>;
  end;

  // The classes below are declared here rather than in the implementation section on
  // purpose: a parameterized method may not name a symbol local to implementation
  // (E2506), and the CommandLine facade below reaches them from generic methods. It
  // has already cost this repo twice -- TGrouping in Daf.Linq, ResolvedMemberName in
  // Specification -- so it is planned for instead of discovered at compile time.

  TSymbol = class(TInterfacedObject, ISymbol)
  private
    FName: string;
    FAliases: TArray<string>;
    FDescription: string;
  public
    constructor Create(const AName: string);
    function AsObject: TObject;
    function Name: string;
    function Aliases: TArray<string>;
    function Description: string;
    function Matches(const Token: string): Boolean;
    procedure AddAlias(const Alias: string);
    procedure SetDescription(const Text: string);
  end;

  TValuedSymbol = class(TSymbol, IValued)
  protected
    FTypeInfo: PTypeInfo;
    FArity: TArity;
    FAccepted: TArray<string>;
    FHasDefault: Boolean;
    FDefaultText: string;
  public
    constructor Create(const AName: string; ATypeInfo: PTypeInfo);
    function TryAccept(const Raw: string; out Error: string): Boolean;
    function IsFlag: Boolean;
    function TypeName: string;
    function Arity: TArity;
    function AcceptedValues: TArray<string>;
    function HasDefault: Boolean;
    function DefaultText: string;
    procedure SetArity(const Value: TArity);
    procedure SetAccepted(const Values: array of string);
    procedure SetDefaultText(const Text: string);
  end;

  TOption<T> = class(TValuedSymbol, IOption, IOption<T>)
  private
    FRecursive: Boolean;
    FRequiredFlag: Boolean;
    FDefaultValue: T;
  public
    constructor Create(const AName: string);
    function IsRecursive: Boolean;
    function IsRequired: Boolean;
    function ValueIn(const Parsed: ICommandLineParseResult): T;
    function ValuesIn(const Parsed: ICommandLineParseResult): TArray<T>;
    function WithDefault(const Value: T): IOption<T>;
    function WithAlias(const Alias: string): IOption<T>;
    function WithArity(const Value: TArity): IOption<T>;
    function Describe(const Text: string): IOption<T>;
    function Recursive: IOption<T>;
    function Required: IOption<T>;
    function AcceptOnly(const Values: array of string): IOption<T>;
  end;

  TArgument<T> = class(TValuedSymbol, IArgument, IArgument<T>)
  private
    FDefaultValue: T;
  public
    constructor Create(const AName: string);
    function ValueIn(const Parsed: ICommandLineParseResult): T;
    function ValuesIn(const Parsed: ICommandLineParseResult): TArray<T>;
    function WithDefault(const Value: T): IArgument<T>;
    function WithArity(const Value: TArity): IArgument<T>;
    function Describe(const Text: string): IArgument<T>;
    function AcceptOnly(const Values: array of string): IArgument<T>;
  end;

  /// <summary>Where a grammar starts. The facade, in the shape Daf.Linq uses.</summary>
  /// <remarks>
  ///   The description is taken at declaration, as in .NET's
  ///   <c>new Command("build", "Build things")</c>. The fluent <c>Describe</c> says the
  ///   same thing later; it is the one to avoid on a root, because it answers
  ///   <c>ICommand</c> and a root would stop being an <c>IRootCommand</c>.
  /// </remarks>
  CommandLine = record
  public
    class function Option<T>(const Name: string; const Description: string = ''): IOption<T>; static;
    class function Argument<T>(const Name: string; const Description: string = ''): IArgument<T>; static;
    class function Command(const Name: string; const Description: string = ''): ICommand; static;
    class function Root(const Name: string; const Description: string = ''): IRootCommand; static;
  end;

const
  /// <summary>The help tokens every grammar answers to unless it declares its own.</summary>
  HelpTokens: array [0 .. 1] of string = ('--help', '-h');
  /// <summary>The version token the ROOT answers to unless it declares its own.</summary>
  VersionToken = '--version';
  /// <summary>Everything after this token is a value, dashes and all. POSIX.</summary>
  EndOfOptions = '--';

implementation

uses
  Daf.CommandLine.Values,
  Daf.CommandLine.Help;

type
  TBinding = record
    Key: ISymbol;
    Values: TArray<string>;
  end;

  TParseResult = class(TInterfacedObject, ICommandLineParseResult)
  private
    FCommand: ICommand;
    FErrors: TArray<string>;
    FBindings: TArray<TBinding>;
    FHelpRequested: Boolean;
    FHelpText: string;
    FVersionRequested: Boolean;
    FVersionText: string;
    function IndexOf(const Symbol: ISymbol): Integer;
  public
    function Command: ICommand;
    function Succeeded: Boolean;
    function Errors: TArray<string>;
    function ErrorText: string;
    function HelpRequested: Boolean;
    function HelpText: string;
    function VersionRequested: Boolean;
    function VersionText: string;
    function WasProvided(const Symbol: ISymbol): Boolean;
    function RawOf(const Symbol: ISymbol): TNullable<string>;
    function RawValuesOf(const Symbol: ISymbol): TArray<string>;
    procedure Bind(const Symbol: ISymbol; const Raw: string);
    procedure AddError(const Text: string);
    procedure SetCommand(const Value: ICommand);
    procedure RequestHelp(const ForCommand: ICommand);
    procedure RequestVersion(const ForCommand: ICommand; const Version: string);
  end;

  TCommand = class(TSymbol, ICommand)
  private
    FOptions: TArray<IOption>;
    FArguments: TArray<IArgument>;
    FSubcommands: TArray<ICommand>;
    // The path is kept as text, set by the parent when the child is added. A child
    // does NOT hold a reference to its parent: that would be a cycle between counted
    // interfaces, and a cycle is a leak the compiler never mentions.
    FPathPrefix: string;
    procedure AddSymbol(const Symbol: ISymbol);
  public
    function Add(const Option: IOption): ICommand; overload;
    function Add(const Argument: IArgument): ICommand; overload;
    function Add(const Subcommand: ICommand): ICommand; overload;
    function Add(const Symbols: array of ISymbol): ICommand; overload;
    function WithAlias(const Alias: string): ICommand;
    function Describe(const Text: string): ICommand;
    function Options: TArray<IOption>;
    function Arguments: TArray<IArgument>;
    function Subcommands: TArray<ICommand>;
    function FullPath: string;
    function HelpText: string;
    procedure SetPathPrefix(const Value: string);
  end;

  TRootCommand = class(TCommand, IRootCommand)
  private
    FVersion: string;
  public
    function Parse(const Args: TArray<string>): ICommandLineParseResult; overload;
    function Parse: ICommandLineParseResult; overload;
    function WithVersion(const Version: string): IRootCommand;
    function VersionText: string;
  end;

{ TArity }

class function TArity.Create(AMin, AMax: Integer): TArity;
begin
  if AMin < 0 then
    raise ECommandLine.Create('An arity minimum cannot be negative');
  if AMax < AMin then
    raise ECommandLine.Create('An arity maximum cannot be below its minimum');
  Result.FMin := AMin;
  Result.FMax := AMax;
end;

class function TArity.Zero: TArity;
begin
  Result := TArity.Create(0, 0);
end;

class function TArity.ZeroOrOne: TArity;
begin
  Result := TArity.Create(0, 1);
end;

class function TArity.ExactlyOne: TArity;
begin
  Result := TArity.Create(1, 1);
end;

class function TArity.ZeroOrMore: TArity;
begin
  Result := TArity.Create(0, Unbounded);
end;

class function TArity.OneOrMore: TArity;
begin
  Result := TArity.Create(1, Unbounded);
end;

function TArity.Min: Integer;
begin
  Result := FMin;
end;

function TArity.Max: Integer;
begin
  Result := FMax;
end;

function TArity.AllowsMany: Boolean;
begin
  Result := FMax > 1;
end;

function TArity.DemandsValue: Boolean;
begin
  Result := FMin >= 1;
end;

function TArity.Describe: string;
begin
  if (FMin = 0) and (FMax = 0) then
    Exit('no value');
  if (FMin = 0) and (FMax = 1) then
    Exit('at most one value');
  if (FMin = 1) and (FMax = 1) then
    Exit('exactly one value');
  if FMin = 0 then
    Exit('any number of values');
  Result := 'at least one value';
end;

{ TSymbol }

constructor TSymbol.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

function TSymbol.AsObject: TObject;
begin
  Result := Self;
end;

function TSymbol.Name: string;
begin
  Result := FName;
end;

function TSymbol.Aliases: TArray<string>;
begin
  Result := FAliases;
end;

function TSymbol.Description: string;
begin
  Result := FDescription;
end;

function TSymbol.Matches(const Token: string): Boolean;
var
  Alias: string;
begin
  if SameText(Token, FName) then
    Exit(True);
  for Alias in FAliases do
    if SameText(Token, Alias) then
      Exit(True);
  Result := False;
end;

procedure TSymbol.AddAlias(const Alias: string);
begin
  FAliases := FAliases + [Alias];
end;

procedure TSymbol.SetDescription(const Text: string);
begin
  FDescription := Text;
end;

{ TValuedSymbol }

constructor TValuedSymbol.Create(const AName: string; ATypeInfo: PTypeInfo);
begin
  inherited Create(AName);
  FTypeInfo := ATypeInfo;
  if not TCommandLineValue.Handles(FTypeInfo) then
    raise ECommandLine.CreateFmt('Symbol "%s" is declared with a type the command line cannot convert: %s',
      [AName, TCommandLineValue.TypeName(FTypeInfo)]);
  // Derived from the type, so the ordinary case is never written by hand (design D6).
  if IsFlag then
    FArity := TArity.ZeroOrOne
  else
    FArity := TArity.ExactlyOne;
end;

function TValuedSymbol.TryAccept(const Raw: string; out Error: string): Boolean;
var
  Parsed: TValue;
  Allowed: TArray<string>;
  Candidate: string;
begin
  Error := '';
  Allowed := AcceptedValues;
  if Length(Allowed) > 0 then
  begin
    Result := False;
    for Candidate in Allowed do
      if SameText(Raw, Candidate) then
      begin
        Result := True;
        Break;
      end;
    if not Result then
    begin
      // Refusing without saying what was expected forces the caller to read the
      // tool's source in order to use the tool.
      Error := Format('"%s" is not a valid value for %s. Accepted: %s', [Raw, Name, string.Join(', ', Allowed)]);
      Exit;
    end;
  end;

  Result := TCommandLineValue.TryParse(FTypeInfo, Raw, Parsed);
  if not Result then
    // Names both the symbol and the text received: "80xyz" is not a valid integer
    // for --port. Refusing without saying what was refused is barely better than
    // inventing a value out of it.
    Error := Format('"%s" is not a valid %s for %s', [Raw, TypeName, Name]);
end;

function TValuedSymbol.IsFlag: Boolean;
begin
  Result := TCommandLineValue.IsFlag(FTypeInfo);
end;

function TValuedSymbol.TypeName: string;
begin
  Result := TCommandLineValue.TypeName(FTypeInfo);
end;

function TValuedSymbol.Arity: TArity;
begin
  Result := FArity;
end;

function TValuedSymbol.AcceptedValues: TArray<string>;
begin
  if Length(FAccepted) > 0 then
    Exit(FAccepted);
  // An enumerated type already names its own values; nobody should write them again.
  Result := TCommandLineValue.AcceptedValuesOf(FTypeInfo);
end;

function TValuedSymbol.HasDefault: Boolean;
begin
  Result := FHasDefault;
end;

function TValuedSymbol.DefaultText: string;
begin
  Result := FDefaultText;
end;

procedure TValuedSymbol.SetArity(const Value: TArity);
begin
  FArity := Value;
end;

procedure TValuedSymbol.SetAccepted(const Values: array of string);
var
  I: Integer;
begin
  SetLength(FAccepted, Length(Values));
  for I := Low(Values) to High(Values) do
    FAccepted[I - Low(Values)] := Values[I];
end;

procedure TValuedSymbol.SetDefaultText(const Text: string);
begin
  FHasDefault := True;
  FDefaultText := Text;
end;

{ TOption<T> }

constructor TOption<T>.Create(const AName: string);
begin
  inherited Create(AName, TypeInfo(T));
  FDefaultValue := Default (T);
end;

function TOption<T>.IsRecursive: Boolean;
begin
  Result := FRecursive;
end;

function TOption<T>.IsRequired: Boolean;
begin
  Result := FRequiredFlag;
end;

function TOption<T>.ValueIn(const Parsed: ICommandLineParseResult): T;
var
  Raw: TNullable<string>;
  Converted: TValue;
begin
  if Parsed = nil then
    Exit(FDefaultValue);
  Raw := Parsed.RawOf(Self);
  if Raw.IsNull then
    Exit(FDefaultValue);
  if not TCommandLineValue.TryParse(FTypeInfo, Raw.Value, Converted) then
    raise ECommandLine.CreateFmt('"%s" is not a valid %s for %s', [Raw.Value, TypeName, Name]);
  Result := Converted.AsType<T>;
end;

function TOption<T>.ValuesIn(const Parsed: ICommandLineParseResult): TArray<T>;
var
  Raw: string;
  Converted: TValue;
begin
  Result := nil;
  if Parsed = nil then
    Exit;
  for Raw in Parsed.RawValuesOf(Self) do
    if TCommandLineValue.TryParse(FTypeInfo, Raw, Converted) then
      Result := Result + [Converted.AsType<T>];
end;

function TOption<T>.WithDefault(const Value: T): IOption<T>;
begin
  FDefaultValue := Value;
  SetDefaultText(TValue.From<T>(Value).ToString);
  Result := Self;
end;

function TOption<T>.WithAlias(const Alias: string): IOption<T>;
begin
  AddAlias(Alias);
  Result := Self;
end;

function TOption<T>.WithArity(const Value: TArity): IOption<T>;
begin
  SetArity(Value);
  Result := Self;
end;

function TOption<T>.Describe(const Text: string): IOption<T>;
begin
  SetDescription(Text);
  Result := Self;
end;

function TOption<T>.Recursive: IOption<T>;
begin
  FRecursive := True;
  Result := Self;
end;

function TOption<T>.Required: IOption<T>;
begin
  FRequiredFlag := True;
  Result := Self;
end;

function TOption<T>.AcceptOnly(const Values: array of string): IOption<T>;
begin
  SetAccepted(Values);
  Result := Self;
end;

{ TArgument<T> }

constructor TArgument<T>.Create(const AName: string);
begin
  inherited Create(AName, TypeInfo(T));
  FDefaultValue := Default (T);
end;

function TArgument<T>.ValueIn(const Parsed: ICommandLineParseResult): T;
var
  Raw: TNullable<string>;
  Converted: TValue;
begin
  if Parsed = nil then
    Exit(FDefaultValue);
  Raw := Parsed.RawOf(Self);
  if Raw.IsNull then
    Exit(FDefaultValue);
  if not TCommandLineValue.TryParse(FTypeInfo, Raw.Value, Converted) then
    raise ECommandLine.CreateFmt('"%s" is not a valid %s for %s', [Raw.Value, TypeName, Name]);
  Result := Converted.AsType<T>;
end;

function TArgument<T>.ValuesIn(const Parsed: ICommandLineParseResult): TArray<T>;
var
  Raw: string;
  Converted: TValue;
begin
  Result := nil;
  if Parsed = nil then
    Exit;
  for Raw in Parsed.RawValuesOf(Self) do
    if TCommandLineValue.TryParse(FTypeInfo, Raw, Converted) then
      Result := Result + [Converted.AsType<T>];
end;

function TArgument<T>.WithDefault(const Value: T): IArgument<T>;
begin
  FDefaultValue := Value;
  SetDefaultText(TValue.From<T>(Value).ToString);
  Result := Self;
end;

function TArgument<T>.WithArity(const Value: TArity): IArgument<T>;
begin
  SetArity(Value);
  Result := Self;
end;

function TArgument<T>.Describe(const Text: string): IArgument<T>;
begin
  SetDescription(Text);
  Result := Self;
end;

function TArgument<T>.AcceptOnly(const Values: array of string): IArgument<T>;
begin
  SetAccepted(Values);
  Result := Self;
end;

{ TParseResult }

function TParseResult.IndexOf(const Symbol: ISymbol): Integer;
var
  I: Integer;
begin
  Result := -1;
  if Symbol = nil then
    Exit;
  for I := 0 to High(FBindings) do
    if FBindings[I].Key.AsObject = Symbol.AsObject then
      Exit(I);
end;

function TParseResult.Command: ICommand;
begin
  Result := FCommand;
end;

function TParseResult.Succeeded: Boolean;
begin
  Result := Length(FErrors) = 0;
end;

function TParseResult.Errors: TArray<string>;
begin
  Result := FErrors;
end;

function TParseResult.ErrorText: string;
begin
  Result := string.Join(sLineBreak, FErrors);
end;

function TParseResult.HelpRequested: Boolean;
begin
  Result := FHelpRequested;
end;

function TParseResult.HelpText: string;
begin
  Result := FHelpText;
end;

function TParseResult.VersionRequested: Boolean;
begin
  Result := FVersionRequested;
end;

function TParseResult.VersionText: string;
begin
  Result := FVersionText;
end;

function TParseResult.WasProvided(const Symbol: ISymbol): Boolean;
begin
  Result := IndexOf(Symbol) >= 0;
end;

function TParseResult.RawOf(const Symbol: ISymbol): TNullable<string>;
var
  At: Integer;
begin
  At := IndexOf(Symbol);
  if (At < 0) or (Length(FBindings[At].Values) = 0) then
    Exit(TNullable<string>.Null);
  Result := TNullable<string>.From(FBindings[At].Values[0]);
end;

function TParseResult.RawValuesOf(const Symbol: ISymbol): TArray<string>;
var
  At: Integer;
begin
  At := IndexOf(Symbol);
  if At < 0 then
    Exit(nil);
  Result := FBindings[At].Values;
end;

procedure TParseResult.Bind(const Symbol: ISymbol; const Raw: string);
var
  At: Integer;
  Entry: TBinding;
begin
  At := IndexOf(Symbol);
  if At >= 0 then
  begin
    FBindings[At].Values := FBindings[At].Values + [Raw];
    Exit;
  end;
  Entry.Key := Symbol;
  Entry.Values := [Raw];
  FBindings := FBindings + [Entry];
end;

procedure TParseResult.AddError(const Text: string);
begin
  FErrors := FErrors + [Text];
end;

procedure TParseResult.SetCommand(const Value: ICommand);
begin
  FCommand := Value;
end;

procedure TParseResult.RequestHelp(const ForCommand: ICommand);
begin
  FHelpRequested := True;
  FCommand := ForCommand;
  FHelpText := ForCommand.HelpText;
end;

procedure TParseResult.RequestVersion(const ForCommand: ICommand; const Version: string);
begin
  FVersionRequested := True;
  FCommand := ForCommand;
  FVersionText := Version;
end;

{ TCommand }

function TCommand.Add(const Option: IOption): ICommand;
begin
  FOptions := FOptions + [Option];
  Result := Self;
end;

function TCommand.Add(const Argument: IArgument): ICommand;
begin
  FArguments := FArguments + [Argument];
  Result := Self;
end;

function TCommand.Add(const Subcommand: ICommand): ICommand;
begin
  FSubcommands := FSubcommands + [Subcommand];
  (Subcommand.AsObject as TCommand).SetPathPrefix(FullPath);
  Result := Self;
end;

function TCommand.Add(const Symbols: array of ISymbol): ICommand;
var
  I: Integer;
begin
  for I := Low(Symbols) to High(Symbols) do
    AddSymbol(Symbols[I]);
  Result := Self;
end;

procedure TCommand.AddSymbol(const Symbol: ISymbol);
var
  AsOption: IOption;
  AsArgument: IArgument;
  AsCommand: ICommand;
begin
  if Supports(Symbol, IOption, AsOption) then
    Add(AsOption)
  else if Supports(Symbol, IArgument, AsArgument) then
    Add(AsArgument)
  else if Supports(Symbol, ICommand, AsCommand) then
    Add(AsCommand)
  else
    raise ECommandLine.Create('Only options, arguments and subcommands can be added to a command');
end;

function TCommand.WithAlias(const Alias: string): ICommand;
begin
  AddAlias(Alias);
  Result := Self;
end;

function TCommand.Describe(const Text: string): ICommand;
begin
  SetDescription(Text);
  Result := Self;
end;

function TCommand.Options: TArray<IOption>;
begin
  Result := FOptions;
end;

function TCommand.Arguments: TArray<IArgument>;
begin
  Result := FArguments;
end;

function TCommand.Subcommands: TArray<ICommand>;
begin
  Result := FSubcommands;
end;

function TCommand.FullPath: string;
begin
  if FPathPrefix = '' then
    Result := Name
  else
    Result := FPathPrefix + ' ' + Name;
end;

function TCommand.HelpText: string;
begin
  Result := TCommandLineHelp.Render(Self);
end;

procedure TCommand.SetPathPrefix(const Value: string);
var
  Child: ICommand;
begin
  FPathPrefix := Value;
  // Whatever hung below keeps its path correct when a whole branch is grafted on.
  for Child in FSubcommands do
    (Child.AsObject as TCommand).SetPathPrefix(FullPath);
end;

{ TRootCommand }

function TRootCommand.WithVersion(const Version: string): IRootCommand;
begin
  FVersion := Version;
  Result := Self;
end;

function TRootCommand.VersionText: string;
begin
  Result := FVersion;
end;

function TRootCommand.Parse: ICommandLineParseResult;
var
  Args: TArray<string>;
  I: Integer;
begin
  SetLength(Args, ParamCount);
  for I := 1 to ParamCount do
    Args[I - 1] := ParamStr(I);
  Result := Parse(Args);
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

// The command's own options plus the inherited ones, with no repeats: a recursive
// option declared on the command itself would otherwise be checked twice and reported
// twice.
function OptionsInScope(const Current: ICommand; const Inherited_: TArray<IOption>): TArray<IOption>;
var
  Candidate, Seen: IOption;
  Known: Boolean;
begin
  Result := Current.Options;
  for Candidate in Inherited_ do
  begin
    Known := False;
    for Seen in Result do
      if Seen.AsObject = Candidate.AsObject then
      begin
        Known := True;
        Break;
      end;
    if not Known then
      Result := Result + [Candidate];
  end;
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

// A token starting with a dash is an option -- unless it is a negative number, and
// then it is a value. Without this, an Integer argument could never be given -5,
// which is one of the two defects this module exists not to inherit.
function LooksLikeOption(const Token: string): Boolean;
begin
  Result := (Length(Token) >= 2) and (Token[1] = '-') and not CharInSet(Token[2], ['0' .. '9', '.']);
end;

procedure SplitToken(const Token: string; out OptionName, InlineValue: string; out HasInline: Boolean);
var
  P: Integer;
begin
  P := Token.IndexOf('=');
  HasInline := P > 0;
  if HasInline then
  begin
    OptionName := Token.Substring(0, P);
    InlineValue := Token.Substring(P + 1);
  end
  else
  begin
    OptionName := Token;
    InlineValue := '';
  end;
end;

function TRootCommand.Parse(const Args: TArray<string>): ICommandLineParseResult;
var
  Outcome: TParseResult;
  Current: ICommand;
  Available: TArray<IOption>;
  Index: Integer;
  Positional: Integer;
  Token, OptionName, InlineValue, Raw, Error: string;
  HasInline, OptionsEnded: Boolean;
  Option: IOption;
  Argument: IArgument;
  Subcommand: ICommand;
begin
  Outcome := TParseResult.Create;
  Result := Outcome;
  Current := Self;
  Available := RecursiveOptionsOf(Current);
  Index := 0;
  Positional := 0;
  OptionsEnded := False;

  while Index < Length(Args) do
  begin
    Token := Args[Index];

    // POSIX: `--` ends the options. Everything after it is a value, dashes and all,
    // which is the only way to pass a positional whose value looks like an option.
    if (Token = EndOfOptions) and not OptionsEnded then
    begin
      OptionsEnded := True;
      Inc(Index);
      Continue;
    end;

    SplitToken(Token, OptionName, InlineValue, HasInline);

    if not OptionsEnded then
    begin
      // Help is resolved BEFORE the grammar is validated (design D4): asking how the
      // syntax works is not an invalid invocation, and a tool that demands you get the
      // syntax right in order to explain the syntax is no use. An application that
      // declared its own --help wins, so this only fires when nothing answers the token.
      if IsHelpToken(OptionName) and (FindOption(Current, Available, OptionName) = nil) then
      begin
        Outcome.RequestHelp(Current);
        Exit;
      end;

      // --version belongs to the ROOT, as in .NET: a subcommand does not answer it,
      // because a version is the tool's and not each order's.
      if IsVersionToken(OptionName) and (Current.AsObject = Self) and
        (FindOption(Current, Available, OptionName) = nil) then
      begin
        Outcome.RequestVersion(Current, FVersion);
        Exit;
      end;
    end;

    if not OptionsEnded and LooksLikeOption(Token) then
    begin
      Option := FindOption(Current, Available, OptionName);
      if Option = nil then
      begin
        Outcome.AddError(Format('Unknown option "%s" for %s', [OptionName, Current.FullPath]));
        Inc(Index);
        Continue;
      end;

      // Repeating an option that admits one value is an ambiguous invocation, and an
      // ambiguous invocation has to fail. Quietly keeping the last one is the same sin
      // as reading 80xyz as 80: a value the parser chose on its own.
      if not Option.Arity.AllowsMany and Outcome.WasProvided(Option) then
      begin
        Outcome.AddError(Format('Option "%s" was given more than once, and it takes %s',
          [Option.Name, Option.Arity.Describe]));
        if not HasInline and not Option.IsFlag and (Index + 1 < Length(Args)) then
          Inc(Index);
        Inc(Index);
        Continue;
      end;

      if HasInline then
        Raw := InlineValue
      else if Option.IsFlag then
      begin
        // A flag may carry its value explicitly -- `--minify false` -- because its
        // arity admits none or one. Only a boolean literal counts as that value;
        // anything else is somebody else's token.
        if (Index + 1 < Length(Args)) and TCommandLineValue.IsBooleanLiteral(Args[Index + 1]) then
        begin
          Inc(Index);
          Raw := Args[Index];
        end
        else
          Raw := 'true';
      end
      else
      begin
        if Index + 1 >= Length(Args) then
        begin
          Outcome.AddError(Format('Option "%s" requires a %s value', [Option.Name, Option.TypeName]));
          Inc(Index);
          Continue;
        end;
        Inc(Index);
        // Taken unconditionally, dash or no dash: this is what lets --port -5 through.
        Raw := Args[Index];
      end;

      if Option.TryAccept(Raw, Error) then
        Outcome.Bind(Option, Raw)
      else
        Outcome.AddError(Error);
      Inc(Index);
      Continue;
    end;

    if not OptionsEnded then
    begin
      Subcommand := FindSubcommand(Current, Token);
      if Subcommand <> nil then
      begin
        Current := Subcommand;
        Available := Available + RecursiveOptionsOf(Current);
        Positional := 0;
        Inc(Index);
        Continue;
      end;
    end;

    if Positional <= High(Current.Arguments) then
    begin
      Argument := Current.Arguments[Positional];
      if Argument.TryAccept(Token, Error) then
        Outcome.Bind(Argument, Token)
      else
        Outcome.AddError(Error);
      // One that admits many keeps consuming; one that does not hands over to the next.
      if not Argument.Arity.AllowsMany then
        Inc(Positional);
    end
    else
      Outcome.AddError(Format('Unexpected argument "%s" for %s', [Token, Current.FullPath]));
    Inc(Index);
  end;

  // A command that only groups is not an invocation of its own.
  if Length(Current.Subcommands) > 0 then
    Outcome.AddError(Format('Command "%s" requires a subcommand: %s', [Current.FullPath, NamesOf(Current.Subcommands)]));

  // What the invocation had to carry and did not. A declared default satisfies the
  // demand, because the value exists -- which is what "required" asks for.
  for Option in OptionsInScope(Current, Available) do
    if Option.IsRequired and not Option.HasDefault and not Outcome.WasProvided(Option) then
      Outcome.AddError(Format('Option "%s" is required', [Option.Name]));

  for Argument in Current.Arguments do
    if Argument.Arity.DemandsValue and not Argument.HasDefault and not Outcome.WasProvided(Argument) then
      Outcome.AddError(Format('Argument "%s" is required', [Argument.Name]));

  Outcome.SetCommand(Current);
end;

{ CommandLine }

class function CommandLine.Option<T>(const Name: string; const Description: string): IOption<T>;
var
  Created: TOption<T>;
begin
  Created := TOption<T>.Create(Name);
  Created.SetDescription(Description);
  Result := Created;
end;

class function CommandLine.Argument<T>(const Name: string; const Description: string): IArgument<T>;
var
  Created: TArgument<T>;
begin
  Created := TArgument<T>.Create(Name);
  Created.SetDescription(Description);
  Result := Created;
end;

class function CommandLine.Command(const Name: string; const Description: string): ICommand;
var
  Created: TCommand;
begin
  Created := TCommand.Create(Name);
  Created.SetDescription(Description);
  Result := Created;
end;

class function CommandLine.Root(const Name: string; const Description: string): IRootCommand;
var
  Created: TRootCommand;
begin
  Created := TRootCommand.Create(Name);
  Created.SetDescription(Description);
  Result := Created;
end;

end.
