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
  Daf.CommandLine.Tokens,
  Daf.Nullable;

type
  ECommandLine = class(Exception);

  /// <summary>
  ///   A conversion declared by whoever uses the module: turns the raw text of one
  ///   token into <c>T</c>, or refuses it with a message of its own.
  /// </summary>
  /// <remarks>
  ///   It is what makes the type engine extensible without touching DAF: a type it does
  ///   not know stops being a wall and becomes a line of the application.
  ///
  ///   It is declared ON THE SYMBOL and not in a registry of types, deliberately: a
  ///   registry would make it process-wide configuration, so two libraries declaring a
  ///   conversion for the same type would fight over it -- and the one that started last
  ///   would win, in silence (design D2). On the symbol, its reach is exactly where it
  ///   was written.
  /// </remarks>
  TCommandLineParser<T> = reference to function(const Raw: string; out Value: T; out Error: string): Boolean;

  /// <summary>
  ///   A validation declared by whoever uses the module: receives the already converted
  ///   value and may refuse it with a message of its own.
  /// </summary>
  /// <remarks>
  ///   It runs INSIDE the parse, so its refusal comes out through <c>Errors</c> like any
  ///   other invocation error (design D3). Validating afterwards moves the failure to
  ///   where it can no longer be reported next to the rest, and the tool ends up with two
  ///   ways of saying that an invocation was wrong -- which is the same displacement the
  ///   parked module made with the help text.
  ///
  ///   A validation should be a pure question about the value.
  ///   <c>AcceptExistingOnly</c> is the conscious exception: it asks the file system.
  /// </remarks>
  TCommandLineValidator<T> = reference to function(const Value: T; out Error: string): Boolean;

  /// <summary>
  ///   The type-erased shapes the engine stores. An application writes the two typed
  ///   forms above; a symbol keeps these, so one conversion and one list of validations
  ///   serve every declared type.
  /// </summary>
  /// <remarks>
  ///   They are declared here, and not in <c>implementation</c>, because the generic
  ///   classes below name them and a parameterized method may not name a type local to
  ///   the implementation section (E2506).
  /// </remarks>
  TUntypedParser = reference to function(const Raw: string; out Value: TValue; out Error: string): Boolean;
  TUntypedValidator = reference to function(const Value: TValue; out Error: string): Boolean;

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
    /// <summary>True when the help does not list it. It changes NOTHING else.</summary>
    /// <remarks>
    ///   It is what a diagnostic option, one in transition, or one only an internal script
    ///   uses needs: to exist without being put in front of everybody who asks how the
    ///   tool works. Today the alternatives are not declaring it -- and then it does not
    ///   work -- or declaring it and accepting that it shows: two bad ones.
    ///
    ///   Hiding is the HELP's business and nothing else (design D5). A hidden symbol that
    ///   gets an invalid value fails the same way and is named in the error: hiding it
    ///   from the help is not hiding it from whoever is already using it, and an error
    ///   that will not say what failed turns a mistake into a riddle.
    /// </remarks>
    function IsHidden: Boolean;
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
    ///   True when one mention of the name may be followed by several values:
    ///   <c>--items a b c</c>. Off unless declared, and it cannot be otherwise -- see
    ///   <c>AllowMultipleArgumentsPerToken</c>.
    /// </summary>
    function AllowsMultipleArgumentsPerToken: Boolean;
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
    /// <summary>True when a token matching nothing is a parse error, which is the default.</summary>
    function TreatsUnmatchedTokensAsErrors: Boolean;
    /// <summary>Makes this command collect what it does not recognise instead of failing.</summary>
    /// <remarks>
    ///   It is what a WRAPPER needs -- <c>sudo -u admin apt update</c> reads its own and
    ///   hands the rest to another program. The mark belongs to the command because it
    ///   says what that command IS (design D5); the default stays as it was, because for
    ///   anything that wraps nobody an unrecognised token is a typo, and swallowing it in
    ///   silence is the worse of the two failures.
    ///
    ///   It answers <c>ICommand</c>, so on a root it is called as a statement rather than
    ///   chained -- the same caveat as <c>Describe</c>.
    /// </remarks>
    function AllowUnmatchedTokens: ICommand;
    /// <summary>Keeps it out of the help of its parent. See <c>ISymbol.IsHidden</c>.</summary>
    function Hidden: ICommand;
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
    /// <summary>
    ///   The tokens the grammar did not claim. Always empty unless the command reached
    ///   declared it collects them, because otherwise they are errors instead.
    /// </summary>
    function UnmatchedTokens: TArray<string>;
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
    function WithDefault(const Value: T): IOption<T>; overload;
    /// <summary>Declares the default as a CALCULATION, run only if it is needed.</summary>
    /// <remarks>
    ///   A default that depends on the surroundings -- a temporary directory, today's
    ///   date, an environment variable -- cannot be written as a constant in the
    ///   declaration of the grammar, which runs before there is anything to know. Without
    ///   this the application leaves the default empty and fills it in after reading, and
    ///   then the help shows a default that is not the one the program uses.
    ///
    ///   The calculation runs ONLY when the invocation did not carry the symbol, and at
    ///   most once: it may open a file or read the environment, and paying for that twice
    ///   is a cost nobody asked for.
    ///
    ///   <c>Text</c> is what the help shows, because the help does NOT run the
    ///   calculation -- it has no way of knowing whether this one has effects, and
    ///   showing no default beats inventing one or paying for a side effect to paint a
    ///   line. With no text, no default is shown.
    /// </remarks>
    function WithDefault(const Factory: TFunc<T>; const Text: string = ''): IOption<T>; overload;
    function WithAlias(const Alias: string): IOption<T>;
    function WithArity(const Value: TArity): IOption<T>;
    function Describe(const Text: string): IOption<T>;
    /// <summary>Makes it available to every descendant of the declaring command.</summary>
    function Recursive: IOption<T>;
    /// <summary>Makes the invocation have to carry it, unless it has a default.</summary>
    function Required: IOption<T>;
    /// <summary>Restricts it to a declared set of values, listed when one is refused.</summary>
    function AcceptOnly(const Values: array of string): IOption<T>;
    /// <summary>Keeps it out of the help. See <c>ISymbol.IsHidden</c>.</summary>
    function Hidden: IOption<T>;
    /// <summary>Adds a validation of its own, which runs inside the parse.</summary>
    /// <remarks>
    ///   Several can be declared, and they run in the order they were declared. The
    ///   first refusal is the error reported for that value.
    /// </remarks>
    function Validate(const Validation: TCommandLineValidator<T>): IOption<T>;
    /// <summary>Accepts only a path that exists, file or directory.</summary>
    /// <remarks>
    ///   It is written over <c>Validate</c>'s own hook and not over a mechanism of its
    ///   own -- if the validations of serie needed one, the hook would not be doing its
    ///   job. It ASKS THE FILE SYSTEM, which is the conscious exception to a validation
    ///   being a pure question about the value.
    /// </remarks>
    function AcceptExistingOnly: IOption<T>;
    /// <summary>Accepts only a legal file name, separators included in what is refused.</summary>
    function AcceptLegalFileNamesOnly: IOption<T>;
    /// <summary>Lets one mention of the name be followed by several values.</summary>
    /// <remarks>
    ///   Opt-in, and it could not be anything else (design D4): were every option with
    ///   room for many values greedy by default, a positional written behind one would
    ///   stop being a positional. The right default depends on the grammar, and only
    ///   whoever wrote it knows. .NET calls it the same thing, for the same reason.
    /// </remarks>
    function AllowMultipleArgumentsPerToken: IOption<T>;
  end;

  /// <summary>An argument identified by its place, not by a name behind dashes.</summary>
  IArgument<T> = interface(IArgument)
    function ValueIn(const Parsed: ICommandLineParseResult): T;
    function ValuesIn(const Parsed: ICommandLineParseResult): TArray<T>;
    function WithDefault(const Value: T): IArgument<T>; overload;
    function WithDefault(const Factory: TFunc<T>; const Text: string = ''): IArgument<T>; overload;
    function WithArity(const Value: TArity): IArgument<T>;
    function Describe(const Text: string): IArgument<T>;
    function AcceptOnly(const Values: array of string): IArgument<T>;
    function Validate(const Validation: TCommandLineValidator<T>): IArgument<T>;
    function AcceptExistingOnly: IArgument<T>;
    function AcceptLegalFileNamesOnly: IArgument<T>;
    function Hidden: IArgument<T>;
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
    FHidden: Boolean;
  public
    constructor Create(const AName: string);
    function AsObject: TObject;
    function Name: string;
    function Aliases: TArray<string>;
    function Description: string;
    function Matches(const Token: string): Boolean;
    function IsHidden: Boolean;
    procedure AddAlias(const Alias: string);
    procedure SetDescription(const Text: string);
    procedure Hide;
  end;

  TValuedSymbol = class(TSymbol, IValued)
  protected
    FTypeInfo: PTypeInfo;
    /// <summary>
    ///   The type ONE token converts into. It is the declared type, except for a
    ///   collection, where it is the element: a collection is a symbol that takes many
    ///   values, and each of them is converted on its own.
    /// </summary>
    FElementTypeInfo: PTypeInfo;
    FArity: TArity;
    FAccepted: TArray<string>;
    FHasDefault: Boolean;
    FDefaultText: string;
    FParser: TUntypedParser;
    FValidators: TArray<TUntypedValidator>;
    /// <summary>Converts one token, through the declared conversion when there is one.</summary>
    function Convert(const Raw: string; out Value: TValue; out Error: string): Boolean;
    /// <summary>True when the declared type takes many values rather than one.</summary>
    function IsCollection: Boolean;
    /// <summary>Refuses at DECLARATION a path validation on something that is not one.</summary>
    procedure DemandPathType(const What: string);
  public
    constructor Create(const AName: string; ATypeInfo: PTypeInfo; const AParser: TUntypedParser);
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
    procedure AddValidator(const Validator: TUntypedValidator);
    // The two validations of serie, written over the hook above and nothing else.
    procedure DemandExistingPath;
    procedure DemandLegalFileName;
  end;

  /// <summary>What an option and an argument share once <c>T</c> is known.</summary>
  /// <remarks>
  ///   It exists so that every mechanism that needs <c>T</c> -- the declared conversion,
  ///   the validations, the default -- is written once instead of once per kind of
  ///   symbol. Two copies of it would be two places for the next mechanism to be
  ///   forgotten in.
  /// </remarks>
  TValuedSymbol<T> = class(TValuedSymbol)
  protected
    FDefaultValue: T;
    FDefaultFactory: TFunc<T>;
    /// <summary>True once <c>FDefaultValue</c> holds the default, computed or written.</summary>
    FDefaultKnown: Boolean;
    function DefaultOf: T;
    function ReadValue(const Parsed: ICommandLineParseResult): T;
    function ReadValues(const Parsed: ICommandLineParseResult): TArray<T>;
    procedure SetDefaultValue(const Value: T);
    procedure SetDefaultFactory(const Factory: TFunc<T>; const Text: string);
    procedure AddTypedValidator(const Validation: TCommandLineValidator<T>);
  public
    constructor Create(const AName: string; const AParser: TCommandLineParser<T>);
  end;

  TOption<T> = class(TValuedSymbol<T>, IOption, IOption<T>)
  private
    FRecursive: Boolean;
    FRequiredFlag: Boolean;
    FMultiplePerToken: Boolean;
  public
    function IsRecursive: Boolean;
    function IsRequired: Boolean;
    function AllowsMultipleArgumentsPerToken: Boolean;
    function AllowMultipleArgumentsPerToken: IOption<T>;
    function ValueIn(const Parsed: ICommandLineParseResult): T;
    function ValuesIn(const Parsed: ICommandLineParseResult): TArray<T>;
    function WithDefault(const Value: T): IOption<T>; overload;
    function WithDefault(const Factory: TFunc<T>; const Text: string = ''): IOption<T>; overload;
    function WithAlias(const Alias: string): IOption<T>;
    function WithArity(const Value: TArity): IOption<T>;
    function Describe(const Text: string): IOption<T>;
    function Recursive: IOption<T>;
    function Required: IOption<T>;
    function AcceptOnly(const Values: array of string): IOption<T>;
    function Validate(const Validation: TCommandLineValidator<T>): IOption<T>;
    function AcceptExistingOnly: IOption<T>;
    function AcceptLegalFileNamesOnly: IOption<T>;
    function Hidden: IOption<T>;
  end;

  TArgument<T> = class(TValuedSymbol<T>, IArgument, IArgument<T>)
  public
    function ValueIn(const Parsed: ICommandLineParseResult): T;
    function ValuesIn(const Parsed: ICommandLineParseResult): TArray<T>;
    function WithDefault(const Value: T): IArgument<T>; overload;
    function WithDefault(const Factory: TFunc<T>; const Text: string = ''): IArgument<T>; overload;
    function WithArity(const Value: TArity): IArgument<T>;
    function Describe(const Text: string): IArgument<T>;
    function AcceptOnly(const Values: array of string): IArgument<T>;
    function Validate(const Validation: TCommandLineValidator<T>): IArgument<T>;
    function AcceptExistingOnly: IArgument<T>;
    function AcceptLegalFileNamesOnly: IArgument<T>;
    function Hidden: IArgument<T>;
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
    class function Option<T>(const Name: string; const Description: string = ''): IOption<T>; overload; static;
    /// <summary>Declares an option together with the conversion that reads its type.</summary>
    /// <remarks>
    ///   The conversion is taken HERE, at declaration, and not set afterwards: a symbol
    ///   that always knows how to convert its type is a symbol whose declaration can be
    ///   refused on the spot when it does not (design D2). It also serves a type the
    ///   engine already knows, for reading it some other way.
    /// </remarks>
    class function Option<T>(const Name: string; const Parser: TCommandLineParser<T>;
      const Description: string = ''): IOption<T>; overload; static;
    class function Argument<T>(const Name: string; const Description: string = ''): IArgument<T>; overload; static;
    class function Argument<T>(const Name: string; const Parser: TCommandLineParser<T>;
      const Description: string = ''): IArgument<T>; overload; static;
    class function Command(const Name: string; const Description: string = ''): ICommand; static;
    class function Root(const Name: string; const Description: string = ''): IRootCommand; static;
  end;

const
  /// <summary>The help tokens every grammar answers to unless it declares its own.</summary>
  HelpTokens: array [0 .. 1] of string = ('--help', '-h');
  /// <summary>The version token the ROOT answers to unless it declares its own.</summary>
  VersionToken = '--version';
  /// <summary>Everything after this token is a value, dashes and all. POSIX.</summary>
  /// <remarks>
  ///   It is declared where the token shapes live and republished here, because this is
  ///   the unit its callers use and a second literal spelling of it is a second place to
  ///   change.
  /// </remarks>
  EndOfOptions = Daf.CommandLine.Tokens.EndOfOptions;

implementation

uses
  Daf.CommandLine.Values,
  Daf.CommandLine.Help;

const
  /// <summary>
  ///   What Windows does not admit in a file NAME. The separators are in the list on
  ///   purpose: this validates a name and not a path, which is the same split .NET makes
  ///   between the characters a name cannot carry and the ones a whole path cannot.
  /// </summary>
  IllegalInFileName: TSysCharSet = ['<', '>', ':', '"', '/', '\', '|', '?', '*'];

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
    FUnmatched: TArray<string>;
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
    function UnmatchedTokens: TArray<string>;
    procedure Bind(const Symbol: ISymbol; const Raw: string);
    procedure AddError(const Text: string);
    procedure AddUnmatched(const Token: string);
    procedure SetCommand(const Value: ICommand);
    procedure RequestHelp(const ForCommand: ICommand);
    procedure RequestVersion(const ForCommand: ICommand; const Version: string);
  end;

  TCommand = class(TSymbol, ICommand)
  private
    FOptions: TArray<IOption>;
    FArguments: TArray<IArgument>;
    FSubcommands: TArray<ICommand>;
    FAllowUnmatched: Boolean;
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
    function TreatsUnmatchedTokensAsErrors: Boolean;
    function AllowUnmatchedTokens: ICommand;
    function Hidden: ICommand;
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

  /// <summary>The grammar in scope, answering the three questions the rewriting asks.</summary>
  /// <remarks>
  ///   It is mutable and reused as the walk descends, because which options are
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

function TSymbol.IsHidden: Boolean;
begin
  Result := FHidden;
end;

procedure TSymbol.SetDescription(const Text: string);
begin
  FDescription := Text;
end;

procedure TSymbol.Hide;
begin
  FHidden := True;
end;

{ TValuedSymbol }

constructor TValuedSymbol.Create(const AName: string; ATypeInfo: PTypeInfo; const AParser: TUntypedParser);
begin
  inherited Create(AName);
  FTypeInfo := ATypeInfo;
  FElementTypeInfo := TCommandLineValue.ElementTypeOf(ATypeInfo);
  FParser := AParser;
  if Assigned(AParser) and IsCollection then
    // A conversion of its own converts ONE token, and for a collection that token is an
    // element -- which T cannot name, because Delphi has no way of saying "the element
    // of T" in an interface. Refused here rather than never run.
    raise ECommandLine.CreateFmt('Symbol "%s" is a collection, and a conversion of its own converts one ' +
      'value: declare it on the element type instead', [AName]);
  // The check stays at DECLARATION, which is where it is worth something, and that is
  // why the conversion is taken at declaration too: a symbol either knows how to convert
  // its type or it is refused on the spot. What changed is that "the engine does not know
  // this type" is no longer the end of the conversation (design D2).
  if not Assigned(FParser) and not TCommandLineValue.Handles(FTypeInfo) then
    raise ECommandLine.CreateFmt('Symbol "%s" is declared with a type the command line cannot convert: %s. ' +
      'Declare a conversion of its own for it', [AName, TCommandLineValue.TypeName(FTypeInfo)]);
  // Derived from the type, so the ordinary case is never written by hand (design D6).
  // A collection says "many" by being one, which is how .NET derives it too.
  if IsCollection then
    FArity := TArity.ZeroOrMore
  else if IsFlag then
    FArity := TArity.ZeroOrOne
  else
    FArity := TArity.ExactlyOne;
end;

function TValuedSymbol.IsCollection: Boolean;
begin
  Result := TCommandLineValue.IsCollection(FTypeInfo);
end;

function TValuedSymbol.TryAccept(const Raw: string; out Error: string): Boolean;
var
  Parsed: TValue;
  Allowed: TArray<string>;
  Candidate: string;
  Check: TUntypedValidator;
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

  if not Convert(Raw, Parsed, Error) then
    Exit(False);

  // Validation runs HERE, inside the parse, and its refusal goes out through the same
  // Errors as everything else. Validating afterwards would move the failure to where it
  // is no longer an invocation error, and whoever invoked the tool would get two lists
  // instead of one (design D3).
  for Check in FValidators do
    if not Check(Parsed, Error) then
    begin
      if Error = '' then
        Error := Format('"%s" is not a valid value for %s', [Raw, Name]);
      Exit(False);
    end;

  Result := True;
end;

function TValuedSymbol.Convert(const Raw: string; out Value: TValue; out Error: string): Boolean;
begin
  Error := '';
  Value := TValue.Empty;
  if Assigned(FParser) then
  begin
    Result := FParser(Raw, Value, Error);
    if not Result and (Error = '') then
      // A conversion of its own that refuses without saying why leaves the caller where
      // the parked module left them: a refusal with nothing to act on.
      Error := Format('"%s" is not a valid %s for %s', [Raw, TypeName, Name]);
    Exit;
  end;
  Result := TCommandLineValue.TryParse(FElementTypeInfo, Raw, Value);
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

procedure TValuedSymbol.AddValidator(const Validator: TUntypedValidator);
begin
  if not Assigned(Validator) then
    // A validation that was declared and is not there would never run and never say so,
    // which is the shape of defect this whole module exists to remove.
    raise ECommandLine.CreateFmt('A validation declared on "%s" is not assigned', [Name]);
  // Appended by index and not as [Validator]: inside an array literal Delphi reads a
  // reference-to-function as a CALL of it, and answers E2035 about parameters nobody
  // was passing.
  SetLength(FValidators, Length(FValidators) + 1);
  FValidators[High(FValidators)] := Validator;
end;

procedure TValuedSymbol.DemandPathType(const What: string);
begin
  // Here a path is a string: what FileInfo and DirectoryInfo contributed in .NET was the
  // validation, and the validation is what gets ported (design D1).
  // Through the element, so a collection of paths validates each one of them.
  if FElementTypeInfo <> System.TypeInfo(string) then
    raise ECommandLine.CreateFmt('%s applies to a path, and "%s" is declared as %s', [What, Name, TypeName]);
end;

procedure TValuedSymbol.DemandExistingPath;
var
  Who: string;
begin
  DemandPathType('AcceptExistingOnly');
  // The NAME is captured, not Self. A closure kept on the object that captures the
  // object is a cycle, and a cycle between counted references is a leak the compiler
  // never mentions.
  Who := Name;
  AddValidator(
    function(const Value: TValue; out Error: string): Boolean
    begin
      Error := '';
      Result := FileExists(Value.AsString) or DirectoryExists(Value.AsString);
      if not Result then
        Error := Format('"%s" does not exist, and %s only accepts a path that does', [Value.AsString, Who]);
    end);
end;

procedure TValuedSymbol.DemandLegalFileName;
var
  Who: string;
begin
  DemandPathType('AcceptLegalFileNamesOnly');
  Who := Name;
  AddValidator(
    function(const Value: TValue; out Error: string): Boolean
    var
      Raw: string;
      Ch: Char;
    begin
      Error := '';
      Raw := Value.AsString;
      // Empty is not a file name either, and the loop below would let it through.
      Result := Raw <> '';
      for Ch in Raw do
        if CharInSet(Ch, IllegalInFileName) or (Ch < ' ') then
        begin
          Result := False;
          Break;
        end;
      if not Result then
        Error := Format('"%s" is not a legal file name, and %s accepts only one that is', [Raw, Who]);
    end);
end;

{ TValuedSymbol<T> }

constructor TValuedSymbol<T>.Create(const AName: string; const AParser: TCommandLineParser<T>);
var
  Untyped: TUntypedParser;
begin
  Untyped := nil;
  if Assigned(AParser) then
    // The typed conversion the application wrote, boxed once, here. Everything below the
    // symbol works on TValue, so one conversion and one list of validations serve every
    // declared type instead of one of each per type.
    Untyped :=
      function(const Raw: string; out Value: TValue; out Error: string): Boolean
      var
        Converted: T;
      begin
        Converted := Default (T);
        Result := AParser(Raw, Converted, Error);
        if Result then
          Value := TValue.From<T>(Converted);
      end;
  inherited Create(AName, TypeInfo(T), Untyped);
  FDefaultValue := Default (T);
  FDefaultKnown := True;
end;

function TValuedSymbol<T>.DefaultOf: T;
begin
  // Run here and nowhere else: this is the one place reached only when the invocation
  // did not carry the symbol. And run at most once, because a calculation that reads the
  // environment or opens a file should not be paid for twice (design D4).
  if not FDefaultKnown then
  begin
    FDefaultValue := FDefaultFactory();
    FDefaultKnown := True;
  end;
  Result := FDefaultValue;
end;

function TValuedSymbol<T>.ReadValue(const Parsed: ICommandLineParseResult): T;
var
  Raw: TNullable<string>;
  Raws: TArray<string>;
  Elements: TArray<TValue>;
  Converted: TValue;
  Error: string;
  I: Integer;
begin
  if Parsed = nil then
    Exit(DefaultOf);

  // A collection holds ONE value, and that value is every token it was given. The array
  // is built through RTTI because T is the array type and the element type is only known
  // to the engine: TValue.FromArray is what bridges the two.
  if IsCollection then
  begin
    Raws := Parsed.RawValuesOf(Self);
    if Length(Raws) = 0 then
      Exit(DefaultOf);
    SetLength(Elements, Length(Raws));
    for I := 0 to High(Raws) do
      if not Convert(Raws[I], Elements[I], Error) then
        raise ECommandLine.Create(Error);
    Exit(TValue.FromArray(FTypeInfo, Elements).AsType<T>);
  end;

  Raw := Parsed.RawOf(Self);
  if Raw.IsNull then
    Exit(DefaultOf);
  if not Convert(Raw.Value, Converted, Error) then
    raise ECommandLine.Create(Error);
  Result := Converted.AsType<T>;
end;

function TValuedSymbol<T>.ReadValues(const Parsed: ICommandLineParseResult): TArray<T>;
var
  Raw: string;
  Converted: TValue;
  Error: string;
begin
  Result := nil;
  if Parsed = nil then
    Exit;
  // One value of the declared type, and for a collection that one value is the array.
  if IsCollection then
  begin
    if Length(Parsed.RawValuesOf(Self)) > 0 then
      Result := [ReadValue(Parsed)];
    Exit;
  end;
  for Raw in Parsed.RawValuesOf(Self) do
    if Convert(Raw, Converted, Error) then
      Result := Result + [Converted.AsType<T>];
end;

procedure TValuedSymbol<T>.SetDefaultValue(const Value: T);
begin
  FDefaultValue := Value;
  FDefaultFactory := nil;
  FDefaultKnown := True;
  SetDefaultText(TValue.From<T>(Value).ToString);
end;

procedure TValuedSymbol<T>.SetDefaultFactory(const Factory: TFunc<T>; const Text: string);
begin
  if not Assigned(Factory) then
    raise ECommandLine.CreateFmt('The computed default of "%s" is not assigned', [Name]);
  FDefaultFactory := Factory;
  FDefaultKnown := False;
  // HasDefault is what satisfies "required", and it is set even with no text to show:
  // the value exists, it is just not known yet. SetDefaultText('') leaves the help
  // showing no default rather than a false one.
  SetDefaultText(Text);
end;

procedure TValuedSymbol<T>.AddTypedValidator(const Validation: TCommandLineValidator<T>);
begin
  if not Assigned(Validation) then
    raise ECommandLine.CreateFmt('A validation declared on "%s" is not assigned', [Name]);
  if IsCollection then
    // Same reason as the conversion: a validation receives the value of one token, and T
    // here is the array. Refused where it is declared rather than silently never run.
    // The two of serie do not have the problem -- they are written over the value the
    // engine boxes, so they run per element.
    raise ECommandLine.CreateFmt('A validation of its own cannot be declared on "%s", which is a collection: ' +
      'it would receive one value and the declared type is the whole collection', [Name]);
  AddValidator(
    function(const Value: TValue; out Error: string): Boolean
    begin
      Error := '';
      Result := Validation(Value.AsType<T>, Error);
    end);
end;

{ TOption<T> }

function TOption<T>.IsRecursive: Boolean;
begin
  Result := FRecursive;
end;

function TOption<T>.IsRequired: Boolean;
begin
  Result := FRequiredFlag;
end;

function TOption<T>.AllowsMultipleArgumentsPerToken: Boolean;
begin
  Result := FMultiplePerToken;
end;

function TOption<T>.AllowMultipleArgumentsPerToken: IOption<T>;
begin
  FMultiplePerToken := True;
  Result := Self;
end;

function TOption<T>.ValueIn(const Parsed: ICommandLineParseResult): T;
begin
  Result := ReadValue(Parsed);
end;

function TOption<T>.ValuesIn(const Parsed: ICommandLineParseResult): TArray<T>;
begin
  Result := ReadValues(Parsed);
end;

function TOption<T>.WithDefault(const Value: T): IOption<T>;
begin
  SetDefaultValue(Value);
  Result := Self;
end;

function TOption<T>.WithDefault(const Factory: TFunc<T>; const Text: string): IOption<T>;
begin
  SetDefaultFactory(Factory, Text);
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

function TOption<T>.Validate(const Validation: TCommandLineValidator<T>): IOption<T>;
begin
  AddTypedValidator(Validation);
  Result := Self;
end;

function TOption<T>.AcceptExistingOnly: IOption<T>;
begin
  DemandExistingPath;
  Result := Self;
end;

function TOption<T>.AcceptLegalFileNamesOnly: IOption<T>;
begin
  DemandLegalFileName;
  Result := Self;
end;

function TOption<T>.Hidden: IOption<T>;
begin
  Hide;
  Result := Self;
end;

{ TArgument<T> }

function TArgument<T>.ValueIn(const Parsed: ICommandLineParseResult): T;
begin
  Result := ReadValue(Parsed);
end;

function TArgument<T>.ValuesIn(const Parsed: ICommandLineParseResult): TArray<T>;
begin
  Result := ReadValues(Parsed);
end;

function TArgument<T>.WithDefault(const Value: T): IArgument<T>;
begin
  SetDefaultValue(Value);
  Result := Self;
end;

function TArgument<T>.WithDefault(const Factory: TFunc<T>; const Text: string): IArgument<T>;
begin
  SetDefaultFactory(Factory, Text);
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

function TArgument<T>.Validate(const Validation: TCommandLineValidator<T>): IArgument<T>;
begin
  AddTypedValidator(Validation);
  Result := Self;
end;

function TArgument<T>.AcceptExistingOnly: IArgument<T>;
begin
  DemandExistingPath;
  Result := Self;
end;

function TArgument<T>.AcceptLegalFileNamesOnly: IArgument<T>;
begin
  DemandLegalFileName;
  Result := Self;
end;

function TArgument<T>.Hidden: IArgument<T>;
begin
  Hide;
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

function TParseResult.UnmatchedTokens: TArray<string>;
begin
  Result := FUnmatched;
end;

procedure TParseResult.AddError(const Text: string);
begin
  FErrors := FErrors + [Text];
end;

procedure TParseResult.AddUnmatched(const Token: string);
begin
  FUnmatched := FUnmatched + [Token];
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

function TCommand.TreatsUnmatchedTokensAsErrors: Boolean;
begin
  Result := not FAllowUnmatched;
end;

function TCommand.AllowUnmatchedTokens: ICommand;
begin
  FAllowUnmatched := True;
  Result := Self;
end;

function TCommand.Hidden: ICommand;
begin
  Hide;
  Result := Self;
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

{ TScopeKnowledge }

procedure TScopeKnowledge.Enter(const Command: ICommand; const Available: TArray<IOption>; AtRoot: Boolean);
begin
  FCurrent := Command;
  FAvailable := Available;
  FAtRoot := AtRoot;
end;

function TScopeKnowledge.Knows(const Token: string): Boolean;
begin
  // The two built-in tokens count as declared. Otherwise `/help` would not reach the
  // help and `-h` could be swallowed into a bundle, which is the kind of hole a
  // rewriting opens by only ever looking at what the application declared.
  Result := (FindOption(FCurrent, FAvailable, Token) <> nil) or IsHelpToken(Token) or
    (FAtRoot and IsVersionToken(Token));
end;

function TScopeKnowledge.KnowsShort(const Letter: Char): Boolean;
begin
  Result := Knows(ShortPrefix + Letter);
end;

function TScopeKnowledge.ShortTakesValue(const Letter: Char): Boolean;
var
  Option: IOption;
begin
  Option := FindOption(FCurrent, FAvailable, ShortPrefix + Letter);
  // A built-in answers nil and is a flag: asking for help takes no value.
  Result := (Option <> nil) and not Option.IsFlag;
end;

function TRootCommand.Parse(const Args: TArray<string>): ICommandLineParseResult;
var
  Outcome: TParseResult;
  Current: ICommand;
  Available: TArray<IOption>;
  Index: Integer;
  Positional: Integer;
  Token, OptionName, InlineValue, Raw, Error, Next: string;
  HasInline, OptionsEnded: Boolean;
  Option: IOption;
  Argument: IArgument;
  Subcommand: ICommand;
  Scope: TScopeKnowledge;
  Known: IKnownTokens;
  Tokens, Expansion: TArray<string>;
  Step: TTokenRewrite;
begin
  Outcome := TParseResult.Create;
  Result := Outcome;
  Current := Self;
  Available := RecursiveOptionsOf(Current);
  Index := 0;
  Positional := 0;
  OptionsEnded := False;

  Scope := TScopeKnowledge.Create;
  Known := Scope;
  Scope.Enter(Current, Available, True);

  // Response files are resolved before anything else is decided about any token,
  // because all they do is contribute tokens and nothing else (design D1, D6).
  Tokens := TResponseFiles.Expand(Args, Expansion);
  for Error in Expansion do
    Outcome.AddError(Error);

  while Index < Length(Tokens) do
  begin
    Token := Tokens[Index];

    // POSIX: `--` ends the options. Everything after it is a value, dashes and all,
    // which is the only way to pass a positional whose value looks like an option.
    if (Token = EndOfOptions) and not OptionsEnded then
    begin
      OptionsEnded := True;
      Inc(Index);
      Continue;
    end;

    // Before anything else is decided about this token, it is put in the canonical form
    // the rest of this loop already understands (design D1). The rewriting is asked
    // HERE, and not once over the whole array, because it needs to know what is
    // declared and that depends on the command reached.
    if not OptionsEnded then
    begin
      Step := TTokenNormalizer.Rewrite(Token, Known);
      if Step.Error <> '' then
      begin
        Outcome.AddError(Step.Error);
        Inc(Index);
        Continue;
      end;
      if Step.Changed then
      begin
        Tokens := TTokenNormalizer.Splice(Tokens, Index, Step.Tokens);
        Continue;
      end;
    end;

    TTokenNormalizer.SplitToken(Token, OptionName, InlineValue, HasInline);

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

    if not OptionsEnded and TTokenNormalizer.LooksLikeOption(Token) then
    begin
      Option := FindOption(Current, Available, OptionName);
      if Option = nil then
      begin
        // A wrapper hands on what is not its own; anything else refuses it, which is
        // still the default (design D5). The token goes on WHOLE, value and all, so
        // whoever receives it gets what was written.
        if Current.TreatsUnmatchedTokensAsErrors then
          Outcome.AddError(Format('Unknown option "%s" for %s', [OptionName, Current.FullPath]))
        else
          Outcome.AddUnmatched(Token);
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
        if not HasInline and not Option.IsFlag and (Index + 1 < Length(Tokens)) then
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
        if (Index + 1 < Length(Tokens)) and TCommandLineValue.IsBooleanLiteral(Tokens[Index + 1]) then
        begin
          Inc(Index);
          Raw := Tokens[Index];
        end
        else
          Raw := 'true';
      end
      else
      begin
        if Index + 1 >= Length(Tokens) then
        begin
          Outcome.AddError(Format('Option "%s" requires a %s value', [Option.Name, Option.TypeName]));
          Inc(Index);
          Continue;
        end;
        Inc(Index);
        // Taken unconditionally, dash or no dash: this is what lets --port -5 through.
        Raw := Tokens[Index];
      end;

      if Option.TryAccept(Raw, Error) then
        Outcome.Bind(Option, Raw)
      else
        Outcome.AddError(Error);

      // One mention of the name, several values -- but only for an option that asked
      // for it (design D4). Taking them stops at the next token that looks like an
      // option, at `--`, and at the maximum the arity declared, so what is swallowed is
      // bounded by what was declared rather than by where the invocation happens to end.
      if Option.AllowsMultipleArgumentsPerToken and Option.Arity.AllowsMany then
        while (Index + 1 < Length(Tokens)) and (Length(Outcome.RawValuesOf(Option)) < Option.Arity.Max) do
        begin
          Next := Tokens[Index + 1];
          if (Next = EndOfOptions) or TTokenNormalizer.LooksLikeOption(Next) then
            Break;
          Inc(Index);
          if Option.TryAccept(Next, Error) then
            Outcome.Bind(Option, Next)
          else
            Outcome.AddError(Error);
        end;

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
        Scope.Enter(Current, Available, False);
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
    else if Current.TreatsUnmatchedTokensAsErrors then
      Outcome.AddError(Format('Unexpected argument "%s" for %s', [Token, Current.FullPath]))
    else
      Outcome.AddUnmatched(Token);
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
  Created := TOption<T>.Create(Name, nil);
  Created.SetDescription(Description);
  Result := Created;
end;

class function CommandLine.Option<T>(const Name: string; const Parser: TCommandLineParser<T>;
  const Description: string): IOption<T>;
var
  Created: TOption<T>;
begin
  Created := TOption<T>.Create(Name, Parser);
  Created.SetDescription(Description);
  Result := Created;
end;

class function CommandLine.Argument<T>(const Name: string; const Description: string): IArgument<T>;
var
  Created: TArgument<T>;
begin
  Created := TArgument<T>.Create(Name, nil);
  Created.SetDescription(Description);
  Result := Created;
end;

class function CommandLine.Argument<T>(const Name: string; const Parser: TCommandLineParser<T>;
  const Description: string): IArgument<T>;
var
  Created: TArgument<T>;
begin
  Created := TArgument<T>.Create(Name, Parser);
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
