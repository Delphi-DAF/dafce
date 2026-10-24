unit Daf.CommandLine;

{
  A port of System.CommandLine -- the library the dotnet CLI itself is built on --
  as close to the original as Delphi allows, with every divergence declared.

  The one divergence that shows in every call is how a value is read. In .NET you
  write result.GetValue(fileOption). Here you write FileOption.ValueIn(Parsed): the
  object asks the result, not the result the object.
  An interface cannot declare a generic method at all --
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
  System.Classes,
  System.TypInfo,
  System.Rtti,
  System.Generics.Defaults,
  Daf.CommandLine.Tokens,
  Daf.CommandLine.Values,
  Daf.CommandLine.Resources,
  Daf.Nullable,
  Daf.Application.Version;

type
  ECommandLine = class(Exception);

  /// <summary>The key of each text the command line writes, with the name .NET gives it.</summary>
  TCommandLineText = Daf.CommandLine.Resources.TCommandLineText;
  /// <summary>The texts an application writes instead of the English ones. See <c>CommandLine.Texts</c>.</summary>
  TCommandLineTexts = Daf.CommandLine.Resources.TCommandLineTexts;

  /// <summary>The path of a file, as .NET's <c>FileInfo</c>: a string the engine knows to be one.</summary>
  /// <remarks>
  ///   It is a <c>type string</c>, so it is assigned to and from a string with no conversion. What it adds is
  ///   what <c>AcceptExistingOnly</c> asks for, and the name the help gives the value.
  /// </remarks>
  TFilePath = Daf.CommandLine.Values.TFilePath;
  /// <summary>The path of a directory, as .NET's <c>DirectoryInfo</c>. See <c>TFilePath</c>.</summary>
  TDirectoryPath = Daf.CommandLine.Values.TDirectoryPath;

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
  ///   would win, in silence. On the symbol, its reach is exactly where it
  ///   was written.
  /// </remarks>
  TCommandLineParser<T> = reference to function(const Raw: string; out Value: T; out Error: string): Boolean;

  /// <summary>
  ///   A validation declared by whoever uses the module: receives the already converted
  ///   value and may refuse it with a message of its own.
  /// </summary>
  /// <remarks>
  ///   It runs INSIDE the parse, so its refusal comes out through <c>Errors</c> like any
  ///   other invocation error. Validating afterwards moves the failure to
  ///   where it can no longer be reported next to the rest, and the tool ends up with two
  ///   ways of saying that an invocation was wrong.
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

  IParseResult = interface;
  ISymbolResult = interface;
  ICommandResult = interface;
  IOptionResult = interface;
  IArgumentResult = interface;
  IDirectiveResult = interface;

  /// <summary>
  ///   What a command does when an invocation reaches it. It receives the parse result, reads the
  ///   values from it like any other code does, and answers the code the process ends with.
  /// </summary>
  TCommandAction = reference to function(const Parsed: IParseResult): Integer;
  /// <summary>An action with no code of its own: the invocation answers <c>TCommandLineExit.Ok</c> for it.</summary>
  TCommandActionProc = reference to procedure(const Parsed: IParseResult);

  /// <summary>A validation of a command's invocation as a whole. It refuses with <c>Result.AddError</c>.</summary>
  /// <remarks>
  ///   It runs only on the innermost command the invocation reached, and before its options and arguments are
  ///   validated: an error it adds keeps them from being validated at all.
  /// </remarks>
  TCommandValidator = reference to procedure(const Result: ICommandResult);
  /// <summary>A validation of an option's result. It runs after the arity is checked and before the value is converted.</summary>
  TOptionValidator = reference to procedure(const Result: IOptionResult);
  /// <summary>A validation of an argument's result. It runs after the arity is checked and before the value is converted.</summary>
  TArgumentValidator = reference to procedure(const Result: IArgumentResult);
  /// <summary>A conversion of the whole result of a symbol: it receives every token, and answers the value whole.</summary>
  /// <remarks>
  ///   It refuses with <c>ArgumentResult.AddError</c>. For a collection it answers the whole array, the only
  ///   conversion of its own a collection takes. It may keep part of the tokens of an argument and leave the
  ///   rest for the next one with <c>OnlyTake</c>. An exception it raises leaves the parse as it is.
  /// </remarks>
  TCustomParser<T> = reference to function(const ArgumentResult: IArgumentResult): T;
  /// <summary>A default calculated from the result of the symbol: it reads the rest of the invocation through it.</summary>
  /// <remarks>It refuses with <c>ArgumentResult.AddError</c>, which fails the value as a conversion would.</remarks>
  TDefaultValueFactory<T> = reference to function(const ArgumentResult: IArgumentResult): T;
  /// <summary>The type-erased shape of <c>TCustomParser&lt;T&gt;</c>, which a symbol keeps.</summary>
  TUntypedWholeParser = reference to function(const ArgumentResult: IArgumentResult): TValue;

  /// <summary>What an invocation runs: the action of a command, of an option, or the one it picks for a parse error.</summary>
  ICommandLineAction = interface
    ['{7C4B1A20-000D-4F31-9C10-4B0E2D5A000D}']
    /// <summary>
    ///   True when it runs instead of the action of the command reached. An option's action that is not
    ///   terminating runs before it, and the action of the command still runs.
    /// </summary>
    function IsTerminating: Boolean;
    /// <summary>True when, winning the invocation, it discards the parse errors that are not its own.</summary>
    function ClearsParseErrors: Boolean;
    /// <summary>Runs it, and answers the code the process ends with.</summary>
    function Invoke(const Parsed: IParseResult): Integer;
  end;

  /// <summary>What the invocation runs when the parse ends with errors and no action takes them.</summary>
  /// <remarks>
  ///   It writes the suggestions of what was meant to the output, the errors to the error followed by a
  ///   blank line, and the help of the command reached to the output, and answers
  ///   <c>TCommandLineExit.InvocationError</c>. <c>IParseResult.Action</c> hands it over, so the two
  ///   parts that are not the errors can be turned off before invoking.
  /// </remarks>
  IParseErrorAction = interface(ICommandLineAction)
    ['{7C4B1A20-000E-4F31-9C10-4B0E2D5A000E}']
    function GetShowHelp: Boolean;
    procedure SetShowHelp(Value: Boolean);
    function GetShowTypoCorrections: Boolean;
    procedure SetShowTypoCorrections(Value: Boolean);
    /// <summary>On, the help follows the errors. On by default.</summary>
    property ShowHelp: Boolean read GetShowHelp write SetShowHelp;
    /// <summary>On, what was meant is suggested before the errors. On by default.</summary>
    property ShowTypoCorrections: Boolean read GetShowTypoCorrections write SetShowTypoCorrections;
  end;

  /// <summary>The built-in action of a help option: it writes the help of the command reached.</summary>
  /// <remarks>A hidden command has no help, and for it the action writes nothing.</remarks>
  IHelpAction = interface(ICommandLineAction)
    ['{7C4B1A20-000F-4F31-9C10-4B0E2D5A000F}']
    function GetMaxWidth: Integer;
    procedure SetMaxWidth(Value: Integer);
    /// <summary>The width the lines of the help are wrapped at. <c>MaxInt</c> by default, which wraps none.</summary>
    /// <remarks>
    ///   The width of the console is never asked: a tool that wants its help to fit one sets it. Setting it below 1
    ///   raises <c>ECommandLine</c>.
    /// </remarks>
    property MaxWidth: Integer read GetMaxWidth write SetMaxWidth;
  end;

  /// <summary>The built-in action of a version option: it writes the version the root declared.</summary>
  /// <remarks>A root that declares no version writes an empty one.</remarks>
  IVersionAction = interface(ICommandLineAction)
    ['{7C4B1A20-0016-4F31-9C10-4B0E2D5A0016}']
    function GetIncludeBuildData: Boolean;
    procedure SetIncludeBuildData(Value: Boolean);
    /// <summary>
    ///   On, the version carries in its build metadata the platform it was compiled for, and whether it is a debug
    ///   build: <c>1.2.3+Win64</c>. Off by default.
    /// </summary>
    property IncludeBuildData: Boolean read GetIncludeBuildData write SetIncludeBuildData;
  end;

  /// <summary>The base of an action written as a class: terminating, and clearing no errors.</summary>
  TSynchronousCommandLineAction = class abstract(TInterfacedObject, ICommandLineAction)
  public
    function IsTerminating: Boolean; virtual;
    function ClearsParseErrors: Boolean; virtual;
    function Invoke(const Parsed: IParseResult): Integer; virtual; abstract;
  end;

  /// <summary>The codes an invocation ends with when the action is not the one deciding.</summary>
  /// <remarks>
  ///   They are contract with whoever invokes the tool from a script, who reads them and nothing
  ///   else.
  ///
  ///   <c>InvocationError</c> is a declared divergence from .NET, which answers 1 to a parse error,
  ///   the same code as an exception nobody caught. Here 1 stays for work that failed, and 2 says the
  ///   invocation does not fit the grammar: a script needs to tell the two apart.
  /// </remarks>
  TCommandLineExit = record
  public
    const Ok = 0;
    /// <summary>What the default exception handler answers.</summary>
    const Failure = 1;
    /// <summary>The invocation does not fit the declared grammar.</summary>
    const InvocationError = 2;
  end;

  /// <summary>Where an invocation writes, and what it does with what the action lets escape.</summary>
  /// <remarks>
  ///   A variable declared and never assigned already holds the defaults: the console for both
  ///   writers, and the default exception handler on, as in .NET.
  ///
  ///   There is no process termination and no cancellation -- no Ctrl+C handling, no grace period
  ///   before terminating. The invocation is synchronous from end to end, like DAF's start-up.
  /// </remarks>
  TInvocationConfiguration = record
  public
    /// <summary>
    ///   Where the help, the version and the suggestions of what was meant are written. <c>nil</c> is
    ///   the console.
    /// </summary>
    Output: TTextWriter;
    /// <summary>Where parse errors and escaped exceptions are written. <c>nil</c> is the console's error.</summary>
    Error: TTextWriter;
    /// <summary>
    ///   On, an exception leaving an action is written to <c>Error</c> as <c>Unhandled exception: </c>
    ///   followed by its class and its message, and its stack when a provider of stack traces gives
    ///   one, and the invocation answers <c>TCommandLineExit.Failure</c>. Off, it leaves the invocation
    ///   and whoever invoked sees it.
    /// </summary>
    EnableDefaultExceptionHandler: Boolean;
    class operator Initialize(out Dest: TInvocationConfiguration);
    /// <summary>Writes a line to <c>Output</c>, or to the console when it is <c>nil</c>.</summary>
    /// <remarks>It is what the default help and version actions write with.</remarks>
    procedure WriteOutput(const Text: string);
  end;

  /// <summary>Replaces a token with other tokens before the parse reads it. See <c>TParserConfiguration</c>.</summary>
  TTryReplaceToken = Daf.CommandLine.Tokens.TTryReplaceToken;

  /// <summary>How the parse reads an invocation.</summary>
  /// <remarks>
  ///   A variable declared and never assigned already holds the defaults, which are .NET's: one-character options
  ///   group, response files are read, and names tell cases apart.
  /// </remarks>
  TParserConfiguration = record
  public
    /// <summary>
    ///   On, one-character options can be written together behind one dash, <c>-fdx</c>, and an option that takes
    ///   a value can carry it glued, <c>-ovalue</c>. On by default.
    /// </summary>
    EnablePosixBundling: Boolean;
    /// <summary>
    ///   On, the names and aliases of options, commands and directives are compared without regard to case, and a grammar in
    ///   which one command answers to two names that differ only in case makes the parse raise <c>ECommandLine</c>.
    ///   Off by default.
    /// </summary>
    /// <remarks>
    ///   Not in .NET, where names always tell cases apart. It reaches no value: the names of an enumerated type
    ///   never tell cases apart, and a set of accepted values compares with its own comparer.
    /// </remarks>
    EnableCaseInsensitiveNames: Boolean;
    /// <summary>
    ///   What replaces a token starting with <c>@</c>: by default, the tokens of the response file it names.
    ///   <c>nil</c> leaves such a token as a value.
    /// </summary>
    ResponseFileTokenReplacer: TTryReplaceToken;
    class operator Initialize(out Dest: TParserConfiguration);
  end;

  /// <summary>How many values a symbol admits: a minimum and a maximum.</summary>
  /// <remarks>
  ///   The five named values are .NET's. It is rarely written by hand because it is
  ///   derived from the declared type -- a boolean admits none or one, anything else
  ///   demands exactly one -- and it is what makes "required" and "many values" the
  ///   same idea seen from its two ends.
  /// </remarks>
  TArgumentArity = record
  strict private
    FMin: Integer;
    FMax: Integer;
  public
    const Unbounded = MaxInt;
    class function Create(AMin, AMax: Integer): TArgumentArity; static;
    class function Zero: TArgumentArity; static;
    class function ZeroOrOne: TArgumentArity; static;
    class function ExactlyOne: TArgumentArity; static;
    class function ZeroOrMore: TArgumentArity; static;
    class function OneOrMore: TArgumentArity; static;
    function MinimumNumberOfValues: Integer;
    function MaximumNumberOfValues: Integer;
    /// <summary>True when more than one value fits, so a second value is not an error.</summary>
    function AllowsMany: Boolean;
    /// <summary>True when at least one value has to be there.</summary>
    function DemandsValue: Boolean;
    class operator Equal(const A, B: TArgumentArity): Boolean;
    class operator NotEqual(const A, B: TArgumentArity): Boolean;
  end;

  /// <summary>A collection a symbol owns: its aliases.</summary>
  /// <remarks>
  ///   The symbol creates it and frees it, as a memo does with its lines, and freezing the
  ///   grammar closes it. It keeps the NAME of its owner and not a reference to it: the
  ///   refusal can say whose collection it is without the two holding each other.
  /// </remarks>
  TCommandLineCollection<T> = class
  public type
    TEnumerator = record
    private
      FItems: TArray<T>;
      FIndex: Integer;
      function GetCurrent: T;
    public
      function MoveNext: Boolean;
      property Current: T read GetCurrent;
    end;
  private
    FItems: TArray<T>;
    FOwnerName: string;
    FFrozen: Boolean;
    function GetItem(Index: Integer): T;
  public
    constructor Create(const OwnerName: string);
    /// <summary>Raises <c>ECommandLine</c> once the grammar is frozen.</summary>
    procedure Add(const Item: T);
    function Count: Integer;
    function ToArray: TArray<T>;
    function GetEnumerator: TEnumerator;
    procedure Freeze;
    property Items[Index: Integer]: T read GetItem; default;
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
    /// <summary>The other names it answers to. <c>Aliases.Add</c> declares one.</summary>
    function Aliases: TCommandLineCollection<string>;
    function Description: string; overload;
    /// <summary>
    ///   True when <c>Token</c> is this symbol's name or one of its aliases, compared as
    ///   written, case included, as in .NET. The parse compares without regard to case
    ///   when its configuration asks it to: see <c>TParserConfiguration</c>.
    /// </summary>
    function Matches(const Token: string): Boolean;
    /// <summary>True when the help does not list it. It changes NOTHING else.</summary>
    /// <remarks>
    ///   It is what a diagnostic option, one in transition, or one only an internal script
    ///   uses needs: to exist without being put in front of everybody who asks how the
    ///   tool works.
    ///
    ///   Hiding is the HELP's business and nothing else. A hidden symbol that
    ///   gets an invalid value fails the same way and is named in the error: hiding it
    ///   from the help is not hiding it from whoever is already using it, and an error
    ///   that will not say what failed turns a mistake into a riddle.
    /// </remarks>
    function IsHidden: Boolean;
    /// <summary>The commands it was added to, in the order they took it.</summary>
    /// <remarks>
    ///   They are held weakly: a symbol does not keep its commands alive, so a grammar dropped by
    ///   whoever built it is freed whole.
    /// </remarks>
    function Parents: TArray<ISymbol>;
  end;

  {$SCOPEDENUMS ON}
  /// <summary>What a token of the invocation turned out to be.</summary>
  TTokenType = (Argument, Command, Option, DoubleDash, Directive);
  {$SCOPEDENUMS OFF}

  /// <summary>One token of the invocation, as the parse read it.</summary>
  TToken = record
    Value: string;
    TokenType: TTokenType;
    /// <summary>The symbol the token reaches; <c>nil</c> when it reaches none.</summary>
    Symbol: ISymbol;
    class operator Equal(const A, B: TToken): Boolean;
    class operator NotEqual(const A, B: TToken): Boolean;
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
    /// <summary>The type its tokens are converted to: the one it was declared with.</summary>
    function ValueType: PTypeInfo;
    /// <summary>The name the help gives its value; empty when none was declared.</summary>
    function HelpName: string; overload;
    function Arity: TArgumentArity; overload;
    /// <summary>
    ///   The values this symbol accepts, or empty when it does not restrict them. An
    ///   enumerated type fills this in by itself.
    /// </summary>
    function AcceptedValues: TArray<string>;
    /// <summary>
    ///   True when it has a default: one declared, or the false every boolean has without declaring it. It is what
    ///   satisfies "required".
    /// </summary>
    function HasDefaultValue: Boolean;
    /// <summary>The declared default rendered as text, for the help.</summary>
    function DefaultText: string;
    /// <summary>The default, calculated now and outside any parse. Raises <c>ECommandLine</c> when there is none.</summary>
    /// <remarks>
    ///   A calculation runs on every call. It receives a result that belongs to no parse, so what it reads of the
    ///   rest of the invocation is the default of its type, and an error it adds reaches nobody.
    /// </remarks>
    function GetDefaultValue: TValue;
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
    /// <summary>The validations of its result. <c>Validators.Add</c> declares one; <c>Freeze</c> closes them.</summary>
    function Validators: TCommandLineCollection<TOptionValidator>;
    /// <summary>What naming it in the invocation runs; <c>nil</c> when it runs nothing.</summary>
    /// <remarks>
    ///   A terminating action runs instead of the action of the command reached; one that is not runs
    ///   before it. The help and the version are options with a terminating action.
    /// </remarks>
    function Action: ICommandLineAction; overload;
  end;

  /// <summary>The option that asks for the help: recursive, and with a terminating action that writes it.</summary>
  /// <remarks>
  ///   Every root has one, answering to the five names of <c>HelpTokens</c>. Its action clears the parse
  ///   errors, because whoever asks how the tool works is not told what else they got wrong.
  ///
  ///   Its names are reserved on every command where it answers: adding an option that takes one raises
  ///   <c>ECommandLine</c>, and the parse raises it for one that an alias added afterwards takes.
  /// </remarks>
  IHelpOption = interface(IOption)
    ['{7C4B1A20-0011-4F31-9C10-4B0E2D5A0011}']
    /// <summary>Declares what naming it runs instead of writing the help. Declaring it again replaces it.</summary>
    /// <remarks>
    ///   Assigning it <c>nil</c> raises <c>ECommandLine</c>, as in .NET. <c>IRootCommand.HelpAction</c> is another
    ///   thing: assigning it <c>nil</c> brings the default action back.
    /// </remarks>
    function Action(const Value: ICommandLineAction): IHelpOption; overload;
  end;

  /// <summary>The option that asks for the version of the tool, with a terminating action that writes it.</summary>
  /// <remarks>
  ///   Every root has one, and only the root: a version belongs to the tool. Its action clears every parse
  ///   error but its own, and its own is to be combined with another symbol of the grammar.
  ///
  ///   Its name is reserved on the root, as the names of the help are.
  /// </remarks>
  IVersionOption = interface(IOption)
    ['{7C4B1A20-0012-4F31-9C10-4B0E2D5A0012}']
    /// <summary>Declares what naming it runs instead of writing the version. See <c>IHelpOption.Action</c>.</summary>
    function Action(const Value: ICommandLineAction): IVersionOption; overload;
  end;

  IArgument = interface(IValued)
    ['{7C4B1A20-0004-4F31-9C10-4B0E2D5A0004}']
    /// <summary>The validations of its result. <c>Validators.Add</c> declares one; <c>Freeze</c> closes them.</summary>
    function Validators: TCommandLineCollection<TArgumentValidator>;
    /// <summary>
    ///   True when it keeps every token that follows its place, whatever it looks like: an option of the command
    ///   behind it is one more value.
    /// </summary>
    function CapturesRemainingTokens: Boolean;
  end;

  ICommand = interface(ISymbol)
    ['{7C4B1A20-0005-4F31-9C10-4B0E2D5A0005}']
    // Fluency lives in the interface, never in a class helper: only one helper per
    // type is in scope and helpers never apply to interfaces, so a fluent API spread
    // over units breaks -- silently, last uses wins -- the moment someone reorders a
    // uses clause.
    function Add(const Option: IOption): ICommand; overload;
    function Add(const Argument: IArgument): ICommand; overload;
    function Add(const Subcommand: ICommand): ICommand; overload;
    /// <summary>Convenience for several at once, the nearest thing to a C# collection initializer.</summary>
    function Add(const Symbols: array of ISymbol): ICommand; overload;
    function Description(const Text: string): ICommand; overload;
    function Options: TArray<IOption>;
    function Arguments: TArray<IArgument>;
    function Subcommands: TArray<ICommand>;
    /// <summary>Everything declared on it: its subcommands, then its options, then its arguments.</summary>
    function Children: TArray<ISymbol>;
    /// <summary>
    ///   The validations of its invocation as a whole. <c>Validators.Add</c> declares one; <c>Freeze</c> closes
    ///   them.
    /// </summary>
    function Validators: TCommandLineCollection<TCommandValidator>;
    /// <summary>How this command is invoked, e.g. <c>mytool build</c>.</summary>
    function FullPath: string;
    /// <summary>True when a token matching nothing is a parse error, which is the default.</summary>
    function TreatsUnmatchedTokensAsErrors: Boolean;
    /// <summary>Makes this command collect what it does not recognise instead of failing.</summary>
    /// <remarks>
    ///   It is what a WRAPPER needs -- <c>sudo -u admin apt update</c> reads its own and
    ///   hands the rest to another program. The mark belongs to the command because it
    ///   says what that command IS; the default stays as it was, because for
    ///   anything that wraps nobody an unrecognised token is a typo, and swallowing it in
    ///   silence is the worse of the two failures.
    ///
    ///   It answers <c>ICommand</c>, so on a root it is called as a statement rather than
    ///   chained -- the same caveat as <c>Description</c>.
    /// </remarks>
    function AllowUnmatchedTokens: ICommand;
    /// <summary>
    ///   Keeps it out of the help of its parent, and leaves it with no help of its own. See
    ///   <c>ISymbol.IsHidden</c>.
    /// </summary>
    function Hidden: ICommand;
    /// <summary>
    ///   The help text, rendered from the grammar every time it is asked for. Nothing
    ///   composed is stored anywhere, so it cannot drift from what the program accepts.
    /// </summary>
    /// <remarks>Its lines are not wrapped, and a hidden command has none.</remarks>
    function HelpText: string;
    /// <summary>The declared action, or <c>nil</c> when there is none.</summary>
    function Action: ICommandLineAction; overload;
    /// <summary>Declares what this command does when an invocation reaches it; <c>nil</c> leaves it with none.</summary>
    /// <remarks>
    ///   Declaring it again replaces it, whoever declared the first one, as in .NET, where the
    ///   action is a property of the command.
    ///
    ///   A command with subcommands and an action is an invocation of its own. Without an action
    ///   it only groups, and the invocation has to name one of its leaves.
    ///
    ///   It answers <c>ICommand</c>, so on a root it is called as a statement rather than
    ///   chained -- the same caveat as <c>Description</c>.
    /// </remarks>
    function Action(const Value: ICommandLineAction): ICommand; overload;
    /// <summary>The same, written as a function. The command holds it inside an action.</summary>
    function SetAction(const Value: TCommandAction): ICommand; overload;
    /// <summary>The same, for an action with no code of its own: the invocation answers <c>Ok</c>.</summary>
    function SetAction(const Value: TCommandActionProc): ICommand; overload;
    /// <summary>Parses an invocation against the grammar of this command, as the root of the parse.</summary>
    /// <remarks>
    ///   The options its ancestors declared recursive answer too. A first token that is its name or one of its
    ///   aliases, or a path whose file name is, is taken for the command and not for a value, as in .NET -- unless
    ///   what precedes its first <c>:</c> or <c>=</c> is one of its options.
    /// </remarks>
    function Parse(const Args: TArray<string>): IParseResult; overload;
    function Parse(const Args: TArray<string>; const Configuration: TParserConfiguration): IParseResult; overload;
    /// <summary>The same, for an invocation written as one line, split with <c>CommandLine.SplitCommandLine</c>.</summary>
    /// <remarks>A first token that is <c>CommandLine.ExecutablePath</c> is taken for the command too.</remarks>
    function Parse(const CommandLine: string): IParseResult; overload;
    function Parse(const CommandLine: string; const Configuration: TParserConfiguration): IParseResult; overload;
  end;

  /// <summary>An instruction to the parser and not to the application, written <c>[name]</c> or <c>[name:value]</c>.</summary>
  /// <remarks>
  ///   Only a root declares them, and an invocation writes them before any other token but the root's own name:
  ///   from the first token that is neither, and behind <c>--</c>, nothing is a directive. Square brackets are
  ///   what no option is written with, so a directive is never taken for the grammar of the application.
  ///
  ///   With at least one declared, a token of that shape in that place that names none of them matches nothing.
  ///   With none declared, it is a value like any other -- a declared divergence from .NET, which drops it in
  ///   silence.
  ///
  ///   Its name tells cases apart as the names of options and commands do: see <c>TParserConfiguration</c>.
  /// </remarks>
  IDirective = interface(ISymbol)
    ['{7C4B1A20-0013-4F31-9C10-4B0E2D5A0013}']
    /// <summary>What writing it runs; <c>nil</c> when it runs nothing.</summary>
    /// <remarks>
    ///   A terminating action runs instead of the action of the command reached, and instead of the help and the
    ///   version too: a directive speaks about the parse, not about the program. One that is not terminating runs
    ///   before the action that wins.
    /// </remarks>
    function Action: ICommandLineAction; overload;
    /// <summary>Declares what writing it runs; <c>nil</c> leaves it with none. Declaring it again replaces it.</summary>
    /// <remarks>Like the action of an option, it stays open after <c>Freeze</c>.</remarks>
    function Action(const Value: ICommandLineAction): IDirective; overload;
  end;

  /// <summary>The <c>[diagram]</c> directive: it writes how the invocation was understood instead of running it.</summary>
  /// <remarks>
  ///   What it writes is <c>IParseResult.Diagram</c>, to the output of the invocation, and the invocation answers
  ///   <c>Ok</c> when the parse has no errors and <c>ParseErrorReturnValue</c> when it has.
  ///
  ///   Its action is terminating, so it wins over the help and the version. An action of its own replaces it, and
  ///   assigning it <c>nil</c> raises <c>ECommandLine</c>, as in .NET.
  /// </remarks>
  IDiagramDirective = interface(IDirective)
    ['{7C4B1A20-0014-4F31-9C10-4B0E2D5A0014}']
    function GetParseErrorReturnValue: Integer;
    procedure SetParseErrorReturnValue(Value: Integer);
    /// <summary>
    ///   What the invocation answers when the parse has errors. <c>TCommandLineExit.InvocationError</c> by default.
    /// </summary>
    /// <remarks>
    ///   A declared divergence from .NET, which answers 1: here 2 is what any parse error answers, and a diagram of
    ///   an invocation that does not fit the grammar says the same. Like the action, it stays open after
    ///   <c>Freeze</c>.
    /// </remarks>
    property ParseErrorReturnValue: Integer read GetParseErrorReturnValue write SetParseErrorReturnValue;
  end;

  IRootCommand = interface(ICommand)
    ['{7C4B1A20-0006-4F31-9C10-4B0E2D5A0006}']
    /// <summary>Parses the process command line, <c>ParamStr(1)</c> onwards.</summary>
    function Parse: IParseResult; overload;
    /// <summary>
    ///   Declares the version this tool answers with. It lives on the ROOT and not on
    ///   each command, as in .NET: a version belongs to the tool, not to its orders.
    /// </summary>
    function WithVersion(const Version: string): IRootCommand; overload;
    /// <summary>The same, naming the application and its company, which the text alone cannot.</summary>
    function WithVersion(const Version, AppName, Company: string): IRootCommand; overload;
    /// <summary>
    ///   Takes the version from a class annotated with it. The author picks which class carries
    ///   the annotation: nothing has to be inherited for this to work.
    /// </summary>
    function WithVersion(const AClass: TClass): IRootCommand; overload;
    /// <summary>What the tool answers with. Empty when no version was declared.</summary>
    function VersionInfo: TVersionInfo;
    /// <summary>The name the help gives the tool; empty when none was declared, and then it is the root's name.</summary>
    function HelpName: string; overload;
    function HelpName(const Value: string): IRootCommand; overload;
    /// <summary>The directives it declares, in the order they were added. A root declares no built-in one.</summary>
    function Directives: TArray<IDirective>;
    /// <summary>Declares a directive.</summary>
    /// <remarks>Only a root takes one: <c>Add</c> with an array raises <c>ECommandLine</c> on any other command.</remarks>
    function Add(const Directive: IDirective): IRootCommand; overload;
    /// <summary>
    ///   Closes the grammar: from here on, changing it raises <c>ECommandLine</c>. It reaches every
    ///   command, option, argument and directive already declared below the root.
    /// </summary>
    /// <remarks>
    ///   What it closes is what the parse judges by -- symbols, aliases, arities, accepted values,
    ///   defaults, validations, the version. The actions stay open, the help and the version ones
    ///   included: they are read when the result is invoked, not when it is parsed.
    /// </remarks>
    procedure Freeze;
    function GetHelpAction: TCommandAction;
    procedure SetHelpAction(const Value: TCommandAction);
    function GetVersionAction: TCommandAction;
    procedure SetVersionAction(const Value: TCommandAction);
    /// <summary>
    ///   What settles an invocation that asks for help, whichever command it reached. Its code is
    ///   the code of the invocation.
    /// </summary>
    /// <remarks>
    ///   It is the action of the root's help option, written as a function: replacing it here
    ///   replaces it there, and the function replacing it clears the parse errors as the help does.
    ///
    ///   The default one writes the help of the command reached to the output of the invocation and
    ///   answers <c>Ok</c>. It is an <c>IHelpAction</c>, which the option hands over, and the width the
    ///   help is wrapped at is set there. An action that writes something of its own and then calls
    ///   the default one puts it above the help, and the help is still the one derived from the
    ///   grammar. Assigning <c>nil</c> brings the default one back.
    ///
    ///   The replacement captures the default action, never the root: an action holding the root it
    ///   is installed on is a cycle between counted references, and neither is ever freed.
    /// </remarks>
    property HelpAction: TCommandAction read GetHelpAction write SetHelpAction;
    /// <summary>What settles an invocation that asks for the version. See <c>HelpAction</c>.</summary>
    /// <remarks>
    ///   The action of the root's version option. The default one writes the declared version and
    ///   answers <c>Ok</c>. It is an <c>IVersionAction</c>, which the option hands over.
    /// </remarks>
    property VersionAction: TCommandAction read GetVersionAction write SetVersionAction;
  end;

  /// <summary>One error of the parse, and the result of the symbol it belongs to.</summary>
  TParseError = record
    Message: string;
    /// <summary>
    ///   <c>nil</c> for an error of reading the invocation itself, such as a response file that does
    ///   not exist: it happened before any symbol was reached.
    /// </summary>
    SymbolResult: ISymbolResult;
  end;

  /// <summary>What the parse made of one symbol: a node of the tree the parse result is.</summary>
  /// <remarks>
  ///   A result is a view of the parse that produced it. Kept beyond the parse result, it still says
  ///   its own symbol and tokens, and answers nothing about the rest of the tree.
  /// </remarks>
  ISymbolResult = interface
    ['{7C4B1A20-0009-4F31-9C10-4B0E2D5A0009}']
    /// <summary>The result of the command it hangs from; <c>nil</c> for the root.</summary>
    /// <remarks>
    ///   Held weakly, as the tree holds its children. A chain such as <c>X.Parent.Tokens</c> keeps the
    ///   parent alive in a hidden temporary until the routine that writes it returns.
    /// </remarks>
    function Parent: ISymbolResult;
    /// <summary>The tokens of the invocation that produced it.</summary>
    function Tokens: TArray<TToken>;
    /// <summary>The errors of the parse that belong to it, in the order they were found.</summary>
    function Errors: TArray<TParseError>;
    /// <summary>Adds an error of its own to the parse.</summary>
    procedure AddError(const Message: string);
    /// <summary>The result of a symbol anywhere in the tree; <c>nil</c> when it has none.</summary>
    function GetResult(const Option: IOption): IOptionResult; overload;
    function GetResult(const Argument: IArgument): IArgumentResult; overload;
    function GetResult(const Command: ICommand): ICommandResult; overload;
    function GetResult(const Directive: IDirective): IDirectiveResult; overload;
  end;

  ICommandResult = interface(ISymbolResult)
    ['{7C4B1A20-000A-4F31-9C10-4B0E2D5A000A}']
    function Command: ICommand;
    /// <summary>
    ///   The token that named the command. The root's carries its name, or the first token as written when the
    ///   invocation began with it.
    /// </summary>
    function IdentifierToken: TToken;
    /// <summary>The results that hang from it, in the order the parse created them.</summary>
    function Children: TArray<ISymbolResult>;
  end;

  IOptionResult = interface(ISymbolResult)
    ['{7C4B1A20-000B-4F31-9C10-4B0E2D5A000B}']
    function Option: IOption;
    /// <summary>
    ///   True when the invocation did not name it: the parse created it for a default, or for a
    ///   required option that is missing, and then it carries that error.
    /// </summary>
    function IsImplicit: Boolean;
    /// <summary>The token that named it first; null when it is implicit.</summary>
    function IdentifierToken: TNullable<TToken>;
    /// <summary>How many times the invocation named it.</summary>
    function IdentifierTokenCount: Integer;
  end;

  IArgumentResult = interface(ISymbolResult)
    ['{7C4B1A20-000C-4F31-9C10-4B0E2D5A000C}']
    function Argument: IArgument;
    /// <summary>
    ///   Keeps the first <c>NumberOfTokens</c> tokens and hands the rest to the arguments that follow, each up to
    ///   the most its arity admits; what none of them takes is a token nothing claimed.
    /// </summary>
    /// <remarks>
    ///   Called from a conversion of its own, once. It raises <c>ECommandLine</c> on the argument an option carries
    ///   its value in, which has no argument behind it.
    /// </remarks>
    procedure OnlyTake(NumberOfTokens: Integer);
    /// <summary>True when it took its default because the invocation carried no value for it.</summary>
    /// <remarks>
    ///   A required argument that is missing also has a result, with that error, and it is NOT
    ///   implicit: the same as in .NET, where only the default makes an argument result implicit.
    /// </remarks>
    function IsImplicit: Boolean;
  end;

  /// <summary>What the parse made of a directive the invocation wrote.</summary>
  /// <remarks>
  ///   It hangs from no command, because a directive belongs to the parser and not to the grammar: its
  ///   <c>Parent</c> is <c>nil</c>, and no <c>Children</c> lists it.
  /// </remarks>
  IDirectiveResult = interface(ISymbolResult)
    ['{7C4B1A20-0015-4F31-9C10-4B0E2D5A0015}']
    function Directive: IDirective;
    /// <summary>The token that wrote it first. <c>Tokens</c> holds every one, when it was written more than once.</summary>
    function Token: TToken;
    /// <summary>What follows the first <c>:</c> of each <c>[name:value]</c> written, in order. Empty for <c>[name]</c>.</summary>
    function Values: TArray<string>;
  end;

  IParseResult = interface
    ['{7C4B1A20-0007-4F31-9C10-4B0E2D5A0007}']
    /// <summary>The leaf command the invocation reached.</summary>
    function Command: ICommand;
    /// <summary>The result of the command the invocation reached.</summary>
    function CommandResult: ICommandResult;
    /// <summary>The result of the root, where the tree starts.</summary>
    function RootCommandResult: ICommandResult;
    /// <summary>The tokens the invocation was read into, response files already expanded.</summary>
    function Tokens: TArray<TToken>;
    function GetResult(const Option: IOption): IOptionResult; overload;
    function GetResult(const Argument: IArgument): IArgumentResult; overload;
    function GetResult(const Command: ICommand): ICommandResult; overload;
    /// <summary>The result of a directive the invocation wrote; <c>nil</c> when it wrote none by that name.</summary>
    function GetResult(const Directive: IDirective): IDirectiveResult; overload;
    /// <summary>
    ///   The result of any symbol, without saying what kind it is: the one the query of its kind gives, or <c>nil</c>
    ///   when it has none.
    /// </summary>
    /// <remarks>A variable of any of the kinds above goes to the query of its kind; this one takes an <c>ISymbol</c>.</remarks>
    function GetResult(const Symbol: ISymbol): ISymbolResult; overload;
    function Succeeded: Boolean;
    /// <summary>The errors of the parse, in the order they were found.</summary>
    /// <remarks>
    ///   A missing subcommand comes first, ahead of the errors of reading the invocation. The rest
    ///   follow the three steps: reading, then the tokens no symbol claimed, then the validation of
    ///   each command -- its own validations on the innermost one, then its options in the order they
    ///   were declared, then its arguments.
    ///
    ///   An action that wins and clears the parse errors, as the help and the version do, leaves only
    ///   the ones of its own option.
    /// </remarks>
    function Errors: TArray<TParseError>;
    function ErrorText: string;
    /// <summary>
    ///   True when the action that wins is the one of a help option. Asking is a valid outcome, not a
    ///   failure.
    /// </summary>
    function IsHelpRequested: Boolean;
    /// <summary>The help of the command reached when it was asked for; empty otherwise.</summary>
    function HelpText: string;
    /// <summary>True when the action that wins is the one of a version option. Also an outcome, not a failure.</summary>
    function IsVersionRequested: Boolean;
    /// <summary>The version the root declared. Formatting it is the caller's decision.</summary>
    function VersionInfo: TVersionInfo;
    /// <summary>True when the invocation carried this symbol at all.</summary>
    function WasProvided(const Symbol: ISymbol): Boolean;
    /// <summary>
    ///   The raw text bound to a symbol; absent when the invocation did not carry it.
    ///   Absent and present-but-empty are different answers.
    /// </summary>
    function RawOf(const Symbol: ISymbol): TNullable<string>;
    /// <summary>Every raw value bound to a symbol, for an arity that admits many.</summary>
    function RawValuesOf(const Symbol: ISymbol): TArray<string>;
    /// <summary>The tokens the grammar did not claim, error or not.</summary>
    /// <remarks>
    ///   Whether they are an error is the command's to say: they are, unless the command reached
    ///   collects them, or the root does. Offered either way, they are what a wrapper hands on and what
    ///   a typo is compared with.
    /// </remarks>
    function UnmatchedTokens: TArray<string>;
    /// <summary>
    ///   The action invoking runs: the terminating action of a directive the invocation wrote, else
    ///   the terminating action of an option it named, the one written for parse errors, or else the
    ///   action of the command reached.
    /// </summary>
    function Action: ICommandLineAction;
    /// <summary>How the invocation was understood, as the <c>[diagram]</c> directive writes it.</summary>
    /// <remarks>
    ///   Each command between <c>[ </c> and <c> ]</c>, with its options, its arguments and the subcommand reached
    ///   inside it, in the order the parse met them. An option shows the name it was written with and its value;
    ///   an argument, its value, and its name too when its command declares more than one. A value reads
    ///   <c>&lt;value&gt;</c>, one per value; a boolean one is not shown. <c>*</c> goes before an option that took
    ///   its default, <c>!</c> before whatever carries an error, and <c>???--&gt;</c> before the tokens that
    ///   matched nothing, at the end.
    ///
    ///   It is what .NET's parse result answers to <c>ToString</c>, which an interface cannot declare.
    /// </remarks>
    function Diagram: string;
    /// <summary>The configuration the invocation was read with.</summary>
    function Configuration: TParserConfiguration;
    /// <summary>
    ///   The configuration of the invocation settling this result, so an action writes where the
    ///   invocation writes. Before any invocation, the default one: the console.
    /// </summary>
    function InvocationConfiguration: TInvocationConfiguration;
    /// <summary>Settles the invocation, writing to the console. See the overload.</summary>
    function Invoke: Integer; overload;
    /// <summary>
    ///   Settles the invocation and answers the code the process ends with. The actions of the named
    ///   options that do not terminate run first, all of them, and then <c>Action</c>. The code is the
    ///   last one other than 0 that one of the first answered, or else the code of <c>Action</c> --
    ///   <c>Ok</c> when there is none.
    /// </summary>
    /// <remarks>
    ///   Help, version and errors never run the action of a command: asking how the tool works, or
    ///   getting its syntax wrong, is not an invocation of the work. A parse error writes what was
    ///   meant to <c>Output</c>, the errors to <c>Error</c> and the help of the command reached to
    ///   <c>Output</c>, and answers <c>InvocationError</c>.
    ///
    ///   The default exception handler covers every action the same.
    /// </remarks>
    function Invoke(const Configuration: TInvocationConfiguration): Integer; overload;
  end;

  /// <summary>An option whose value is read back with the very object declared here.</summary>
  IOption<T> = interface(IOption)
    /// <summary>
    ///   The value the invocation carried, or the declared default when the invocation reached its
    ///   command and did not carry it.
    /// </summary>
    /// <remarks>
    ///   Reads as <c>FileOption.ValueIn(Parsed)</c>. The order is inverted with respect
    ///   to .NET's <c>result.GetValue(fileOption)</c> on purpose -- see the unit header.
    ///
    ///   A value the parse refused raises <c>ECommandLine</c> with that error: one that does not
    ///   convert, more values than it admits, a required option that is missing. An option of a command the
    ///   invocation did not reach has no result, and reads as the default of its TYPE, not the
    ///   declared one.
    /// </remarks>
    function ValueIn(const Parsed: IParseResult): T; overload;
    /// <summary>The same, read on the result of a symbol: the tree is the same whichever node asks.</summary>
    function ValueIn(const SymbolResult: ISymbolResult): T; overload;
    /// <summary>The value, raising <c>ECommandLine</c> when the option has no result at all.</summary>
    function RequiredValueIn(const Parsed: IParseResult): T; overload;
    function RequiredValueIn(const SymbolResult: ISymbolResult): T; overload;
    /// <summary>Every value the invocation carried, for an arity that admits many.</summary>
    function ValuesIn(const Parsed: IParseResult): TArray<T>;
    /// <summary>Declares the default as a constant.</summary>
    function DefaultValue(const Value: T): IOption<T>;
    /// <summary>The calculation of the default, receiving the result of the option; <c>nil</c> when there is no default.</summary>
    /// <remarks>
    ///   Declared with a <c>TDefaultValueFactory</c>, it is that very one. A constant, or a calculation with no
    ///   argument, comes back inside one that receives the result and does not read it.
    /// </remarks>
    function DefaultValueFactory: TDefaultValueFactory<T>; overload;
    /// <summary>Declares the default as a CALCULATION, which the parse runs when the invocation does not carry the symbol.</summary>
    /// <remarks>
    ///   A default that depends on the surroundings -- a temporary directory, today's
    ///   date, an environment variable -- cannot be written as a constant in the
    ///   declaration of the grammar, which runs before there is anything to know. Without
    ///   this the application leaves the default empty and fills it in after reading, and
    ///   then the help shows a default that is not the one the program uses.
    ///
    ///   Each parse runs it once, when the invocation reaches the command that declares the
    ///   symbol and does not carry it -- whether or not the value is read afterwards, because the
    ///   default is part of the parse result. Nothing is kept from one parse for the next.
    ///
    ///   <c>Text</c> is what the help shows, because the help does NOT run the
    ///   calculation -- it has no way of knowing whether this one has effects, and
    ///   showing no default beats inventing one or paying for a side effect to paint a
    ///   line. With no text, no default is shown.
    /// </remarks>
    function DefaultValueFactory(const Factory: TFunc<T>; const Text: string = ''): IOption<T>; overload;
    /// <summary>
    ///   The same, receiving the result of the option, so the calculation can read the rest of the invocation
    ///   through it.
    /// </summary>
    function DefaultValueFactory(const Factory: TDefaultValueFactory<T>; const Text: string = ''): IOption<T>; overload;
    /// <summary>The conversion of its own over the whole result; <c>nil</c> when there is none.</summary>
    function CustomParser: TCustomParser<T>; overload;
    /// <summary>Declares a conversion of its own that receives every token and answers the value whole.</summary>
    function CustomParser(const Parser: TCustomParser<T>): IOption<T>; overload;
    function Arity(const Value: TArgumentArity): IOption<T>; overload;
    function Description(const Text: string): IOption<T>; overload;
    /// <summary>Declares the name the help gives its value.</summary>
    function HelpName(const Value: string): IOption<T>; overload;
    /// <summary>Makes it available to every descendant of the declaring command.</summary>
    function Recursive: IOption<T>;
    /// <summary>Makes the invocation have to carry it, unless it has a default.</summary>
    function Required: IOption<T>;
    /// <summary>Restricts it to a declared set of values, listed when one is refused.</summary>
    /// <remarks>
    ///   The values are compared as they are written, case included, as in .NET: the names of the grammar
    ///   and the values it receives are different things. The overload with a comparer says otherwise.
    ///
    ///   It is a validation of the result, so it runs before any conversion, a conversion of its own
    ///   included. Declaring it again replaces the set.
    /// </remarks>
    function AcceptOnlyFromAmong(const Values: array of string): IOption<T>; overload;
    /// <summary>The same, comparing with <c>Comparer</c>: <c>TIStringComparer.Ordinal</c> does not tell cases apart.</summary>
    function AcceptOnlyFromAmong(const Comparer: IEqualityComparer<string>; const Values: array of string): IOption<T>;
      overload;
    /// <summary>Keeps it out of the help. See <c>ISymbol.IsHidden</c>.</summary>
    function Hidden: IOption<T>;
    /// <summary>Adds a validation of its own, which runs inside the parse.</summary>
    /// <remarks>
    ///   Several can be declared, and they run in the order they were declared. The
    ///   first refusal is the error reported for that value.
    /// </remarks>
    function Validate(const Validation: TCommandLineValidator<T>): IOption<T>;
    /// <summary>Accepts only a path that exists.</summary>
    /// <remarks>
    ///   A <c>TDirectoryPath</c> has to be a directory. A <c>TFilePath</c> and a <c>string</c> take a file or a
    ///   directory, as .NET's <c>FileInfo</c> does.
    ///
    ///   It is written over <c>Validate</c>'s own hook and not over a mechanism of its
    ///   own -- if the built-in validations needed one, the hook would not be doing its
    ///   job. It ASKS THE FILE SYSTEM, which is the conscious exception to a validation
    ///   being a pure question about the value.
    /// </remarks>
    function AcceptExistingOnly: IOption<T>;
    /// <summary>
    ///   Accepts only a legal file name: none of the characters the platform refuses in one, separators
    ///   included. The error names the character.
    /// </summary>
    function AcceptLegalFileNamesOnly: IOption<T>;
    /// <summary>Accepts only a legal path: none of the characters the platform refuses in one. The error names the character.</summary>
    function AcceptLegalFilePathsOnly: IOption<T>;
    /// <summary>Lets one mention of the name be followed by several values.</summary>
    /// <remarks>
    ///   Opt-in, and it could not be anything else: were every option with
    ///   room for many values greedy by default, a positional written behind one would
    ///   stop being a positional. The right default depends on the grammar, and only
    ///   whoever wrote it knows. .NET calls it the same thing, for the same reason.
    /// </remarks>
    function AllowMultipleArgumentsPerToken: IOption<T>;
    /// <summary>Declares what naming it runs; <c>nil</c> leaves it with none. Declaring it again replaces it.</summary>
    /// <remarks>Like the action of a command, it stays open after <c>Freeze</c>.</remarks>
    function Action(const Value: ICommandLineAction): IOption<T>; overload;
  end;

  /// <summary>An argument identified by its place, not by a name behind dashes.</summary>
  IArgument<T> = interface(IArgument)
    /// <summary>The value, read as <c>IOption&lt;T&gt;.ValueIn</c> reads it.</summary>
    function ValueIn(const Parsed: IParseResult): T; overload;
    function ValueIn(const SymbolResult: ISymbolResult): T; overload;
    /// <summary>The value, and when the argument has no result, what .NET's GetRequiredValue answers.</summary>
    /// <remarks>
    ///   It raises <c>ECommandLine</c> when the default of the type is null in .NET: a string, a class,
    ///   an interface. Any other type answers its default, and an array answers empty.
    /// </remarks>
    function RequiredValueIn(const Parsed: IParseResult): T; overload;
    function RequiredValueIn(const SymbolResult: ISymbolResult): T; overload;
    function ValuesIn(const Parsed: IParseResult): TArray<T>;
    function DefaultValue(const Value: T): IArgument<T>;
    function DefaultValueFactory: TDefaultValueFactory<T>; overload;
    function DefaultValueFactory(const Factory: TFunc<T>; const Text: string = ''): IArgument<T>; overload;
    function DefaultValueFactory(const Factory: TDefaultValueFactory<T>; const Text: string = ''): IArgument<T>; overload;
    function CustomParser: TCustomParser<T>; overload;
    function CustomParser(const Parser: TCustomParser<T>): IArgument<T>; overload;
    function Arity(const Value: TArgumentArity): IArgument<T>; overload;
    function Description(const Text: string): IArgument<T>; overload;
    function HelpName(const Value: string): IArgument<T>; overload;
    function AcceptOnlyFromAmong(const Values: array of string): IArgument<T>; overload;
    function AcceptOnlyFromAmong(const Comparer: IEqualityComparer<string>; const Values: array of string): IArgument<T>;
      overload;
    function Validate(const Validation: TCommandLineValidator<T>): IArgument<T>;
    function AcceptExistingOnly: IArgument<T>;
    function AcceptLegalFileNamesOnly: IArgument<T>;
    function AcceptLegalFilePathsOnly: IArgument<T>;
    function Hidden: IArgument<T>;
    /// <summary>Makes it keep every token that follows its place, options included.</summary>
    /// <remarks>
    ///   What a command that hands its tail to another program needs -- <c>exec app --verbose</c> -- without
    ///   asking whoever invokes it to write <c>--</c>. Taking starts at its place: an option before the first
    ///   value is still an option.
    /// </remarks>
    function CaptureRemainingTokens: IArgument<T>;
  end;

  // The classes below are declared here rather than in the implementation section on
  // purpose: a parameterized method may not name a symbol local to implementation
  // (E2506), and the CommandLine facade below reaches them from generic methods.

  TSymbol = class(TInterfacedObject, ISymbol)
  private type
    /// <summary>
    ///   A record and not the interface itself, because a dynamic array cannot mark its elements
    ///   <c>[weak]</c>, and a symbol holding its command strongly is a cycle: the command holds it.
    /// </summary>
    TParentLink = record
      [weak] Symbol: ISymbol;
    end;
  private
    FName: string;
    FAliases: TCommandLineCollection<string>;
    FDescription: string;
    FHidden: Boolean;
    FFrozen: Boolean;
    FParents: TArray<TParentLink>;
    class procedure RaiseFrozen(const SymbolName: string); static;
  protected
    /// <summary>Raises when the grammar this symbol belongs to was frozen.</summary>
    procedure CheckOpen;
    /// <summary>Raises when <c>Value</c> is <c>nil</c>: for what cannot be left with no action.</summary>
    procedure DemandAction(const Value: ICommandLineAction);
  public
    constructor Create(const AName: string);
    destructor Destroy; override;
    function AsObject: TObject;
    function Name: string;
    function Aliases: TCommandLineCollection<string>;
    function Description: string; overload;
    function Matches(const Token: string): Boolean;
    function IsHidden: Boolean;
    function Parents: TArray<ISymbol>;
    procedure AddParent(const Parent: ISymbol);
    procedure SetDescription(const Text: string);
    procedure Hide;
    procedure Freeze; virtual;
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
    FArity: TArgumentArity;
    FAccepted: TArray<string>;
    FAcceptedComparer: IEqualityComparer<string>;
    FHelpName: string;
    FHasDefault: Boolean;
    FDefaultText: string;
    FParser: TUntypedParser;
    FWholeParser: TUntypedWholeParser;
    /// <summary>True when the declared type takes many values rather than one.</summary>
    function IsCollection: Boolean;
    /// <summary>Refuses at DECLARATION a path validation on something that is not one.</summary>
    procedure DemandPathType(const What: string);
    /// <summary>
    ///   The values of this symbol's result, converted. False when the symbol has no result;
    ///   <c>FromDefault</c> says the one value is the whole default. Raises the error of a result
    ///   the parse refused.
    /// </summary>
    /// <remarks><c>Whole</c> says the one value is the whole value: the default, or what a conversion of its own answered.</remarks>
    function TryReadIn(const Found: ISymbolResult; out Values: TArray<TValue>; out FromDefault, Whole: Boolean): Boolean;
    /// <summary>The same, raising when there is no result and the default of the type is null in .NET.</summary>
    function TryReadRequiredIn(const Found: ISymbolResult; MissingRaises: Boolean; out Values: TArray<TValue>;
      out FromDefault, Whole: Boolean): Boolean;
    /// <summary>True for the types whose default is null in .NET: a string, a class, an interface.</summary>
    function DefaultIsNull: Boolean;
    /// <summary>The error of a value that does not convert. See <c>Convert</c>.</summary>
    function ConversionError(const Raw, CommandName: string): string;
    /// <summary>
    ///   A new argument to carry the value of this symbol, for an option. Here and not in the generic class,
    ///   which may not name a type of the implementation section (E2506).
    /// </summary>
    function NewValueArgument: IArgument;
    class procedure FreezeValueArgument(const Argument: IArgument); static;
  public
    constructor Create(const AName: string; ATypeInfo: PTypeInfo; const AParser: TUntypedParser);
    /// <summary>Converts one token, through the declared conversion when there is one.</summary>
    /// <remarks>
    ///   The names of an enumerated type are checked here, as part of converting to it. The declared set of
    ///   accepted values is not: it is a validation, and the validations run before, on the whole result --
    ///   see <c>ValueValidators</c>.
    /// </remarks>
    /// <remarks>
    ///   <c>CommandName</c> is the command the value was given to: a refused value of an argument names it, and one
    ///   of an option names the option. Empty, the value was given to none, and the refusal names only the type.
    /// </remarks>
    function Convert(const Raw, CommandName: string; out Value: TValue; out Error: string): Boolean;
    /// <summary>True when the declared set of accepted values holds <c>Raw</c>, by its comparer.</summary>
    function IsAccepted(const Raw: string): Boolean;
    /// <summary>
    ///   Where the validations of its value go -- the typed <c>Validate</c> and the built-in ones: the
    ///   validations of the argument the value is carried in.
    /// </summary>
    /// <remarks>
    ///   An argument carries its own value; an option carries it in an argument of its own, as in .NET. Either
    ///   way an error they add fails the conversion, so reading the value raises it.
    /// </remarks>
    function ValueValidators: TCommandLineCollection<TArgumentValidator>; virtual; abstract;
    /// <summary>The argument the value is carried in: the argument itself, or the one of an option.</summary>
    function ValueArgument: IArgument; virtual; abstract;
    /// <summary>The conversion of its own over the whole result, type-erased; <c>nil</c> when there is none.</summary>
    function WholeParser: TUntypedWholeParser;
    /// <summary>
    ///   Refuses, naming the symbol and the type, a type the engine does not convert when no conversion of its own
    ///   was declared for it.
    /// </summary>
    /// <remarks>
    ///   Adding the symbol to a command is where it is asked: a conversion of its own can be declared after the
    ///   symbol is, so its declaration cannot know yet, and every symbol the parse reads was added to a command.
    /// </remarks>
    procedure DemandConvertible;
    /// <summary>
    ///   The declared default, calculated now when it is a calculation. <c>Found</c> is the result the value is
    ///   carried in, which a calculation that receives it reads.
    /// </summary>
    function BoxedDefault(const Found: IArgumentResult): TValue; virtual; abstract;
    function TryAccept(const Raw: string; out Error: string): Boolean;
    function IsFlag: Boolean;
    function TypeName: string;
    function ValueType: PTypeInfo;
    function HelpName: string; overload;
    function Arity: TArgumentArity; overload;
    function AcceptedValues: TArray<string>;
    function HasDefaultValue: Boolean;
    function DefaultText: string;
    function GetDefaultValue: TValue;
    procedure SetArity(const Value: TArgumentArity);
    procedure SetHelpName(const Value: string);
    procedure SetAccepted(const Comparer: IEqualityComparer<string>; const Values: array of string);
    procedure SetDefaultText(const Text: string);
    procedure AddValidator(const Validator: TUntypedValidator);
    // The built-in validations on a path, written over the hook above and nothing else.
    procedure DemandExistingPath;
    procedure DemandLegalFileName;
    procedure DemandLegalFilePath;
  end;

  /// <summary>What an option and an argument share once <c>T</c> is known.</summary>
  /// <remarks>
  ///   It exists so that every mechanism that needs <c>T</c> -- the declared conversion,
  ///   the validations, the default -- is written once instead of once per kind of
  ///   symbol. Two copies of it would be two places for the next mechanism to be
  ///   forgotten in.
  /// </remarks>
  TValuedSymbol<T> = class(TValuedSymbol)
  private
    // Static, so the closures they build capture what they are handed and never the symbol, which holds them.
    class function ConstantFactory(const Value: T): TDefaultValueFactory<T>; static;
    class function CalculationFactory(const Calculation: TFunc<T>): TDefaultValueFactory<T>; static;
  protected
    /// <summary>The default however it was declared: a constant or a calculation with no argument comes in here wrapped.</summary>
    FDefaultFactory: TDefaultValueFactory<T>;
    FCustomParser: TCustomParser<T>;
    function DefaultOf(const Found: IArgumentResult): T;
    function Unbox(const Values: TArray<TValue>; Whole: Boolean): T;
    /// <summary><c>Found</c> is this symbol's result, or <c>nil</c> when it has none.</summary>
    function ValueOf(const Found: ISymbolResult): T;
    function RequiredValueOf(const Found: ISymbolResult; MissingRaises: Boolean): T;
    function ValuesOf(const Found: ISymbolResult): TArray<T>;
    procedure SetDefaultValue(const Value: T);
    procedure SetDefaultFactory(const Factory: TFunc<T>; const Text: string);
    procedure SetDefaultResultFactory(const Factory: TDefaultValueFactory<T>; const Text: string);
    /// <summary>Leaves it with no default, the false of a boolean included.</summary>
    procedure ClearDefault;
    procedure AddTypedValidator(const Validation: TCommandLineValidator<T>);
    procedure SetCustomParser(const Parser: TCustomParser<T>);
  public
    constructor Create(const AName: string; const AParser: TCommandLineParser<T>);
    function DefaultValueFactory: TDefaultValueFactory<T>; overload;
    function CustomParser: TCustomParser<T>; overload;
    function BoxedDefault(const Found: IArgumentResult): TValue; override;
  end;

  TOption<T> = class(TValuedSymbol<T>, IOption, IOption<T>)
  private
    FRecursive: Boolean;
    FRequiredFlag: Boolean;
    FMultiplePerToken: Boolean;
    FAction: ICommandLineAction;
    FResultValidators: TCommandLineCollection<TOptionValidator>;
    FValueArgument: IArgument;
    function Found(const Where: ISymbolResult): ISymbolResult;
  public
    constructor Create(const AName: string; const AParser: TCommandLineParser<T>);
    destructor Destroy; override;
    procedure Freeze; override;
    function Validators: TCommandLineCollection<TOptionValidator>;
    function ValueValidators: TCommandLineCollection<TArgumentValidator>; override;
    function ValueArgument: IArgument; override;
    function IsRecursive: Boolean;
    function IsRequired: Boolean;
    function AllowsMultipleArgumentsPerToken: Boolean;
    function AllowMultipleArgumentsPerToken: IOption<T>;
    function Action: ICommandLineAction; overload;
    function Action(const Value: ICommandLineAction): IOption<T>; overload;
    function ValueIn(const Parsed: IParseResult): T; overload;
    function ValueIn(const SymbolResult: ISymbolResult): T; overload;
    function RequiredValueIn(const Parsed: IParseResult): T; overload;
    function RequiredValueIn(const SymbolResult: ISymbolResult): T; overload;
    function ValuesIn(const Parsed: IParseResult): TArray<T>;
    function DefaultValue(const Value: T): IOption<T>;
    function DefaultValueFactory(const Factory: TFunc<T>; const Text: string = ''): IOption<T>; overload;
    function DefaultValueFactory(const Factory: TDefaultValueFactory<T>; const Text: string = ''): IOption<T>; overload;
    function CustomParser(const Parser: TCustomParser<T>): IOption<T>; overload;
    function Arity(const Value: TArgumentArity): IOption<T>; overload;
    function Description(const Text: string): IOption<T>; overload;
    function HelpName(const Value: string): IOption<T>; overload;
    function Recursive: IOption<T>;
    function Required: IOption<T>;
    function AcceptOnlyFromAmong(const Values: array of string): IOption<T>; overload;
    function AcceptOnlyFromAmong(const Comparer: IEqualityComparer<string>; const Values: array of string): IOption<T>;
      overload;
    function Validate(const Validation: TCommandLineValidator<T>): IOption<T>;
    function AcceptExistingOnly: IOption<T>;
    function AcceptLegalFileNamesOnly: IOption<T>;
    function AcceptLegalFilePathsOnly: IOption<T>;
    function Hidden: IOption<T>;
  end;

  TArgument<T> = class(TValuedSymbol<T>, IArgument, IArgument<T>)
  private
    FResultValidators: TCommandLineCollection<TArgumentValidator>;
    FCapturesRemainingTokens: Boolean;
    function Found(const Where: ISymbolResult): ISymbolResult;
  public
    constructor Create(const AName: string; const AParser: TCommandLineParser<T>);
    destructor Destroy; override;
    procedure Freeze; override;
    function Validators: TCommandLineCollection<TArgumentValidator>;
    function ValueValidators: TCommandLineCollection<TArgumentValidator>; override;
    function ValueArgument: IArgument; override;
    function CapturesRemainingTokens: Boolean;
    function CaptureRemainingTokens: IArgument<T>;
    function ValueIn(const Parsed: IParseResult): T; overload;
    function ValueIn(const SymbolResult: ISymbolResult): T; overload;
    function RequiredValueIn(const Parsed: IParseResult): T; overload;
    function RequiredValueIn(const SymbolResult: ISymbolResult): T; overload;
    function ValuesIn(const Parsed: IParseResult): TArray<T>;
    function DefaultValue(const Value: T): IArgument<T>;
    function DefaultValueFactory(const Factory: TFunc<T>; const Text: string = ''): IArgument<T>; overload;
    function DefaultValueFactory(const Factory: TDefaultValueFactory<T>; const Text: string = ''): IArgument<T>; overload;
    function CustomParser(const Parser: TCustomParser<T>): IArgument<T>; overload;
    function Arity(const Value: TArgumentArity): IArgument<T>; overload;
    function Description(const Text: string): IArgument<T>; overload;
    function HelpName(const Value: string): IArgument<T>; overload;
    function AcceptOnlyFromAmong(const Values: array of string): IArgument<T>; overload;
    function AcceptOnlyFromAmong(const Comparer: IEqualityComparer<string>; const Values: array of string): IArgument<T>;
      overload;
    function Validate(const Validation: TCommandLineValidator<T>): IArgument<T>;
    function AcceptExistingOnly: IArgument<T>;
    function AcceptLegalFileNamesOnly: IArgument<T>;
    function AcceptLegalFilePathsOnly: IArgument<T>;
    function Hidden: IArgument<T>;
  end;

  /// <summary>Where a grammar starts. The facade, in the shape Daf.Linq uses.</summary>
  /// <remarks>
  ///   A command and a root take their description at declaration, as in .NET's
  ///   <c>new Command("build", "Build things")</c>. An option and an argument take theirs
  ///   through <c>Description</c>, and an option takes its aliases at declaration, as in
  ///   .NET. The fluent <c>Description</c> is the one to avoid on a root, because it answers
  ///   <c>ICommand</c> and a root would stop being an <c>IRootCommand</c>.
  /// </remarks>
  CommandLine = record
  private
    class function GetTexts: TCommandLineTexts; static;
    class procedure SetTexts(const Value: TCommandLineTexts); static;
  public
    class function Option<T>(const Name: string): IOption<T>; overload; static;
    class function Option<T>(const Name: string; const Aliases: array of string): IOption<T>; overload; static;
    /// <summary>Declares an option together with a conversion of one token that reads its type.</summary>
    /// <remarks>
    ///   It converts each token on its own, so a collection refuses it: <c>CustomParser</c> converts the whole
    ///   result instead. It also serves a type the engine already knows, for reading it some other way.
    /// </remarks>
    class function Option<T>(const Name: string; const Parser: TCommandLineParser<T>): IOption<T>; overload; static;
    class function Option<T>(const Name: string; const Parser: TCommandLineParser<T>;
      const Aliases: array of string): IOption<T>; overload; static;
    class function Argument<T>(const Name: string): IArgument<T>; overload; static;
    class function Argument<T>(const Name: string; const Parser: TCommandLineParser<T>): IArgument<T>; overload; static;
    class function Command(const Name: string; const Description: string = ''): ICommand; static;
    /// <summary>A root, with a help option and a version option already among its options.</summary>
    class function Root(const Name: string; const Description: string = ''): IRootCommand; static;
    /// <summary>A help option answering to the names of <c>HelpTokens</c>.</summary>
    class function HelpOption: IHelpOption; overload; static;
    class function HelpOption(const Name: string; const Aliases: array of string): IHelpOption; overload; static;
    /// <summary>A version option answering to <c>VersionToken</c>.</summary>
    class function VersionOption: IVersionOption; overload; static;
    class function VersionOption(const Name: string; const Aliases: array of string): IVersionOption; overload; static;
    /// <summary>
    ///   A directive written <c>[Name]</c>, with no action: what the invocation wrote is read with <c>GetResult</c>.
    /// </summary>
    class function Directive(const Name: string): IDirective; static;
    /// <summary>The <c>[diagram]</c> directive.</summary>
    class function DiagramDirective: IDiagramDirective; static;
    /// <summary>
    ///   Splits a line into tokens as the command line does: white space separates, and double quotes group and are
    ///   not part of the token. What follows a double quote that is never closed is dropped, as in .NET.
    /// </summary>
    class function SplitCommandLine(const Text: string): TArray<string>; static;
    /// <summary>The path of the running executable, <c>ParamStr(0)</c>.</summary>
    class function ExecutablePath: string; static;
    /// <summary>The file name of the running executable, without its extension.</summary>
    class function ExecutableName: string; static;
    /// <summary>
    ///   The texts the application writes instead of the English ones: the parse errors and what the help writes. A
    ///   key it leaves empty is written in English.
    /// </summary>
    /// <remarks>
    ///   <para>
    ///     Assigning checks every text against the arguments its key receives, and raises <c>ECommandLine</c> naming
    ///     the first one that asks for an argument more, so a template that cannot be written fails here rather than
    ///     while reporting an error.
    ///   </para>
    ///   <para>
    ///     It is a setting of the process, assigned at start-up before declaring the grammar: the help and version
    ///     options take their description when they are created. It is not synchronized between threads, and an
    ///     error already reported keeps the text it was written with.
    ///   </para>
    /// </remarks>
    class property Texts: TCommandLineTexts read GetTexts write SetTexts;
  end;

const
  /// <summary>The names the help option of every root answers to: its name, then its aliases.</summary>
  HelpTokens: array [0 .. 4] of string = ('--help', '-h', '/h', '-?', '/?');
  /// <summary>The name the version option of every root answers to.</summary>
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
  System.IOUtils,
  System.Generics.Collections,
  Daf.CommandLine.Help,
  Daf.CommandLine.Parsing,
  Daf.CommandLine.Invocation;

type
  /// <summary>
  ///   The argument an option carries its value in, as .NET's <c>Option.Argument</c>: the result its
  ///   conversion and its default receive is this argument's.
  /// </summary>
  /// <remarks>
  ///   It answers with the option's name, arity and type, and keeps the validations of the value. It
  ///   lives as long as its option, which holds it.
  /// </remarks>
  TOptionArgument = class(TInterfacedObject, ISymbol, IValued, IArgument)
  private
    FOwner: TValuedSymbol;
    FAliases: TCommandLineCollection<string>;
    FValidators: TCommandLineCollection<TArgumentValidator>;
  public
    constructor Create(AOwner: TValuedSymbol);
    destructor Destroy; override;
    function Owner: TValuedSymbol;
    procedure Freeze;
    function AsObject: TObject;
    function Name: string;
    function Aliases: TCommandLineCollection<string>;
    function Description: string;
    function Matches(const Token: string): Boolean;
    function IsHidden: Boolean;
    function Parents: TArray<ISymbol>;
    function TryAccept(const Raw: string; out Error: string): Boolean;
    function IsFlag: Boolean;
    function TypeName: string;
    function ValueType: PTypeInfo;
    function HelpName: string;
    function Arity: TArgumentArity;
    function AcceptedValues: TArray<string>;
    function HasDefaultValue: Boolean;
    function DefaultText: string;
    function GetDefaultValue: TValue;
    function Validators: TCommandLineCollection<TArgumentValidator>;
    function CapturesRemainingTokens: Boolean;
  end;

  TCommand = class(TSymbol, ICommand)
  private
    FOptions: TArray<IOption>;
    FArguments: TArray<IArgument>;
    FSubcommands: TArray<ICommand>;
    FAllowUnmatched: Boolean;
    FAction: ICommandLineAction;
    FValidators: TCommandLineCollection<TCommandValidator>;
    // The path is kept as text, set by the parent when the child is added, so it is there
    // for the help and the errors without walking the parents, which are held weakly.
    FPathPrefix: string;
    procedure AddSymbol(const Symbol: ISymbol);
  public
    constructor Create(const AName: string);
    destructor Destroy; override;
    function Add(const Option: IOption): ICommand; overload;
    function Add(const Argument: IArgument): ICommand; overload;
    function Add(const Subcommand: ICommand): ICommand; overload;
    function Add(const Symbols: array of ISymbol): ICommand; overload;
    function Description(const Text: string): ICommand; overload;
    function Validators: TCommandLineCollection<TCommandValidator>;
    function Options: TArray<IOption>;
    function Arguments: TArray<IArgument>;
    function Subcommands: TArray<ICommand>;
    function Children: TArray<ISymbol>;
    function FullPath: string;
    function TreatsUnmatchedTokensAsErrors: Boolean;
    function AllowUnmatchedTokens: ICommand;
    function Hidden: ICommand;
    function HelpText: string;
    function Action: ICommandLineAction; overload;
    function Action(const Value: ICommandLineAction): ICommand; overload;
    function SetAction(const Value: TCommandAction): ICommand; overload;
    function SetAction(const Value: TCommandActionProc): ICommand; overload;
    function Parse(const Args: TArray<string>): IParseResult; overload;
    function Parse(const Args: TArray<string>; const Configuration: TParserConfiguration): IParseResult; overload;
    function Parse(const CommandLine: string): IParseResult; overload;
    function Parse(const CommandLine: string; const Configuration: TParserConfiguration): IParseResult; overload;
    procedure SetPathPrefix(const Value: string);
    procedure Freeze; override;
  end;

  // Each declares Action with the parameters of the one it inherits and the type its interface chains with: it
  // hides that one, so a call on the object, which the root makes, refuses nil too.
  THelpOption = class(TOption<Boolean>, IHelpOption)
  public
    constructor Create(const AName: string; const AAliases: array of string);
    /// <summary>The one every root has: the names of <c>HelpTokens</c>.</summary>
    constructor CreateDefault;
    function Action(const Value: ICommandLineAction): IHelpOption; overload;
  end;

  TVersionOption = class(TOption<Boolean>, IVersionOption)
  public
    constructor Create(const AName: string; const AAliases: array of string);
    function Action(const Value: ICommandLineAction): IVersionOption; overload;
  end;

  TDirective = class(TSymbol, IDirective)
  protected
    FAction: ICommandLineAction;
  public
    function Action: ICommandLineAction; overload;
    function Action(const Value: ICommandLineAction): IDirective; overload; virtual;
  end;

  TDiagramDirective = class(TDirective, IDiagramDirective)
  private
    FParseErrorReturnValue: Integer;
  public
    constructor Create;
    function Action(const Value: ICommandLineAction): IDirective; overload; override;
    function GetParseErrorReturnValue: Integer;
    procedure SetParseErrorReturnValue(Value: Integer);
  end;

  TRootCommand = class(TCommand, IRootCommand)
  private
    FVersionInfo: TVersionInfo;
    FHelpName: string;
    // Also among its options, which hold them; these are for its two shortcuts to their actions.
    FHelpOption: THelpOption;
    FVersionOption: TVersionOption;
    FDirectives: TArray<IDirective>;
  public
    constructor Create(const AName: string);
    function Directives: TArray<IDirective>;
    function Add(const Directive: IDirective): IRootCommand; overload;
    procedure Freeze; override;
    function GetHelpAction: TCommandAction;
    procedure SetHelpAction(const Value: TCommandAction);
    function GetVersionAction: TCommandAction;
    procedure SetVersionAction(const Value: TCommandAction);
    function Parse: IParseResult; overload;
    function WithVersion(const Version: string): IRootCommand; overload;
    function WithVersion(const Version, AppName, Company: string): IRootCommand; overload;
    function WithVersion(const AClass: TClass): IRootCommand; overload;
    function VersionInfo: TVersionInfo;
    function HelpName: string; overload;
    function HelpName(const Value: string): IRootCommand; overload;
  end;

{ TInvocationConfiguration }

procedure WriteTo(const Writer: TTextWriter; var Console: TextFile; const Text: string);
begin
  if Assigned(Writer) then
    Writer.WriteLine(Text)
  else
    Writeln(Console, Text);
end;

class operator TInvocationConfiguration.Initialize(out Dest: TInvocationConfiguration);
begin
  Dest.Output := nil;
  Dest.Error := nil;
  Dest.EnableDefaultExceptionHandler := True;
end;

procedure TInvocationConfiguration.WriteOutput(const Text: string);
begin
  WriteTo(Output, System.Output, Text);
end;

{ TParserConfiguration }

class operator TParserConfiguration.Initialize(out Dest: TParserConfiguration);
begin
  Dest.EnablePosixBundling := True;
  Dest.EnableCaseInsensitiveNames := False;
  Dest.ResponseFileTokenReplacer := TryReadResponseFile;
end;

{ TSynchronousCommandLineAction }

function TSynchronousCommandLineAction.IsTerminating: Boolean;
begin
  Result := True;
end;

function TSynchronousCommandLineAction.ClearsParseErrors: Boolean;
begin
  Result := False;
end;

{ TArgumentArity }

class function TArgumentArity.Create(AMin, AMax: Integer): TArgumentArity;
begin
  if AMin < 0 then
    raise ECommandLine.Create('An arity minimum cannot be negative');
  if AMax < AMin then
    raise ECommandLine.Create('An arity maximum cannot be below its minimum');
  Result.FMin := AMin;
  Result.FMax := AMax;
end;

class function TArgumentArity.Zero: TArgumentArity;
begin
  Result := TArgumentArity.Create(0, 0);
end;

class function TArgumentArity.ZeroOrOne: TArgumentArity;
begin
  Result := TArgumentArity.Create(0, 1);
end;

class function TArgumentArity.ExactlyOne: TArgumentArity;
begin
  Result := TArgumentArity.Create(1, 1);
end;

class function TArgumentArity.ZeroOrMore: TArgumentArity;
begin
  Result := TArgumentArity.Create(0, Unbounded);
end;

class function TArgumentArity.OneOrMore: TArgumentArity;
begin
  Result := TArgumentArity.Create(1, Unbounded);
end;

function TArgumentArity.MinimumNumberOfValues: Integer;
begin
  Result := FMin;
end;

function TArgumentArity.MaximumNumberOfValues: Integer;
begin
  Result := FMax;
end;

function TArgumentArity.AllowsMany: Boolean;
begin
  Result := FMax > 1;
end;

function TArgumentArity.DemandsValue: Boolean;
begin
  Result := FMin >= 1;
end;

class operator TArgumentArity.Equal(const A, B: TArgumentArity): Boolean;
begin
  Result := (A.FMin = B.FMin) and (A.FMax = B.FMax);
end;

class operator TArgumentArity.NotEqual(const A, B: TArgumentArity): Boolean;
begin
  Result := not (A = B);
end;

{ TToken }

class operator TToken.Equal(const A, B: TToken): Boolean;
begin
  // The symbol by identity, as .NET compares it by reference: two references to one symbol are
  // the same symbol, whichever interface they came through.
  Result := (A.Value = B.Value) and (A.TokenType = B.TokenType) and
    (((A.Symbol = nil) and (B.Symbol = nil)) or
    ((A.Symbol <> nil) and (B.Symbol <> nil) and (A.Symbol.AsObject = B.Symbol.AsObject)));
end;

class operator TToken.NotEqual(const A, B: TToken): Boolean;
begin
  Result := not (A = B);
end;

{ TCommandLineCollection<T>.TEnumerator }

function TCommandLineCollection<T>.TEnumerator.GetCurrent: T;
begin
  Result := FItems[FIndex];
end;

function TCommandLineCollection<T>.TEnumerator.MoveNext: Boolean;
begin
  Inc(FIndex);
  Result := FIndex < Length(FItems);
end;

{ TCommandLineCollection<T> }

constructor TCommandLineCollection<T>.Create(const OwnerName: string);
begin
  inherited Create;
  FOwnerName := OwnerName;
end;

procedure TCommandLineCollection<T>.Add(const Item: T);
begin
  if FFrozen then
    TSymbol.RaiseFrozen(FOwnerName);
  FItems := FItems + [Item];
end;

procedure TCommandLineCollection<T>.Freeze;
begin
  FFrozen := True;
end;

function TCommandLineCollection<T>.Count: Integer;
begin
  Result := Length(FItems);
end;

function TCommandLineCollection<T>.GetItem(Index: Integer): T;
begin
  Result := FItems[Index];
end;

function TCommandLineCollection<T>.ToArray: TArray<T>;
begin
  Result := Copy(FItems);
end;

function TCommandLineCollection<T>.GetEnumerator: TEnumerator;
begin
  Result.FItems := FItems;
  Result.FIndex := -1;
end;

{ TSymbol }

constructor TSymbol.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
  FAliases := TCommandLineCollection<string>.Create(AName);
end;

destructor TSymbol.Destroy;
begin
  FAliases.Free;
  inherited;
end;

function TSymbol.AsObject: TObject;
begin
  Result := Self;
end;

function TSymbol.Name: string;
begin
  Result := FName;
end;

function TSymbol.Aliases: TCommandLineCollection<string>;
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
  if Token = FName then
    Exit(True);
  for Alias in FAliases do
    if Token = Alias then
      Exit(True);
  Result := False;
end;

function TSymbol.IsHidden: Boolean;
begin
  Result := FHidden;
end;

function TSymbol.Parents: TArray<ISymbol>;
var
  Link: TParentLink;
  Parent: ISymbol;
begin
  Result := nil;
  for Link in FParents do
  begin
    Parent := Link.Symbol;
    if Parent <> nil then
      Result := Result + [Parent];
  end;
end;

procedure TSymbol.AddParent(const Parent: ISymbol);
var
  Link: TParentLink;
begin
  Link.Symbol := Parent;
  FParents := FParents + [Link];
end;

procedure TSymbol.SetDescription(const Text: string);
begin
  CheckOpen;
  FDescription := Text;
end;

procedure TSymbol.Hide;
begin
  CheckOpen;
  FHidden := True;
end;

class procedure TSymbol.RaiseFrozen(const SymbolName: string);
begin
  raise ECommandLine.CreateFmt('The grammar is frozen: "%s" can no longer change', [SymbolName]);
end;

procedure TSymbol.CheckOpen;
begin
  if FFrozen then
    RaiseFrozen(Name);
end;

procedure TSymbol.DemandAction(const Value: ICommandLineAction);
begin
  if Value = nil then
    raise ECommandLine.CreateFmt('The action of "%s" is not assigned', [Name]);
end;

procedure TSymbol.Freeze;
begin
  FFrozen := True;
  FAliases.Freeze;
end;

{ TValuedSymbol }

// The symbol whose value an argument carries: the argument itself, or the option of an argument of its own.
function ValuedOf(const Argument: IArgument): TValuedSymbol;
begin
  if Argument.AsObject is TOptionArgument then
    Result := TOptionArgument(Argument.AsObject).Owner
  else
    Result := Argument.AsObject as TValuedSymbol;
end;

// A check of one value, run as a validation of the result: each token is converted and checked, in order,
// and the first refusal is the error. A token that does not convert is left to the conversion, which says so.
// Outside the classes, so the closure captures the check and nothing else.
function ValueValidator(const Check: TUntypedValidator): TArgumentValidator;
begin
  Result :=
    procedure(const Found: IArgumentResult)
    var
      Valued: TValuedSymbol;
      Token: TToken;
      Value: TValue;
      Error: string;
    begin
      Valued := ValuedOf(Found.Argument);
      for Token in Found.Tokens do
      begin
        if not Valued.Convert(Token.Value, '', Value, Error) then
          Continue;
        Error := '';
        if not Check(Value, Error) then
        begin
          if Error = '' then
            Error := FormatText(TCommandLineText.InvalidValue, [Token.Value, Valued.Name]);
          Found.AddError(Error);
          Exit;
        end;
      end;
    end;
end;

// Values as an error lists them: each on a line of its own, between Before and After.
function ValuesOnLines(const Values: TArray<string>; const Before, After: string): string;
var
  Value: string;
begin
  Result := '';
  for Value in Values do
    Result := Result + sLineBreak + Before + Value + After;
end;

// The declared set of accepted values, checked as .NET checks it: a validation of the result that compares
// each token as it was written, before any conversion -- a conversion of its own included. It reads the set
// from the symbol when it runs, so declaring the set again replaces it. Outside the classes, so the closure
// captures nothing.
function AcceptedValuesValidator: TArgumentValidator;
begin
  Result :=
    procedure(const Found: IArgumentResult)
    var
      Valued: TValuedSymbol;
      Token: TToken;
    begin
      Valued := ValuedOf(Found.Argument);
      for Token in Found.Tokens do
        if not Valued.IsAccepted(Token.Value) then
        begin
          Found.AddError(FormatText(TCommandLineText.UnrecognizedArgument,
            [Token.Value, ValuesOnLines(Valued.FAccepted, #9'''', '''')]));
          Exit;
        end;
    end;
end;

// A character as an error names it: one with no glyph is named by its code.
function CharText(Ch: Char): string;
begin
  if Ch < ' ' then
    Result := Format('#%d', [Ord(Ch)])
  else
    Result := Ch;
end;

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
  // Derived from the type, so the ordinary case is never written by hand.
  // A collection says "many" by being one, which is how .NET derives it too.
  if IsCollection then
    FArity := TArgumentArity.ZeroOrMore
  else if IsFlag then
    FArity := TArgumentArity.ZeroOrOne
  else
    FArity := TArgumentArity.ExactlyOne;
end;

procedure TValuedSymbol.DemandConvertible;
begin
  if not Assigned(FParser) and not Assigned(FWholeParser) and not TCommandLineValue.Handles(FTypeInfo) then
    raise ECommandLine.CreateFmt('Symbol "%s" is declared with a type the command line cannot convert: %s. ' +
      'Declare a conversion of its own for it', [Name, TCommandLineValue.TypeName(FTypeInfo)]);
end;

function TValuedSymbol.IsCollection: Boolean;
begin
  Result := TCommandLineValue.IsCollection(FTypeInfo);
end;

function TValuedSymbol.TryAccept(const Raw: string; out Error: string): Boolean;
var
  Parsed: TValue;
begin
  Result := Convert(Raw, '', Parsed, Error);
end;

function TValuedSymbol.IsAccepted(const Raw: string): Boolean;
var
  Candidate: string;
begin
  for Candidate in FAccepted do
    if FAcceptedComparer.Equals(Raw, Candidate) then
      Exit(True);
  Result := False;
end;

function TValuedSymbol.TryReadIn(const Found: ISymbolResult; out Values: TArray<TValue>;
  out FromDefault, Whole: Boolean): Boolean;
begin
  Values := nil;
  FromDefault := False;
  Whole := False;
  Result := Found <> nil;
  if Result then
    Values := ResultValues(Found, FromDefault, Whole);
end;

function TValuedSymbol.TryReadRequiredIn(const Found: ISymbolResult; MissingRaises: Boolean;
  out Values: TArray<TValue>; out FromDefault, Whole: Boolean): Boolean;
begin
  Result := TryReadIn(Found, Values, FromDefault, Whole);
  if not Result and (MissingRaises or DefaultIsNull) then
    raise ECommandLine.CreateFmt(SValueNotProvided, [Name]);
end;

function TValuedSymbol.DefaultIsNull: Boolean;
begin
  // C#'s null, mapped by kind: a Delphi string has no null of its own, but a .NET string does. And a value
  // that may be missing is .NET's Nullable<T>, whose default is null.
  Result := (FTypeInfo.Kind in [tkString, tkLString, tkWString, tkUString, tkClass, tkClassRef, tkInterface,
    tkPointer, tkProcedure]) or (TCommandLineValue.NullableValueTypeOf(FTypeInfo) <> nil);
end;

function TValuedSymbol.Convert(const Raw, CommandName: string; out Value: TValue; out Error: string): Boolean;
begin
  Error := '';
  Value := TValue.Empty;
  if Assigned(FParser) then
  begin
    Result := FParser(Raw, Value, Error);
    if not Result and (Error = '') then
      // A conversion of its own that refuses without saying why leaves the caller with
      // nothing to act on, so the engine supplies the message.
      Error := ConversionError(Raw, CommandName);
    Exit;
  end;
  Result := TCommandLineValue.TryParse(FElementTypeInfo, Raw, Value);
  if not Result then
    Error := ConversionError(Raw, CommandName);
end;

function TValuedSymbol.ConversionError(const Raw, CommandName: string): string;
var
  Allowed: TArray<string>;
  IsOption: Boolean;
begin
  // An enumerated type names its own values, so its refusal lists them, sorted.
  Allowed := TCommandLineValue.AcceptedValuesOf(FTypeInfo);
  TArray.Sort<string>(Allowed, TComparer<string>.Construct(
    function(const A, B: string): Integer
    begin
      Result := CompareText(A, B);
    end));
  IsOption := GetInterfaceEntry(IOption) <> nil;
  if IsOption and (Length(Allowed) > 0) then
    Result := FormatText(TCommandLineText.ArgumentConversionCannotParseForOption_Completions,
      [Raw, Name, TypeName, ValuesOnLines(Allowed, '', '')])
  else if IsOption then
    Result := FormatText(TCommandLineText.ArgumentConversionCannotParseForOption, [Raw, Name, TypeName])
  else if CommandName = '' then
    Result := FormatText(TCommandLineText.ArgumentConversionCannotParse, [Raw, TypeName])
  else if Length(Allowed) > 0 then
    Result := FormatText(TCommandLineText.ArgumentConversionCannotParseForCommand_Completions,
      [Raw, CommandName, TypeName, ValuesOnLines(Allowed, '', '')])
  else
    Result := FormatText(TCommandLineText.ArgumentConversionCannotParseForCommand, [Raw, CommandName, TypeName]);
end;

function TValuedSymbol.IsFlag: Boolean;
begin
  Result := TCommandLineValue.IsFlag(FTypeInfo);
end;

function TValuedSymbol.TypeName: string;
begin
  Result := TCommandLineValue.TypeName(FTypeInfo);
end;

function TValuedSymbol.ValueType: PTypeInfo;
begin
  Result := FTypeInfo;
end;

function TValuedSymbol.HelpName: string;
begin
  Result := FHelpName;
end;

procedure TValuedSymbol.SetHelpName(const Value: string);
begin
  CheckOpen;
  FHelpName := Value;
end;

function TValuedSymbol.Arity: TArgumentArity;
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

function TValuedSymbol.HasDefaultValue: Boolean;
begin
  Result := FHasDefault;
end;

function TValuedSymbol.DefaultText: string;
begin
  Result := FDefaultText;
end;

function TValuedSymbol.GetDefaultValue: TValue;
begin
  if not HasDefaultValue then
    raise ECommandLine.CreateFmt('Argument "%s" does not have a default value', [Name]);
  Result := BoxedDefault(LooseArgumentResult(ValueArgument));
end;

procedure TValuedSymbol.SetArity(const Value: TArgumentArity);
begin
  CheckOpen;
  FArity := Value;
end;

procedure TValuedSymbol.SetAccepted(const Comparer: IEqualityComparer<string>; const Values: array of string);
var
  I: Integer;
begin
  CheckOpen;
  if Comparer = nil then
    raise ECommandLine.CreateFmt('The comparer of the accepted values of "%s" is not assigned', [Name]);
  // The validation is added once, the first time: it reads the set when it runs, so it checks
  // whatever the last declaration left.
  if FAcceptedComparer = nil then
    ValueValidators.Add(AcceptedValuesValidator());
  FAcceptedComparer := Comparer;
  SetLength(FAccepted, Length(Values));
  for I := Low(Values) to High(Values) do
    FAccepted[I - Low(Values)] := Values[I];
end;

procedure TValuedSymbol.SetDefaultText(const Text: string);
begin
  CheckOpen;
  FHasDefault := True;
  FDefaultText := Text;
end;

function TValuedSymbol.WholeParser: TUntypedWholeParser;
begin
  Result := FWholeParser;
end;

function TValuedSymbol.NewValueArgument: IArgument;
begin
  Result := TOptionArgument.Create(Self);
end;

class procedure TValuedSymbol.FreezeValueArgument(const Argument: IArgument);
begin
  (Argument.AsObject as TOptionArgument).Freeze;
end;

procedure TValuedSymbol.AddValidator(const Validator: TUntypedValidator);
begin
  CheckOpen;
  if not Assigned(Validator) then
    // A validation that was declared and is not there would never run and never say so,
    // which is the shape of defect this whole module exists to remove.
    raise ECommandLine.CreateFmt('A validation declared on "%s" is not assigned', [Name]);
  ValueValidators.Add(ValueValidator(Validator));
end;

procedure TValuedSymbol.DemandPathType(const What: string);
begin
  // A path is a string, or one of the two types that say which kind of path it is.
  // Through the element, so a collection of paths validates each one of them.
  if not TCommandLineValue.IsPath(FElementTypeInfo) then
    raise ECommandLine.CreateFmt('%s applies to a path, and "%s" is declared as %s', [What, Name, TypeName]);
end;

procedure TValuedSymbol.DemandExistingPath;
var
  Key: TCommandLineText;
  DirectoryOnly: Boolean;
begin
  CheckOpen;
  DemandPathType('AcceptExistingOnly');
  // A directory path has to be a directory. A file path takes a directory too, as .NET's FileInfo does:
  // its check is the one of a path that may be either.
  DirectoryOnly := FElementTypeInfo = System.TypeInfo(TDirectoryPath);
  if DirectoryOnly then
    Key := TCommandLineText.DirectoryDoesNotExist
  else if FElementTypeInfo = System.TypeInfo(TFilePath) then
    Key := TCommandLineText.FileDoesNotExist
  else
    Key := TCommandLineText.FileOrDirectoryDoesNotExist;
  // Nothing of Self is captured: a closure kept on the object that captures the object is a cycle, and a cycle
  // between counted references is a leak the compiler never mentions.
  AddValidator(
    function(const Value: TValue; out Error: string): Boolean
    begin
      Error := '';
      Result := DirectoryExists(Value.AsString) or (not DirectoryOnly and FileExists(Value.AsString));
      if not Result then
        Error := FormatText(Key, [Value.AsString]);
    end);
end;

// A validation that refuses a path or a name carrying a character of Illegal, naming the first one, and
// an empty one, which is neither -- a declared divergence: .NET takes it. Outside the classes, so the closure
// captures what it is handed.
function LegalCharactersValidator(const Illegal: TArray<Char>; Empty, Refused: TCommandLineText): TUntypedValidator;
begin
  Result :=
    function(const Value: TValue; out Error: string): Boolean
    var
      Raw: string;
      At: Integer;
    begin
      Error := '';
      Raw := Value.AsString;
      if Raw = '' then
      begin
        Error := FormatText(Empty, []);
        Exit(False);
      end;
      At := Raw.IndexOfAny(Illegal);
      Result := At < 0;
      if not Result then
        Error := FormatText(Refused, [CharText(Raw.Chars[At])]);
    end;
end;

procedure TValuedSymbol.DemandLegalFileName;
begin
  CheckOpen;
  DemandPathType('AcceptLegalFileNamesOnly');
  // What the platform refuses in a file NAME, which includes the separators: this validates a name and
  // not a path, the same split .NET makes.
  AddValidator(LegalCharactersValidator(TPath.GetInvalidFileNameChars, TCommandLineText.EmptyFileName,
    TCommandLineText.InvalidCharactersInFileName));
end;

procedure TValuedSymbol.DemandLegalFilePath;
begin
  CheckOpen;
  DemandPathType('AcceptLegalFilePathsOnly');
  AddValidator(LegalCharactersValidator(TPath.GetInvalidPathChars, TCommandLineText.EmptyPath,
    TCommandLineText.InvalidCharactersInPath));
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
  // A boolean has false for its default without declaring it, as in .NET: required, it never fails, and absent it
  // still has a result. No text, because the help shows no default that is false.
  if TypeInfo(T) = TypeInfo(Boolean) then
  begin
    FDefaultFactory := ConstantFactory(Default (T));
    FHasDefault := True;
  end;
end;

procedure TValuedSymbol<T>.ClearDefault;
begin
  FDefaultFactory := nil;
  FHasDefault := False;
  FDefaultText := '';
end;

class function TValuedSymbol<T>.ConstantFactory(const Value: T): TDefaultValueFactory<T>;
begin
  Result :=
    function(const ArgumentResult: IArgumentResult): T
    begin
      Result := Value;
    end;
end;

class function TValuedSymbol<T>.CalculationFactory(const Calculation: TFunc<T>): TDefaultValueFactory<T>;
begin
  Result :=
    function(const ArgumentResult: IArgumentResult): T
    begin
      Result := Calculation();
    end;
end;

function TValuedSymbol<T>.DefaultValueFactory: TDefaultValueFactory<T>;
begin
  Result := FDefaultFactory;
end;

function TValuedSymbol<T>.DefaultOf(const Found: IArgumentResult): T;
begin
  if Assigned(FDefaultFactory) then
    Result := FDefaultFactory(Found)
  else
    Result := Default (T);
end;

function TValuedSymbol<T>.BoxedDefault(const Found: IArgumentResult): TValue;
begin
  Result := TValue.From<T>(DefaultOf(Found));
end;

function TValuedSymbol<T>.Unbox(const Values: TArray<TValue>; Whole: Boolean): T;
begin
  // A default, or what a conversion of its own answered, is held whole, collection or not.
  if Whole then
    Exit(Values[0].AsType<T>);
  // A collection holds ONE value, and that value is every token it was given. The array is built
  // through RTTI because T is the array type and the element type is only known to the engine:
  // TValue.FromArray is what bridges the two.
  if IsCollection then
    Exit(TValue.FromArray(FTypeInfo, Values).AsType<T>);
  if Length(Values) = 0 then
    Exit(Default (T));
  Result := Values[0].AsType<T>;
end;

function TValuedSymbol<T>.ValueOf(const Found: ISymbolResult): T;
var
  Values: TArray<TValue>;
  FromDefault, Whole: Boolean;
begin
  // No result is a command the invocation did not reach, and it reads as the default of the type,
  // the CreateDefaultValue of .NET: nothing in this invocation gave the symbol a value.
  if not TryReadIn(Found, Values, FromDefault, Whole) then
    Exit(Default (T));
  Result := Unbox(Values, Whole);
end;

function TValuedSymbol<T>.RequiredValueOf(const Found: ISymbolResult; MissingRaises: Boolean): T;
var
  Values: TArray<TValue>;
  FromDefault, Whole: Boolean;
begin
  if not TryReadRequiredIn(Found, MissingRaises, Values, FromDefault, Whole) then
    Exit(Default (T));
  Result := Unbox(Values, Whole);
end;

function TValuedSymbol<T>.ValuesOf(const Found: ISymbolResult): TArray<T>;
var
  Values: TArray<TValue>;
  FromDefault, Whole: Boolean;
  I: Integer;
begin
  Result := nil;
  // What the invocation carried, and only that: a default is not a value it carried.
  if not TryReadIn(Found, Values, FromDefault, Whole) or FromDefault then
    Exit;
  // One value of the declared type, and for a collection that one value is the array.
  if IsCollection or Whole then
  begin
    Result := [Unbox(Values, Whole)];
    Exit;
  end;
  SetLength(Result, Length(Values));
  for I := 0 to High(Values) do
    Result[I] := Values[I].AsType<T>;
end;

procedure TValuedSymbol<T>.SetDefaultValue(const Value: T);
begin
  CheckOpen;
  FDefaultFactory := ConstantFactory(Value);
  SetDefaultText(TCommandLineValue.ToText(TValue.From<T>(Value)));
end;

procedure TValuedSymbol<T>.SetDefaultFactory(const Factory: TFunc<T>; const Text: string);
begin
  CheckOpen;
  if not Assigned(Factory) then
    raise ECommandLine.CreateFmt('The computed default of "%s" is not assigned', [Name]);
  FDefaultFactory := CalculationFactory(Factory);
  // HasDefaultValue is what satisfies "required", and it is set even with no text to show:
  // the value exists, it is just not known yet. SetDefaultText('') leaves the help
  // showing no default rather than a false one.
  SetDefaultText(Text);
end;

procedure TValuedSymbol<T>.SetDefaultResultFactory(const Factory: TDefaultValueFactory<T>; const Text: string);
begin
  CheckOpen;
  if not Assigned(Factory) then
    raise ECommandLine.CreateFmt('The computed default of "%s" is not assigned', [Name]);
  FDefaultFactory := Factory;
  SetDefaultText(Text);
end;

function TValuedSymbol<T>.CustomParser: TCustomParser<T>;
begin
  Result := FCustomParser;
end;

procedure TValuedSymbol<T>.SetCustomParser(const Parser: TCustomParser<T>);
begin
  CheckOpen;
  FCustomParser := Parser;
  if not Assigned(Parser) then
    FWholeParser := nil
  else
    // Boxed once, here, as the conversion of one token is: everything below the symbol works on TValue.
    FWholeParser :=
      function(const ArgumentResult: IArgumentResult): TValue
      begin
        Result := TValue.From<T>(Parser(ArgumentResult));
      end;
end;

procedure TValuedSymbol<T>.AddTypedValidator(const Validation: TCommandLineValidator<T>);
begin
  CheckOpen;
  if not Assigned(Validation) then
    raise ECommandLine.CreateFmt('A validation declared on "%s" is not assigned', [Name]);
  if IsCollection then
    // Same reason as the conversion: a validation receives the value of one token, and T
    // here is the array. Refused where it is declared rather than silently never run.
    // The two built-in ones do not have the problem -- they are written over the value the
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

constructor TOption<T>.Create(const AName: string; const AParser: TCommandLineParser<T>);
begin
  inherited Create(AName, AParser);
  FResultValidators := TCommandLineCollection<TOptionValidator>.Create(AName);
  FValueArgument := NewValueArgument;
end;

destructor TOption<T>.Destroy;
begin
  FResultValidators.Free;
  inherited;
end;

procedure TOption<T>.Freeze;
begin
  inherited;
  FResultValidators.Freeze;
  FreezeValueArgument(FValueArgument);
end;

function TOption<T>.Validators: TCommandLineCollection<TOptionValidator>;
begin
  Result := FResultValidators;
end;

function TOption<T>.ValueValidators: TCommandLineCollection<TArgumentValidator>;
begin
  Result := FValueArgument.Validators;
end;

function TOption<T>.ValueArgument: IArgument;
begin
  Result := FValueArgument;
end;

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
  CheckOpen;
  FMultiplePerToken := True;
  Result := Self;
end;

function TOption<T>.Action: ICommandLineAction;
begin
  Result := FAction;
end;

function TOption<T>.Action(const Value: ICommandLineAction): IOption<T>;
begin
  FAction := Value;
  Result := Self;
end;

function TOption<T>.Found(const Where: ISymbolResult): ISymbolResult;
var
  Me: IOption;
begin
  if Where = nil then
    Exit(nil);
  Me := Self;
  Result := Where.GetResult(Me);
end;

function TOption<T>.ValueIn(const Parsed: IParseResult): T;
begin
  if Parsed = nil then
    Result := ValueOf(nil)
  else
    Result := ValueOf(Found(Parsed.RootCommandResult));
end;

function TOption<T>.ValueIn(const SymbolResult: ISymbolResult): T;
begin
  Result := ValueOf(Found(SymbolResult));
end;

function TOption<T>.RequiredValueIn(const Parsed: IParseResult): T;
begin
  if Parsed = nil then
    Result := RequiredValueOf(nil, True)
  else
    Result := RequiredValueOf(Found(Parsed.RootCommandResult), True);
end;

function TOption<T>.RequiredValueIn(const SymbolResult: ISymbolResult): T;
begin
  Result := RequiredValueOf(Found(SymbolResult), True);
end;

function TOption<T>.ValuesIn(const Parsed: IParseResult): TArray<T>;
begin
  if Parsed = nil then
    Result := nil
  else
    Result := ValuesOf(Found(Parsed.RootCommandResult));
end;

function TOption<T>.DefaultValue(const Value: T): IOption<T>;
begin
  SetDefaultValue(Value);
  Result := Self;
end;

function TOption<T>.DefaultValueFactory(const Factory: TFunc<T>; const Text: string): IOption<T>;
begin
  SetDefaultFactory(Factory, Text);
  Result := Self;
end;

function TOption<T>.DefaultValueFactory(const Factory: TDefaultValueFactory<T>; const Text: string): IOption<T>;
begin
  SetDefaultResultFactory(Factory, Text);
  Result := Self;
end;

function TOption<T>.CustomParser(const Parser: TCustomParser<T>): IOption<T>;
begin
  SetCustomParser(Parser);
  Result := Self;
end;

function TOption<T>.Arity(const Value: TArgumentArity): IOption<T>;
begin
  SetArity(Value);
  Result := Self;
end;

function TOption<T>.Description(const Text: string): IOption<T>;
begin
  SetDescription(Text);
  Result := Self;
end;

function TOption<T>.HelpName(const Value: string): IOption<T>;
begin
  SetHelpName(Value);
  Result := Self;
end;

function TOption<T>.Recursive: IOption<T>;
begin
  CheckOpen;
  FRecursive := True;
  Result := Self;
end;

function TOption<T>.Required: IOption<T>;
begin
  CheckOpen;
  FRequiredFlag := True;
  Result := Self;
end;

function TOption<T>.AcceptOnlyFromAmong(const Values: array of string): IOption<T>;
begin
  SetAccepted(TStringComparer.Ordinal, Values);
  Result := Self;
end;

function TOption<T>.AcceptOnlyFromAmong(const Comparer: IEqualityComparer<string>; const Values: array of string)
  : IOption<T>;
begin
  SetAccepted(Comparer, Values);
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

function TOption<T>.AcceptLegalFilePathsOnly: IOption<T>;
begin
  DemandLegalFilePath;
  Result := Self;
end;

function TOption<T>.Hidden: IOption<T>;
begin
  Hide;
  Result := Self;
end;

{ TArgument<T> }

constructor TArgument<T>.Create(const AName: string; const AParser: TCommandLineParser<T>);
begin
  inherited Create(AName, AParser);
  FResultValidators := TCommandLineCollection<TArgumentValidator>.Create(AName);
end;

destructor TArgument<T>.Destroy;
begin
  FResultValidators.Free;
  inherited;
end;

procedure TArgument<T>.Freeze;
begin
  inherited;
  FResultValidators.Freeze;
end;

function TArgument<T>.Validators: TCommandLineCollection<TArgumentValidator>;
begin
  Result := FResultValidators;
end;

function TArgument<T>.ValueValidators: TCommandLineCollection<TArgumentValidator>;
begin
  Result := FResultValidators;
end;

function TArgument<T>.ValueArgument: IArgument;
begin
  Result := Self;
end;

function TArgument<T>.CapturesRemainingTokens: Boolean;
begin
  Result := FCapturesRemainingTokens;
end;

function TArgument<T>.CaptureRemainingTokens: IArgument<T>;
begin
  CheckOpen;
  FCapturesRemainingTokens := True;
  Result := Self;
end;

function TArgument<T>.Found(const Where: ISymbolResult): ISymbolResult;
var
  Me: IArgument;
begin
  if Where = nil then
    Exit(nil);
  Me := Self;
  Result := Where.GetResult(Me);
end;

function TArgument<T>.ValueIn(const Parsed: IParseResult): T;
begin
  if Parsed = nil then
    Result := ValueOf(nil)
  else
    Result := ValueOf(Found(Parsed.RootCommandResult));
end;

function TArgument<T>.ValueIn(const SymbolResult: ISymbolResult): T;
begin
  Result := ValueOf(Found(SymbolResult));
end;

function TArgument<T>.RequiredValueIn(const Parsed: IParseResult): T;
begin
  if Parsed = nil then
    Result := RequiredValueOf(nil, False)
  else
    Result := RequiredValueOf(Found(Parsed.RootCommandResult), False);
end;

function TArgument<T>.RequiredValueIn(const SymbolResult: ISymbolResult): T;
begin
  Result := RequiredValueOf(Found(SymbolResult), False);
end;

function TArgument<T>.ValuesIn(const Parsed: IParseResult): TArray<T>;
begin
  if Parsed = nil then
    Result := nil
  else
    Result := ValuesOf(Found(Parsed.RootCommandResult));
end;

function TArgument<T>.DefaultValue(const Value: T): IArgument<T>;
begin
  SetDefaultValue(Value);
  Result := Self;
end;

function TArgument<T>.DefaultValueFactory(const Factory: TFunc<T>; const Text: string): IArgument<T>;
begin
  SetDefaultFactory(Factory, Text);
  Result := Self;
end;

function TArgument<T>.DefaultValueFactory(const Factory: TDefaultValueFactory<T>; const Text: string): IArgument<T>;
begin
  SetDefaultResultFactory(Factory, Text);
  Result := Self;
end;

function TArgument<T>.CustomParser(const Parser: TCustomParser<T>): IArgument<T>;
begin
  SetCustomParser(Parser);
  Result := Self;
end;

function TArgument<T>.Arity(const Value: TArgumentArity): IArgument<T>;
begin
  SetArity(Value);
  Result := Self;
end;

function TArgument<T>.Description(const Text: string): IArgument<T>;
begin
  SetDescription(Text);
  Result := Self;
end;

function TArgument<T>.HelpName(const Value: string): IArgument<T>;
begin
  SetHelpName(Value);
  Result := Self;
end;

function TArgument<T>.AcceptOnlyFromAmong(const Values: array of string): IArgument<T>;
begin
  SetAccepted(TStringComparer.Ordinal, Values);
  Result := Self;
end;

function TArgument<T>.AcceptOnlyFromAmong(const Comparer: IEqualityComparer<string>; const Values: array of string)
  : IArgument<T>;
begin
  SetAccepted(Comparer, Values);
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

function TArgument<T>.AcceptLegalFilePathsOnly: IArgument<T>;
begin
  DemandLegalFilePath;
  Result := Self;
end;

function TArgument<T>.Hidden: IArgument<T>;
begin
  Hide;
  Result := Self;
end;

{ TOptionArgument }

constructor TOptionArgument.Create(AOwner: TValuedSymbol);
begin
  inherited Create;
  FOwner := AOwner;
  FAliases := TCommandLineCollection<string>.Create(AOwner.Name);
  FValidators := TCommandLineCollection<TArgumentValidator>.Create(AOwner.Name);
end;

destructor TOptionArgument.Destroy;
begin
  FValidators.Free;
  FAliases.Free;
  inherited;
end;

function TOptionArgument.Owner: TValuedSymbol;
begin
  Result := FOwner;
end;

procedure TOptionArgument.Freeze;
begin
  FAliases.Freeze;
  FValidators.Freeze;
end;

function TOptionArgument.AsObject: TObject;
begin
  Result := Self;
end;

function TOptionArgument.Name: string;
begin
  Result := FOwner.Name;
end;

function TOptionArgument.Aliases: TCommandLineCollection<string>;
begin
  Result := FAliases;
end;

function TOptionArgument.Description: string;
begin
  Result := FOwner.Description;
end;

function TOptionArgument.Matches(const Token: string): Boolean;
begin
  // No token names it: it is reached through its option.
  Result := False;
end;

function TOptionArgument.IsHidden: Boolean;
begin
  Result := FOwner.IsHidden;
end;

function TOptionArgument.Parents: TArray<ISymbol>;
begin
  Result := [FOwner as ISymbol];
end;

function TOptionArgument.TryAccept(const Raw: string; out Error: string): Boolean;
begin
  Result := FOwner.TryAccept(Raw, Error);
end;

function TOptionArgument.IsFlag: Boolean;
begin
  Result := FOwner.IsFlag;
end;

function TOptionArgument.TypeName: string;
begin
  Result := FOwner.TypeName;
end;

function TOptionArgument.ValueType: PTypeInfo;
begin
  Result := FOwner.ValueType;
end;

function TOptionArgument.HelpName: string;
begin
  Result := FOwner.HelpName;
end;

function TOptionArgument.Arity: TArgumentArity;
begin
  Result := FOwner.Arity;
end;

function TOptionArgument.AcceptedValues: TArray<string>;
begin
  Result := FOwner.AcceptedValues;
end;

function TOptionArgument.HasDefaultValue: Boolean;
begin
  Result := FOwner.HasDefaultValue;
end;

function TOptionArgument.DefaultText: string;
begin
  Result := FOwner.DefaultText;
end;

function TOptionArgument.GetDefaultValue: TValue;
begin
  Result := FOwner.GetDefaultValue;
end;

function TOptionArgument.Validators: TCommandLineCollection<TArgumentValidator>;
begin
  Result := FValidators;
end;

function TOptionArgument.CapturesRemainingTokens: Boolean;
begin
  Result := False;
end;

{ TCommand }

constructor TCommand.Create(const AName: string);
begin
  inherited Create(AName);
  FValidators := TCommandLineCollection<TCommandValidator>.Create(AName);
end;

destructor TCommand.Destroy;
begin
  FValidators.Free;
  inherited;
end;

function TCommand.Validators: TCommandLineCollection<TCommandValidator>;
begin
  Result := FValidators;
end;

function TCommand.Add(const Option: IOption): ICommand;
begin
  CheckOpen;
  (Option.AsObject as TValuedSymbol).DemandConvertible;
  DemandOptionAddable(Self, Option);
  FOptions := FOptions + [Option];
  (Option.AsObject as TSymbol).AddParent(Self);
  Result := Self;
end;

function TCommand.Add(const Argument: IArgument): ICommand;
begin
  CheckOpen;
  (Argument.AsObject as TValuedSymbol).DemandConvertible;
  FArguments := FArguments + [Argument];
  (Argument.AsObject as TSymbol).AddParent(Self);
  Result := Self;
end;

function TCommand.Add(const Subcommand: ICommand): ICommand;
begin
  CheckOpen;
  DemandSubcommandAddable(Self, Subcommand);
  FSubcommands := FSubcommands + [Subcommand];
  (Subcommand.AsObject as TCommand).SetPathPrefix(FullPath);
  (Subcommand.AsObject as TSymbol).AddParent(Self);
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
  AsDirective: IDirective;
begin
  if Supports(Symbol, IOption, AsOption) then
    Add(AsOption)
  else if Supports(Symbol, IArgument, AsArgument) then
    Add(AsArgument)
  else if Supports(Symbol, ICommand, AsCommand) then
    Add(AsCommand)
  else if Supports(Symbol, IDirective, AsDirective) then
  begin
    if not (Self is TRootCommand) then
      raise ECommandLine.CreateFmt('Directive "%s" can only be added to a root, and "%s" is not one',
        [AsDirective.Name, FullPath]);
    TRootCommand(Self).Add(AsDirective);
  end
  else
    raise ECommandLine.Create('Only options, arguments and subcommands can be added to a command');
end;

function TCommand.Description(const Text: string): ICommand;
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

function TCommand.Children: TArray<ISymbol>;
var
  I, At: Integer;
begin
  SetLength(Result, Length(FSubcommands) + Length(FOptions) + Length(FArguments));
  At := 0;
  for I := 0 to High(FSubcommands) do
  begin
    Result[At] := FSubcommands[I];
    Inc(At);
  end;
  for I := 0 to High(FOptions) do
  begin
    Result[At] := FOptions[I];
    Inc(At);
  end;
  for I := 0 to High(FArguments) do
  begin
    Result[At] := FArguments[I];
    Inc(At);
  end;
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
  CheckOpen;
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

// Outside the class, so the closure captures the procedure and nothing else: a command holding a
// closure that holds the command is a cycle between counted references.
function AsCommandAction(const Action: TCommandActionProc): TCommandAction;
begin
  if not Assigned(Action) then
    Exit(nil);
  Result :=
    function(const Parsed: IParseResult): Integer
    begin
      Action(Parsed);
      Result := TCommandLineExit.Ok;
    end;
end;

function TCommand.Action(const Value: ICommandLineAction): ICommand;
begin
  FAction := Value;
  Result := Self;
end;

function TCommand.SetAction(const Value: TCommandAction): ICommand;
begin
  Result := Action(FunctionAction(Value));
end;

function TCommand.SetAction(const Value: TCommandActionProc): ICommand;
begin
  Result := SetAction(AsCommandAction(Value));
end;

function TCommand.Action: ICommandLineAction;
begin
  Result := FAction;
end;

function TCommand.Parse(const Args: TArray<string>): IParseResult;
var
  Configuration: TParserConfiguration;
begin
  Result := Parse(Args, Configuration);
end;

function TCommand.Parse(const Args: TArray<string>; const Configuration: TParserConfiguration): IParseResult;
begin
  Result := ParseInvocation(Self, Args, Configuration, False);
end;

function TCommand.Parse(const CommandLine: string): IParseResult;
var
  Configuration: TParserConfiguration;
begin
  Result := Parse(CommandLine, Configuration);
end;

function TCommand.Parse(const CommandLine: string; const Configuration: TParserConfiguration): IParseResult;
begin
  Result := ParseInvocation(Self, Daf.CommandLine.Tokens.SplitCommandLine(CommandLine), Configuration, True);
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

procedure TCommand.Freeze;
begin
  inherited;
  FValidators.Freeze;
  for var Option in FOptions do
    (Option.AsObject as TSymbol).Freeze;
  for var Argument in FArguments do
    (Argument.AsObject as TSymbol).Freeze;
  for var Subcommand in FSubcommands do
    (Subcommand.AsObject as TSymbol).Freeze;
end;

{ THelpOption }

constructor THelpOption.Create(const AName: string; const AAliases: array of string);
var
  Alias: string;
begin
  inherited Create(AName, nil);
  for Alias in AAliases do
    Aliases.Add(Alias);
  SetDescription(TextOf(TCommandLineText.HelpOptionDescription));
  SetArity(TArgumentArity.Zero);
  // An option that takes no value rather than a boolean, as in .NET: with the false of a boolean, every parse would
  // give it a result.
  ClearDefault;
  FRecursive := True;
  FAction := NewHelpAction;
end;

constructor THelpOption.CreateDefault;
var
  I: Integer;
begin
  Create(HelpTokens[0], []);
  for I := 1 to High(HelpTokens) do
    Aliases.Add(HelpTokens[I]);
end;

function THelpOption.Action(const Value: ICommandLineAction): IHelpOption;
begin
  DemandAction(Value);
  FAction := Value;
  Result := Self;
end;

{ TVersionOption }

// The validation of every version option: the version is the only thing an invocation asking for it carries.
// A token that matches nothing does not count -- it has no result among the children of the command, and
// its own error is somebody else's. Outside the class, so the option does not hold a closure that holds it.
procedure ValidateVersionAlone(const Result: IOptionResult);
var
  Command: ICommandResult;
  Child: ISymbolResult;
  AsOption: IOptionResult;
  Named: string;
begin
  if not Supports(Result.Parent, ICommandResult, Command) then
    Exit;
  for Child in Command.Children do
  begin
    if Supports(Child, IOptionResult, AsOption) and (Supports(AsOption.Option, IVersionOption) or AsOption.IsImplicit) then
      Continue;
    if Result.IdentifierToken.HasValue then
      Named := Result.IdentifierToken.Value.Value
    else
      Named := Result.Option.Name;
    Result.AddError(FormatText(TCommandLineText.VersionOptionCannotBeCombinedWithOtherArguments, [Named]));
    Exit;
  end;
end;

constructor TVersionOption.Create(const AName: string; const AAliases: array of string);
var
  Alias: string;
begin
  inherited Create(AName, nil);
  for Alias in AAliases do
    Aliases.Add(Alias);
  SetDescription(TextOf(TCommandLineText.VersionOptionDescription));
  SetArity(TArgumentArity.Zero);
  // See THelpOption.Create.
  ClearDefault;
  FAction := NewVersionAction;
  Validators.Add(ValidateVersionAlone);
end;

function TVersionOption.Action(const Value: ICommandLineAction): IVersionOption;
begin
  DemandAction(Value);
  FAction := Value;
  Result := Self;
end;

{ TDirective }

function TDirective.Action: ICommandLineAction;
begin
  Result := FAction;
end;

function TDirective.Action(const Value: ICommandLineAction): IDirective;
begin
  FAction := Value;
  Result := Self;
end;

{ TDiagramDirective }

constructor TDiagramDirective.Create;
begin
  inherited Create('diagram');
  FParseErrorReturnValue := TCommandLineExit.InvocationError;
  FAction := NewDiagramAction(Self);
end;

function TDiagramDirective.Action(const Value: ICommandLineAction): IDirective;
begin
  DemandAction(Value);
  Result := inherited Action(Value);
end;

function TDiagramDirective.GetParseErrorReturnValue: Integer;
begin
  Result := FParseErrorReturnValue;
end;

procedure TDiagramDirective.SetParseErrorReturnValue(Value: Integer);
begin
  FParseErrorReturnValue := Value;
end;

{ TRootCommand }

constructor TRootCommand.Create(const AName: string);
var
  Help: IHelpOption;
  Version: IVersionOption;
begin
  inherited Create(AName);
  // Through interface variables, which count them: a cast of the object would hand Add a reference that
  // nobody counts, and the first that does would free the option when it lets it go.
  FHelpOption := THelpOption.CreateDefault;
  Help := FHelpOption;
  Add(Help);
  FVersionOption := TVersionOption.Create(VersionToken, []);
  Version := FVersionOption;
  Add(Version);
end;

function TRootCommand.WithVersion(const Version: string): IRootCommand;
begin
  Result := WithVersion(Version, '', '');
end;

function TRootCommand.WithVersion(const Version, AppName, Company: string): IRootCommand;
begin
  CheckOpen;
  FVersionInfo := TVersionInfo.Create(Version, AppName, Company);
  Result := Self;
end;

function TRootCommand.WithVersion(const AClass: TClass): IRootCommand;
begin
  CheckOpen;
  FVersionInfo := TVersionInfo.GetFrom(AClass);
  Result := Self;
end;

function TRootCommand.VersionInfo: TVersionInfo;
begin
  Result := FVersionInfo;
end;

function TRootCommand.HelpName: string;
begin
  Result := FHelpName;
end;

function TRootCommand.HelpName(const Value: string): IRootCommand;
begin
  CheckOpen;
  FHelpName := Value;
  Result := Self;
end;

function TRootCommand.Directives: TArray<IDirective>;
begin
  Result := FDirectives;
end;

function TRootCommand.Add(const Directive: IDirective): IRootCommand;
begin
  CheckOpen;
  FDirectives := FDirectives + [Directive];
  (Directive.AsObject as TSymbol).AddParent(Self);
  Result := Self;
end;

procedure TRootCommand.Freeze;
begin
  inherited;
  for var Directive in FDirectives do
    (Directive.AsObject as TSymbol).Freeze;
end;

// Outside the classes, so the closure captures the action it is handed and nothing else: a function
// holding the root, set as the action of one of its options, is a cycle between counted references.
function AsFunction(const Action: ICommandLineAction): TCommandAction;
begin
  if Action = nil then
    Exit(nil);
  Result :=
    function(const Parsed: IParseResult): Integer
    begin
      Result := Action.Invoke(Parsed);
    end;
end;

function TRootCommand.GetHelpAction: TCommandAction;
begin
  Result := AsFunction(FHelpOption.Action);
end;

procedure TRootCommand.SetHelpAction(const Value: TCommandAction);
begin
  // A function in the place of the help settles the invocation as the help does.
  if Assigned(Value) then
    FHelpOption.Action(FunctionAction(Value, True))
  else
    FHelpOption.Action(NewHelpAction);
end;

function TRootCommand.GetVersionAction: TCommandAction;
begin
  Result := AsFunction(FVersionOption.Action);
end;

procedure TRootCommand.SetVersionAction(const Value: TCommandAction);
begin
  if Assigned(Value) then
    FVersionOption.Action(FunctionAction(Value, True))
  else
    FVersionOption.Action(NewVersionAction);
end;

function TRootCommand.Parse: IParseResult;
var
  Args: TArray<string>;
  I: Integer;
begin
  SetLength(Args, ParamCount);
  for I := 1 to ParamCount do
    Args[I - 1] := ParamStr(I);
  Result := Parse(Args);
end;

{ CommandLine }

class function CommandLine.Option<T>(const Name: string): IOption<T>;
begin
  Result := TOption<T>.Create(Name, nil);
end;

class function CommandLine.Option<T>(const Name: string; const Aliases: array of string): IOption<T>;
begin
  Result := Option<T>(Name, nil, Aliases);
end;

class function CommandLine.Option<T>(const Name: string; const Parser: TCommandLineParser<T>): IOption<T>;
begin
  Result := TOption<T>.Create(Name, Parser);
end;

class function CommandLine.Option<T>(const Name: string; const Parser: TCommandLineParser<T>;
  const Aliases: array of string): IOption<T>;
var
  Alias: string;
begin
  Result := TOption<T>.Create(Name, Parser);
  for Alias in Aliases do
    Result.Aliases.Add(Alias);
end;

class function CommandLine.Argument<T>(const Name: string): IArgument<T>;
begin
  Result := TArgument<T>.Create(Name, nil);
end;

class function CommandLine.Argument<T>(const Name: string; const Parser: TCommandLineParser<T>): IArgument<T>;
begin
  Result := TArgument<T>.Create(Name, Parser);
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

class function CommandLine.HelpOption: IHelpOption;
begin
  Result := THelpOption.CreateDefault;
end;

class function CommandLine.HelpOption(const Name: string; const Aliases: array of string): IHelpOption;
begin
  Result := THelpOption.Create(Name, Aliases);
end;

class function CommandLine.VersionOption: IVersionOption;
begin
  Result := TVersionOption.Create(VersionToken, []);
end;

class function CommandLine.VersionOption(const Name: string; const Aliases: array of string): IVersionOption;
begin
  Result := TVersionOption.Create(Name, Aliases);
end;

class function CommandLine.Directive(const Name: string): IDirective;
begin
  Result := TDirective.Create(Name);
end;

class function CommandLine.DiagramDirective: IDiagramDirective;
begin
  Result := TDiagramDirective.Create;
end;

class function CommandLine.SplitCommandLine(const Text: string): TArray<string>;
begin
  Result := Daf.CommandLine.Tokens.SplitCommandLine(Text);
end;

class function CommandLine.ExecutablePath: string;
begin
  Result := ParamStr(0);
end;

class function CommandLine.ExecutableName: string;
begin
  Result := TPath.GetFileNameWithoutExtension(ParamStr(0));
end;

class function CommandLine.GetTexts: TCommandLineTexts;
begin
  Result := TextsInForce;
end;

class procedure CommandLine.SetTexts(const Value: TCommandLineTexts);
var
  Key: TCommandLineText;
  Reason: string;
begin
  if not TryValidateTexts(Value, Key, Reason) then
    raise ECommandLine.CreateFmt('The text for %s is not a template of the %d string argument(s) it receives: %s',
      [TextKeyName(Key), TextArgumentCount(Key), Reason]);
  PutTextsInForce(Value);
end;

end.
