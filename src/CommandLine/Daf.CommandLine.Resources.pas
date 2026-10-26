unit Daf.CommandLine.Resources;

{
  The texts the command line writes to whoever invokes the tool -- every parse error, the headings and labels of the
  help, the descriptions of the help and version options, and what the invocation writes around an error -- and the
  dictionary with which an application writes its own instead.

  It lives below Daf.CommandLine because the tokenizer writes texts too. Daf.CommandLine republishes the types.
}

interface

type
  {$SCOPEDENUMS ON}
  /// <summary>
  ///   The key of each text the command line writes, named as .NET names it in the resources of System.CommandLine,
  ///   so its translation files serve as a reference to whoever translates.
  /// </summary>
  TCommandLineText = (
    ArgumentConversionCannotParse,
    ArgumentConversionCannotParseForCommand,
    ArgumentConversionCannotParseForCommand_Completions,
    ArgumentConversionCannotParseForOption,
    ArgumentConversionCannotParseForOption_Completions,
    CommandRequiredArgumentMissing,
    DirectoryDoesNotExist,
    ErrorReadingResponseFile,
    ExceptionHandlerHeader,
    FileDoesNotExist,
    FileOrDirectoryDoesNotExist,
    HelpAdditionalArgumentsDescription,
    HelpAdditionalArgumentsTitle,
    HelpArgumentDefaultValueLabel,
    HelpArgumentsTitle,
    HelpCommandsTitle,
    HelpDescriptionTitle,
    HelpOptionDescription,
    HelpOptionsRequiredLabel,
    HelpOptionsTitle,
    HelpUsageAdditionalArguments,
    HelpUsageCommand,
    HelpUsageOptions,
    HelpUsageTitle,
    InvalidCharactersInFileName,
    InvalidCharactersInPath,
    OptionExpectsOneArgument,
    OptionRequiredArgumentMissing,
    RequiredCommandWasNotProvided,
    RequiredOptionWasNotProvided,
    ResponseFileNotFound,
    SuggestionsTokenNotMatched,
    UnrecognizedArgument,
    UnrecognizedCommandOrArgument,
    VersionOptionCannotBeCombinedWithOtherArguments,
    VersionOptionDescription,
    // DAF's own: what DAF refuses and System.CommandLine does not.
    UnrecognizedOptionInBundle,
    ResponseFileNestedTooDeep,
    InvalidValue,
    EmptyFileName,
    EmptyPath);
  {$SCOPEDENUMS OFF}

  /// <summary>The texts an application writes instead of the English ones. A key left empty is written in English.</summary>
  /// <remarks>
  ///   A text that receives arguments is a template of <c>Format</c> whose arguments are all strings, named by their
  ///   index -- <c>%0:s</c>, <c>%1:s</c> -- so a language that puts them in another order can. A text that receives
  ///   none is written as it is.
  /// </remarks>
  TCommandLineTexts = record
  private
    FItems: array [TCommandLineText] of string;
    function GetItem(Key: TCommandLineText): string;
    procedure SetItem(Key: TCommandLineText; const Value: string);
  public
    property Items[Key: TCommandLineText]: string read GetItem write SetItem; default;
    /// <summary>The keys left empty, which are written in English, in the order they are declared.</summary>
    function Missing: TArray<TCommandLineText>;
  end;

/// <summary>The English text of a key.</summary>
function EnglishText(Key: TCommandLineText): string;
/// <summary>How many arguments the text of a key receives.</summary>
function TextArgumentCount(Key: TCommandLineText): Integer;
/// <summary>The name of a key, as the enumeration declares it.</summary>
function TextKeyName(Key: TCommandLineText): string;

/// <summary>The text of a key in force: the application's, or the English one.</summary>
function TextOf(Key: TCommandLineText): string;
/// <summary>The text of a key in force, with its arguments.</summary>
function FormatText(Key: TCommandLineText; const Args: array of string): string;

/// <summary>The texts the application put in force.</summary>
function TextsInForce: TCommandLineTexts;
/// <summary>
///   False naming the first key whose text is not a template of the arguments the key receives: one that asks for an
///   argument more, or one <c>Format</c> rejects.
/// </summary>
function TryValidateTexts(const Texts: TCommandLineTexts; out Key: TCommandLineText; out Reason: string): Boolean;
/// <summary>Puts the texts in force as they come: checking them is <c>TryValidateTexts</c>.</summary>
procedure PutTextsInForce(const Texts: TCommandLineTexts);

resourcestring
  // What reading a value nobody provided raises. It speaks to whoever writes the program, as the declaration
  // errors do, so it is no text of the dictionary.
  SValueNotProvided = '%s is required but was not provided';

implementation

uses
  System.SysUtils,
  System.TypInfo;

type
  TEnglishText = record
    Text: string;
    Arguments: Integer;
  end;

var
  // Fixed at start-up and not synchronized between threads.
  InForce: TCommandLineTexts;

function English(const Text: string; Arguments: Integer): TEnglishText;
begin
  Result.Text := Text;
  Result.Arguments := Arguments;
end;

function EnglishOf(Key: TCommandLineText): TEnglishText;
begin
  case Key of
    TCommandLineText.ArgumentConversionCannotParse:
      Result := English('Cannot parse argument ''%0:s'' as expected type ''%1:s''.', 2);
    TCommandLineText.ArgumentConversionCannotParseForCommand:
      Result := English('Cannot parse argument ''%0:s'' for command ''%1:s'' as expected type ''%2:s''.', 3);
    TCommandLineText.ArgumentConversionCannotParseForCommand_Completions:
      Result := English('Cannot parse argument ''%0:s'' for command ''%1:s'' as expected type ''%2:s''. ' +
        'Did you mean one of the following?%3:s', 4);
    TCommandLineText.ArgumentConversionCannotParseForOption:
      Result := English('Cannot parse argument ''%0:s'' for option ''%1:s'' as expected type ''%2:s''.', 3);
    TCommandLineText.ArgumentConversionCannotParseForOption_Completions:
      Result := English('Cannot parse argument ''%0:s'' for option ''%1:s'' as expected type ''%2:s''. ' +
        'Did you mean one of the following?%3:s', 4);
    TCommandLineText.CommandRequiredArgumentMissing:
      Result := English('Required argument ''%0:s'' missing for command: ''%1:s''.', 2);
    TCommandLineText.DirectoryDoesNotExist:
      Result := English('Directory does not exist: ''%0:s''.', 1);
    TCommandLineText.ErrorReadingResponseFile:
      Result := English('Error reading response file ''%0:s'': %1:s', 2);
    TCommandLineText.ExceptionHandlerHeader:
      Result := English('Unhandled exception: ', 0);
    TCommandLineText.FileDoesNotExist:
      Result := English('File does not exist: ''%0:s''.', 1);
    TCommandLineText.FileOrDirectoryDoesNotExist:
      Result := English('File or directory does not exist: ''%0:s''.', 1);
    TCommandLineText.HelpAdditionalArgumentsDescription:
      Result := English('Arguments passed to the application that is being run.', 0);
    TCommandLineText.HelpAdditionalArgumentsTitle:
      Result := English('Additional Arguments:', 0);
    TCommandLineText.HelpArgumentDefaultValueLabel:
      Result := English('default', 0);
    TCommandLineText.HelpArgumentsTitle:
      Result := English('Arguments:', 0);
    TCommandLineText.HelpCommandsTitle:
      Result := English('Commands:', 0);
    TCommandLineText.HelpDescriptionTitle:
      Result := English('Description:', 0);
    TCommandLineText.HelpOptionDescription:
      Result := English('Show help and usage information', 0);
    TCommandLineText.HelpOptionsRequiredLabel:
      Result := English('(REQUIRED)', 0);
    TCommandLineText.HelpOptionsTitle:
      Result := English('Options:', 0);
    // A declared divergence: .NET's text closes one bracket more than it opens.
    TCommandLineText.HelpUsageAdditionalArguments:
      Result := English('[[--] <additional arguments>...]', 0);
    TCommandLineText.HelpUsageCommand:
      Result := English('[command]', 0);
    TCommandLineText.HelpUsageOptions:
      Result := English('[options]', 0);
    TCommandLineText.HelpUsageTitle:
      Result := English('Usage:', 0);
    TCommandLineText.InvalidCharactersInFileName:
      Result := English('Character not allowed in a file name: ''%0:s''.', 1);
    TCommandLineText.InvalidCharactersInPath:
      Result := English('Character not allowed in a path: ''%0:s''.', 1);
    TCommandLineText.OptionExpectsOneArgument:
      Result := English('Option ''%0:s'' expects a single argument but %1:s were provided.', 2);
    TCommandLineText.OptionRequiredArgumentMissing:
      Result := English('Required argument missing for option: ''%0:s''.', 1);
    TCommandLineText.RequiredCommandWasNotProvided:
      Result := English('Required command was not provided.', 0);
    TCommandLineText.RequiredOptionWasNotProvided:
      Result := English('Option ''%0:s'' is required.', 1);
    TCommandLineText.ResponseFileNotFound:
      Result := English('Response file not found ''%0:s''.', 1);
    TCommandLineText.SuggestionsTokenNotMatched:
      Result := English('''%0:s'' was not matched. Did you mean one of the following?', 1);
    TCommandLineText.UnrecognizedArgument:
      Result := English('Argument ''%0:s'' not recognized. Must be one of:%1:s', 2);
    TCommandLineText.UnrecognizedCommandOrArgument:
      Result := English('Unrecognized command or argument ''%0:s''.', 1);
    TCommandLineText.VersionOptionCannotBeCombinedWithOtherArguments:
      Result := English('%0:s option cannot be combined with other arguments.', 1);
    TCommandLineText.VersionOptionDescription:
      Result := English('Show version information', 0);
    TCommandLineText.UnrecognizedOptionInBundle:
      Result := English('Unrecognized option ''%0:s'' in bundle ''%1:s''.', 2);
    TCommandLineText.ResponseFileNestedTooDeep:
      Result := English('Response file ''%0:s'' is nested more than %1:s levels deep.', 2);
    TCommandLineText.InvalidValue:
      Result := English('Argument ''%0:s'' is not valid for ''%1:s''.', 2);
    TCommandLineText.EmptyFileName:
      Result := English('A file name cannot be empty.', 0);
    TCommandLineText.EmptyPath:
      Result := English('A path cannot be empty.', 0);
  else
    Result := English('', 0);
  end;
end;

function EnglishText(Key: TCommandLineText): string;
begin
  Result := EnglishOf(Key).Text;
end;

function TextArgumentCount(Key: TCommandLineText): Integer;
begin
  Result := EnglishOf(Key).Arguments;
end;

function TextKeyName(Key: TCommandLineText): string;
begin
  Result := GetEnumName(TypeInfo(TCommandLineText), Ord(Key));
end;

function FormatStrings(const Template: string; const Args: array of string): string;
var
  Items: TArray<TVarRec>;
  I: Integer;
begin
  SetLength(Items, Length(Args));
  for I := 0 to High(Args) do
  begin
    Items[I].VType := vtUnicodeString;
    Items[I].VUnicodeString := Pointer(Args[I]);
  end;
  Result := Format(Template, Items);
end;

function TextOf(Key: TCommandLineText): string;
begin
  Result := InForce[Key];
  if Result = '' then
    Result := EnglishText(Key);
end;

function FormatText(Key: TCommandLineText; const Args: array of string): string;
begin
  if TextArgumentCount(Key) = 0 then
    Exit(TextOf(Key));
  Result := FormatStrings(TextOf(Key), Args);
end;

function TextsInForce: TCommandLineTexts;
begin
  Result := InForce;
end;

function TryValidateTexts(const Texts: TCommandLineTexts; out Key: TCommandLineText; out Reason: string): Boolean;
var
  Each: TCommandLineText;
  Args: TArray<string>;
  I: Integer;
begin
  Key := Low(TCommandLineText);
  Reason := '';
  for Each := Low(TCommandLineText) to High(TCommandLineText) do
  begin
    if (Texts[Each] = '') or (TextArgumentCount(Each) = 0) then
      Continue;
    SetLength(Args, TextArgumentCount(Each));
    for I := 0 to High(Args) do
      Args[I] := '';
    try
      FormatStrings(Texts[Each], Args);
    except
      on E: EConvertError do
      begin
        Key := Each;
        Reason := E.Message;
        Exit(False);
      end;
    end;
  end;
  Result := True;
end;

procedure PutTextsInForce(const Texts: TCommandLineTexts);
begin
  InForce := Texts;
end;

{ TCommandLineTexts }

function TCommandLineTexts.GetItem(Key: TCommandLineText): string;
begin
  Result := FItems[Key];
end;

procedure TCommandLineTexts.SetItem(Key: TCommandLineText; const Value: string);
begin
  FItems[Key] := Value;
end;

function TCommandLineTexts.Missing: TArray<TCommandLineText>;
var
  Key: TCommandLineText;
begin
  Result := nil;
  for Key := Low(TCommandLineText) to High(TCommandLineText) do
    if FItems[Key] = '' then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := Key;
    end;
end;

end.
