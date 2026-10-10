unit Daf.CommandLine.Help;

{
  The help renderer.

  It walks the declared grammar and formats it, every time it is asked. No composed
  text is stored anywhere, and that is the whole point: help that is kept
  as text is documentation NEXT TO the code instead of derived FROM it, so it goes
  stale the moment somebody adds an option and forgets it -- and the mismatch breaks
  nothing, it only lies to whoever asked.

  The layout is the one of System.CommandLine's HelpBuilder: the same sections in the same order,
  columns measured section by section, and lines wrapped only at a width somebody set.
}

interface

uses
  Daf.CommandLine;

type
  TCommandLineHelp = record
  public
    /// <summary>Renders the help of one command from the grammar hanging off it.</summary>
    /// <remarks>
    ///   Its lines are wrapped at <c>MaxWidth</c>, and the default wraps none. A hidden command renders an
    ///   empty text.
    /// </remarks>
    class function Render(const Command: ICommand; MaxWidth: Integer = MaxInt): string; static;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.Character,
  System.Math,
  System.StrUtils,
  Daf.CommandLine.Resources;

const
  Indent = '  ';

type
  /// <summary>One entry of a section in two columns: how the symbol is written, and what it is.</summary>
  TRow = record
    First: string;
    Second: string;
  end;

  /// <summary>A name as the help sorts it: its prefix of dashes or slash, and the rest.</summary>
  TSplitName = record
    Prefix: string;
    Alias: string;
  end;

function IsBlank(const Text: string): Boolean;
var
  C: Char;
begin
  for C in Text do
    if not C.IsWhiteSpace then
      Exit(False);
  Result := True;
end;

// The lines of a text, split at CR LF or at LF.
function SplitLines(const Text: string): TArray<string>;
var
  Rest: string;
  At: Integer;
begin
  Result := nil;
  Rest := Text.Replace(#13#10, #10);
  At := Rest.IndexOf(#10);
  while At >= 0 do
  begin
    Result := Result + [Rest.Substring(0, At)];
    Rest := Rest.Substring(At + 1);
    At := Rest.IndexOf(#10);
  end;
  Result := Result + [Rest];
end;

// Cuts each line of a text into pieces of at most MaxWidth, after the last white space that fits, which
// stays at the end of its piece. A word longer than the width is cut where the width ends.
function WrapText(const Text: string; MaxWidth: Integer): TArray<string>;
var
  Part: string;
  At, Taken, I: Integer;
begin
  Result := nil;
  if IsBlank(Text) then
    Exit;
  // Below one, a piece would take nothing and the cut would never advance.
  MaxWidth := Max(MaxWidth, 1);
  for Part in SplitLines(Text) do
    if Part.Length <= MaxWidth then
      Result := Result + [Part]
    else
    begin
      At := 0;
      while At < Part.Length do
      begin
        if Part.Length - At < MaxWidth then
        begin
          Result := Result + [Part.Substring(At)];
          Break;
        end;
        Taken := -1;
        I := 0;
        while (At + I < Part.Length) and (I < MaxWidth) do
        begin
          if Part.Chars[At + I].IsWhiteSpace then
            Taken := I + 1;
          Inc(I);
        end;
        if Taken = -1 then
          Taken := MaxWidth;
        Result := Result + [Part.Substring(At, Taken)];
        Inc(At, Taken);
      end;
    end;
end;

procedure WriteHeading(Lines: TStrings; const Heading, Description: string; MaxWidth: Integer);
var
  Part: string;
begin
  if not IsBlank(Heading) then
    Lines.Add(Heading);
  if not IsBlank(Description) then
    for Part in WrapText(Description, MaxWidth - Length(Indent)) do
      Lines.Add(Indent + Part);
end;

// The first column is as wide as its longest entry. When the two do not fit in MaxWidth, the first takes
// at most half of it and the second what is left, and each entry is wrapped within its own column.
procedure WriteColumns(Lines: TStrings; const Rows: TArray<TRow>; MaxWidth: Integer);
var
  Row: TRow;
  FirstWidth, SecondWidth, FirstMaxWidth, I: Integer;
  Firsts, Seconds: TArray<string>;
  First, Second, Part, Line: string;
begin
  FirstWidth := 0;
  SecondWidth := 0;
  for Row in Rows do
  begin
    FirstWidth := Max(FirstWidth, Row.First.Length);
    SecondWidth := Max(SecondWidth, Row.Second.Length);
  end;
  if FirstWidth + SecondWidth + 2 * Length(Indent) > MaxWidth then
  begin
    FirstMaxWidth := MaxWidth div 2 - Length(Indent);
    if FirstWidth > FirstMaxWidth then
    begin
      FirstWidth := 0;
      for Row in Rows do
        for Part in WrapText(Row.First, FirstMaxWidth) do
          FirstWidth := Max(FirstWidth, Part.Length);
    end;
    SecondWidth := MaxWidth - FirstWidth - 2 * Length(Indent);
  end;
  for Row in Rows do
  begin
    Firsts := WrapText(Row.First, FirstWidth);
    Seconds := WrapText(Row.Second, SecondWidth);
    for I := 0 to Max(Length(Firsts), Length(Seconds)) - 1 do
    begin
      First := '';
      if I < Length(Firsts) then
        First := Firsts[I];
      Second := '';
      if I < Length(Seconds) then
        Second := Seconds[I];
      Line := Indent + First;
      if not IsBlank(Second) then
        Line := Line + StringOfChar(' ', Max(FirstWidth - First.Length, 0)) + Indent + Second;
      Lines.Add(Line);
    end;
  end;
end;

{ Names }

function SplitName(const Name: string): TSplitName;
begin
  if Name.StartsWith('/') then
    Result.Prefix := '/'
  else if Name.StartsWith('--') then
    Result.Prefix := '--'
  else if Name.StartsWith('-') then
    Result.Prefix := '-'
  else
    Result.Prefix := '';
  Result.Alias := Name.Substring(Result.Prefix.Length);
end;

function CompareSplitNames(const A, B: TSplitName): Integer;
begin
  Result := CompareText(A.Prefix, B.Prefix);
  if Result = 0 then
    Result := CompareText(A.Alias, B.Alias);
end;

// The names of a symbol, shortest prefix first and then by the rest, without regard to case. A rest that
// two prefixes share is listed once, with the first: -h and /h are listed as -h.
function NamesLabel(const Names: TArray<string>): string;
var
  Split: TArray<TSplitName>;
  Item: TSplitName;
  Listed: TArray<string>;
  I, J: Integer;
begin
  SetLength(Split, Length(Names));
  for I := 0 to High(Names) do
    Split[I] := SplitName(Names[I]);
  // An insertion sort, because two names that compare equal keep the order they were declared in.
  for I := 1 to High(Split) do
  begin
    Item := Split[I];
    J := I - 1;
    while (J >= 0) and (CompareSplitNames(Split[J], Item) > 0) do
    begin
      Split[J + 1] := Split[J];
      Dec(J);
    end;
    Split[J + 1] := Item;
  end;
  Result := '';
  Listed := nil;
  for Item in Split do
    if not MatchStr(Item.Alias, Listed) then
    begin
      Listed := Listed + [Item.Alias];
      if Result <> '' then
        Result := Result + ', ';
      Result := Result + Item.Prefix + Item.Alias;
    end;
end;

function NamesOf(const Symbol: ISymbol): TArray<string>;
var
  Alias: string;
begin
  Result := [Symbol.Name];
  for Alias in Symbol.Aliases do
    Result := Result + [Alias];
end;

{ Values }

// The values a symbol accepts as the help lists them: each once, sorted without regard to case.
function SortedAcceptedValues(const Symbol: IValued): TArray<string>;
var
  Value, Item: string;
  I, J: Integer;
begin
  Result := nil;
  for Value in Symbol.AcceptedValues do
    if not MatchStr(Value, Result) then
      Result := Result + [Value];
  for I := 1 to High(Result) do
  begin
    Item := Result[I];
    J := I - 1;
    while (J >= 0) and (CompareText(Result[J], Item) > 0) do
    begin
      Result[J + 1] := Result[J];
      Dec(J);
    end;
    Result[J + 1] := Item;
  end;
end;

// How the value of a symbol is written: its help name, or the values it accepts, or its name. An option
// that takes no value shows none; an argument always shows one.
function ValueLabel(const Symbol: IValued; IsOption: Boolean): string;
var
  Accepted: TArray<string>;
begin
  if not IsBlank(Symbol.HelpName) then
    Exit('<' + Symbol.HelpName + '>');
  if not Symbol.IsFlag and (Symbol.Arity.MaximumNumberOfValues > 0) then
  begin
    Accepted := SortedAcceptedValues(Symbol);
    if Length(Accepted) > 0 then
      Exit('<' + string.Join('|', Accepted) + '>');
    if IsOption then
      Exit('<' + Symbol.Name.TrimLeft(['-', '/']) + '>');
  end;
  if IsOption then
    Result := ''
  else
    Result := '<' + Symbol.Name + '>';
end;

// The default as the help shows it. A boolean shows it only when it is true.
function DefaultShown(const Symbol: IValued): string;
begin
  if not Symbol.HasDefaultValue then
    Exit('');
  Result := Symbol.DefaultText;
  if Symbol.IsFlag then
    if SameText(Result, 'True') then
      Result := 'true'
    else if SameText(Result, 'False') then
      Result := '';
end;

// A default makes an argument optional, which its arity alone does not say.
function IsOptional(const Argument: IArgument): Boolean;
begin
  Result := (Argument.Arity.MinimumNumberOfValues = 0) or Argument.HasDefaultValue;
end;

{ Rows }

function OptionRow(const Option: IOption): TRow;
var
  Value, Shown: string;
begin
  Result.First := NamesLabel(NamesOf(Option));
  Value := ValueLabel(Option, True);
  if not IsBlank(Value) then
    Result.First := Result.First + ' ' + Value;
  if Option.IsRequired then
    Result.First := Result.First + ' ' + TextOf(TCommandLineText.HelpOptionsRequiredLabel);
  Shown := DefaultShown(Option);
  if IsBlank(Shown) then
    Shown := ''
  else
    Shown := '[' + TextOf(TCommandLineText.HelpArgumentDefaultValueLabel) + ': ' + Shown + ']';
  Result.Second := Trim(Option.Description + ' ' + Shown);
end;

function ArgumentRow(const Argument: IArgument): TRow;
var
  Shown: string;
begin
  Result.First := ValueLabel(Argument, False);
  Shown := DefaultShown(Argument);
  if IsBlank(Shown) then
    Shown := ''
  else
    Shown := '[' + TextOf(TCommandLineText.HelpArgumentDefaultValueLabel) + ': ' + Shown + ']';
  Result.Second := Trim(Argument.Description + ' ' + Shown);
end;

// A subcommand is written with its arguments, and the defaults of those arguments follow its
// description: as "default" when only one has a default, by the name of each when several do.
function CommandRow(const Command: ICommand): TRow;
var
  Argument: IArgument;
  Defaulted: TArray<IArgument>;
  Shown, Listed, Named: string;
begin
  Result.First := NamesLabel(NamesOf(Command));
  Defaulted := nil;
  for Argument in Command.Arguments do
    if not Argument.IsHidden then
    begin
      Result.First := Result.First + ' ' + ValueLabel(Argument, False);
      if Argument.HasDefaultValue then
        Defaulted := Defaulted + [Argument];
    end;
  Listed := '';
  for Argument in Defaulted do
  begin
    Shown := DefaultShown(Argument);
    if IsBlank(Shown) then
      Continue;
    if Length(Defaulted) = 1 then
      Named := TextOf(TCommandLineText.HelpArgumentDefaultValueLabel)
    else
      Named := Argument.Name;
    if Listed <> '' then
      Listed := Listed + ', ';
    Listed := Listed + Named + ': ' + Shown;
  end;
  if Listed <> '' then
    Listed := '[' + Listed + ']';
  Result.Second := Trim(Command.Description + ' ' + Listed);
end;

{ The command and its ancestors }

function ParentCommand(const Command: ICommand): ICommand;
var
  Parent: ISymbol;
begin
  for Parent in Command.Parents do
    if Supports(Parent, ICommand, Result) then
      Exit;
  Result := nil;
end;

// The command and the commands above it, the root first. A command added to several follows the first.
function Lineage(const Command: ICommand): TArray<ICommand>;
var
  Current: ICommand;
begin
  Result := nil;
  Current := Command;
  while Current <> nil do
  begin
    Result := [Current] + Result;
    Current := ParentCommand(Current);
  end;
end;

// The help and the version options a root brings with it.
function IsStandardOption(const Option: IOption): Boolean;
begin
  Result := Supports(Option, IHelpOption) or Supports(Option, IVersionOption);
end;

{ Sections }

// Optional arguments open a bracket each, and all of them close at the end: [<b> <c>...].
function ArgumentsUsage(const Arguments: TArray<IArgument>): string;
var
  Argument: IArgument;
  Many, Closing: string;
begin
  Result := '';
  Closing := '';
  for Argument in Arguments do
  begin
    if Argument.IsHidden then
      Continue;
    if Argument.Arity.MaximumNumberOfValues > 1 then
      Many := '...'
    else
      Many := '';
    if IsOptional(Argument) then
    begin
      Result := Result + '[<' + Argument.Name + '>' + Many + ' ';
      Closing := Closing + ']';
    end
    else
      Result := Result + '<' + Argument.Name + '>' + Many + ' ';
  end;
  if Result <> '' then
    Result := Result.Substring(0, Result.Length - 1) + Closing;
end;

function UsageOf(const Chain: TArray<ICommand>): string;
var
  Parts: TArray<string>;
  Ancestor, Command, Subcommand: ICommand;
  Option: IOption;
  Root: IRootCommand;
  ShowsOptions, ShowsCommand: Boolean;
  Part: string;
begin
  Parts := nil;
  ShowsOptions := False;
  for Ancestor in Chain do
  begin
    for Option in Ancestor.Options do
      if Option.IsRecursive and not Option.IsHidden then
        ShowsOptions := True;
    if Supports(Ancestor, IRootCommand, Root) and not IsBlank(Root.HelpName) then
      Parts := Parts + [Root.HelpName]
    else
      Parts := Parts + [Ancestor.Name];
    Parts := Parts + [ArgumentsUsage(Ancestor.Arguments)];
  end;
  Command := Chain[High(Chain)];
  ShowsCommand := False;
  for Subcommand in Command.Subcommands do
    if not Subcommand.IsHidden then
      ShowsCommand := True;
  if ShowsCommand then
    Parts := Parts + [TextOf(TCommandLineText.HelpUsageCommand)];
  for Option in Command.Options do
    if not Option.IsHidden then
      ShowsOptions := True;
  if ShowsOptions then
    Parts := Parts + [TextOf(TCommandLineText.HelpUsageOptions)];
  if not Command.TreatsUnmatchedTokensAsErrors then
    Parts := Parts + [TextOf(TCommandLineText.HelpUsageAdditionalArguments)];
  Result := '';
  for Part in Parts do
    if not IsBlank(Part) then
      if Result = '' then
        Result := Part
      else
        Result := Result + ' ' + Part;
end;

// The arguments of the command and of its ancestors, the root's first, each entry once.
function ArgumentRows(const Chain: TArray<ICommand>): TArray<TRow>;
var
  Ancestor: ICommand;
  Argument: IArgument;
  Row, Listed: TRow;
  Repeated: Boolean;
begin
  Result := nil;
  for Ancestor in Chain do
    for Argument in Ancestor.Arguments do
      if not Argument.IsHidden then
      begin
        Row := ArgumentRow(Argument);
        Repeated := False;
        for Listed in Result do
          if (Listed.First = Row.First) and (Listed.Second = Row.Second) then
            Repeated := True;
        if not Repeated then
          Result := Result + [Row];
      end;
end;

// The options of the command, the help and the version last, and then the recursive ones of each
// ancestor, the nearest first. The help of an ancestor is not listed again when the command has its own,
// and the version is not listed while the root declares none.
function OptionRows(const Chain: TArray<ICommand>): TArray<TRow>;
var
  Command: ICommand;
  Option: IOption;
  Root: IRootCommand;
  VersionDeclared, AddedHelp, Standard: Boolean;
  Rows: TArray<TRow>;
  I: Integer;

  procedure TryAdd(const Candidate: IOption);
  begin
    if Supports(Candidate, IVersionOption) and not VersionDeclared then
      Exit;
    Rows := Rows + [OptionRow(Candidate)];
  end;

begin
  Rows := nil;
  Command := Chain[High(Chain)];
  VersionDeclared := Supports(Chain[0], IRootCommand, Root) and not Root.VersionInfo.IsEmpty;
  AddedHelp := False;
  for Standard := False to True do
    for Option in Command.Options do
      if (IsStandardOption(Option) = Standard) and not Option.IsHidden then
      begin
        if Supports(Option, IHelpOption) then
          AddedHelp := True;
        TryAdd(Option);
      end;
  for I := High(Chain) - 1 downto 0 do
    for Option in Chain[I].Options do
      if Option.IsRecursive and not Option.IsHidden and not (Supports(Option, IHelpOption) and AddedHelp) then
        TryAdd(Option);
  Result := Rows;
end;

function SubcommandRows(const Command: ICommand): TArray<TRow>;
var
  Subcommand: ICommand;
begin
  Result := nil;
  for Subcommand in Command.Subcommands do
    if not Subcommand.IsHidden then
      Result := Result + [CommandRow(Subcommand)];
end;

{ TCommandLineHelp }

class function TCommandLineHelp.Render(const Command: ICommand; MaxWidth: Integer): string;
var
  Lines: TStringList;
  Chain: TArray<ICommand>;

  procedure WriteSection(const Heading: string; const Rows: TArray<TRow>);
  begin
    if Length(Rows) = 0 then
      Exit;
    WriteHeading(Lines, Heading, '', MaxWidth);
    WriteColumns(Lines, Rows, MaxWidth);
    Lines.Add('');
  end;

begin
  if Command.IsHidden then
    Exit('');
  Chain := Lineage(Command);
  Lines := TStringList.Create;
  try
    // Each section is followed by a blank line, the last one too. The description and the usage are
    // always written, even a description with nothing under its heading; the others only when they have
    // something to list.
    WriteHeading(Lines, TextOf(TCommandLineText.HelpDescriptionTitle), Command.Description, MaxWidth);
    Lines.Add('');
    WriteHeading(Lines, TextOf(TCommandLineText.HelpUsageTitle), UsageOf(Chain), MaxWidth);
    Lines.Add('');
    WriteSection(TextOf(TCommandLineText.HelpArgumentsTitle), ArgumentRows(Chain));
    WriteSection(TextOf(TCommandLineText.HelpOptionsTitle), OptionRows(Chain));
    WriteSection(TextOf(TCommandLineText.HelpCommandsTitle), SubcommandRows(Command));
    if not Command.TreatsUnmatchedTokensAsErrors then
    begin
      WriteHeading(Lines, TextOf(TCommandLineText.HelpAdditionalArgumentsTitle),
        TextOf(TCommandLineText.HelpAdditionalArgumentsDescription), MaxWidth);
      Lines.Add('');
    end;
    Result := string.Join(sLineBreak, Lines.ToStringArray);
  finally
    Lines.Free;
  end;
end;

end.
