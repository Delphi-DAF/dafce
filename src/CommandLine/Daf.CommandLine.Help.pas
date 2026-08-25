unit Daf.CommandLine.Help;

{
  The help renderer.

  It walks the declared grammar and formats it, every time it is asked. No composed
  text is stored anywhere, and that is the whole point (design D4): help that is kept
  as text is documentation NEXT TO the code instead of derived FROM it, so it goes
  stale the moment somebody adds an option and forgets it -- and the mismatch breaks
  nothing, it only lies to whoever asked.

  The withdrawn Daf.CmdLn did store a help string per argument, and nothing ever read it.
  Declared, and never reaching any output.
}

interface

uses
  Daf.CommandLine;

type
  TCommandLineHelp = record
  public
    /// <summary>Renders the help of one command from the grammar hanging off it.</summary>
    class function Render(const Command: ICommand): string; static;
  end;

implementation

uses
  System.SysUtils,
  System.Classes;

const
  // Where the description column starts. Anything longer gets its description on the
  // next line rather than pushing the column out for every other row.
  DescriptionColumn = 24;

function Padded(const Left, Right: string): string;
begin
  if Right = '' then
    Exit('  ' + Left);
  if Length(Left) + 2 >= DescriptionColumn then
    Exit('  ' + Left + sLineBreak + StringOfChar(' ', DescriptionColumn) + Right);
  Result := '  ' + Left + StringOfChar(' ', DescriptionColumn - Length(Left) - 2) + Right;
end;

function OptionLabel(const Option: IOption): string;
var
  Alias: string;
begin
  Result := Option.Name;
  for Alias in Option.Aliases do
    Result := Result + ', ' + Alias;
  if not Option.IsFlag then
    Result := Result + ' <' + Option.TypeName + '>';
end;

function ArgumentLabel(const Argument: IArgument): string;
begin
  Result := '<' + Argument.Name + '>';
end;

// What the symbol itself knows and the application would otherwise have to repeat in
// prose: the values it accepts, its default, and whether it is obligatory. Repeating
// them by hand is how a help text starts disagreeing with the program.
function Annotated(const Symbol: IValued; Required: Boolean): string;
begin
  Result := Symbol.Description;
  if Length(Symbol.AcceptedValues) > 0 then
    Result := Trim(Result + ' [allowed: ' + string.Join('|', Symbol.AcceptedValues) + ']');
  if Symbol.HasDefault and (Symbol.DefaultText <> '') then
    Result := Trim(Result + ' [default: ' + Symbol.DefaultText + ']');
  if Required then
    Result := Trim(Result + ' [required]');
end;

function CommandLabel(const Command: ICommand): string;
var
  Alias: string;
begin
  Result := Command.Name;
  for Alias in Command.Aliases do
    Result := Result + ', ' + Alias;
end;

function UsageLine(const Command: ICommand): string;
var
  Argument: IArgument;
begin
  Result := Command.FullPath;
  for Argument in Command.Arguments do
    if not Argument.IsHidden then
      Result := Result + ' ' + ArgumentLabel(Argument);
  if Length(Command.Subcommands) > 0 then
    Result := Result + ' <command>';
  Result := Result + ' [options]';
end;

// How many of each the help is going to list. Written twice, and it has to be: a
// TArray<IArgument> is not assignable to an array of ISymbol -- Delphi has no
// covariance for dynamic arrays -- and a standalone generic function is not allowed
// either. One loop each beats building a converted array to save six lines.
function VisibleArguments(const Command: ICommand): Integer;
var
  Argument: IArgument;
begin
  Result := 0;
  for Argument in Command.Arguments do
    if not Argument.IsHidden then
      Inc(Result);
end;

function VisibleSubcommands(const Command: ICommand): Integer;
var
  Subcommand: ICommand;
begin
  Result := 0;
  for Subcommand in Command.Subcommands do
    if not Subcommand.IsHidden then
      Inc(Result);
end;

function DeclaresOwnHelp(const Command: ICommand): Boolean;
var
  Option: IOption;
  Token: string;
begin
  for Option in Command.Options do
    for Token in HelpTokens do
      if Option.Matches(Token) then
        Exit(True);
  Result := False;
end;

function DeclaresOwnVersion(const Command: ICommand): Boolean;
var
  Option: IOption;
begin
  for Option in Command.Options do
    if Option.Matches(VersionToken) then
      Exit(True);
  Result := False;
end;

class function TCommandLineHelp.Render(const Command: ICommand): string;
var
  Lines: TStringList;
  Option: IOption;
  Argument: IArgument;
  Subcommand: ICommand;
  Root: IRootCommand;
begin
  Lines := TStringList.Create;
  try
    Lines.LineBreak := sLineBreak;

    if Command.Description <> '' then
    begin
      Lines.Add(Command.Description);
      Lines.Add('');
    end;

    Lines.Add('Usage:');
    Lines.Add('  ' + UsageLine(Command));

    // Hidden is filtered HERE and nowhere else (design D5). The heading is only written
    // when something is going to follow it: a section listing nothing reads as a bug in
    // the tool rather than as a symbol somebody chose not to advertise.
    if VisibleArguments(Command) > 0 then
    begin
      Lines.Add('');
      Lines.Add('Arguments:');
      for Argument in Command.Arguments do
        if not Argument.IsHidden then
          Lines.Add(Padded(ArgumentLabel(Argument), Annotated(Argument, Argument.Arity.DemandsValue and not Argument.HasDefault)));
    end;

    if VisibleSubcommands(Command) > 0 then
    begin
      Lines.Add('');
      Lines.Add('Commands:');
      for Subcommand in Command.Subcommands do
        if not Subcommand.IsHidden then
          Lines.Add(Padded(CommandLabel(Subcommand), Subcommand.Description));
    end;

    Lines.Add('');
    Lines.Add('Options:');
    for Option in Command.Options do
      if not Option.IsHidden then
        Lines.Add(Padded(OptionLabel(Option), Annotated(Option, Option.IsRequired and not Option.HasDefault)));
    // Listed because they answer, not because anybody declared them. When the
    // application declared its own, the loop above already printed that one.
    if not DeclaresOwnHelp(Command) then
      Lines.Add(Padded(HelpTokens[0] + ', ' + HelpTokens[1], 'Show help and usage information'));
    // The version is the root's, so only the root advertises it.
    if Supports(Command, IRootCommand, Root) and (Root.VersionText <> '') and not DeclaresOwnVersion(Command) then
      Lines.Add(Padded(VersionToken, 'Show version information'));

    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

end.
