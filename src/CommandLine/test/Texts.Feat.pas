unit Texts.Feat;

interface

uses
  Daf.CommandLine;

type
  TTextsWorld = class
  public
    Root: IRootCommand;
    NameOpt: IOption<string>;
    CountOpt: IOption<Integer>;
    Texts: TCommandLineTexts;
    Parsed: IParseResult;
    Report: string;
    procedure BuildGrammar;
    procedure PutInForce;
    /// <summary>The lines of the help that head a section, joined by '|'.</summary>
    function HelpTitles: string;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.TypInfo,
  Daf.CommandLine.Resources,
  Daf.MiniSpec;

var
  TextsBefore: TCommandLineTexts;

{ TTextsWorld }

procedure TTextsWorld.BuildGrammar;
begin
  NameOpt := CommandLine.Option<string>('--name').Description('Who to greet').Required;
  CountOpt := CommandLine.Option<Integer>('--count').Description('How many times');
  Root := CommandLine.Root('mytool', 'A tool');
  Root.Add([NameOpt, CountOpt]);
  Root.SetAction(
    procedure(const Parsed: IParseResult)
    begin
    end);
end;

procedure TTextsWorld.PutInForce;
begin
  try
    CommandLine.Texts := Texts;
    Report := '<nothing>';
  except
    on E: Exception do
      Report := E.ClassName + ': ' + E.Message;
  end;
end;

function TTextsWorld.HelpTitles: string;
var
  Lines: TStringList;
  Line: string;
begin
  Result := '';
  Lines := TStringList.Create;
  try
    Lines.Text := Root.HelpText;
    for Line in Lines do
      if (Line <> '') and (Line[1] <> ' ') and Line.EndsWith(':') then
      begin
        if Result <> '' then
          Result := Result + '|';
        Result := Result + Line;
      end;
  finally
    Lines.Free;
  end;
end;

initialization

Feature('''
Feature The texts the command line writes @commandline @texts

  As the author of a tool written in another language
  I want to write the texts of the command line in that language
  So its help and its errors do not mix my words with English ones
''')

.UseWorld<TTextsWorld>

.Before('keep the texts in force before the feature',
  procedure
  begin
    TextsBefore := CommandLine.Texts;
  end)

.After('put back the texts in force before the feature',
  procedure
  begin
    CommandLine.Texts := TextsBefore;
  end)

.Rule('the texts are the ones of System.CommandLine, in English')

  .Scenario('A required option that is not given')
    .Given('a required option --name, with no default',
      procedure(W: TTextsWorld)
      begin
        W.PutInForce;
        W.BuildGrammar;
      end)
    .When('an invocation that does not name it is parsed',
      procedure(W: TTextsWorld)
      begin
        W.Parsed := W.Root.Parse([]);
      end)
    .&Then('the error is the one of System.CommandLine',
      procedure(W: TTextsWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('Option ''--name'' is required.');
      end)

  .Scenario('A token that matches nothing')
    .Given('a command that does not collect what it does not recognise',
      procedure(W: TTextsWorld)
      begin
        W.PutInForce;
        W.BuildGrammar;
      end)
    .When('an invocation passing bogus is parsed',
      procedure(W: TTextsWorld)
      begin
        W.Parsed := W.Root.Parse(['--name', 'Ana', 'bogus']);
      end)
    .&Then('the error is the one of System.CommandLine',
      procedure(W: TTextsWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('Unrecognized command or argument ''bogus''.');
      end)

  .Scenario('The headings of the help')
    .Given('a command with options',
      procedure(W: TTextsWorld)
      begin
        W.PutInForce;
        W.BuildGrammar;
      end)
    .When('its help is asked for',
      procedure(W: TTextsWorld)
      begin
        W.Report := W.HelpTitles;
      end)
    .&Then('its sections are headed Usage: and Options:',
      procedure(W: TTextsWorld)
      begin
        Expect(W.Report).ToEqual('Description:|Usage:|Options:');
      end)

.Rule('the application can replace the texts')

  .Scenario('A replaced text')
    .Given('texts that replace the one of a required option',
      procedure(W: TTextsWorld)
      begin
        W.Texts[TCommandLineText.RequiredOptionWasNotProvided] := 'La opción ''%0:s'' es obligatoria.';
        W.PutInForce;
        W.BuildGrammar;
      end)
    .When('an invocation without the required option --name is parsed',
      procedure(W: TTextsWorld)
      begin
        W.Parsed := W.Root.Parse([]);
      end)
    .&Then('the error is the replaced text',
      procedure(W: TTextsWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('La opción ''--name'' es obligatoria.');
      end)

  .Scenario('A text that is not replaced is written in English')
    .Given('texts that only replace the heading of the usage',
      procedure(W: TTextsWorld)
      begin
        W.Texts[TCommandLineText.HelpUsageTitle] := 'Uso:';
        W.PutInForce;
        W.BuildGrammar;
      end)
    .When('the help of a command with options is asked for',
      procedure(W: TTextsWorld)
      begin
        W.Report := W.HelpTitles;
      end)
    .&Then('the heading of the usage is the replaced one and the one of the options is English',
      procedure(W: TTextsWorld)
      begin
        Expect(W.Report).ToEqual('Description:|Uso:|Options:');
      end)

  .Scenario('A template asking for more arguments than its text receives')
    .Given('texts with a template of two arguments for a text that receives one',
      procedure(W: TTextsWorld)
      begin
        W.PutInForce;
        W.Texts[TCommandLineText.RequiredOptionWasNotProvided] := '%0:s, %1:s';
      end)
    .When('the application puts them in force',
      procedure(W: TTextsWorld)
      begin
        W.PutInForce;
      end)
    .&Then('the assignment is refused naming the key',
      procedure(W: TTextsWorld)
      begin
        Expect(W.Report).ToContain('ECommandLine: ');
        Expect(W.Report).ToContain('RequiredOptionWasNotProvided');
      end)
    .&And('the texts in force are still the ones before',
      procedure(W: TTextsWorld)
      begin
        Expect(CommandLine.Texts[TCommandLineText.RequiredOptionWasNotProvided]).ToEqual('');
      end)

  .Scenario('The arguments in another order')
    .Given('texts that replace the conversion error of an option with one naming the option before the value',
      procedure(W: TTextsWorld)
      begin
        W.Texts[TCommandLineText.ArgumentConversionCannotParseForOption] := 'La opción ''%1:s'' no admite ''%0:s''.';
        W.PutInForce;
        W.BuildGrammar;
      end)
    .When('an invocation giving an Integer option a value that is no integer is parsed',
      procedure(W: TTextsWorld)
      begin
        W.Parsed := W.Root.Parse(['--name', 'Ana', '--count', 'abc']);
      end)
    .&Then('the error names the option first and the value after it',
      procedure(W: TTextsWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('La opción ''--count'' no admite ''abc''.');
      end)

  .Scenario('An error already reported keeps its text')
    .Given('an invocation parsed with texts that replace the one of a required option',
      procedure(W: TTextsWorld)
      begin
        W.Texts[TCommandLineText.RequiredOptionWasNotProvided] := 'Falta ''%0:s''.';
        W.PutInForce;
        W.BuildGrammar;
        W.Parsed := W.Root.Parse([]);
      end)
    .When('the application puts other texts in force',
      procedure(W: TTextsWorld)
      begin
        W.Texts[TCommandLineText.RequiredOptionWasNotProvided] := 'Se necesita ''%0:s''.';
        W.PutInForce;
      end)
    .&Then('the error keeps the text it was written with',
      procedure(W: TTextsWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('Falta ''--name''.');
      end)

.Rule('the application can know which texts it has not replaced')

  .Scenario('The keys left')
    .Given('texts that replace every one but the heading of the usage',
      procedure(W: TTextsWorld)
      var
        Key: TCommandLineText;
      begin
        for Key := Low(TCommandLineText) to High(TCommandLineText) do
          if Key <> TCommandLineText.HelpUsageTitle then
            W.Texts[Key] := 'x';
      end)
    .When('the keys left are asked for',
      procedure(W: TTextsWorld)
      var
        Key: TCommandLineText;
      begin
        W.Report := '';
        for Key in W.Texts.Missing do
          W.Report := W.Report + GetEnumName(TypeInfo(TCommandLineText), Ord(Key)) + ';';
      end)
    .&Then('the answer is that key and no other',
      procedure(W: TTextsWorld)
      begin
        Expect(W.Report).ToEqual('HelpUsageTitle;');
      end)

.Rule('each English text is a template of the arguments its key receives')

  .Scenario('Every English text put in force as the application''s')
    .Given('texts holding the English text of every key',
      procedure(W: TTextsWorld)
      var
        Key: TCommandLineText;
      begin
        for Key := Low(TCommandLineText) to High(TCommandLineText) do
          W.Texts[Key] := EnglishText(Key);
      end)
    .When('the application puts them in force',
      procedure(W: TTextsWorld)
      begin
        W.PutInForce;
      end)
    .&Then('none is refused',
      procedure(W: TTextsWorld)
      begin
        Expect(W.Report).ToEqual('<nothing>');
      end)
    .&And('no key is left without its English text',
      procedure(W: TTextsWorld)
      begin
        Expect(Length(W.Texts.Missing)).ToEqual(0);
      end)

end.
