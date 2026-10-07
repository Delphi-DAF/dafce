unit Types.Feat;

interface

uses
  System.TimeSpan,
  Daf.Nullable,
  Daf.CommandLine;

type
  /// <summary>
  ///   A type the engine knows nothing about, and never will. That is the point of the
  ///   hook: what has to become possible is a type DAF never heard of, declared by an
  ///   application, without a change to the framework.
  /// </summary>
  TSemVer = record
    Major: Integer;
    Minor: Integer;
    Patch: Integer;
  end;

  /// <summary>A record with the two fields of <c>TNullable</c> and another name: not a value that may be missing.</summary>
  TMaybe<T> = record
    FValue: T;
    FHasValue: Boolean;
  end;

  {$SCOPEDENUMS ON}
  TLetter = (A, B, C);
  {$SCOPEDENUMS OFF}
  TLetters = set of TLetter;

  TTypesWorld = class
  public
    // One small grammar per rule: a flat root, so nothing else interferes with what is
    // being asserted.
    Own: IRootCommand;
    MinVersionOpt: IOption<TSemVer>;

    Wide: IRootCommand;
    SinceOpt: IOption<TDateTime>;
    IdOpt: IOption<TGUID>;
    BytesOpt: IOption<Int64>;
    PriceOpt: IOption<Currency>;

    Many: IRootCommand;
    FilesOpt: IOption<TArray<string>>;
    // A conversion over every token: a collection, and two arguments sharing the tokens.
    Whole: IRootCommand;
    SortedOpt: IOption<TArray<string>>;
    FirstArg: IArgument<string>;
    RestArg: IArgument<TArray<string>>;
    ReadFirst: string;
    ReadRest: TArray<string>;

    // The integers of every width and the simple real, one option each, named after its type.
    Numbers: IRootCommand;
    ShortIntOpt: IOption<ShortInt>;
    ByteOpt: IOption<Byte>;
    SmallIntOpt: IOption<SmallInt>;
    WordOpt: IOption<Word>;
    CardinalOpt: IOption<Cardinal>;
    UInt64Opt: IOption<UInt64>;
    SingleOpt: IOption<Single>;
    // The columns of the outlines over them.
    Kind: string;
    Text: string;
    Expected: string;

    Times: IRootCommand;
    DayOpt: IOption<TDate>;
    AtOpt: IOption<TTime>;
    WaitOpt: IOption<TTimeSpan>;

    Missing: IRootCommand;
    LimitOpt: IOption<TNullable<Integer>>;
    DryOpt: IOption<TNullable<Boolean>>;

    Paths: IRootCommand;
    SourceOpt: IOption<TFilePath>;
    TargetOpt: IOption<TDirectoryPath>;

    Sets: IRootCommand;
    LettersOpt: IOption<TLetters>;

    Parsed: IParseResult;
    Report: string;
    ReadDay: TDate;
    ReadAt: TTime;
    ReadWait: TTimeSpan;
    ReadSingle: Single;
    ReadLimit: TNullable<Integer>;
    ReadDry: TNullable<Boolean>;
    ReadSource: TFilePath;
    ReadLetters: TLetters;
    ReadArityMin: Integer;
    ReadArityMax: Integer;
    ReadAllowsMany: Boolean;
    ReadVersion: TSemVer;
    ReadSince: TDateTime;
    ReadId: TGUID;
    ReadBytes: Int64;
    ReadPrice: Currency;
    ReadFiles: TArray<string>;
    procedure BuildOwnType;
    procedure BuildWideTypes;
    procedure BuildCollection;
    procedure BuildWholeConversions;
    procedure BuildNumbers;
    procedure BuildTimes;
    procedure BuildMissing;
    procedure BuildPaths;
    procedure BuildSets;
    /// <summary>The value of the option named after <c>Kind</c>, rendered.</summary>
    function ReadNumber: string;
  end;

implementation

uses
  System.SysUtils,
  System.DateUtils,
  System.Generics.Collections,
  Daf.MiniSpec;

// A set rendered as the names of its members, in order, so a failed assertion says which are there.
function LettersText(const Letters: TLetters): string;
const
  Names: array [TLetter] of string = ('A', 'B', 'C');
var
  Letter: TLetter;
begin
  Result := '';
  for Letter in Letters do
    if Result = '' then
      Result := Names[Letter]
    else
      Result := Result + ',' + Names[Letter];
end;

{ TTypesWorld }

procedure TTypesWorld.BuildNumbers;
begin
  ShortIntOpt := CommandLine.Option<ShortInt>('--shortint').Description('A small signed number');
  ByteOpt := CommandLine.Option<Byte>('--byte').Description('A byte');
  SmallIntOpt := CommandLine.Option<SmallInt>('--smallint').Description('A short signed number');
  WordOpt := CommandLine.Option<Word>('--word').Description('A short unsigned number');
  CardinalOpt := CommandLine.Option<Cardinal>('--cardinal').Description('An unsigned number');
  UInt64Opt := CommandLine.Option<UInt64>('--uint64').Description('A wide unsigned number');
  SingleOpt := CommandLine.Option<Single>('--single').Description('A simple real');
  Numbers := CommandLine.Root('tool', 'A tool');
  Numbers.Add([ShortIntOpt, ByteOpt, SmallIntOpt, WordOpt, CardinalOpt, UInt64Opt, SingleOpt]);
end;

function TTypesWorld.ReadNumber: string;
begin
  if Kind = 'shortint' then
    Result := IntToStr(ShortIntOpt.ValueIn(Parsed))
  else if Kind = 'byte' then
    Result := IntToStr(ByteOpt.ValueIn(Parsed))
  else if Kind = 'smallint' then
    Result := IntToStr(SmallIntOpt.ValueIn(Parsed))
  else if Kind = 'word' then
    Result := IntToStr(WordOpt.ValueIn(Parsed))
  else if Kind = 'cardinal' then
    Result := UIntToStr(CardinalOpt.ValueIn(Parsed))
  else if Kind = 'uint64' then
    Result := UIntToStr(UInt64Opt.ValueIn(Parsed))
  else if Kind = 'single' then
    Result := FloatToStr(SingleOpt.ValueIn(Parsed), TFormatSettings.Invariant)
  else
    Result := 'unknown kind ' + Kind;
end;

procedure TTypesWorld.BuildTimes;
begin
  DayOpt := CommandLine.Option<TDate>('--day').Description('Which day');
  AtOpt := CommandLine.Option<TTime>('--at').Description('At what time');
  WaitOpt := CommandLine.Option<TTimeSpan>('--wait').Description('How long');
  Times := CommandLine.Root('tool', 'A tool');
  Times.Add([DayOpt, AtOpt, WaitOpt]);
end;

procedure TTypesWorld.BuildMissing;
begin
  LimitOpt := CommandLine.Option<TNullable<Integer>>('--limit').Description('How many at most');
  DryOpt := CommandLine.Option<TNullable<Boolean>>('--dry').Description('Change nothing');
  Missing := CommandLine.Root('tool', 'A tool');
  Missing.Add([LimitOpt, DryOpt]);
end;

procedure TTypesWorld.BuildPaths;
begin
  SourceOpt := CommandLine.Option<TFilePath>('--source').Description('What to read').AcceptExistingOnly;
  TargetOpt := CommandLine.Option<TDirectoryPath>('--target').Description('Where to write').AcceptExistingOnly;
  Paths := CommandLine.Root('tool', 'A tool');
  Paths.Add([SourceOpt, TargetOpt]);
end;

procedure TTypesWorld.BuildSets;
begin
  LettersOpt := CommandLine.Option<TLetters>('--letters').Description('Which letters');
  Sets := CommandLine.Root('tool', 'A tool');
  Sets.Add(LettersOpt);
end;

procedure TTypesWorld.BuildWideTypes;
begin
  SinceOpt := CommandLine.Option<TDateTime>('--since').Description('Oldest date considered');
  IdOpt := CommandLine.Option<TGUID>('--id').Description('Which run');
  BytesOpt := CommandLine.Option<Int64>('--bytes').Description('How many bytes at most');
  PriceOpt := CommandLine.Option<Currency>('--price').Description('What it costs');
  Wide := CommandLine.Root('tool', 'A tool');
  Wide.Add([SinceOpt, IdOpt, BytesOpt, PriceOpt]);
end;

procedure TTypesWorld.BuildCollection;
begin
  // No arity declared: a collection says its own, the way .NET derives it from the type.
  FilesOpt := CommandLine.Option<TArray<string>>('--files').Description('Files to compile');
  Many := CommandLine.Root('tool', 'A tool');
  Many.Add(FilesOpt);
end;

procedure TTypesWorld.BuildWholeConversions;
begin
  SortedOpt := CommandLine.Option<TArray<string>>('--sorted').AllowMultipleArgumentsPerToken.CustomParser(
    function(const Found: IArgumentResult): TArray<string>
    var
      I: Integer;
    begin
      SetLength(Result, Length(Found.Tokens));
      for I := 0 to High(Result) do
        Result[I] := Found.Tokens[I].Value;
      TArray.Sort<string>(Result);
    end);
  FirstArg := CommandLine.Argument<string>('first').Arity(TArgumentArity.ZeroOrMore).CustomParser(
    function(const Found: IArgumentResult): string
    begin
      Found.OnlyTake(1);
      Result := Found.Tokens[0].Value;
    end);
  RestArg := CommandLine.Argument<TArray<string>>('rest');
  Whole := CommandLine.Root('tool', 'A tool');
  Whole.Add([SortedOpt, FirstArg, RestArg]);
end;

procedure TTypesWorld.BuildOwnType;
begin
  // The conversion is declared where the symbol is, and nowhere else: a registry of
  // types would be process-wide configuration, and two libraries registering the same
  // type would fight over it in silence.
  MinVersionOpt := CommandLine.Option<TSemVer>('--min',
    function(const Raw: string; out Value: TSemVer; out Error: string): Boolean
    var
      Parts: TArray<string>;
    begin
      Error := '';
      Parts := Raw.Split(['.']);
      if Length(Parts) <> 3 then
      begin
        Error := Format('"%s" is not a version: write it as major.minor.patch', [Raw]);
        Exit(False);
      end;
      Result := TryStrToInt(Parts[0], Value.Major) and TryStrToInt(Parts[1], Value.Minor) and
        TryStrToInt(Parts[2], Value.Patch);
      if not Result then
        Error := Format('"%s" is not a version: its three parts have to be numbers', [Raw]);
    end).Description('Lowest version accepted');
  Own := CommandLine.Root('tool', 'A tool');
  Own.Add(MinVersionOpt);
end;

initialization

Feature('''
Feature Command line types @commandline @types

  As a developer writing a console application
  I want the engine to convert the types a command line really uses, and to accept a
  conversion of my own for one it does not know
  So a missing type is a line of my application instead of a change to the framework
''')

.UseWorld<TTypesWorld>

.Rule('a type the engine does not know is declared with a conversion of its own')

  .Scenario('A type of the application, with its conversion declared')
    .Given('an option declared with a type of the application and a conversion for it',
      procedure(W: TTypesWorld)
      begin
        W.BuildOwnType;
      end)
    .When('the invocation passes a value that conversion accepts',
      procedure(W: TTypesWorld)
      begin
        W.Parsed := W.Own.Parse(['--min', '1.4.2']);
        W.ReadVersion := W.MinVersionOpt.ValueIn(W.Parsed);
      end)
    .&Then('the value is read with the declared type',
      procedure(W: TTypesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.ReadVersion.Major).ToEqual(1);
        Expect(W.ReadVersion.Minor).ToEqual(4);
        Expect(W.ReadVersion.Patch).ToEqual(2);
      end)

  .Scenario('A conversion of its own that refuses the value')
    .Given('an option with a conversion of its own declared',
      procedure(W: TTypesWorld)
      begin
        W.BuildOwnType;
      end)
    .When('the invocation passes a value that conversion refuses',
      procedure(W: TTypesWorld)
      begin
        W.Parsed := W.Own.Parse(['--min', '1.4']);
      end)
    .&Then('the parse fails with the message the conversion wrote',
      procedure(W: TTypesWorld)
      begin
        Expect(W.Parsed.Succeeded).ToBeFalse;
        Expect(W.Parsed.ErrorText).ToContain('major.minor.patch');
      end)
    .&And('the refusal arrives as an invocation error, not as an exception',
      procedure(W: TTypesWorld)
      begin
        // Reaching this step at all is half the assertion: Parse returned instead of
        // raising. The other half is that the error is in the same list as the rest.
        Expect(Length(W.Parsed.Errors)).ToEqual(1);
      end)

.Rule('a conversion of its own can take every token of the symbol')

  .Scenario('A conversion of its own over a collection')
    .Given('a collection option with a conversion of its own that takes all its tokens and answers them sorted',
      procedure(W: TTypesWorld)
      begin
        W.BuildWholeConversions;
      end)
    .When('the invocation passes it c, a and b',
      procedure(W: TTypesWorld)
      begin
        W.Parsed := W.Whole.Parse(['--sorted', 'c', 'a', 'b']);
        W.ReadFiles := W.SortedOpt.ValueIn(W.Parsed);
      end)
    .&Then('the collection read is a, b, c',
      procedure(W: TTypesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(string.Join(',', W.ReadFiles)).ToEqual('a,b,c');
      end)

  .Scenario('A conversion that leaves tokens for the next argument')
    .Given('a command with two arguments of variable arity, the first with a conversion that keeps only its first token',
      procedure(W: TTypesWorld)
      begin
        W.BuildWholeConversions;
      end)
    .When('the invocation passes three values',
      procedure(W: TTypesWorld)
      begin
        W.Parsed := W.Whole.Parse(['x', 'y', 'z']);
        W.ReadFirst := W.FirstArg.ValueIn(W.Parsed);
        W.ReadRest := W.RestArg.ValueIn(W.Parsed);
      end)
    .&Then('the first argument reads the first one, and the second the other two',
      procedure(W: TTypesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(Format('first=%s rest=%s', [W.ReadFirst, string.Join(',', W.ReadRest)])).ToEqual('first=x rest=y,z');
      end)

  .Scenario('A conversion of one token is still refused on a collection')
    .Given('a collection type').NoAction
    .When('an option is declared with it and a conversion of one token',
      procedure(W: TTypesWorld)
      begin
        try
          CommandLine.Option<TArray<string>>('--parts',
            function(const Raw: string; out Value: TArray<string>; out Error: string): Boolean
            begin
              Error := '';
              Value := [Raw];
              Result := True;
            end);
          W.Report := '<the declaration was accepted>';
        except
          on E: Exception do
            W.Report := E.ClassName + ': ' + E.Message;
        end;
      end)
    .&Then('the declaration is refused, naming the symbol',
      procedure(W: TTypesWorld)
      begin
        Expect(W.Report).ToStartWith('ECommandLine: Symbol "--parts" is a collection');
      end)

.Rule('the engine covers the types a command line really uses')

  .Scenario('A date with a time and no zone')
    .Given('an option declared with a date type',
      procedure(W: TTypesWorld)
      begin
        W.BuildWideTypes;
      end)
    .When('the invocation passes a time late in the day, with no zone',
      procedure(W: TTypesWorld)
      begin
        W.Parsed := W.Wide.Parse(['--since', '2026-08-25T23:30']);
        W.ReadSince := W.SinceOpt.ValueIn(W.Parsed);
      end)
    .&Then('the date and the time read are the ones written, not shifted by the zone of the machine',
      procedure(W: TTypesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(FormatDateTime('yyyy-mm-dd hh:nn', W.ReadSince, TFormatSettings.Invariant)).ToEqual('2026-08-25 23:30');
      end)

  .Scenario('A date with a time and a zone')
    .Given('an option declared with a date type',
      procedure(W: TTypesWorld)
      begin
        W.BuildWideTypes;
      end)
    .When('the invocation passes a time in UTC',
      procedure(W: TTypesWorld)
      begin
        W.Parsed := W.Wide.Parse(['--since', '2026-08-25T23:30Z']);
        W.ReadSince := W.SinceOpt.ValueIn(W.Parsed);
      end)
    .&Then('it is read converted to the local time of the machine',
      procedure(W: TTypesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(FormatDateTime('yyyy-mm-dd hh:nn', W.ReadSince, TFormatSettings.Invariant)).ToEqual(
          FormatDateTime('yyyy-mm-dd hh:nn', TTimeZone.Local.ToLocalTime(EncodeDateTime(2026, 8, 25, 23, 30, 0, 0)),
          TFormatSettings.Invariant));
      end)

  .Scenario('A date')
    .Given('an option declared with a date type',
      procedure(W: TTypesWorld)
      begin
        W.BuildWideTypes;
      end)
    .When('the invocation passes a valid date',
      procedure(W: TTypesWorld)
      begin
        W.Parsed := W.Wide.Parse(['--since', '2026-08-25']);
        W.ReadSince := W.SinceOpt.ValueIn(W.Parsed);
      end)
    .&Then('it is read with the declared type, with no conversion by the application',
      procedure(W: TTypesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(FormatDateTime('yyyy-mm-dd', W.ReadSince)).ToEqual('2026-08-25');
      end)

  .Scenario('A date written some other way')
    .Given('an option declared with a date type',
      procedure(W: TTypesWorld)
      begin
        W.BuildWideTypes;
      end)
    .When('the invocation writes it in a local convention instead of ISO 8601',
      procedure(W: TTypesWorld)
      begin
        // Declared divergence: only ISO 8601 is read. A command line is what a script
        // wrote down, and 01/02/2026 means two different days depending on who reads it.
        W.Parsed := W.Wide.Parse(['--since', '25/08/2026']);
      end)
    .&Then('the parse fails naming the option and the text received',
      procedure(W: TTypesWorld)
      begin
        Expect(W.Parsed.Succeeded).ToBeFalse;
        Expect(W.Parsed.ErrorText).ToContain('--since');
        Expect(W.Parsed.ErrorText).ToContain('25/08/2026');
      end)

  .Scenario('An identifier')
    .Given('an option declared with an identifier type',
      procedure(W: TTypesWorld)
      begin
        W.BuildWideTypes;
      end)
    .When('the invocation passes one, written without braces',
      procedure(W: TTypesWorld)
      begin
        W.Parsed := W.Wide.Parse(['--id', '3F2504E0-4F89-41D3-9A0C-0305E82C3301']);
        W.ReadId := W.IdOpt.ValueIn(W.Parsed);
      end)
    .&Then('it is read with the declared type',
      procedure(W: TTypesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(GUIDToString(W.ReadId)).ToEqual('{3F2504E0-4F89-41D3-9A0C-0305E82C3301}');
      end)

  .Scenario('An identifier that is not one')
    .Given('an option declared with an identifier type',
      procedure(W: TTypesWorld)
      begin
        W.BuildWideTypes;
      end)
    .When('the invocation passes something that is not an identifier',
      procedure(W: TTypesWorld)
      begin
        W.Parsed := W.Wide.Parse(['--id', 'not-an-id']);
      end)
    .&Then('the parse fails naming the option and the text received',
      procedure(W: TTypesWorld)
      begin
        Expect(W.Parsed.Succeeded).ToBeFalse;
        Expect(W.Parsed.ErrorText).ToContain('--id');
        Expect(W.Parsed.ErrorText).ToContain('not-an-id');
      end)

  .Scenario('An integer wider than the ordinary one')
    .Given('an option declared with a wider integer type',
      procedure(W: TTypesWorld)
      begin
        W.BuildWideTypes;
      end)
    .When('the invocation passes a value that does not fit in the ordinary one',
      procedure(W: TTypesWorld)
      begin
        W.Parsed := W.Wide.Parse(['--bytes', '5000000000']);
        W.ReadBytes := W.BytesOpt.ValueIn(W.Parsed);
      end)
    .&Then('the whole value is read, not a truncation of it',
      procedure(W: TTypesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(IntToStr(W.ReadBytes)).ToEqual('5000000000');
      end)

  .Scenario('An exact decimal amount')
    .Given('an option declared with the exact decimal type',
      procedure(W: TTypesWorld)
      begin
        W.BuildWideTypes;
      end)
    .When('the invocation passes an amount with four decimals',
      procedure(W: TTypesWorld)
      begin
        W.Parsed := W.Wide.Parse(['--price', '1234.5678']);
        W.ReadPrice := W.PriceOpt.ValueIn(W.Parsed);
      end)
    .&Then('every decimal comes back, with no rounding of its own',
      procedure(W: TTypesWorld)
      begin
        // Rendered rather than compared to a literal: an assertion that printed nothing
        // would not say WHICH digit was lost, and losing one is the whole point of using
        // the exact decimal instead of a binary float.
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(CurrToStrF(W.ReadPrice, ffFixed, 4, TFormatSettings.Invariant)).ToEqual('1234.5678');
      end)

  .Scenario('An amount that is not one')
    .Given('an option declared with the exact decimal type',
      procedure(W: TTypesWorld)
      begin
        W.BuildWideTypes;
      end)
    .When('the invocation passes something that is not an amount',
      procedure(W: TTypesWorld)
      begin
        W.Parsed := W.Wide.Parse(['--price', '12.34.56']);
      end)
    .&Then('the parse fails naming the option and the text received',
      procedure(W: TTypesWorld)
      begin
        Expect(W.Parsed.Succeeded).ToBeFalse;
        Expect(W.Parsed.ErrorText).ToContain('--price');
        Expect(W.Parsed.ErrorText).ToContain('12.34.56');
      end)

.Rule('a type with neither support nor a conversion is refused before any invocation is read')

  .Scenario('A type of the application, declared with no conversion')
    // Nothing to build and nothing to assert: TSemVer is a compiled type, and whether the
    // engine knows it cannot be asked before a symbol is declared with it.
    .Given('a type the engine does not know').NoAction
    .When('an option is declared with it and no conversion for it, and an invocation is parsed',
      procedure(W: TTypesWorld)
      begin
        // The conversion of its own is declared after the option is, so the declaration cannot know yet
        // whether there will be one. Adding the option to a command is the first place that can.
        try
          var Min := CommandLine.Option<TSemVer>('--min').Description('Lowest version accepted');
          W.Report := 'declared';
          var Root := CommandLine.Root('tool', 'A tool');
          Root.Add(Min);
          Root.Parse(['--min', '1.2.3']);
          W.Report := W.Report + ', <added and parsed>';
        except
          on E: Exception do
            W.Report := W.Report + ', ' + E.ClassName + ': ' + E.Message;
        end;
      end)
    .&Then('it is refused before the invocation is read, naming the symbol and the type',
      procedure(W: TTypesWorld)
      begin
        Expect(W.Report).ToStartWith('declared, ECommandLine: ');
        Expect(W.Report).ToContain('--min');
        Expect(W.Report).ToContain('TSemVer');
      end)

.Rule('a collection is a declared type, and its arity comes from it')

  .Scenario('A collection derives its arity')
    .Given('an option declared with a collection type and no explicit arity',
      procedure(W: TTypesWorld)
      begin
        W.BuildCollection;
      end)
    .When('its arity is inspected',
      procedure(W: TTypesWorld)
      begin
        W.ReadArityMin := W.FilesOpt.Arity.MinimumNumberOfValues;
        W.ReadArityMax := W.FilesOpt.Arity.MaximumNumberOfValues;
        W.ReadAllowsMany := W.FilesOpt.Arity.AllowsMany;
      end)
    .&Then('it admits any number of values, as in .NET',
      procedure(W: TTypesWorld)
      begin
        Expect(W.ReadArityMin).ToEqual(0);
        Expect(W.ReadArityMax).ToEqual(TArgumentArity.Unbounded);
        Expect(W.ReadAllowsMany).ToBeTrue;
      end)

  .Scenario('A collection reads every value it was given')
    .Given('an option declared with a collection type',
      procedure(W: TTypesWorld)
      begin
        W.BuildCollection;
      end)
    .When('the invocation gives it several values',
      procedure(W: TTypesWorld)
      begin
        W.Parsed := W.Many.Parse(['--files', 'a.pas', '--files', 'b.pas']);
        W.ReadFiles := W.FilesOpt.ValueIn(W.Parsed);
      end)
    .&Then('the value read is the collection, with every value in the order they came',
      procedure(W: TTypesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(Length(W.ReadFiles)).ToEqual(2);
        Expect(W.ReadFiles[0]).ToEqual('a.pas');
        Expect(W.ReadFiles[1]).ToEqual('b.pas');
      end)

.Rule('an integer of any width is read whole and within its range')

  .ScenarioOutline('The <Kind> type reads <Text>')
    .Given('an option declared with each integer width and with the simple real',
      procedure(W: TTypesWorld)
      begin
        W.BuildNumbers;
      end)
    .When('the invocation passes <Text> to the <Kind> one',
      procedure(W: TTypesWorld)
      begin
        W.Parsed := W.Numbers.Parse(['--' + W.Kind, W.Text]);
        W.Report := W.ReadNumber;
      end)
    .&Then('it is read with its type, whole',
      procedure(W: TTypesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.Report).ToEqual(W.Text);
      end)
    .Examples(
      [['Kind',     'Text'],
       ['shortint', '-128'],
       ['byte',     '255'],
       ['smallint', '-32768'],
       ['word',     '65535'],
       ['cardinal', '4294967295'],
       ['uint64',   '18446744073709551615'],
       ['single',   '1.5']])

  .Scenario('A negative for an unsigned integer')
    .Given('an option declared as an unsigned integer',
      procedure(W: TTypesWorld)
      begin
        W.BuildNumbers;
      end)
    .When('the invocation passes it -1',
      procedure(W: TTypesWorld)
      begin
        W.Parsed := W.Numbers.Parse(['--cardinal', '-1']);
      end)
    .&Then('the parse fails naming the option and the value',
      procedure(W: TTypesWorld)
      begin
        Expect(W.Parsed.Succeeded).ToBeFalse;
        Expect(W.Parsed.ErrorText).ToContain('--cardinal');
        Expect(W.Parsed.ErrorText).ToContain('-1');
      end)

  .ScenarioOutline('<Text> is past the range of the <Kind> type')
    .Given('an option declared with each integer width',
      procedure(W: TTypesWorld)
      begin
        W.BuildNumbers;
      end)
    .When('the invocation passes <Text> to the <Kind> one',
      procedure(W: TTypesWorld)
      begin
        W.Parsed := W.Numbers.Parse(['--' + W.Kind, W.Text]);
      end)
    .&Then('the parse fails as a value that does not convert: <Expected>',
      procedure(W: TTypesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual(W.Expected);
      end)
    .Examples(
      [['Kind',     'Text',                 'Expected'],
       ['shortint', '128',                  '"128" is not a valid integer for --shortint'],
       ['byte',     '256',                  '"256" is not a valid unsigned integer for --byte'],
       ['smallint', '-32769',               '"-32769" is not a valid integer for --smallint'],
       ['word',     '65536',                '"65536" is not a valid unsigned integer for --word'],
       ['cardinal', '4294967296',           '"4294967296" is not a valid unsigned integer for --cardinal'],
       ['uint64',   '18446744073709551616', '"18446744073709551616" is not a valid unsigned integer for --uint64'],
       ['uint64',   '-1',                   '"-1" is not a valid unsigned integer for --uint64']])

  .Scenario('A simple real too large for its type')
    .Given('an option declared as a simple real',
      procedure(W: TTypesWorld)
      begin
        W.BuildNumbers;
      end)
    .When('the invocation passes it a magnitude only a double real holds',
      procedure(W: TTypesWorld)
      begin
        W.Parsed := W.Numbers.Parse(['--single', '1e39']);
        W.ReadSingle := W.SingleOpt.ValueIn(W.Parsed);
      end)
    .&Then('it is read as infinity, as .NET reads it',
      procedure(W: TTypesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(FloatToStr(W.ReadSingle, TFormatSettings.Invariant)).ToEqual('INF');
      end)

.Rule('a date without time and a time are read in ISO 8601, and a time span in .NET''s format')

  .Scenario('A date without time')
    .Given('an option declared as a date without time',
      procedure(W: TTypesWorld)
      begin
        W.BuildTimes;
      end)
    .When('the invocation passes 2026-10-05',
      procedure(W: TTypesWorld)
      begin
        W.Parsed := W.Times.Parse(['--day', '2026-10-05']);
        W.ReadDay := W.DayOpt.ValueIn(W.Parsed);
      end)
    .&Then('that date is read, with no time part',
      procedure(W: TTypesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', W.ReadDay, TFormatSettings.Invariant)).ToEqual('2026-10-05 00:00:00.000');
      end)

  .Scenario('A date without time that carries one')
    .Given('an option declared as a date without time',
      procedure(W: TTypesWorld)
      begin
        W.BuildTimes;
      end)
    .When('the invocation passes a date with a time behind it',
      procedure(W: TTypesWorld)
      begin
        W.Parsed := W.Times.Parse(['--day', '2026-10-05T10:00']);
      end)
    .&Then('the parse fails, rather than dropping the time',
      procedure(W: TTypesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('"2026-10-05T10:00" is not a valid date for --day');
      end)

  .Scenario('A time in ISO 8601')
    .Given('an option declared as a time',
      procedure(W: TTypesWorld)
      begin
        W.BuildTimes;
      end)
    .When('the invocation passes 10:30:15.5',
      procedure(W: TTypesWorld)
      begin
        W.Parsed := W.Times.Parse(['--at', '10:30:15.5']);
        W.ReadAt := W.AtOpt.ValueIn(W.Parsed);
      end)
    .&Then('that time is read, to the millisecond',
      procedure(W: TTypesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(FormatDateTime('hh:nn:ss.zzz', W.ReadAt, TFormatSettings.Invariant)).ToEqual('10:30:15.500');
      end)

  .Scenario('A time written some other way')
    .Given('an option declared as a time',
      procedure(W: TTypesWorld)
      begin
        W.BuildTimes;
      end)
    .When('the invocation writes it in a local convention instead of ISO 8601',
      procedure(W: TTypesWorld)
      begin
        W.Parsed := W.Times.Parse(['--at', '10:30 PM']);
      end)
    .&Then('the parse fails naming the option and the text received',
      procedure(W: TTypesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('"10:30 PM" is not a valid time for --at');
      end)

  .Scenario('A time span')
    .Given('an option declared as a time span',
      procedure(W: TTypesWorld)
      begin
        W.BuildTimes;
      end)
    .When('the invocation passes 1.02:03:04',
      procedure(W: TTypesWorld)
      begin
        W.Parsed := W.Times.Parse(['--wait', '1.02:03:04']);
        W.ReadWait := W.WaitOpt.ValueIn(W.Parsed);
      end)
    .&Then('it reads one day, two hours, three minutes and four seconds',
      procedure(W: TTypesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(Format('%d d %d h %d m %d s', [W.ReadWait.Days, W.ReadWait.Hours, W.ReadWait.Minutes,
          W.ReadWait.Seconds])).ToEqual('1 d 2 h 3 m 4 s');
      end)

.Rule('a value that may be missing stays empty when the invocation does not bring it')

  .Scenario('A value that may be missing and does not come')
    .Given('an option declared as an integer that may be missing',
      procedure(W: TTypesWorld)
      begin
        W.BuildMissing;
      end)
    .When('an invocation that does not name it is parsed',
      procedure(W: TTypesWorld)
      begin
        W.Parsed := W.Missing.Parse([]);
        W.ReadLimit := W.LimitOpt.ValueIn(W.Parsed);
      end)
    .&Then('its value is empty, and not 0',
      procedure(W: TTypesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(W.ReadLimit.HasValue).ToBeFalse;
      end)

  .Scenario('A value that may be missing and comes')
    .Given('an option declared as an integer that may be missing',
      procedure(W: TTypesWorld)
      begin
        W.BuildMissing;
      end)
    .When('the invocation passes it 5',
      procedure(W: TTypesWorld)
      begin
        W.Parsed := W.Missing.Parse(['--limit', '5']);
        W.ReadLimit := W.LimitOpt.ValueIn(W.Parsed);
      end)
    .&Then('it holds 5',
      procedure(W: TTypesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(Format('has value=%s, value=%d', [BoolToStr(W.ReadLimit.HasValue, True), W.ReadLimit.Value]))
          .ToEqual('has value=True, value=5');
      end)

  .Scenario('A boolean that may be missing, named alone')
    .Given('an option declared as a boolean that may be missing',
      procedure(W: TTypesWorld)
      begin
        W.BuildMissing;
      end)
    .When('the invocation names it with no value',
      procedure(W: TTypesWorld)
      begin
        W.Parsed := W.Missing.Parse(['--dry']);
        W.ReadDry := W.DryOpt.ValueIn(W.Parsed);
      end)
    .&Then('it is a flag, as .NET''s bool? is, and holds true',
      procedure(W: TTypesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(Format('has value=%s, value=%s', [BoolToStr(W.ReadDry.HasValue, True), BoolToStr(W.ReadDry.Value,
          True)])).ToEqual('has value=True, value=True');
      end)

  .Scenario('A record shaped like a value that may be missing, with another name')
    .Given('a record of the application with the two fields of a value that may be missing').NoAction
    .When('an option is declared with it, with no conversion, and added to a command',
      procedure(W: TTypesWorld)
      begin
        try
          var Maybe := CommandLine.Option<TMaybe<Integer>>('--maybe').Description('Perhaps');
          W.Report := 'declared';
          var Root := CommandLine.Root('tool', 'A tool');
          Root.Add(Maybe);
          W.Report := W.Report + ', <added>';
        except
          on E: Exception do
            W.Report := W.Report + ', ' + E.ClassName + ': ' + E.Message;
        end;
      end)
    .&Then('the engine does not take it for one, and refuses it naming the type',
      procedure(W: TTypesWorld)
      begin
        Expect(W.Report).ToStartWith('declared, ECommandLine: ');
        Expect(W.Report).ToContain('TMaybe');
      end)

.Rule('a path type says what has to exist')

  .Scenario('A directory path that has to exist')
    .Given('an option declared as a directory path, to accept only what exists',
      procedure(W: TTypesWorld)
      begin
        W.BuildPaths;
      end)
    .When('the invocation passes the path of a file that exists',
      procedure(W: TTypesWorld)
      begin
        // The running executable, which exists by definition.
        W.Parsed := W.Paths.Parse(['--target', ParamStr(0)]);
      end)
    .&Then('the parse fails, because what exists is not a directory',
      procedure(W: TTypesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual(Format('"%s" is not a directory that exists, and --target only accepts one',
          [ParamStr(0)]));
      end)

  .Scenario('A file path that has to exist takes a directory too')
    .Given('an option declared as a file path, to accept only what exists',
      procedure(W: TTypesWorld)
      begin
        W.BuildPaths;
      end)
    .When('the invocation passes the path of a directory that exists',
      procedure(W: TTypesWorld)
      begin
        W.Parsed := W.Paths.Parse(['--source', ExtractFileDir(ParamStr(0))]);
        W.ReadSource := W.SourceOpt.ValueIn(W.Parsed);
      end)
    .&Then('it is read, as .NET''s file type reads it',
      procedure(W: TTypesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(string(W.ReadSource)).ToEqual(ExtractFileDir(ParamStr(0)));
      end)

  .Scenario('A file path that does not exist')
    .Given('an option declared as a file path, to accept only what exists',
      procedure(W: TTypesWorld)
      begin
        W.BuildPaths;
      end)
    .When('the invocation passes a path that does not exist',
      procedure(W: TTypesWorld)
      begin
        W.Parsed := W.Paths.Parse(['--source', 'no-such-file-anywhere.txt']);
      end)
    .&Then('the parse fails naming the path, the option and that it wanted a file',
      procedure(W: TTypesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual(
          '"no-such-file-anywhere.txt" does not exist, and --source only accepts a file that does');
      end)

.Rule('a set of an enumerated type is written with the names of its members')

  .Scenario('A set of an enumerated type')
    .Given('an option declared as a set of an enumerated type with the values A, B and C',
      procedure(W: TTypesWorld)
      begin
        W.BuildSets;
      end)
    .When('the invocation passes A,C',
      procedure(W: TTypesWorld)
      begin
        W.Parsed := W.Sets.Parse(['--letters', 'A,C']);
        W.ReadLetters := W.LettersOpt.ValueIn(W.Parsed);
      end)
    .&Then('the set with A and C is read',
      procedure(W: TTypesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('');
        Expect(LettersText(W.ReadLetters)).ToEqual('A,C');
      end)

  .Scenario('A set with a name its type does not have')
    .Given('an option declared as a set of an enumerated type with the values A, B and C',
      procedure(W: TTypesWorld)
      begin
        W.BuildSets;
      end)
    .When('the invocation passes A,X',
      procedure(W: TTypesWorld)
      begin
        W.Parsed := W.Sets.Parse(['--letters', 'A,X']);
      end)
    .&Then('the parse fails listing the names it accepts',
      procedure(W: TTypesWorld)
      begin
        Expect(W.Parsed.ErrorText).ToEqual('"A,X" is not a valid value for --letters. Accepted: A, B, C');
      end)

.Rule('a symbol says the type its tokens are converted to')

  .Scenario('The type a symbol converts to')
    .Given('an option declared as a set of an enumerated type',
      procedure(W: TTypesWorld)
      begin
        W.BuildSets;
      end)
    .When('its value type is read',
      procedure(W: TTypesWorld)
      begin
        W.Report := string(W.LettersOpt.ValueType.Name);
      end)
    .&Then('it is the declared type',
      procedure(W: TTypesWorld)
      begin
        Expect(W.Report).ToEqual('TLetters');
      end);

end.
