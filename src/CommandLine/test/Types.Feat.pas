unit Types.Feat;

interface

uses
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

    Parsed: IParseResult;
    Report: string;
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
  end;

implementation

uses
  System.SysUtils,
  System.Generics.Collections,
  Daf.MiniSpec;

{ TTypesWorld }

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
      end);

end.
