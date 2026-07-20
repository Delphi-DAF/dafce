unit Daf.MiniSpec.Reporter.Console;

interface

uses
  System.RegularExpressions,
  Daf.MiniSpec.Types,
  Daf.MiniSpec.DataTable,
  Daf.MiniSpec.Runner;

type
  /// <summary>
  /// Console reporter — progressive output: Feature/Scenario as headers,
  /// steps with Gherkin-aligned status symbols emitted immediately after execution.
  /// </summary>
  TConsoleReporter = class(TCustomListener)
  private
    FCurrentRule: IRule;
    procedure OutputLn(const Level: Byte; const Text: string); overload;
    procedure Output(const Level: Byte; const Text: string);
    procedure OutputDataTable(const Level: Byte; const Table: TDataTableObj);
    function ExtractValue(const Match: TMatch): string;
    function Level2Margin(const Level: Byte): string;
    function ResultSymbol(const AResult: TSpecRunResult): string;
    procedure OutputStep(const Context: IRunContext; const Item: ISpecItem);
  public
    function UseConsole: Boolean; override;
    procedure OnBeginSuite(const Context: IRunContext; const Suite: ISpecSuite); override;
    procedure OnBeginReport(const Context: IRunContext); override;
    procedure OnEndReport(const Context: IRunContext); override;
    procedure OnBeginFeature(const Context: IRunContext; const Feature: IFeature); override;
    procedure OnBeginScenario(const Context: IRunContext; const Scenario: IScenario); override;
    procedure OnBeginOutline(const Context: IRunContext; const Outline: IScenarioOutline); override;
    procedure OnItem(const Context: IRunContext; const Item: ISpecItem); override;
    procedure OnEndOutline(const Context: IRunContext; const Outline: IScenarioOutline); override;
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  System.Rtti;

const
  CHECK_MARK  = #$2713;  // ✓
  CROSS_MARK  = #$2717;  // ✗
  TILDE       = '~';
  PENDING_SYM = 'P';
  UNDEF_SYM   = '?';

{ TConsoleReporter }

function TConsoleReporter.UseConsole: Boolean;
begin
  Result := True;
end;

function TConsoleReporter.ExtractValue(const Match: TMatch): string;
begin
  Result := Match.Groups[1].Value;
end;

function TConsoleReporter.Level2Margin(const Level: Byte): string;
begin
  Result := DupeString(' ', 2 * Level);
end;

function TConsoleReporter.ResultSymbol(const AResult: TSpecRunResult): string;
begin
  case AResult of
    srrSuccess:   Result := CHECK_MARK;
    srrFail:      Result := CROSS_MARK;
    srrError:     Result := CROSS_MARK;
    srrSkipped:   Result := TILDE;
    srrPending:   Result := PENDING_SYM;
    srrUndefined: Result := UNDEF_SYM;
  else
    Result := '?';
  end;
end;

procedure TConsoleReporter.Output(const Level: Byte; const Text: string);
var
  Margin, OutputText: string;
  Regex: TRegEx;
begin
  Margin := Level2Margin(Level);
  Regex := TRegEx.Create('#\{([^\}]+)\}');
  OutputText := Regex.Replace(Text, ExtractValue);
  if Level = 0 then
    Margin := SLineBreak + Margin;
  Write(Margin + OutputText);
end;

procedure TConsoleReporter.OutputLn(const Level: Byte; const Text: string);
begin
  Output(Level, Text + SLineBreak);
end;

procedure TConsoleReporter.OutputDataTable(const Level: Byte; const Table: TDataTableObj);
var
  ColWidths: TArray<Integer>;
  Row: TArray<TValue>;
  I, J: Integer;
  Line: string;
begin
  if Table = nil then Exit;

  SetLength(ColWidths, Table.ColCount);
  for I := 0 to Table.ColCount - 1 do
    ColWidths[I] := 0;

  for I := 0 to High(Table.Headers) do
    if Length(Table.Headers[I]) > ColWidths[I] then
      ColWidths[I] := Length(Table.Headers[I]);

  for Row in Table.Rows do
    for J := 0 to High(Row) do
      if Row[J].ToString.Length > ColWidths[J] then
        ColWidths[J] := Row[J].ToString.Length;

  Line := '| ';
  for I := 0 to High(Table.Headers) do
    Line := Line + Format('%-*s | ', [ColWidths[I], Table.Headers[I]]);
  OutputLn(Level, Line);

  for Row in Table.Rows do
  begin
    Line := '| ';
    for J := 0 to High(Row) do
      Line := Line + Format('%-*s | ', [ColWidths[J], Row[J].ToString]);
    OutputLn(Level, Line);
  end;
end;

procedure TConsoleReporter.OutputStep(const Context: IRunContext; const Item: ISpecItem);
var
  Level: Byte;
  Symbol, ErrMsg, Line: string;
  Step: IScenarioStep;
begin
  Level := Item.Level;
  if Assigned(FCurrentRule) and not (Item.Kind in [sikFeature, sikRule]) then
    Inc(Level);

  Symbol := ResultSymbol(Item.RunInfo.Result);

  ErrMsg := '';
  if Item.RunInfo.Result in [srrFail, srrError] then
    ErrMsg := Context.GetErrorDetail(Item.RunInfo);

  if not ErrMsg.IsEmpty then
    Line := Format('%s %s %s (%d ms)%s%s  %s',
      [Symbol, Item.KeyWord, Item.Description, Item.RunInfo.ExecTimeMs,
       SLineBreak, Level2Margin(Level), 'ERROR: "' + ErrMsg + '"'])
  else
    Line := Format('%s %s %s (%d ms)',
      [Symbol, Item.KeyWord, Item.Description, Item.RunInfo.ExecTimeMs]);

  OutputLn(Level, Line);

  if Supports(Item, IScenarioStep, Step) and Assigned(Step.DataTable) then
    OutputDataTable(Level + 1, Step.DataTable);
end;

procedure TConsoleReporter.OnBeginSuite(const Context: IRunContext; const Suite: ISpecSuite);
begin
  if not Suite.Title.IsEmpty then
    OutputLn(0, 'Suite: ' + Suite.Title);
end;

procedure TConsoleReporter.OnBeginReport(const Context: IRunContext);
begin
end;

procedure TConsoleReporter.OnEndReport(const Context: IRunContext);
begin
end;

procedure TConsoleReporter.OnBeginFeature(const Context: IRunContext; const Feature: IFeature);
begin
  FCurrentRule := nil;
  OutputLn(0, 'Feature: ' + Feature.Title);
  if not Feature.Narrative.IsEmpty then
    for var Line in Feature.Narrative.Split([#13, #10]) do
      if Line.Trim <> '' then
        OutputLn(1, Line.Trim);
end;

procedure TConsoleReporter.OnBeginScenario(const Context: IRunContext; const Scenario: IScenario);
var
  Level: Byte;
begin
  Level := 1;
  if Assigned(FCurrentRule) then
    Inc(Level);
  OutputLn(Level, Scenario.KeyWord + ': ' + Scenario.Description);
end;

procedure TConsoleReporter.OnBeginOutline(const Context: IRunContext; const Outline: IScenarioOutline);
begin
  // Outline header printed in OnEndOutline after all examples are known
end;

procedure TConsoleReporter.OnItem(const Context: IRunContext; const Item: ISpecItem);
var
  Rule: IRule;
begin
  // Track current rule for indentation
  if Supports(Item, IRule, Rule) and (Rule.Kind = sikRule) then
  begin
    FCurrentRule := Rule;
    OutputLn(1, 'Rule: ' + Item.Description);
    Exit;
  end;

  // Feature and Scenario headers already printed in OnBegin* hooks — skip here
  if Item.Kind in [sikFeature, sikScenario, sikScenarioOutline] then
    Exit;

  // Background header
  if Item.Kind = sikBackground then
  begin
    var Level: Byte := 1;
    if Assigned(FCurrentRule) then Inc(Level);
    OutputLn(Level, 'Background: ' + Item.Description);
    Exit;
  end;

  // Steps (Given/When/Then/And/But) and background steps
  if Item.Kind in [sikGiven, sikWhen, sikThen, sikAnd, sikBut] then
  begin
    OutputStep(Context, Item);
    Exit;
  end;
end;

procedure TConsoleReporter.OnEndOutline(const Context: IRunContext; const Outline: IScenarioOutline);
var
  TotalTime: Int64;
  ColWidths: TArray<Integer>;
  Headers: TArray<string>;
  i: Integer;
  HeaderLine, Row: string;
  Values: TArray<TValue>;
  BaseLevel: Byte;
begin
  BaseLevel := 1;
  if Assigned(FCurrentRule) then
    Inc(BaseLevel);

  TotalTime := 0;
  for var Example in Outline.Examples do
    if Example.RunInfo.State = srsFinished then
      TotalTime := TotalTime + Example.RunInfo.ExecTimeMs;

  OutputLn(BaseLevel, 'Scenario Outline: ' + Outline.Description);

  // Steps template
  for var Step in Outline.StepsGiven do
    OutputLn(BaseLevel + 1, Step.KeyWord + ' ' + Step.Description);
  for var Step in Outline.StepsWhen do
    OutputLn(BaseLevel + 1, Step.KeyWord + ' ' + Step.Description);
  for var Step in Outline.StepsThen do
    OutputLn(BaseLevel + 1, Step.KeyWord + ' ' + Step.Description);

  // Calculate column widths
  Headers := Outline.Headers;
  SetLength(ColWidths, Length(Headers));
  for i := 0 to High(Headers) do
    ColWidths[i] := Length(Headers[i]);

  for var Example in Outline.Examples do
  begin
    Values := Example.ExampleMeta.Values;
    for i := 0 to High(Values) do
      if (i <= High(ColWidths)) and (Length(Val2Str(Values[i])) > ColWidths[i]) then
        ColWidths[i] := Length(Val2Str(Values[i]));
  end;

  // Examples table
  OutputLn(BaseLevel + 1, 'Examples:');

  HeaderLine := '|';
  for i := 0 to High(Headers) do
    HeaderLine := HeaderLine + ' ' + Headers[i].PadRight(ColWidths[i]) + ' |';
  OutputLn(BaseLevel + 2, '  ' + HeaderLine);

  for var Example in Outline.Examples do
  begin
    if Example.RunInfo.State = srsFinished then
    begin
      Values := Example.ExampleMeta.Values;
      Row := '|';
      for i := 0 to High(Headers) do
      begin
        if i <= High(Values) then
          Row := Row + ' ' + Val2Str(Values[i]).PadRight(ColWidths[i]) + ' |'
        else
          Row := Row + ' ' + ''.PadRight(ColWidths[i]) + ' |';
      end;
      var Symbol := ResultSymbol(Example.RunInfo.Result);
      OutputLn(BaseLevel + 2, Symbol + ' ' + Row + Format(' (%d ms)', [Example.RunInfo.ExecTimeMs]));
    end;
  end;
end;

end.
