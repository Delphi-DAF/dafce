unit Daf.MiniSpec.Reporter.Console;

interface

uses
  System.RegularExpressions,
  Daf.MiniSpec.Types,
  Daf.MiniSpec.Reporter;

type
  /// <summary>
  /// Console reporter - outputs test results to console in hierarchical format.
  /// Implements ISpecListener for pure observer pattern.
  /// </summary>
  TConsoleReporter = class(TCustomListener)
  private
    FCurrentRule: IRule;
    procedure OutputLn(const Level: Byte; const Text: string; const Success: Boolean; const Duration: Integer; const ErrorMessage: string = '');overload;
    procedure OutputLn(const Level: Byte; const Text: string);overload;
    procedure Output(const Level: Byte; const Text: string);
    function ExtractValue(const Match: TMatch): string;
    function Level2Margin(const Level: Byte): string;
  public
    function UseConsole: Boolean;override;
    procedure OnBeginReport(const Context: IRunContext);override;
    procedure OnEndReport(const Context: IRunContext);override;
    procedure OnItem(const Context: IRunContext; const Item: ISpecItem);override;
    procedure OnEndOutline(const Context: IRunContext; const Outline: IScenarioOutline; const Counters: TSpecCounters);override;
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  System.Rtti;

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

procedure TConsoleReporter.OutputLn(const Level: Byte; const Text: string; const Success: Boolean; const Duration: Integer; const ErrorMessage: string);
const
  CHECK_MARK = #$2713;  // ✓
  CROSS_MARK = #$2717;  // ✗
var
  Msg: string;
begin
  Msg := ErrorMessage;
  if not Msg.IsEmpty then
    Msg := SLineBreak + Level2Margin(Level) + 'ERROR: "' + Msg + '"';
  if Success then
    OutputLn(Level, Format(CHECK_MARK + ' %s (%d ms)', [Text, Duration]))
  else
    OutputLn(Level, Format(CROSS_MARK + ' %s (%d ms)%s', [Text, Duration, Msg]));
end;

procedure TConsoleReporter.OnBeginReport(const Context: IRunContext);
begin
  // Nothing to initialize for console output
end;

procedure TConsoleReporter.OnEndReport(const Context: IRunContext);
begin
  // Summary is printed by TReportSummaryWriter decorator, not here
end;

procedure TConsoleReporter.OnItem(const Context: IRunContext; const Item: ISpecItem);
var
  Feat: IFeature;
  Rule: IRule;
  DisplayText: string;
  AllSkipped: Boolean;
  Kind: string;
  Level: Byte;
begin
  Kind := Item.KeyWord;
  Level := Item.Level;

  // Track current rule for indentation
  if Supports(Item, IRule, Rule) then
  begin
    if Rule.Kind = sikRule then
      FCurrentRule := Rule;
  end;

  // Adjust level if inside an explicit Rule
  if Assigned(FCurrentRule) and not (Item.Kind in [sikFeature, sikRule]) then
    Inc(Level);

  // For features, show only the Title
  if (Item.Kind = sikFeature) and Supports(Item, IFeature, Feat) then
  begin
    DisplayText := Feat.Title;
    // Check if all scenarios were skipped
    AllSkipped := True;
    for var R in Feat.Rules do
      for var Scenario in R.Scenarios do
        if Scenario.RunInfo.State = srsFinished then
        begin
          AllSkipped := False;
          Break;
        end;
    if AllSkipped then
    begin
      OutputLn(Level, Format('- %s (skip)', [Kind + ' ' + DisplayText]));
      Exit;
    end;
  end
  else
    DisplayText := Item.Description;

  // Handle 3 states: Finished (Pass/Fail), Skipped
  if Item.RunInfo.State = srsSkiped then
    OutputLn(Level, Format('- %s (skip)', [Kind + ' ' + DisplayText]))
  else
    OutputLn(Level, Kind + ' ' + DisplayText, Item.RunInfo.IsSuccess, Item.RunInfo.ExecTimeMs, Context.GetErrorDetail(Item.RunInfo));

  // Clear rule context when rule ends
  if (Item.Kind = sikRule) and Assigned(FCurrentRule) then
  begin
    // Rule end will be triggered separately; for now just keep tracking
  end;
end;

procedure TConsoleReporter.OnEndOutline(const Context: IRunContext; const Outline: IScenarioOutline; const Counters: TSpecCounters);
var
  AllSuccess, AllSkipped: Boolean;
  TotalTime: Int64;
  ColWidths: TArray<Integer>;
  Headers: TArray<string>;
  i: Integer;
  HeaderLine, Row: string;
  Values: TArray<TValue>;
  BaseLevel: Byte;
begin
  // Calculate base level (adjusted if inside a Rule)
  BaseLevel := 1;
  if Assigned(FCurrentRule) then
    Inc(BaseLevel);

  // Determine overall Outline state
  AllSuccess := True;
  AllSkipped := True;
  TotalTime := 0;
  for var Example in Outline.Examples do
  begin
    if Example.RunInfo.State = srsFinished then
    begin
      AllSkipped := False;
      TotalTime := TotalTime + Example.RunInfo.ExecTimeMs;
      if not Example.RunInfo.IsSuccess then
        AllSuccess := False;
    end;
  end;

  // Header of Scenario Outline with result or skip
  if AllSkipped then
    OutputLn(BaseLevel, Format('- Scenario Outline: %s (skip)', [Outline.Description]))
  else
    OutputLn(BaseLevel, 'Scenario Outline: ' + Outline.Description, AllSuccess, TotalTime);

  // If all skipped, don't show details
  if AllSkipped then
    Exit;

  // Steps template (without individual time)
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

  // Table header (3 spaces to align with emoji)
  HeaderLine := '|';
  for i := 0 to High(Headers) do
    HeaderLine := HeaderLine + ' ' + Headers[i].PadRight(ColWidths[i]) + ' |';
  OutputLn(BaseLevel + 2, '   ' + HeaderLine);

  // Each row with its result
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
      OutputLn(BaseLevel + 2, Row, Example.RunInfo.IsSuccess, Example.RunInfo.ExecTimeMs, Context.GetErrorDetail(Example.RunInfo));
    end;
  end;
end;

end.
