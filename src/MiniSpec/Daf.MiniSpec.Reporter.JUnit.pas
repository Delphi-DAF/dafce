unit Daf.MiniSpec.Reporter.JUnit;

interface

uses
  System.Classes,
  System.SysUtils,
  Daf.MiniSpec.Types,
  Daf.MiniSpec.Runner;

type
  /// <summary>
  /// JUnit XML reporter - generates JUnit XML format for CI/CD integration.
  /// Supports GitHub Actions, GitLab CI, Jenkins, Azure DevOps, etc.
  /// Uses RunInfo from Context for all counts - no internal counters.
  /// </summary>
  TJUnitReporter = class(TCustomListener)
  private
    FOutput: string;
    function BuildXml(const Context: IRunContext): string;
  public
    function GetContent: string; override;
    function GetFileExt: string; override;
    function ShowHelp: Boolean; override;
    procedure OnEndReport(const Context: IRunContext); override;
  end;

implementation

uses
  System.IOUtils,
  System.Generics.Collections;

type
  /// <summary>What a block lists: its attributes count the cases it carries.</summary>
  TCaseCounts = record
    Tests: Integer;
    Failures: Integer;
    Skipped: Integer;
    procedure Add(const Other: TCaseCounts);
  end;

var
  /// Format settings with dot as decimal separator for XML compatibility
  XmlFormatSettings: TFormatSettings;

procedure TCaseCounts.Add(const Other: TCaseCounts);
begin
  Inc(Tests, Other.Tests);
  Inc(Failures, Other.Failures);
  Inc(Skipped, Other.Skipped);
end;

function EscapeXml(const S: string): string;
begin
  Result := S;
  Result := StringReplace(Result, '&', '&amp;', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
  Result := StringReplace(Result, '''', '&apos;', [rfReplaceAll]);
end;

function TimeToXml(const Ms: Int64): string;
begin
  // Format time in seconds with dot as decimal separator (XML/JUnit standard)
  Result := FormatFloat('0.000', Ms / 1000.0, XmlFormatSettings);
end;

function FirstStepWith(const Steps: TList<IScenarioStep>; Outcome: TSpecRunResult): IScenarioStep;
begin
  Result := nil;
  if Assigned(Steps) then
    for var Step in Steps do
      if Step.RunInfo.Result = Outcome then
        Exit(Step);
end;

// The step that left the scenario Pending or Undefined: its own first, then those of its Background.
function DecidingStep(const Scenario: IScenario; Outcome: TSpecRunResult): IScenarioStep;
begin
  Result := FirstStepWith(Scenario.StepsGiven, Outcome);
  if not Assigned(Result) then
    Result := FirstStepWith(Scenario.StepsWhen, Outcome);
  if not Assigned(Result) then
    Result := FirstStepWith(Scenario.StepsThen, Outcome);
  if Assigned(Result) then
    Exit;
  var Owner := Scenario.Parent;
  if Supports(Owner, IScenarioOutline) then
    Owner := Owner.Parent;
  var Rule: IRule;
  if not Supports(Owner, IRule, Rule) then
    Exit;
  if Assigned(Rule.Feature) and Assigned(Rule.Feature.BackGround) then
    Result := FirstStepWith(Rule.Feature.BackGround.StepsGiven, Outcome);
  if not Assigned(Result) and Assigned(Rule.BackGround) then
    Result := FirstStepWith(Rule.BackGround.StepsGiven, Outcome);
end;

function OutcomeMessage(const Scenario: IScenario; Outcome: TSpecRunResult): string;
begin
  if Outcome = srrPending then
    Result := 'Pending'
  else
    Result := 'Undefined';
  var Step := DecidingStep(Scenario, Outcome);
  if Assigned(Step) then
    Result := Result + ' step: ' + Step.KeyWord + ' ' + Step.Description;
end;

function ClosedCase(const Open, Child: string): string;
begin
  Result := Open + '>' + sLineBreak + '      ' + Child + sLineBreak + '    </testcase>' + sLineBreak;
end;

// The case of a scenario or of a row: Fail, Error and Undefined give a failure, Pending a skipped.
// What did not run is not listed.
function TestCase(const Context: IRunContext; const Scenario: IScenario; const Name, ClassName: string;
  var Counts: TCaseCounts): string;
begin
  Result := '';
  var RunInfo := (Scenario as ISpecItem).RunInfo;
  if RunInfo.State <> srsFinished then
    Exit;
  Inc(Counts.Tests);
  var Open := Format('    <testcase name="%s" classname="%s" time="%s"',
    [EscapeXml(Name), EscapeXml(ClassName), TimeToXml(RunInfo.ExecTimeMs)]);
  case RunInfo.Result of
    srrFail, srrError:
      begin
        Inc(Counts.Failures);
        Result := ClosedCase(Open, Format('<failure message="%s" type="AssertionError"/>',
          [EscapeXml(ScenarioFailure(Context, Scenario))]));
      end;
    srrUndefined:
      begin
        Inc(Counts.Failures);
        Result := ClosedCase(Open, Format('<failure message="%s" type="Undefined"/>',
          [EscapeXml(OutcomeMessage(Scenario, srrUndefined))]));
      end;
    srrPending:
      begin
        Inc(Counts.Skipped);
        Result := ClosedCase(Open, Format('<skipped message="%s"/>', [EscapeXml(OutcomeMessage(Scenario, srrPending))]));
      end;
  else
    Result := Open + '/>' + sLineBreak;
  end;
end;

// Each hook that failed is a failed case with its description and its message. One that passed is
// not listed.
function HookCases(const Hooks: TList<IHook>; const ClassName: string; var Counts: TCaseCounts): string;
begin
  Result := '';
  for var Hook in Hooks do
    if Hook.RunInfo.Result in [srrFail, srrError] then
    begin
      Inc(Counts.Tests);
      Inc(Counts.Failures);
      Result := Result + ClosedCase(Format('    <testcase name="%s" classname="%s" time="%s"',
        [EscapeXml(Hook.Description), EscapeXml(ClassName), TimeToXml(Hook.RunInfo.ExecTimeMs)]),
        Format('<failure message="%s" type="HookError"/>', [EscapeXml(Hook.RunInfo.ErrMsg)]));
    end;
end;

function FeatureCases(const Context: IRunContext; const Feature: IFeature; var Counts: TCaseCounts): string;
begin
  Result := HookCases(Feature.BeforeHooks, Feature.Title, Counts);
  for var Rule in Feature.Rules do
    for var Scenario in Rule.Scenarios do
    begin
      var Outline: IScenarioOutline;
      if Supports(Scenario, IScenarioOutline, Outline) and (Outline.Examples.Count > 0) then
        for var I := 0 to Outline.Examples.Count - 1 do
          Result := Result + TestCase(Context, Outline.Examples[I],
            Format('%s [Example %d]', [Outline.Description, I + 1]), Feature.Title, Counts)
      else
        Result := Result + TestCase(Context, Scenario, Scenario.Description, Feature.Title, Counts);
    end;
  Result := Result + HookCases(Feature.AfterHooks, Feature.Title, Counts);
end;

function TestSuite(const Name, Cases: string; const Counts: TCaseCounts; const TimeMs: Int64): string;
begin
  Result := Format('  <testsuite name="%s" tests="%d" failures="%d" errors="0" skipped="%d" time="%s">',
    [EscapeXml(Name), Counts.Tests, Counts.Failures, Counts.Skipped, TimeToXml(TimeMs)]) + sLineBreak +
    Cases + '  </testsuite>' + sLineBreak;
end;

{ TJUnitReporter }

function TJUnitReporter.ShowHelp: Boolean;
begin
  WriteLn('JUnit Reporter - JUnit XML format for CI/CD integration');
  WriteLn;
  WriteLn('Options:');
  WriteLn('  output=<file>   Output file path (default: minispec-results.xml)');
  WriteLn;
  WriteLn('Examples:');
  WriteLn('  -r junit                       Output to minispec-results.xml');
  WriteLn('  -r junit:output=results.xml    Custom output file');
  WriteLn('  -r junit:output=test-results/report.xml');
  WriteLn;
  WriteLn('CI/CD Integration:');
  WriteLn('  GitHub Actions: uses: dorny/test-reporter@v1');
  WriteLn('  GitLab CI:      artifacts:reports:junit: results.xml');
  WriteLn('  Jenkins:        junit ''**/results.xml''');
  WriteLn('  Azure DevOps:   - task: PublishTestResults@2');
  Result := True;
end;

function TJUnitReporter.BuildXml(const Context: IRunContext): string;
var
  SB: TStringBuilder;
begin
  var Total := Default(TCaseCounts);
  var Blocks := '';
  // The hooks of the suite get a block of their own, which exists only when one of them failed.
  var SuiteName := Context.Suite.Title;
  if SuiteName.IsEmpty then
    SuiteName := 'MiniSpec';
  var SuiteCounts := Default(TCaseCounts);
  var SuiteCases := HookCases(Context.Suite.BeforeHooks, SuiteName, SuiteCounts) +
    HookCases(Context.Suite.AfterHooks, SuiteName, SuiteCounts);
  if SuiteCounts.Tests > 0 then
  begin
    Blocks := Blocks + TestSuite(SuiteName, SuiteCases, SuiteCounts, Context.Suite.RunInfo.ExecTimeMs);
    Total.Add(SuiteCounts);
  end;
  // A feature with no case listed is left out.
  for var Feature in Context.Suite.Features do
  begin
    var Counts := Default(TCaseCounts);
    var Cases := FeatureCases(Context, Feature, Counts);
    if Counts.Tests > 0 then
    begin
      Blocks := Blocks + TestSuite(Feature.Title, Cases, Counts, (Feature as ISpecItem).RunInfo.ExecTimeMs);
      Total.Add(Counts);
    end;
  end;

  SB := TStringBuilder.Create;
  try
    SB.AppendLine('<?xml version="1.0" encoding="UTF-8"?>');
    SB.AppendFormat('<testsuites name="MiniSpec" tests="%d" failures="%d" errors="0" skipped="%d" time="%s">',
      [Total.Tests, Total.Failures, Total.Skipped, TimeToXml(Context.Suite.RunInfo.ExecTimeMs)]);
    SB.AppendLine;
    SB.Append(Blocks);
    SB.AppendLine('</testsuites>');
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

procedure TJUnitReporter.OnEndReport(const Context: IRunContext);
var
  OutputFile: string;
begin
  FOutput := BuildXml(Context);

  // Write to file
  OutputFile := GetCliOption('output', 'minispec-results.xml');
  if TPath.IsRelativePath(OutputFile) then
    OutputFile := ExpandFileName(TPath.Combine(ExtractFilePath(ParamStr(0)), OutputFile));

  TFile.WriteAllText(OutputFile, FOutput, TEncoding.UTF8);
  WriteLn('JUnit XML report: file:///' + StringReplace(OutputFile, '\', '/', [rfReplaceAll]));
end;

function TJUnitReporter.GetContent: string;
begin
  Result := FOutput;
end;

function TJUnitReporter.GetFileExt: string;
begin
  Result := 'xml';
end;

initialization
  // Initialize format settings with dot as decimal separator for XML compatibility
  XmlFormatSettings := TFormatSettings.Create('en-US');
  XmlFormatSettings.DecimalSeparator := '.';

end.
