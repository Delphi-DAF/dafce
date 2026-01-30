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
    function EscapeXml(const S: string): string;
    function TimeToXml(const Ms: Int64): string;
    function BuildTestCase(const Scenario: IScenario; const ClassName: string): string;
    function BuildTestSuite(const Feature: IFeature): string;
    function BuildXml(const Context: IRunContext): string;
  public
    function GetContent: string; override;
    function GetFileExt: string; override;
    function ShowHelp: Boolean; override;
    procedure OnEndReport(const Context: IRunContext); override;
  end;

implementation

uses
  System.IOUtils;

var
  /// Format settings with dot as decimal separator for XML compatibility
  XmlFormatSettings: TFormatSettings;

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

function TJUnitReporter.EscapeXml(const S: string): string;
begin
  Result := S;
  Result := StringReplace(Result, '&', '&amp;', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
  Result := StringReplace(Result, '''', '&apos;', [rfReplaceAll]);
end;

function TJUnitReporter.TimeToXml(const Ms: Int64): string;
begin
  // Format time in seconds with dot as decimal separator (XML/JUnit standard)
  Result := FormatFloat('0.000', Ms / 1000.0, XmlFormatSettings);
end;

function TJUnitReporter.BuildTestCase(const Scenario: IScenario; const ClassName: string): string;
var
  SB: TStringBuilder;
  RunInfo: TSpecRunInfo;
begin
  RunInfo := (Scenario as ISpecItem).RunInfo;

  SB := TStringBuilder.Create;
  try
    if RunInfo.State <> srsFinished then
    begin
      // Skipped
      SB.AppendFormat('    <testcase name="%s" classname="%s" time="%s">',
        [EscapeXml(Scenario.Description), EscapeXml(ClassName), TimeToXml(RunInfo.ExecTimeMs)]);
      SB.AppendLine;
      SB.AppendLine('      <skipped/>');
      SB.AppendLine('    </testcase>');
    end
    else if RunInfo.Result = srrFail then
    begin
      // Failed
      SB.AppendFormat('    <testcase name="%s" classname="%s" time="%s">',
        [EscapeXml(Scenario.Description), EscapeXml(ClassName), TimeToXml(RunInfo.ExecTimeMs)]);
      SB.AppendLine;
      SB.AppendFormat('      <failure message="%s" type="AssertionError"/>',
        [EscapeXml(RunInfo.ErrMsg)]);
      SB.AppendLine;
      SB.AppendLine('    </testcase>');
    end
    else
    begin
      // Passed
      SB.AppendFormat('    <testcase name="%s" classname="%s" time="%s"/>',
        [EscapeXml(Scenario.Description), EscapeXml(ClassName), TimeToXml(RunInfo.ExecTimeMs)]);
      SB.AppendLine;
    end;

    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function TJUnitReporter.BuildTestSuite(const Feature: IFeature): string;
var
  SB: TStringBuilder;
  Rule: IRule;
  Scenario: IScenario;
  Outline: IScenarioOutline;
  Example: IScenario;
  RunInfo, ExampleRunInfo: TSpecRunInfo;
  ExampleNum: Integer;
  ExampleDesc: string;
begin
  RunInfo := (Feature as ISpecItem).RunInfo;

  SB := TStringBuilder.Create;
  try
    SB.AppendFormat('  <testsuite name="%s" tests="%d" failures="%d" errors="0" skipped="%d" time="%s">',
      [EscapeXml(Feature.Title), RunInfo.TotalCount, RunInfo.FailCount, RunInfo.SkipCount, TimeToXml(RunInfo.ExecTimeMs)]);
    SB.AppendLine;

    // Iterate through all Rules (including ImplicitRule via Feature.Rules)
    for Rule in Feature.Rules do
    begin
      for Scenario in Rule.Scenarios do
      begin
        // Check if it's a ScenarioOutline
        if Supports(Scenario, IScenarioOutline, Outline) then
        begin
          // Report each Example as a separate test case
          ExampleNum := 0;
          for Example in Outline.Examples do
          begin
            Inc(ExampleNum);
            ExampleDesc := Format('%s [Example %d]', [Outline.Description, ExampleNum]);
            ExampleRunInfo := (Example as ISpecItem).RunInfo;

            if ExampleRunInfo.State <> srsFinished then
            begin
              SB.AppendFormat('    <testcase name="%s" classname="%s" time="%s">',
                [EscapeXml(ExampleDesc), EscapeXml(Feature.Title), TimeToXml(ExampleRunInfo.ExecTimeMs)]);
              SB.AppendLine;
              SB.AppendLine('      <skipped/>');
              SB.AppendLine('    </testcase>');
            end
            else if ExampleRunInfo.Result = srrFail then
            begin
              SB.AppendFormat('    <testcase name="%s" classname="%s" time="%s">',
                [EscapeXml(ExampleDesc), EscapeXml(Feature.Title), TimeToXml(ExampleRunInfo.ExecTimeMs)]);
              SB.AppendLine;
              SB.AppendFormat('      <failure message="%s" type="AssertionError"/>',
                [EscapeXml(ExampleRunInfo.ErrMsg)]);
              SB.AppendLine;
              SB.AppendLine('    </testcase>');
            end
            else
            begin
              SB.AppendFormat('    <testcase name="%s" classname="%s" time="%s"/>',
                [EscapeXml(ExampleDesc), EscapeXml(Feature.Title), TimeToXml(ExampleRunInfo.ExecTimeMs)]);
              SB.AppendLine;
            end;
          end;
        end
        else
        begin
          // Regular scenario
          SB.Append(BuildTestCase(Scenario, Feature.Title));
        end;
      end;
    end;

    SB.AppendLine('  </testsuite>');
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function TJUnitReporter.BuildXml(const Context: IRunContext): string;
var
  SB: TStringBuilder;
  Feature: IFeature;
  SuiteRunInfo: TSpecRunInfo;
begin
  SuiteRunInfo := Context.Suite.RunInfo;

  SB := TStringBuilder.Create;
  try
    // XML header
    SB.AppendLine('<?xml version="1.0" encoding="UTF-8"?>');

    // Root testsuites element - use Suite.RunInfo for totals
    SB.AppendFormat('<testsuites name="MiniSpec" tests="%d" failures="%d" errors="0" skipped="%d" time="%s">',
      [SuiteRunInfo.TotalCount, SuiteRunInfo.FailCount, SuiteRunInfo.SkipCount, TimeToXml(SuiteRunInfo.ExecTimeMs)]);
    SB.AppendLine;

    // Each feature as a testsuite
    for Feature in Context.Suite.Features do
      SB.Append(BuildTestSuite(Feature));

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
