unit Daf.MiniSpec.Reporter;

interface
uses
  System.Classes,
  System.RegularExpressions,
  System.Generics.Collections,
  System.SysUtils,
  System.JSON,
  System.Rtti,
  Daf.MiniSpec.Types;

type
  ISpecReporter = interface(IInvokable)
    ['{CD69B272-5B38-4CCC-A64F-2B2A57ACB540}']
    function GetFailCount: Cardinal;
    function GetPassCount: Cardinal;
    function GetSkipCount: Cardinal;
    procedure Report(Features: TList<IFeature>);
    procedure BeginReport;
    procedure DoReport(const S: ISpecItem);
    procedure ReportOutline(const Outline: IScenarioOutline);
    procedure EndReport;
    function GetContent: string;
    function GetFileExt: string;
    function UseConsole: Boolean;
    property Content: string read GetContent;
    property PassCount: Cardinal read GetPassCount;
    property FailCount: Cardinal read GetFailCount;
    property SkipCount: Cardinal read GetSkipCount;
  end;

  TCustomReporter = class(TInterfacedObject, ISpecReporter)
  strict private
    FPassCount: Cardinal;
    FFailCount: Cardinal;
    FSkipCount: Cardinal;
  private
  protected
    function GetContent: string;virtual;abstract;
    procedure ResetCounters;
    procedure IncPass;
    procedure IncFail;
    procedure IncSkip;
    function GetFailCount: Cardinal;virtual;
    function GetPassCount: Cardinal;virtual;
    function GetSkipCount: Cardinal;virtual;
    function GetLevel(const Kind: TSpecItemKind): Byte;virtual;
    function GetKeyWord(const Kind: TSpecItemKind): string;virtual;
    function GetFileExt: string;virtual;
    procedure DoReport(const S: ISpecItem);virtual;
    procedure Report(Feature: IFeature);overload;
    procedure Report(Rule: IRule);overload;
    procedure Report(Background: IBackground);overload;
    procedure Report(Scenario: IScenario);overload;
    procedure ReportOutline(const Outline: IScenarioOutline);virtual;
  public
    function UseConsole: Boolean;virtual;
    procedure BeginReport;virtual;
    procedure Report(Features: TList<IFeature>);overload;

    procedure EndReport;virtual;
    property PassCount: Cardinal read GetPassCount;
    property FailCount: Cardinal read GetFailCount;
    property SkipCount: Cardinal read GetSkipCount;
  end;

  TReporterDecorator = class(TCustomReporter)
  strict private
    FDecorated: ISpecReporter;
  protected
    function GetFailCount: Cardinal;override;
    function GetPassCount: Cardinal;override;
    function GetSkipCount: Cardinal;override;
    procedure DoReport(const S: ISpecItem);override;
    procedure ReportOutline(const Outline: IScenarioOutline);override;
  public
    constructor Create(const Decorated: ISpecReporter);
    function UseConsole: Boolean;override;
    function GetFileExt: string;override;
    procedure BeginReport;override;
    procedure EndReport;override;
    function GetContent: string;override;
    property Decorated: ISpecReporter read FDecorated;
  end;

  TConsoleReporter = class(TCustomReporter)
  private
    procedure OutputLn(const Level: Byte; const Text: string; const Success: Boolean; const Duration: Integer; const ErrorMessage: string = '');overload;
    procedure OutputLn(const Level: Byte; const Text: string);overload;
    procedure Output(const Level: Byte; const Text: string);
    function ExtractValue(const Match: TMatch): string;
    function Level2Margin(const Level: Byte): string;
  protected
    function GetContent: string;override;
    procedure ReportOutline(const Outline: IScenarioOutline);override;
  public
    function UseConsole: Boolean;override;
    procedure DoReport(const S: ISpecItem);override;
    procedure BeginReport;override;
  end;

  TJsonReporter = class(TCustomReporter)
  private
    FOutput: string;
    FFeatures: TJSONArray;
    FCurrentFeature: TJSONObject;
    FCurrentScenarios: TJSONArray;
    FCurrentScenario: TJSONObject;
    FCurrentSteps: TJSONArray;
    procedure AddStep(const Kind, Description: string; Success: Boolean; Duration: Integer; const ErrorMessage: string = '');
  protected
    function GetContent: string;override;
    function GetFileExt: string;override;
    procedure DoReport(const S: ISpecItem);override;
    procedure ReportOutline(const Outline: IScenarioOutline);override;
  public
    procedure BeginReport;override;
    procedure EndReport;override;
    procedure Feature(const Description: string);
    procedure Scenario(const Kind: string; const Description: string; const Status: string; Duration: Integer; const ErrorMessage: string = '');
    procedure Step(const S: ISpecItem; Success: Boolean; Duration: Integer; const ErrorMessage: string = '');
    property Output: string read GetContent;
  end;

  THTMLReporter = class(TReporterDecorator)
  private
  public
    constructor Create;
    function GetContent: string;override;
    function GetFileExt: string;override;
  end;

  TGherkinReporter = class(TCustomReporter)
  private
    FLines: TStringList;
    FIndent: Integer;
    FWithResults: Boolean;
    FCurrentFeatureName: string;
    FOutputDir: string;
    FFilesWritten: TStringList;
    procedure AddLine(const Text: string);
    procedure AddTags(const Tags: TSpecTags);
    function GetGherkinKeyword(Kind: TSpecItemKind): string;
    function RestorePlaceholders(const Text: string): string;
    function IsTagLine(const Line: string): Boolean;
    function ExtractTitle(const Desc: string): string;
    function ExtractMotivation(const Desc: string): string;
    procedure WriteExamplesTable(const Outline: IScenarioOutline);
    function ResultComment(const Item: ISpecItem): string;
    function SanitizeFileName(const Name: string): string;
    procedure FlushCurrentFeature;
  protected
    function GetContent: string;override;
    function GetFileExt: string;override;
    procedure DoReport(const S: ISpecItem);override;
    procedure ReportOutline(const Outline: IScenarioOutline);override;
  public
    constructor Create(AWithResults: Boolean = False);
    destructor Destroy;override;
    procedure BeginReport;override;
    procedure EndReport;override;
    property WithResults: Boolean read FWithResults write FWithResults;
  end;

implementation
uses
  System.StrUtils,
  System.IOUtils,
  Daf.MiniSpec.Utils,
  Daf.MiniSpec;

{$REGION 'Dashboard html'}
const
  MINI_SPEC_DASHBOARD_HTML = '''
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>MiniSpec Dashboard</title>
  <link rel="stylesheet" href="https://unpkg.com/@picocss/pico@1.*/css/pico.min.css">
  <style>
    details.motivation-block {
      margin-bottom: 0.5em;
      background: #f8f9fa;
      border-left: 3px solid #bbb;
      border-radius: 3px;
      font-size: 0.97em;
    }
    details.motivation-block summary {
      cursor: pointer;
      font-weight: bold;
      color: #777;
      padding: 0.2em 0.4em;
    }
    pre.motivation {
      margin: 0 0.1em 0.1em 1.2em;
      padding: 0 0.2em;
      background: none;
      border: none;
      color: #555;
      border-radius: 0;
      font-family: inherit;
      font-size: inherit;
      white-space: pre-wrap;
    }
    html, body { font-size: 14px; }
    main.container { max-width: 700px; margin: auto; padding: 0.2em; }
    h1 { font-size: 1.08em; margin-bottom: 0.4em; }
    h2.feature { font-size: 1em; margin-bottom: 0.22em; }
    details { margin-bottom: 0.08em; }
    details[open] { margin-bottom: 0.11em; }
    summary.pass, summary.fail {
      font-size: 0.96em !important;
      padding: 0.11em 0.09em 0.08em 0.11em;
      margin-bottom: 0;
    }
    summary.pass { color: #090 !important; font-weight: bold; }
    summary.fail { color: #c00 !important; font-weight: bold; }
    details > summary { cursor: pointer; }
    ul { margin: 0.01em 0 0.04em 0.85em; padding: 0;}
    li { font-size: 11px; margin: 0 0 0.01em 0; padding: 0.04em 0 0.01em 0; line-height: 1.13; }
    .pass { color: #090 !important; }
    .fail { color: #c00 !important; font-weight: bold; }
    .skip { color: #888 !important; font-style: italic; }
    .duration { color: #999; font-size: 0.88em;}
    .placeholder { background: #ffe4a0; color: #a65c00; border-radius: 2px; padding: 0 2px; font-family: monospace; }
    .summary { margin-top: 0.55em; font-size: 0.98em; border-top: 1px solid #ddd; padding-top: 0.22em;}
    footer { margin-top: 0.2em; font-size: 0.91em; color: #777; }
    @media (max-width: 700px) {
      main.container { padding: 0.1em;}
      h2.feature { font-size: 0.97em;}
    }
  </style>
</head>
<body>
  <main class="container">
    <div style="display:flex; justify-content:space-between; align-items:center; margin-bottom:0.5em; border-bottom:1px solid #ddd; padding-bottom:0.5em;">
      <h1 style="margin:0;">MiniSpec Dashboard</h1>
      <div style="font-weight:normal; font-size:0.93em; white-space:nowrap;">
        <label style="margin-right:1em; display:inline-block;">
          <input type="checkbox" id="filter-pass" checked style="vertical-align:middle;">
          <span class="pass">Pass</span>
        </label>
        <label style="margin-right:1em; display:inline-block;">
          <input type="checkbox" id="filter-fail" checked style="vertical-align:middle;">
          <span class="fail">Fail</span>
        </label>
        <label style="margin-right:1em; display:inline-block;">
          <input type="checkbox" id="filter-skip" style="vertical-align:middle;">
          <span class="skip">Skip</span>
        </label>
        <span style="border-left:1px solid #ccc; padding-left:1em; display:inline-block;">
          <label>
            <input type="checkbox" id="autoreload" role="switch" style="vertical-align:middle;">
            Auto-reload
          </label>
        </span>
      </div>
    </div>
    <div id="minispec-dashboard"></div>
    <div class="summary" id="summary"></div>
    <footer>
      <small>Last updated: <span id="last-update"></span></small>
    </footer>
  </main>
  <script>
    window.miniSpecReportData = {{MINISPEC_JSON}};

    function htmlEncode(str) {
      return String(str).replace(/[&<>"']/g, function(m) {
        return ({
          '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;'
        })[m];
      });
    }
    function highlightPlaceholders(str) {
      return str.replace(/#\{(.+?)\}/g, '<span class="placeholder">$1</span>');
    }
    function preserveLineBreaks(str) {
      return str.replace(/\r\n|\n|\r/g, '<br>');
    }
    function renderFeatureHeader(featureText) {
      if (!featureText) return '';
      const lines = featureText.split(/\r?\n/).map(line => line.trim());
      let title = '', idx = 0;
      while (idx < lines.length && lines[idx] === '') idx++;
      if (idx < lines.length) title = lines[idx++];
      while (idx < lines.length && lines[idx] === '') idx++;
      let motivationArr = [];
      while (idx < lines.length) motivationArr.push(lines[idx++]);
      while (motivationArr.length && motivationArr[0] === '') motivationArr.shift();
      while (motivationArr.length && motivationArr[motivationArr.length - 1] === '') motivationArr.pop();

      let html = `<h2 class="feature">Feature: ${htmlEncode(title)}</h2>`;
      if (motivationArr.length) {
        let motivation = motivationArr.join('\n');
        html += `<details class="motivation-block"><summary>Motivation</summary><pre class="motivation">${highlightPlaceholders(htmlEncode(motivation))}</pre></details>`;
      }
      return html;
    }
    function renderReport(data, filters) {
      filters = filters || { pass: true, fail: true, skip: false };
      if (!data || !Array.isArray(data.features)) {
        document.getElementById('minispec-dashboard').innerHTML = '<em>No results.</em>';
        return;
      }
      let html = '';
      data.features.forEach(function(feature, fidx) {
        let featureHtml = '';
        let hasVisibleScenarios = false;
        if (Array.isArray(feature.scenarios)) {
          feature.scenarios.forEach(function(scenario, sidx) {
            let status = scenario.status || 'fail';
            let isSkip = (status === 'skip');
            let isFail = (status === 'fail') || (!isSkip && scenario.steps && scenario.steps.some(step => !step.success));
            let isPass = !isSkip && !isFail;

            // Para Scenario Outline, verificar si algún example falló
            if (scenario.kind === 'Scenario Outline' && scenario.examples) {
              isFail = scenario.examples.some(ex => ex.status === 'fail');
              isPass = !isFail && !isSkip;
            }

            // Aplicar filtros
            if (isPass && !filters.pass) return;
            if (isFail && !filters.fail) return;
            if (isSkip && !filters.skip) return;

            hasVisibleScenarios = true;
            let resultClass = isSkip ? 'skip' : (isFail ? 'fail' : 'pass');
            featureHtml += `<details ${isFail ? 'open' : ''} style="margin-bottom:0;">`;
            featureHtml += `<summary class="${resultClass}" style="font-size:0.93em;">${isSkip ? '⊘' : ''} ${scenario.kind}: ${highlightPlaceholders(preserveLineBreaks(htmlEncode(scenario.description)))}</summary>`;

            // Renderizar Scenario Outline con tabla
            if (scenario.kind === 'Scenario Outline' && scenario.headers && scenario.examples) {
              featureHtml += `<div style="margin-left:1em; margin-top:0.3em;">`;
              featureHtml += `<strong>Examples:</strong>`;
              featureHtml += `<table style="font-size:11px; border-collapse:collapse; margin:0.3em 0;">`;
              featureHtml += `<tr style="background:#f0f0f0;">`;
              featureHtml += `<th style="padding:2px 6px; border:1px solid #ccc;"></th>`;
              scenario.headers.forEach(h => {
                featureHtml += `<th style="padding:2px 6px; border:1px solid #ccc;">${htmlEncode(h)}</th>`;
              });
              featureHtml += `<th style="padding:2px 6px; border:1px solid #ccc;">Time</th>`;
              featureHtml += `</tr>`;
              scenario.examples.forEach(ex => {
                let exClass = ex.status === 'pass' ? 'pass' : 'fail';
                let icon = ex.status === 'pass' ? '✅' : '❌';
                featureHtml += `<tr class="${exClass}">`;
                featureHtml += `<td style="padding:2px 6px; border:1px solid #ccc;">${icon}</td>`;
                ex.values.forEach(v => {
                  featureHtml += `<td style="padding:2px 6px; border:1px solid #ccc;">${htmlEncode(v)}</td>`;
                });
                featureHtml += `<td style="padding:2px 6px; border:1px solid #ccc;" class="duration">${ex.duration} ms</td>`;
                featureHtml += `</tr>`;
                if (ex.status === 'fail' && ex.error) {
                  featureHtml += `<tr><td colspan="${scenario.headers.length + 2}" style="padding:2px 6px; border:1px solid #ccc;" class="fail">ERROR: ${htmlEncode(ex.error)}</td></tr>`;
                }
              });
              featureHtml += `</table></div>`;
            } else {
              // Renderizar escenario normal con steps
              featureHtml += `<ul>`;
              if (scenario.steps && !isSkip) {
                scenario.steps.forEach(function(step) {
                  let stepClass = step.success ? "pass" : "fail";
                  let errorMsg = step.success ? "" : ` <span class="fail"><br>ERROR:"${htmlEncode(step.error || "")}"</span>`;
                  featureHtml += `<li class="${stepClass}">${htmlEncode(step.kind)}: ${highlightPlaceholders(preserveLineBreaks(htmlEncode(step.description)))} <span class="duration">(${step.duration} ms)</span>${errorMsg}</li>`;
                });
              }
              featureHtml += `</ul>`;
            }
            featureHtml += `</details>`;
          });
        }
        if (hasVisibleScenarios) {
          html += renderFeatureHeader(feature.description);
          html += featureHtml;
        }
      });
      document.getElementById('minispec-dashboard').innerHTML = html;
      let pass = data.passCount || 0, fail = data.failCount || 0, skip = data.skipCount || 0;
      let skipHtml = skip > 0 ? ` &nbsp; Skipped: <span class="skip">${skip}</span>` : '';
      document.getElementById('summary').innerHTML =
        `Passed: <span class="pass">${pass}</span> &nbsp; Failed: <span class="fail">${fail}</span>${skipHtml}`;
    }

    var minispecReloadTimeout = null;
    const reload_interval = 2000;

    function scheduleAutoReload() {
      if (minispecReloadTimeout) clearTimeout(minispecReloadTimeout);
      var reloadEnabled = document.getElementById('autoreload').checked;
      if (reloadEnabled) {
        minispecReloadTimeout = setTimeout(function() {
          var url = window.location.pathname + '?_ts=' + Date.now();
          window.location.href = url;
        }, reload_interval);
      }
    }

    document.addEventListener('DOMContentLoaded', function() {
      // Filtros de estado
      var filterPass = document.getElementById('filter-pass');
      var filterFail = document.getElementById('filter-fail');
      var filterSkip = document.getElementById('filter-skip');

      // Cargar estado de filtros desde localStorage
      filterPass.checked = localStorage.getItem('minispec-filter-pass') !== 'false';
      filterFail.checked = localStorage.getItem('minispec-filter-fail') !== 'false';
      filterSkip.checked = localStorage.getItem('minispec-filter-skip') === 'true';

      function getFilters() {
        return {
          pass: filterPass.checked,
          fail: filterFail.checked,
          skip: filterSkip.checked
        };
      }

      function reRender() {
        renderReport(window.miniSpecReportData, getFilters());
      }

      filterPass.addEventListener('change', function() {
        localStorage.setItem('minispec-filter-pass', filterPass.checked ? 'true' : 'false');
        reRender();
      });
      filterFail.addEventListener('change', function() {
        localStorage.setItem('minispec-filter-fail', filterFail.checked ? 'true' : 'false');
        reRender();
      });
      filterSkip.addEventListener('change', function() {
        localStorage.setItem('minispec-filter-skip', filterSkip.checked ? 'true' : 'false');
        reRender();
      });

      renderReport(window.miniSpecReportData, getFilters());
      document.getElementById('last-update').textContent = new Date().toLocaleString();

      var reloadCheckbox = document.getElementById('autoreload');
      var reloadEnabled = localStorage.getItem('minispec-autoreload') !== 'false';
      reloadCheckbox.checked = reloadEnabled;

      reloadCheckbox.addEventListener('change', function() {
        localStorage.setItem('minispec-autoreload', reloadCheckbox.checked ? 'true' : 'false');
        scheduleAutoReload();
      });

      scheduleAutoReload();
    });
  </script>
</body>
</html>
''';
{$ENDREGION}


{ TCustomReporter }

procedure TCustomReporter.IncPass;
begin
  Inc(FPassCount);
end;

procedure TCustomReporter.IncFail;
begin
  Inc(FFailCount);
end;

procedure TCustomReporter.IncSkip;
begin
  Inc(FSkipCount);
end;

procedure TCustomReporter.BeginReport;
begin
  ResetCounters;
end;

procedure TCustomReporter.Report(Feature: IFeature);

  function HasExecutedScenarios: Boolean;
  begin
    // Verificar escenarios en todas las Rules (incluyendo ImplicitRule)
    for var Rule in Feature.Rules do
      for var Scenario in Rule.Scenarios do
        if Scenario.RunInfo.State = srsFinished then
          Exit(True);

    Result := False;
  end;

begin
  // Solo reportar la Feature si tiene al menos un escenario ejecutado
  if not HasExecutedScenarios then
    Exit;

  DoReport(Feature);

  // Reportar todas las Rules (ImplicitRule se trata especialmente)
  for var Rule in Feature.Rules do
    Report(Rule);
end;

procedure TCustomReporter.ReportOutline(const Outline: IScenarioOutline);
begin
  // Reportar el ScenarioOutline como header
  DoReport(Outline);

  // Reportar los steps del outline (template)
  for var Step in Outline.StepsGiven do
    DoReport(Step);
  for var Step in Outline.StepsWhen do
    DoReport(Step);
  for var Step in Outline.StepsThen do
    DoReport(Step);

  // Reportar cada Example
  for var Example in Outline.Examples do
  begin
    if Example.RunInfo.State = srsFinished then
      DoReport(Example);
  end;
end;

procedure TCustomReporter.Report(Rule: IRule);
begin
  // Solo reportar la Rule si tiene al menos un escenario ejecutado
  var HasExecutedScenario := False;
  for var Scenario in Rule.Scenarios do
    if Scenario.RunInfo.State = srsFinished then
    begin
      HasExecutedScenario := True;
      Break;
    end;

  if not HasExecutedScenario then
    Exit;

  // Solo mostrar header si es Rule explícita (no ImplicitRule)
  if Rule.Kind = sikRule then
    DoReport(Rule);

  Report(Rule.BackGround);

  // Iterar scenarios - polimórficamente detectar Outlines
  for var Scenario in Rule.Scenarios do
  begin
    if Scenario.RunInfo.State <> srsFinished then
      Continue;

    var Outline: IScenarioOutline;
    if Supports(Scenario, IScenarioOutline, Outline) then
      ReportOutline(Outline)
    else
      Report(Scenario);
  end;
end;

procedure TCustomReporter.Report(Background: IBackground);
begin
  if not Assigned(Background) then Exit;
  DoReport(Background);
  for var Step in BackGround.StepsGiven do
    DoReport(Step);
end;

procedure TCustomReporter.Report(Scenario: IScenario);
begin
  // Solo reportar escenarios que fueron ejecutados
  if Scenario.RunInfo.State <> srsFinished then
    Exit;

  DoReport(Scenario);
  for var Step in Scenario.StepsGiven do
    DoReport(Step);
  for var Step in Scenario.StepsWhen do
    DoReport(Step);
  for var Step in Scenario.StepsThen do
    DoReport(Step);
end;

procedure TCustomReporter.DoReport(const S: ISpecItem);
begin
  if (S.Kind in [sikScenario, sikExample]) then
  begin
    // Contar escenarios según su estado
    if S.RunInfo.State <> srsFinished then
    begin
      IncSkip;
      Exit;
    end;
    if S.RunInfo.IsSuccess then
      IncPass
    else
      IncFail;
  end;
end;

procedure TCustomReporter.EndReport;
begin

end;

function TCustomReporter.GetFailCount: Cardinal;
begin
  Result := FFailCount;
end;

function TCustomReporter.GetSkipCount: Cardinal;
begin
  Result := FSkipCount;
end;

function TCustomReporter.GetFileExt: string;
begin
  Result := '';
end;

function TCustomReporter.GetKeyWord(const Kind: TSpecItemKind): string;
begin
  case Kind of
    sikFeature: Result := 'Feature';
    sikImplicitRule: Result := '';  // No mostrar keyword para Rule implícita
    sikRule: Result := 'Rule';
    sikBackground: Result :=  'Background';
    sikScenario: Result :=  'Scenario';
    sikScenarioOutline: Result := 'Scenario Outline';
    sikExample: Result :=  'Example';
    sikExampleInit: Result := '';
    sikGiven: Result := 'Given';
    sikWhen: Result := 'When';
    sikThen: Result := 'Then';
    else
      Result := '';
  end;
end;

function TCustomReporter.GetLevel(const Kind: TSpecItemKind): Byte;
begin
  case Kind of
    sikFeature: Result := 0;
    sikRule: Result := 1;  // Rule está un nivel debajo de Feature
    sikBackground, sikScenario, sikScenarioOutline, sikExample: Result := 1;
    sikExampleInit: Result := 2;
    sikGiven: Result := 2;
    sikWhen: Result := 2;
    sikThen: Result := 2;
    else
      Result := 0;
  end;
end;

function TCustomReporter.GetPassCount: Cardinal;
begin
  Result := FPassCount;
end;

procedure TCustomReporter.Report(Features: TList<IFeature>);
begin
  BeginReport;
  for var F in Features do
    Report(F);
  EndReport;
end;

procedure TCustomReporter.ResetCounters;
begin
  FPassCount := 0;
  FFailCount := 0;
  FSkipCount := 0;
end;

function TCustomReporter.UseConsole: Boolean;
begin
  Result := False;
end;

{ TConsoleReporter }

procedure TConsoleReporter.DoReport(const S: ISpecItem);
begin
  inherited;
  var Kind := GetKeyWord(S.Kind);
  var Level := GetLevel(S.Kind);
  OutputLn(Level, Kind + ' ' +  S.Description, S.RunInfo.IsSuccess, S.RunInfo.ExecTimeMs, S.RunInfo.ErrMsg);
end;

function TConsoleReporter.ExtractValue(const Match: TMatch): string;
begin
  Result := Match.Groups[1].Value;
end;

procedure TConsoleReporter.OutputLn(const Level: Byte; const Text: string; const Success: Boolean; const Duration: Integer; const ErrorMessage: string);
begin
  var Msg := ErrorMessage;
  if not Msg.IsEmpty then
    Msg := SLineBreak + Level2Margin(Level) + 'ERROR: "' + Msg + '"';
  if Success then
    OutputLn(Level, Format('✅ %s (%d ms)', [Text, Duration]))
  else
    OutputLn(Level, Format('❌ %s (%d ms)%s', [Text, Duration, Msg]));
end;

procedure TConsoleReporter.OutputLn(const Level: Byte; const Text: string);
begin
  Output(Level, Text + SLineBreak);
end;

function TConsoleReporter.Level2Margin(const Level: Byte):string;
begin
  Result := DupeString(' ', 2 * Level);
end;

procedure TConsoleReporter.Output(const Level: Byte; const Text: string);
begin
  var Margin := Level2Margin(Level);
  var Regex := TRegEx.Create('#\{([^\}]+)\}');
  var OutputText := Regex.Replace(Text, ExtractValue);
  if Level = 0 then
    Margin := SLineBreak + Margin;
  Write(Margin + OutputText);
end;

function TConsoleReporter.UseConsole: Boolean;
begin
  Result := True;
end;

function TConsoleReporter.GetContent: string;
begin
  Result := '';
end;

procedure TConsoleReporter.BeginReport;
begin
  inherited;
  OSShell.UseUTF8;
  Output(0, '+----------------------+');
  Output(0, '|   MiniSpec v' +  TMiniSpec.Version + '    |');
  Output(0, '| Full specs, zero fat |');
  Output(0, '+----------------------+');
  OutputLn(0, '');
end;

procedure TConsoleReporter.ReportOutline(const Outline: IScenarioOutline);
var
  AllSuccess: Boolean;
  TotalTime: Int64;
  ColWidths: TArray<Integer>;
  Headers: TArray<string>;
  i: Integer;
  HeaderLine, Row: string;
  Values: TArray<TValue>;
begin
  // Calcular resultado agregado
  AllSuccess := True;
  TotalTime := 0;
  for var Example in Outline.Examples do
  begin
    if Example.RunInfo.State = srsFinished then
    begin
      TotalTime := TotalTime + Example.RunInfo.ExecTimeMs;
      if not Example.RunInfo.IsSuccess then
        AllSuccess := False;
    end;
  end;

  // Header del Scenario Outline con resultado
  OutputLn(1, 'Scenario Outline: ' + Outline.Description, AllSuccess, TotalTime);

  // Steps template (sin tiempo individual)
  for var Step in Outline.StepsGiven do
    OutputLn(2, GetKeyWord(Step.Kind) + ' ' + Step.Description);
  for var Step in Outline.StepsWhen do
    OutputLn(2, GetKeyWord(Step.Kind) + ' ' + Step.Description);
  for var Step in Outline.StepsThen do
    OutputLn(2, GetKeyWord(Step.Kind) + ' ' + Step.Description);

  // Calcular anchos de columna
  Headers := Outline.Headers;
  SetLength(ColWidths, Length(Headers));
  for i := 0 to High(Headers) do
    ColWidths[i] := Length(Headers[i]);

  for var Example in Outline.Examples do
  begin
    Values := Example.ExampleMeta.Values;
    for i := 0 to High(Values) do
      if (i <= High(ColWidths)) and (Length(Values[i].ToString) > ColWidths[i]) then
        ColWidths[i] := Length(Values[i].ToString);
  end;

  // Tabla de Examples
  OutputLn(2, 'Examples:');

  // Header de la tabla (3 espacios para alinear con emoji ✅)
  HeaderLine := '|';
  for i := 0 to High(Headers) do
    HeaderLine := HeaderLine + ' ' + Headers[i].PadRight(ColWidths[i]) + ' |';
  OutputLn(3, '   ' + HeaderLine);

  // Cada fila con su resultado
  for var Example in Outline.Examples do
  begin
    if Example.RunInfo.State = srsFinished then
    begin
      Values := Example.ExampleMeta.Values;
      Row := '|';
      for i := 0 to High(Headers) do
      begin
        if i <= High(Values) then
          Row := Row + ' ' + Values[i].ToString.PadRight(ColWidths[i]) + ' |'
        else
          Row := Row + ' ' + ''.PadRight(ColWidths[i]) + ' |';
      end;
      OutputLn(3, Row, Example.RunInfo.IsSuccess, Example.RunInfo.ExecTimeMs, Example.RunInfo.ErrMsg);

      // Contar para estadísticas
      if Example.RunInfo.IsSuccess then
        IncPass
      else
        IncFail;
    end;
  end;
end;

{ TJsonReporter }

procedure TJsonReporter.BeginReport;
begin
  inherited;
  FFeatures := TJSONArray.Create;
  FCurrentFeature := nil;
  FCurrentScenarios := nil;
  FCurrentScenario := nil;
  FCurrentSteps := nil;
end;

procedure TJsonReporter.Feature(const Description: string);
begin
  if Assigned(FCurrentFeature) then
  begin
    if Assigned(FCurrentScenario) then
    begin
      FCurrentScenario.AddPair('steps', FCurrentSteps);
      FCurrentScenarios.AddElement(FCurrentScenario);
      FCurrentScenario := nil;
      FCurrentSteps := nil;
    end;
    FCurrentFeature.AddPair('scenarios', FCurrentScenarios);
    FFeatures.AddElement(FCurrentFeature);
    FCurrentFeature := nil;
    FCurrentScenarios := nil;
  end;
  FCurrentFeature := TJSONObject.Create;
  FCurrentFeature.AddPair('description', Description);
  FCurrentScenarios := TJSONArray.Create;
end;

procedure TJsonReporter.Scenario(const Kind: string; const Description: string; const Status: string; Duration: Integer; const ErrorMessage: string);
begin
  if Assigned(FCurrentScenario) then
  begin
    FCurrentScenario.AddPair('steps', FCurrentSteps);
    FCurrentScenarios.AddElement(FCurrentScenario);
  end;
  FCurrentScenario := TJSONObject.Create;
  FCurrentScenario.AddPair('kind', Kind);
  FCurrentScenario.AddPair('description', Description);
  FCurrentScenario.AddPair('status', Status);
  FCurrentScenario.AddPair('duration', TJSONNumber.Create(Duration));
  if (Status = 'fail') and (ErrorMessage <> '') then
    FCurrentScenario.AddPair('error', ErrorMessage);
  FCurrentSteps := TJSONArray.Create;
end;

procedure TJsonReporter.DoReport(const S: ISpecItem);

  function GetStatus(const Item: ISpecItem): string;
  begin
    if Item.RunInfo.State <> srsFinished then
      Result := 'skip'
    else if Item.RunInfo.Result = srrSuccess then
      Result := 'pass'
    else
      Result := 'fail';
  end;

begin
  inherited;
  case S.Kind of
    sikFeature: Feature(S.Description);
    sikRule: Scenario(GetKeyWord(s.Kind), S.Description, GetStatus(S), S.RunInfo.ExecTimeMs, S.RunInfo.ErrMsg);
    sikBackground: Scenario(GetKeyWord(s.Kind), S.Description, GetStatus(S), S.RunInfo.ExecTimeMs, S.RunInfo.ErrMsg);
    sikScenario, sikExample: Scenario(GetKeyWord(s.Kind), S.Description, GetStatus(S), S.RunInfo.ExecTimeMs, S.RunInfo.ErrMsg);
    sikExampleInit: ;
    sikGiven,
    sikWhen,
    sikThen: Step(S, S.RunInfo.Result = srrSuccess, S.RunInfo.ExecTimeMs, S.RunInfo.ErrMsg);
  end;
end;

procedure TJsonReporter.ReportOutline(const Outline: IScenarioOutline);

  function GetStatus(const Item: ISpecItem): string;
  begin
    if Item.RunInfo.State <> srsFinished then
      Result := 'skip'
    else if Item.RunInfo.Result = srrSuccess then
      Result := 'pass'
    else
      Result := 'fail';
  end;

var
  OutlineObj: TJSONObject;
  HeadersArr: TJSONArray;
  ExamplesArr: TJSONArray;
  ExampleObj: TJSONObject;
  ValuesArr: TJSONArray;
  Example: IScenario;
  Header: string;
  Value: TValue;
begin
  // Close previous scenario if needed
  if Assigned(FCurrentScenario) then
  begin
    FCurrentScenario.AddPair('steps', FCurrentSteps);
    FCurrentScenarios.AddElement(FCurrentScenario);
    FCurrentScenario := nil;
    FCurrentSteps := nil;
  end;

  // Create outline object
  OutlineObj := TJSONObject.Create;
  OutlineObj.AddPair('kind', 'Scenario Outline');
  OutlineObj.AddPair('description', Outline.Description);
  OutlineObj.AddPair('status', GetStatus(Outline as ISpecItem));
  OutlineObj.AddPair('duration', TJSONNumber.Create((Outline as ISpecItem).RunInfo.ExecTimeMs));

  // Add headers
  HeadersArr := TJSONArray.Create;
  for Header in Outline.Headers do
    HeadersArr.Add(Header);
  OutlineObj.AddPair('headers', HeadersArr);

  // Add examples with their values and status
  ExamplesArr := TJSONArray.Create;
  for Example in Outline.Examples do
  begin
    ExampleObj := TJSONObject.Create;

    ValuesArr := TJSONArray.Create;
    for Value in Example.ExampleMeta.Values do
      ValuesArr.Add(Value.ToString);
    ExampleObj.AddPair('values', ValuesArr);

    ExampleObj.AddPair('status', GetStatus(Example as ISpecItem));
    ExampleObj.AddPair('duration', TJSONNumber.Create((Example as ISpecItem).RunInfo.ExecTimeMs));

    if (Example as ISpecItem).RunInfo.Result = srrFail then
      ExampleObj.AddPair('error', (Example as ISpecItem).RunInfo.ErrMsg);

    ExamplesArr.AddElement(ExampleObj);
  end;
  OutlineObj.AddPair('examples', ExamplesArr);

  FCurrentScenarios.AddElement(OutlineObj);
end;

procedure TJsonReporter.Step(const S: ISpecItem; Success: Boolean; Duration: Integer; const ErrorMessage: string);
begin
  AddStep(GetKeyWord(S.Kind), S.Description, Success, Duration, ErrorMessage);
end;

procedure TJsonReporter.AddStep(const Kind, Description: string; Success: Boolean; Duration: Integer; const ErrorMessage: string);
var
  StepObj: TJSONObject;
begin
  if not Assigned(FCurrentSteps) then Exit;
  StepObj := TJSONObject.Create;
  StepObj.AddPair('kind', Kind);
  StepObj.AddPair('description', Description);
  StepObj.AddPair('success', TJSONBool.Create(Success));
  StepObj.AddPair('duration', TJSONNumber.Create(Duration));
  if not Success and (ErrorMessage <> '') then
    StepObj.AddPair('error', ErrorMessage);
  FCurrentSteps.AddElement(StepObj);
end;

procedure TJsonReporter.EndReport;
begin
  if Assigned(FCurrentScenario) then
  begin
    FCurrentScenario.AddPair('steps', FCurrentSteps);
    FCurrentScenarios.AddElement(FCurrentScenario);
    FCurrentScenario := nil;
    FCurrentSteps := nil;
  end;
  if Assigned(FCurrentFeature) then
  begin
    FCurrentFeature.AddPair('scenarios', FCurrentScenarios);
    FFeatures.AddElement(FCurrentFeature);
    FCurrentFeature := nil;
    FCurrentScenarios := nil;
  end;

  var Root := TJSONObject.Create;
  try
    Root.AddPair('features', FFeatures);
    Root.AddPair('passCount', TJSONNumber.Create(PassCount));
    Root.AddPair('failCount', TJSONNumber.Create(FailCount));
    Root.AddPair('skipCount', TJSONNumber.Create(SkipCount));
    FOutput := Root.Format(4);
  finally
    Root.Free;
  end;
  inherited;
end;

function TJsonReporter.GetContent: string;
begin
  Result := FOutput;
end;

function TJsonReporter.GetFileExt: string;
begin
  Result := 'json';
end;

{ TReporterDecorator }

constructor TReporterDecorator.Create(const Decorated: ISpecReporter);
begin
  inherited Create;
  FDecorated := Decorated;
end;

procedure TReporterDecorator.DoReport(const S: ISpecItem);
begin
  if not Assigned(Decorated) then Exit;
  Decorated.DoReport(S);
end;

procedure TReporterDecorator.ReportOutline(const Outline: IScenarioOutline);
begin
  if not Assigned(Decorated) then Exit;
  Decorated.ReportOutline(Outline);
end;

procedure TReporterDecorator.BeginReport;
begin
  if not Assigned(Decorated) then Exit;
  Decorated.BeginReport;
end;

procedure TReporterDecorator.EndReport;
begin
  if not Assigned(Decorated) then Exit;
  Decorated.EndReport;
end;

function TReporterDecorator.GetContent: string;
begin
  if not Assigned(Decorated) then Exit('');
  Result := Decorated.Content;
end;

function TReporterDecorator.GetFailCount: Cardinal;
begin
  if not Assigned(Decorated) then Exit(0);
  Result := Decorated.GetFailCount;
end;

function TReporterDecorator.GetFileExt: string;
begin
  if not Assigned(Decorated) then Exit('');
  Result := Decorated.GetFileExt;
end;

function TReporterDecorator.GetPassCount: Cardinal;
begin
  if not Assigned(Decorated) then Exit(0);
  Result := Decorated.GetPassCount;
end;

function TReporterDecorator.GetSkipCount: Cardinal;
begin
  if not Assigned(Decorated) then Exit(0);
  Result := Decorated.GetSkipCount;
end;

function TReporterDecorator.UseConsole: Boolean;
begin
  if not Assigned(Decorated) then Exit(False);
  Result := Decorated.UseConsole;
end;

{ THTMLReporter }

constructor THTMLReporter.Create;
begin
  inherited Create(TJsonReporter.Create);
end;

function THTMLReporter.GetContent: string;
begin
  var JsonReport := inherited GetContent;
  Result := MINI_SPEC_DASHBOARD_HTML.Replace('{{MINISPEC_JSON}};', JsonReport);
end;
function THTMLReporter.GetFileExt: string;
begin
  Result := 'html';
end;

{ TGherkinReporter }

constructor TGherkinReporter.Create(AWithResults: Boolean);
begin
  inherited Create;
  FWithResults := AWithResults;
  FLines := TStringList.Create;
  FFilesWritten := TStringList.Create;
  FIndent := 0;
  FOutputDir := ExtractFilePath(ParamStr(0));
end;

destructor TGherkinReporter.Destroy;
begin
  FFilesWritten.Free;
  FLines.Free;
  inherited;
end;

procedure TGherkinReporter.BeginReport;
begin
  inherited;
  FLines.Clear;
  FFilesWritten.Clear;
  FCurrentFeatureName := '';
  FIndent := 0;
end;

procedure TGherkinReporter.EndReport;
begin
  FlushCurrentFeature; // Escribir último feature
  inherited;
end;

function TGherkinReporter.SanitizeFileName(const Name: string): string;
var
  C: Char;
begin
  Result := '';
  // Excluir solo caracteres ilegales en Windows: \ / : * ? " < > |
  for C in Name do
    if not CharInSet(C, ['\', '/', ':', '*', '?', '"', '<', '>', '|']) then
      Result := Result + C;
  Result := Result.Trim.Replace(' ', '_', [rfReplaceAll]);
  if Result = '' then
    Result := 'Feature';
end;

procedure TGherkinReporter.FlushCurrentFeature;
var
  FileName, FilePath: string;
begin
  if (FCurrentFeatureName = '') or (FLines.Count = 0) then
    Exit;
  FileName := SanitizeFileName(FCurrentFeatureName) + '.feature';
  FilePath := TPath.Combine(FOutputDir, FileName);
  TFile.WriteAllText(FilePath, FLines.Text, TEncoding.UTF8);
  FFilesWritten.Add(FilePath);
  FLines.Clear;
  FCurrentFeatureName := '';
end;

procedure TGherkinReporter.AddLine(const Text: string);
begin
  FLines.Add(StringOfChar(' ', FIndent * 2) + Text);
end;

procedure TGherkinReporter.AddTags(const Tags: TSpecTags);
var
  TagArray: TArray<string>;
begin
  TagArray := Tags.ToArray;
  if Length(TagArray) > 0 then
    AddLine('@' + string.Join(' @', TagArray));
end;

function TGherkinReporter.GetGherkinKeyword(Kind: TSpecItemKind): string;
begin
  case Kind of
    sikFeature: Result := 'Feature:';
    sikRule: Result := 'Rule:';
    sikBackground: Result := 'Background:';
    sikScenario: Result := 'Scenario:';
    sikScenarioOutline: Result := 'Scenario Outline:';
    sikGiven: Result := 'Given';
    sikWhen: Result := 'When';
    sikThen: Result := 'Then';
  else
    Result := '';
  end;
end;

function TGherkinReporter.RestorePlaceholders(const Text: string): string;
begin
  // Convertir #{value} a <value>
  Result := TRegEx.Replace(Text, '#\{([^}]+)\}', '<$1>');
end;

function TGherkinReporter.IsTagLine(const Line: string): Boolean;
begin
  // Una línea es de tags si empieza con @ y solo contiene tags
  Result := Line.Trim.StartsWith('@');
end;

function TGherkinReporter.ExtractTitle(const Desc: string): string;
var
  Lines: TArray<string>;
  Line: string;
begin
  Lines := Desc.Split([#13#10, #10]);
  for Line in Lines do
    if (Line.Trim <> '') and not IsTagLine(Line) then
      Exit(Line.Trim);
  Result := Desc.Trim;
end;

function TGherkinReporter.ExtractMotivation(const Desc: string): string;
var
  Lines: TArray<string>;
  FoundTitle: Boolean;
  Motivation: TStringList;
  Line: string;
begin
  Lines := Desc.Split([#13#10, #10]);
  FoundTitle := False;
  Motivation := TStringList.Create;
  try
    for Line in Lines do
    begin
      // Saltar líneas de tags
      if IsTagLine(Line) then
        Continue;
      if not FoundTitle then
      begin
        if Line.Trim <> '' then
          FoundTitle := True;
      end
      else
        Motivation.Add(Line);
    end;
    // Trim empty lines at start and end
    while (Motivation.Count > 0) and (Motivation[0].Trim = '') do
      Motivation.Delete(0);
    while (Motivation.Count > 0) and (Motivation[Motivation.Count - 1].Trim = '') do
      Motivation.Delete(Motivation.Count - 1);
    Result := Motivation.Text.TrimRight;
  finally
    Motivation.Free;
  end;
end;

function TGherkinReporter.ResultComment(const Item: ISpecItem): string;
begin
  if not FWithResults then
    Exit('');
  if Item.RunInfo.State <> srsFinished then
    Exit(' # skip');
  if Item.RunInfo.IsSuccess then
    Result := ' # pass'
  else
    Result := ' # FAIL';
end;

procedure TGherkinReporter.WriteExamplesTable(const Outline: IScenarioOutline);
var
  ColWidths: TArray<Integer>;
  Headers: TArray<string>;
  HeaderLine, DataLine: string;
  i: Integer;
  Example: IScenario;
  Values: TArray<TValue>;
begin
  Headers := Outline.Headers;
  SetLength(ColWidths, Length(Headers));

  // Calculate column widths
  for i := 0 to High(Headers) do
    ColWidths[i] := Length(Headers[i]);

  for Example in Outline.Examples do
  begin
    Values := Example.ExampleMeta.Values;
    for i := 0 to High(Values) do
      if i <= High(ColWidths) then
        if Length(Values[i].ToString) > ColWidths[i] then
          ColWidths[i] := Length(Values[i].ToString);
  end;

  // Write header row
  HeaderLine := '|';
  for i := 0 to High(Headers) do
    HeaderLine := HeaderLine + ' ' + Headers[i].PadRight(ColWidths[i]) + ' |';
  AddLine(HeaderLine);

  // Write data rows
  for Example in Outline.Examples do
  begin
    Values := Example.ExampleMeta.Values;
    DataLine := '|';
    for i := 0 to High(Values) do
      if i <= High(ColWidths) then
        DataLine := DataLine + ' ' + Values[i].ToString.PadRight(ColWidths[i]) + ' |';
    AddLine(DataLine + ResultComment(Example));
  end;
end;

procedure TGherkinReporter.DoReport(const S: ISpecItem);
var
  Motivation: string;
  Line: string;
  FeatureTitle: string;
begin
  // NO llamar inherited - no queremos contar pass/fail
  case S.Kind of
    sikFeature: begin
      // Escribir feature anterior antes de empezar nuevo
      FlushCurrentFeature;
      // Empezar nuevo feature
      FeatureTitle := ExtractTitle(S.Description);
      FCurrentFeatureName := FeatureTitle;
      FIndent := 0;
      AddTags(S.Tags);
      AddLine('Feature: ' + FeatureTitle);
      Motivation := ExtractMotivation(S.Description);
      if Motivation <> '' then
      begin
        Inc(FIndent);
        for Line in Motivation.Split([#13#10, #10]) do
          AddLine(Line);
        Dec(FIndent);
      end;
      AddLine('');
      FIndent := 1; // Scenarios at indent 1
    end;

    sikRule: begin
      FIndent := 1;
      AddLine('');
      AddTags(S.Tags);
      AddLine('Rule: ' + S.Description);
      FIndent := 2; // Scenarios under Rule at indent 2
    end;

    sikBackground: begin
      AddLine('Background:' + ResultComment(S));
      FIndent := 2; // Steps at indent 2
    end;

    sikScenario: begin
      FIndent := 1; // Reset to scenario level
      AddLine('');
      AddTags(S.Tags);
      AddLine('Scenario: ' + S.Description + ResultComment(S));
      FIndent := 2; // Steps at indent 2
    end;

    sikImplicitRule: ; // Ignorar rule implícita

    sikGiven, sikWhen, sikThen: begin
      AddLine(GetGherkinKeyword(S.Kind) + ' ' + S.Description + ResultComment(S));
    end;
  end;
end;

procedure TGherkinReporter.ReportOutline(const Outline: IScenarioOutline);
var
  Step: IScenarioStep;
begin
  FIndent := 1; // Reset to scenario level
  AddLine('');
  AddTags((Outline as ISpecItem).Tags);
  AddLine('Scenario Outline: ' + Outline.Description + ResultComment(Outline as ISpecItem));
  FIndent := 2; // Steps at indent 2

  // Steps template con placeholders <name>
  for Step in Outline.StepsGiven do
    AddLine(GetGherkinKeyword(Step.Kind) + ' ' + RestorePlaceholders(Step.Description));
  for Step in Outline.StepsWhen do
    AddLine(GetGherkinKeyword(Step.Kind) + ' ' + RestorePlaceholders(Step.Description));
  for Step in Outline.StepsThen do
    AddLine(GetGherkinKeyword(Step.Kind) + ' ' + RestorePlaceholders(Step.Description));

  // Tabla Examples
  AddLine('');
  AddLine('Examples:');
  FIndent := 3;
  WriteExamplesTable(Outline);
end;

function TGherkinReporter.GetContent: string;
var
  i: Integer;
  FileName, FeatureName: string;
begin
  // Devuelve índice en formato Quarto con include-code-files
  if FFilesWritten.Count = 0 then
    Result := '# MiniSpec' + sLineBreak + sLineBreak + 'No features generated.'
  else
  begin
    Result := '---' + sLineBreak +
              'title: "MiniSpec Features"' + sLineBreak +
              '---' + sLineBreak + sLineBreak +
              'Generated **' + IntToStr(FFilesWritten.Count) + '** features.' + sLineBreak + sLineBreak;
    for i := 0 to FFilesWritten.Count - 1 do
    begin
      FileName := ExtractFileName(FFilesWritten[i]);
      FeatureName := ChangeFileExt(FileName, '').Replace('_', ' ', [rfReplaceAll]);
      Result := Result + '## ' + FeatureName + sLineBreak + sLineBreak +
                '```{.gherkin include="' + FileName + '"}' + sLineBreak +
                '```' + sLineBreak + sLineBreak;
    end;
  end;
end;

function TGherkinReporter.GetFileExt: string;
begin
  Result := 'md';
end;

end.
