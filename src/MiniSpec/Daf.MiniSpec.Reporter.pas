unit Daf.MiniSpec.Reporter;

interface
uses
  System.RegularExpressions,
  System.Generics.Collections,
  System.SysUtils,
  System.JSON,
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
    function GetLevel(const Kind: TSpecItemKind): Byte;
    function GetKeyWord(const Kind: TSpecItemKind): string;
    function GetFileExt: string;virtual;
    procedure DoReport(const S: ISpecItem);virtual;
    procedure Report(Feature: IFeature);overload;
    procedure Report(Background: IBackground);overload;
    procedure Report(Scenario: IScenario);overload;
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

implementation
uses
  System.StrUtils,
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

            // Aplicar filtros
            if (isPass && !filters.pass) return;
            if (isFail && !filters.fail) return;
            if (isSkip && !filters.skip) return;

            hasVisibleScenarios = true;
            let resultClass = isSkip ? 'skip' : (isFail ? 'fail' : 'pass');
            featureHtml += `<details ${isFail ? 'open' : ''} style="margin-bottom:0;">`;
            featureHtml += `<summary class="${resultClass}" style="font-size:0.93em;">${isSkip ? '?' : ''} ${scenario.kind}: ${highlightPlaceholders(preserveLineBreaks(htmlEncode(scenario.description)))}</summary>`;
            featureHtml += `<ul>`;
            if (scenario.steps && !isSkip) {
              scenario.steps.forEach(function(step) {
                let stepClass = step.success ? "pass" : "fail";
                let errorMsg = step.success ? "" : ` <span class="fail"><br>ERROR:"${htmlEncode(step.error || "")}"</span>`;
                featureHtml += `<li class="${stepClass}">${htmlEncode(step.kind)}: ${highlightPlaceholders(preserveLineBreaks(htmlEncode(step.description)))} <span class="duration">(${step.duration} ms)</span>${errorMsg}</li>`;
              });
            }
            featureHtml += `</ul>`;
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
begin
  // Solo reportar la Feature si tiene al menos un escenario ejecutado
  var HasExecutedScenario := False;
  for var Scenario in Feature.Scenarios do
    if Scenario.RunInfo.State = srsFinished then
    begin
      HasExecutedScenario := True;
      Break;
    end;

  if not HasExecutedScenario then
    Exit;

  DoReport(Feature);
  Report(Feature.BackGround);
  for var Scenario in Feature.Scenarios do
    Report(Scenario);
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
    sikBackground: Result :=  'Background';
    sikScenario: Result :=  'Scenario';
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
    sikBackground, sikScenario, sikExample: Result :=  1;
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
    sikBackground: Scenario(GetKeyWord(s.Kind), S.Description, GetStatus(S), S.RunInfo.ExecTimeMs, S.RunInfo.ErrMsg);
    sikScenario, sikExample: Scenario(GetKeyWord(s.Kind), S.Description, GetStatus(S), S.RunInfo.ExecTimeMs, S.RunInfo.ErrMsg);
    sikExampleInit: ;
    sikGiven,
    sikWhen,
    sikThen: Step(S, S.RunInfo.Result = srrSuccess, S.RunInfo.ExecTimeMs, S.RunInfo.ErrMsg);
  end;
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

end.
