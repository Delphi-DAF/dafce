unit Daf.MiniSpec.Reporter;

interface
uses
  System.Classes,
  System.RegularExpressions,
  System.Generics.Collections,
  System.SysUtils,
  System.JSON,
  System.Rtti,
  System.SyncObjs,
  IdHTTPServer,
  IdCustomHTTPServer,
  IdContext,
  Daf.MiniSpec.Types;

type
  /// <summary>
  /// Opciones de reporte pasadas desde la línea de comandos.
  /// Permite opciones estándar (DryRun, TagMatcher) y específicas por reporter.
  /// </summary>
  TReportOptions = class
  public const
    OPT_DRY_RUN = 'DryRun';
  strict private
    FOptions: TDictionary<string, TValue>;
    FTagMatcher: TTagMatcher;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetOption(const Key: string; const Value: TValue);
    function GetOption(const Key: string; const Default: TValue): TValue;
    function HasOption(const Key: string): Boolean;
    // Helpers para opciones comunes
    function DryRun: Boolean;
    property TagMatcher: TTagMatcher read FTagMatcher write FTagMatcher;
  end;

  /// <summary>
  /// Value object that encapsulates pass/fail/skip counters and elapsed time.
  /// Used at Report, Feature, Scenario, and Outline levels.
  /// </summary>
  TSpecCounters = record
  private
    FPassCount: Cardinal;
    FFailCount: Cardinal;
    FSkipCount: Cardinal;
    FStartTime: TDateTime;
    FElapsedMs: Integer;
  public
    procedure Reset;
    procedure Start;
    procedure Stop;
    procedure IncPass;
    procedure IncFail;
    procedure IncSkip;
    function IsSuccess: Boolean;
    property PassCount: Cardinal read FPassCount;
    property FailCount: Cardinal read FFailCount;
    property SkipCount: Cardinal read FSkipCount;
    property ElapsedMs: Integer read FElapsedMs;
  end;

  ISpecReporter = interface(IInvokable)
    ['{CD69B272-5B38-4CCC-A64F-2B2A57ACB540}']
    function GetFailCount: Cardinal;
    function GetPassCount: Cardinal;
    function GetSkipCount: Cardinal;
    function GetCompletedAt: TDateTime;
    procedure Report(Features: TList<IFeature>; Options: TReportOptions);
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
    property CompletedAt: TDateTime read GetCompletedAt;
  end;

  TCustomReporter = class(TInterfacedObject, ISpecReporter)
  strict private
    // Counters at different levels
    FReportCounters: TSpecCounters;
    FFeatureCounters: TSpecCounters;
    FScenarioCounters: TSpecCounters;
    FOutlineCounters: TSpecCounters;
    // Current context
    FCurrentFeature: IFeature;
    FCurrentScenario: IScenario;
    FCurrentOutline: IScenarioOutline;
    // Report options
    FOptions: TReportOptions;
    // Timestamp
    FCompletedAt: TDateTime;
  protected
    function GetContent: string;virtual;abstract;
    function GetFailCount: Cardinal;virtual;
    function GetPassCount: Cardinal;virtual;
    function GetSkipCount: Cardinal;virtual;
    function GetCompletedAt: TDateTime;virtual;
    function GetLevel(const Kind: TSpecItemKind): Byte;virtual;
    function GetKeyWord(const Kind: TSpecItemKind): string;virtual;
    function GetFileExt: string;virtual;
    procedure DoReport(const S: ISpecItem);virtual;
    // Template Methods (non-virtual) - define the algorithm
    procedure BeginFeature(const Feature: IFeature);
    procedure EndFeature(const Feature: IFeature);
    procedure BeginScenario(const Scenario: IScenario);
    procedure EndScenario(const Scenario: IScenario);
    procedure BeginOutline(const Outline: IScenarioOutline);
    procedure EndOutline(const Outline: IScenarioOutline);
    // Hooks (virtual) - extension points for subclasses
    procedure DoFeatureBegin(const Feature: IFeature);virtual;
    procedure DoFeatureEnd(const Feature: IFeature; const Counters: TSpecCounters);virtual;
    procedure DoScenarioBegin(const Scenario: IScenario);virtual;
    procedure DoScenarioEnd(const Scenario: IScenario; const Counters: TSpecCounters);virtual;
    procedure DoOutlineBegin(const Outline: IScenarioOutline);virtual;
    procedure DoOutlineEnd(const Outline: IScenarioOutline; const Counters: TSpecCounters);virtual;
    // Reporting methods
    procedure Report(Feature: IFeature);overload;
    procedure Report(Rule: IRule);overload;
    procedure Report(Background: IBackground);overload;
    procedure Report(Scenario: IScenario);overload;
    procedure ReportOutline(const Outline: IScenarioOutline);virtual;
    // Properties for current context
    property CurrentFeature: IFeature read FCurrentFeature;
    property CurrentScenario: IScenario read FCurrentScenario;
    property CurrentOutline: IScenarioOutline read FCurrentOutline;
    property ReportCounters: TSpecCounters read FReportCounters;
    property FeatureCounters: TSpecCounters read FFeatureCounters;
    property ScenarioCounters: TSpecCounters read FScenarioCounters;
    property OutlineCounters: TSpecCounters read FOutlineCounters;
    property Options: TReportOptions read FOptions;
  public
    function UseConsole: Boolean;virtual;
    procedure BeginReport;virtual;
    procedure Report(Features: TList<IFeature>; Options: TReportOptions);overload;
    procedure EndReport;virtual;
    property PassCount: Cardinal read GetPassCount;
    property FailCount: Cardinal read GetFailCount;
    property SkipCount: Cardinal read GetSkipCount;
    property CompletedAt: TDateTime read FCompletedAt;
  end;

  TReporterDecorator = class(TCustomReporter)
  strict private
    FDecorated: ISpecReporter;
  protected
    function GetFailCount: Cardinal;override;
    function GetPassCount: Cardinal;override;
    function GetSkipCount: Cardinal;override;
    function GetCompletedAt: TDateTime;override;
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
    procedure Feature(const Title, Narrative: string);
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

  TLiveReporter = class(TReporterDecorator)
  private
    FServer: TIdHTTPServer;
    FPort: Integer;
    FEvents: TStringList;
    FEventsLock: TCriticalSection;
    FLiveClients: TList<TIdContext>;
    FClientsLock: TCriticalSection;
    FReportFinished: Boolean;
    FScenarioCount: Integer; // Contador de escenarios emitidos
    procedure HandleRequest(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure BroadcastEvent(const EventJson: string);
    function BuildEventJson(const EventType: string; const Data: TJSONObject): string;
    function HasConnectedClients: Boolean;
    function StepsToJsonArray(const Steps: TList<IScenarioStep>; const StepType: string): TJSONArray;
  protected
    procedure DoReport(const S: ISpecItem);override;
    procedure DoFeatureBegin(const Feature: IFeature);override;
    procedure DoFeatureEnd(const Feature: IFeature; const Counters: TSpecCounters);override;
    procedure DoScenarioEnd(const Scenario: IScenario; const Counters: TSpecCounters);override;
    procedure DoOutlineBegin(const Outline: IScenarioOutline);override;
    procedure DoOutlineEnd(const Outline: IScenarioOutline; const Counters: TSpecCounters);override;
  public
    constructor Create(const Decorated: ISpecReporter; APort: Integer = 9999);
    destructor Destroy;override;
    procedure BeginReport;override;
    procedure EndReport;override;
    function GetContent: string;override;
    property Port: Integer read FPort;
  end;

implementation
uses
  System.StrUtils,
  System.IOUtils,
  IdGlobal,
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
    function renderFeatureHeader(title, narrative) {
      let html = `<h2 class="feature">Feature: ${htmlEncode(title || '')}</h2>`;
      if (narrative && narrative.trim()) {
        html += `<details class="motivation-block"><summary>Motivation</summary><pre class="motivation">${highlightPlaceholders(htmlEncode(narrative))}</pre></details>`;
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

            // Para Scenario Outline, verificar si alg�n example fall�
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
            featureHtml += `<summary class="${resultClass}" style="font-size:0.93em;">${isSkip ? '?' : ''} ${scenario.kind}: ${highlightPlaceholders(preserveLineBreaks(htmlEncode(scenario.description)))}</summary>`;

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
                let icon = ex.status === 'pass' ? '?' : '?';
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
          html += renderFeatureHeader(feature.name, feature.narrative);
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

{$REGION 'Live Dashboard HTML'}
const
  LIVE_DASHBOARD_HTML = '''
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>MiniSpec Live</title>
  <script src="https://cdn.tailwindcss.com"></script>
  <script defer src="https://unpkg.com/alpinejs@3.x.x/dist/cdn.min.js"></script>
</head>
<body class="bg-gray-900 text-white min-h-screen">
  <div x-data="dashboard()" x-init="init()" class="container mx-auto p-6">
    <div class="flex justify-between items-center mb-6">
      <h1 class="text-3xl font-bold flex items-center gap-3">
        <svg width="40" height="40" viewBox="0 0 256 256" fill="none" xmlns="http://www.w3.org/2000/svg">
          <rect width="256" height="256" rx="38" fill="#242526"/>
          <text x="128" y="74" text-anchor="middle" font-family="Segoe UI, Arial, sans-serif" font-size="44" fill="#fff" font-weight="bold">mSpec</text>
          <text x="38" y="172" font-family="Fira Mono, Consolas, monospace" font-size="54" fill="#cfcfcf">$&gt;</text>
          <text x="110" y="172" font-family="Segoe UI Emoji" font-size="54" fill="#c92828">?</text>
          <text x="140" y="172" font-family="Fira Mono, Consolas, monospace" font-size="54" fill="#bdbdbd">.</text>
          <text x="160" y="172" font-family="Fira Mono, Consolas, monospace" font-size="54" fill="#bdbdbd">.</text>
          <text x="190" y="172" font-family="Segoe UI Emoji" font-size="54" fill="#34bf4a">?</text>
        </svg>
        <span>MiniSpec Live <span class="text-sm text-gray-500 font-normal">v{{MINISPEC_VERSION}}</span></span>
      </h1>
      <div class="flex items-center gap-4">
        <span x-show="connected" class="text-green-400 flex items-center gap-1">
          <span class="w-2 h-2 bg-green-400 rounded-full animate-pulse"></span> Connected
        </span>
        <span x-show="!connected" class="text-red-400 flex items-center gap-1">
          <span class="w-2 h-2 bg-red-400 rounded-full"></span> Disconnected
        </span>
      </div>
    </div>

    <!-- Progress Bar -->
    <div class="mb-4 bg-gray-800 rounded-lg p-4">
      <div class="w-full bg-gray-700 rounded-full h-4 overflow-hidden">
        <div class="h-full flex">
          <div class="bg-green-500 transition-all duration-300" :style="`width: ${passPercent}%`"></div>
          <div class="bg-red-500 transition-all duration-300" :style="`width: ${failPercent}%`"></div>
          <div class="bg-yellow-500 transition-all duration-300" :style="`width: ${skipPercent}%`"></div>
        </div>
      </div>
      <div class="flex justify-center gap-6 mt-2 text-sm">
        <span class="text-green-400" x-text="`✓ ${pass} pass`"></span>
        <span class="text-red-400" x-text="`✗ ${fail} fail`"></span>
        <span class="text-yellow-400" x-show="skip > 0" x-text="`○ ${skip} skip`"></span>
        <span class="text-gray-400" x-text="`${totalTests} scenarios in ${features.length} features`"></span>
        <span class="text-gray-500" x-show="reportComplete &amp;&amp; completedAt" x-text="`at ${completedAt}`"></span>
      </div>
    </div>

    <!-- Filters Panel -->
    <div class="mb-4 bg-gray-800 rounded-lg p-4">
      <div class="flex flex-wrap items-center gap-4">
        <!-- Status Filters -->
        <div class="flex items-center gap-3">
          <label class="flex items-center gap-1 cursor-pointer">
            <input type="checkbox" x-model="showPass" class="w-4 h-4 accent-green-500">
            <span class="text-green-400 text-sm">Pass</span>
          </label>
          <label class="flex items-center gap-1 cursor-pointer">
            <input type="checkbox" x-model="showFail" class="w-4 h-4 accent-red-500">
            <span class="text-red-400 text-sm">Fail</span>
          </label>
          <label class="flex items-center gap-1 cursor-pointer">
            <input type="checkbox" x-model="showSkip" class="w-4 h-4 accent-yellow-500">
            <span class="text-yellow-400 text-sm">Skip</span>
          </label>
        </div>

        <!-- Regex Filter -->
        <div class="flex items-center gap-2 flex-1 min-w-[200px]">
          <span class="text-gray-400 text-sm">🔍</span>
          <input type="text" x-model="filterRegex" placeholder="Filter by regex..."
                 class="bg-gray-700 border border-gray-600 rounded px-2 py-1 text-sm flex-1 focus:outline-none focus:border-blue-500">
          <button x-show="filterRegex" @click="filterRegex = ''" class="text-gray-400 hover:text-white text-sm">✕</button>
        </div>

        <!-- Tag Filter -->
        <div class="flex items-center gap-2">
          <span class="text-gray-400 text-sm">🏷️</span>
          <select x-model="filterTag" class="bg-gray-700 border border-gray-600 rounded px-2 py-1 text-sm focus:outline-none focus:border-blue-500">
            <option value="">All tags</option>
            <template x-for="tag in allTags" :key="tag">
              <option :value="tag" x-text="tag"></option>
            </template>
          </select>
        </div>
      </div>
    </div>

    <!-- Quick Actions: First Failure & Top 10 Slowest -->
    <div class="mb-4 flex gap-4" x-show="reportComplete">
      <!-- First Failure -->
      <template x-if="firstFailure">
        <button @click="scrollToFeature(firstFailure.featureName)"
                class="bg-red-900/50 border border-red-700 rounded-lg px-4 py-2 text-sm hover:bg-red-900/70 transition flex items-center gap-2">
          <span>⚡</span>
          <span>First Failure: <span class="font-medium" x-text="firstFailure.scenarioName"></span></span>
        </button>
      </template>

      <!-- Top 10 Slowest Toggle -->
      <button @click="showSlowest = !showSlowest"
              class="bg-yellow-900/50 border border-yellow-700 rounded-lg px-4 py-2 text-sm hover:bg-yellow-900/70 transition flex items-center gap-2">
        <span>🐢</span>
        <span x-text="showSlowest ? 'Hide Slowest' : 'Top 10 Slowest'"></span>
      </button>
    </div>

    <!-- Top 10 Slowest Panel -->
    <template x-if="showSlowest && reportComplete">
      <div class="mb-4 bg-yellow-900/30 border border-yellow-700 rounded-lg p-4">
        <div class="text-yellow-400 font-medium mb-2">🐢 Top 10 Slowest Scenarios</div>
        <div class="space-y-1 text-sm">
          <template x-for="(item, idx) in slowestScenarios" :key="idx">
            <div class="flex items-center gap-2 py-1">
              <span class="text-gray-500 w-6" x-text="`#${idx + 1}`"></span>
              <span class="text-yellow-300 w-16 text-right" x-text="`${item.ms}ms`"></span>
              <span :class="item.success ? 'text-green-400' : 'text-red-400'" x-text="item.success ? '✓' : '✗'"></span>
              <span class="text-gray-300 truncate" x-text="item.name"></span>
              <span class="text-gray-500 text-xs" x-text="`(${item.feature})`"></span>
            </div>
          </template>
        </div>
      </div>
    </template>

    <!-- Current Feature -->
    <template x-if="currentFeature">
      <div class="bg-yellow-900/30 border border-yellow-600 rounded-lg p-4 mb-4 animate-pulse">
        <div class="flex items-center gap-2">
          <span class="text-yellow-400 text-xl">▶</span>
          <span class="font-semibold" x-text="currentFeature"></span>
        </div>
        <div x-show="currentScenario" class="ml-6 mt-2 text-gray-400">
          <span x-text="currentScenario"></span>
        </div>
      </div>
    </template>

    <!-- Completed Features -->
    <div class="space-y-2">
      <template x-for="feature in filteredFeatures" :key="feature.name">
        <div :id="`feature-${feature.name.replace(/\\s+/g, '-')}`"
             :class="feature.success ? 'bg-green-900/30 border-green-700' : 'bg-red-900/30 border-red-700'"
             class="border rounded-lg p-4">
          <details>
            <summary class="cursor-pointer list-none flex justify-between items-center [&::-webkit-details-marker]:hidden">
              <div class="flex items-center gap-2">
                <span class="text-gray-400 text-xs">▸</span>
                <span x-text="feature.success ? '✓' : '✗'" class="text-xl"></span>
                <span class="font-semibold" x-text="feature.name"></span>
                <span class="text-gray-500 text-sm" x-text="`(${feature.pass + feature.fail} examples)`"></span>
              </div>
              <div class="text-gray-400 text-sm">
                <span class="text-green-400" x-text="`${feature.pass}✓`"></span>
                <span class="text-red-400 ml-1" x-text="`${feature.fail}✗`"></span>
                <span class="ml-2" x-text="`${feature.ms}ms`"></span>
              </div>
            </summary>
            <div class="mt-3 ml-4 space-y-3 border-l border-gray-700 pl-4 max-h-[600px] overflow-y-auto">
              <!-- Narrativa del feature -->
              <template x-if="feature.narrative">
                <div class="text-gray-400 text-sm italic whitespace-pre-line" x-text="feature.narrative"></div>
              </template>

              <!-- Background -->
              <template x-if="feature.background && feature.background.length > 0">
                <div class="mb-3">
                  <div class="text-gray-300 font-medium mb-1">Background</div>
                  <div class="ml-4 space-y-0.5 text-xs">
                    <template x-for="(step, idx) in feature.background" :key="idx">
                      <div class="flex items-start gap-2 py-0.5 text-gray-400">
                        <span class="text-blue-400 font-medium w-12 flex-shrink-0" x-text="step.type"></span>
                        <span x-text="step.text"></span>
                      </div>
                    </template>
                  </div>
                </div>
              </template>

              <div x-show="feature.scenarios.length === 0" class="text-gray-500 text-sm italic">No scenarios recorded</div>

              <template x-for="(scenario, idx) in feature.scenarios" :key="idx">
                <div class="mb-3" x-show="(scenario.skipped && showSkip) || (!scenario.skipped && scenario.success && showPass) || (!scenario.skipped && !scenario.success && showFail)">
                  <!-- Scenario Outline con tabla -->
                  <template x-if="scenario.type === 'outline'">
                    <details :open="!scenario.success && !scenario.skipped">
                      <summary class="cursor-pointer list-none flex items-center gap-2 text-sm py-1 [&::-webkit-details-marker]:hidden" :class="scenario.skipped ? 'text-yellow-400' : (scenario.success ? 'text-green-400' : 'text-red-400')">
                        <span class="text-gray-500 text-xs">▸</span>
                        <span x-text="scenario.skipped ? '○' : (scenario.success ? '✓' : '✗')" class="w-4"></span>
                        <span class="font-medium">Scenario Outline:</span>
                        <span x-text="scenario.name" class="flex-1"></span>
                        <span x-show="!scenario.skipped" class="text-gray-500 text-xs" x-text="`${scenario.pass}✓ ${scenario.fail}✗`"></span>
                        <span x-show="scenario.skipped" class="text-gray-500 text-xs">(skip)</span>
                        <span class="text-gray-500 flex-shrink-0" x-text="`${scenario.ms}ms`"></span>
                      </summary>
                      <div class="ml-6 mt-2 space-y-2 text-xs">
                        <!-- Steps template -->
                        <div class="space-y-0.5">
                          <template x-for="(step, stepIdx) in scenario.steps" :key="stepIdx">
                            <div class="flex items-start gap-2 py-0.5 text-gray-400">
                              <span class="text-blue-400 font-medium w-12 flex-shrink-0" x-text="step.type"></span>
                              <span x-text="step.text"></span>
                            </div>
                          </template>
                        </div>
                        <!-- Examples table -->
                        <div class="mt-2">
                          <div class="text-gray-300 font-medium mb-1">Examples:</div>
                          <table class="text-xs border-collapse">
                            <thead>
                              <tr class="text-gray-400">
                                <th class="pr-2 text-left"></th>
                                <template x-for="header in scenario.headers" :key="header">
                                  <th class="px-2 py-1 text-left border-b border-gray-700" x-text="header"></th>
                                </template>
                                <th class="px-2 py-1 text-left border-b border-gray-700">ms</th>
                              </tr>
                            </thead>
                            <tbody>
                              <template x-for="(ex, exIdx) in scenario.examples" :key="exIdx">
                                <tr :class="ex.skipped ? 'text-yellow-400' : (ex.success ? 'text-green-400' : 'text-red-400')">
                                  <td class="pr-2" x-text="ex.skipped ? '○' : (ex.success ? '✓' : '✗')"></td>
                                  <template x-for="(val, valIdx) in ex.values" :key="valIdx">
                                    <td class="px-2 py-0.5 border-b border-gray-800" x-text="val"></td>
                                  </template>
                                  <td class="px-2 py-0.5 border-b border-gray-800 text-gray-500" x-text="ex.skipped ? 'skip' : ex.ms"></td>
                                </tr>
                              </template>
                            </tbody>
                          </table>
                        </div>
                        <!-- Errors -->
                        <template x-for="(ex, exIdx) in scenario.examples.filter(e => e.error)" :key="'err'+exIdx">
                          <div class="mt-1 p-2 bg-red-900/50 border border-red-700 rounded text-red-300 font-mono text-xs whitespace-pre-wrap" x-text="ex.error"></div>
                        </template>
                      </div>
                    </details>
                  </template>

                  <!-- Scenario normal -->
                  <template x-if="scenario.type !== 'outline'">
                    <details :open="!scenario.success && !scenario.skipped">
                      <summary class="cursor-pointer list-none flex items-center gap-2 text-sm py-1 [&::-webkit-details-marker]:hidden" :class="scenario.skipped ? 'text-yellow-400' : (scenario.success ? 'text-green-400' : 'text-red-400')">
                        <span class="text-gray-500 text-xs">▸</span>
                        <span x-text="scenario.skipped ? '○' : (scenario.success ? '✓' : '✗')" class="w-4"></span>
                        <span class="font-medium">Scenario:</span>
                        <span x-text="scenario.name" class="flex-1"></span>
                        <span x-show="scenario.skipped" class="text-gray-500 text-xs">(skip)</span>
                        <span class="text-gray-500 flex-shrink-0" x-text="`${scenario.ms}ms`"></span>
                      </summary>
                      <div class="ml-6 mt-1 space-y-1 text-xs">
                        <template x-if="scenario.steps && scenario.steps.length > 0">
                          <div class="space-y-0.5">
                            <template x-for="(step, stepIdx) in scenario.steps" :key="stepIdx">
                              <div class="flex items-start gap-2 py-0.5" :class="step.success ? 'text-gray-400' : 'text-red-400'">
                                <span x-text="step.success ? '✓' : '✗'" class="w-3 flex-shrink-0"></span>
                                <span class="text-blue-400 font-medium w-12 flex-shrink-0" x-text="step.type"></span>
                                <span x-text="step.text" class="flex-1"></span>
                                <span class="text-gray-600" x-text="`${step.ms}ms`"></span>
                              </div>
                            </template>
                          </div>
                        </template>
                        <template x-if="scenario.error">
                          <div class="mt-2 p-2 bg-red-900/50 border border-red-700 rounded text-red-300 font-mono text-xs whitespace-pre-wrap" x-text="scenario.error"></div>
                        </template>
                      </div>
                    </details>
                  </template>
                </div>
              </template>
            </div>
          </details>
        </div>
      </template>
    </div>

  </div>

  <script>
    function dashboard() {
      return {
        connected: false,
        features: [],
        currentFeature: null,
        currentFeatureNarrative: null,
        currentFeatureBackground: null,
        currentFeatureTags: [],
        currentFeatureScenarios: [],
        currentScenario: null,
        pass: 0,
        fail: 0,
        skip: 0,
        completedAt: null,
        reportComplete: false,
        eventSource: null,

        // Filters
        showPass: true,
        showFail: true,
        showSkip: true,
        filterRegex: '',
        filterTag: '',
        showSlowest: false,

        get totalTests() {
          return this.pass + this.fail + this.skip;
        },
        get executedTests() {
          return this.pass + this.fail;
        },
        get passPercent() {
          const total = this.totalTests;
          return total > 0 ? (this.pass / total * 100) : 0;
        },
        get failPercent() {
          const total = this.totalTests;
          return total > 0 ? (this.fail / total * 100) : 0;
        },
        get skipPercent() {
          const total = this.totalTests;
          return total > 0 ? (this.skip / total * 100) : 0;
        },

        get allTags() {
          const tags = new Set();
          for (const f of this.features) {
            if (f.tags) f.tags.forEach(t => tags.add(t));
            for (const s of f.scenarios || []) {
              if (s.tags) s.tags.forEach(t => tags.add(t));
            }
          }
          return Array.from(tags).sort();
        },

        get filteredFeatures() {
          let regex = null;
          if (this.filterRegex) {
            try { regex = new RegExp(this.filterRegex, 'i'); } catch(e) {}
          }

          return this.features.filter(f => {
            // Tag filter
            if (this.filterTag && (!f.tags || !f.tags.includes(this.filterTag))) {
              // Check scenarios for tag
              const hasTagInScenarios = f.scenarios?.some(s => s.tags?.includes(this.filterTag));
              if (!hasTagInScenarios) return false;
            }

            // Regex filter
            if (regex && !regex.test(f.name) && !regex.test(f.description || '')) {
              const matchesScenario = f.scenarios?.some(s => regex.test(s.name));
              if (!matchesScenario) return false;
            }

            // Status filter
            if (f.success && !this.showPass) return false;
            if (!f.success && !this.showFail) return false;

            return true;
          });
        },

        get firstFailure() {
          for (const f of this.features) {
            for (const s of f.scenarios || []) {
              if (!s.success) {
                return { featureName: f.name, scenarioName: s.name };
              }
            }
          }
          return null;
        },

        get slowestScenarios() {
          const all = [];
          for (const f of this.features) {
            for (const s of f.scenarios || []) {
              if (s.type === 'outline') {
                // Para outlines, usar el tiempo total
                all.push({ name: s.name, ms: s.ms, success: s.success, feature: f.name });
              } else {
                all.push({ name: s.name, ms: s.ms, success: s.success, feature: f.name });
              }
            }
          }
          return all.sort((a, b) => b.ms - a.ms).slice(0, 10);
        },

        scrollToFeature(name) {
          const id = 'feature-' + name.replace(/\\s+/g, '-');
          const el = document.getElementById(id);
          if (el) {
            el.scrollIntoView({ behavior: 'smooth', block: 'center' });
            el.querySelector('details')?.setAttribute('open', '');
          }
        },

        init() {
          this.connect();
        },

        connect() {
          // Cerrar conexi�n anterior si existe
          if (this.eventSource) {
            this.eventSource.close();
            this.eventSource = null;
          }

          this.eventSource = new EventSource('/events');

          this.eventSource.onopen = () => {
            this.connected = true;
          };

          this.eventSource.onerror = (e) => {
            this.connected = false;
            // Solo reconectar si no se ha completado el reporte
            if (!this.reportComplete && this.eventSource) {
              this.eventSource.close();
              this.eventSource = null;
              // Reconectar despu�s de 3 segundos
              setTimeout(() => this.connect(), 3000);
            }
          };

          this.eventSource.onmessage = (e) => {
            const data = JSON.parse(e.data);
            this.handleEvent(data);
          };
        },

        handleEvent(data) {
          switch(data.event) {
            case 'report:start':
              this.features = [];
              this.pass = 0;
              this.fail = 0;
              this.skip = 0;
              this.reportComplete = false;
              this.currentFeatureScenarios = [];
              this.currentFeatureNarrative = null;
              this.currentFeatureTags = [];
              break;
            case 'feature:start':
              this.currentFeature = data.name;
              this.currentFeatureNarrative = data.narrative || null;
              this.currentFeatureBackground = data.background || null;
              this.currentFeatureTags = data.tags || [];
              this.currentScenario = null;
              this.currentFeatureScenarios = [];
              break;
            case 'scenario:start':
              this.currentScenario = data.name;
              break;
            case 'scenario:end':
              // Manejar skip, pass y fail
              if (data.skipped) {
                this.skip++;
              } else if (data.success) {
                this.pass++;
              } else {
                this.fail++;
              }
              this.currentFeatureScenarios.push({
                type: 'scenario',
                name: data.name,
                success: data.success,
                skipped: data.skipped || false,
                ms: data.ms,
                error: data.error || null,
                steps: data.steps || []
              });
              this.currentScenario = null;
              break;
            case 'outline:end':
              // Sumar todos los examples al conteo (incluyendo skip)
              this.pass += data.pass || 0;
              this.fail += data.fail || 0;
              // Contar skipped de examples
              const skipCount = (data.examples || []).filter(e => e.skipped).length;
              this.skip += skipCount;
              this.currentFeatureScenarios.push({
                type: 'outline',
                name: data.name,
                success: data.success,
                skipped: skipCount > 0 && data.pass === 0 && data.fail === 0,
                ms: data.ms,
                pass: data.pass,
                fail: data.fail,
                skip: skipCount,
                headers: data.headers || [],
                steps: data.steps || [],
                examples: data.examples || []
              });
              break;
            case 'feature:end':
              this.features.push({
                name: data.name,
                narrative: this.currentFeatureNarrative,
                background: this.currentFeatureBackground,
                tags: this.currentFeatureTags,
                success: data.fail === 0,
                pass: data.pass,
                fail: data.fail,
                ms: data.ms,
                scenarios: [...this.currentFeatureScenarios]
              });
              this.currentFeature = null;
              this.currentFeatureNarrative = null;
              this.currentFeatureBackground = null;
              this.currentFeatureTags = [];
              this.currentFeatureScenarios = [];
              break;
            case 'report:end':
              this.reportComplete = true;
              this.completedAt = data.completedAt;
              this.pass = data.pass;
              this.fail = data.fail;
              this.skip = data.skip || 0;
              this.currentFeature = null;
              this.currentScenario = null;
              // Cerrar conexi�n - el reporte termin�
              if (this.eventSource) {
                this.eventSource.close();
                this.eventSource = null;
              }
              this.connected = false;
              break;
          }
        }
      }
    }
  </script>
</body>
</html>
''';
{$ENDREGION}

{ TReportOptions }

constructor TReportOptions.Create;
begin
  inherited;
  FOptions := TDictionary<string, TValue>.Create;
  FTagMatcher := nil;
end;

destructor TReportOptions.Destroy;
begin
  FOptions.Free;
  inherited;
end;

procedure TReportOptions.SetOption(const Key: string; const Value: TValue);
begin
  FOptions.AddOrSetValue(Key, Value);
end;

function TReportOptions.GetOption(const Key: string; const Default: TValue): TValue;
begin
  if not FOptions.TryGetValue(Key, Result) then
    Result := Default;
end;

function TReportOptions.HasOption(const Key: string): Boolean;
begin
  Result := FOptions.ContainsKey(Key);
end;

function TReportOptions.DryRun: Boolean;
begin
  Result := GetOption(OPT_DRY_RUN, TValue.From<Boolean>(False)).AsBoolean;
end;

{ TSpecCounters }

procedure TSpecCounters.Reset;
begin
  FPassCount := 0;
  FFailCount := 0;
  FSkipCount := 0;
  FElapsedMs := 0;
  FStartTime := 0;
end;

procedure TSpecCounters.Start;
begin
  FStartTime := Now;
end;

procedure TSpecCounters.Stop;
begin
  FElapsedMs := Round((Now - FStartTime) * 24 * 60 * 60 * 1000);
end;

procedure TSpecCounters.IncPass;
begin
  Inc(FPassCount);
end;

procedure TSpecCounters.IncFail;
begin
  Inc(FFailCount);
end;

procedure TSpecCounters.IncSkip;
begin
  Inc(FSkipCount);
end;

function TSpecCounters.IsSuccess: Boolean;
begin
  Result := FFailCount = 0;
end;

{ TCustomReporter }

procedure TCustomReporter.DoFeatureBegin(const Feature: IFeature);
begin
  // Hook - subclasses can override
end;

procedure TCustomReporter.DoFeatureEnd(const Feature: IFeature; const Counters: TSpecCounters);
begin
  // Hook - subclasses can override
end;

procedure TCustomReporter.DoScenarioBegin(const Scenario: IScenario);
begin
  // Hook - subclasses can override
end;

procedure TCustomReporter.DoScenarioEnd(const Scenario: IScenario; const Counters: TSpecCounters);
begin
  // Hook - subclasses can override
end;

procedure TCustomReporter.DoOutlineBegin(const Outline: IScenarioOutline);
begin
  // Hook - subclasses can override
end;

procedure TCustomReporter.DoOutlineEnd(const Outline: IScenarioOutline; const Counters: TSpecCounters);
begin
  // Hook - subclasses can override
end;

// Template Method: BeginFeature (non-virtual)
procedure TCustomReporter.BeginFeature(const Feature: IFeature);
begin
  FCurrentFeature := Feature;
  FFeatureCounters.Reset;
  FFeatureCounters.Start;
  DoFeatureBegin(Feature);  // Hook
end;

// Template Method: EndFeature (non-virtual)
procedure TCustomReporter.EndFeature(const Feature: IFeature);
begin
  FFeatureCounters.Stop;
  DoFeatureEnd(Feature, FFeatureCounters);  // Hook
  FCurrentFeature := nil;
end;

// Template Method: BeginScenario (non-virtual)
procedure TCustomReporter.BeginScenario(const Scenario: IScenario);
begin
  FCurrentScenario := Scenario;
  FScenarioCounters.Reset;
  FScenarioCounters.Start;
  DoScenarioBegin(Scenario);  // Hook
end;

// Template Method: EndScenario (non-virtual)
procedure TCustomReporter.EndScenario(const Scenario: IScenario);
begin
  FScenarioCounters.Stop;
  DoScenarioEnd(Scenario, FScenarioCounters);  // Hook
  FCurrentScenario := nil;
end;

// Template Method: BeginOutline (non-virtual)
procedure TCustomReporter.BeginOutline(const Outline: IScenarioOutline);
begin
  FCurrentOutline := Outline;
  FOutlineCounters.Reset;
  FOutlineCounters.Start;
  DoOutlineBegin(Outline);  // Hook
end;

// Template Method: EndOutline (non-virtual)
procedure TCustomReporter.EndOutline(const Outline: IScenarioOutline);
begin
  FOutlineCounters.Stop;
  DoOutlineEnd(Outline, FOutlineCounters);  // Hook
  FCurrentOutline := nil;
end;

procedure TCustomReporter.BeginReport;
begin
  FReportCounters.Reset;
  FFeatureCounters.Reset;
  FOutlineCounters.Reset;
  FCurrentFeature := nil;
  FCurrentScenario := nil;
  FCurrentOutline := nil;
end;

procedure TCustomReporter.Report(Feature: IFeature);
begin
  BeginFeature(Feature);
  DoReport(Feature);
  for var Rule in Feature.Rules do
    Report(Rule);
  EndFeature(Feature);
end;

procedure TCustomReporter.ReportOutline(const Outline: IScenarioOutline);
begin
  BeginOutline(Outline);

  // Count Examples basándose en el estado real
  for var Example in Outline.Examples do
  begin
    case Example.RunInfo.State of
      srsSkiped:
        FReportCounters.IncSkip;
      srsFinished:
        if Example.RunInfo.IsSuccess then
        begin
          FReportCounters.IncPass;
          FFeatureCounters.IncPass;
          FOutlineCounters.IncPass;
        end
        else
        begin
          FReportCounters.IncFail;
          FFeatureCounters.IncFail;
          FOutlineCounters.IncFail;
        end;
    end;
  end;

  EndOutline(Outline);
end;

procedure TCustomReporter.Report(Rule: IRule);
begin
  // Solo reportar la Rule si tiene al menos un escenario visitado (ejecutado o skipped)
  var HasVisitedScenario := False;
  for var Scenario in Rule.Scenarios do
    if Scenario.RunInfo.State in [srsFinished, srsSkiped] then
    begin
      HasVisitedScenario := True;
      Break;
    end;

  if not HasVisitedScenario then
    Exit;

  // Solo mostrar header si es Rule explícita (no ImplicitRule)
  if Rule.Kind = sikRule then
    DoReport(Rule);

  Report(Rule.BackGround);

  // Iterar scenarios - polimórficamente detectar Outlines
  for var Scenario in Rule.Scenarios do
  begin
    if not (Scenario.RunInfo.State in [srsFinished, srsSkiped]) then
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
  if not (Scenario.RunInfo.State in [srsFinished, srsSkiped]) then
    Exit;

  BeginScenario(Scenario);
  DoReport(Scenario);
  for var Step in Scenario.StepsGiven do
    DoReport(Step);
  for var Step in Scenario.StepsWhen do
    DoReport(Step);
  for var Step in Scenario.StepsThen do
    DoReport(Step);
  EndScenario(Scenario);
end;

procedure TCustomReporter.DoReport(const S: ISpecItem);
begin
  if (S.Kind in [sikScenario, sikExample]) then
  begin
    // Contar basándose en el estado real
    case S.RunInfo.State of
      srsSkiped:
        FReportCounters.IncSkip;
      srsFinished:
        if S.RunInfo.IsSuccess then
        begin
          FReportCounters.IncPass;
          FFeatureCounters.IncPass;
        end
        else
        begin
          FReportCounters.IncFail;
          FFeatureCounters.IncFail;
        end;
    end;
  end;
end;

procedure TCustomReporter.EndReport;
begin
  FCompletedAt := Now;
end;

function TCustomReporter.GetFailCount: Cardinal;
begin
  Result := FReportCounters.FailCount;
end;

function TCustomReporter.GetSkipCount: Cardinal;
begin
  Result := FReportCounters.SkipCount;
end;

function TCustomReporter.GetCompletedAt: TDateTime;
begin
  Result := FCompletedAt;
end;

function TCustomReporter.GetFileExt: string;
begin
  Result := '';
end;

function TCustomReporter.GetKeyWord(const Kind: TSpecItemKind): string;
begin
  case Kind of
    sikFeature: Result := 'Feature';
    sikImplicitRule: Result := '';  // No mostrar keyword para Rule impl�cita
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
    sikRule: Result := 1;  // Rule est� un nivel debajo de Feature
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
  Result := FReportCounters.PassCount;
end;

procedure TCustomReporter.Report(Features: TList<IFeature>; Options: TReportOptions);
begin
  FOptions := Options;
  BeginReport;
  for var F in Features do
    Report(F);
  EndReport;
end;

function TCustomReporter.UseConsole: Boolean;
begin
  Result := False;
end;

{ TConsoleReporter }

procedure TConsoleReporter.DoReport(const S: ISpecItem);
var
  Feat: IFeature;
  DisplayText: string;
begin
  inherited;
  var Kind := GetKeyWord(S.Kind);
  var Level := GetLevel(S.Kind);

  // Para features, mostrar solo el Title
  if (S.Kind = sikFeature) and Supports(S, IFeature, Feat) then
    DisplayText := Feat.Title
  else
    DisplayText := S.Description;

  // Manejar los 3 estados: Finished (Pass/Fail), Skipped
  if S.RunInfo.State = srsSkiped then
    OutputLn(Level, Format('- %s (skip)', [Kind + ' ' + DisplayText]))
  else
    OutputLn(Level, Kind + ' ' + DisplayText, S.RunInfo.IsSuccess, S.RunInfo.ExecTimeMs, S.RunInfo.ErrMsg);
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
    OutputLn(Level, Format('✓ %s (%d ms)', [Text, Duration]))
  else
    OutputLn(Level, Format('✗ %s (%d ms)%s', [Text, Duration, Msg]));
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
  // Primero delegar al base para conteo de estad�sticas
  inherited;

  // Ahora solo presentaci�n - sin tocar contadores
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

  // Header de la tabla (3 espacios para alinear con emoji ?)
  HeaderLine := '|';
  for i := 0 to High(Headers) do
    HeaderLine := HeaderLine + ' ' + Headers[i].PadRight(ColWidths[i]) + ' |';
  OutputLn(3, '   ' + HeaderLine);

  // Cada fila con su resultado (solo presentaci�n, sin contar)
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

procedure TJsonReporter.Feature(const Title, Narrative: string);
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
  FCurrentFeature.AddPair('title', Title);
  FCurrentFeature.AddPair('narrative', Narrative);
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

var
  Feat: IFeature;
begin
  inherited;
  case S.Kind of
    sikFeature: begin
      if Supports(S, IFeature, Feat) then
        Feature(Feat.Title, Feat.Narrative)
      else
        Feature(S.Description, '');
    end;
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
  inherited; // Primero para que CompletedAt tenga valor
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
    Root.AddPair('completedAt', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', CompletedAt));
    FOutput := Root.Format(4);
  finally
    Root.Free;
  end;
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
  inherited; // Actualiza contadores del decorator
  if not Assigned(Decorated) then Exit;
  Decorated.DoReport(S);
end;

procedure TReporterDecorator.ReportOutline(const Outline: IScenarioOutline);
begin
  inherited; // Procesa el outline localmente (contadores del decorator)
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

function TReporterDecorator.GetCompletedAt: TDateTime;
begin
  if not Assigned(Decorated) then Exit(0);
  Result := Decorated.GetCompletedAt;
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
  inherited; // Primero para que CompletedAt tenga valor
  FlushCurrentFeature; // Escribir último feature
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
  Line: string;
  Feature: IFeature;
begin
  // NO llamar inherited - no queremos contar pass/fail
  case S.Kind of
    sikFeature: begin
      // Escribir feature anterior antes de empezar nuevo
      FlushCurrentFeature;
      // Empezar nuevo feature usando IFeature.Title y Narrative
      if Supports(S, IFeature, Feature) then
      begin
        FCurrentFeatureName := Feature.Title;
        FIndent := 0;
        AddTags(S.Tags);
        AddLine('Feature: ' + Feature.Title);
        if Feature.Narrative <> '' then
        begin
          Inc(FIndent);
          for Line in Feature.Narrative.Split([#13#10, #10]) do
            AddLine(Line);
          Dec(FIndent);
        end;
        AddLine('');
        FIndent := 1; // Scenarios at indent 1
      end;
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

    sikImplicitRule: ; // Ignorar rule impl�cita

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
  // Devuelve �ndice en formato Quarto con include-code-files
  if FFilesWritten.Count = 0 then
    Result := '# MiniSpec' + sLineBreak + sLineBreak + 'No features generated.'
  else
  begin
    Result := '---' + sLineBreak +
              'title: "MiniSpec Features"' + sLineBreak +
              '---' + sLineBreak + sLineBreak +
              'Generated **' + IntToStr(FFilesWritten.Count) + '** features at ' +
              FormatDateTime('yyyy-mm-dd hh:nn:ss', CompletedAt) + '.' + sLineBreak + sLineBreak;
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

{ TLiveReporter }

constructor TLiveReporter.Create(const Decorated: ISpecReporter; APort: Integer);
begin
  inherited Create(Decorated);
  FPort := APort;
  FEvents := TStringList.Create;
  FEventsLock := TCriticalSection.Create;
  FLiveClients := TList<TIdContext>.Create;
  FClientsLock := TCriticalSection.Create;
  FReportFinished := False;

  FServer := TIdHTTPServer.Create(nil);
  FServer.DefaultPort := FPort;
  FServer.OnCommandGet := HandleRequest;
end;

destructor TLiveReporter.Destroy;
begin
  if FServer.Active then
    FServer.Active := False;
  FServer.Free;
  FClientsLock.Free;
  FLiveClients.Free;
  FEventsLock.Free;
  FEvents.Free;
  inherited;
end;

function TLiveReporter.HasConnectedClients: Boolean;
begin
  FClientsLock.Enter;
  try
    Result := FLiveClients.Count > 0;
  finally
    FClientsLock.Leave;
  end;
end;

procedure TLiveReporter.BeginReport;
var
  Data: TJSONObject;
  WaitCount: Integer;
begin
  inherited;
  FReportFinished := False;
  FEvents.Clear;
  FScenarioCount := 0;

  // Iniciar servidor HTTP
  try
    FServer.Active := True;
    WriteLn(Format('Live Dashboard: http://localhost:%d', [FPort]));
    WriteLn('Waiting for browser connection...');

    // Esperar hasta que haya al menos un cliente conectado (max 30 segundos)
    WaitCount := 0;
    while not HasConnectedClients and (WaitCount < 300) do
    begin
      Sleep(100);
      Inc(WaitCount);
    end;

    if HasConnectedClients then
      WriteLn('Browser connected. Running specs...')
    else
      WriteLn('Timeout. Running specs anyway...');

  except
    on E: Exception do
      WriteLn('Live server error: ' + E.Message);
  end;

  // Evento de inicio
  Data := TJSONObject.Create;
  try
    Data.AddPair('timestamp', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Now));
    BroadcastEvent(BuildEventJson('report:start', Data));
  finally
    Data.Free;
  end;
end;

procedure TLiveReporter.EndReport;
var
  Data: TJSONObject;
begin
  inherited; // Primero para que CompletedAt tenga valor
  // Evento de fin de reporte
  Data := TJSONObject.Create;
  try
    Data.AddPair('pass', TJSONNumber.Create(Decorated.PassCount));
    Data.AddPair('fail', TJSONNumber.Create(Decorated.FailCount));
    Data.AddPair('skip', TJSONNumber.Create(Decorated.SkipCount));
    Data.AddPair('completedAt', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Decorated.CompletedAt));
    BroadcastEvent(BuildEventJson('report:end', Data));
  finally
    Data.Free;
  end;

  FReportFinished := True;

  // Mantener servidor activo para que el usuario vea el resultado
  WriteLn(Format('Done. %d scenarios (%d pass, %d fail, %d skip). Press Enter to exit...',
    [FScenarioCount, Decorated.PassCount, Decorated.FailCount, Decorated.SkipCount]));
  ReadLn;
  FServer.Active := False;
end;

function TLiveReporter.GetContent: string;
begin
  Result := Format('Live Dashboard served at http://localhost:%d', [FPort]);
end;

function TLiveReporter.BuildEventJson(const EventType: string; const Data: TJSONObject): string;
var
  Wrapper: TJSONObject;
begin
  Wrapper := TJSONObject.Create;
  try
    Wrapper.AddPair('event', EventType);
    // Copiar pares de Data
    for var Pair in Data do
      Wrapper.AddPair(Pair.JsonString.Value, Pair.JsonValue.Clone as TJSONValue);
    Result := Wrapper.ToJSON;
  finally
    Wrapper.Free;
  end;
end;

function TLiveReporter.StepsToJsonArray(const Steps: TList<IScenarioStep>; const StepType: string): TJSONArray;
var
  StepObj: TJSONObject;
  Step: IScenarioStep;
begin
  Result := TJSONArray.Create;
  if Steps = nil then Exit;
  for Step in Steps do
  begin
    StepObj := TJSONObject.Create;
    StepObj.AddPair('type', StepType);
    StepObj.AddPair('text', Step.Description);
    StepObj.AddPair('success', TJSONBool.Create(Step.RunInfo.IsSuccess));
    StepObj.AddPair('ms', TJSONNumber.Create(Step.RunInfo.ExecTimeMs));
    if not Step.RunInfo.IsSuccess and (Step.RunInfo.ErrMsg <> '') then
      StepObj.AddPair('error', Step.RunInfo.ErrMsg);
    Result.AddElement(StepObj);
  end;
end;

procedure TLiveReporter.BroadcastEvent(const EventJson: string);
var
  Client: TIdContext;
  SSEData: string;
begin
  SSEData := 'data: ' + EventJson + #10#10;

  // Guardar evento para nuevos clientes
  FEventsLock.Enter;
  try
    FEvents.Add(EventJson);
  finally
    FEventsLock.Leave;
  end;

  // Enviar a clientes conectados
  FClientsLock.Enter;
  try
    for Client in FLiveClients do
    begin
      try
        Client.Connection.IOHandler.Write(SSEData, IndyTextEncoding_UTF8);
      except
        // Cliente desconectado, ignorar
      end;
    end;
  finally
    FClientsLock.Leave;
  end;
end;

procedure TLiveReporter.HandleRequest(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  Doc: string;
begin
  Doc := ARequestInfo.Document;

  if SameText(Doc, '/events') or Doc.EndsWith('/events', True) then
  begin
    // SSE headers - importante: NO usar WriteHeader, enviar manualmente
    AContext.Connection.IOHandler.WriteLn('HTTP/1.1 200 OK');
    AContext.Connection.IOHandler.WriteLn('Content-Type: text/event-stream');
    AContext.Connection.IOHandler.WriteLn('Cache-Control: no-cache');
    AContext.Connection.IOHandler.WriteLn('Connection: keep-alive');
    AContext.Connection.IOHandler.WriteLn('Access-Control-Allow-Origin: *');
    AContext.Connection.IOHandler.WriteLn(''); // L�nea vac�a para terminar headers

    // Registrar cliente
    FClientsLock.Enter;
    try
      FLiveClients.Add(AContext);
    finally
      FClientsLock.Leave;
    end;

    // Enviar comentario de bienvenida
    try
      AContext.Connection.IOHandler.Write(': welcome'#10#10, IndyTextEncoding_UTF8);
    except
    end;

    // Enviar eventos anteriores (replay)
    FEventsLock.Enter;
    try
      for var EventJson in FEvents do
      begin
        try
          AContext.Connection.IOHandler.Write('data: ' + EventJson + #10#10, IndyTextEncoding_UTF8);
        except
          Break;
        end;
      end;
    finally
      FEventsLock.Leave;
    end;

    // Mantener conexi�n abierta hasta que termine el reporte
    // Sin keepalive agresivo - los eventos mantienen la conexi�n viva
    while not FReportFinished and AContext.Connection.Connected do
      Sleep(200);

    // Desregistrar cliente
    FClientsLock.Enter;
    try
      FLiveClients.Remove(AContext);
    finally
      FClientsLock.Leave;
    end;
  end
  else
  begin
    // Dashboard HTML
    AResponseInfo.ContentType := 'text/html; charset=utf-8';
    AResponseInfo.ContentText := StringReplace(LIVE_DASHBOARD_HTML,
      '{{MINISPEC_VERSION}}', TMiniSpec.Version, [rfReplaceAll]);
  end;
end;

procedure TLiveReporter.DoFeatureBegin(const Feature: IFeature);
var
  Data: TJSONObject;
  TagsArray, BgSteps: TJSONArray;
begin
  inherited;

  if not Assigned(Feature) then Exit;

  Data := TJSONObject.Create;
  try
    Data.AddPair('name', Feature.Title);
    Data.AddPair('narrative', Feature.Narrative);
    TagsArray := TJSONArray.Create;
    for var Tag in Feature.Tags.ToArray do
      TagsArray.Add(Tag);
    Data.AddPair('tags', TagsArray);
    // Agregar Background si existe
    if Feature.BackGround <> nil then
    begin
      BgSteps := StepsToJsonArray(Feature.BackGround.StepsGiven, 'Given');
      Data.AddPair('background', BgSteps);
    end;
    BroadcastEvent(BuildEventJson('feature:start', Data));
  finally
    Data.Free;
  end;
end;

procedure TLiveReporter.DoFeatureEnd(const Feature: IFeature; const Counters: TSpecCounters);
var
  Data: TJSONObject;
begin
  inherited;
  if not Assigned(Feature) then Exit;

  Data := TJSONObject.Create;
  try
    Data.AddPair('name', Feature.Title);
    Data.AddPair('pass', TJSONNumber.Create(Counters.PassCount));
    Data.AddPair('fail', TJSONNumber.Create(Counters.FailCount));
    Data.AddPair('ms', TJSONNumber.Create(Counters.ElapsedMs));
    BroadcastEvent(BuildEventJson('feature:end', Data));
  finally
    Data.Free;
  end;
end;

procedure TLiveReporter.DoReport(const S: ISpecItem);
begin
  inherited; // Delegar al decorado
  // Feature se maneja en DoFeatureBegin/End
  // Scenario se maneja en DoScenarioEnd
  // Outline se maneja en DoOutlineEnd
end;

procedure TLiveReporter.DoScenarioEnd(const Scenario: IScenario; const Counters: TSpecCounters);
var
  Data: TJSONObject;
  IsSkipped: Boolean;
begin
  inherited;

  Inc(FScenarioCount);
  IsSkipped := Scenario.RunInfo.State = srsSkiped;

  Data := TJSONObject.Create;
  try
    Data.AddPair('name', Scenario.Description);
    Data.AddPair('skipped', TJSONBool.Create(IsSkipped));
    if IsSkipped then
      Data.AddPair('success', TJSONBool.Create(True)) // Skip no es fallo
    else
      Data.AddPair('success', TJSONBool.Create(Scenario.RunInfo.IsSuccess));
    Data.AddPair('ms', TJSONNumber.Create(Counters.ElapsedMs));
    if not IsSkipped and not Scenario.RunInfo.IsSuccess and (Scenario.RunInfo.ErrMsg <> '') then
      Data.AddPair('error', Scenario.RunInfo.ErrMsg);
    BroadcastEvent(BuildEventJson('scenario:end', Data));
  finally
    Data.Free;
  end;
end;

procedure TLiveReporter.DoOutlineBegin(const Outline: IScenarioOutline);
begin
  inherited;
  // Por ahora no emitimos eventos al inicio del outline
end;

procedure TLiveReporter.DoOutlineEnd(const Outline: IScenarioOutline; const Counters: TSpecCounters);
var
  Data: TJSONObject;
  StepsArray, HeadersArray, ExamplesArray, RowArray: TJSONArray;
  ExampleObj: TJSONObject;
begin
  inherited;

  // Emitir un solo evento outline:end con toda la información
  Data := TJSONObject.Create;
  try
    Data.AddPair('name', Outline.Description);
    Data.AddPair('type', 'outline');

    // Agregar headers de la tabla de examples
    HeadersArray := TJSONArray.Create;
    for var H in Outline.Headers do
      HeadersArray.Add(H);
    Data.AddPair('headers', HeadersArray);

    // Agregar steps template (Given/When/Then)
    StepsArray := TJSONArray.Create;
    var TempArr := StepsToJsonArray(Outline.StepsGiven, 'Given');
    for var i := 0 to TempArr.Count - 1 do
      StepsArray.AddElement(TempArr.Items[i].Clone as TJSONValue);
    TempArr.Free;
    TempArr := StepsToJsonArray(Outline.StepsWhen, 'When');
    for var i := 0 to TempArr.Count - 1 do
      StepsArray.AddElement(TempArr.Items[i].Clone as TJSONValue);
    TempArr.Free;
    TempArr := StepsToJsonArray(Outline.StepsThen, 'Then');
    for var i := 0 to TempArr.Count - 1 do
      StepsArray.AddElement(TempArr.Items[i].Clone as TJSONValue);
    TempArr.Free;
    Data.AddPair('steps', StepsArray);

    // Agregar tabla de examples con resultados
    ExamplesArray := TJSONArray.Create;
    for var Example in Outline.Examples do
    begin
      if Example.RunInfo.State in [srsFinished, srsSkiped] then
      begin
        Inc(FScenarioCount);
        var IsSkipped := Example.RunInfo.State = srsSkiped;

        ExampleObj := TJSONObject.Create;
        // Valores de este example
        RowArray := TJSONArray.Create;
        for var Val in Example.ExampleMeta.Values do
          RowArray.Add(Val.ToString);
        ExampleObj.AddPair('values', RowArray);
        ExampleObj.AddPair('skipped', TJSONBool.Create(IsSkipped));
        if IsSkipped then
          ExampleObj.AddPair('success', TJSONBool.Create(True))
        else
          ExampleObj.AddPair('success', TJSONBool.Create(Example.RunInfo.IsSuccess));
        ExampleObj.AddPair('ms', TJSONNumber.Create(Example.RunInfo.ExecTimeMs));
        if not IsSkipped and not Example.RunInfo.IsSuccess and (Example.RunInfo.ErrMsg <> '') then
          ExampleObj.AddPair('error', Example.RunInfo.ErrMsg);
        ExamplesArray.AddElement(ExampleObj);
      end;
    end;
    Data.AddPair('examples', ExamplesArray);
    Data.AddPair('pass', TJSONNumber.Create(Counters.PassCount));
    Data.AddPair('fail', TJSONNumber.Create(Counters.FailCount));
    Data.AddPair('success', TJSONBool.Create(Counters.IsSuccess));
    Data.AddPair('ms', TJSONNumber.Create(Counters.ElapsedMs));

    BroadcastEvent(BuildEventJson('outline:end', Data));
  finally
    Data.Free;
  end;
end;

end.
