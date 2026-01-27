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
  System.IniFiles,
  IdHTTPServer,
  IdCustomHTTPServer,
  IdContext,
  Daf.MiniSpec.Types;

type
  /// <summary>
  /// Dictionary of string options passed from CLI/config to reporters.
  /// Each reporter reads the keys it needs (e.g., 'output', 'port').
  /// </summary>
  TReporterOptions = TDictionary<string, string>;

  /// <summary>
  /// Opciones globales de MiniSpec con persistencia en archivo .cfg (formato INI).
  /// Incluye opciones de ejecución y configuración de reporters.
  /// </summary>
  TMiniSpecOptions = class
  public const
    OPT_DRY_RUN = 'DryRun';
    SEC_MINISPEC = 'minispec';
    SEC_REPORTER_PREFIX = 'reporter.';
  strict private
    FOptions: TDictionary<string, TValue>;
    FTagMatcher: TTagMatcher;
    FFilter: string;
    FPause: Boolean;
    FDryRun: Boolean;
    FStackTrace: Boolean;
    FReporterName: string;
    FReporterOptions: TObjectDictionary<string, TReporterOptions>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetOption(const Key: string; const Value: TValue);
    function GetOption(const Key: string; const Default: TValue): TValue;
    function HasOption(const Key: string): Boolean;
    // Helpers para opciones comunes
    function DryRun: Boolean;
    // Persistencia
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    // Reporter options
    function GetReporterOptions(const ReporterName: string): TReporterOptions;
    procedure SetReporterOption(const ReporterName, Key, Value: string);
    // Properties
    property TagMatcher: TTagMatcher read FTagMatcher write FTagMatcher;
    property Filter: string read FFilter write FFilter;
    property Pause: Boolean read FPause write FPause;
    property IsDryRun: Boolean read FDryRun write FDryRun;
    property StackTrace: Boolean read FStackTrace write FStackTrace;
    property ReporterName: string read FReporterName write FReporterName;
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

  /// <summary>
  /// Read-only context passed to listeners during spec execution.
  /// Provides access to counters, current feature/scenario, and options.
  /// </summary>
  IRunContext = interface
    ['{A1B2C3D4-E5F6-7A8B-9C0D-1E2F3A4B5C6D}']
    function GetReportCounters: TSpecCounters;
    function GetFeatureCounters: TSpecCounters;
    function GetScenarioCounters: TSpecCounters;
    function GetOutlineCounters: TSpecCounters;
    function GetCurrentFeature: IFeature;
    function GetCurrentRule: IRule;
    function GetCurrentScenario: IScenario;
    function GetCurrentOutline: IScenarioOutline;
    function GetOptions: TMiniSpecOptions;
    function GetCompletedAt: TDateTime;
    function GetErrorDetail(const RunInfo: TSpecRunInfo): string;
    property ReportCounters: TSpecCounters read GetReportCounters;
    property FeatureCounters: TSpecCounters read GetFeatureCounters;
    property ScenarioCounters: TSpecCounters read GetScenarioCounters;
    property OutlineCounters: TSpecCounters read GetOutlineCounters;
    property CurrentFeature: IFeature read GetCurrentFeature;
    property CurrentRule: IRule read GetCurrentRule;
    property CurrentScenario: IScenario read GetCurrentScenario;
    property CurrentOutline: IScenarioOutline read GetCurrentOutline;
    property Options: TMiniSpecOptions read GetOptions;
    property CompletedAt: TDateTime read GetCompletedAt;
  end;

  /// <summary>
  /// Observer interface for spec execution events.
  /// Listeners receive notifications but cannot modify execution.
  /// </summary>
  ISpecListener = interface
    ['{F8A1B2C3-D4E5-6F7A-8B9C-0D1E2F3A4B5C}']
    procedure Configure(const Options: TReporterOptions);
    function ShowHelp: Boolean;
    procedure OnBeginReport(const Context: IRunContext);
    procedure OnEndReport(const Context: IRunContext);
    procedure OnBeginFeature(const Context: IRunContext; const Feature: IFeature);
    procedure OnEndFeature(const Context: IRunContext; const Feature: IFeature; const Counters: TSpecCounters);
    procedure OnBeginScenario(const Context: IRunContext; const Scenario: IScenario);
    procedure OnEndScenario(const Context: IRunContext; const Scenario: IScenario; const Counters: TSpecCounters);
    procedure OnBeginOutline(const Context: IRunContext; const Outline: IScenarioOutline);
    procedure OnEndOutline(const Context: IRunContext; const Outline: IScenarioOutline; const Counters: TSpecCounters);
    procedure OnItem(const Context: IRunContext; const Item: ISpecItem);
  end;

  /// <summary>
  /// Base class for spec listeners with empty implementations.
  /// Subclass and override only the methods you need.
  /// </summary>
  TCustomListener = class(TInterfacedObject, ISpecListener)
  protected
    FCliOptions: TReporterOptions;
    function GetCliOption(const Key: string; const Default: string = ''): string;
  public
    destructor Destroy; override;
    procedure Configure(const Options: TReporterOptions); virtual;
    function ShowHelp: Boolean; virtual;
    procedure OnBeginReport(const Context: IRunContext); virtual;
    procedure OnEndReport(const Context: IRunContext); virtual;
    procedure OnBeginFeature(const Context: IRunContext; const Feature: IFeature); virtual;
    procedure OnEndFeature(const Context: IRunContext; const Feature: IFeature; const Counters: TSpecCounters); virtual;
    procedure OnBeginScenario(const Context: IRunContext; const Scenario: IScenario); virtual;
    procedure OnEndScenario(const Context: IRunContext; const Scenario: IScenario; const Counters: TSpecCounters); virtual;
    procedure OnBeginOutline(const Context: IRunContext; const Outline: IScenarioOutline); virtual;
    procedure OnEndOutline(const Context: IRunContext; const Outline: IScenarioOutline; const Counters: TSpecCounters); virtual;
    procedure OnItem(const Context: IRunContext; const Item: ISpecItem); virtual;
    function GetContent: string; virtual;
    function GetFileExt: string; virtual;
    function UseConsole: Boolean; virtual;
  end;

  ISpecReporter = interface(IInvokable)
    ['{CD69B272-5B38-4CCC-A64F-2B2A57ACB540}']
    function GetFailCount: Cardinal;
    function GetPassCount: Cardinal;
    function GetSkipCount: Cardinal;
    function GetElapsedMs: Integer;
    function GetFeatureCount: Integer;
    function GetCompletedAt: TDateTime;
    procedure Configure(const Options: TReporterOptions);
    function ShowHelp: Boolean;
    procedure AddListener(const Listener: ISpecListener);
    procedure Report(Features: TList<IFeature>; Options: TMiniSpecOptions);
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
    property ElapsedMs: Integer read GetElapsedMs;
    property FeatureCount: Integer read GetFeatureCount;
    property CompletedAt: TDateTime read GetCompletedAt;
  end;

  TSpecRunner = class(TInterfacedObject, ISpecReporter, IRunContext)
  strict private
    FFeatureCount: Integer;
    // Counters at different levels
    FReportCounters: TSpecCounters;
    FFeatureCounters: TSpecCounters;
    FScenarioCounters: TSpecCounters;
    FOutlineCounters: TSpecCounters;
    // Current context
    FCurrentFeature: IFeature;
    FCurrentRule: IRule;
    FCurrentScenario: IScenario;
    FCurrentOutline: IScenarioOutline;
    // Report options
    FOptions: TMiniSpecOptions;
    // CLI options (output, port, etc.)
    FCliOptions: TReporterOptions;
    // Timestamp
    FCompletedAt: TDateTime;
    // Listeners
    FListeners: TList<ISpecListener>;
    // IRunContext getters
    function IRunContext.GetReportCounters = GetReportCountersImpl;
    function IRunContext.GetFeatureCounters = GetFeatureCountersImpl;
    function IRunContext.GetScenarioCounters = GetScenarioCountersImpl;
    function IRunContext.GetOutlineCounters = GetOutlineCountersImpl;
    function IRunContext.GetCurrentFeature = GetCurrentFeatureImpl;
    function IRunContext.GetCurrentRule = GetCurrentRuleImpl;
    function IRunContext.GetCurrentScenario = GetCurrentScenarioImpl;
    function IRunContext.GetCurrentOutline = GetCurrentOutlineImpl;
    function IRunContext.GetOptions = GetOptionsImpl;
    function IRunContext.GetCompletedAt = GetCompletedAtImpl;
    function IRunContext.GetErrorDetail = GetErrorDetailImpl;
  protected
    // IRunContext implementation
    function GetReportCountersImpl: TSpecCounters;
    function GetFeatureCountersImpl: TSpecCounters;
    function GetScenarioCountersImpl: TSpecCounters;
    function GetOutlineCountersImpl: TSpecCounters;
    function GetCurrentFeatureImpl: IFeature;
    function GetCurrentRuleImpl: IRule;
    function GetCurrentScenarioImpl: IScenario;
    function GetCurrentOutlineImpl: IScenarioOutline;
    function GetOptionsImpl: TMiniSpecOptions;
    function GetCompletedAtImpl: TDateTime;
    function GetErrorDetailImpl(const RunInfo: TSpecRunInfo): string;
    // Listener notifications
    procedure NotifyBeginReport;
    procedure NotifyEndReport;
    procedure NotifyBeginFeature(const Feature: IFeature);
    procedure NotifyEndFeature(const Feature: IFeature; const Counters: TSpecCounters);
    procedure NotifyBeginScenario(const Scenario: IScenario);
    procedure NotifyEndScenario(const Scenario: IScenario; const Counters: TSpecCounters);
    procedure NotifyBeginOutline(const Outline: IScenarioOutline);
    procedure NotifyEndOutline(const Outline: IScenarioOutline; const Counters: TSpecCounters);
    procedure NotifyItem(const Item: ISpecItem);
    // Original protected members
    function GetContent: string;virtual;
    function GetFailCount: Cardinal;virtual;
    function GetPassCount: Cardinal;virtual;
    function GetSkipCount: Cardinal;virtual;
    function GetElapsedMs: Integer;virtual;
    function GetFeatureCount: Integer;virtual;
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
    property CurrentRule: IRule read FCurrentRule;
    property CurrentScenario: IScenario read FCurrentScenario;
    property CurrentOutline: IScenarioOutline read FCurrentOutline;
    property ReportCounters: TSpecCounters read FReportCounters;
    property FeatureCounters: TSpecCounters read FFeatureCounters;
    property ScenarioCounters: TSpecCounters read FScenarioCounters;
    property OutlineCounters: TSpecCounters read FOutlineCounters;
    property Options: TMiniSpecOptions read FOptions;
    // CLI options for subclasses
    function GetCliOption(const Key: string; const Default: string = ''): string;
    /// <summary>
    /// Returns error message, optionally with stack trace if Options.StackTrace is enabled.
    /// </summary>
    function GetErrorDetail(const RunInfo: TSpecRunInfo): string;
  public
    constructor Create;
    destructor Destroy; override;
    // Listener management
    procedure AddListener(const Listener: ISpecListener);
    procedure RemoveListener(const Listener: ISpecListener);
    procedure Configure(const Options: TReporterOptions);virtual;
    function ShowHelp: Boolean;virtual;
    function UseConsole: Boolean;virtual;
    procedure BeginReport;virtual;
    procedure Report(Features: TList<IFeature>; Options: TMiniSpecOptions);overload;
    procedure EndReport;virtual;
    property PassCount: Cardinal read GetPassCount;
    property FailCount: Cardinal read GetFailCount;
    property SkipCount: Cardinal read GetSkipCount;
    property ElapsedMs: Integer read GetElapsedMs;
    property FeatureCount: Integer read GetFeatureCount;
    property CompletedAt: TDateTime read FCompletedAt;
    property Listeners: TList<ISpecListener> read FListeners;
  end;

  /// <summary>
  /// Console listener - outputs test results to console in hierarchical format.
  /// Implements ISpecListener for pure observer pattern.
  /// </summary>
  TConsoleListener = class(TCustomListener)
  private
    FCurrentRule: IRule;
    procedure OutputLn(const Level: Byte; const Text: string; const Success: Boolean; const Duration: Integer; const ErrorMessage: string = '');overload;
    procedure OutputLn(const Level: Byte; const Text: string);overload;
    procedure Output(const Level: Byte; const Text: string);
    function ExtractValue(const Match: TMatch): string;
    function Level2Margin(const Level: Byte): string;
    function GetKeyWord(const Kind: TSpecItemKind): string;
    function GetLevel(const Kind: TSpecItemKind): Byte;
  public
    function UseConsole: Boolean;override;
    procedure OnBeginReport(const Context: IRunContext);override;
    procedure OnEndReport(const Context: IRunContext);override;
    procedure OnItem(const Context: IRunContext; const Item: ISpecItem);override;
    procedure OnEndOutline(const Context: IRunContext; const Outline: IScenarioOutline; const Counters: TSpecCounters);override;
  end;

  /// <summary>
  /// JSON listener - builds JSON output from test results.
  /// Implements ISpecListener for pure observer pattern.
  /// </summary>
  TJsonListener = class(TCustomListener)
  private
    FOutput: string;
    FFeatures: TJSONArray;
    FCurrentFeature: TJSONObject;
    FCurrentScenarios: TJSONArray;
    FCurrentScenario: TJSONObject;
    FCurrentSteps: TJSONArray;
    procedure AddStep(const Kind, Description: string; Success: Boolean; Duration: Integer; const ErrorMessage: string = '');
    procedure CloseCurrentScenario;
    procedure CloseCurrentFeature;
    function GetStatus(const Item: ISpecItem): string;
    function GetKeyWord(const Kind: TSpecItemKind): string;
  public
    function GetContent: string;override;
    function GetFileExt: string;override;
    procedure OnBeginReport(const Context: IRunContext);override;
    procedure OnEndReport(const Context: IRunContext);override;
    procedure OnItem(const Context: IRunContext; const Item: ISpecItem);override;
    procedure OnEndOutline(const Context: IRunContext; const Outline: IScenarioOutline; const Counters: TSpecCounters);override;
  end;

  /// <summary>
  /// Gherkin listener - generates .feature files from test results.
  /// Implements ISpecListener for pure observer pattern.
  /// </summary>
  TGherkinListener = class(TCustomListener)
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
  public
    constructor Create(AWithResults: Boolean = False);
    destructor Destroy;override;
    procedure Configure(const Options: TReporterOptions);override;
    function GetContent: string;override;
    function GetFileExt: string;override;
    procedure OnBeginReport(const Context: IRunContext);override;
    procedure OnEndReport(const Context: IRunContext);override;
    procedure OnItem(const Context: IRunContext; const Item: ISpecItem);override;
    procedure OnEndOutline(const Context: IRunContext; const Outline: IScenarioOutline; const Counters: TSpecCounters);override;
    property WithResults: Boolean read FWithResults write FWithResults;
  end;

  /// <summary>
  /// Live listener - broadcasts test results via SSE to browser dashboard.
  /// Implements ISpecListener for pure observer pattern.
  /// </summary>
  TLiveListener = class(TCustomListener)
  private
    FServer: TIdHTTPServer;
    FPort: Integer;
    FEvents: TStringList;
    FEventsLock: TCriticalSection;
    FLiveClients: TList<TIdContext>;
    FClientsLock: TCriticalSection;
    FReportFinished: Boolean;
    FWaitClient: Boolean;
    FContext: IRunContext;
    procedure HandleRequest(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure BroadcastEvent(const EventJson: string);
    function BuildEventJson(const EventType: string; const Data: TJSONObject): string;
    function HasConnectedClients: Boolean;
    function StepsToJsonArray(const Steps: TList<IScenarioStep>; const StepType: string): TJSONArray;
  public
    constructor Create(APort: Integer = 8080);
    destructor Destroy;override;
    procedure Configure(const Options: TReporterOptions);override;
    function ShowHelp: Boolean;override;
    function UseConsole: Boolean;override;
    function GetContent: string;override;
    procedure OnBeginReport(const Context: IRunContext);override;
    procedure OnEndReport(const Context: IRunContext);override;
    procedure OnBeginFeature(const Context: IRunContext; const Feature: IFeature);override;
    procedure OnEndFeature(const Context: IRunContext; const Feature: IFeature; const Counters: TSpecCounters);override;
    procedure OnEndScenario(const Context: IRunContext; const Scenario: IScenario; const Counters: TSpecCounters);override;
    procedure OnEndOutline(const Context: IRunContext; const Outline: IScenarioOutline; const Counters: TSpecCounters);override;
    property Port: Integer read FPort;
    property WaitClient: Boolean read FWaitClient;
  end;

implementation
uses
  System.StrUtils,
  System.IOUtils,
  IdGlobal,
  Daf.MiniSpec.Utils,
  Daf.MiniSpec,
  Daf.MiniSpec.LiveDashboard;

{ TCustomListener }

destructor TCustomListener.Destroy;
begin
  FCliOptions.Free;
  inherited;
end;

procedure TCustomListener.Configure(const Options: TReporterOptions);
var
  Pair: TPair<string, string>;
begin
  // Make a copy of the options since the original may be freed
  FreeAndNil(FCliOptions);
  if Assigned(Options) then
  begin
    FCliOptions := TReporterOptions.Create;
    for Pair in Options do
      FCliOptions.Add(Pair.Key, Pair.Value);
  end;
end;

function TCustomListener.ShowHelp: Boolean;
begin
  Result := False;
end;

function TCustomListener.GetCliOption(const Key: string; const Default: string): string;
begin
  Result := Default;
  if Assigned(FCliOptions) and FCliOptions.ContainsKey(Key) then
    Result := FCliOptions[Key];
end;

procedure TCustomListener.OnBeginReport(const Context: IRunContext);
begin
  // Empty - override in subclasses
end;

procedure TCustomListener.OnEndReport(const Context: IRunContext);
begin
  // Empty - override in subclasses
end;

procedure TCustomListener.OnBeginFeature(const Context: IRunContext; const Feature: IFeature);
begin
  // Empty - override in subclasses
end;

procedure TCustomListener.OnEndFeature(const Context: IRunContext; const Feature: IFeature; const Counters: TSpecCounters);
begin
  // Empty - override in subclasses
end;

procedure TCustomListener.OnBeginScenario(const Context: IRunContext; const Scenario: IScenario);
begin
  // Empty - override in subclasses
end;

procedure TCustomListener.OnEndScenario(const Context: IRunContext; const Scenario: IScenario; const Counters: TSpecCounters);
begin
  // Empty - override in subclasses
end;

procedure TCustomListener.OnBeginOutline(const Context: IRunContext; const Outline: IScenarioOutline);
begin
  // Empty - override in subclasses
end;

procedure TCustomListener.OnEndOutline(const Context: IRunContext; const Outline: IScenarioOutline; const Counters: TSpecCounters);
begin
  // Empty - override in subclasses
end;

procedure TCustomListener.OnItem(const Context: IRunContext; const Item: ISpecItem);
begin
  // Empty - override in subclasses
end;

function TCustomListener.GetContent: string;
begin
  Result := '';
end;

function TCustomListener.GetFileExt: string;
begin
  Result := 'txt';
end;

function TCustomListener.UseConsole: Boolean;
begin
  Result := False;
end;

{ TMiniSpecOptions }

constructor TMiniSpecOptions.Create;
begin
  inherited;
  FOptions := TDictionary<string, TValue>.Create;
  FReporterOptions := TObjectDictionary<string, TReporterOptions>.Create([doOwnsValues]);
  FTagMatcher := nil;
  FFilter := '';
  FPause := False;
  FDryRun := False;
  FReporterName := '';  // No default reporter - will use console if none specified
end;

destructor TMiniSpecOptions.Destroy;
begin
  FReporterOptions.Free;
  FOptions.Free;
  inherited;
end;

procedure TMiniSpecOptions.SetOption(const Key: string; const Value: TValue);
begin
  FOptions.AddOrSetValue(Key, Value);
end;

function TMiniSpecOptions.GetOption(const Key: string; const Default: TValue): TValue;
begin
  if not FOptions.TryGetValue(Key, Result) then
    Result := Default;
end;

function TMiniSpecOptions.HasOption(const Key: string): Boolean;
begin
  Result := FOptions.ContainsKey(Key);
end;

function TMiniSpecOptions.DryRun: Boolean;
begin
  Result := FDryRun;
end;

function TMiniSpecOptions.GetReporterOptions(const ReporterName: string): TReporterOptions;
begin
  if not FReporterOptions.TryGetValue(LowerCase(ReporterName), Result) then
  begin
    Result := TReporterOptions.Create;
    FReporterOptions.Add(LowerCase(ReporterName), Result);
  end;
end;

procedure TMiniSpecOptions.SetReporterOption(const ReporterName, Key, Value: string);
var
  Opts: TReporterOptions;
begin
  Opts := GetReporterOptions(ReporterName);
  Opts.AddOrSetValue(Key, Value);
end;

procedure TMiniSpecOptions.LoadFromFile(const FileName: string);
var
  Ini: TMemIniFile;
  Sections, Keys: TStringList;
  i, j: Integer;
  Section, Key, Value, RepName: string;
  Opts: TReporterOptions;
begin
  if not FileExists(FileName) then
    Exit;

  Ini := TMemIniFile.Create(FileName);
  Sections := TStringList.Create;
  Keys := TStringList.Create;
  try
    Ini.ReadSections(Sections);
    for i := 0 to Sections.Count - 1 do
    begin
      Section := Sections[i];
      Keys.Clear;
      Ini.ReadSection(Section, Keys);

      if SameText(Section, SEC_MINISPEC) then
      begin
        // Opciones globales
        for j := 0 to Keys.Count - 1 do
        begin
          Key := Keys[j];
          Value := Ini.ReadString(Section, Key, '');
          if SameText(Key, 'filter') then
            FFilter := Value
          else if SameText(Key, 'pause') then
            FPause := SameText(Value, 'true') or (Value = '1')
          else if SameText(Key, 'dry-run') then
            FDryRun := SameText(Value, 'true') or (Value = '1')
          else if SameText(Key, 'reporter') then
            FReporterName := Value;
        end;
      end
      else if Section.StartsWith(SEC_REPORTER_PREFIX, True) then
      begin
        // Opciones de reporter: [reporter.html], [reporter.live], etc.
        RepName := Copy(Section, Length(SEC_REPORTER_PREFIX) + 1, MaxInt);
        // Si no hay reporter principal definido, usar el primero que encontremos
        if FReporterName.IsEmpty then
          FReporterName := RepName;
        Opts := GetReporterOptions(RepName);
        for j := 0 to Keys.Count - 1 do
        begin
          Key := Keys[j];
          Value := Ini.ReadString(Section, Key, '');
          Opts.AddOrSetValue(Key, Value);
        end;
      end;
    end;
  finally
    Keys.Free;
    Sections.Free;
    Ini.Free;
  end;
end;

procedure TMiniSpecOptions.SaveToFile(const FileName: string);
var
  Ini: TMemIniFile;
  Pair: TPair<string, TReporterOptions>;
  OptPair: TPair<string, string>;
begin
  Ini := TMemIniFile.Create(FileName);
  try
    // Siempre escribir sección [minispec] con reporter (aunque sea console)
    Ini.WriteString(SEC_MINISPEC, 'reporter', IfThen(FReporterName.IsEmpty, 'console', FReporterName));

    // Opciones globales
    if not FFilter.IsEmpty then
      Ini.WriteString(SEC_MINISPEC, 'filter', FFilter);
    if FPause then
      Ini.WriteString(SEC_MINISPEC, 'pause', 'true');
    if FDryRun then
      Ini.WriteString(SEC_MINISPEC, 'dry-run', 'true');

    // Opciones por reporter
    for Pair in FReporterOptions do
    begin
      for OptPair in Pair.Value do
        Ini.WriteString(SEC_REPORTER_PREFIX + Pair.Key, OptPair.Key, OptPair.Value);
    end;

    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
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

{ TSpecRunner }

constructor TSpecRunner.Create;
begin
  inherited Create;
  FListeners := TList<ISpecListener>.Create;
end;

procedure TSpecRunner.AddListener(const Listener: ISpecListener);
begin
  if not FListeners.Contains(Listener) then
    FListeners.Add(Listener);
end;

procedure TSpecRunner.RemoveListener(const Listener: ISpecListener);
begin
  FListeners.Remove(Listener);
end;

// IRunContext implementation
function TSpecRunner.GetReportCountersImpl: TSpecCounters;
begin
  Result := FReportCounters;
end;

function TSpecRunner.GetFeatureCountersImpl: TSpecCounters;
begin
  Result := FFeatureCounters;
end;

function TSpecRunner.GetScenarioCountersImpl: TSpecCounters;
begin
  Result := FScenarioCounters;
end;

function TSpecRunner.GetOutlineCountersImpl: TSpecCounters;
begin
  Result := FOutlineCounters;
end;

function TSpecRunner.GetCurrentFeatureImpl: IFeature;
begin
  Result := FCurrentFeature;
end;

function TSpecRunner.GetCurrentRuleImpl: IRule;
begin
  Result := FCurrentRule;
end;

function TSpecRunner.GetCurrentScenarioImpl: IScenario;
begin
  Result := FCurrentScenario;
end;

function TSpecRunner.GetCurrentOutlineImpl: IScenarioOutline;
begin
  Result := FCurrentOutline;
end;

function TSpecRunner.GetOptionsImpl: TMiniSpecOptions;
begin
  Result := FOptions;
end;

function TSpecRunner.GetCompletedAtImpl: TDateTime;
begin
  Result := FCompletedAt;
end;

function TSpecRunner.GetErrorDetailImpl(const RunInfo: TSpecRunInfo): string;
begin
  Result := GetErrorDetail(RunInfo);
end;

// Listener notifications
procedure TSpecRunner.NotifyBeginReport;
var
  i: Integer;
begin
  for i := 0 to FListeners.Count - 1 do
    FListeners[i].OnBeginReport(Self as IRunContext);
end;

procedure TSpecRunner.NotifyEndReport;
var
  Listener: ISpecListener;
begin
  for Listener in FListeners do
    Listener.OnEndReport(Self as IRunContext);
end;

procedure TSpecRunner.NotifyBeginFeature(const Feature: IFeature);
var
  Listener: ISpecListener;
begin
  for Listener in FListeners do
    Listener.OnBeginFeature(Self as IRunContext, Feature);
end;

procedure TSpecRunner.NotifyEndFeature(const Feature: IFeature; const Counters: TSpecCounters);
var
  Listener: ISpecListener;
begin
  for Listener in FListeners do
    Listener.OnEndFeature(Self as IRunContext, Feature, Counters);
end;

procedure TSpecRunner.NotifyBeginScenario(const Scenario: IScenario);
var
  Listener: ISpecListener;
begin
  for Listener in FListeners do
    Listener.OnBeginScenario(Self as IRunContext, Scenario);
end;

procedure TSpecRunner.NotifyEndScenario(const Scenario: IScenario; const Counters: TSpecCounters);
var
  Listener: ISpecListener;
begin
  for Listener in FListeners do
    Listener.OnEndScenario(Self as IRunContext, Scenario, Counters);
end;

procedure TSpecRunner.NotifyBeginOutline(const Outline: IScenarioOutline);
var
  Listener: ISpecListener;
begin
  for Listener in FListeners do
    Listener.OnBeginOutline(Self as IRunContext, Outline);
end;

procedure TSpecRunner.NotifyEndOutline(const Outline: IScenarioOutline; const Counters: TSpecCounters);
var
  Listener: ISpecListener;
begin
  for Listener in FListeners do
    Listener.OnEndOutline(Self as IRunContext, Outline, Counters);
end;

procedure TSpecRunner.NotifyItem(const Item: ISpecItem);
var
  Listener: ISpecListener;
begin
  for Listener in FListeners do
    Listener.OnItem(Self as IRunContext, Item);
end;

procedure TSpecRunner.DoFeatureBegin(const Feature: IFeature);
begin
  NotifyBeginFeature(Feature);
end;

procedure TSpecRunner.DoFeatureEnd(const Feature: IFeature; const Counters: TSpecCounters);
begin
  NotifyEndFeature(Feature, Counters);
end;

procedure TSpecRunner.DoScenarioBegin(const Scenario: IScenario);
begin
  NotifyBeginScenario(Scenario);
end;

procedure TSpecRunner.DoScenarioEnd(const Scenario: IScenario; const Counters: TSpecCounters);
begin
  NotifyEndScenario(Scenario, Counters);
end;

procedure TSpecRunner.DoOutlineBegin(const Outline: IScenarioOutline);
begin
  NotifyBeginOutline(Outline);
end;

procedure TSpecRunner.DoOutlineEnd(const Outline: IScenarioOutline; const Counters: TSpecCounters);
begin
  NotifyEndOutline(Outline, Counters);
end;

// Template Method: BeginFeature (non-virtual)
procedure TSpecRunner.BeginFeature(const Feature: IFeature);
begin
  FCurrentFeature := Feature;
  FFeatureCounters.Reset;
  FFeatureCounters.Start;
  DoFeatureBegin(Feature);  // Hook
end;

// Template Method: EndFeature (non-virtual)
procedure TSpecRunner.EndFeature(const Feature: IFeature);
begin
  FFeatureCounters.Stop;
  DoFeatureEnd(Feature, FFeatureCounters);  // Hook
  FCurrentFeature := nil;
end;

// Template Method: BeginScenario (non-virtual)
procedure TSpecRunner.BeginScenario(const Scenario: IScenario);
begin
  FCurrentScenario := Scenario;
  FScenarioCounters.Reset;
  FScenarioCounters.Start;
  DoScenarioBegin(Scenario);  // Hook
end;

// Template Method: EndScenario (non-virtual)
procedure TSpecRunner.EndScenario(const Scenario: IScenario);
begin
  FScenarioCounters.Stop;
  DoScenarioEnd(Scenario, FScenarioCounters);  // Hook
  FCurrentScenario := nil;
end;

// Template Method: BeginOutline (non-virtual)
procedure TSpecRunner.BeginOutline(const Outline: IScenarioOutline);
begin
  FCurrentOutline := Outline;
  FOutlineCounters.Reset;
  FOutlineCounters.Start;
  DoOutlineBegin(Outline);  // Hook
end;

// Template Method: EndOutline (non-virtual)
procedure TSpecRunner.EndOutline(const Outline: IScenarioOutline);
begin
  FOutlineCounters.Stop;
  DoOutlineEnd(Outline, FOutlineCounters);  // Hook
  FCurrentOutline := nil;
end;

destructor TSpecRunner.Destroy;
begin
  FListeners.Free;
  FCliOptions.Free;
  inherited;
end;

procedure TSpecRunner.Configure(const Options: TReporterOptions);
var
  Pair: TPair<string, string>;
begin
  // Make a copy of the options since the original may be freed
  FreeAndNil(FCliOptions);
  if Assigned(Options) then
  begin
    FCliOptions := TReporterOptions.Create;
    for Pair in Options do
      FCliOptions.Add(Pair.Key, Pair.Value);
  end;
end;

function TSpecRunner.GetCliOption(const Key: string; const Default: string): string;
begin
  if Assigned(FCliOptions) and FCliOptions.ContainsKey(Key) then
    Result := FCliOptions[Key]
  else
    Result := Default;
end;

function TSpecRunner.ShowHelp: Boolean;
begin
  // Base implementation: no help available
  Result := False;
end;

procedure TSpecRunner.BeginReport;
begin
  FFeatureCount := 0;
  FReportCounters.Reset;
  FReportCounters.Start;
  FFeatureCounters.Reset;
  FOutlineCounters.Reset;
  FCurrentFeature := nil;
  FCurrentScenario := nil;
  FCurrentOutline := nil;
  NotifyBeginReport;
end;

procedure TSpecRunner.Report(Feature: IFeature);
begin
  Inc(FFeatureCount);
  BeginFeature(Feature);
  DoReport(Feature);
  for var Rule in Feature.Rules do
    Report(Rule);
  EndFeature(Feature);
end;

procedure TSpecRunner.ReportOutline(const Outline: IScenarioOutline);
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

procedure TSpecRunner.Report(Rule: IRule);
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

  // Establecer contexto de Rule actual (solo para Rules explícitas)
  if Rule.Kind = sikRule then
    FCurrentRule := Rule;

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

  // Limpiar contexto de Rule
  if Rule.Kind = sikRule then
    FCurrentRule := nil;
end;

procedure TSpecRunner.Report(Background: IBackground);
begin
  if not Assigned(Background) then Exit;
  DoReport(Background);
  for var Step in BackGround.StepsGiven do
    DoReport(Step);
end;

procedure TSpecRunner.Report(Scenario: IScenario);
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

procedure TSpecRunner.DoReport(const S: ISpecItem);
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
  NotifyItem(S);
end;

procedure TSpecRunner.EndReport;
begin
  FReportCounters.Stop;
  FCompletedAt := Now;
  NotifyEndReport;
end;

function TSpecRunner.GetFailCount: Cardinal;
begin
  Result := FReportCounters.FailCount;
end;

function TSpecRunner.GetSkipCount: Cardinal;
begin
  Result := FReportCounters.SkipCount;
end;

function TSpecRunner.GetElapsedMs: Integer;
begin
  Result := FReportCounters.ElapsedMs;
end;

function TSpecRunner.GetFeatureCount: Integer;
begin
  Result := FFeatureCount;
end;

function TSpecRunner.GetCompletedAt: TDateTime;
begin
  Result := FCompletedAt;
end;

function TSpecRunner.GetContent: string;
begin
  Result := '';
end;

function TSpecRunner.GetFileExt: string;
begin
  Result := '';
end;

function TSpecRunner.GetKeyWord(const Kind: TSpecItemKind): string;
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

function TSpecRunner.GetLevel(const Kind: TSpecItemKind): Byte;
begin
  // Nivel base por tipo (sin considerar contexto de Rule)
  case Kind of
    sikFeature: Result := 0;
    sikRule: Result := 1;
    sikBackground, sikScenario, sikScenarioOutline, sikExample: Result := 1;
    sikExampleInit: Result := 2;
    sikGiven, sikWhen, sikThen: Result := 2;
    else
      Result := 0;
  end;
end;

function TSpecRunner.GetPassCount: Cardinal;
begin
  Result := FReportCounters.PassCount;
end;

function TSpecRunner.GetErrorDetail(const RunInfo: TSpecRunInfo): string;
begin
  if not Assigned(RunInfo.Error) then
    Exit('');

  Result := RunInfo.Error.Message;
  if Assigned(FOptions) and FOptions.StackTrace then
  begin
    var Stack := RunInfo.Error.StackTrace;
    if not Stack.IsEmpty then
      Result := Result + SLineBreak + 'Stack trace:' + SLineBreak + Stack;
  end;
end;

procedure TSpecRunner.Report(Features: TList<IFeature>; Options: TMiniSpecOptions);
begin
  FOptions := Options;
  BeginReport;
  for var F in Features do
    Report(F);
  EndReport;
end;

function TSpecRunner.UseConsole: Boolean;
begin
  Result := False;
end;

{ TConsoleListener }

function TConsoleListener.UseConsole: Boolean;
begin
  Result := True;
end;

function TConsoleListener.GetKeyWord(const Kind: TSpecItemKind): string;
begin
  case Kind of
    sikFeature: Result := 'Feature';
    sikImplicitRule: Result := '';
    sikRule: Result := 'Rule';
    sikBackground: Result := 'Background';
    sikScenario: Result := 'Scenario';
    sikScenarioOutline: Result := 'Scenario Outline';
    sikExample: Result := 'Example';
    sikExampleInit: Result := '';
    sikGiven: Result := 'Given';
    sikWhen: Result := 'When';
    sikThen: Result := 'Then';
    else
      Result := '';
  end;
end;

function TConsoleListener.GetLevel(const Kind: TSpecItemKind): Byte;
begin
  case Kind of
    sikFeature: Result := 0;
    sikRule: Result := 1;
    sikBackground, sikScenario, sikScenarioOutline, sikExample: Result := 1;
    sikExampleInit: Result := 2;
    sikGiven, sikWhen, sikThen: Result := 2;
    else
      Result := 0;
  end;
end;

function TConsoleListener.ExtractValue(const Match: TMatch): string;
begin
  Result := Match.Groups[1].Value;
end;

function TConsoleListener.Level2Margin(const Level: Byte): string;
begin
  Result := DupeString(' ', 2 * Level);
end;

procedure TConsoleListener.Output(const Level: Byte; const Text: string);
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

procedure TConsoleListener.OutputLn(const Level: Byte; const Text: string);
begin
  Output(Level, Text + SLineBreak);
end;

procedure TConsoleListener.OutputLn(const Level: Byte; const Text: string; const Success: Boolean; const Duration: Integer; const ErrorMessage: string);
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

procedure TConsoleListener.OnBeginReport(const Context: IRunContext);
begin
  // Nothing to initialize for console output
end;

procedure TConsoleListener.OnEndReport(const Context: IRunContext);
begin
  // Summary is printed by TReportSummaryWriter decorator, not here
end;

procedure TConsoleListener.OnItem(const Context: IRunContext; const Item: ISpecItem);
var
  Feat: IFeature;
  Rule: IRule;
  DisplayText: string;
  AllSkipped: Boolean;
  Kind: string;
  Level: Byte;
begin
  Kind := GetKeyWord(Item.Kind);
  Level := GetLevel(Item.Kind);

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

procedure TConsoleListener.OnEndOutline(const Context: IRunContext; const Outline: IScenarioOutline; const Counters: TSpecCounters);
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
    OutputLn(BaseLevel + 1, GetKeyWord(Step.Kind) + ' ' + Step.Description);
  for var Step in Outline.StepsWhen do
    OutputLn(BaseLevel + 1, GetKeyWord(Step.Kind) + ' ' + Step.Description);
  for var Step in Outline.StepsThen do
    OutputLn(BaseLevel + 1, GetKeyWord(Step.Kind) + ' ' + Step.Description);

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

{ TJsonListener }

function TJsonListener.GetKeyWord(const Kind: TSpecItemKind): string;
begin
  case Kind of
    sikFeature: Result := 'Feature';
    sikImplicitRule: Result := '';
    sikRule: Result := 'Rule';
    sikBackground: Result := 'Background';
    sikScenario: Result := 'Scenario';
    sikScenarioOutline: Result := 'Scenario Outline';
    sikExample: Result := 'Example';
    sikExampleInit: Result := '';
    sikGiven: Result := 'Given';
    sikWhen: Result := 'When';
    sikThen: Result := 'Then';
    else
      Result := '';
  end;
end;

function TJsonListener.GetStatus(const Item: ISpecItem): string;
begin
  if Item.RunInfo.State <> srsFinished then
    Result := 'skip'
  else if Item.RunInfo.Result = srrSuccess then
    Result := 'pass'
  else
    Result := 'fail';
end;

procedure TJsonListener.CloseCurrentScenario;
begin
  if Assigned(FCurrentScenario) then
  begin
    FCurrentScenario.AddPair('steps', FCurrentSteps);
    FCurrentScenarios.AddElement(FCurrentScenario);
    FCurrentScenario := nil;
    FCurrentSteps := nil;
  end;
end;

procedure TJsonListener.CloseCurrentFeature;
begin
  CloseCurrentScenario;
  if Assigned(FCurrentFeature) then
  begin
    FCurrentFeature.AddPair('scenarios', FCurrentScenarios);
    FFeatures.AddElement(FCurrentFeature);
    FCurrentFeature := nil;
    FCurrentScenarios := nil;
  end;
end;

procedure TJsonListener.AddStep(const Kind, Description: string; Success: Boolean; Duration: Integer; const ErrorMessage: string);
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

procedure TJsonListener.OnBeginReport(const Context: IRunContext);
begin
  FFeatures := TJSONArray.Create;
  FCurrentFeature := nil;
  FCurrentScenarios := nil;
  FCurrentScenario := nil;
  FCurrentSteps := nil;
  FOutput := '';
end;

procedure TJsonListener.OnEndReport(const Context: IRunContext);
var
  OutputFile: string;
  Root: TJSONObject;
begin
  CloseCurrentFeature;

  Root := TJSONObject.Create;
  try
    Root.AddPair('features', FFeatures);
    FFeatures := nil; // Root now owns FFeatures
    Root.AddPair('passCount', TJSONNumber.Create(Context.ReportCounters.PassCount));
    Root.AddPair('failCount', TJSONNumber.Create(Context.ReportCounters.FailCount));
    Root.AddPair('skipCount', TJSONNumber.Create(Context.ReportCounters.SkipCount));
    Root.AddPair('completedAt', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Context.CompletedAt));
    FOutput := Root.Format(4);
  finally
    Root.Free;
  end;

  // Write to file if output option is set
  OutputFile := GetCliOption('output');
  if not OutputFile.IsEmpty then
  begin
    if TPath.IsRelativePath(OutputFile) then
      OutputFile := ExpandFileName(TPath.Combine(ExtractFilePath(ParamStr(0)), OutputFile));
    TFile.WriteAllText(OutputFile, FOutput, TEncoding.UTF8);
    WriteLn('report detail: file:///' + StringReplace(OutputFile, '\', '/', [rfReplaceAll]));
  end;
end;

procedure TJsonListener.OnItem(const Context: IRunContext; const Item: ISpecItem);
var
  Feat: IFeature;
begin
  case Item.Kind of
    sikFeature: begin
      if Supports(Item, IFeature, Feat) then
      begin
        CloseCurrentFeature;
        FCurrentFeature := TJSONObject.Create;
        FCurrentFeature.AddPair('title', Feat.Title);
        FCurrentFeature.AddPair('narrative', Feat.Narrative);
        FCurrentScenarios := TJSONArray.Create;
      end;
    end;
    sikRule, sikBackground: begin
      CloseCurrentScenario;
      FCurrentScenario := TJSONObject.Create;
      FCurrentScenario.AddPair('kind', GetKeyWord(Item.Kind));
      FCurrentScenario.AddPair('description', Item.Description);
      FCurrentScenario.AddPair('status', GetStatus(Item));
      FCurrentScenario.AddPair('duration', TJSONNumber.Create(Item.RunInfo.ExecTimeMs));
      if (GetStatus(Item) = 'fail') and (Item.RunInfo.ErrMsg <> '') then
        FCurrentScenario.AddPair('error', Item.RunInfo.ErrMsg);
      FCurrentSteps := TJSONArray.Create;
    end;
    sikScenario, sikExample: begin
      CloseCurrentScenario;
      FCurrentScenario := TJSONObject.Create;
      FCurrentScenario.AddPair('kind', GetKeyWord(Item.Kind));
      FCurrentScenario.AddPair('description', Item.Description);
      FCurrentScenario.AddPair('status', GetStatus(Item));
      FCurrentScenario.AddPair('duration', TJSONNumber.Create(Item.RunInfo.ExecTimeMs));
      if (GetStatus(Item) = 'fail') and (Item.RunInfo.ErrMsg <> '') then
        FCurrentScenario.AddPair('error', Item.RunInfo.ErrMsg);
      FCurrentSteps := TJSONArray.Create;
    end;
    sikExampleInit: ;
    sikGiven, sikWhen, sikThen: begin
      AddStep(GetKeyWord(Item.Kind), Item.Description, Item.RunInfo.Result = srrSuccess,
              Item.RunInfo.ExecTimeMs, Item.RunInfo.ErrMsg);
    end;
  end;
end;

procedure TJsonListener.OnEndOutline(const Context: IRunContext; const Outline: IScenarioOutline; const Counters: TSpecCounters);
var
  OutlineObj: TJSONObject;
  HeadersArr, ExamplesArr, ValuesArr: TJSONArray;
  ExampleObj: TJSONObject;
  Example: IScenario;
  Header: string;
  Value: TValue;
begin
  CloseCurrentScenario;

  OutlineObj := TJSONObject.Create;
  OutlineObj.AddPair('kind', 'Scenario Outline');
  OutlineObj.AddPair('description', Outline.Description);
  OutlineObj.AddPair('status', GetStatus(Outline as ISpecItem));
  OutlineObj.AddPair('duration', TJSONNumber.Create((Outline as ISpecItem).RunInfo.ExecTimeMs));

  HeadersArr := TJSONArray.Create;
  for Header in Outline.Headers do
    HeadersArr.Add(Header);
  OutlineObj.AddPair('headers', HeadersArr);

  ExamplesArr := TJSONArray.Create;
  for Example in Outline.Examples do
  begin
    ExampleObj := TJSONObject.Create;
    ValuesArr := TJSONArray.Create;
    for Value in Example.ExampleMeta.Values do
      ValuesArr.Add(Val2Str(Value));
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

function TJsonListener.GetContent: string;
begin
  Result := FOutput;
end;

function TJsonListener.GetFileExt: string;
begin
  Result := 'json';
end;

{ TGherkinListener }

constructor TGherkinListener.Create(AWithResults: Boolean);
begin
  inherited Create;
  FWithResults := AWithResults;
  FLines := TStringList.Create;
  FFilesWritten := TStringList.Create;
  FIndent := 0;
  FOutputDir := ExtractFilePath(ParamStr(0));
end;

destructor TGherkinListener.Destroy;
begin
  FFilesWritten.Free;
  FLines.Free;
  inherited;
end;

procedure TGherkinListener.Configure(const Options: TReporterOptions);
begin
  inherited;
  // WithResults option
  if Assigned(Options) and Options.ContainsKey('results') then
    FWithResults := SameText(Options['results'], 'true');
end;

procedure TGherkinListener.AddLine(const Text: string);
begin
  FLines.Add(StringOfChar(' ', FIndent * 2) + Text);
end;

procedure TGherkinListener.AddTags(const Tags: TSpecTags);
var
  TagArray: TArray<string>;
begin
  TagArray := Tags.ToArray;
  if Length(TagArray) > 0 then
    AddLine('@' + string.Join(' @', TagArray));
end;

function TGherkinListener.GetGherkinKeyword(Kind: TSpecItemKind): string;
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

function TGherkinListener.RestorePlaceholders(const Text: string): string;
begin
  Result := TRegEx.Replace(Text, '#\{([^}]+)\}', '<$1>');
end;

function TGherkinListener.ResultComment(const Item: ISpecItem): string;
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

function TGherkinListener.SanitizeFileName(const Name: string): string;
var
  C: Char;
begin
  Result := '';
  for C in Name do
    if not CharInSet(C, ['\', '/', ':', '*', '?', '"', '<', '>', '|']) then
      Result := Result + C;
  Result := Result.Trim.Replace(' ', '_', [rfReplaceAll]);
  if Result = '' then
    Result := 'Feature';
end;

procedure TGherkinListener.FlushCurrentFeature;
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

procedure TGherkinListener.WriteExamplesTable(const Outline: IScenarioOutline);
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

  for i := 0 to High(Headers) do
    ColWidths[i] := Length(Headers[i]);

  for Example in Outline.Examples do
  begin
    Values := Example.ExampleMeta.Values;
    for i := 0 to High(Values) do
      if i <= High(ColWidths) then
        if Length(Val2Str(Values[i])) > ColWidths[i] then
          ColWidths[i] := Length(Val2Str(Values[i]));
  end;

  HeaderLine := '|';
  for i := 0 to High(Headers) do
    HeaderLine := HeaderLine + ' ' + Headers[i].PadRight(ColWidths[i]) + ' |';
  AddLine(HeaderLine);

  for Example in Outline.Examples do
  begin
    Values := Example.ExampleMeta.Values;
    DataLine := '|';
    for i := 0 to High(Values) do
      if i <= High(ColWidths) then
        DataLine := DataLine + ' ' + Val2Str(Values[i]).PadRight(ColWidths[i]) + ' |';
    AddLine(DataLine + ResultComment(Example));
  end;
end;

procedure TGherkinListener.OnBeginReport(const Context: IRunContext);
var
  OutputDir: string;
begin
  FLines.Clear;
  FFilesWritten.Clear;
  FCurrentFeatureName := '';
  FIndent := 0;
  OutputDir := GetCliOption('output');
  if not OutputDir.IsEmpty then
  begin
    if TPath.IsRelativePath(OutputDir) then
      OutputDir := ExpandFileName(TPath.Combine(ExtractFilePath(ParamStr(0)), OutputDir));
    FOutputDir := OutputDir;
    ForceDirectories(FOutputDir);
  end;
end;

procedure TGherkinListener.OnEndReport(const Context: IRunContext);
var
  FilePath: string;
begin
  FlushCurrentFeature;
  if FFilesWritten.Count > 0 then
  begin
    WriteLn(Format('Gherkin: %d feature file(s) written to: %s', [FFilesWritten.Count, FOutputDir]));
    for FilePath in FFilesWritten do
      WriteLn('  - ' + ExtractFileName(FilePath));
  end;
end;

procedure TGherkinListener.OnItem(const Context: IRunContext; const Item: ISpecItem);
var
  Feature: IFeature;
  Line: string;
begin
  case Item.Kind of
    sikFeature: begin
      FlushCurrentFeature;
      if Supports(Item, IFeature, Feature) then
      begin
        FCurrentFeatureName := Feature.Title;
        FIndent := 0;
        AddTags(Item.Tags);
        AddLine('Feature: ' + Feature.Title);
        if Feature.Narrative <> '' then
        begin
          Inc(FIndent);
          for Line in Feature.Narrative.Split([#13#10, #10]) do
            AddLine(Line);
          Dec(FIndent);
        end;
        AddLine('');
        FIndent := 1;
      end;
    end;

    sikRule: begin
      FIndent := 1;
      AddLine('');
      AddTags(Item.Tags);
      AddLine('Rule: ' + Item.Description);
      FIndent := 2;
    end;

    sikBackground: begin
      AddLine('Background:' + ResultComment(Item));
      FIndent := 2;
    end;

    sikScenario: begin
      FIndent := 1;
      AddLine('');
      AddTags(Item.Tags);
      AddLine('Scenario: ' + Item.Description + ResultComment(Item));
      FIndent := 2;
    end;

    sikImplicitRule: ;

    sikGiven, sikWhen, sikThen: begin
      AddLine(GetGherkinKeyword(Item.Kind) + ' ' + Item.Description + ResultComment(Item));
    end;
  end;
end;

procedure TGherkinListener.OnEndOutline(const Context: IRunContext; const Outline: IScenarioOutline; const Counters: TSpecCounters);
var
  Step: IScenarioStep;
begin
  FIndent := 1;
  AddLine('');
  AddTags((Outline as ISpecItem).Tags);
  AddLine('Scenario Outline: ' + Outline.Description + ResultComment(Outline as ISpecItem));
  FIndent := 2;

  for Step in Outline.StepsGiven do
    AddLine(GetGherkinKeyword(Step.Kind) + ' ' + RestorePlaceholders(Step.Description));
  for Step in Outline.StepsWhen do
    AddLine(GetGherkinKeyword(Step.Kind) + ' ' + RestorePlaceholders(Step.Description));
  for Step in Outline.StepsThen do
    AddLine(GetGherkinKeyword(Step.Kind) + ' ' + RestorePlaceholders(Step.Description));

  AddLine('');
  AddLine('Examples:');
  FIndent := 3;
  WriteExamplesTable(Outline);
end;

function TGherkinListener.GetContent: string;
var
  i: Integer;
  FileName, FeatureName: string;
begin
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

function TGherkinListener.GetFileExt: string;
begin
  Result := 'md';
end;

{ TLiveListener }

constructor TLiveListener.Create(APort: Integer);
begin
  inherited Create;
  FPort := APort;
  FWaitClient := False;
  FEvents := TStringList.Create;
  FEventsLock := TCriticalSection.Create;
  FLiveClients := TList<TIdContext>.Create;
  FClientsLock := TCriticalSection.Create;
  FReportFinished := False;
  FContext := nil;

  FServer := TIdHTTPServer.Create(nil);
  FServer.DefaultPort := FPort;
  FServer.OnCommandGet := HandleRequest;
end;

destructor TLiveListener.Destroy;
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

procedure TLiveListener.Configure(const Options: TReporterOptions);
begin
  inherited;
  if Assigned(Options) and Options.ContainsKey('port') then
  begin
    FPort := StrToIntDef(Options['port'], FPort);
    FServer.DefaultPort := FPort;
  end;
  FWaitClient := SameText(GetCliOption('wait', 'false'), 'true');
end;

function TLiveListener.ShowHelp: Boolean;
begin
  WriteLn('Live Listener - Real-time test results in browser');
  WriteLn;
  WriteLn('Options:');
  WriteLn('  port=<number>   HTTP server port (default: 8080)');
  WriteLn('  wait            Wait for browser connection before running tests');
  WriteLn;
  WriteLn('Examples:');
  WriteLn('  -r live                 Start on default port 8080');
  WriteLn('  -r live:port=9000       Start on port 9000');
  WriteLn('  -r live:wait            Wait for browser before running');
  WriteLn('  -r live:port=9000,wait  Custom port + wait for browser');
  WriteLn;
  WriteLn('Usage:');
  WriteLn('  1. Run tests with -r live');
  WriteLn('  2. Open http://localhost:<port> in browser');
  WriteLn('  3. Watch results update in real-time');
  Result := True;
end;

function TLiveListener.UseConsole: Boolean;
begin
  Result := True;
end;

function TLiveListener.GetContent: string;
begin
  Result := Format('Live Dashboard served at http://localhost:%d', [FPort]);
end;

function TLiveListener.HasConnectedClients: Boolean;
begin
  FClientsLock.Enter;
  try
    Result := FLiveClients.Count > 0;
  finally
    FClientsLock.Leave;
  end;
end;

function TLiveListener.BuildEventJson(const EventType: string; const Data: TJSONObject): string;
var
  Wrapper: TJSONObject;
begin
  Wrapper := TJSONObject.Create;
  try
    Wrapper.AddPair('event', EventType);
    for var Pair in Data do
      Wrapper.AddPair(Pair.JsonString.Value, Pair.JsonValue.Clone as TJSONValue);
    Result := Wrapper.ToJSON;
  finally
    Wrapper.Free;
  end;
end;

function TLiveListener.StepsToJsonArray(const Steps: TList<IScenarioStep>; const StepType: string): TJSONArray;
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

procedure TLiveListener.BroadcastEvent(const EventJson: string);
var
  Client: TIdContext;
  SSEData: string;
begin
  SSEData := 'data: ' + EventJson + #10#10;

  FEventsLock.Enter;
  try
    FEvents.Add(EventJson);
  finally
    FEventsLock.Leave;
  end;

  FClientsLock.Enter;
  try
    for Client in FLiveClients do
    begin
      try
        Client.Connection.IOHandler.Write(SSEData, IndyTextEncoding_UTF8);
      except
      end;
    end;
  finally
    FClientsLock.Leave;
  end;
end;

procedure TLiveListener.HandleRequest(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  Doc: string;
begin
  Doc := ARequestInfo.Document;

  if SameText(Doc, '/events') or Doc.EndsWith('/events', True) then
  begin
    AContext.Connection.IOHandler.WriteLn('HTTP/1.1 200 OK');
    AContext.Connection.IOHandler.WriteLn('Content-Type: text/event-stream');
    AContext.Connection.IOHandler.WriteLn('Cache-Control: no-cache');
    AContext.Connection.IOHandler.WriteLn('Connection: keep-alive');
    AContext.Connection.IOHandler.WriteLn('Access-Control-Allow-Origin: *');
    AContext.Connection.IOHandler.WriteLn('');

    FClientsLock.Enter;
    try
      FLiveClients.Add(AContext);
    finally
      FClientsLock.Leave;
    end;

    try
      AContext.Connection.IOHandler.Write(': welcome'#10#10, IndyTextEncoding_UTF8);
    except
    end;

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

    while not FReportFinished and AContext.Connection.Connected do
      Sleep(200);

    FClientsLock.Enter;
    try
      FLiveClients.Remove(AContext);
    finally
      FClientsLock.Leave;
    end;
  end
  else
  begin
    AResponseInfo.ContentType := 'text/html; charset=utf-8';
    AResponseInfo.ContentText := StringReplace(LIVE_DASHBOARD_HTML,
      '{{MINISPEC_VERSION}}', TMiniSpec.Version, [rfReplaceAll]);
  end;
end;

procedure TLiveListener.OnBeginReport(const Context: IRunContext);
var
  Data: TJSONObject;
  WaitCount: Integer;
begin
  FContext := Context;
  FReportFinished := False;
  FEvents.Clear;

  try
    FServer.Active := True;
    WriteLn(Format('Live Dashboard: http://localhost:%d', [FPort]));

    if FWaitClient then
    begin
      WriteLn('Waiting for browser connection...');
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
    end;
  except
    on E: Exception do
      WriteLn('Live server error: ' + E.Message);
  end;

  Data := TJSONObject.Create;
  try
    Data.AddPair('timestamp', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Now));
    BroadcastEvent(BuildEventJson('report:start', Data));
  finally
    Data.Free;
  end;
end;

procedure TLiveListener.OnEndReport(const Context: IRunContext);
var
  Data: TJSONObject;
begin
  Data := TJSONObject.Create;
  try
    Data.AddPair('pass', TJSONNumber.Create(Context.ReportCounters.PassCount));
    Data.AddPair('fail', TJSONNumber.Create(Context.ReportCounters.FailCount));
    Data.AddPair('skip', TJSONNumber.Create(Context.ReportCounters.SkipCount));
    Data.AddPair('completedAt', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Context.CompletedAt));
    BroadcastEvent(BuildEventJson('report:end', Data));
  finally
    Data.Free;
  end;

  FReportFinished := True;
  FServer.Active := False;
  FContext := nil;
end;

procedure TLiveListener.OnBeginFeature(const Context: IRunContext; const Feature: IFeature);
var
  Data: TJSONObject;
  TagsArray, BgSteps: TJSONArray;
begin
  if not Assigned(Feature) then Exit;

  Data := TJSONObject.Create;
  try
    Data.AddPair('name', Feature.Title);
    Data.AddPair('narrative', Feature.Narrative);
    TagsArray := TJSONArray.Create;
    for var Tag in Feature.Tags.ToArray do
      TagsArray.Add(Tag);
    Data.AddPair('tags', TagsArray);
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

procedure TLiveListener.OnEndFeature(const Context: IRunContext; const Feature: IFeature; const Counters: TSpecCounters);
var
  Data: TJSONObject;
begin
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

procedure TLiveListener.OnEndScenario(const Context: IRunContext; const Scenario: IScenario; const Counters: TSpecCounters);
var
  Data: TJSONObject;
  StepsArray, TempArr: TJSONArray;
  IsSkipped: Boolean;
begin
  IsSkipped := Scenario.RunInfo.State = srsSkiped;

  Data := TJSONObject.Create;
  try
    Data.AddPair('name', Scenario.Description);
    Data.AddPair('skipped', TJSONBool.Create(IsSkipped));
    if IsSkipped then
      Data.AddPair('success', TJSONBool.Create(True))
    else
      Data.AddPair('success', TJSONBool.Create(Scenario.RunInfo.IsSuccess));
    Data.AddPair('ms', TJSONNumber.Create(Counters.ElapsedMs));
    if not IsSkipped and not Scenario.RunInfo.IsSuccess and (Scenario.RunInfo.ErrMsg <> '') then
      Data.AddPair('error', Scenario.RunInfo.ErrMsg);

    StepsArray := TJSONArray.Create;
    TempArr := StepsToJsonArray(Scenario.StepsGiven, 'Given');
    for var i := 0 to TempArr.Count - 1 do
      StepsArray.AddElement(TempArr.Items[i].Clone as TJSONValue);
    TempArr.Free;
    TempArr := StepsToJsonArray(Scenario.StepsWhen, 'When');
    for var i := 0 to TempArr.Count - 1 do
      StepsArray.AddElement(TempArr.Items[i].Clone as TJSONValue);
    TempArr.Free;
    TempArr := StepsToJsonArray(Scenario.StepsThen, 'Then');
    for var i := 0 to TempArr.Count - 1 do
      StepsArray.AddElement(TempArr.Items[i].Clone as TJSONValue);
    TempArr.Free;
    Data.AddPair('steps', StepsArray);

    Data.AddPair('totalPass', TJSONNumber.Create(Context.ReportCounters.PassCount));
    Data.AddPair('totalFail', TJSONNumber.Create(Context.ReportCounters.FailCount));
    Data.AddPair('totalSkip', TJSONNumber.Create(Context.ReportCounters.SkipCount));
    BroadcastEvent(BuildEventJson('scenario:end', Data));
  finally
    Data.Free;
  end;
end;

procedure TLiveListener.OnEndOutline(const Context: IRunContext; const Outline: IScenarioOutline; const Counters: TSpecCounters);
var
  Data: TJSONObject;
  StepsArray, HeadersArray, ExamplesArray, RowArray, TempArr: TJSONArray;
  ExampleObj: TJSONObject;
begin
  Data := TJSONObject.Create;
  try
    Data.AddPair('name', Outline.Description);
    Data.AddPair('type', 'outline');

    HeadersArray := TJSONArray.Create;
    for var H in Outline.Headers do
      HeadersArray.Add(H);
    Data.AddPair('headers', HeadersArray);

    StepsArray := TJSONArray.Create;
    TempArr := StepsToJsonArray(Outline.StepsGiven, 'Given');
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

    ExamplesArray := TJSONArray.Create;
    for var Example in Outline.Examples do
    begin
      if Example.RunInfo.State in [srsFinished, srsSkiped] then
      begin
        var IsSkipped := Example.RunInfo.State = srsSkiped;

        ExampleObj := TJSONObject.Create;
        RowArray := TJSONArray.Create;
        for var Val in Example.ExampleMeta.Values do
          RowArray.Add(Val2Str(Val));
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

    Data.AddPair('totalPass', TJSONNumber.Create(Context.ReportCounters.PassCount));
    Data.AddPair('totalFail', TJSONNumber.Create(Context.ReportCounters.FailCount));
    Data.AddPair('totalSkip', TJSONNumber.Create(Context.ReportCounters.SkipCount));

    BroadcastEvent(BuildEventJson('outline:end', Data));
  finally
    Data.Free;
  end;
end;

end.
