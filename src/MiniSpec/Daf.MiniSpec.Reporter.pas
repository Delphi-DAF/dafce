unit Daf.MiniSpec.Reporter;

interface
uses
  System.Classes,
  System.RegularExpressions,
  System.Generics.Collections,
  System.SysUtils,
  System.JSON,
  System.Rtti,
  System.IniFiles,
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

  ISpecListener = interface(IInvokable)
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

  TSpecRunner = class(TInterfacedObject, ISpecListener, IRunContext)
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

end.
