unit Daf.MiniSpec;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.RegularExpressions,
  System.Diagnostics,
  System.Rtti,
  System.Classes,
  Daf.MiniSpec.Types,
  Daf.MiniSpec.Builders,
  Daf.MiniSpec.Reporter,
  Daf.MiniSpec.Reporter.Console,
  Daf.MiniSpec.Reporter.Json,
  Daf.MiniSpec.Reporter.Gherkin,
  Daf.MiniSpec.Reporter.Live,
  Daf.MiniSpec.Expects;

type
{$SCOPEDENUMS ON}
  TRunMode = (rmRun, rmListTags, rmQuery, rmHelp, rmReporterHelp);
{$SCOPEDENUMS Off}

  TMiniSpec = class
  public
    const Version = '1.2.0';
  strict private
    FSuite: ISpecSuite;
    FRunner: ISpecRunner;
    FListeners: TList<ISpecListener>;
    FOptions: TMiniSpecOptions;
    FRunMode: TRunMode;
    FQueryExpr: string;
    FConfigFile: string;
    FConfigExisted: Boolean;
    class var FInstance: TMiniSpec;
  protected
    class function CreateSingleton: TMiniSpec;
    class function Instance: TMiniSpec; static;
    class procedure ParseReporterSpec(const Spec: string; out Name: string; out Options: TReporterOptions);
    function CreateListener(const Name: string; const Options: TReporterOptions): ISpecListener;
    procedure LoadConfig;
    procedure ParseArgs;
    procedure ListTags;
    procedure QueryTags;
    procedure ShowHelp;
  public
    constructor Create;
    destructor Destroy; override;
    class destructor ClasDestroy;
    {$REGION 'Fluent api for setup'}
    function Reporter(const Spec: string): TMiniSpec;overload;
    function Runner: ISpecRunner;
    function Tags: string;overload;
    procedure Tags(const Value: string);overload;
    function Pause: Boolean;overload;
    function Pause(const Value: Boolean): TMiniSpec;overload;
    function DryRun: Boolean;overload;
    function DryRun(const Value: Boolean): TMiniSpec;overload;
    /// <summary>
    /// Sets the title/description of the test suite.
    /// </summary>
    function Category(const Name: string): TMiniSpec;
    /// <summary>
    /// Adds a Before hook that runs once before all features.
    /// </summary>
    function Before(const Description: string; Hook: THookProc): TMiniSpec;
    /// <summary>
    /// Adds an After hook that runs once after all features.
    /// </summary>
    function After(const Description: string; Hook: THookProc): TMiniSpec;
    {$ENDREGION}
    procedure Register(Feature: IFeature);
    procedure Run;
    /// <summary>
    /// Returns the test suite containing all features.
    /// </summary>
    function Suite: ISpecSuite;
 end;

function Expect(const Value: Variant): TExpect; overload;
function Expect(Proc: TProc): TExpectException; overload;
function Expect(const Capture: TCapturedRaise): TExpectException; overload;
/// <summary>
/// Returns the captured exception from a When step.
/// Use in Then step: Expect(Raised).ToBe(EDivByZero)
/// </summary>
function Raised: TCapturedRaise;
function Feature(const Description: string): TFeatureBuilder;
function MiniSpec: TMiniSpec;inline;

implementation
uses
  System.IOUtils,
  System.TypInfo,
  Daf.MiniSpec.Utils,
  Daf.MiniSpec.Filter;

function MiniSpec: TMiniSpec;
begin
  Result := TMiniSpec.Instance;
end;

function Expect(const Value: Variant): TExpect;
begin
  Result := TExpect.Create(Value);
end;

function Expect(Proc: TProc): TExpectException;
begin
  Result := TExpectException.Create(TExceptionCapture.Create(Proc));
end;

function Expect(const Capture: TCapturedRaise): TExpectException;
begin
  Result := TExpectException.Create(TExceptionCapture.CreateFrom(Capture));
end;

function Raised: TCapturedRaise;
var
  Scenario: IScenario;
begin
  Scenario := GetCurrentScenario;
  if not Assigned(Scenario) then
    raise TExpect.Fail('Raised called outside of a scenario context');
  Result := Scenario.ConsumeCapturedRaise;
end;

function Feature(const Description: string): TFeatureBuilder;
begin
  Result := TFeatureBuilder.Create(Description);
end;

{ TMiniSpec }

class function TMiniSpec.CreateSingleton: TMiniSpec;
begin
  Result := TMiniSpec.Create;
end;

class function TMiniSpec.Instance: TMiniSpec;
begin
  if not Assigned(FInstance) then
    FInstance := CreateSingleton;
  Result := FInstance;
end;

constructor TMiniSpec.Create;
begin
  inherited;
  FSuite := TSpecSuite.Create;
  FOptions := TMiniSpecOptions.Create;
  FRunner := TSpecRunner.Create;
  FListeners := TList<ISpecListener>.Create;
  FConfigFile := ExtractFilePath(ParamStr(0)) + 'MiniSpec.ini';
  FConfigExisted := FileExists(FConfigFile);
end;

destructor TMiniSpec.Destroy;
begin
  FRunner := nil;  // Release runner first, it has refs to listeners
  FListeners.Free;
  FOptions.Free;
  FSuite := nil;  // Release via ARC
  inherited;
end;

class destructor TMiniSpec.ClasDestroy;
begin
  FInstance.Free;
end;

function TMiniSpec.Suite: ISpecSuite;
begin
  Result := FSuite;
end;

function TMiniSpec.Category(const Name: string): TMiniSpec;
begin
  Result := Self;
  FSuite.Title := Name;
end;

function TMiniSpec.Before(const Description: string; Hook: THookProc): TMiniSpec;
begin
  Result := Self;
  FSuite.AddBeforeHook(Description, Hook);
end;

function TMiniSpec.After(const Description: string; Hook: THookProc): TMiniSpec;
begin
  Result := Self;
  FSuite.AddAfterHook(Description, Hook);
end;

procedure TMiniSpec.Register(Feature: IFeature);
begin
  FSuite.AddFeature(Feature);
end;

class procedure TMiniSpec.ParseReporterSpec(const Spec: string; out Name: string; out Options: TReporterOptions);
var
  ColonPos, EqPos: Integer;
  OptPart, Pair, Key, Value: string;
  Pairs: TArray<string>;
begin
  Options := TReporterOptions.Create;
  ColonPos := Pos(':', Spec);
  if ColonPos = 0 then
  begin
    Name := Spec;
    Exit;
  end;
  Name := Copy(Spec, 1, ColonPos - 1);
  OptPart := Copy(Spec, ColonPos + 1, MaxInt);
  Pairs := OptPart.Split([',']);
  for Pair in Pairs do
  begin
    EqPos := Pos('=', Pair);
    if EqPos > 0 then
    begin
      Key := Copy(Pair, 1, EqPos - 1).Trim;
      Value := Copy(Pair, EqPos + 1, MaxInt).Trim;
      Options.AddOrSetValue(Key, Value);
    end
    else if Pair.Trim <> '' then
    begin
      // Flag sin valor: se interpreta como true
      Options.AddOrSetValue(Pair.Trim, 'true');
    end;
  end;
end;

function TMiniSpec.CreateListener(const Name: string; const Options: TReporterOptions): ISpecListener;
var
  Port: Integer;
begin
  if SameText(Name, 'console') then
    Result := TConsoleReporter.Create
  else if SameText(Name, 'json') then
    Result := TJsonReporter.Create
  else if SameText(Name, 'gherkin') then
    Result := TGherkinReporter.Create(False)
  else if SameText(Name, 'gherkin-results') then
    Result := TGherkinReporter.Create(True)
  else if SameText(Name, 'live') then
  begin
    Port := 8080;
    if Options.ContainsKey('port') then
      Port := StrToIntDef(Options['port'], 8080);
    Result := TLiveReporter.Create(Port);
  end
  else
    raise Exception.CreateFmt('Unknown reporter name: %s', [Name]);

  Result.Configure(Options);
end;

{$REGION 'Fluent api for setup'}
function TMiniSpec.Reporter(const Spec: string): TMiniSpec;
var
  Name: string;
  Options: TReporterOptions;
  Pair: TPair<string, string>;
  Listener: ISpecListener;
begin
  Result := Self;
  ParseReporterSpec(LowerCase(Spec), Name, Options);
  try
    Listener := CreateListener(Name, Options);
    FListeners.Add(Listener);
    // Guardar en FOptions para persistencia
    FOptions.ReporterName := Name;
    for Pair in Options do
      FOptions.SetReporterOption(Name, Pair.Key, Pair.Value);
    // Check if reporter help was requested
    if Options.ContainsKey('help') and SameText(Options['help'], 'true') then
      FRunMode := TRunMode.rmReporterHelp;
  finally
    Options.Free;
  end;
end;

function TMiniSpec.Runner: ISpecRunner;
begin
  Result := FRunner;
end;

procedure TMiniSpec.Tags(const Value: string);
begin
  FOptions.Filter := Value;
end;

function TMiniSpec.Pause: Boolean;
begin
  Result := FOptions.Pause;
end;

function TMiniSpec.Pause(const Value: Boolean): TMiniSpec;
begin
  Result := Self;
  FOptions.Pause := Value;
end;

function TMiniSpec.Tags: string;
begin
  Result := FOptions.Filter;
end;

function TMiniSpec.DryRun: Boolean;
begin
  Result := FOptions.IsDryRun;
end;

function TMiniSpec.DryRun(const Value: Boolean): TMiniSpec;
begin
  Result := Self;
  FOptions.IsDryRun := Value;
end;
{$ENDREGION}

procedure TMiniSpec.LoadConfig;
begin
  // Solo cargar opciones desde archivo .ini si existe
  // No crear listeners aquí - se hace en Run() basándose en la config final
  FOptions.LoadFromFile(FConfigFile);
end;

procedure TMiniSpec.ParseArgs;
var
  idxParam: Integer;
  function NextArg: string;
  begin
    Inc(idxParam);
    if idxParam <= ParamCount then
      Result := ParamStr(idxParam)
    else
      Result := '';
  end;
begin
  // Primero cargar config desde archivo
  LoadConfig;

  // Luego procesar argumentos CLI (sobrescriben config)
  idxParam := 0;
  FRunMode := TRunMode.rmRun;
  while idxParam <= ParamCount do
  begin
    var Param := NextArg;
    if (Param = '--filter') or (Param = '-f') then
      Tags(NextArg)
    else if (Param = '--reporter') or (Param = '-r') then
      Reporter(NextArg)
    else if (Param = '--tags') or (Param = '-t') then
      FRunMode := TRunMode.rmListTags
    else if (Param = '--query') or (Param = '-q') then
    begin
      FRunMode := TRunMode.rmQuery;
      FQueryExpr := NextArg;
    end
    else if (Param = '--help') or (Param = '-h') then
      FRunMode := TRunMode.rmHelp
    else if Param = '--dry-run' then
      FOptions.IsDryRun := True
    else if Param = '--pause' then
      FOptions.Pause := True
    else if Param = '--stacktrace' then
      FOptions.StackTrace := True;
  end;

  // Guardar config si el archivo no existía
  if not FConfigExisted then
    FOptions.SaveToFile(FConfigFile);
end;

procedure TMiniSpec.Run;
var
  SpecFilter: TSpecFilter;
  Matcher: TSpecMatcher;
  Listener: ISpecListener;
begin
  ParseArgs;

  // Modos de consulta sin ejecutar tests
  case FRunMode of
    TRunMode.rmHelp:
    begin
      ShowHelp;
      Exit;
    end;
    TRunMode.rmReporterHelp:
    begin
      WriteLn;
      WriteLn('+----------------------+');
      WriteLn('|   MiniSpec v' + Version + '    |');
      WriteLn('| Full specs, zero fat |');
      WriteLn('+----------------------+');
      WriteLn;
      // Show help for the last listener added
      if (FListeners.Count > 0) and FListeners.Last.ShowHelp then
        Exit
      else
      begin
        WriteLn('No help available for this reporter.');
        Exit;
      end;
    end;
    TRunMode.rmListTags:
    begin
      ListTags;
      Exit;
    end;
    TRunMode.rmQuery:
    begin
      QueryTags;
      Exit;
    end;
  end;

  // Si no hay listeners explícitos, crear desde config o usar console por defecto
  if FListeners.Count = 0 then
  begin
    if not FOptions.ReporterName.IsEmpty then
    begin
      var RepOpts := FOptions.GetReporterOptions(FOptions.ReporterName);
      FListeners.Add(CreateListener(FOptions.ReporterName, RepOpts));
    end
    else
      FListeners.Add(TConsoleReporter.Create);
  end;

  // Agregar todos los listeners al runner
  for Listener in FListeners do
    FRunner.AddListener(Listener);

  // Modo normal: ejecutar tests
  OSShell.UseUTF8;
  WriteLn;
  WriteLn('+----------------------+');
  WriteLn('|   MiniSpec v' + Version + '    |');
  WriteLn('| Full specs, zero fat |');
  WriteLn('+----------------------+');
  WriteLn;

  SpecFilter := TSpecFilter.Parse(Tags);
  try
    if SpecFilter.IsEmpty then
      Matcher := nil
    else
      Matcher := function(const Scenario: IScenario): Boolean
                 var
                   Context: TSpecFilterContext;
                 begin
                   Context := TSpecFilterContext.FromScenario(Scenario);
                   Result := SpecFilter.Matches(Context);
                 end;
    FOptions.SpecMatcher := Matcher;

    // Run suite-level Before hooks
    FSuite.RunBeforeHooks;
    try
      // Ejecutar TODAS las Features - el conteo debe reflejar el total definido
      // Cada Feature/Rule/Scenario decide si se ejecuta o marca como Skip
      for var F in FSuite.Features do
        F.Run(Matcher);
    finally
      // Run suite-level After hooks
      FSuite.RunAfterHooks;
    end;

    // Usar el runner para reportar (notificará a todos los listeners)
    FRunner.Report(FSuite, FOptions);
  finally
    SpecFilter.Free;
  end;
  WriteLn(Format('Pass: %d | Fail: %d | Skip: %d | Total: %d Specs in %d Features | %d ms | at %s',
    [FRunner.PassCount, FRunner.FailCount, FRunner.SkipCount,
     FRunner.PassCount + FRunner.FailCount + FRunner.SkipCount,
     FRunner.FeatureCount, FRunner.ElapsedMs,
     FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', FRunner.CompletedAt)]));

  if Pause then
    OSShell.WaitForShutdown;
end;

procedure TMiniSpec.ListTags;
var
  TagCounts: TDictionary<string, Integer>;
  ScenarioCount: Integer;
  FeatureCount: Integer;

  procedure CollectTags(const Tags: TSpecTags);
  var
    Tag: string;
  begin
    for Tag in Tags.ToArray do
    begin
      if TagCounts.ContainsKey(Tag) then
        TagCounts[Tag] := TagCounts[Tag] + 1
      else
        TagCounts.Add(Tag, 1);
    end;
  end;

begin
  TagCounts := TDictionary<string, Integer>.Create;
  try
    FeatureCount := 0;
    ScenarioCount := 0;

    for var F in FSuite.Features do
    begin
      Inc(FeatureCount);
      CollectTags(F.Tags);
      for var S in F.Scenarios do
      begin
        Inc(ScenarioCount);
        CollectTags(S.Tags);
      end;
    end;

    WriteLn(Format('Features: %d | Scenarios: %d', [FeatureCount, ScenarioCount]));
    WriteLn('');
    WriteLn('Tags:');
    for var Pair in TagCounts do
      WriteLn(Format('  @%-20s %d', [Pair.Key, Pair.Value]));
  finally
    TagCounts.Free;
  end;
end;

procedure TMiniSpec.QueryTags;
var
  SpecFilter: TSpecFilter;
  MatchCount: Integer;
begin
  if FQueryExpr.IsEmpty then
  begin
    WriteLn('Error: --query requires a filter expression');
    WriteLn('Usage: --query "@tag1 and @tag2" or --query "F:Calculator"');
    Exit;
  end;

  SpecFilter := TSpecFilter.Parse(FQueryExpr);
  try
    MatchCount := 0;
    WriteLn(Format('Query: %s', [FQueryExpr]));
    WriteLn('');

    for var F in FSuite.Features do
    begin
      var FeatureShown := False;

      for var R in F.Rules do
        for var S in R.Scenarios do
        begin
          var Context := TSpecFilterContext.FromScenario(S);

          if SpecFilter.Matches(Context) then
          begin
            if not FeatureShown then
            begin
              WriteLn('Feature: ' + F.Title);
              FeatureShown := True;
            end;
            if (R.Kind = sikRule) then
              WriteLn('  Rule: ' + R.Description);
            WriteLn('    - ' + S.Description);
            Inc(MatchCount);
          end;
        end;
    end;

    WriteLn('');
    WriteLn(Format('Matching scenarios: %d', [MatchCount]));
  finally
    SpecFilter.Free;
  end;
end;

procedure TMiniSpec.ShowHelp;
begin
  WriteLn;
  WriteLn('+----------------------+');
  WriteLn('|   MiniSpec v' + Version + '    |');
  WriteLn('| Full specs, zero fat |');
  WriteLn('+----------------------+');
  WriteLn('');
  WriteLn('Usage: ' + ExtractFileName(ParamStr(0)) + ' [options]');
  WriteLn('');
  WriteLn('Options:');
  WriteLn('  -h, --help              Show this help message');
  WriteLn('  -r, --reporter <spec>   Reporter with options: <name>[:<key>=<value>,...]');
  WriteLn('  -f, --filter <expr>     Run only scenarios matching tag expression');
  WriteLn('  -t, --tags              List all tags with scenario counts (no tests run)');
  WriteLn('  -q, --query <expr>      Show scenarios matching expression (no tests run)');
  WriteLn('  --dry-run               Show what would run without executing tests');
  WriteLn('  --pause                 Wait for keypress before closing console');
  WriteLn('  --stacktrace            Show full stack trace on errors');
  WriteLn('');
  WriteLn('Reporters:');
  WriteLn('  console                 Console output (default)');
  WriteLn('  html:output=<file>      HTML dashboard report');
  WriteLn('  json:output=<file>      JSON report');
  WriteLn('  gherkin:output=<dir>    Export .feature files');
  WriteLn('  gherkin-results:output=<dir>  Export .feature with results');
  WriteLn('  live[:port=N][,wait]    Live dashboard on localhost (default port: 8080)');
  WriteLn('');
  WriteLn('Filter expressions:');
  WriteLn('  @tag                    Scenarios with tag');
  WriteLn('  ~@tag                   Scenarios without tag (also: not @tag)');
  WriteLn('  Feat:text               Feature title contains text (case-insensitive)');
  WriteLn('  Scen:text               Scenario description contains text');
  WriteLn('  Rule:text               Rule description contains text');
  WriteLn('  Cat:text                Category name contains text');
  WriteLn('  @a and @b               Match both conditions');
  WriteLn('  @a or @b                Match either condition');
  WriteLn('  (expr) and ~@c          Complex expressions with parentheses');
  WriteLn('');
  WriteLn('Examples:');
  WriteLn('  ' + ExtractFileName(ParamStr(0)) + ' -f "@unit"');
  WriteLn('  ' + ExtractFileName(ParamStr(0)) + ' -f "@integration and ~@slow"');
  WriteLn('  ' + ExtractFileName(ParamStr(0)) + ' -f "Feat:Calculator"');
  WriteLn('  ' + ExtractFileName(ParamStr(0)) + ' -f "Cat:Lifecycle"');
  WriteLn('  ' + ExtractFileName(ParamStr(0)) + ' -f "Scen:division and @arithmetic"');
  WriteLn('  ' + ExtractFileName(ParamStr(0)) + ' -r html:output=report.html');
  WriteLn('  ' + ExtractFileName(ParamStr(0)) + ' -r live:port=9000');
  WriteLn('  ' + ExtractFileName(ParamStr(0)) + ' -t');
  WriteLn('  ' + ExtractFileName(ParamStr(0)) + ' -q "F:Calculator"');
end;

initialization
 TRttiContext.KeepContext;
finalization
 TRttiContext.DropContext;
end.
