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
  Daf.MiniSpec.Expects;

type
{$SCOPEDENUMS ON}
  TRunMode = (rmRun, rmListTags, rmQuery, rmHelp);
{$SCOPEDENUMS Off}
  TReporterOptions = TDictionary<string, string>;

  TMiniSpec = class
  public
    const Version = '1.5.0';
  strict private
    FFeatures: TList<IFeature>;
    FReporter: ISpecReporter;
    FPause: Boolean;
    FTags: string;
    FRunMode: TRunMode;
    FQueryExpr: string;
    FDryRun: Boolean;
    class var FInstance: TMiniSpec;
  public
  protected
    class function CreateSingleton: TMiniSpec;
    class function Instance: TMiniSpec; static;
    class procedure ParseReporterSpec(const Spec: string; out Name: string; out Options: TReporterOptions);
    function CreateReporter(const Name: string; const Options: TReporterOptions): ISpecReporter;
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
    function Reporter: ISpecReporter;overload;
    function Tags: string;overload;
    procedure Tags(const Value: string);overload;
    function Pause: Boolean;overload;
    function Pause(const Value: Boolean): TMiniSpec;overload;
    function DryRun: Boolean;overload;
    function DryRun(const Value: Boolean): TMiniSpec;overload;
    {$ENDREGION}
    procedure Register(Feature: IFeature);
    procedure Run;
 end;

function Expect(const Value: Variant): TExpect; overload;
function Expect(Proc: TProc): TExpectException; overload;
function Feature(const Description: string): TFeatureBuilder;
function MiniSpec: TMiniSpec;inline;

implementation
uses
  System.IOUtils,
  System.TypInfo,
  Daf.MiniSpec.Utils,
  Daf.MiniSpec.TagFilter;

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
  FFeatures := TList<IFeature>.Create;
  FReporter := TConsoleReporter.Create;
end;

destructor TMiniSpec.Destroy;
begin
  FFeatures.Free;
  inherited;
end;

class destructor TMiniSpec.ClasDestroy;
begin
  FInstance.Free;
end;

procedure TMiniSpec.Register(Feature: IFeature);
begin
  FFeatures.Add(Feature);
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
    end;
  end;
end;

function TMiniSpec.CreateReporter(const Name: string; const Options: TReporterOptions): ISpecReporter;
var
  Port: Integer;
begin
  if SameText(Name, 'console') then
    Result := TConsoleReporter.Create
  else if SameText(Name, 'html') then
    Result := THTMLReporter.Create
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
    Result := TLiveReporter.Create(TConsoleReporter.Create, Port);
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
begin
  Result := Self;
  ParseReporterSpec(LowerCase(Spec), Name, Options);
  try
    FReporter := CreateReporter(Name, Options);
  finally
    Options.Free;
  end;
end;

function TMiniSpec.Reporter: ISpecReporter;
begin
  Result := FReporter;
end;

procedure TMiniSpec.Tags(const Value: string);
begin
  FTags := Value;
end;

function TMiniSpec.Pause: Boolean;
begin
  Result := FPause;
end;

function TMiniSpec.Pause(const Value: Boolean): TMiniSpec;
begin
  Result := Self;
  FPause := Value;
end;

function TMiniSpec.Tags: string;
begin
  Result := FTags;
end;

function TMiniSpec.DryRun: Boolean;
begin
  Result := FDryRun;
end;

function TMiniSpec.DryRun(const Value: Boolean): TMiniSpec;
begin
  Result := Self;
  FDryRun := Value;
end;
{$ENDREGION}

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
      FDryRun := True
    else if Param = '--pause' then
      FPause := True;
  end;
end;

procedure TMiniSpec.Run;
var
  TagFilter: TTagExpression;
  Matcher: TTagMatcher;
  ReportOpts: TReportOptions;
begin
  ParseArgs;

  // Modos de consulta sin ejecutar tests
  case FRunMode of
    TRunMode.rmHelp:
    begin
      ShowHelp;
      Exit;
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

  // Modo normal: ejecutar tests
  TagFilter := TTagExpression.Parse(FTags);
  ReportOpts := TReportOptions.Create;
  try
    // Configurar opciones de reporte
    if FDryRun then
      ReportOpts.SetOption(TReportOptions.OPT_DRY_RUN, TValue.From<Boolean>(True));

    if TagFilter.IsEmpty then
      Matcher := nil
    else
      Matcher := function(const Tags: TSpecTags): Boolean
                 begin
                   Result := TagFilter.Matches(Tags);
                 end;
    ReportOpts.TagMatcher := Matcher;

    // Ejecutar TODAS las Features - el conteo debe reflejar el total definido
    // Cada Feature/Rule/Scenario decide si se ejecuta o marca como Skip
    for var F in FFeatures do
      F.Run(Matcher);

    Reporter.Report(FFeatures, ReportOpts);
  finally
    ReportOpts.Free;
    TagFilter.Free;
  end;
  if FReporter.SkipCount > 0 then
    WriteLn(Format('Pass: %d | Fail: %d | Skip: %d | Completed: %s', [FReporter.PassCount, FReporter.FailCount, FReporter.SkipCount, FormatDateTime('yyyy-mm-dd hh:nn:ss', FReporter.CompletedAt)]))
  else
    WriteLn(Format('Pass: %d | Fail: %d | Completed: %s', [FReporter.PassCount, FReporter.FailCount, FormatDateTime('yyyy-mm-dd hh:nn:ss', FReporter.CompletedAt)]));

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

    for var F in FFeatures do
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
  TagFilter: TTagExpression;
  MatchCount: Integer;
begin
  if FQueryExpr.IsEmpty then
  begin
    WriteLn('Error: --query requires a tag expression');
    WriteLn('Usage: --query "@tag1 and @tag2"');
    Exit;
  end;

  TagFilter := TTagExpression.Parse(FQueryExpr);
  try
    MatchCount := 0;
    WriteLn(Format('Query: %s', [FQueryExpr]));
    WriteLn('');

    for var F in FFeatures do
    begin
      var FeatureShown := False;

      for var S in F.Scenarios do
      begin
        // Combinar tags de Feature y Scenario
        var CombinedTags := F.Tags;
        CombinedTags.Merge(S.Tags);

        if TagFilter.Matches(CombinedTags) then
        begin
          if not FeatureShown then
          begin
            WriteLn('Feature: ' + F.Title);
            FeatureShown := True;
          end;
          WriteLn('  - ' + S.Description);
          Inc(MatchCount);
        end;
      end;
    end;

    WriteLn('');
    WriteLn(Format('Matching scenarios: %d', [MatchCount]));
  finally
    TagFilter.Free;
  end;
end;

procedure TMiniSpec.ShowHelp;
begin
  WriteLn('MiniSpec v' + Version + ' - BDD Testing Framework');
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
  WriteLn('');
  WriteLn('Reporters:');
  WriteLn('  console                 Console output (default)');
  WriteLn('  html:output=<file>      HTML dashboard report');
  WriteLn('  json:output=<file>      JSON report');
  WriteLn('  gherkin:output=<dir>    Export .feature files');
  WriteLn('  gherkin-results:output=<dir>  Export .feature with results');
  WriteLn('  live:port=<port>        Live dashboard on localhost (default: 8080)');
  WriteLn('');
  WriteLn('Tag expressions:');
  WriteLn('  @tag                    Scenarios with tag');
  WriteLn('  ~@tag                   Scenarios without tag');
  WriteLn('  @a and @b               Scenarios with both tags');
  WriteLn('  @a or @b                Scenarios with either tag');
  WriteLn('  (@a or @b) and ~@c      Complex expressions with parentheses');
  WriteLn('');
  WriteLn('Examples:');
  WriteLn('  ' + ExtractFileName(ParamStr(0)) + ' -f "@unit"');
  WriteLn('  ' + ExtractFileName(ParamStr(0)) + ' -f "@integration and ~@slow"');
  WriteLn('  ' + ExtractFileName(ParamStr(0)) + ' -r html:output=report.html');
  WriteLn('  ' + ExtractFileName(ParamStr(0)) + ' -r live:port=9000');
  WriteLn('  ' + ExtractFileName(ParamStr(0)) + ' -t');
  WriteLn('  ' + ExtractFileName(ParamStr(0)) + ' -q "@usesDB"');
end;

initialization
 TRttiContext.KeepContext;
finalization
 TRttiContext.DropContext;
end.
