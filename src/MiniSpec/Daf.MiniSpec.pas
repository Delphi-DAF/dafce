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
  TMiniSpec = class
  public
    const Version = '1.4.0';
  strict private
    FFeatures: TList<IFeature>;
    FReporter: ISpecReporter;
    FOutputFile: string;
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
    procedure ParseArgs;
    function DefaultOutputFile: string;
    procedure ListTags;
    procedure QueryTags;
    procedure ShowHelp;
  public
    constructor Create;
    destructor Destroy; override;
    class destructor ClasDestroy;
    {$REGION 'Fluent api for setup'}
    function Reporter(const Name: string): TMiniSpec;overload;
    function Reporter: ISpecReporter;overload;
    function Tags: string;overload;
    procedure Tags(const Value: string);overload;
    function OutputFile: string;overload;
    function OutputFile(const Filename: string): TMiniSpec;overload;
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

function TMiniSpec.OutputFile: string;
begin
  Result := FOutputFile;
end;

class destructor TMiniSpec.ClasDestroy;
begin
  FInstance.Free;
end;

procedure TMiniSpec.Register(Feature: IFeature);
begin
  FFeatures.Add(Feature);
end;

function TMiniSpec.DefaultOutputFile: string;
begin
  Result := TPath.ChangeExtension(ParamStr(0), 'MiniSpec.' + Reporter.GetFileExt);
end;

{$REGION 'Fluent api for setup'}
function TMiniSpec.Reporter(const Name: string): TMiniSpec;
begin
  Result := Self;
  if SameText(Name, 'console') then
    FReporter := TConsoleReporter.Create
  else
  if SameText(Name, 'html') then
    FReporter := THTMLReporter.Create
  else
  if SameText(Name, 'json') then
    FReporter := TJsonReporter.Create
  else
  if SameText(Name, 'gherkin') then
    FReporter := TGherkinReporter.Create(False)
  else
  if SameText(Name, 'gherkin-results') then
    FReporter := TGherkinReporter.Create(True)
  else
  if SameText(Name, 'live') then
    FReporter := TLiveReporter.Create(TConsoleReporter.Create)
  else
    raise Exception.CreateFmt('Unknow report name: %s', [Name]);
end;

function TMiniSpec.Reporter: ISpecReporter;
begin
  Result := FReporter;
end;

function TMiniSpec.OutputFile(const Filename: string): TMiniSpec;
begin
  Result := Self;
  if TPath.IsRelativePath(Filename) then
    FOutputfile := ExpandFileName(TPath.Combine(ExtractFilePath(ParamStr(0)), Filename))
  else
    FOutputfile := FileName;
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
    if (Param = '--output') or (Param = '-o') then
      OutputFile(NextArg)
    else if (Param = '--filter') or (Param = '-f') then
      Tags(NextArg)
    else if (Param = '--reporter') or (Param = '-r') then
      Reporter(LowerCase(NextArg))
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
  if OutputFile.IsEmpty then
    OutputFile(DefaultOutputFile);

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

  if FReporter.UseConsole then
  begin
    if Pause then
      OSShell.WaitForShutdown;
    Exit;
  end;
  TFile.WriteAllText(FOutputFile, FReporter.GetContent, TEncoding.UTF8);
  var FileURL := 'file:///' + StringReplace(FOutputfile, '\', '/', [rfReplaceAll]);
  WriteLn('report detail: ' + FileURL);
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
  WriteLn('  -r, --reporter <name>   Output format: html, json, live (default: console)');
  WriteLn('  -o, --output <file>     Output file path');
  WriteLn('  -f, --filter <expr>     Run only scenarios matching tag expression');
  WriteLn('  -t, --tags              List all tags with scenario counts (no tests run)');
  WriteLn('  -q, --query <expr>      Show scenarios matching expression (no tests run)');
  WriteLn('  --dry-run               Show what would run without executing tests');
  WriteLn('  --pause                 Wait for keypress before closing console');
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
  WriteLn('  ' + ExtractFileName(ParamStr(0)) + ' -r html -o report.html');
  WriteLn('  ' + ExtractFileName(ParamStr(0)) + ' -t');
  WriteLn('  ' + ExtractFileName(ParamStr(0)) + ' -q "@usesDB"');
end;

initialization
 TRttiContext.KeepContext;
finalization
 TRttiContext.DropContext;
end.
