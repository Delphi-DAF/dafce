unit Daf.MiniSpec.Reporter.Json;

interface

uses
  System.JSON,
  Daf.MiniSpec.Types,
  Daf.MiniSpec.Runner;

type
  /// <summary>
  /// JSON listener - builds JSON output from test results.
  /// Implements ISpecListener for pure observer pattern.
  /// </summary>
  TJsonReporter = class(TCustomListener)
  private
    FOutput: string;
    FFeatures: TJSONArray;
    FCurrentFeature: TJSONObject;
    FCurrentScenarios: TJSONArray;
    FCurrentScenario: TJSONObject;
    FCurrentSteps: TJSONArray;
    procedure AddStep(const Kind, Description: string; Success: Boolean; Duration: Integer; const ErrorMessage: string = '');
    procedure AddStepWithTable(const Step: IScenarioStep);
    procedure CloseCurrentScenario;
    procedure CloseCurrentFeature;
    function GetStatus(const Item: ISpecItem): string;
  public
    function GetContent: string;override;
    function GetFileExt: string;override;
    procedure OnBeginReport(const Context: IRunContext);override;
    procedure OnEndReport(const Context: IRunContext);override;
    procedure OnItem(const Context: IRunContext; const Item: ISpecItem);override;
    procedure OnEndOutline(const Context: IRunContext; const Outline: IScenarioOutline);override;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.Rtti,
  Daf.MiniSpec.DataTable;

{ TJsonReporter }

function TJsonReporter.GetStatus(const Item: ISpecItem): string;
begin
  if Item.RunInfo.State <> srsFinished then
    Result := 'skip'
  else if Item.RunInfo.Result = srrSuccess then
    Result := 'pass'
  else
    Result := 'fail';
end;

procedure TJsonReporter.CloseCurrentScenario;
begin
  if Assigned(FCurrentScenario) then
  begin
    FCurrentScenario.AddPair('steps', FCurrentSteps);
    FCurrentScenarios.AddElement(FCurrentScenario);
    FCurrentScenario := nil;
    FCurrentSteps := nil;
  end;
end;

procedure TJsonReporter.CloseCurrentFeature;
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

function DataTableToJsonArray(const Table: TDataTableObj): TJSONArray;
var
  Row: TArray<TValue>;
  RowArr: TJSONArray;
  V: TValue;
begin
  Result := TJSONArray.Create;
  if Table = nil then Exit;
  // Add headers
  RowArr := TJSONArray.Create;
  for var H in Table.Headers do
    RowArr.Add(H);
  Result.AddElement(RowArr);
  // Add data rows
  for Row in Table.Rows do
  begin
    RowArr := TJSONArray.Create;
    for V in Row do
      RowArr.Add(V.ToString);
    Result.AddElement(RowArr);
  end;
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

procedure TJsonReporter.AddStepWithTable(const Step: IScenarioStep);
var
  StepObj: TJSONObject;
begin
  if not Assigned(FCurrentSteps) then Exit;
  StepObj := TJSONObject.Create;
  StepObj.AddPair('kind', Step.KeyWord);
  StepObj.AddPair('description', Step.Description);
  StepObj.AddPair('success', TJSONBool.Create(Step.RunInfo.IsSuccess));
  StepObj.AddPair('duration', TJSONNumber.Create(Step.RunInfo.ExecTimeMs));
  if not Step.RunInfo.IsSuccess and (Step.RunInfo.ErrMsg <> '') then
    StepObj.AddPair('error', Step.RunInfo.ErrMsg);
  if Assigned(Step.DataTable) then
    StepObj.AddPair('dataTable', DataTableToJsonArray(Step.DataTable));
  FCurrentSteps.AddElement(StepObj);
end;

procedure TJsonReporter.OnBeginReport(const Context: IRunContext);
begin
  FFeatures := TJSONArray.Create;
  FCurrentFeature := nil;
  FCurrentScenarios := nil;
  FCurrentScenario := nil;
  FCurrentSteps := nil;
  FOutput := '';
end;

procedure TJsonReporter.OnEndReport(const Context: IRunContext);
var
  OutputFile: string;
  Root, SuiteObj: TJSONObject;
begin
  CloseCurrentFeature;

  Root := TJSONObject.Create;
  try
    // Add suite info if available
    if Assigned(Context.Suite) and not Context.Suite.Title.IsEmpty then
    begin
      SuiteObj := TJSONObject.Create;
      SuiteObj.AddPair('title', Context.Suite.Title);
      SuiteObj.AddPair('features', FFeatures);
      FFeatures := nil;
      Root.AddPair('suite', SuiteObj);
    end
    else
    begin
      Root.AddPair('features', FFeatures);
      FFeatures := nil; // Root now owns FFeatures
    end;
    // Use Suite.RunInfo for totals
    Root.AddPair('passCount', TJSONNumber.Create(Context.Suite.RunInfo.PassCount));
    Root.AddPair('failCount', TJSONNumber.Create(Context.Suite.RunInfo.FailCount));
    Root.AddPair('skipCount', TJSONNumber.Create(Context.Suite.RunInfo.SkipCount));
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

procedure TJsonReporter.OnItem(const Context: IRunContext; const Item: ISpecItem);
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
      FCurrentScenario.AddPair('kind', Item.KeyWord);
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
      FCurrentScenario.AddPair('kind', Item.KeyWord);
      FCurrentScenario.AddPair('description', Item.Description);
      FCurrentScenario.AddPair('status', GetStatus(Item));
      FCurrentScenario.AddPair('duration', TJSONNumber.Create(Item.RunInfo.ExecTimeMs));
      if (GetStatus(Item) = 'fail') and (Item.RunInfo.ErrMsg <> '') then
        FCurrentScenario.AddPair('error', Item.RunInfo.ErrMsg);
      FCurrentSteps := TJSONArray.Create;
    end;
    sikExampleInit: ;
    sikGiven, sikWhen, sikThen: begin
      var Step: IScenarioStep;
      if Supports(Item, IScenarioStep, Step) then
        AddStepWithTable(Step)
      else
        AddStep(Item.KeyWord, Item.Description, Item.RunInfo.Result = srrSuccess,
                Item.RunInfo.ExecTimeMs, Item.RunInfo.ErrMsg);
    end;
  end;
end;

procedure TJsonReporter.OnEndOutline(const Context: IRunContext; const Outline: IScenarioOutline);
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

function TJsonReporter.GetContent: string;
begin
  Result := FOutput;
end;

function TJsonReporter.GetFileExt: string;
begin
  Result := 'json';
end;

end.
