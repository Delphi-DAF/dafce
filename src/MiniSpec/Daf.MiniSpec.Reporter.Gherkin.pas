unit Daf.MiniSpec.Reporter.Gherkin;

interface

uses
  System.Classes,
  Daf.MiniSpec.Types,
  Daf.MiniSpec.Reporter;

type
  /// <summary>
  /// Gherkin reporter - generates .feature files from test results.
  /// Implements ISpecListener for pure observer pattern.
  /// </summary>
  TGherkinReporter = class(TCustomListener)
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

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.RegularExpressions,
  System.Rtti;

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

procedure TGherkinReporter.Configure(const Options: TReporterOptions);
begin
  inherited;
  // WithResults option
  if Assigned(Options) and Options.ContainsKey('results') then
    FWithResults := SameText(Options['results'], 'true');
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

function TGherkinReporter.SanitizeFileName(const Name: string): string;
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

procedure TGherkinReporter.OnBeginReport(const Context: IRunContext);
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

procedure TGherkinReporter.OnEndReport(const Context: IRunContext);
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

procedure TGherkinReporter.OnItem(const Context: IRunContext; const Item: ISpecItem);
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

procedure TGherkinReporter.OnEndOutline(const Context: IRunContext; const Outline: IScenarioOutline; const Counters: TSpecCounters);
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

function TGherkinReporter.GetContent: string;
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

function TGherkinReporter.GetFileExt: string;
begin
  Result := 'md';
end;

end.
