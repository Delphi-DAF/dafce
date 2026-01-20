unit Daf.MiniSpec.Types;

interface
uses
  System.Generics.Collections,
  System.Rtti,
  System.SysUtils;

type
  TStepProc<T> = reference to procedure(World: T);
  TExamplesTable = TArray<TArray<TValue>>;
  TSpecItemKind = (sikFeature, sikBackground, sikScenario, sikExample, sikExampleInit, sikGiven, sikWhen, sikThen, sikAnd, sikBut);
  TLastStepKind = (lskNone, lskGiven, lskWhen, lskThen);
  TSpecRunState =  (srsPrepared, srsSkiped, srsRunning, srsFinished);
  TSpecRunResult =  (srrNone, srrSuccess, srrFail, srrError);
  TSpecTags = record
  strict private
    FTags: TArray<string>;
  public
    procedure AddFrom(const Text: string);
    procedure Merge(const Other: TSpecTags);
    function Contains(const Tag: string): Boolean;overload;
    function ContainsAll(const Tags: TArray<string>): Boolean;overload;
    function ContainsAny(const Tags: TArray<string>): Boolean;
    function ToArray: TArray<string>;
  end;

  TTagMatcher = reference to function(const Tags: TSpecTags): Boolean;

  TSpecRunInfo = record
  public
    State: TSpecRunState;
    Result: TSpecRunResult;
    ExecTimeMs: Int64;
    Error: Exception;
    procedure Clear;
    function ErrMsg: string;
    function IsSuccess: Boolean;
  end;

  IScenarioBuilder<T: class, constructor> = interface;
  IScenarioOutlineBuilder<T: class, constructor> = interface;
  IBackgroundBuilder<T: class, constructor> = interface
    function Given(const Desc: string; Step: TStepProc<T>): IBackgroundBuilder<T>;
    function &And(const Desc: string; Step: TStepProc<T>): IBackgroundBuilder<T>;
    function But(const Desc: string; Step: TStepProc<T>): IBackgroundBuilder<T>;
    function Scenario(const Description: string): IScenarioBuilder<T>;overload;
    function ScenarioOutline(const Description: string): IScenarioOutlineBuilder<T>;
  end;

  IFeatureBuilder<T: class, constructor> = interface
    function Background: IBackgroundBuilder<T>;
    function Scenario(const Description: string): IScenarioBuilder<T>;overload;
    function ScenarioOutline(const Description: string): IScenarioOutlineBuilder<T>;
  end;

  IScenarioBuilder<T: class, constructor> = interface
    function Given(const Desc: string; Step: TStepProc<T>): IScenarioBuilder<T>;
    function When(const Desc: string; Step: TStepProc<T>) : IScenarioBuilder<T>;
    function &Then(const Desc: string; Step: TStepProc<T>) : IScenarioBuilder<T>;
    function &And(const Desc: string; Step: TStepProc<T>): IScenarioBuilder<T>;
    function But(const Desc: string; Step: TStepProc<T>): IScenarioBuilder<T>;

    function Scenario(const Description: string): IScenarioBuilder<T>;overload;
    function ScenarioOutline(const Description: string): IScenarioOutlineBuilder<T>;
  end;

  IScenarioOutlineBuilder<T: class, constructor> = interface
    function Given(const Desc: string; Step: TStepProc<T> = nil) : IScenarioOutlineBuilder<T>;
    function When(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>;
    function &Then(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>;
    function &And(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>;
    function But(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>;
    function Examples(const Table: TExamplesTable): IFeatureBuilder<T>;
  end;

  ISpecItem = interface
    ['{122463BA-E861-4B68-8B4B-D6E76A8B3CA0}']
    function GetTags: TSpecTags;
    function GetDescription: string;
    function GetKind: TSpecItemKind;
    function GetRunInfo: TSpecRunInfo;
    function GetParent: ISpecItem;

    procedure Run(World: TObject);
    property Kind: TSpecItemKind read GetKind;
    property Parent: ISpecItem read GetParent;
    property Description: string read GetDescription;
    property Tags: TSpecTags read GetTags;
    property RunInfo: TSpecRunInfo read GetRunInfo;
  end;

  IScenarioStep = interface(ISpecItem)
    ['{3F16A9FA-0F64-4B4C-985F-D09087DD7404}']
  end;

  IBackground = interface(ISpecItem)
    function GetStepsGiven: TList<IScenarioStep>;
    property StepsGiven: TList<IScenarioStep> read GetStepsGiven;
  end;

  IScenario = interface(IBackground)
    ['{45DA94A6-DE59-4EB7-B528-6E5157DC9169}']
    function GetStepsWhen: TList<IScenarioStep>;
    function GetStepsThen: TList<IScenarioStep>;
    property StepsWhen: TList<IScenarioStep> read GetStepsWhen;
    property StepsThen: TList<IScenarioStep> read GetStepsThen;
  end;

  IFeature = interface(ISpecItem)
    ['{025FBE2B-E0B2-47D2-B50A-65A381F119AC}']
    function GetBackGround: IBackground;
    procedure SetBackGround(const Value: IBackground);
    function GetScenarios: TList<IScenario>;
    procedure Run(const TagMatcher: TTagMatcher = nil);
    property BackGround: IBackground read GetBackground write SetBackground;
    property Scenarios: TList<IScenario> read GetScenarios;
  end;

  TSpecItem = class(TInterfacedObject, ISpecItem)
  strict private
    FTags: TSpecTags;
    FDescription: string;
    [weak]
    FParent: ISpecItem;
    function PlaceHolder(Template, PHName, PHValue: string): string;
    procedure ExpandPlaceholders(const World: TObject);
  protected
    FKind: TSpecItemKind;
    FRunInfo: TSpecRunInfo;
    function GetKind: TSpecItemKind;
    function GetParent: ISpecItem;
    function GetDescription: string;
    function GetTags: TSpecTags;
    function GetRunInfo: TSpecRunInfo;
  public
    constructor Create(const Kind: TSpecItemKind; const Parent: ISpecItem; const Description: string);
    destructor Destroy;override;
    procedure Run(World: TObject);virtual;
    property Parent: ISpecItem read FParent;
    property Description: string read GetDescription;
    property Tags: TSpecTags read GetTags;
  end;

  TScenarioStep<T: class> = class(TSpecItem, IScenarioStep)
  strict private
    FProc: TStepProc<T>;
  protected
  public
    constructor Create(const Kind: TSpecItemKind; const Parent: ISpecItem; const Description: string; const Proc: TStepProc<T>);
    destructor Destroy;override;
    procedure Run(World: TObject);override;
    property Proc: TStepProc<T> read FProc;
  end;

  TBackground<T: class, constructor> = class(TSpecItem, IBackground)
  strict private
    FStepsGiven: TList<IScenarioStep>;
    function GetStepsGiven: TList<IScenarioStep>;
  private
    function GetFeature: IFeature;
    procedure RunSteps(Steps: TList<IScenarioStep>; World: TObject);
  public
    constructor Create(const Feature: IFeature);
    destructor Destroy;override;
    function Given(const Desc: string; Step: TStepProc<T>): TBackground<T>;
    procedure Run(World: TObject);override;
    property Feature: IFeature read GetFeature;
  end;

  TScenario<T: class, constructor> = class(TSpecItem, IScenario)
  strict private
    FInitExample: IScenarioStep;
    FStepsGiven: TList<IScenarioStep>;
    FStepsWhen: TList<IScenarioStep>;
    FStepsThen: TList<IScenarioStep>;
    function GetStepsGiven: TList<IScenarioStep>;
    function GetStepsWhen: TList<IScenarioStep>;
    function GetStepsThen: TList<IScenarioStep>;
  private
    function GetFeature: IFeature;
    procedure RunSteps(Steps: TList<IScenarioStep>; World: TObject);
  public
    constructor Create(const Feature: IFeature; const Description: string);
    destructor Destroy;override;
    function ExampleInit(Step: TStepProc<T>): TScenario<T>;
    function Given(const Desc: string; Step: TStepProc<T>): TScenario<T>;
    function When(const Desc: string; Step: TStepProc<T>) : TScenario<T>;
    function &Then(const Desc: string; Step: TStepProc<T>) : TScenario<T>;
    procedure Run(World: TObject);override;
    property Feature: IFeature read GetFeature;
  end;

  TFeature<T: class,constructor> = class(TSpecItem, IFeature)
  strict private
    FBackground: IBackground;
    FScenarios: TList<IScenario>;
    function GetBackGround: IBackground;
    procedure SetBackGround(const Value: IBackground);
  private
    procedure RunBeforeHooks;
    procedure RunAfterHooks;
    procedure RunBackground(World: T);
    function GetScenarios: TList<IScenario>;
    function CreateWorld: T;
  public
    constructor Create(const Description: string);
    destructor Destroy; override;
    procedure Run(const TagMatcher: TTagMatcher = nil);reintroduce;
    property Background: IBackground read GetBackGround write SetBackGround;
  end;

implementation
uses
  System.RegularExpressions,
  System.Diagnostics,
  Daf.MiniSpec.Expects,
  Daf.MiniSpec;

function Val2Str(const V: TValue): string;
begin
  if V.IsArray then
  begin
    var StrAry: TArray<string>;
    SetLength(StrAry, V.GetArrayLength);
    for var idx := 0 to V.GetArrayLength - 1 do
    begin
      StrAry[idx] := Val2Str(V.GetArrayElement(idx));
    end;
    Result := '[' + string.Join(', ', StrAry) + ']';
  end
  else
    Result := V.ToString;
end;

{ TSpecTags }

procedure TSpecTags.AddFrom(const Text: string);
begin
  var RegEx := TRegEx.Create('@([a-zA-Z0-9_\-:]+)');
  var Matches := RegEx.Matches(Text);
  for var Match in Matches do
  begin
    var NewTag := Match.Groups[1].Value;
    if not Contains(NewTag) then
      FTags := FTags + [NewTag];
  end;
end;

procedure TSpecTags.Merge(const Other: TSpecTags);
begin
  for var Tag in Other.FTags do
    if not Contains(Tag) then
      FTags := FTags + [Tag];
end;

function TSpecTags.Contains(const Tag: string): Boolean;
begin
  for var MyTag in FTags do
    if SameText(MyTag, Tag) then
      Exit(True);
  Result := False;
end;

function TSpecTags.ContainsAll(const Tags: TArray<string>): Boolean;
begin
  for var Tag in Tags do
    if not Contains(Tag) then
      Exit(False);
  Result := True;
end;

function TSpecTags.ContainsAny(const Tags: TArray<string>): Boolean;
begin
  for var Tag in Tags do
    if Contains(Tag) then
      Exit(True);
  Result := False;
end;

function TSpecTags.ToArray: TArray<string>;
begin
  Result := FTags;
end;

{ TSpecRunInfo }

procedure TSpecRunInfo.Clear;
begin
  State := TSpecRunState.srsPrepared;
  Result := TSpecRunResult.srrNone;
  ExecTimeMs := 0;
  Error := nil;
end;

function TSpecRunInfo.ErrMsg: string;
begin
  if Assigned(Error) then
    Result := Error.Message
  else
    Result := '';
end;

function TSpecRunInfo.IsSuccess: Boolean;
begin
  Result := Self.Result = srrSuccess;
end;

{ TSpecItem }

constructor TSpecItem.Create(const Kind: TSpecItemKind; const Parent: ISpecItem; const Description: string);
begin
  inherited Create;
  FKind := Kind;
  FParent := Parent;
  FDescription := Description;
  FTags.AddFrom(Description);
end;

function TSpecItem.GetKind: TSpecItemKind;
begin
  Result := FKind;
end;

function TSpecItem.PlaceHolder(Template, PHName, PHValue: string): string;
begin
  Result := StringReplace(Template, '<' + PHName + '>', '#{' + PHValue + '}', [rfReplaceAll]);
end;

destructor TSpecItem.Destroy;
begin
 if Assigned(FRunInfo.Error) then;
   FRunInfo.Error.Free;
  inherited;
end;

procedure TSpecItem.ExpandPlaceholders(const World: TObject);
begin
  var RttiCtx: TRttiContext;
  var Regex := TRegEx.Create('<(\w+)>');
  var RTTType := RttiCtx.GetType(World.ClassInfo) as TRttiInstanceType;

  for var Match in Regex.Matches(FDescription) do
  begin
    var PHName := Match.Groups[1].Value;
    var Field := RTTType.GetField(PHName);
    if not Assigned(Field) then
      raise Exception.CreateFmt('%s: %s not found in World', [FDescription, PHName]);
    var ValueStr := Val2Str(Field.GetValue(World));
    FDescription := PlaceHolder(FDescription, PHName, ValueStr);
  end;
end;

procedure TSpecItem.Run(World: TObject);
begin
  ExpandPlaceholders(World);
  FRunInfo.Clear;
end;

function TSpecItem.GetDescription: string;
begin
  Result := FDescription;
end;

function TSpecItem.GetParent: ISpecItem;
begin
  Result := FParent;
end;

function TSpecItem.GetRunInfo: TSpecRunInfo;
begin
  Result := FRunInfo;
end;

function TSpecItem.GetTags: TSpecTags;
begin
  Result := FTags;
end;

{ TScenarioStep<T> }

constructor TScenarioStep<T>.Create(const Kind: TSpecItemKind; const Parent: ISpecItem; const Description: string; const Proc: TStepProc<T>);
begin
  inherited Create(Kind, Parent, Description);
  FProc := Proc;
end;

destructor TScenarioStep<T>.Destroy;
begin
  FProc := nil;
  inherited;
end;

procedure TScenarioStep<T>.Run(World: TObject);
begin
  var SW := TStopwatch.StartNew;
  try
    inherited Run(World);
    FRunInfo.State := srsRunning;
    FRunInfo.Result := srrSuccess;
    if Assigned(FProc) then
      FProc(World as T);
  except
    on E: ExpectFail do
    begin
      FRunInfo.Result := srrFail;
      FRunInfo.Error := Exception(AcquireExceptionObject);
    end;
    on E: Exception do
    begin
      FRunInfo.Result := srrError;
      FRunInfo.Error := Exception(AcquireExceptionObject);
    end;
  end;
  FRunInfo.State := srsFinished;
  FRunInfo.ExecTimeMs := SW.ElapsedMilliseconds;
end;

{ TBackground<T> }

constructor TBackground<T>.Create(const Feature: IFeature);
begin
  inherited Create(sikBackground, Feature, Description);
  FStepsGiven := TList<IScenarioStep>.Create;
  Feature.Background := Self;
end;

destructor TBackground<T>.Destroy;
begin
  FStepsGiven.Free;
  inherited;
end;

function TBackground<T>.GetFeature: IFeature;
begin
  Result := (Parent as IFeature);
end;

function TBackground<T>.GetStepsGiven: TList<IScenarioStep>;
begin
  Result := FStepsGiven;
end;

function TBackground<T>.Given(const Desc: string; Step: TStepProc<T>): TBackground<T>;
begin
  Result := Self;
  FStepsGiven.Add(TScenarioStep<T>.Create(sikGiven, Self, Desc, Step));
end;

procedure TBackground<T>.Run(World: TObject);
begin
  var SW := TStopwatch.StartNew;
  try
    inherited;
    FRunInfo.State := srsRunning;
    FRunInfo.Result := srrSuccess;
    if FRunInfo.Result = srrSuccess then RunSteps(GetStepsGiven, World);
  except
    on E: Exception do
    begin
      FRunInfo.Result := srrError;
      FRunInfo.Error := E;
    end;
  end;
  FRunInfo.State := srsFinished;
  FRunInfo.ExecTimeMs := SW.ElapsedMilliseconds;
end;

procedure TBackground<T>.RunSteps(Steps: TList<IScenarioStep>; World: TObject);
begin
  for var Step in Steps do
  begin
    Step.Run(World);
    if Step.RunInfo.Result in [srrFail, srrError] then
      FRunInfo.Result := srrFail;
  end;
end;

{ TScenario<T> }

constructor TScenario<T>.Create(const Feature: IFeature; const Description: string);
begin
  inherited Create(sikScenario, Feature, Description);
  FStepsGiven := TList<IScenarioStep>.Create;
  FStepsWhen := TList<IScenarioStep>.Create;
  FStepsThen := TList<IScenarioStep>.Create;

  Feature.Scenarios.Add(Self);
end;

destructor TScenario<T>.Destroy;
begin
  FStepsThen.Free;
  FStepsWhen.Free;
  FStepsGiven.Free;
  inherited;
end;

function TScenario<T>.ExampleInit(Step: TStepProc<T>): TScenario<T>;
begin
  Result := Self;
  Self.FKind := sikExample;
  FInitExample := TScenarioStep<T>.Create(sikExampleInit, Self, '', Step);
end;

function TScenario<T>.GetFeature: IFeature;
begin
  Result := (Parent as IFeature);
end;

function TScenario<T>.GetStepsGiven: TList<IScenarioStep>;
begin
  Result := FStepsGiven
end;

function TScenario<T>.GetStepsThen: TList<IScenarioStep>;
begin
  Result := FStepsThen
end;

function TScenario<T>.GetStepsWhen: TList<IScenarioStep>;
begin
  Result := FStepsWhen
end;

function TScenario<T>.Given(const Desc: string; Step: TStepProc<T>): TScenario<T>;
begin
  Result := Self;
  FStepsGiven.Add(TScenarioStep<T>.Create(sikGiven, Self, Desc, Step));
end;

procedure TScenario<T>.RunSteps(Steps: TList<IScenarioStep>; World: TObject);
begin
  for var Step in Steps do
  begin
    Step.Run(World);
    if Step.RunInfo.Result in [srrFail, srrError] then
      FRunInfo.Result := srrFail;
  end;
end;

procedure TScenario<T>.Run(World: TObject);
begin
  var SW := TStopwatch.StartNew;
  try
    if Assigned(FInitExample) then
      FInitExample.Run(World);
    inherited;
    FRunInfo.State := srsRunning;
    FRunInfo.Result := srrSuccess;
    if FRunInfo.Result = srrSuccess then RunSteps(GetStepsGiven, World);
    if FRunInfo.Result = srrSuccess then RunSteps(GetStepsWhen, World);
    if FRunInfo.Result = srrSuccess then RunSteps(GetStepsThen, World);
  except
    on E: Exception do
    begin
      FRunInfo.Result := srrError;
      FRunInfo.Error := E;
    end;
  end;
  FRunInfo.State := srsFinished;
  FRunInfo.ExecTimeMs := SW.ElapsedMilliseconds;
end;

function TScenario<T>.When(const Desc: string; Step: TStepProc<T>): TScenario<T>;
begin
  Result := Self;
  FStepsWhen.Add(TScenarioStep<T>.Create(sikWhen, Self, Desc, Step));
end;

function TScenario<T>.&Then(const Desc: string; Step: TStepProc<T>): TScenario<T>;
begin
  Result := Self;
  FStepsThen.Add(TScenarioStep<T>.Create(sikThen, Self, Desc, Step));
end;

{ TFeature<T>}

constructor TFeature<T>.Create(const Description: string);
begin
  inherited Create(sikFeature, nil, Description);
  FScenarios := TList<IScenario>.Create;
  MiniSpec.Register(Self as IFeature);
end;

destructor TFeature<T>.Destroy;
begin
  FScenarios.Free;
  inherited;
end;

procedure TFeature<T>.RunBackground(World: T);
begin
   if Assigned(FBackground) then
    FBackground.Run(World);
end;

function TFeature<T>.GetBackGround: IBackground;
begin
  Result := FBackground;
end;

function TFeature<T>.GetScenarios: TList<IScenario>;
begin
  Result := FScenarios;
end;

procedure TFeature<T>.Run(const TagMatcher: TTagMatcher);
var
  CombinedTags: TSpecTags;
begin
  var SW := TStopwatch.StartNew;
  FRunInfo.State := srsRunning;
  FRunInfo.Result := srrSuccess;
  try
    for var Scenario in FScenarios do
    begin
      // Saltar escenarios que no coinciden con el filtro de tags
      // Los tags del Feature se heredan a los escenarios
      if Assigned(TagMatcher) then
      begin
        CombinedTags := Self.Tags;
        CombinedTags.Merge(Scenario.Tags);
        if not TagMatcher(CombinedTags) then
          Continue;
      end;
      var World := CreateWorld;
      RunBeforeHooks;
      RunBackground(World);
      Scenario.Run(World);
      if Scenario.RunInfo.Result in [srrFail, srrError] then
        FRunInfo.Result := srrFail;
      RunAfterHooks;
      World.Free;
    end;
  except
    on E: Exception do
    begin
      FRunInfo.Result := srrError;
      FRunInfo.Error := E;
    end;
  end;
  FRunInfo.State := srsFinished;
  FRunInfo.ExecTimeMs := SW.ElapsedMilliseconds;
end;

function TFeature<T>.CreateWorld: T;
begin
  Result := T.Create;
end;

procedure TFeature<T>.RunBeforeHooks;
begin
end;

procedure TFeature<T>.SetBackGround(const Value: IBackground);
begin
  FBackGround := Value;
end;

procedure TFeature<T>.RunAfterHooks;
begin
end;

end.
