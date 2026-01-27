unit Daf.MiniSpec.Types;

interface
uses
  System.Classes,
  System.Generics.Collections,
  System.Rtti,
  System.SysUtils;

type
  TStepProc<T> = reference to procedure(World: T);
  TExamplesTable = TArray<TArray<TValue>>;
  TSpecItemKind = (sikFeature, sikImplicitRule, sikRule, sikBackground, sikScenario, sikScenarioOutline, sikExample, sikExampleInit, sikGiven, sikWhen, sikThen, sikAnd, sikBut);
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

  /// <summary>
  /// Metadata para scenarios Example (hijos de ScenarioOutline).
  /// Solo contiene los valores de la fila; los Headers están en el padre.
  /// </summary>
  /// <remarks>
  /// Los valores en Example.Description usan el formato #{valor} intencionalmente
  /// (ej: "Outline: N=#{1}" en vez de "Outline: N=1").
  /// Esto permite a los Listeners/Reporters distinguir valores de Examples
  /// y formatearlos según su necesidad (ej: convertir a &lt;1&gt; para Gherkin).
  /// Los valores reales están disponibles en ExampleMeta.Values.
  /// </remarks>
  TExampleMeta = record
    Values: TArray<TValue>;        // Valores de la fila
    RowIndex: Integer;             // Índice de la fila (1-based)
  end;

  /// <summary>
  /// Captures exception info from When step without keeping the Exception object.
  /// This avoids memory leaks from AcquireExceptionObject.
  /// </summary>
  TCapturedRaise = record
    WasRaised: Boolean;
    ExceptionClass: ExceptClass;
    ExceptionMessage: string;
    class function Empty: TCapturedRaise; static;
    class function From(E: Exception): TCapturedRaise; static;
  end;

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
  IRuleBuilder<T: class, constructor> = interface;

  IBackgroundBuilder<T: class, constructor> = interface
    function Given(const Desc: string; Step: TStepProc<T>): IBackgroundBuilder<T>;
    function &And(const Desc: string; Step: TStepProc<T>): IBackgroundBuilder<T>;
    function But(const Desc: string; Step: TStepProc<T>): IBackgroundBuilder<T>;
    function Scenario(const Description: string): IScenarioBuilder<T>;overload;
    function ScenarioOutline(const Description: string): IScenarioOutlineBuilder<T>;
    function Rule(const Description: string): IRuleBuilder<T>;
  end;

  IFeatureBuilder<T: class, constructor> = interface
    function Background: IBackgroundBuilder<T>;
    function Scenario(const Description: string): IScenarioBuilder<T>;overload;
    function ScenarioOutline(const Description: string): IScenarioOutlineBuilder<T>;
    function Rule(const Description: string): IRuleBuilder<T>;
  end;

  /// <summary>
  /// Builder para construir Rules dentro de una Feature.
  /// Las Rules agrupan escenarios relacionados.
  /// </summary>
  IRuleBuilder<T: class, constructor> = interface
    function Background: IBackgroundBuilder<T>;
    function Scenario(const Description: string): IScenarioBuilder<T>;
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
    function Rule(const Description: string): IRuleBuilder<T>;
  end;

  IScenarioOutlineBuilder<T: class, constructor> = interface
    function Given(const Desc: string; Step: TStepProc<T> = nil) : IScenarioOutlineBuilder<T>;
    function When(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>;
    function &Then(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>;
    function &And(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>;
    function But(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>;
    function Examples(const Table: TExamplesTable): IFeatureBuilder<T>;

    // Continuar dentro de la misma Rule
    function Scenario(const Description: string): IScenarioBuilder<T>;
    function ScenarioOutline(const Description: string): IScenarioOutlineBuilder<T>;
    function Rule(const Description: string): IRuleBuilder<T>;
  end;

  ISpecItem = interface
    ['{122463BA-E861-4B68-8B4B-D6E76A8B3CA0}']
    function GetTags: TSpecTags;
    function GetEffectiveTags: TSpecTags;
    function GetDescription: string;
    function GetKind: TSpecItemKind;
    function GetRunInfo: TSpecRunInfo;
    function GetParent: ISpecItem;

    procedure Run(World: TObject);
    property Kind: TSpecItemKind read GetKind;
    property Parent: ISpecItem read GetParent;
    property Description: string read GetDescription;
    property Tags: TSpecTags read GetTags;
    property EffectiveTags: TSpecTags read GetEffectiveTags;
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
    function GetExampleMeta: TExampleMeta;
    function GetCapturedRaise: TCapturedRaise;
    procedure SetCapturedRaise(const Value: TCapturedRaise);
    procedure ClearCapturedRaise;
    function ConsumeCapturedRaise: TCapturedRaise;
    property StepsWhen: TList<IScenarioStep> read GetStepsWhen;
    property StepsThen: TList<IScenarioStep> read GetStepsThen;
    property ExampleMeta: TExampleMeta read GetExampleMeta;
    property CapturedRaise: TCapturedRaise read GetCapturedRaise write SetCapturedRaise;
  end;

  /// <summary>
  /// ScenarioOutline agrupa Examples generados desde una tabla.
  /// Contiene los headers de la tabla y los steps template.
  /// </summary>
  IScenarioOutline = interface(IScenario)
    ['{7A8E3F21-B4C5-4D89-9E6A-1F2C3D4E5A6B}']
    function GetHeaders: TArray<string>;
    function GetExamples: TList<IScenario>;
    property Headers: TArray<string> read GetHeaders;
    property Examples: TList<IScenario> read GetExamples;
  end;

  IRule = interface;  // forward declaration

  IFeature = interface(ISpecItem)
    ['{025FBE2B-E0B2-47D2-B50A-65A381F119AC}']
    function GetTitle: string;
    function GetNarrative: string;
    function GetBackGround: IBackground;
    procedure SetBackGround(const Value: IBackground);
    function GetScenarios: TList<IScenario>;
    function GetRules: TList<IRule>;
    function HasMatchingScenarios(const TagMatcher: TTagMatcher): Boolean;
    procedure Run(const TagMatcher: TTagMatcher = nil);
    property Title: string read GetTitle;
    property Narrative: string read GetNarrative;
    property BackGround: IBackground read GetBackground write SetBackground;
    property Scenarios: TList<IScenario> read GetScenarios;
    property Rules: TList<IRule> read GetRules;
  end;

  /// <summary>
  /// Rule agrupa escenarios relacionados dentro de una Feature.
  /// En Gherkin: Feature > Rule > Scenario
  /// </summary>
  IRule = interface(ISpecItem)
    ['{7A8B9C0D-1E2F-3A4B-5C6D-7E8F9A0B1C2D}']
    function GetBackGround: IBackground;
    procedure SetBackGround(const Value: IBackground);
    function GetScenarios: TList<IScenario>;
    function GetFeature: IFeature;
    function HasMatchingScenarios(const TagMatcher: TTagMatcher): Boolean;
    procedure Run(const TagMatcher: TTagMatcher = nil);
    property Feature: IFeature read GetFeature;
    property BackGround: IBackground read GetBackground write SetBackground;
    property Scenarios: TList<IScenario> read GetScenarios;
  end;

  /// <summary>
  /// Interfaz para acceder al contexto de ejecución desde el World.
  /// Solo visible si se hace cast explícito: (World as ISpecContext).CurrentStep
  /// </summary>
  ISpecContext = interface
    ['{B8C9D0E1-F2A3-4B5C-6D7E-8F9A0B1C2D3E}']
    function GetCurrentStep: IScenarioStep;
    procedure SetCurrentStep(const Value: IScenarioStep);
    function CurrentScenario: IScenario;
    function CurrentRule: IRule;
    function CurrentFeature: IFeature;
    property CurrentStep: IScenarioStep read GetCurrentStep write SetCurrentStep;
  end;

  /// <summary>
  /// Clase base opcional para Worlds que necesitan acceso al contexto de ejecución.
  /// Hereda de TNoRefCountObject para evitar ARC.
  /// El contexto está oculto y solo es accesible via ISpecContext.
  /// </summary>
  TFeatureWorld = class(System.TNoRefCountObject, ISpecContext)
  strict private
    FCurrentStep: IScenarioStep;
    function GetCurrentStep: IScenarioStep;
    procedure SetCurrentStep(const Value: IScenarioStep);
    function CurrentScenario: IScenario;
    function CurrentRule: IRule;
    function CurrentFeature: IFeature;
  end;

  TSpecItem = class(TInterfacedObject, ISpecItem)
  strict private
    FTags: TSpecTags;
    FDescription: string;
    function PlaceHolder(Template, PHName, PHValue: string): string;
    procedure ExpandPlaceholders(const World: TObject);
  protected
    [weak]
    FParent: ISpecItem;
    FKind: TSpecItemKind;
    FRunInfo: TSpecRunInfo;
    function GetKind: TSpecItemKind;
    function GetParent: ISpecItem;
    function GetDescription: string;
    function GetTags: TSpecTags;
    function GetEffectiveTags: TSpecTags;
    function GetRunInfo: TSpecRunInfo;
  public
    constructor Create(const Kind: TSpecItemKind; const Parent: ISpecItem; const Description: string);
    destructor Destroy;override;
    procedure Run(World: TObject);virtual;
    property Parent: ISpecItem read FParent;
    property Description: string read GetDescription;
    property Tags: TSpecTags read GetTags;
    property EffectiveTags: TSpecTags read GetEffectiveTags;
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
    FExampleMeta: TExampleMeta;
    FStepsGiven: TList<IScenarioStep>;
    FStepsWhen: TList<IScenarioStep>;
    FStepsThen: TList<IScenarioStep>;
    FCapturedRaise: TCapturedRaise;
    function GetStepsGiven: TList<IScenarioStep>;
    function GetStepsWhen: TList<IScenarioStep>;
    function GetStepsThen: TList<IScenarioStep>;
    function GetExampleMeta: TExampleMeta;
    function GetCapturedRaise: TCapturedRaise;
    procedure SetCapturedRaise(const Value: TCapturedRaise);
  private
    function GetFeature: IFeature;
    procedure RunSteps(Steps: TList<IScenarioStep>; World: TObject);
    procedure ClearCapturedRaise;
    function ConsumeCapturedRaise: TCapturedRaise;
  public
    constructor Create(const Feature: IFeature; const Description: string);overload;
    constructor Create(const Feature: IFeature; const Description: string; AddToFeatureScenarios: Boolean);overload;
    constructor Create(const ARule: IRule; const Description: string);overload;
    constructor CreateExample(const Outline: IScenarioOutline; const Description: string);
    destructor Destroy;override;
    function ExampleInit(Step: TStepProc<T>): TScenario<T>;
    procedure SetExampleMeta(const Meta: TExampleMeta);
    function Given(const Desc: string; Step: TStepProc<T>): TScenario<T>;
    function When(const Desc: string; Step: TStepProc<T>) : TScenario<T>;
    function &Then(const Desc: string; Step: TStepProc<T>) : TScenario<T>;
    procedure Run(World: TObject);override;
    property Feature: IFeature read GetFeature;
    property ExampleMeta: TExampleMeta read GetExampleMeta;
  end;

  /// <summary>
  /// ScenarioOutline contiene los steps template y genera Examples.
  /// Los Examples son escenarios hijos con valores específicos de la tabla.
  /// </summary>
  TScenarioOutline<T: class, constructor> = class(TScenario<T>, IScenarioOutline)
  strict private
    FHeaders: TArray<string>;
    FExamples: TList<IScenario>;
    function GetHeaders: TArray<string>;
    function GetExamples: TList<IScenario>;
  public
    constructor Create(const ARule: IRule; const Description: string; const Headers: TArray<string>);
    destructor Destroy; override;
    procedure AddExample(const Example: IScenario);
    procedure Run(World: TObject); override;
    property Headers: TArray<string> read GetHeaders;
    property Examples: TList<IScenario> read GetExamples;
  end;

  TFeature<T: class,constructor> = class(TSpecItem, IFeature)
  strict private
    FRules: TList<IRule>;
    FImplicitRule: IRule;  // Rule implícita para escenarios/background sin Rule explícita
    FTitle: string;
    FNarrative: string;
    function GetTitle: string;
    function GetNarrative: string;
    function GetBackGround: IBackground;
    procedure SetBackGround(const Value: IBackground);
    function GetScenarios: TList<IScenario>;
    function GetRules: TList<IRule>;
    procedure ParseDescription(const Description: string);
  public
    constructor Create(const Description: string);
    destructor Destroy; override;
    function HasMatchingScenarios(const TagMatcher: TTagMatcher): Boolean;
    procedure Run(const TagMatcher: TTagMatcher = nil);reintroduce;
    property Title: string read GetTitle;
    property Narrative: string read GetNarrative;
    property Background: IBackground read GetBackGround write SetBackGround;
    property Rules: TList<IRule> read GetRules;
    property ImplicitRule: IRule read FImplicitRule;  // Solo para builders
  end;

  /// <summary>
  /// Rule agrupa escenarios relacionados dentro de una Feature.
  /// Puede tener su propio Background que se ejecuta después del Background de Feature.
  /// </summary>
  TRule<T: class, constructor> = class(TSpecItem, IRule)
  strict private
    [weak]
    FFeature: IFeature;
    FBackground: IBackground;
    FScenarios: TList<IScenario>;
    function GetBackGround: IBackground;
    procedure SetBackGround(const Value: IBackground);
    function GetFeature: IFeature;
  private
    procedure RunBackground(World: T);
    function GetScenarios: TList<IScenario>;
    function CreateWorld: T;
  public
    constructor Create(const Feature: IFeature; const Description: string);overload;
    constructor Create(const Feature: IFeature; const Description: string; const Kind: TSpecItemKind);overload;
    destructor Destroy; override;
    function HasMatchingScenarios(const TagMatcher: TTagMatcher): Boolean;
    procedure Run(const TagMatcher: TTagMatcher = nil);reintroduce;
    property Feature: IFeature read GetFeature;
    property Background: IBackground read GetBackGround write SetBackGround;
    property Scenarios: TList<IScenario> read GetScenarios;
  end;

/// <summary>
/// Converts a TValue to string, with proper array formatting.
/// </summary>
function Val2Str(const V: TValue): string;

/// <summary>
/// Returns the currently executing scenario. Used by ExpectRaised to access pending exceptions.
/// </summary>
function GetCurrentScenario: IScenario;

/// <summary>
/// Sets the currently executing scenario. Used internally by TScenario.Run.
/// </summary>
procedure SetCurrentScenario(const Value: IScenario);

implementation
uses
  System.RegularExpressions,
  System.Diagnostics,
  Daf.MiniSpec.Expects,
  Daf.MiniSpec;

threadvar
  /// <summary>
  /// The currently executing scenario. Used by Expect functions to access pending exceptions.
  /// </summary>
  CurrentScenario: IScenario;

{ TCapturedRaise }

class function TCapturedRaise.Empty: TCapturedRaise;
begin
  Result.WasRaised := False;
  Result.ExceptionClass := nil;
  Result.ExceptionMessage := '';
end;

class function TCapturedRaise.From(E: Exception): TCapturedRaise;
begin
  Result.WasRaised := Assigned(E);
  if Result.WasRaised then
  begin
    Result.ExceptionClass := ExceptClass(E.ClassType);
    Result.ExceptionMessage := E.Message;
  end
  else
  begin
    Result.ExceptionClass := nil;
    Result.ExceptionMessage := '';
  end;
end;

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

{ TFeatureWorld }

function TFeatureWorld.GetCurrentStep: IScenarioStep;
begin
  Result := FCurrentStep;
end;

procedure TFeatureWorld.SetCurrentStep(const Value: IScenarioStep);
begin
  FCurrentStep := Value;
end;

function TFeatureWorld.CurrentScenario: IScenario;
var
  Parent: ISpecItem;
begin
  Result := nil;
  if not Assigned(FCurrentStep) then Exit;
  Parent := FCurrentStep.Parent;
  while Assigned(Parent) do
  begin
    if Supports(Parent, IScenario, Result) then
      Exit;
    Parent := Parent.Parent;
  end;
end;

function TFeatureWorld.CurrentRule: IRule;
var
  Parent: ISpecItem;
begin
  Result := nil;
  if not Assigned(FCurrentStep) then Exit;
  Parent := FCurrentStep.Parent;
  while Assigned(Parent) do
  begin
    if Supports(Parent, IRule, Result) then
      Exit;
    Parent := Parent.Parent;
  end;
end;

function TFeatureWorld.CurrentFeature: IFeature;
var
  Parent: ISpecItem;
begin
  Result := nil;
  if not Assigned(FCurrentStep) then Exit;
  Parent := FCurrentStep.Parent;
  while Assigned(Parent) do
  begin
    if Supports(Parent, IFeature, Result) then
      Exit;
    Parent := Parent.Parent;
  end;
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
  FreeAndNil(FRunInfo.Error);
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

function TSpecItem.GetEffectiveTags: TSpecTags;
begin
  // EffectiveTags = DeclaredTags + Parent.EffectiveTags
  Result := FTags;
  if Assigned(FParent) then
    Result.Merge(FParent.EffectiveTags);
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
var
  SpecContext: ISpecContext;
begin
  var SW := TStopwatch.StartNew;
  try
    inherited Run(World);
    FRunInfo.State := srsRunning;
    FRunInfo.Result := srrSuccess;
    if Assigned(FProc) then
    begin
      // Si el World implementa ISpecContext, setear el Step actual
      if Supports(World, ISpecContext, SpecContext) then
        SpecContext.CurrentStep := Self;
      FProc(World as T);
    end;
  except
    on E: ExpectFail do
    begin
      FRunInfo.Result := srrFail;
      FRunInfo.Error := Exception(AcquireExceptionObject);
    end;
    on E: Exception do
    begin
      // For When steps, capture exception info instead of marking as error
      // The Then step will verify it with ExpectRaised
      var Scenario := GetCurrentScenario;
      if (FKind = sikWhen) and Assigned(Scenario) then
      begin
        Scenario.CapturedRaise := TCapturedRaise.From(E);
        FRunInfo.Result := srrSuccess;  // Don't fail yet, wait for Then to verify
      end
      else
      begin
        FRunInfo.Result := srrError;
        FRunInfo.Error := Exception(AcquireExceptionObject);
      end;
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
  Create(Feature, Description, True);  // Por defecto añade a Feature.Scenarios
end;

constructor TScenario<T>.Create(const Feature: IFeature; const Description: string; AddToFeatureScenarios: Boolean);
begin
  inherited Create(sikScenario, Feature, Description);
  FStepsGiven := TList<IScenarioStep>.Create;
  FStepsWhen := TList<IScenarioStep>.Create;
  FStepsThen := TList<IScenarioStep>.Create;

  if AddToFeatureScenarios then
    Feature.Scenarios.Add(Self);
end;

constructor TScenario<T>.Create(const ARule: IRule; const Description: string);
begin
  // Crear escenario con Rule como parent para correcta navegación del contexto
  inherited Create(sikScenario, ARule, Description);
  FStepsGiven := TList<IScenarioStep>.Create;
  FStepsWhen := TList<IScenarioStep>.Create;
  FStepsThen := TList<IScenarioStep>.Create;
  // El Builder es responsable de añadirlo a Rule.Scenarios
end;

constructor TScenario<T>.CreateExample(const Outline: IScenarioOutline; const Description: string);
begin
  // Crear un Example con el ScenarioOutline como parent
  inherited Create(sikExample, Outline, Description);
  FStepsGiven := TList<IScenarioStep>.Create;
  FStepsWhen := TList<IScenarioStep>.Create;
  FStepsThen := TList<IScenarioStep>.Create;
  // No añadir a Feature.Scenarios aquí; el Builder lo hace después
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

function TScenario<T>.GetExampleMeta: TExampleMeta;
begin
  Result := FExampleMeta;
end;

function TScenario<T>.GetCapturedRaise: TCapturedRaise;
begin
  Result := FCapturedRaise;
end;

procedure TScenario<T>.SetCapturedRaise(const Value: TCapturedRaise);
begin
  FCapturedRaise := Value;
end;

procedure TScenario<T>.ClearCapturedRaise;
begin
  FCapturedRaise := TCapturedRaise.Empty;
end;

function TScenario<T>.ConsumeCapturedRaise: TCapturedRaise;
begin
  Result := FCapturedRaise;
  FCapturedRaise := TCapturedRaise.Empty;
end;

procedure TScenario<T>.SetExampleMeta(const Meta: TExampleMeta);
begin
  FExampleMeta := Meta;
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
var
  OldScenario: IScenario;
begin
  var SW := TStopwatch.StartNew;
  OldScenario := GetCurrentScenario;
  SetCurrentScenario(Self);
  try
    FCapturedRaise := TCapturedRaise.Empty;  // Clear any captured raise from previous run
    if Assigned(FInitExample) then
      FInitExample.Run(World);
    inherited;
    FRunInfo.State := srsRunning;
    FRunInfo.Result := srrSuccess;
    if FRunInfo.Result = srrSuccess then RunSteps(GetStepsGiven, World);
    if FRunInfo.Result = srrSuccess then RunSteps(GetStepsWhen, World);
    // Always run Then steps, even if When raised an exception (it's captured now)
    if FRunInfo.Result = srrSuccess then RunSteps(GetStepsThen, World);
    // After Then: check if there's an unconsumed captured raise
    if (FRunInfo.Result = srrSuccess) and FCapturedRaise.WasRaised then
    begin
      FRunInfo.Result := srrError;
      // Create a new exception to report the unconsumed raise
      FRunInfo.Error := FCapturedRaise.ExceptionClass.Create(
        'Unhandled exception from When step: ' + FCapturedRaise.ExceptionMessage);
      FCapturedRaise := TCapturedRaise.Empty;
    end;
  except
    on E: Exception do
    begin
      FRunInfo.Result := srrError;
      FRunInfo.Error := Exception(AcquireExceptionObject);
    end;
  end;
  SetCurrentScenario(OldScenario);
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

{ TScenarioOutline<T> }

constructor TScenarioOutline<T>.Create(const ARule: IRule; const Description: string; const Headers: TArray<string>);
begin
  // Crear como sikScenarioOutline y añadir a la Rule (polimorfismo)
  inherited Create(ARule.Feature, Description, False);
  FKind := sikScenarioOutline;
  FParent := ARule;
  FHeaders := Headers;
  FExamples := TList<IScenario>.Create;
  // El Outline se añade a Rule.Scenarios como cualquier otro Scenario
  (ARule as TRule<T>).Scenarios.Add(Self);
end;

destructor TScenarioOutline<T>.Destroy;
begin
  FExamples.Free;
  inherited;
end;

function TScenarioOutline<T>.GetHeaders: TArray<string>;
begin
  Result := FHeaders;
end;

function TScenarioOutline<T>.GetExamples: TList<IScenario>;
begin
  Result := FExamples;
end;

procedure TScenarioOutline<T>.AddExample(const Example: IScenario);
begin
  FExamples.Add(Example);
end;

procedure TScenarioOutline<T>.Run(World: TObject);
var
  Rule: TRule<T>;
  ExampleWorld: T;
begin
  // El Outline ejecuta cada Example con su propio World
  Rule := FParent as TRule<T>;
  FRunInfo.State := srsRunning;
  FRunInfo.Result := srrSuccess;

  for var Example in FExamples do
  begin
    ExampleWorld := Rule.CreateWorld;
    try
      Rule.RunBackground(ExampleWorld);
      Example.Run(ExampleWorld);
      if not Example.RunInfo.IsSuccess then
        FRunInfo.Result := srrFail;
    finally
      ExampleWorld.Free;
    end;
  end;

  FRunInfo.State := srsFinished;
end;

{ TFeature<T>}

procedure TFeature<T>.ParseDescription(const Description: string);
var
  Lines: TArray<string>;
  I, TitleIndex, BlankAfterTitle: Integer;
  FoundTitle, FoundBlank: Boolean;
  NarrativeLines: TStringList;
begin
  // Parsear descripción: título es primera línea no vacía,
  // narrativa empieza tras al menos una línea en blanco después del título
  Lines := Description.Split([#13, #10]);
  FTitle := '';
  FNarrative := '';
  TitleIndex := -1;
  FoundTitle := False;
  FoundBlank := False;

  // Encontrar título (primera línea no vacía)
  for I := 0 to High(Lines) do
  begin
    if Lines[I].Trim <> '' then
    begin
      FTitle := Lines[I].Trim;
      TitleIndex := I;
      FoundTitle := True;
      Break;
    end;
  end;

  if not FoundTitle then Exit;

  // Buscar línea en blanco después del título
  BlankAfterTitle := -1;
  for I := TitleIndex + 1 to High(Lines) do
  begin
    if Lines[I].Trim = '' then
    begin
      BlankAfterTitle := I;
      FoundBlank := True;
      Break;
    end;
  end;

  if not FoundBlank then Exit;

  // Narrativa: desde primera línea no vacía después de la línea en blanco
  NarrativeLines := TStringList.Create;
  try
    for I := BlankAfterTitle + 1 to High(Lines) do
      NarrativeLines.Add(Lines[I]);
    FNarrative := NarrativeLines.Text.Trim;
  finally
    NarrativeLines.Free;
  end;
end;

function TFeature<T>.GetTitle: string;
begin
  Result := FTitle;
end;

function TFeature<T>.GetNarrative: string;
begin
  Result := FNarrative;
end;

constructor TFeature<T>.Create(const Description: string);
begin
  inherited Create(sikFeature, nil, Description);
  ParseDescription(Description);
  FRules := TList<IRule>.Create;
  // Crear Rule implícita como contenedor por defecto
  FImplicitRule := TRule<T>.Create(Self, '', sikImplicitRule);
  FRules.Add(FImplicitRule);
  MiniSpec.Register(Self as IFeature);
end;

destructor TFeature<T>.Destroy;
begin
  FRules.Free;
  inherited;
end;

function TFeature<T>.GetBackGround: IBackground;
begin
  Result := FImplicitRule.BackGround;
end;

function TFeature<T>.GetScenarios: TList<IScenario>;
begin
  // Para compatibilidad: devuelve escenarios de la Rule implícita
  Result := FImplicitRule.Scenarios;
end;

function TFeature<T>.GetRules: TList<IRule>;
begin
  Result := FRules;
end;

function TFeature<T>.HasMatchingScenarios(const TagMatcher: TTagMatcher): Boolean;
begin
  if not Assigned(TagMatcher) then
    Exit(True);

  // Verificar escenarios en todas las Rules (incluyendo ImplicitRule)
  for var Rule in FRules do
    if Rule.HasMatchingScenarios(TagMatcher) then
      Exit(True);

  Result := False;
end;

procedure TFeature<T>.Run(const TagMatcher: TTagMatcher);
begin
  var SW := TStopwatch.StartNew;
  FRunInfo.State := srsRunning;
  FRunInfo.Result := srrSuccess;
  try
    // Ejecutar todas las Rules (incluyendo ImplicitRule)
    // Cada Rule decide internamente qué escenarios ejecutar o marcar como Skip
    for var Rule in FRules do
    begin
      Rule.Run(TagMatcher);
      if Rule.RunInfo.Result in [srrFail, srrError] then
        FRunInfo.Result := srrFail;
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

procedure TFeature<T>.SetBackGround(const Value: IBackground);
begin
  FImplicitRule.BackGround := Value;
end;

{ TRule<T> }

constructor TRule<T>.Create(const Feature: IFeature; const Description: string);
begin
  Create(Feature, Description, sikRule);
end;

constructor TRule<T>.Create(const Feature: IFeature; const Description: string; const Kind: TSpecItemKind);
begin
  inherited Create(Kind, Feature, Description);
  FFeature := Feature;
  FScenarios := TList<IScenario>.Create;
end;

destructor TRule<T>.Destroy;
begin
  FScenarios.Free;
  inherited;
end;

function TRule<T>.GetFeature: IFeature;
begin
  Result := FFeature;
end;

function TRule<T>.GetBackGround: IBackground;
begin
  Result := FBackground;
end;

procedure TRule<T>.SetBackGround(const Value: IBackground);
begin
  FBackGround := Value;
end;

function TRule<T>.GetScenarios: TList<IScenario>;
begin
  Result := FScenarios;
end;

function TRule<T>.CreateWorld: T;
begin
  Result := T.Create;
end;

procedure TRule<T>.RunBackground(World: T);
begin
  // Primero ejecutar Background de Feature, luego el de Rule
  if Assigned(FFeature) and Assigned(FFeature.BackGround) then
    FFeature.BackGround.Run(World);
  if Assigned(FBackground) then
    FBackground.Run(World);
end;

function TRule<T>.HasMatchingScenarios(const TagMatcher: TTagMatcher): Boolean;
var
  CombinedTags: TSpecTags;
begin
  if not Assigned(TagMatcher) then
    Exit(True);

  for var Scenario in FScenarios do
  begin
    // Tags combinados: Feature + Rule + Scenario
    CombinedTags := FFeature.Tags;
    CombinedTags.Merge(Self.Tags);
    CombinedTags.Merge(Scenario.Tags);
    if TagMatcher(CombinedTags) then
      Exit(True);
  end;
  Result := False;
end;

procedure TRule<T>.Run(const TagMatcher: TTagMatcher);
var
  ShouldExecute: Boolean;
begin
  var SW := TStopwatch.StartNew;
  FRunInfo.State := srsRunning;
  FRunInfo.Result := srrSuccess;
  try
    for var Scenario in FScenarios do
    begin
      // Decidir si ejecutar basándose en EffectiveTags y DryRun
      ShouldExecute := True;

      // Verificar filtro de tags usando EffectiveTags
      if Assigned(TagMatcher) then
        ShouldExecute := TagMatcher(Scenario.EffectiveTags);

      // Verificar modo DryRun
      if ShouldExecute and MiniSpec.DryRun then
        ShouldExecute := False;

      if not ShouldExecute then
      begin
        // Marcar como Skip y continuar (pero el escenario queda visible para el reporter)
        TSpecItem(Scenario).FRunInfo.State := srsSkiped;
        // Si es un Outline, marcar también todos sus Examples como skip
        var Outline: IScenarioOutline;
        if Supports(Scenario, IScenarioOutline, Outline) then
          for var Example in Outline.Examples do
            TSpecItem(Example).FRunInfo.State := srsSkiped;
        Continue;
      end;

      // ScenarioOutline maneja sus propios Worlds (uno por Example)
      // No crear World aquí para evitar Worlds fantasma
      if Supports(Scenario, IScenarioOutline) then
      begin
        Scenario.Run(nil);
        if Scenario.RunInfo.Result in [srrFail, srrError] then
          FRunInfo.Result := srrFail;
      end
      else
      begin
        var World := CreateWorld;
        RunBackground(World);
        Scenario.Run(World);
        if Scenario.RunInfo.Result in [srrFail, srrError] then
          FRunInfo.Result := srrFail;
        World.Free;
      end;
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

function GetCurrentScenario: IScenario;
begin
  Result := CurrentScenario;
end;

procedure SetCurrentScenario(const Value: IScenario);
begin
  CurrentScenario := Value;
end;

end.
