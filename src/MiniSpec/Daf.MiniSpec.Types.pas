unit Daf.MiniSpec.Types;

interface
uses
  System.Classes,
  System.Generics.Collections,
  System.Rtti,
  System.SysUtils,
  Daf.MiniSpec.DataTable;

type
  TStepProc<T> = reference to procedure(World: T);
  THookProc = reference to procedure;  // For Before/After hooks (no World)
  TExamplesTable = TArray<TArray<TValue>>;
  TSpecItemKind = (sikSuite, sikFeature, sikImplicitRule, sikRule, sikBackground, sikScenario, sikScenarioOutline, sikExample, sikExampleInit, sikGiven, sikWhen, sikThen, sikAnd, sikBut, sikBefore, sikAfter);
  TLastStepKind = (lskNone, lskGiven, lskWhen, lskThen);
  TSpecRunState =  (srsPrepared, srsSkiped, srsRunning, srsFinished);
  TSpecRunResult =  (srrNone, srrSuccess, srrFail, srrError);
  /// <summary>
  /// Result kind for counting Examples: Pass, Fail, or Skip.
  /// Used in TSpecRunInfo.Counts for aggregated results.
  /// </summary>
  TResultKind = (rkPass, rkFail, rkSkip);
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

  // Forward declaration
  IScenario = interface;

  /// <summary>
  /// Matcher function for filtering scenarios based on full context.
  /// Used by Feat:, Scen:, Rule:, Cat: filters in addition to @tag filters.
  /// </summary>
  TSpecMatcher = reference to function(const Scenario: IScenario): Boolean;

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
    /// <summary>
    /// Aggregated counts of child Examples by result kind.
    /// Propagated from children via ISpecItem.IncCount.
    /// </summary>
    Counts: array[TResultKind] of Cardinal;
    procedure Clear;
    function ErrMsg: string;
    function IsSuccess: Boolean;
    function PassCount: Cardinal;
    function FailCount: Cardinal;
    function SkipCount: Cardinal;
    function TotalCount: Cardinal;
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
    /// <summary>
    /// Sets the category name for filtering with C:.
    /// </summary>
    function Category(const Name: string): IFeatureBuilder<T>; overload;
    /// <summary>
    /// Sets the category extracting the name from QualifiedClassName.
    /// Convention: declare TCategory = class end; in each feature unit.
    /// </summary>
    function Category(AClass: TClass): IFeatureBuilder<T>; overload;
    /// <summary>
    /// Adds a Before hook that runs once before all scenarios in the feature.
    /// </summary>
    function Before(const Description: string; Hook: THookProc): IFeatureBuilder<T>;
    /// <summary>
    /// Adds an After hook that runs once after all scenarios in the feature.
    /// </summary>
    function After(const Description: string; Hook: THookProc): IFeatureBuilder<T>;
    function Background: IBackgroundBuilder<T>;
    function Scenario(const Description: string): IScenarioBuilder<T>;overload;
    function ScenarioOutline(const Description: string): IScenarioOutlineBuilder<T>;
    function Rule(const Description: string): IRuleBuilder<T>;
  end;

  /// <summary>
  /// Builder para construir Rules dentro de una Feature.
  /// Las Rules agrupan escenarios relacionados.
  /// </summary>
  /// <remarks>
  /// Nota: Tras Examples() el fluent API permanece en la Rule actual.
  /// Para añadir scenarios sin Rule después de una Rule, usar EndRule
  /// para volver a Feature, o escribir los scenarios sin Rule al principio.
  /// </remarks>
  IRuleBuilder<T: class, constructor> = interface
    function Background: IBackgroundBuilder<T>;
    function Scenario(const Description: string): IScenarioBuilder<T>;
    function ScenarioOutline(const Description: string): IScenarioOutlineBuilder<T>;
    /// <summary>
    /// Inicia una nueva Rule hermana en la misma Feature.
    /// </summary>
    function Rule(const Description: string): IRuleBuilder<T>;
    /// <summary>
    /// Cierra la Rule actual y vuelve a Feature para añadir
    /// scenarios en ImplicitRule o iniciar nuevas Rules.
    /// </summary>
    function EndRule: IFeatureBuilder<T>;
  end;

  IScenarioBuilder<T: class, constructor> = interface
    function Given(const Desc: string; Step: TStepProc<T>): IScenarioBuilder<T>; overload;
    function Given(const Desc: string; const Table: TDataTable; Step: TStepProc<T>): IScenarioBuilder<T>; overload;
    function When(const Desc: string; Step: TStepProc<T>) : IScenarioBuilder<T>; overload;
    function When(const Desc: string; const Table: TDataTable; Step: TStepProc<T>): IScenarioBuilder<T>; overload;
    function &Then(const Desc: string; Step: TStepProc<T>) : IScenarioBuilder<T>; overload;
    function &Then(const Desc: string; const Table: TDataTable; Step: TStepProc<T>): IScenarioBuilder<T>; overload;
    function &And(const Desc: string; Step: TStepProc<T>): IScenarioBuilder<T>; overload;
    function &And(const Desc: string; const Table: TDataTable; Step: TStepProc<T>): IScenarioBuilder<T>; overload;
    function But(const Desc: string; Step: TStepProc<T>): IScenarioBuilder<T>; overload;
    function But(const Desc: string; const Table: TDataTable; Step: TStepProc<T>): IScenarioBuilder<T>; overload;

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
    /// <summary>
    /// Define los ejemplos para el ScenarioOutline.
    /// Devuelve IRuleBuilder para continuar en la Rule actual
    /// o usar EndRule para volver a Feature.
    /// </summary>
    function Examples(const Table: TExamplesTable): IRuleBuilder<T>;

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
    function GetKeyWord: string;
    function GetLevel: Byte;

    procedure Run(World: TObject);
    /// <summary>
    /// Increments the count for the given result kind and propagates to parent.
    /// Called by Scenario/Example when execution completes.
    /// </summary>
    procedure IncCount(Kind: TResultKind);
    property Kind: TSpecItemKind read GetKind;
    property Parent: ISpecItem read GetParent;
    property Description: string read GetDescription;
    property Tags: TSpecTags read GetTags;
    property EffectiveTags: TSpecTags read GetEffectiveTags;
    property RunInfo: TSpecRunInfo read GetRunInfo;
    /// <summary>
    /// Returns the Gherkin keyword for this item (Feature, Scenario, Given, etc.)
    /// Centralized for consistency and future i18n support.
    /// </summary>
    property KeyWord: string read GetKeyWord;
    /// <summary>
    /// Returns the nesting level of this item (Feature=0, Rule/Scenario=1, Step=2, etc.)
    /// Calculated as Parent.Level + 1, with Feature being level 0.
    /// </summary>
    property Level: Byte read GetLevel;
  end;

  IScenarioStep = interface(ISpecItem)
    ['{3F16A9FA-0F64-4B4C-985F-D09087DD7404}']
    function GetDataTable: TDataTableObj;
    property DataTable: TDataTableObj read GetDataTable;
  end;

  /// <summary>
  /// Hook for Before/After feature execution.
  /// Unlike steps, hooks don't receive a World parameter.
  /// </summary>
  IHook = interface(ISpecItem)
    ['{8B7C6D5E-4F3A-2B1C-0D9E-8F7A6B5C4D3E}']
    procedure Execute;
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
    function GetCategory: string;
    procedure SetCategory(const Value: string);
    function GetBackGround: IBackground;
    procedure SetBackGround(const Value: IBackground);
    function GetScenarios: TList<IScenario>;
    function GetRules: TList<IRule>;
    function GetBeforeHooks: TList<IHook>;
    function GetAfterHooks: TList<IHook>;
    function HasMatchingScenarios(const Matcher: TSpecMatcher): Boolean;
    procedure Run(const Matcher: TSpecMatcher = nil);
    property Title: string read GetTitle;
    property Narrative: string read GetNarrative;
    /// <summary>
    /// Category for grouping/filtering features.
    /// Set automatically or manually with .Category('name').
    /// </summary>
    property Category: string read GetCategory write SetCategory;
    property BackGround: IBackground read GetBackground write SetBackground;
    property Scenarios: TList<IScenario> read GetScenarios;
    property Rules: TList<IRule> read GetRules;
    /// <summary>
    /// Hooks that run once before all scenarios in the feature.
    /// </summary>
    property BeforeHooks: TList<IHook> read GetBeforeHooks;
    /// <summary>
    /// Hooks that run once after all scenarios in the feature.
    /// </summary>
    property AfterHooks: TList<IHook> read GetAfterHooks;
  end;

  /// <summary>
  /// Suite is the root node containing all Features.
  /// Provides global setup/teardown and aggregated counters.
  /// </summary>
  ISpecSuite = interface(ISpecItem)
    ['{A1B2C3D4-E5F6-7A8B-9C0D-1E2F3A4B5C6D}']
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    function GetFeatures: TList<IFeature>;
    function GetBeforeHooks: TList<IHook>;
    function GetAfterHooks: TList<IHook>;
    procedure AddFeature(const Feature: IFeature);
    procedure AddBeforeHook(const Description: string; const Hook: THookProc);
    procedure AddAfterHook(const Description: string; const Hook: THookProc);
    procedure RunBeforeHooks;
    procedure RunAfterHooks;
    /// <summary>
    /// Title of the test suite (set via MiniSpec.Category).
    /// </summary>
    property Title: string read GetTitle write SetTitle;
    /// <summary>
    /// All features registered in this suite.
    /// </summary>
    property Features: TList<IFeature> read GetFeatures;
    /// <summary>
    /// Hooks that run once before all features in the suite.
    /// </summary>
    property BeforeHooks: TList<IHook> read GetBeforeHooks;
    /// <summary>
    /// Hooks that run once after all features in the suite.
    /// </summary>
    property AfterHooks: TList<IHook> read GetAfterHooks;
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
    function HasMatchingScenarios(const Matcher: TSpecMatcher): Boolean;
    procedure Run(const Matcher: TSpecMatcher = nil);
    property Feature: IFeature read GetFeature;
    property BackGround: IBackground read GetBackground write SetBackground;
    property Scenarios: TList<IScenario> read GetScenarios;
  end;

  /// <summary>
  /// Interfaz global para acceder al contexto de ejecución.
  /// Accesible desde cualquier lugar via función SpecContext.
  /// </summary>
  ISpecContext = interface
    ['{B8C9D0E1-F2A3-4B5C-6D7E-8F9A0B1C2D3E}']
    // Metadatos de ejecución
    function Suite: ISpecSuite;
    function Feature: IFeature;
    function Rule: IRule;
    function Scenario: IScenario;
    function Step: IScenarioStep;
    function DataTable: TDataTableObj;
    // Contexts del usuario (para UseContext)
    function SuiteContext: TObject;
    function FeatureContext: TObject;
    function ScenarioContext: TObject;
    // Setters internos
    procedure SetStep(const Value: IScenarioStep);
    procedure SetDataTable(const Value: TDataTableObj);
    procedure SetScenario(const Value: IScenario);
  end;

  /// <summary>
  /// Implementación del contexto de ejecución global.
  /// </summary>
  TSpecContextImpl = class(TInterfacedObject, ISpecContext)
  strict private
    FSuite: ISpecSuite;
    FFeature: IFeature;
    FRule: IRule;
    FScenario: IScenario;
    FStep: IScenarioStep;
    FDataTable: TDataTableObj;
    FSuiteContext: TObject;
    FFeatureContext: TObject;
    FScenarioContext: TObject;
  public
    function Suite: ISpecSuite;
    function Feature: IFeature;
    function Rule: IRule;
    function Scenario: IScenario;
    function Step: IScenarioStep;
    function DataTable: TDataTableObj;
    function SuiteContext: TObject;
    function FeatureContext: TObject;
    function ScenarioContext: TObject;
    procedure SetStep(const Value: IScenarioStep);
    procedure SetDataTable(const Value: TDataTableObj);
    procedure SetScenario(const Value: IScenario);
    procedure SetSuite(const Value: ISpecSuite);
    procedure SetFeature(const Value: IFeature);
    procedure SetRule(const Value: IRule);
    procedure SetSuiteContext(const Value: TObject);
    procedure SetFeatureContext(const Value: TObject);
    procedure SetScenarioContext(const Value: TObject);
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
    function GetKeyWord: string;virtual;
    function GetLevel: Byte;virtual;
    procedure SetParent(const Value: ISpecItem);
  public
    constructor Create(const Kind: TSpecItemKind; const Parent: ISpecItem; const Description: string);
    destructor Destroy;override;
    procedure Run(World: TObject);virtual;
    procedure IncCount(Kind: TResultKind);virtual;
    property Parent: ISpecItem read FParent write SetParent;
    property Description: string read GetDescription;
    property Tags: TSpecTags read GetTags;
    property EffectiveTags: TSpecTags read GetEffectiveTags;
    property KeyWord: string read GetKeyWord;
    property Level: Byte read GetLevel;
  end;

  TScenarioStep<T: class> = class(TSpecItem, IScenarioStep)
  strict private
    FProc: TStepProc<T>;
    FDataTable: TDataTableObj;
    function GetDataTable: TDataTableObj;
  protected
  public
    constructor Create(const Kind: TSpecItemKind; const Parent: ISpecItem; const Description: string; const Proc: TStepProc<T>); overload;
    constructor Create(const Kind: TSpecItemKind; const Parent: ISpecItem; const Description: string; const Proc: TStepProc<T>; const ADataTable: TDataTable); overload;
    destructor Destroy;override;
    procedure Run(World: TObject);override;
    property Proc: TStepProc<T> read FProc;
    property DataTable: TDataTableObj read GetDataTable;
  end;

  /// <summary>
  /// Hook for Before/After feature execution.
  /// Wraps a THookProc (no World parameter) in the standard TSpecItem infrastructure.
  /// </summary>
  THook = class(TSpecItem, IHook)
  strict private
    FProc: THookProc;
  public
    constructor Create(const Kind: TSpecItemKind; const Parent: ISpecItem; const Description: string; const Proc: THookProc);
    procedure Execute;
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
    function Given(const Desc: string; Step: TStepProc<T>): TScenario<T>; overload;
    function Given(const Desc: string; const Table: TDataTable; Step: TStepProc<T>): TScenario<T>; overload;
    function When(const Desc: string; Step: TStepProc<T>) : TScenario<T>; overload;
    function When(const Desc: string; const Table: TDataTable; Step: TStepProc<T>): TScenario<T>; overload;
    function &Then(const Desc: string; Step: TStepProc<T>) : TScenario<T>; overload;
    function &Then(const Desc: string; const Table: TDataTable; Step: TStepProc<T>): TScenario<T>; overload;
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
    FCategory: string;
    FBeforeHooks: TList<IHook>;
    FAfterHooks: TList<IHook>;
    function GetTitle: string;
    function GetNarrative: string;
    function GetCategory: string;
    procedure SetCategory(const Value: string);
    function GetBackGround: IBackground;
    procedure SetBackGround(const Value: IBackground);
    function GetScenarios: TList<IScenario>;
    function GetRules: TList<IRule>;
    function GetBeforeHooks: TList<IHook>;
    function GetAfterHooks: TList<IHook>;
    procedure ParseDescription(const Description: string);
  public
    constructor Create(const Description: string);
    destructor Destroy; override;
    function HasMatchingScenarios(const Matcher: TSpecMatcher): Boolean;
    procedure Run(const Matcher: TSpecMatcher = nil);reintroduce;
    property Title: string read GetTitle;
    property Narrative: string read GetNarrative;
    property Category: string read GetCategory write SetCategory;
    property Background: IBackground read GetBackGround write SetBackGround;
    property Rules: TList<IRule> read GetRules;
    property BeforeHooks: TList<IHook> read GetBeforeHooks;
    property AfterHooks: TList<IHook> read GetAfterHooks;
    property ImplicitRule: IRule read FImplicitRule;  // Solo para builders
  end;

  /// <summary>
  /// Suite is the root node containing all Features.
  /// Provides global setup/teardown for the entire test run.
  /// </summary>
  TSpecSuite = class(TSpecItem, ISpecSuite)
  strict private
    FTitle: string;
    FFeatures: TList<IFeature>;
    FBeforeHooks: TList<IHook>;
    FAfterHooks: TList<IHook>;
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    function GetFeatures: TList<IFeature>;
    function GetBeforeHooks: TList<IHook>;
    function GetAfterHooks: TList<IHook>;
  public
    constructor Create(const ATitle: string = '');
    destructor Destroy; override;
    procedure AddFeature(const Feature: IFeature);
    procedure AddBeforeHook(const Description: string; const Hook: THookProc);
    procedure AddAfterHook(const Description: string; const Hook: THookProc);
    procedure RunBeforeHooks;
    procedure RunAfterHooks;
    property Title: string read GetTitle write SetTitle;
    property Features: TList<IFeature> read GetFeatures;
    property BeforeHooks: TList<IHook> read GetBeforeHooks;
    property AfterHooks: TList<IHook> read GetAfterHooks;
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
    function HasMatchingScenarios(const Matcher: TSpecMatcher): Boolean;
    procedure Run(const Matcher: TSpecMatcher = nil);reintroduce;
    property Feature: IFeature read GetFeature;
    property Background: IBackground read GetBackGround write SetBackGround;
    property Scenarios: TList<IScenario> read GetScenarios;
  end;

/// <summary>
/// Converts a TValue to string, with proper array formatting.
/// </summary>
function Val2Str(const V: TValue): string;

/// <summary>
/// Global execution context. Returns current ISpecContext.
/// </summary>
function SpecContext: ISpecContext;

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
  /// Global execution context. Access via SpecContext function.
  /// </summary>
  GSpecContext: ISpecContext;

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
  for var K := Low(TResultKind) to High(TResultKind) do
    Counts[K] := 0;
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
  // Success means no failures (skips are acceptable)
  Result := Counts[rkFail] = 0;
end;

function TSpecRunInfo.PassCount: Cardinal;
begin
  Result := Counts[rkPass];
end;

function TSpecRunInfo.FailCount: Cardinal;
begin
  Result := Counts[rkFail];
end;

function TSpecRunInfo.SkipCount: Cardinal;
begin
  Result := Counts[rkSkip];
end;

function TSpecRunInfo.TotalCount: Cardinal;
begin
  Result := Counts[rkPass] + Counts[rkFail] + Counts[rkSkip];
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

procedure TSpecItem.IncCount(Kind: TResultKind);
begin
  Inc(FRunInfo.Counts[Kind]);
  if Assigned(FParent) then
    FParent.IncCount(Kind);
end;

function TSpecItem.GetDescription: string;
begin
  Result := FDescription;
end;

function TSpecItem.GetParent: ISpecItem;
begin
  Result := FParent;
end;

procedure TSpecItem.SetParent(const Value: ISpecItem);
begin
  FParent := Value;
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

function TSpecItem.GetKeyWord: string;
begin
  case FKind of
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
    sikAnd: Result := 'And';
    sikBut: Result := 'But';
    else
      Result := '';
  end;
end;

function TSpecItem.GetLevel: Byte;
begin
  if FKind = sikFeature then
    Result := 0
  else if Assigned(FParent) then
    Result := FParent.Level + 1
  else
    Result := 0;
end;

{ TScenarioStep<T> }

constructor TScenarioStep<T>.Create(const Kind: TSpecItemKind; const Parent: ISpecItem; const Description: string; const Proc: TStepProc<T>);
begin
  inherited Create(Kind, Parent, Description);
  FProc := Proc;
  FDataTable := nil;
end;

constructor TScenarioStep<T>.Create(const Kind: TSpecItemKind; const Parent: ISpecItem; const Description: string; const Proc: TStepProc<T>; const ADataTable: TDataTable);
begin
  inherited Create(Kind, Parent, Description);
  FProc := Proc;
  if Length(ADataTable) > 0 then
    FDataTable := TDataTableObj.Create(ADataTable)
  else
    FDataTable := nil;
end;

destructor TScenarioStep<T>.Destroy;
begin
  FProc := nil;
  FDataTable.Free;
  inherited;
end;

function TScenarioStep<T>.GetDataTable: TDataTableObj;
begin
  Result := FDataTable;
end;

procedure TScenarioStep<T>.Run(World: TObject);
var
  Ctx: TSpecContextImpl;
begin
  var SW := TStopwatch.StartNew;
  try
    inherited Run(World);
    FRunInfo.State := srsRunning;
    FRunInfo.Result := srrSuccess;
    if Assigned(FProc) then
    begin
      // Usar contexto global
      Ctx := TSpecContextImpl(SpecContext);
      Ctx.SetStep(Self);
      Ctx.SetDataTable(FDataTable);
      FProc(World as T);
      // Clear DataTable after step execution
      Ctx.SetDataTable(nil);
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

{ THook }

constructor THook.Create(const Kind: TSpecItemKind; const Parent: ISpecItem; const Description: string; const Proc: THookProc);
begin
  inherited Create(Kind, Parent, Description);
  FProc := Proc;
end;

procedure THook.Execute;
begin
  var SW := TStopwatch.StartNew;
  FRunInfo.State := srsRunning;
  FRunInfo.Result := srrSuccess;
  try
    if Assigned(FProc) then
      FProc();
  except
    on E: Exception do
    begin
      FRunInfo.Result := srrError;
      FRunInfo.Error := Exception(AcquireExceptionObject);
      raise;  // Re-raise to stop feature execution
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

function TScenario<T>.Given(const Desc: string; const Table: TDataTable; Step: TStepProc<T>): TScenario<T>;
begin
  Result := Self;
  FStepsGiven.Add(TScenarioStep<T>.Create(sikGiven, Self, Desc, Step, Table));
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
  // Propagate count to parent (Feature/Rule/Outline)
  case FRunInfo.Result of
    srrSuccess: IncCount(rkPass);
    srrFail, srrError: IncCount(rkFail);
  end;
end;

function TScenario<T>.When(const Desc: string; Step: TStepProc<T>): TScenario<T>;
begin
  Result := Self;
  FStepsWhen.Add(TScenarioStep<T>.Create(sikWhen, Self, Desc, Step));
end;

function TScenario<T>.When(const Desc: string; const Table: TDataTable; Step: TStepProc<T>): TScenario<T>;
begin
  Result := Self;
  FStepsWhen.Add(TScenarioStep<T>.Create(sikWhen, Self, Desc, Step, Table));
end;

function TScenario<T>.&Then(const Desc: string; Step: TStepProc<T>): TScenario<T>;
begin
  Result := Self;
  FStepsThen.Add(TScenarioStep<T>.Create(sikThen, Self, Desc, Step));
end;

function TScenario<T>.&Then(const Desc: string; const Table: TDataTable; Step: TStepProc<T>): TScenario<T>;
begin
  Result := Self;
  FStepsThen.Add(TScenarioStep<T>.Create(sikThen, Self, Desc, Step, Table));
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

function TFeature<T>.GetCategory: string;
begin
  Result := FCategory;
end;

procedure TFeature<T>.SetCategory(const Value: string);
begin
  FCategory := Value;
end;

constructor TFeature<T>.Create(const Description: string);
begin
  inherited Create(sikFeature, nil, Description);
  ParseDescription(Description);
  FRules := TList<IRule>.Create;
  FBeforeHooks := TList<IHook>.Create;
  FAfterHooks := TList<IHook>.Create;
  // Crear Rule implícita como contenedor por defecto
  FImplicitRule := TRule<T>.Create(Self, '', sikImplicitRule);
  FRules.Add(FImplicitRule);
  MiniSpec.Register(Self as IFeature);
end;

destructor TFeature<T>.Destroy;
begin
  FAfterHooks.Free;
  FBeforeHooks.Free;
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

function TFeature<T>.GetBeforeHooks: TList<IHook>;
begin
  Result := FBeforeHooks;
end;

function TFeature<T>.GetAfterHooks: TList<IHook>;
begin
  Result := FAfterHooks;
end;

function TFeature<T>.HasMatchingScenarios(const Matcher: TSpecMatcher): Boolean;
begin
  if not Assigned(Matcher) then
    Exit(True);

  // Verificar escenarios en todas las Rules (incluyendo ImplicitRule)
  for var Rule in FRules do
    if Rule.HasMatchingScenarios(Matcher) then
      Exit(True);

  Result := False;
end;

procedure TFeature<T>.Run(const Matcher: TSpecMatcher);
begin
  var SW := TStopwatch.StartNew;
  FRunInfo.State := srsRunning;
  FRunInfo.Result := srrSuccess;
  try
    // Execute Before hooks (once before all scenarios)
    for var Hook in FBeforeHooks do
      Hook.Execute;

    // Ejecutar todas las Rules (incluyendo ImplicitRule)
    // Cada Rule decide internamente qué escenarios ejecutar o marcar como Skip
    for var Rule in FRules do
    begin
      Rule.Run(Matcher);
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

  // Execute After hooks (once after all scenarios, even if there were errors)
  try
    for var Hook in FAfterHooks do
      Hook.Execute;
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

{ TSpecSuite }

constructor TSpecSuite.Create(const ATitle: string = '');
begin
  inherited Create(sikSuite, nil, ATitle);
  FTitle := ATitle;
  FFeatures := TList<IFeature>.Create;
  FBeforeHooks := TList<IHook>.Create;
  FAfterHooks := TList<IHook>.Create;
end;

destructor TSpecSuite.Destroy;
begin
  FAfterHooks.Free;
  FBeforeHooks.Free;
  FFeatures.Free;
  inherited;
end;

function TSpecSuite.GetTitle: string;
begin
  Result := FTitle;
end;

procedure TSpecSuite.SetTitle(const Value: string);
begin
  FTitle := Value;
end;

function TSpecSuite.GetFeatures: TList<IFeature>;
begin
  Result := FFeatures;
end;

function TSpecSuite.GetBeforeHooks: TList<IHook>;
begin
  Result := FBeforeHooks;
end;

function TSpecSuite.GetAfterHooks: TList<IHook>;
begin
  Result := FAfterHooks;
end;

procedure TSpecSuite.AddFeature(const Feature: IFeature);
begin
  FFeatures.Add(Feature);
  // Set Suite as parent so counters propagate correctly
  (Feature as TSpecItem).Parent := Self;
end;

procedure TSpecSuite.AddBeforeHook(const Description: string; const Hook: THookProc);
begin
  FBeforeHooks.Add(THook.Create(sikBefore, Self, Description, Hook));
end;

procedure TSpecSuite.AddAfterHook(const Description: string; const Hook: THookProc);
begin
  FAfterHooks.Add(THook.Create(sikAfter, Self, Description, Hook));
end;

procedure TSpecSuite.RunBeforeHooks;
begin
  for var Hook in FBeforeHooks do
    Hook.Execute;
end;

procedure TSpecSuite.RunAfterHooks;
begin
  for var Hook in FAfterHooks do
    Hook.Execute;
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

function TRule<T>.HasMatchingScenarios(const Matcher: TSpecMatcher): Boolean;
begin
  if not Assigned(Matcher) then
    Exit(True);

  for var Scenario in FScenarios do
  begin
    if Matcher(Scenario) then
      Exit(True);
  end;
  Result := False;
end;

procedure TRule<T>.Run(const Matcher: TSpecMatcher);
var
  ShouldExecute: Boolean;
begin
  var SW := TStopwatch.StartNew;
  FRunInfo.State := srsRunning;
  FRunInfo.Result := srrSuccess;
  try
    for var Scenario in FScenarios do
    begin
      // Decidir si ejecutar basándose en Matcher y DryRun
      ShouldExecute := True;

      // Verificar filtro usando el Matcher completo
      if Assigned(Matcher) then
        ShouldExecute := Matcher(Scenario);

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
        begin
          for var Example in Outline.Examples do
          begin
            TSpecItem(Example).FRunInfo.State := srsSkiped;
            Example.IncCount(rkSkip);  // Propagate skip count
          end;
        end
        else
          Scenario.IncCount(rkSkip);  // Propagate skip count for simple scenario
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

{ TSpecContextImpl }

function TSpecContextImpl.Suite: ISpecSuite;
begin
  Result := FSuite;
end;

function TSpecContextImpl.Feature: IFeature;
begin
  Result := FFeature;
end;

function TSpecContextImpl.Rule: IRule;
begin
  Result := FRule;
end;

function TSpecContextImpl.Scenario: IScenario;
begin
  Result := FScenario;
end;

function TSpecContextImpl.Step: IScenarioStep;
begin
  Result := FStep;
end;

function TSpecContextImpl.DataTable: TDataTableObj;
begin
  Result := FDataTable;
end;

function TSpecContextImpl.SuiteContext: TObject;
begin
  Result := FSuiteContext;
end;

function TSpecContextImpl.FeatureContext: TObject;
begin
  Result := FFeatureContext;
end;

function TSpecContextImpl.ScenarioContext: TObject;
begin
  Result := FScenarioContext;
end;

procedure TSpecContextImpl.SetStep(const Value: IScenarioStep);
begin
  FStep := Value;
end;

procedure TSpecContextImpl.SetDataTable(const Value: TDataTableObj);
begin
  FDataTable := Value;
end;

procedure TSpecContextImpl.SetScenario(const Value: IScenario);
begin
  FScenario := Value;
end;

procedure TSpecContextImpl.SetSuite(const Value: ISpecSuite);
begin
  FSuite := Value;
end;

procedure TSpecContextImpl.SetFeature(const Value: IFeature);
begin
  FFeature := Value;
end;

procedure TSpecContextImpl.SetRule(const Value: IRule);
begin
  FRule := Value;
end;

procedure TSpecContextImpl.SetSuiteContext(const Value: TObject);
begin
  FSuiteContext := Value;
end;

procedure TSpecContextImpl.SetFeatureContext(const Value: TObject);
begin
  FFeatureContext := Value;
end;

procedure TSpecContextImpl.SetScenarioContext(const Value: TObject);
begin
  FScenarioContext := Value;
end;

{ Global SpecContext }

function SpecContext: ISpecContext;
begin
  if GSpecContext = nil then
    GSpecContext := TSpecContextImpl.Create;
  Result := GSpecContext;
end;

function GetCurrentScenario: IScenario;
begin
  Result := SpecContext.Scenario;
end;

procedure SetCurrentScenario(const Value: IScenario);
var
  Ctx: TSpecContextImpl;
  Parent: ISpecItem;
begin
  Ctx := TSpecContextImpl(SpecContext);
  Ctx.SetScenario(Value);

  // Derivar Feature y Rule del Scenario navegando por parents usando Kind
  if Assigned(Value) then
  begin
    Parent := Value.Parent;
    while Assigned(Parent) do
    begin
      case Parent.Kind of
        sikRule, sikImplicitRule:
          if Ctx.Rule = nil then
            Ctx.SetRule(Parent as IRule);
        sikFeature:
          begin
            Ctx.SetFeature(Parent as IFeature);
            Break;  // Feature es el nivel más alto
          end;
      end;
      Parent := Parent.Parent;
    end;
  end
  else
  begin
    // Limpiar cuando se setea nil
    Ctx.SetRule(nil);
    Ctx.SetFeature(nil);
  end;
end;

initialization
  // Nada que inicializar

finalization
  // Liberar singleton del thread principal para evitar memory leak
  GSpecContext := nil;

end.
