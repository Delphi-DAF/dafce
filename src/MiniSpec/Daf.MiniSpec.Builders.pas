unit Daf.MiniSpec.Builders;

interface
uses
  System.Generics.Collections,
  System.Rtti,
  System.SysUtils,
  System.TypInfo,
  Daf.MiniSpec.DataTable,
  Daf.MiniSpec.Types,
  Daf.MiniSpec.Binding;

type
  TRuleBuilder<T: class, constructor> = class;  // forward declaration

  TFeatureBuilder<T: class, constructor> = class(TInterfacedObject, IFeatureBuilder<T>)
  strict private
    FFeature: TFeature<T>;
  public
    constructor Create(const Feature: TFeature<T>);overload;
    constructor Create(const Description: string; const Category: string = ''; const Suite: ISpecSuite = nil);overload;
    function Category(const Name: string): IFeatureBuilder<T>; overload;
    function Category(AClass: TClass): IFeatureBuilder<T>; overload;
    function Before(const Description: string; Hook: THookProc): IFeatureBuilder<T>;
    function After(const Description: string; Hook: THookProc): IFeatureBuilder<T>;
    function Background: IBackgroundBuilder<T>;
    function Scenario(const Description: string): IScenarioBuilder<T>;overload;
    function ScenarioOutline(const Description: string): IScenarioOutlineBuilder<T>;
    function Rule(const Description: string): IRuleBuilder<T>;
    property Feature: TFeature<T> read FFeature;  // Para configuración desde TFeatureBuilder
  end;

  TFeatureBuilder = class
  strict private
    FDescription: string;
    FCategory: string;
    /// La suite destino. Sin ella la feature va a la del proceso, que es lo que hace cualquier
    /// fichero de specs; con ella, a la de una ejecucion anidada.
    FSuite: ISpecSuite;
    FContextCreator: TFunc<TObject>;  // Creador del FeatureContext (si se usa UseFeatureContext)
  public
    constructor Create(const Description: string; const Suite: ISpecSuite = nil);
    function Category(AClass: TClass): TFeatureBuilder;
    /// <summary>Define un contexto compartido para toda la Feature. Debe invocarse antes de UseWorld.</summary>
    function UseFeatureContext<T: class, constructor>: TFeatureBuilder;
    /// <summary>Define el tipo de contexto (World) para los escenarios de esta Feature</summary>
    function UseWorld<T: class, constructor>: IFeatureBuilder<T>;
  end;

  TBackgroundBuilder<T: class, constructor> = class(TInterfacedObject, IBackgroundBuilder<T>)
  strict private
    FBackground: TBackground<T>;
    FRule: TRule<T>;  // Siempre es una Rule (explícita o implícita)
    class procedure NoActionStep(World: T); static;
    function CreateBindingStep(Kind: TStepKind; const Desc: string): TStepProc<T>;
  public
    constructor Create(const ARule: IRule);
    function Given(const Desc: string): IBackgroundBuilder<T>; overload;
    function Given(const Desc: string; Step: TStepProc<T>): IBackgroundBuilder<T>; overload;
    function Given(const Desc: string; const Table: TDataTable): IBackgroundBuilder<T>; overload;
    function Given(const Desc: string; const Table: TDataTable; Step: TStepProc<T>): IBackgroundBuilder<T>; overload;
    function &And(const Desc: string): IBackgroundBuilder<T>; overload;
    function &And(const Desc: string; Step: TStepProc<T>): IBackgroundBuilder<T>; overload;
    function But(const Desc: string): IBackgroundBuilder<T>; overload;
    function But(const Desc: string; Step: TStepProc<T>): IBackgroundBuilder<T>; overload;
    function Pending: IBackgroundBuilder<T>;
    function NoAction: IBackgroundBuilder<T>;
    function Scenario(const Description: string): IScenarioBuilder<T>;overload;
    function ScenarioOutline(const Description: string): IScenarioOutlineBuilder<T>;
    function Rule(const Description: string): IRuleBuilder<T>;
  end;

  TScenarioBuilder<T: class, constructor> = class(TInterfacedObject, IScenarioBuilder<T>)
  strict private
    FScenario: TScenario<T>;
    FLastStep: TLastStepKind;
    FLastTable: TDataTable;
    FRule: TRule<T>;  // Siempre es una Rule (explícita o implícita)
    class procedure NoActionStep(World: T); static;
  private
    class function CreateBindingStep(Kind: TStepKind; const Desc: string): TStepProc<T>; static;
  public
    constructor Create(const ARule: IRule; const Description: string);
    function ExampleInit(Step: TStepProc<T>): IScenarioBuilder<T>;
    procedure SetExampleMeta(const Meta: TExampleMeta);
    function Given(const Desc: string): IScenarioBuilder<T>; overload;
    function Given(const Desc: string; Step: TStepProc<T>): IScenarioBuilder<T>; overload;
    function Given(const Desc: string; const Table: TDataTable): IScenarioBuilder<T>; overload;
    function Given(const Desc: string; const Table: TDataTable; Step: TStepProc<T>): IScenarioBuilder<T>; overload;
    function When(const Desc: string): IScenarioBuilder<T>; overload;
    function When(const Desc: string; Step: TStepProc<T>): IScenarioBuilder<T>; overload;
    function When(const Desc: string; const Table: TDataTable; Step: TStepProc<T>): IScenarioBuilder<T>; overload;
    function &Then(const Desc: string): IScenarioBuilder<T>; overload;
    function &Then(const Desc: string; Step: TStepProc<T>) : IScenarioBuilder<T>; overload;
    function &Then(const Desc: string; const Table: TDataTable; Step: TStepProc<T>): IScenarioBuilder<T>; overload;
    function &And(const Desc: string): IScenarioBuilder<T>; overload;
    function &And(const Desc: string; Step: TStepProc<T>): IScenarioBuilder<T>; overload;
    function &And(const Desc: string; const Table: TDataTable; Step: TStepProc<T>): IScenarioBuilder<T>; overload;
    function But(const Desc: string): IScenarioBuilder<T>; overload;
    function But(const Desc: string; Step: TStepProc<T>): IScenarioBuilder<T>; overload;
    function But(const Desc: string; const Table: TDataTable; Step: TStepProc<T>): IScenarioBuilder<T>; overload;
    function Pending: IScenarioBuilder<T>;
    function NoAction: IScenarioBuilder<T>;

    function Scenario(const Description: string): IScenarioBuilder<T>;overload;
    function ScenarioOutline(const Description: string): IScenarioOutlineBuilder<T>;
    function Rule(const Description: string): IRuleBuilder<T>;
  end;

  TScenarioOutlineBuilder<T: class, constructor> = class(TinterfacedObject, IScenarioOutlineBuilder<T>)
  strict private
    FRule: TRule<T>;  // Siempre es una Rule (explícita o implícita)
    FDescription: string;
    FStepsGiven: TList<TScenarioStep<T>>;
    FStepsWhen: TList<TScenarioStep<T>>;
    FStepsThen: TList<TScenarioStep<T>>;
    FLastStep: TLastStepKind;
    // Registrado en la regla desde que se declara: sin Examples se ejecuta como un escenario. Lo
    // posee la regla.
    FOutline: TScenarioOutline<T>;
    procedure AddToOutline(Kind: TStepKind; const Desc: string; Step: TStepProc<T>; const Table: TDataTable);
    function OutlineSteps(Kind: TLastStepKind): TList<IScenarioStep>;
    function BuildInitStep(Headers, Row: TArray<TValue>): TStepProc<T>;
    function SubstitutePlaceholders(const Template: string;
      const Headers: TArray<string>; const Row: TArray<TValue>): string;
    procedure CopyStepsToExample(const Example: TScenario<T>;
      const Steps: TList<TScenarioStep<T>>; Kind: TStepKind;
      const HeaderStrings: TArray<string>; const CurrentRow: TArray<TValue>);
    function GetFeature: TFeature<T>;
    class procedure NoActionStep(World: T); static;
    function CreateBindingStep(Kind: TStepKind; const Desc: string): TStepProc<T>;
  public
    constructor Create(const ARule: IRule; const Desc: string);
    destructor Destroy; override;
    function Given(const Desc: string): IScenarioOutlineBuilder<T>; overload;
    function Given(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>; overload;
    function Given(const Desc: string; const Table: TDataTable): IScenarioOutlineBuilder<T>; overload;
    function Given(const Desc: string; const Table: TDataTable; Step: TStepProc<T>): IScenarioOutlineBuilder<T>; overload;
    function When(const Desc: string): IScenarioOutlineBuilder<T>; overload;
    function When(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>; overload;
    function When(const Desc: string; const Table: TDataTable; Step: TStepProc<T>): IScenarioOutlineBuilder<T>; overload;
    function &Then(const Desc: string): IScenarioOutlineBuilder<T>; overload;
    function &Then(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>; overload;
    function &Then(const Desc: string; const Table: TDataTable; Step: TStepProc<T>): IScenarioOutlineBuilder<T>; overload;
    function &And(const Desc: string): IScenarioOutlineBuilder<T>; overload;
    function &And(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>; overload;
    function &And(const Desc: string; const Table: TDataTable; Step: TStepProc<T>): IScenarioOutlineBuilder<T>; overload;
    function But(const Desc: string): IScenarioOutlineBuilder<T>; overload;
    function But(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>; overload;
    function But(const Desc: string; const Table: TDataTable; Step: TStepProc<T>): IScenarioOutlineBuilder<T>; overload;
    function Pending: IScenarioOutlineBuilder<T>;
    function NoAction: IScenarioOutlineBuilder<T>;
    function Examples(const Table: TExamplesTable): IRuleBuilder<T>;
    function Scenario(const Description: string): IScenarioBuilder<T>;
    function ScenarioOutline(const Description: string): IScenarioOutlineBuilder<T>;
    function Rule(const Description: string): IRuleBuilder<T>;
  end;

  /// <summary>
  /// Builder para construir Rules dentro de una Feature.
  /// Implementa IFeatureBuilder para poder ser retornado por Examples()
  /// </summary>
  TRuleBuilder<T: class, constructor> = class(TInterfacedObject, IRuleBuilder<T>, IFeatureBuilder<T>)
  strict private
    FRule: TRule<T>;
    FFeature: TFeature<T>;
  public
    constructor Create(const AFeature: TFeature<T>; const Description: string);overload;
    constructor Create(const ARule: TRule<T>);overload;  // Para continuar dentro de una Rule existente
    // IRuleBuilder + IFeatureBuilder
    function Category(const Name: string): IFeatureBuilder<T>; overload;
    function Category(AClass: TClass): IFeatureBuilder<T>; overload;
    function Before(const Description: string; Hook: THookProc): IFeatureBuilder<T>;
    function After(const Description: string; Hook: THookProc): IFeatureBuilder<T>;
    function Background: IBackgroundBuilder<T>;
    function Scenario(const Description: string): IScenarioBuilder<T>;
    function ScenarioOutline(const Description: string): IScenarioOutlineBuilder<T>;
    function Rule(const Description: string): IRuleBuilder<T>;  // Requerido por IFeatureBuilder
    function EndRule: IFeatureBuilder<T>;  // Vuelve a Feature (IRuleBuilder)
  end;

implementation

{ TFeatureBuilder<T> }

constructor TFeatureBuilder<T>.Create(const Feature: TFeature<T>);
begin
  inherited Create;
  FFeature := Feature;
end;

constructor TFeatureBuilder<T>.Create(const Description: string; const Category: string; const Suite: ISpecSuite);
begin
  inherited Create;
  if Assigned(Suite) then
    FFeature := TFeature<T>.Create(Description, Suite)
  else
    FFeature := TFeature<T>.Create(Description);
  if Category <> '' then
    FFeature.Category := Category;
end;

function TFeatureBuilder<T>.Category(const Name: string): IFeatureBuilder<T>;
begin
  FFeature.Category := Name;
  Result := Self;
end;

function TFeatureBuilder<T>.Category(AClass: TClass): IFeatureBuilder<T>;
var
  QualifiedName: string;
  DotPos: Integer;
begin
  // AClass.QualifiedClassName returns 'UnitName.TCategory'
  // Extract the part before the last dot
  QualifiedName := AClass.QualifiedClassName;
  DotPos := QualifiedName.LastIndexOf('.');
  if DotPos > 0 then
    FFeature.Category := QualifiedName.Substring(0, DotPos)
  else
    FFeature.Category := QualifiedName;
  Result := Self;
end;

function TFeatureBuilder<T>.Before(const Description: string; Hook: THookProc): IFeatureBuilder<T>;
begin
  FFeature.BeforeHooks.Add(THook.Create(sikBefore, FFeature, Description, Hook));
  Result := Self;
end;

function TFeatureBuilder<T>.After(const Description: string; Hook: THookProc): IFeatureBuilder<T>;
begin
  FFeature.AfterHooks.Add(THook.Create(sikAfter, FFeature, Description, Hook));
  Result := Self;
end;

function TFeatureBuilder<T>.Background: IBackgroundBuilder<T>;
begin
  // Delegar a la Rule implícita
  Result := TBackgroundBuilder<T>.Create(FFeature.ImplicitRule);
end;

function TFeatureBuilder<T>.Scenario(const Description: string): IScenarioBuilder<T>;
begin
  // Delegar a la Rule implícita
  Result := TScenarioBuilder<T>.Create(FFeature.ImplicitRule, Description);
end;

function TFeatureBuilder<T>.ScenarioOutline(const Description: string): IScenarioOutlineBuilder<T>;
begin
  // Delegar a la Rule implícita
  Result := TScenarioOutlineBuilder<T>.Create(FFeature.ImplicitRule, Description);
end;

function TFeatureBuilder<T>.Rule(const Description: string): IRuleBuilder<T>;
begin
  Result := TRuleBuilder<T>.Create(FFeature, Description);
end;

{ TFeatureBuilder }

constructor TFeatureBuilder.Create(const Description: string; const Suite: ISpecSuite);
begin
  inherited Create;
  FDescription := Description;
  FCategory := '';
  FSuite := Suite;
end;

function TFeatureBuilder.Category(AClass: TClass): TFeatureBuilder;
var
  QualifiedName: string;
  DotPos: Integer;
begin
  // AClass.QualifiedClassName returns 'UnitName.TCategory'
  // Extract the part before the last dot
  QualifiedName := AClass.QualifiedClassName;
  DotPos := QualifiedName.LastIndexOf('.');
  if DotPos > 0 then
    FCategory := QualifiedName.Substring(0, DotPos)
  else
    FCategory := QualifiedName;
  Result := Self;
end;

function TFeatureBuilder.UseFeatureContext<T>: TFeatureBuilder;
begin
  FContextCreator := function: TObject
    begin
      Result := T.Create;
    end;
  Result := Self;
end;

function TFeatureBuilder.UseWorld<T>: IFeatureBuilder<T>;
var
  Builder: TFeatureBuilder<T>;
begin
  Builder := TFeatureBuilder<T>.Create(FDescription, FCategory, FSuite);
  // Si se configuró un FeatureContext, pasarlo a la Feature
  if Assigned(FContextCreator) then
    Builder.Feature.SetContextCreator(FContextCreator);
  Result := Builder;
  Free;
end;

{ TBackgroundBuilder<T> }

constructor TBackgroundBuilder<T>.Create(const ARule: IRule);
begin
  inherited Create;
  FRule := TRule<T>(ARule);
  FBackground := TBackground<T>.Create(FRule.Feature);
  FRule.BackGround := FBackground;
end;

class procedure TBackgroundBuilder<T>.NoActionStep(World: T);
begin
  // Intentionally empty - step passes without doing anything
end;

function TBackgroundBuilder<T>.CreateBindingStep(Kind: TStepKind; const Desc: string): TStepProc<T>;
var
  Binding: TStepBinding;
  Captures: TArray<string>;
begin
  if Bindings.FindBinding(Kind, Desc, Binding, Captures) then
    Result := procedure(World: T)
      begin
        Bindings.Invoke(Binding, World, Captures);
      end
  else
    Result := procedure(World: T)
      var
        LBinding: TStepBinding;
        LCaptures: TArray<string>;
      begin
        if Bindings.FindBinding(Kind, Desc, LBinding, LCaptures) then
          Bindings.Invoke(LBinding, World, LCaptures)
        else
          SpecContext.Step.MarkAsUndefined;
      end;
end;

function TBackgroundBuilder<T>.Given(const Desc: string): IBackgroundBuilder<T>;
begin
  Result := Given(Desc, CreateBindingStep(skGiven, Desc));
end;

function TBackgroundBuilder<T>.Given(const Desc: string; Step: TStepProc<T>): IBackgroundBuilder<T>;
begin
  FBackground.Given(Desc, Step);
  Result := Self;
end;

function TBackgroundBuilder<T>.Given(const Desc: string; const Table: TDataTable): IBackgroundBuilder<T>;
begin
  Result := Given(Desc, Table, CreateBindingStep(skGiven, Desc));
end;

function TBackgroundBuilder<T>.Given(const Desc: string; const Table: TDataTable; Step: TStepProc<T>): IBackgroundBuilder<T>;
begin
  FBackground.Given(Desc, Table, Step);
  Result := Self;
end;

function TBackgroundBuilder<T>.&And(const Desc: string): IBackgroundBuilder<T>;
begin
  Result := Given(Desc);
end;

function TBackgroundBuilder<T>.&And(const Desc: string; Step: TStepProc<T>): IBackgroundBuilder<T>;
begin
  FBackground.Given(Desc, Step);
  Result := Self;
end;

function TBackgroundBuilder<T>.But(const Desc: string): IBackgroundBuilder<T>;
begin
  Result := Given(Desc);
end;

function TBackgroundBuilder<T>.But(const Desc: string; Step: TStepProc<T>): IBackgroundBuilder<T>;
begin
  FBackground.Given(Desc, Step);
  Result := Self;
end;

function TBackgroundBuilder<T>.Pending: IBackgroundBuilder<T>;
begin
  if FBackground.StepsGiven.Count > 0 then
    TScenarioStep<T>(FBackground.StepsGiven.Last).DeclarePending;
  Result := Self;
end;

function TBackgroundBuilder<T>.NoAction: IBackgroundBuilder<T>;
begin
  if FBackground.StepsGiven.Count > 0 then
    TScenarioStep<T>(FBackground.StepsGiven.Last).SetProc(NoActionStep);
  Result := Self;
end;

function TBackgroundBuilder<T>.Scenario(const Description: string): IScenarioBuilder<T>;
begin
  Result := TScenarioBuilder<T>.Create(FRule, Description);
end;

function TBackgroundBuilder<T>.ScenarioOutline(const Description: string): IScenarioOutlineBuilder<T>;
begin
  Result := TScenarioOutlineBuilder<T>.Create(FRule, Description);
end;

function TBackgroundBuilder<T>.Rule(const Description: string): IRuleBuilder<T>;
begin
  Result := TRuleBuilder<T>.Create(FRule.Feature as TFeature<T>, Description);
end;

{ TScenarioBuilder<T> }

constructor TScenarioBuilder<T>.Create(const ARule: IRule; const Description: string);
begin
  inherited Create;
  FRule := TRule<T>(ARule);
  FLastStep := lskNone;
  // Crear escenario con Rule como parent para correcta navegación del contexto
  FScenario := TScenario<T>.Create(ARule, Description);
  FRule.Scenarios.Add(FScenario);
end;

class procedure TScenarioBuilder<T>.NoActionStep(World: T);
begin
  // Intentionally empty - step passes without doing anything
end;

class function TScenarioBuilder<T>.CreateBindingStep(Kind: TStepKind; const Desc: string): TStepProc<T>;
var
  Binding: TStepBinding;
  Captures: TArray<string>;
begin
  if Bindings.FindBinding(Kind, Desc, Binding, Captures) then
    Result := procedure(World: T)
      begin
        Bindings.Invoke(Binding, World, Captures);
      end
  else
    // Binding not yet registered (Steps unit initialises after Feat due to
    // unit dependency).  Defer resolution to run time so the fully populated
    // binding table is always consulted when the suite executes.
    Result := procedure(World: T)
      var
        LBinding: TStepBinding;
        LCaptures: TArray<string>;
      begin
        if Bindings.FindBinding(Kind, Desc, LBinding, LCaptures) then
          Bindings.Invoke(LBinding, World, LCaptures)
        else
          SpecContext.Step.MarkAsUndefined;
      end;
end;

function TScenarioBuilder<T>.ExampleInit(Step: TStepProc<T>): IScenarioBuilder<T>;
begin
  FScenario.ExampleInit(Step);
  Result := Self;
end;

procedure TScenarioBuilder<T>.SetExampleMeta(const Meta: TExampleMeta);
begin
  FScenario.SetExampleMeta(Meta);
end;

function TScenarioBuilder<T>.Given(const Desc: string): IScenarioBuilder<T>;
begin
  Result := Given(Desc, CreateBindingStep(skGiven, Desc));
end;

function TScenarioBuilder<T>.Given(const Desc: string; Step: TStepProc<T>): IScenarioBuilder<T>;
begin
  FScenario.Given(Desc, Step);
  FLastStep := lskGiven;
  FLastTable := nil;
  Result := Self;
end;

function TScenarioBuilder<T>.Given(const Desc: string; const Table: TDataTable): IScenarioBuilder<T>;
begin
  Result := Given(Desc, Table, CreateBindingStep(skGiven, Desc));
end;

function TScenarioBuilder<T>.Given(const Desc: string; const Table: TDataTable; Step: TStepProc<T>): IScenarioBuilder<T>;
begin
  FScenario.Given(Desc, Table, Step);
  FLastStep := lskGiven;
  FLastTable := Table;
  Result := Self;
end;

function TScenarioBuilder<T>.When(const Desc: string): IScenarioBuilder<T>;
begin
  Result := When(Desc, CreateBindingStep(skWhen, Desc));
end;

function TScenarioBuilder<T>.When(const Desc: string; Step: TStepProc<T>): IScenarioBuilder<T>;
begin
  FScenario.When(Desc, Step);
  FLastStep := lskWhen;
  FLastTable := nil;
  Result := Self;
end;

function TScenarioBuilder<T>.When(const Desc: string; const Table: TDataTable; Step: TStepProc<T>): IScenarioBuilder<T>;
begin
  FScenario.When(Desc, Table, Step);
  FLastStep := lskWhen;
  FLastTable := Table;
  Result := Self;
end;

function TScenarioBuilder<T>.&Then(const Desc: string): IScenarioBuilder<T>;
begin
  Result := &Then(Desc, CreateBindingStep(skThen, Desc));
end;

function TScenarioBuilder<T>.&Then(const Desc: string; Step: TStepProc<T>): IScenarioBuilder<T>;
begin
  FScenario.&Then(Desc, Step);
  FLastStep := lskThen;
  FLastTable := nil;
  Result := Self;
end;

function TScenarioBuilder<T>.&Then(const Desc: string; const Table: TDataTable; Step: TStepProc<T>): IScenarioBuilder<T>;
begin
  FScenario.&Then(Desc, Table, Step);
  FLastStep := lskThen;
  FLastTable := Table;
  Result := Self;
end;

function TScenarioBuilder<T>.&And(const Desc: string): IScenarioBuilder<T>;
var
  Kind: TStepKind;
begin
  case FLastStep of
    lskGiven: Kind := skGiven;
    lskWhen:  Kind := skWhen;
    lskThen:  Kind := skThen;
  else
    raise Exception.Create('And must follow Given, When or Then');
  end;
  Result := &And(Desc, CreateBindingStep(Kind, Desc));
end;

function TScenarioBuilder<T>.&And(const Desc: string; Step: TStepProc<T>): IScenarioBuilder<T>;
begin
  case FLastStep of
    lskGiven: FScenario.Given(Desc, Step);
    lskWhen:  FScenario.When(Desc, Step);
    lskThen:  FScenario.&Then(Desc, Step);
  else
    raise Exception.Create('And must follow Given, When or Then');
  end;
  Result := Self;
end;

function TScenarioBuilder<T>.&And(const Desc: string; const Table: TDataTable; Step: TStepProc<T>): IScenarioBuilder<T>;
begin
  case FLastStep of
    lskGiven: FScenario.Given(Desc, Table, Step);
    lskWhen:  FScenario.When(Desc, Table, Step);
    lskThen:  FScenario.&Then(Desc, Table, Step);
  else
    raise Exception.Create('And must follow Given, When or Then');
  end;
  Result := Self;
end;

function TScenarioBuilder<T>.But(const Desc: string): IScenarioBuilder<T>;
var
  Kind: TStepKind;
begin
  case FLastStep of
    lskGiven: Kind := skGiven;
    lskWhen:  Kind := skWhen;
    lskThen:  Kind := skThen;
  else
    raise Exception.Create('But must follow Given, When or Then');
  end;
  Result := But(Desc, CreateBindingStep(Kind, Desc));
end;

function TScenarioBuilder<T>.But(const Desc: string; Step: TStepProc<T>): IScenarioBuilder<T>;
begin
  case FLastStep of
    lskGiven: FScenario.Given(Desc, Step);
    lskWhen:  FScenario.When(Desc, Step);
    lskThen:  FScenario.&Then(Desc, Step);
  else
    raise Exception.Create('But must follow Given, When or Then');
  end;
  Result := Self;
end;

function TScenarioBuilder<T>.But(const Desc: string; const Table: TDataTable; Step: TStepProc<T>): IScenarioBuilder<T>;
begin
  case FLastStep of
    lskGiven: FScenario.Given(Desc, Table, Step);
    lskWhen:  FScenario.When(Desc, Table, Step);
    lskThen:  FScenario.&Then(Desc, Table, Step);
  else
    raise Exception.Create('But must follow Given, When or Then');
  end;
  Result := Self;
end;

function TScenarioBuilder<T>.Pending: IScenarioBuilder<T>;
begin
  case FLastStep of
    lskGiven: if FScenario.StepsGiven.Count > 0 then
                TScenarioStep<T>(FScenario.StepsGiven.Last).DeclarePending;
    lskWhen:  if FScenario.StepsWhen.Count > 0 then
                TScenarioStep<T>(FScenario.StepsWhen.Last).DeclarePending;
    lskThen:  if FScenario.StepsThen.Count > 0 then
                TScenarioStep<T>(FScenario.StepsThen.Last).DeclarePending;
  end;
  Result := Self;
end;

function TScenarioBuilder<T>.NoAction: IScenarioBuilder<T>;
begin
  case FLastStep of
    lskGiven: if FScenario.StepsGiven.Count > 0 then
                TScenarioStep<T>(FScenario.StepsGiven.Last).SetProc(NoActionStep);
    lskWhen:  if FScenario.StepsWhen.Count > 0 then
                TScenarioStep<T>(FScenario.StepsWhen.Last).SetProc(NoActionStep);
    lskThen:  if FScenario.StepsThen.Count > 0 then
                TScenarioStep<T>(FScenario.StepsThen.Last).SetProc(NoActionStep);
  end;
  Result := Self;
end;

function TScenarioBuilder<T>.Scenario(const Description: string): IScenarioBuilder<T>;
begin
  Result := TScenarioBuilder<T>.Create(FRule, Description);
end;

function TScenarioBuilder<T>.ScenarioOutline(const Description: string): IScenarioOutlineBuilder<T>;
begin
  Result := TScenarioOutlineBuilder<T>.Create(FRule, Description);
end;

function TScenarioBuilder<T>.Rule(const Description: string): IRuleBuilder<T>;
begin
  Result := TRuleBuilder<T>.Create(FRule.Feature as TFeature<T>, Description);
end;

{ TScenarioOutlineBuilder<T> }

constructor TScenarioOutlineBuilder<T>.Create(const ARule: IRule; const Desc: string);
begin
  FDescription := Desc;
  FRule := TRule<T>(ARule);
  FStepsGiven := TObjectList<TScenarioStep<T>>.Create;
  FStepsWhen := TObjectList<TScenarioStep<T>>.Create;
  FStepsThen := TObjectList<TScenarioStep<T>>.Create;
  FLastStep := lskNone;
  FOutline := TScenarioOutline<T>.Create(FRule, FDescription, nil);
end;

procedure TScenarioOutlineBuilder<T>.AddToOutline(Kind: TStepKind; const Desc: string; Step: TStepProc<T>;
  const Table: TDataTable);
begin
  // La copia del Outline es la que se ejecuta cuando no hay filas: un step sin lambda resuelve su
  // binding como en un Scenario.
  var Proc := Step;
  if not Assigned(Proc) then
    Proc := TScenarioBuilder<T>.CreateBindingStep(Kind, Desc);
  case Kind of
    skGiven: FOutline.Given(Desc, Table, Proc);
    skWhen:  FOutline.When(Desc, Table, Proc);
    skThen:  FOutline.&Then(Desc, Table, Proc);
  end;
end;

function TScenarioOutlineBuilder<T>.OutlineSteps(Kind: TLastStepKind): TList<IScenarioStep>;
begin
  case Kind of
    lskGiven: Result := FOutline.StepsGiven;
    lskWhen:  Result := FOutline.StepsWhen;
    lskThen:  Result := FOutline.StepsThen;
  else
    Result := nil;
  end;
end;

function TScenarioOutlineBuilder<T>.GetFeature: TFeature<T>;
begin
  Result := FRule.Feature as TFeature<T>;
end;

destructor TScenarioOutlineBuilder<T>.Destroy;
begin
  FStepsGiven.Free;
  FStepsWhen.Free;
  FStepsThen.Free;
  inherited;
end;

class procedure TScenarioOutlineBuilder<T>.NoActionStep(World: T);
begin
  // Intentionally empty - step passes without doing anything
end;

function TScenarioOutlineBuilder<T>.CreateBindingStep(Kind: TStepKind; const Desc: string): TStepProc<T>;
var
  Binding: TStepBinding;
  Captures: TArray<string>;
begin
  if Bindings.FindBinding(Kind, Desc, Binding, Captures) then
    Result := procedure(World: T)
      begin
        Bindings.Invoke(Binding, World, Captures);
      end
  else
    // No binding found: treat as display-only no-op.
    // ExampleInit already injects placeholder values into the World via RTTI;
    // a nil-proc step in an outline is not a missing definition.
    Result := procedure(World: T)
      var
        LBinding: TStepBinding;
        LCaptures: TArray<string>;
      begin
        if Bindings.FindBinding(Kind, Desc, LBinding, LCaptures) then
          Bindings.Invoke(LBinding, World, LCaptures);
      end;
end;

function TScenarioOutlineBuilder<T>.Given(const Desc: string): IScenarioOutlineBuilder<T>;
begin
  // In ScenarioOutline, Given without lambda stores nil (values come from Examples)
  Result := Given(Desc, nil);
end;

function TScenarioOutlineBuilder<T>.Given(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>;
begin
  FStepsGiven.Add(TScenarioStep<T>.Create(sikGiven, nil, Desc, Step));
  AddToOutline(skGiven, Desc, Step, nil);
  FLastStep := lskGiven;
  Result := Self;
end;

function TScenarioOutlineBuilder<T>.Given(const Desc: string; const Table: TDataTable): IScenarioOutlineBuilder<T>;
begin
  Result := Given(Desc, Table, nil);
end;

function TScenarioOutlineBuilder<T>.Given(const Desc: string; const Table: TDataTable; Step: TStepProc<T>): IScenarioOutlineBuilder<T>;
begin
  FStepsGiven.Add(TScenarioStep<T>.Create(sikGiven, nil, Desc, Step, Table));
  AddToOutline(skGiven, Desc, Step, Table);
  FLastStep := lskGiven;
  Result := Self;
end;

function TScenarioOutlineBuilder<T>.When(const Desc: string): IScenarioOutlineBuilder<T>;
begin
  // Defer binding resolution to Examples() where placeholders are substituted
  Result := When(Desc, nil);
end;

function TScenarioOutlineBuilder<T>.When(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>;
begin
  FStepsWhen.Add(TScenarioStep<T>.Create(sikWhen, nil, Desc, Step));
  AddToOutline(skWhen, Desc, Step, nil);
  FLastStep := lskWhen;
  Result := Self;
end;

function TScenarioOutlineBuilder<T>.When(const Desc: string; const Table: TDataTable; Step: TStepProc<T>): IScenarioOutlineBuilder<T>;
begin
  FStepsWhen.Add(TScenarioStep<T>.Create(sikWhen, nil, Desc, Step, Table));
  AddToOutline(skWhen, Desc, Step, Table);
  FLastStep := lskWhen;
  Result := Self;
end;

function TScenarioOutlineBuilder<T>.&Then(const Desc: string): IScenarioOutlineBuilder<T>;
begin
  // Defer binding resolution to Examples() where placeholders are substituted
  Result := &Then(Desc, nil);
end;

function TScenarioOutlineBuilder<T>.&Then(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>;
begin
  FStepsThen.Add(TScenarioStep<T>.Create(sikThen, nil, Desc, Step));
  AddToOutline(skThen, Desc, Step, nil);
  FLastStep := lskThen;
  Result := Self;
end;

function TScenarioOutlineBuilder<T>.&Then(const Desc: string; const Table: TDataTable; Step: TStepProc<T>): IScenarioOutlineBuilder<T>;
begin
  FStepsThen.Add(TScenarioStep<T>.Create(sikThen, nil, Desc, Step, Table));
  AddToOutline(skThen, Desc, Step, Table);
  FLastStep := lskThen;
  Result := Self;
end;

function TScenarioOutlineBuilder<T>.&And(const Desc: string): IScenarioOutlineBuilder<T>;
begin
  // Defer binding resolution to Examples() where placeholders are substituted
  Result := &And(Desc, nil);
end;

function TScenarioOutlineBuilder<T>.&And(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>;
begin
  case FLastStep of
    lskGiven: begin
                FStepsGiven.Add(TScenarioStep<T>.Create(sikAnd, nil, Desc, Step));
                AddToOutline(skGiven, Desc, Step, nil);
              end;
    lskWhen:  begin
                FStepsWhen.Add(TScenarioStep<T>.Create(sikAnd, nil, Desc, Step));
                AddToOutline(skWhen, Desc, Step, nil);
              end;
    lskThen:  begin
                FStepsThen.Add(TScenarioStep<T>.Create(sikAnd, nil, Desc, Step));
                AddToOutline(skThen, Desc, Step, nil);
              end;
  else
    raise Exception.Create('And must follow Given, When or Then');
  end;
  Result := Self;
end;

function TScenarioOutlineBuilder<T>.&And(const Desc: string; const Table: TDataTable; Step: TStepProc<T>): IScenarioOutlineBuilder<T>;
begin
  case FLastStep of
    lskGiven: begin
                FStepsGiven.Add(TScenarioStep<T>.Create(sikAnd, nil, Desc, Step, Table));
                AddToOutline(skGiven, Desc, Step, Table);
              end;
    lskWhen:  begin
                FStepsWhen.Add(TScenarioStep<T>.Create(sikAnd, nil, Desc, Step, Table));
                AddToOutline(skWhen, Desc, Step, Table);
              end;
    lskThen:  begin
                FStepsThen.Add(TScenarioStep<T>.Create(sikAnd, nil, Desc, Step, Table));
                AddToOutline(skThen, Desc, Step, Table);
              end;
  else
    raise Exception.Create('And must follow Given, When or Then');
  end;
  Result := Self;
end;

function TScenarioOutlineBuilder<T>.But(const Desc: string): IScenarioOutlineBuilder<T>;
begin
  // Defer binding resolution to Examples() where placeholders are substituted
  Result := But(Desc, nil);
end;

function TScenarioOutlineBuilder<T>.But(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>;
begin
  case FLastStep of
    lskGiven: begin
                FStepsGiven.Add(TScenarioStep<T>.Create(sikBut, nil, Desc, Step));
                AddToOutline(skGiven, Desc, Step, nil);
              end;
    lskWhen:  begin
                FStepsWhen.Add(TScenarioStep<T>.Create(sikBut, nil, Desc, Step));
                AddToOutline(skWhen, Desc, Step, nil);
              end;
    lskThen:  begin
                FStepsThen.Add(TScenarioStep<T>.Create(sikBut, nil, Desc, Step));
                AddToOutline(skThen, Desc, Step, nil);
              end;
  else
    raise Exception.Create('But must follow Given, When or Then');
  end;
  Result := Self;
end;

function TScenarioOutlineBuilder<T>.But(const Desc: string; const Table: TDataTable; Step: TStepProc<T>): IScenarioOutlineBuilder<T>;
begin
  case FLastStep of
    lskGiven: begin
                FStepsGiven.Add(TScenarioStep<T>.Create(sikBut, nil, Desc, Step, Table));
                AddToOutline(skGiven, Desc, Step, Table);
              end;
    lskWhen:  begin
                FStepsWhen.Add(TScenarioStep<T>.Create(sikBut, nil, Desc, Step, Table));
                AddToOutline(skWhen, Desc, Step, Table);
              end;
    lskThen:  begin
                FStepsThen.Add(TScenarioStep<T>.Create(sikBut, nil, Desc, Step, Table));
                AddToOutline(skThen, Desc, Step, Table);
              end;
  else
    raise Exception.Create('But must follow Given, When or Then');
  end;
  Result := Self;
end;

function TScenarioOutlineBuilder<T>.Pending: IScenarioOutlineBuilder<T>;
begin
  case FLastStep of
    lskGiven: if FStepsGiven.Count > 0 then
                FStepsGiven.Last.DeclarePending;
    lskWhen:  if FStepsWhen.Count > 0 then
                FStepsWhen.Last.DeclarePending;
    lskThen:  if FStepsThen.Count > 0 then
                FStepsThen.Last.DeclarePending;
  end;
  var Steps := OutlineSteps(FLastStep);
  if Assigned(Steps) and (Steps.Count > 0) then
    TScenarioStep<T>(Steps.Last).DeclarePending;
  Result := Self;
end;

function TScenarioOutlineBuilder<T>.NoAction: IScenarioOutlineBuilder<T>;
begin
  case FLastStep of
    lskGiven: if FStepsGiven.Count > 0 then
                FStepsGiven.Last.SetProc(NoActionStep);
    lskWhen:  if FStepsWhen.Count > 0 then
                FStepsWhen.Last.SetProc(NoActionStep);
    lskThen:  if FStepsThen.Count > 0 then
                FStepsThen.Last.SetProc(NoActionStep);
  end;
  var Steps := OutlineSteps(FLastStep);
  if Assigned(Steps) and (Steps.Count > 0) then
    TScenarioStep<T>(Steps.Last).SetProc(NoActionStep);
  Result := Self;
end;

function TScenarioOutlineBuilder<T>.BuildInitStep(Headers, Row: TArray<TValue>) : TStepProc<T>;
begin
  var RttiCtx: TRttiContext;
  var RttiType := RttiCtx.GetType(TypeInfo(T)) as TRttiInstanceType;
  Result := procedure(World: T)
    begin
      for var k := 0 to High(Headers) do
      begin
        var FieldName := Headers[k].AsString;
        var Field := RttiType.GetField(FieldName);
        var Value := Row[k];
        if Assigned(Field) then
          Field.SetValue(TObject(World), Value);
      end;
    end;
end;

function TScenarioOutlineBuilder<T>.SubstitutePlaceholders(
  const Template: string; const Headers: TArray<string>;
  const Row: TArray<TValue>): string;
begin
  Result := Template;
  for var k := 0 to High(Headers) do
    Result := StringReplace(Result, '<' + Headers[k] + '>',
      Row[k].ToString, [rfReplaceAll]);
end;

procedure TScenarioOutlineBuilder<T>.CopyStepsToExample(
  const Example: TScenario<T>; const Steps: TList<TScenarioStep<T>>;
  Kind: TStepKind; const HeaderStrings: TArray<string>;
  const CurrentRow: TArray<TValue>);
begin
  for var Step in Steps do
  begin
    var Desc := SubstitutePlaceholders(Step.Description, HeaderStrings, CurrentRow);
    var Proc := Step.Proc;
    if not Assigned(Proc) then
      Proc := CreateBindingStep(Kind, Desc);
    if Assigned(Step.DataTable) then
    begin
      case Kind of
        skGiven: Example.Given(Desc, Step.DataTable.Raw, Proc);
        skWhen:  Example.When(Desc, Step.DataTable.Raw, Proc);
        skThen:  Example.&Then(Desc, Step.DataTable.Raw, Proc);
      end;
    end
    else
    begin
      case Kind of
        skGiven: Example.Given(Desc, Proc);
        skWhen:  Example.When(Desc, Proc);
        skThen:  Example.&Then(Desc, Proc);
      end;
    end;
    // Cada fila es un escenario con sus steps, y lo que declara la plantilla vale para todas.
    if Step.DeclaredPending then
      case Kind of
        skGiven: TScenarioStep<T>(Example.StepsGiven.Last).DeclarePending;
        skWhen:  TScenarioStep<T>(Example.StepsWhen.Last).DeclarePending;
        skThen:  TScenarioStep<T>(Example.StepsThen.Last).DeclarePending;
      end;
  end;
end;

function TScenarioOutlineBuilder<T>.Examples(const Table: TExamplesTable): IRuleBuilder<T>;
begin
  if Length(Table) < 2 then
    raise Exception.Create('Examples must include headers and at least one data row.');

  var Headers := Table[0];
  // Construir array de strings para headers
  var HeaderStrings: TArray<string>;
  SetLength(HeaderStrings, Length(Headers));
  for var i := 0 to High(Headers) do
    HeaderStrings[i] := Headers[i].AsString;

  FOutline.SetHeaders(HeaderStrings);

  for var RowIdx := 1 to High(Table) do
  begin
    var CurrentRow := Table[RowIdx];
    if Length(CurrentRow) <> Length(Headers) then
      raise Exception.CreateFmt('Row %d does not match header column count.', [RowIdx]);

    // Crear Example con el Outline como parent
    var Example := TScenario<T>.CreateExample(FOutline, FDescription);
    Example.ExampleInit(BuildInitStep(Headers, CurrentRow));

    // Asignar metadata simplificada
    var Meta: TExampleMeta;
    Meta.Values := CurrentRow;
    Meta.RowIndex := RowIdx;
    Example.SetExampleMeta(Meta);

    // Copiar los steps (cada Example tiene su propia copia)
    // Substitute placeholders and resolve bindings for steps without lambda
    CopyStepsToExample(Example, FStepsGiven, skGiven, HeaderStrings, CurrentRow);
    CopyStepsToExample(Example, FStepsWhen, skWhen, HeaderStrings, CurrentRow);
    CopyStepsToExample(Example, FStepsThen, skThen, HeaderStrings, CurrentRow);

    // Registrar el Example solo en el Outline (no en Rule.Scenarios)
    // El Outline ya está en Rule.Scenarios y ejecuta sus Examples
    FOutline.AddExample(Example);
  end;

  // Siempre devolver builder de la Rule actual
  Result := TRuleBuilder<T>.Create(FRule);
end;

function TScenarioOutlineBuilder<T>.Scenario(const Description: string): IScenarioBuilder<T>;
begin
  Result := TScenarioBuilder<T>.Create(FRule, Description);
end;

function TScenarioOutlineBuilder<T>.ScenarioOutline(const Description: string): IScenarioOutlineBuilder<T>;
begin
  Result := TScenarioOutlineBuilder<T>.Create(FRule, Description);
end;

function TScenarioOutlineBuilder<T>.Rule(const Description: string): IRuleBuilder<T>;
begin
  Result := TRuleBuilder<T>.Create(GetFeature, Description);
end;

{ TRuleBuilder<T> }

constructor TRuleBuilder<T>.Create(const AFeature: TFeature<T>; const Description: string);
begin
  inherited Create;
  FFeature := AFeature;
  FRule := TRule<T>.Create(AFeature, Description);
  AFeature.Rules.Add(FRule);
end;

constructor TRuleBuilder<T>.Create(const ARule: TRule<T>);
begin
  inherited Create;
  FRule := ARule;
  FFeature := ARule.Feature as TFeature<T>;
end;

function TRuleBuilder<T>.Category(const Name: string): IFeatureBuilder<T>;
begin
  FFeature.Category := Name;
  Result := Self;
end;

function TRuleBuilder<T>.Category(AClass: TClass): IFeatureBuilder<T>;
var
  QualifiedName: string;
  DotPos: Integer;
begin
  QualifiedName := AClass.QualifiedClassName;
  DotPos := QualifiedName.LastIndexOf('.');
  if DotPos > 0 then
    FFeature.Category := QualifiedName.Substring(0, DotPos)
  else
    FFeature.Category := QualifiedName;
  Result := Self;
end;

function TRuleBuilder<T>.Before(const Description: string; Hook: THookProc): IFeatureBuilder<T>;
begin
  FFeature.BeforeHooks.Add(THook.Create(sikBefore, FFeature, Description, Hook));
  Result := Self;
end;

function TRuleBuilder<T>.After(const Description: string; Hook: THookProc): IFeatureBuilder<T>;
begin
  FFeature.AfterHooks.Add(THook.Create(sikAfter, FFeature, Description, Hook));
  Result := Self;
end;

function TRuleBuilder<T>.Background: IBackgroundBuilder<T>;
begin
  Result := TBackgroundBuilder<T>.Create(FRule);
end;

function TRuleBuilder<T>.Scenario(const Description: string): IScenarioBuilder<T>;
begin
  Result := TScenarioBuilder<T>.Create(FRule, Description);
end;

function TRuleBuilder<T>.ScenarioOutline(const Description: string): IScenarioOutlineBuilder<T>;
begin
  Result := TScenarioOutlineBuilder<T>.Create(FRule, Description);
end;

function TRuleBuilder<T>.Rule(const Description: string): IRuleBuilder<T>;
begin
  // Nueva Rule hermana en la misma Feature (requerido por IFeatureBuilder)
  Result := TRuleBuilder<T>.Create(FFeature, Description);
end;

function TRuleBuilder<T>.EndRule: IFeatureBuilder<T>;
begin
  // Volver a Feature para añadir scenarios en ImplicitRule o nuevas Rules
  Result := TFeatureBuilder<T>.Create(FFeature);
end;

end.
