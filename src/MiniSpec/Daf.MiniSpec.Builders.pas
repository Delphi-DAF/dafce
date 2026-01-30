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
    constructor Create(const Description: string; const Category: string = '');overload;
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
    FContextCreator: TFunc<TObject>;  // Creador del FeatureContext (si se usa UseFeatureContext)
  public
    constructor Create(const Description: string);
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
    class procedure PendingStep(World: T); static;
  public
    constructor Create(const ARule: IRule);
    function Given(const Desc: string): IBackgroundBuilder<T>; overload;
    function Given(const Desc: string; Step: TStepProc<T>): IBackgroundBuilder<T>; overload;
    function &And(const Desc: string): IBackgroundBuilder<T>; overload;
    function &And(const Desc: string; Step: TStepProc<T>): IBackgroundBuilder<T>; overload;
    function But(const Desc: string): IBackgroundBuilder<T>; overload;
    function But(const Desc: string; Step: TStepProc<T>): IBackgroundBuilder<T>; overload;
    function Pending: IBackgroundBuilder<T>;
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
    class procedure PendingStep(World: T); static;
    function CreateBindingStep(Kind: TStepKind; const Desc: string): TStepProc<T>;
  public
    constructor Create(const ARule: IRule; const Description: string);
    function ExampleInit(Step: TStepProc<T>): IScenarioBuilder<T>;
    procedure SetExampleMeta(const Meta: TExampleMeta);
    function Given(const Desc: string): IScenarioBuilder<T>; overload;
    function Given(const Desc: string; Step: TStepProc<T>): IScenarioBuilder<T>; overload;
    function Given(const Desc: string; const Table: TDataTable; Step: TStepProc<T>): IScenarioBuilder<T>; overload;
    function When(const Desc: string): IScenarioBuilder<T>; overload;
    function When(const Desc: string; Step: TStepProc<T>) : IScenarioBuilder<T>; overload;
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
    function BuildInitStep(Headers, Row: TArray<TValue>): TStepProc<T>;
    function GetFeature: TFeature<T>;
    class procedure PendingStep(World: T); static;
    function CreateBindingStep(Kind: TStepKind; const Desc: string): TStepProc<T>;
  public
    constructor Create(const ARule: IRule; const Desc: string);
    destructor Destroy; override;
    function Given(const Desc: string): IScenarioOutlineBuilder<T>; overload;
    function Given(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>; overload;
    function When(const Desc: string): IScenarioOutlineBuilder<T>; overload;
    function When(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>; overload;
    function &Then(const Desc: string): IScenarioOutlineBuilder<T>; overload;
    function &Then(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>; overload;
    function &And(const Desc: string): IScenarioOutlineBuilder<T>; overload;
    function &And(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>; overload;
    function But(const Desc: string): IScenarioOutlineBuilder<T>; overload;
    function But(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>; overload;
    function Pending: IScenarioOutlineBuilder<T>;
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

constructor TFeatureBuilder<T>.Create(const Description: string; const Category: string);
begin
  inherited Create;
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

constructor TFeatureBuilder.Create(const Description: string);
begin
  inherited Create;
  FDescription := Description;
  FCategory := '';
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
  Builder := TFeatureBuilder<T>.Create(FDescription, FCategory);
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

class procedure TBackgroundBuilder<T>.PendingStep(World: T);
begin
  SpecContext.Step.MarkAsPending;
end;

function TBackgroundBuilder<T>.Given(const Desc: string): IBackgroundBuilder<T>;
var
  Binding: TStepBinding;
  Captures: TArray<string>;
begin
  if Bindings.FindBinding(skGiven, Desc, Binding, Captures) then
    Result := Given(Desc, procedure(World: T)
      begin
        Bindings.Invoke(Binding, World, Captures);
      end)
  else
    Result := Given(Desc, PendingStep);
end;

function TBackgroundBuilder<T>.Given(const Desc: string; Step: TStepProc<T>): IBackgroundBuilder<T>;
begin
  FBackground.Given(Desc, Step);
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
    FBackground.StepsGiven.Last.MarkAsPending;
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

class procedure TScenarioBuilder<T>.PendingStep(World: T);
begin
  SpecContext.Step.MarkAsPending;
end;

function TScenarioBuilder<T>.CreateBindingStep(Kind: TStepKind; const Desc: string): TStepProc<T>;
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
    Result := PendingStep;
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
                FScenario.StepsGiven.Last.MarkAsPending;
    lskWhen:  if FScenario.StepsWhen.Count > 0 then
                FScenario.StepsWhen.Last.MarkAsPending;
    lskThen:  if FScenario.StepsThen.Count > 0 then
                FScenario.StepsThen.Last.MarkAsPending;
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

class procedure TScenarioOutlineBuilder<T>.PendingStep(World: T);
begin
  SpecContext.Step.MarkAsPending;
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
    Result := PendingStep;
end;

function TScenarioOutlineBuilder<T>.Given(const Desc: string): IScenarioOutlineBuilder<T>;
begin
  // In ScenarioOutline, Given without lambda stores nil (values come from Examples)
  Result := Given(Desc, nil);
end;

function TScenarioOutlineBuilder<T>.Given(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>;
begin
  FStepsGiven.Add(TScenarioStep<T>.Create(sikGiven, nil, Desc, Step));
  FLastStep := lskGiven;
  Result := Self;
end;

function TScenarioOutlineBuilder<T>.When(const Desc: string): IScenarioOutlineBuilder<T>;
begin
  Result := When(Desc, CreateBindingStep(skWhen, Desc));
end;

function TScenarioOutlineBuilder<T>.When(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>;
begin
  FStepsWhen.Add(TScenarioStep<T>.Create(sikWhen, nil, Desc, Step));
  FLastStep := lskWhen;
  Result := Self;
end;

function TScenarioOutlineBuilder<T>.&Then(const Desc: string): IScenarioOutlineBuilder<T>;
begin
  Result := &Then(Desc, CreateBindingStep(skThen, Desc));
end;

function TScenarioOutlineBuilder<T>.&Then(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>;
begin
  FStepsThen.Add(TScenarioStep<T>.Create(sikThen, nil, Desc, Step));
  FLastStep := lskThen;
  Result := Self;
end;

function TScenarioOutlineBuilder<T>.&And(const Desc: string): IScenarioOutlineBuilder<T>;
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

function TScenarioOutlineBuilder<T>.&And(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>;
begin
  case FLastStep of
    lskGiven: FStepsGiven.Add(TScenarioStep<T>.Create(sikAnd, nil, Desc, Step));
    lskWhen:  FStepsWhen.Add(TScenarioStep<T>.Create(sikAnd, nil, Desc, Step));
    lskThen:  FStepsThen.Add(TScenarioStep<T>.Create(sikAnd, nil, Desc, Step));
  else
    raise Exception.Create('And must follow Given, When or Then');
  end;
  Result := Self;
end;

function TScenarioOutlineBuilder<T>.But(const Desc: string): IScenarioOutlineBuilder<T>;
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

function TScenarioOutlineBuilder<T>.But(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>;
begin
  case FLastStep of
    lskGiven: FStepsGiven.Add(TScenarioStep<T>.Create(sikBut, nil, Desc, Step));
    lskWhen:  FStepsWhen.Add(TScenarioStep<T>.Create(sikBut, nil, Desc, Step));
    lskThen:  FStepsThen.Add(TScenarioStep<T>.Create(sikBut, nil, Desc, Step));
  else
    raise Exception.Create('But must follow Given, When or Then');
  end;
  Result := Self;
end;

function TScenarioOutlineBuilder<T>.Pending: IScenarioOutlineBuilder<T>;
begin
  case FLastStep of
    lskGiven: if FStepsGiven.Count > 0 then
                FStepsGiven.Last.MarkAsPending;
    lskWhen:  if FStepsWhen.Count > 0 then
                FStepsWhen.Last.MarkAsPending;
    lskThen:  if FStepsThen.Count > 0 then
                FStepsThen.Last.MarkAsPending;
  end;
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

  // Crear el ScenarioOutline como nodo padre
  var Outline := TScenarioOutline<T>.Create(FRule, FDescription, HeaderStrings);
  // Copiar los steps template al outline
  for var Step in FStepsGiven do
    Outline.Given(Step.Description, Step.Proc);
  for var Step in FStepsWhen do
    Outline.When(Step.Description, Step.Proc);
  for var Step in FStepsThen do
    Outline.&Then(Step.Description, Step.Proc);

  for var RowIdx := 1 to High(Table) do
  begin
    var CurrentRow := Table[RowIdx];
    if Length(CurrentRow) <> Length(Headers) then
      raise Exception.CreateFmt('Row %d does not match header column count.', [RowIdx]);

    // Crear Example con el Outline como parent
    var Example := TScenario<T>.CreateExample(Outline, FDescription);
    Example.ExampleInit(BuildInitStep(Headers, CurrentRow));

    // Asignar metadata simplificada
    var Meta: TExampleMeta;
    Meta.Values := CurrentRow;
    Meta.RowIndex := RowIdx;
    Example.SetExampleMeta(Meta);

    // Copiar los steps (cada Example tiene su propia copia)
    for var Step in FStepsGiven do
      Example.Given(Step.Description, Step.Proc);
    for var Step in FStepsWhen do
      Example.When(Step.Description, Step.Proc);
    for var Step in FStepsThen do
      Example.&Then(Step.Description, Step.Proc);

    // Registrar el Example solo en el Outline (no en Rule.Scenarios)
    // El Outline ya está en Rule.Scenarios y ejecuta sus Examples
    Outline.AddExample(Example);
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
