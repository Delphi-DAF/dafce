unit Daf.MiniSpec.Builders;

interface
uses
  System.Generics.Collections,
  System.Rtti,
  System.SysUtils,
  Daf.MiniSpec.Types;

type
  TRuleBuilder<T: class, constructor> = class;  // forward declaration

  TFeatureBuilder<T: class, constructor> = class(TInterfacedObject, IFeatureBuilder<T>)
  strict private
    FFeature: TFeature<T>;
  public
    constructor Create(const Feature: TFeature<T>);overload;
    constructor Create(const Description: string);overload;
    function Background: IBackgroundBuilder<T>;
    function Scenario(const Description: string): IScenarioBuilder<T>;overload;
    function ScenarioOutline(const Description: string): IScenarioOutlineBuilder<T>;
    function Rule(const Description: string): IRuleBuilder<T>;
  end;

  TFeatureBuilder = class
  strict private
    FDescription: string;
  public
    constructor Create(const Description: string);
    function UseWorld<T: class, constructor>: IFeatureBuilder<T>;
  end;

  TBackgroundBuilder<T: class, constructor> = class(TInterfacedObject, IBackgroundBuilder<T>)
  strict private
    FBackground: TBackground<T>;
    FRule: TRule<T>;  // Siempre es una Rule (explícita o implícita)
  public
    constructor Create(const ARule: IRule);
    function Given(const Desc: string; Step: TStepProc<T>): IBackgroundBuilder<T>;
    function &And(const Desc: string; Step: TStepProc<T>): IBackgroundBuilder<T>;
    function But(const Desc: string; Step: TStepProc<T>): IBackgroundBuilder<T>;
    function Scenario(const Description: string): IScenarioBuilder<T>;overload;
    function ScenarioOutline(const Description: string): IScenarioOutlineBuilder<T>;
    function Rule(const Description: string): IRuleBuilder<T>;
  end;

  TScenarioBuilder<T: class, constructor> = class(TInterfacedObject, IScenarioBuilder<T>)
  strict private
    FScenario: TScenario<T>;
    FLastStep: TLastStepKind;
    FRule: TRule<T>;  // Siempre es una Rule (explícita o implícita)
  public
    constructor Create(const ARule: IRule; const Description: string);
    function ExampleInit(Step: TStepProc<T>): IScenarioBuilder<T>;
    procedure SetExampleMeta(const Meta: TExampleMeta);
    function Given(const Desc: string; Step: TStepProc<T>): IScenarioBuilder<T>;
    function When(const Desc: string; Step: TStepProc<T>) : IScenarioBuilder<T>;
    function &Then(const Desc: string; Step: TStepProc<T>) : IScenarioBuilder<T>;
    function &And(const Desc: string; Step: TStepProc<T>): IScenarioBuilder<T>;
    function But(const Desc: string; Step: TStepProc<T>): IScenarioBuilder<T>;

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
  public
    constructor Create(const ARule: IRule; const Desc: string);
    destructor Destroy; override;
    function Given(const Desc: string; Step: TStepProc<T> = nil) : IScenarioOutlineBuilder<T>;
    function When(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>;
    function &Then(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>;
    function &And(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>;
    function But(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>;
    function Examples(const Table: TExamplesTable): IFeatureBuilder<T>;
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
    function Background: IBackgroundBuilder<T>;
    function Scenario(const Description: string): IScenarioBuilder<T>;
    function ScenarioOutline(const Description: string): IScenarioOutlineBuilder<T>;
    function Rule(const Description: string): IRuleBuilder<T>;  // Requerido por IFeatureBuilder
  end;

implementation

{ TFeatureBuilder<T> }

constructor TFeatureBuilder<T>.Create(const Feature: TFeature<T>);
begin
  inherited Create;
  FFeature := Feature;
end;

constructor TFeatureBuilder<T>.Create(const Description: string);
begin
  inherited Create;
  FFeature := TFeature<T>.Create(Description);
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
end;

function TFeatureBuilder.UseWorld<T>: IFeatureBuilder<T>;
begin
  Result := TFeatureBuilder<T>.Create(FDescription);
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

function TBackgroundBuilder<T>.Given(const Desc: string; Step: TStepProc<T>): IBackgroundBuilder<T>;
begin
  FBackground.Given(Desc, Step);
  Result := Self;
end;

function TBackgroundBuilder<T>.&And(const Desc: string; Step: TStepProc<T>): IBackgroundBuilder<T>;
begin
  FBackground.Given(Desc, Step);
  Result := Self;
end;

function TBackgroundBuilder<T>.But(const Desc: string; Step: TStepProc<T>): IBackgroundBuilder<T>;
begin
  FBackground.Given(Desc, Step);
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

function TScenarioBuilder<T>.ExampleInit(Step: TStepProc<T>): IScenarioBuilder<T>;
begin
  FScenario.ExampleInit(Step);
  Result := Self;
end;

procedure TScenarioBuilder<T>.SetExampleMeta(const Meta: TExampleMeta);
begin
  FScenario.SetExampleMeta(Meta);
end;

function TScenarioBuilder<T>.Given(const Desc: string; Step: TStepProc<T>): IScenarioBuilder<T>;
begin
  FScenario.Given(Desc, Step);
  FLastStep := lskGiven;
  Result := Self;
end;

function TScenarioBuilder<T>.When(const Desc: string; Step: TStepProc<T>): IScenarioBuilder<T>;
begin
  FScenario.When(Desc, Step);
  FLastStep := lskWhen;
  Result := Self;
end;

function TScenarioBuilder<T>.&Then(const Desc: string; Step: TStepProc<T>): IScenarioBuilder<T>;
begin
  FScenario.&Then(Desc, Step);
  FLastStep := lskThen;
  Result := Self;
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

function TScenarioOutlineBuilder<T>.Given(const Desc: string; Step: TStepProc<T> = nil): IScenarioOutlineBuilder<T>;
begin
  FStepsGiven.Add(TScenarioStep<T>.Create(sikGiven, nil, Desc,Step));
  FLastStep := lskGiven;
  Result := Self;
end;

function TScenarioOutlineBuilder<T>.When(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>;
begin
  FStepsWhen.Add(TScenarioStep<T>.Create(sikWhen, nil, Desc, Step));
  FLastStep := lskWhen;
  Result := Self;
end;

function TScenarioOutlineBuilder<T>.&Then(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>;
begin
  FStepsThen.Add(TScenarioStep<T>.Create(sikThen, nil, Desc, Step));
  FLastStep := lskThen;
  Result := Self;
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

function TScenarioOutlineBuilder<T>.Examples(const Table: TExamplesTable): IFeatureBuilder<T>;
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

end.
