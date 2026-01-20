unit Daf.MiniSpec.Builders;

interface
uses
  System.Generics.Collections,
  System.Rtti,
  System.SysUtils,
  Daf.MiniSpec.Types;

type
  TFeatureBuilder<T: class, constructor> = class(TInterfacedObject, IFeatureBuilder<T>)
  strict private
    FFeature: TFeature<T>;
  public
    constructor Create(const Feature: TFeature<T>);overload;
    constructor Create(const Description: string);overload;
    function Background: IBackgroundBuilder<T>;
    function Scenario(const Description: string): IScenarioBuilder<T>;overload;
    function ScenarioOutline(const Description: string): IScenarioOutlineBuilder<T>;
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
  public
    constructor Create(const AFeature: TFeature<T>);
    function Given(const Desc: string; Step: TStepProc<T>): IBackgroundBuilder<T>;
    function &And(const Desc: string; Step: TStepProc<T>): IBackgroundBuilder<T>;
    function But(const Desc: string; Step: TStepProc<T>): IBackgroundBuilder<T>;
    function Scenario(const Description: string): IScenarioBuilder<T>;overload;
    function ScenarioOutline(const Description: string): IScenarioOutlineBuilder<T>;
  end;

  TScenarioBuilder<T: class, constructor> = class(TInterfacedObject, IScenarioBuilder<T>)
  strict private
    FScenario: TScenario<T>;
    FLastStep: TLastStepKind;
  public
    constructor Create(const AFeature: TFeature<T>; const Description: string);
    function ExampleInit(Step: TStepProc<T>): IScenarioBuilder<T>;
    function Given(const Desc: string; Step: TStepProc<T>): IScenarioBuilder<T>;
    function When(const Desc: string; Step: TStepProc<T>) : IScenarioBuilder<T>;
    function &Then(const Desc: string; Step: TStepProc<T>) : IScenarioBuilder<T>;
    function &And(const Desc: string; Step: TStepProc<T>): IScenarioBuilder<T>;
    function But(const Desc: string; Step: TStepProc<T>): IScenarioBuilder<T>;

    function Scenario(const Description: string): IScenarioBuilder<T>;overload;
    function ScenarioOutline(const Description: string): IScenarioOutlineBuilder<T>;
  end;

  TScenarioOutlineBuilder<T: class, constructor> = class(TinterfacedObject, IScenarioOutlineBuilder<T>)
  strict private
    FFeature: TFeature<T>;
    FDescription: string;
    FStepsGiven: TList<TScenarioStep<T>>;
    FStepsWhen: TList<TScenarioStep<T>>;
    FStepsThen: TList<TScenarioStep<T>>;
    FLastStep: TLastStepKind;
    function BuildInitStep(Headers, Row: TArray<TValue>): TStepProc<T>;
  public
    constructor Create(Feature: TFeature<T>; const Desc: string);
    destructor Destroy; override;
    function Given(const Desc: string; Step: TStepProc<T> = nil) : IScenarioOutlineBuilder<T>;
    function When(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>;
    function &Then(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>;
    function &And(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>;
    function But(const Desc: string; Step: TStepProc<T>): IScenarioOutlineBuilder<T>;
    function Examples(const Table: TExamplesTable): IFeatureBuilder<T>;
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
  Result := TBackgroundBuilder<T>.Create(FFeature);
end;

function TFeatureBuilder<T>.Scenario(const Description: string): IScenarioBuilder<T>;
begin
  Result := TScenarioBuilder<T>.Create(FFeature, Description);
end;

function TFeatureBuilder<T>.ScenarioOutline(const Description: string): IScenarioOutlineBuilder<T>;
begin
  Result := TScenarioOutlineBuilder<T>.Create(FFeature, Description);
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

constructor TBackgroundBuilder<T>.Create(const AFeature: TFeature<T>);
begin
  inherited Create;
  FBackground := TBackground<T>.Create(AFeature);
end;

function TBackgroundBuilder<T>.Given(const Desc: string; Step: TStepProc<T>): IBackgroundBuilder<T>;
begin
  FBackground.Given(Desc, Step);
  Result := Self;
end;

function TBackgroundBuilder<T>.&And(const Desc: string; Step: TStepProc<T>): IBackgroundBuilder<T>;
begin
  // And en Background siempre es un Given adicional
  FBackground.Given(Desc, Step);
  Result := Self;
end;

function TBackgroundBuilder<T>.But(const Desc: string; Step: TStepProc<T>): IBackgroundBuilder<T>;
begin
  // But en Background siempre es un Given adicional
  FBackground.Given(Desc, Step);
  Result := Self;
end;

function TBackgroundBuilder<T>.Scenario(const Description: string): IScenarioBuilder<T>;
begin
  Result := TScenarioBuilder<T>.Create(FBackground.Feature as TFeature<T>, Description);
end;

function TBackgroundBuilder<T>.ScenarioOutline(const Description: string): IScenarioOutlineBuilder<T>;
begin
  Result := TScenarioOutlineBuilder<T>.Create(FBackground.Feature as TFeature<T>, Description);
end;

{ TScenarioBuilder<T> }

constructor TScenarioBuilder<T>.Create(const AFeature: TFeature<T>; const Description: string);
begin
  inherited Create;
  FScenario := TScenario<T>.Create(AFeature, Description);
  FLastStep := lskNone;
end;

function TScenarioBuilder<T>.ExampleInit(Step: TStepProc<T>): IScenarioBuilder<T>;
begin
  FScenario.ExampleInit(Step);
  Result := Self;
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
  Result := TScenarioBuilder<T>.Create(FScenario.Feature as TFeature<T>, Description);
end;

function TScenarioBuilder<T>.ScenarioOutline(const Description: string): IScenarioOutlineBuilder<T>;
begin
  Result := TScenarioOutlineBuilder<T>.Create(FScenario.Feature as TFeature<T>, Description);
end;

{ TScenarioOutlineBuilder<T> }

constructor TScenarioOutlineBuilder<T>.Create(Feature: TFeature<T>; const Desc: string);
begin
  FDescription := Desc;
  FFeature := Feature;
  FStepsGiven := TObjectList<TScenarioStep<T>>.Create;
  FStepsWhen := TObjectList<TScenarioStep<T>>.Create;
  FStepsThen := TObjectList<TScenarioStep<T>>.Create;
  FLastStep := lskNone;
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
  for var RowIdx := 1 to High(Table) do
  begin
    var CurrentRow := Table[RowIdx];
    if Length(CurrentRow) <> Length(Headers) then
      raise Exception.CreateFmt('Row %d does not match header column count.', [RowIdx]);

    var ScnBuilder := TScenarioBuilder<T>.Create(FFeature, FDescription);
    ScnBuilder.ExampleInit(BuildInitStep(Headers, CurrentRow));

    for var Step in FStepsGiven do
      ScnBuilder.Given(Step.Description, Step.Proc);

    for var Step in FStepsWhen do
      ScnBuilder.When(Step.Description, Step.Proc);

    for var Step in FStepsThen do
      ScnBuilder.&Then(Step.Description, Step.Proc);
  end;
  Result := TFeatureBuilder<T>.Create(FFeature);
end;

end.
