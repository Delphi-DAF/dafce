unit InjectionPrecedence.Feat;

// Los contextos se ejercitan sobre un TInjectorService construido aqui, y no montando una suite con
// tres contextos vivos: la precedencia es una funcion de lo registrado, asi que la tabla entera cabe
// en un escenario. Que los tres lleguen de verdad al World lo prueban SuiteContext y
// PropertyInjection, por la via de siempre.

interface

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.MiniSpec.Types,
  Daf.MiniSpec.Runner,
  Daf.MiniSpec.Injection,
  SuiteContext.Feat;

type
  TCtxBase = class end;
  TFeatureCtx = class(TCtxBase) end;
  TSuiteCtx = class(TCtxBase) end;

  /// El tipo que nadie registra. Existe para que la propiedad de abajo no se pueda satisfacer.
  TUnavailable = class end;

  /// El contexto de suite que declara la ejecucion anidada, y solo ella.
  TOwnCtx = class end;

  /// Pide el contexto que declara la anidada: se satisface dentro de ella.
  TInsiderWorld = class
  strict private
    FOwn: TOwnCtx;
  public
    [MiniInject]
    property Own: TOwnCtx read FOwn write FOwn;
  end;

  /// Pide el contexto de suite de la ejecucion EXTERIOR, que esta vivo ahi fuera mientras esta
  /// spec corre. Si la anidada lo heredase, este World se construiria sin protestar.
  TOutsiderWorld = class
  strict private
    FGlobal: TSuiteResources;
  public
    [MiniInject]
    property Global: TSuiteResources read FGlobal write FGlobal;
  end;

  TNeedy = class
  strict private
    FMissing: TUnavailable;
  public
    [MiniInject]
    property Missing: TUnavailable read FMissing write FMissing;
  end;

  TInjWorld = class
  strict private
    FInjector: TInjectorService;
  public
    Base: TCtxBase;
    FeatureCtx: TFeatureCtx;
    SuiteCtx: TSuiteCtx;
    Needy: TNeedy;
    Inner: ISpecContext;
    Nested: TSpecRun;
    Resolved: string;
    Report: string;
    SeenInside: string;
    SeenOutside: string;
    constructor Create;
    destructor Destroy; override;
    /// Devuelve el nombre de clase, o el centinela: ToBeNull no imprime el valor real, y aqui lo
    /// que hay que ver cuando falla es QUE contexto contesto.
    function NameOf(Instance: TObject): string;
    property Injector: TInjectorService read FInjector;
  end;

constructor TInjWorld.Create;
begin
  inherited;
  Nested := TSpecRun.Create('Nested');
  FInjector := TInjectorService.Create;
  Base := TCtxBase.Create;
  FeatureCtx := TFeatureCtx.Create;
  SuiteCtx := TSuiteCtx.Create;
  Needy := TNeedy.Create;
end;

destructor TInjWorld.Destroy;
begin
  Nested.Free;
  Needy.Free;
  SuiteCtx.Free;
  FeatureCtx.Free;
  Base.Free;
  FInjector.Free;
  inherited;
end;

function TInjWorld.NameOf(Instance: TObject): string;
begin
  if Instance = nil then
    Result := '<none>'
  else
    Result := Instance.ClassName;
end;

initialization

Feature('''
Injection precedence @injection

  As whoever declares a context at more than one level
  I want the one that wins to be declared
  So the answer does not depend on the order some dictionary happens to keep
''')

.UseWorld<TInjWorld>

.Rule('The nearest context to the step wins')

  .Scenario('Two contexts that share an ancestor')
    .Given('a suite context and a feature context that descend from the same base', procedure(W: TInjWorld)
      begin
        W.Injector.Register(csSuite, W.SuiteCtx);
        W.Injector.Register(csFeature, W.FeatureCtx);
      end)
    .When('a World asks for a property declared with the type of that base', procedure(W: TInjWorld)
      begin
        W.Resolved := W.NameOf(W.Injector.Resolve(TCtxBase));
      end)
    .&Then('it gets the feature one, for being the nearest to the step', procedure(W: TInjWorld)
      begin
        Expect(W.Resolved).ToEqual('TFeatureCtx');
      end)

  // El control del anterior: con el mismo par, cambiar el tipo pedido cambia el ganador. Sin este,
  // un Resolve que devolviera siempre el de feature pasaria aquel y este no.
  .Scenario('An exact match beats one by inheritance')
    .Given('a suite context of exactly the type asked for and a feature one that only inherits', procedure(W: TInjWorld)
      begin
        W.Injector.Register(csSuite, W.Base);
        W.Injector.Register(csFeature, W.FeatureCtx);
      end)
    .When('a World asks for that property', procedure(W: TInjWorld)
      begin
        W.Resolved := W.NameOf(W.Injector.Resolve(TCtxBase));
      end)
    .&Then('it gets the suite one, because the exact type is preferred', procedure(W: TInjWorld)
      begin
        Expect(W.Resolved).ToEqual('TCtxBase');
      end)

.Rule('A declared dependency that is absent is an omission, not a value')

  .Scenario('Nobody registered that type')
    .When('a World with an [MiniInject] property nothing satisfies is built', procedure(W: TInjWorld)
      begin
        W.Injector.InjectInto(W.Needy);
      end)
    .&Then('it fails naming the property, its type and the class that declares it', procedure(W: TInjWorld)
      var
        Captured: TCapturedRaise;
      begin
        // Raised consumes what the When left, so it is read once and held.
        Captured := Raised;
        ExpectException(Captured).ToBe(EInjectionError);
        ExpectException(Captured).ToHaveMessage('Missing');
        ExpectException(Captured).ToHaveMessage('TUnavailable');
        ExpectException(Captured).ToHaveMessage('TNeedy');
      end)

.Rule('The registered contexts belong to their run')

  .Scenario('What a run registers does not outlive it')
    .Given('a step pushes a context of its own and registers a context in it', procedure(W: TInjWorld)
      begin
        W.Inner := TSpecContextImpl.Create;
        PushSpecContext(W.Inner);
        try
          SpecContext.Injector.Register(csSuite, W.SuiteCtx);
          W.SeenInside := W.NameOf(SpecContext.Injector.Resolve(TSuiteCtx));
        finally
          PopSpecContext;
        end;
      end)
    .When('that context is no longer the active one', procedure(W: TInjWorld)
      begin
        W.SeenOutside := W.NameOf(SpecContext.Injector.Resolve(TSuiteCtx));
      end)
    .&Then('the run that goes on never saw that registration', procedure(W: TInjWorld)
      begin
        Expect(W.SeenInside).ToEqual('TSuiteCtx');
        Expect(W.SeenOutside).ToEqual('<none>');
      end)

  .Scenario('A nested run does not inherit the contexts of the outer one')
    .Given('a nested run that declares a suite context of its own', procedure(W: TInjWorld)
      begin
        W.Nested.UseSuiteContext<TOwnCtx>;
        Feature('Asks for the context the nested run declares', W.Nested.Suite)
          .UseWorld<TInsiderWorld>
          .Scenario('Its own context is injected')
            .&Then('the property holds it', procedure(Ins: TInsiderWorld)
              begin
                Expect(Ins.Own <> nil).ToBeTrue;
              end);
        Feature('Asks for the suite context of the outer run', W.Nested.Suite)
          .UseWorld<TOutsiderWorld>
          .Scenario('That one is not there')
            .&Then('this step never runs, because the World cannot be built', procedure(Outs: TOutsiderWorld)
              begin
                Expect('the World was built').ToEqual('it should not have been');
              end);
      end)
    .When('it builds the Worlds of its scenarios', procedure(W: TInjWorld)
      begin
        W.Nested.Execute;
      end)
    .&Then('only the contexts the nested run declared are injected', procedure(W: TInjWorld)
      begin
        Expect(Integer(W.Nested.PassCount)).ToEqual(1);
        Expect(Integer(W.Nested.FailCount)).ToEqual(1);
      end);

end.
