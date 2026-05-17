unit MediatR.Steps;

interface

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.MiniSpec.Binding,
  Daf.Rtti,
  Daf.DependencyInjection,
  Daf.Extensions.DependencyInjection,
  Daf.MediatR.Contracts,
  Daf.MediatR.DependencyInjection,
  MediatR.Feat;

type
  /// <summary>
  /// Step bindings for the MediatR feature.
  /// Regex captures are auto-converted to method parameters.
  /// </summary>
  TMediatRSteps = class
  private
    procedure RebuildWorld(W: TMediatRWorld);
  public
    // === Given ===

    [Given('a configured mediator')]
    procedure GivenMediatorReady(W: TMediatRWorld);

    [Given('a configured mediator with ordered pipeline behaviors')]
    procedure GivenMediatorWithOrderedBehaviors(W: TMediatRWorld);

    [Given('a configured mediator with discovered pipeline behaviors')]
    procedure GivenMediatorWithDiscoveredBehaviors(W: TMediatRWorld);

    [Given('a configured mediator with short-circuit behavior')]
    procedure GivenMediatorWithShortCircuit(W: TMediatRWorld);

    // === When ===

    [When('I send a TJing request')]
    procedure SendJing(W: TMediatRWorld);

    [When('I send a TPing request and get Pong1')]
    procedure SendPingAndVerifyPong1(W: TMediatRWorld);

    [When('I publish a TPonged notification')]
    procedure PublishPonged(W: TMediatRWorld);

    [When('I send requests from different scopes')]
    procedure SendFromScopes(W: TMediatRWorld);

    // === Then ===

    [ThenAttribute('the TJing handler should have been invoked')]
    procedure VerifyJingHandled(W: TMediatRWorld);

    [ThenAttribute('the dependency mock should have been visited once')]
    procedure VerifyDependencyVisited(W: TMediatRWorld);

    [ThenAttribute('handler 1 should have been invoked')]
    procedure VerifyPongedHandler1(W: TMediatRWorld);

    [ThenAttribute('handler 2 should have been invoked')]
    procedure VerifyPongedHandler2(W: TMediatRWorld);

    [ThenAttribute('each scope has its own dependency instance')]
    procedure VerifyScopedDependencies(W: TMediatRWorld);

    [ThenAttribute('the pipeline trace should be outer then inner then handler')]
    procedure VerifyPipelineOrder(W: TMediatRWorld);

    [ThenAttribute('the pipeline trace should show short-circuit')]
    procedure VerifyPipelineShortCircuit(W: TMediatRWorld);

    [ThenAttribute('the pipeline trace should be empty')]
    procedure VerifyPipelineEmpty(W: TMediatRWorld);
  end;

{ TMediatRSteps }

procedure TMediatRSteps.RebuildWorld(W: TMediatRWorld);
begin
  W.Mediator := nil;
  if W.RootProvider <> nil then
    W.RootProvider.ShutDown;
  W.RootProvider := nil;

  W.ServiceCollection := TServiceCollection.Create;
  W.ServiceCollection.AddMediatR;
  W.ServiceCollection.AddMediatRClasses(_T.PackageOf<TMediatRWorld>);
  W.ServiceCollection.AddScoped<IDependencyMock, TDependencyMock>;
end;

procedure TMediatRSteps.GivenMediatorReady(W: TMediatRWorld);
begin
  RebuildWorld(W);

  // Reset all class-level state to ensure test isolation
  TJingHandler.Done := False;
  TJingHandler.Count := 0;
  TPongedHandler1.Done := False;
  TPongedHandler2.Done := False;
  TPipelineState.Reset;

  W.RootProvider := W.ServiceCollection.BuildServiceProvider;
  W.Mediator := W.RootProvider.GetRequiredService<IMediatorImpl>;
end;

procedure TMediatRSteps.GivenMediatorWithDiscoveredBehaviors(W: TMediatRWorld);
begin
  GivenMediatorReady(W);

  RebuildWorld(W);
  W.ServiceCollection.AddMediatRBehaviors(_T.PackageOf<TMediatRWorld>);

  TJingHandler.Done := False;
  TJingHandler.Count := 0;
  TPongedHandler1.Done := False;
  TPongedHandler2.Done := False;
  TPipelineState.Reset;

  W.RootProvider := W.ServiceCollection.BuildServiceProvider;
  W.Mediator := W.RootProvider.GetRequiredService<IMediatorImpl>;
end;

procedure TMediatRSteps.GivenMediatorWithOrderedBehaviors(W: TMediatRWorld);
begin
  GivenMediatorReady(W);

  RebuildWorld(W);
  W.ServiceCollection.AddTransient(TypeInfo(IPipelineBehaviorInvoker), TOuterBehavior);
  W.ServiceCollection.AddTransient(TypeInfo(IPipelineBehaviorInvoker), TInnerBehavior);

  TJingHandler.Done := False;
  TJingHandler.Count := 0;
  TPongedHandler1.Done := False;
  TPongedHandler2.Done := False;
  TPipelineState.Reset;

  W.RootProvider := W.ServiceCollection.BuildServiceProvider;
  W.Mediator := W.RootProvider.GetRequiredService<IMediatorImpl>;
end;

procedure TMediatRSteps.GivenMediatorWithShortCircuit(W: TMediatRWorld);
begin
  GivenMediatorReady(W);

  RebuildWorld(W);
  W.ServiceCollection.AddTransient(TypeInfo(IPipelineBehaviorInvoker), TOuterBehavior);
  W.ServiceCollection.AddTransient(TypeInfo(IPipelineBehaviorInvoker), TShortCircuitBehavior);
  W.ServiceCollection.AddTransient(TypeInfo(IPipelineBehaviorInvoker), TInnerBehavior);

  TJingHandler.Done := False;
  TJingHandler.Count := 0;
  TPongedHandler1.Done := False;
  TPongedHandler2.Done := False;
  TPipelineState.Reset;

  W.RootProvider := W.ServiceCollection.BuildServiceProvider;
  W.Mediator := W.RootProvider.GetRequiredService<IMediatorImpl>;
end;

procedure TMediatRSteps.SendJing(W: TMediatRWorld);
begin
  W.Mediator.Send(TJing.Create);
end;

procedure TMediatRSteps.SendPingAndVerifyPong1(W: TMediatRWorld);
var
  Response: string;
begin
  Response := W.Mediator.Send<string, TPing>(TPing.Create);
  Expect(Response).ToEqual('Pong1');
end;

procedure TMediatRSteps.PublishPonged(W: TMediatRWorld);
begin
  W.Mediator.Publish(TPonged.Create);
end;

procedure TMediatRSteps.SendFromScopes(W: TMediatRWorld);
var
  Scope1, Scope2: IServiceScope;
  Mediator1, Mediator2: IMediator;
  Response: string;
begin
  Scope1 := W.RootProvider.CreateScope;
  Scope2 := W.RootProvider.CreateScope;

  Mediator1 := Scope1.ServiceProvider.GetRequiredService<IMediatorImpl>;
  Mediator2 := Scope2.ServiceProvider.GetRequiredService<IMediatorImpl>;

  Response := Mediator1.Send<string, TPing>(TPing.Create);
  Expect(Response).ToEqual('Pong1');

  Response := Mediator1.Send<string, TPing>(TPing.Create);
  Expect(Response).ToEqual('Pong2');

  Response := Mediator2.Send<string, TPing>(TPing.Create);
  Expect(Response).ToEqual('Pong1');

  Response := Mediator2.Send<string, TPing>(TPing.Create);
  Expect(Response).ToEqual('Pong2');
end;

procedure TMediatRSteps.VerifyJingHandled(W: TMediatRWorld);
begin
  Expect(TJingHandler.Done).ToBeTrue;
end;

procedure TMediatRSteps.VerifyDependencyVisited(W: TMediatRWorld);
var
  D: IDependencyMock;
begin
  D := W.RootProvider.GetRequiredService<IDependencyMock>;
  Expect(D.Visites).ToEqual(1);
end;

procedure TMediatRSteps.VerifyPongedHandler1(W: TMediatRWorld);
begin
  Expect(TPongedHandler1.Done).ToBeTrue;
end;

procedure TMediatRSteps.VerifyPongedHandler2(W: TMediatRWorld);
begin
  Expect(TPongedHandler2.Done).ToBeTrue;
end;

procedure TMediatRSteps.VerifyScopedDependencies(W: TMediatRWorld);
var
  Scope1, Scope2: IServiceScope;
  D1, D2: IDependencyMock;
begin
  Scope1 := W.RootProvider.CreateScope;
  Scope2 := W.RootProvider.CreateScope;

  D1 := Scope1.ServiceProvider.GetRequiredService<IDependencyMock>;
  D2 := Scope2.ServiceProvider.GetRequiredService<IDependencyMock>;
  // Fresh scopes = fresh dependency instances, not visited yet
  Expect(D1.Visites).ToEqual(0);
  Expect(D2.Visites).ToEqual(0);
  // Different instances
  Expect(D1 = D2).ToBeFalse;
end;

procedure TMediatRSteps.VerifyPipelineOrder(W: TMediatRWorld);
begin
  Expect(TPipelineState.Trace).ToEqual('outer-before>inner-before>handler>inner-after>outer-after');
end;

procedure TMediatRSteps.VerifyPipelineShortCircuit(W: TMediatRWorld);
begin
  Expect(TPipelineState.Trace).ToEqual('outer-before>short>outer-after');
end;

procedure TMediatRSteps.VerifyPipelineEmpty(W: TMediatRWorld);
begin
  Expect(TPipelineState.Trace).ToEqual('');
end;

initialization
  Bindings.RegisterSteps<TMediatRSteps>;

end.
