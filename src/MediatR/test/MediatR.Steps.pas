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

    [Given('a configured mediator with typed ping behavior')]
    procedure GivenMediatorWithTypedPingBehavior(W: TMediatRWorld);

    [Given('a configured mediator with global outer behavior')]
    procedure GivenMediatorWithGlobalOuterBehavior(W: TMediatRWorld);

    [Given('a configured mediator with typed ping behavior for ping descendants')]
    procedure GivenMediatorWithTypedPingBehaviorForDescendants(W: TMediatRWorld);

    [Given('a configured mediator with response short-circuit behavior')]
    procedure GivenMediatorWithResponseShortCircuit(W: TMediatRWorld);

    [Given('a configured mediator with response-modifying behavior')]
    procedure GivenMediatorWithResponseModifyingBehavior(W: TMediatRWorld);

    [Given('a configured mediator with exception-catching behavior')]
    procedure GivenMediatorWithExceptionCatchingBehavior(W: TMediatRWorld);

    [Given('a configured mediator with global and typed behaviors')]
    procedure GivenMediatorWithGlobalAndTypedBehaviors(W: TMediatRWorld);

    [Given('a configured mediator with dependency-aware behavior')]
    procedure GivenMediatorWithDependencyAwareBehavior(W: TMediatRWorld);

    [Given('a configured mediator with typed void tracing behavior')]
    procedure GivenMediatorWithVoidTracingBehavior(W: TMediatRWorld);

    [Given('a configured mediator with void tracing behavior for non-matching request')]
    procedure GivenMediatorWithVoidTracingBehaviorNonMatch(W: TMediatRWorld);

    [Given('a configured mediator with typed void short-circuit behavior')]
    procedure GivenMediatorWithVoidShortCircuit(W: TMediatRWorld);

    [Given('a configured mediator with before-raising behavior')]
    procedure GivenMediatorWithBeforeRaising(W: TMediatRWorld);

    [Given('a configured mediator with after-raising behavior')]
    procedure GivenMediatorWithAfterRaising(W: TMediatRWorld);

    [Given('a configured mediator with two typed ping behaviors')]
    procedure GivenMediatorWithTwoTypedPingBehaviors(W: TMediatRWorld);

    [Given('a configured mediator with nested-send behavior')]
    procedure GivenMediatorWithNestedSendBehavior(W: TMediatRWorld);

    // === When ===

    [When('I send a TJing request')]
    procedure SendJing(W: TMediatRWorld);

    [When('I send a TBoomRequest')]
    procedure SendBoom(W: TMediatRWorld);

    [When('I send a TPing request and get Pong1')]
    procedure SendPingAndVerifyPong1(W: TMediatRWorld);

    [When('I publish a TPonged notification')]
    procedure PublishPonged(W: TMediatRWorld);

    [When('I send requests from different scopes')]
    procedure SendFromScopes(W: TMediatRWorld);

    [When('I send a TPingChild request')]
    procedure SendPingChild(W: TMediatRWorld);

    [When('I send a TPing request via response short-circuit')]
    procedure SendPingViaShortCircuit(W: TMediatRWorld);

    [When('I send a TPing request via response-modifying behavior')]
    procedure SendPingViaModifyingBehavior(W: TMediatRWorld);

    [When('I send a TBoomRequest without exception propagating')]
    procedure SendBoomSuppressed(W: TMediatRWorld);

    [When('I send an unhandled void request')]
    procedure SendUnhandledVoidRequest(W: TMediatRWorld);

    [When('I send an unhandled response request')]
    procedure SendUnhandledResponseRequest(W: TMediatRWorld);

    [When('I publish an unsubscribed notification')]
    procedure PublishUnsubscribed(W: TMediatRWorld);

    [When('I send a TPing request via after-raising behavior')]
    procedure SendPingViaAfterRaising(W: TMediatRWorld);

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

    [ThenAttribute('an exception should have been raised')]
    procedure VerifyExceptionRaised(W: TMediatRWorld);

    [ThenAttribute('the pipeline trace should not contain typed behavior')]
    procedure VerifyTypedBehaviorNotExecuted(W: TMediatRWorld);

    [ThenAttribute('the pipeline trace should contain typed behavior')]
    procedure VerifyTypedBehaviorExecuted(W: TMediatRWorld);

    [ThenAttribute('the pipeline trace should contain outer behavior')]
    procedure VerifyOuterBehaviorInTrace(W: TMediatRWorld);

    [ThenAttribute('the response short-circuit value should be returned')]
    procedure VerifyResponseShortCircuitValue(W: TMediatRWorld);

    [ThenAttribute('the response should be the modified value')]
    procedure VerifyModifiedResponseValue(W: TMediatRWorld);

    [ThenAttribute('the pipeline trace should show the exception was caught')]
    procedure VerifyExceptionCaughtByBehavior(W: TMediatRWorld);

    [ThenAttribute('both behaviors should appear in the pipeline trace')]
    procedure VerifyBothBehaviorsInTrace(W: TMediatRWorld);

    [ThenAttribute('the behavior dependency should appear in the pipeline trace')]
    procedure VerifyDepBehaviorInTrace(W: TMediatRWorld);

    [ThenAttribute('the pipeline trace should contain the void-before marker')]
    procedure VerifyVoidBeforeMarker(W: TMediatRWorld);

    [ThenAttribute('the pipeline trace should not contain the void-before marker')]
    procedure VerifyVoidBeforeMarkerAbsent(W: TMediatRWorld);

    [ThenAttribute('the TJing handler should not have been invoked')]
    procedure VerifyJingNotInvoked(W: TMediatRWorld);

    [ThenAttribute('no exception should have been raised')]
    procedure VerifyNoException(W: TMediatRWorld);

    [ThenAttribute('the pipeline trace should show both ping behaviors in order')]
    procedure VerifyTwoPingBehaviorOrder(W: TMediatRWorld);
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
  Daf.MediatR.DependencyInjection.MediatR.AddBehavior(W.ServiceCollection, TOuterBehavior);
  Daf.MediatR.DependencyInjection.MediatR.AddBehavior(W.ServiceCollection, TInnerBehavior);

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
  Daf.MediatR.DependencyInjection.MediatR.AddBehavior(W.ServiceCollection, TOuterBehavior);
  Daf.MediatR.DependencyInjection.MediatR.AddBehavior(W.ServiceCollection, TShortCircuitBehavior);
  Daf.MediatR.DependencyInjection.MediatR.AddBehavior(W.ServiceCollection, TInnerBehavior);

  TJingHandler.Done := False;
  TJingHandler.Count := 0;
  TPongedHandler1.Done := False;
  TPongedHandler2.Done := False;
  TPipelineState.Reset;

  W.RootProvider := W.ServiceCollection.BuildServiceProvider;
  W.Mediator := W.RootProvider.GetRequiredService<IMediatorImpl>;
end;

procedure TMediatRSteps.GivenMediatorWithTypedPingBehavior(W: TMediatRWorld);
begin
  RebuildWorld(W);
  Daf.MediatR.DependencyInjection.MediatR.AddBehavior(W.ServiceCollection, TPingBehavior);

  TJingHandler.Done := False;
  TJingHandler.Count := 0;
  TPipelineState.Reset;

  W.RootProvider := W.ServiceCollection.BuildServiceProvider;
  W.Mediator := W.RootProvider.GetRequiredService<IMediatorImpl>;
end;

procedure TMediatRSteps.SendJing(W: TMediatRWorld);
begin
  W.Mediator.Send(TJing.Create);
end;

procedure TMediatRSteps.SendBoom(W: TMediatRWorld);
begin
  W.Mediator.Send(TBoomRequest.Create);
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

procedure TMediatRSteps.VerifyExceptionRaised(W: TMediatRWorld);
begin
  ExpectException(Raised).ToBeAny;
end;

procedure TMediatRSteps.VerifyTypedBehaviorNotExecuted(W: TMediatRWorld);
begin
  Expect(TPipelineState.Trace.Contains('ping-before')).ToBeFalse;
end;

procedure TMediatRSteps.VerifyTypedBehaviorExecuted(W: TMediatRWorld);
begin
  Expect(TPipelineState.Trace.Contains('ping-before')).ToBeTrue;
end;

procedure TMediatRSteps.GivenMediatorWithGlobalOuterBehavior(W: TMediatRWorld);
begin
  RebuildWorld(W);
  Daf.MediatR.DependencyInjection.MediatR.AddBehavior(W.ServiceCollection, TOuterBehavior);
  TPingHandler.Invoked := False;
  TPipelineState.Reset;
  W.RootProvider := W.ServiceCollection.BuildServiceProvider;
  W.Mediator := W.RootProvider.GetRequiredService<IMediatorImpl>;
end;

procedure TMediatRSteps.GivenMediatorWithTypedPingBehaviorForDescendants(W: TMediatRWorld);
begin
  RebuildWorld(W);
  Daf.MediatR.DependencyInjection.MediatR.AddBehavior(W.ServiceCollection, TPingBehavior);
  TPipelineState.Reset;
  W.RootProvider := W.ServiceCollection.BuildServiceProvider;
  W.Mediator := W.RootProvider.GetRequiredService<IMediatorImpl>;
end;

procedure TMediatRSteps.GivenMediatorWithResponseShortCircuit(W: TMediatRWorld);
begin
  RebuildWorld(W);
  Daf.MediatR.DependencyInjection.MediatR.AddBehavior(W.ServiceCollection, TResponseShortCircuitBehavior);
  TPingHandler.Invoked := False;
  TPipelineState.Reset;
  W.RootProvider := W.ServiceCollection.BuildServiceProvider;
  W.Mediator := W.RootProvider.GetRequiredService<IMediatorImpl>;
end;

procedure TMediatRSteps.GivenMediatorWithResponseModifyingBehavior(W: TMediatRWorld);
begin
  RebuildWorld(W);
  Daf.MediatR.DependencyInjection.MediatR.AddBehavior(W.ServiceCollection, TResponseModifyingBehavior);
  TPipelineState.Reset;
  W.RootProvider := W.ServiceCollection.BuildServiceProvider;
  W.Mediator := W.RootProvider.GetRequiredService<IMediatorImpl>;
end;

procedure TMediatRSteps.GivenMediatorWithExceptionCatchingBehavior(W: TMediatRWorld);
begin
  RebuildWorld(W);
  Daf.MediatR.DependencyInjection.MediatR.AddBehavior(W.ServiceCollection, TExceptionCatchingBehavior);
  TPipelineState.Reset;
  W.RootProvider := W.ServiceCollection.BuildServiceProvider;
  W.Mediator := W.RootProvider.GetRequiredService<IMediatorImpl>;
end;

procedure TMediatRSteps.GivenMediatorWithGlobalAndTypedBehaviors(W: TMediatRWorld);
begin
  RebuildWorld(W);
  Daf.MediatR.DependencyInjection.MediatR.AddBehavior(W.ServiceCollection, TOuterBehavior);
  Daf.MediatR.DependencyInjection.MediatR.AddBehavior(W.ServiceCollection, TPingBehavior);
  TPipelineState.Reset;
  W.RootProvider := W.ServiceCollection.BuildServiceProvider;
  W.Mediator := W.RootProvider.GetRequiredService<IMediatorImpl>;
end;

procedure TMediatRSteps.GivenMediatorWithDependencyAwareBehavior(W: TMediatRWorld);
begin
  RebuildWorld(W);
  Daf.MediatR.DependencyInjection.MediatR.AddBehavior(W.ServiceCollection, TDependencyAwareBehavior);
  TJingHandler.Done := False;
  TJingHandler.Count := 0;
  TPipelineState.Reset;
  W.RootProvider := W.ServiceCollection.BuildServiceProvider;
  W.Mediator := W.RootProvider.GetRequiredService<IMediatorImpl>;
end;

procedure TMediatRSteps.SendPingChild(W: TMediatRWorld);
begin
  W.LastStringResponse := W.Mediator.Send<string, TPingChild>(TPingChild.Create);
end;

procedure TMediatRSteps.SendPingViaShortCircuit(W: TMediatRWorld);
begin
  W.LastStringResponse := W.Mediator.Send<string, TPing>(TPing.Create);
end;

procedure TMediatRSteps.SendPingViaModifyingBehavior(W: TMediatRWorld);
begin
  W.LastStringResponse := W.Mediator.Send<string, TPing>(TPing.Create);
end;

procedure TMediatRSteps.SendBoomSuppressed(W: TMediatRWorld);
begin
  W.Mediator.Send(TBoomRequest.Create);
end;

procedure TMediatRSteps.VerifyOuterBehaviorInTrace(W: TMediatRWorld);
begin
  Expect(TPipelineState.Trace.Contains('outer-before')).ToBeTrue;
end;

procedure TMediatRSteps.VerifyResponseShortCircuitValue(W: TMediatRWorld);
begin
  Expect(W.LastStringResponse).ToEqual('Short');
end;

procedure TMediatRSteps.VerifyModifiedResponseValue(W: TMediatRWorld);
begin
  Expect(W.LastStringResponse).ToEqual('[Pong1]');
end;

procedure TMediatRSteps.VerifyExceptionCaughtByBehavior(W: TMediatRWorld);
begin
  Expect(TPipelineState.Trace.Contains('caught')).ToBeTrue;
end;

procedure TMediatRSteps.VerifyBothBehaviorsInTrace(W: TMediatRWorld);
begin
  Expect(TPipelineState.Trace.Contains('outer-before')).ToBeTrue;
  Expect(TPipelineState.Trace.Contains('ping-before')).ToBeTrue;
end;

procedure TMediatRSteps.VerifyDepBehaviorInTrace(W: TMediatRWorld);
begin
  Expect(TPipelineState.Trace.Contains('dep-visited')).ToBeTrue;
end;

procedure TMediatRSteps.GivenMediatorWithVoidTracingBehavior(W: TMediatRWorld);
begin
  RebuildWorld(W);
  Daf.MediatR.DependencyInjection.MediatR.AddBehavior(W.ServiceCollection, TVoidTracingBehavior<TJing>);
  TJingHandler.Done := False;
  TJingHandler.Count := 0;
  TPipelineState.Reset;
  W.RootProvider := W.ServiceCollection.BuildServiceProvider;
  W.Mediator := W.RootProvider.GetRequiredService<IMediatorImpl>;
end;

procedure TMediatRSteps.GivenMediatorWithVoidTracingBehaviorNonMatch(W: TMediatRWorld);
begin
  RebuildWorld(W);
  // TBoomRequest explicitly implements IRequest but is not TJing — behavior skipped for TJing sends
  Daf.MediatR.DependencyInjection.MediatR.AddBehavior(W.ServiceCollection, TVoidTracingBehavior<TBoomRequest>);
  TJingHandler.Done := False;
  TJingHandler.Count := 0;
  TPipelineState.Reset;
  W.RootProvider := W.ServiceCollection.BuildServiceProvider;
  W.Mediator := W.RootProvider.GetRequiredService<IMediatorImpl>;
end;

procedure TMediatRSteps.GivenMediatorWithVoidShortCircuit(W: TMediatRWorld);
begin
  RebuildWorld(W);
  Daf.MediatR.DependencyInjection.MediatR.AddBehavior(W.ServiceCollection, TVoidShortCircuitBehavior<TJing>);
  TJingHandler.Done := False;
  TJingHandler.Count := 0;
  TPipelineState.Reset;
  W.RootProvider := W.ServiceCollection.BuildServiceProvider;
  W.Mediator := W.RootProvider.GetRequiredService<IMediatorImpl>;
end;

procedure TMediatRSteps.GivenMediatorWithBeforeRaising(W: TMediatRWorld);
begin
  RebuildWorld(W);
  Daf.MediatR.DependencyInjection.MediatR.AddBehavior(W.ServiceCollection, TBeforeRaisingBehavior);
  TJingHandler.Done := False;
  TJingHandler.Count := 0;
  TPipelineState.Reset;
  W.RootProvider := W.ServiceCollection.BuildServiceProvider;
  W.Mediator := W.RootProvider.GetRequiredService<IMediatorImpl>;
end;

procedure TMediatRSteps.GivenMediatorWithAfterRaising(W: TMediatRWorld);
begin
  RebuildWorld(W);
  Daf.MediatR.DependencyInjection.MediatR.AddBehavior(W.ServiceCollection, TAfterRaisingBehavior);
  TPingHandler.Invoked := False;
  TPipelineState.Reset;
  W.RootProvider := W.ServiceCollection.BuildServiceProvider;
  W.Mediator := W.RootProvider.GetRequiredService<IMediatorImpl>;
end;

procedure TMediatRSteps.GivenMediatorWithTwoTypedPingBehaviors(W: TMediatRWorld);
begin
  RebuildWorld(W);
  // TPingBehavior registered first = outermost; TSecondPingBehavior = inner
  Daf.MediatR.DependencyInjection.MediatR.AddBehavior(W.ServiceCollection, TPingBehavior);
  Daf.MediatR.DependencyInjection.MediatR.AddBehavior(W.ServiceCollection, TSecondPingBehavior);
  TPingHandler.Invoked := False;
  TPipelineState.Reset;
  W.RootProvider := W.ServiceCollection.BuildServiceProvider;
  W.Mediator := W.RootProvider.GetRequiredService<IMediatorImpl>;
end;

procedure TMediatRSteps.GivenMediatorWithNestedSendBehavior(W: TMediatRWorld);
begin
  RebuildWorld(W);
  Daf.MediatR.DependencyInjection.MediatR.AddBehavior(W.ServiceCollection, TNestedSendBehavior);
  TJingHandler.Done := False;
  TJingHandler.Count := 0;
  TPingHandler.Invoked := False;
  TPipelineState.Reset;
  W.RootProvider := W.ServiceCollection.BuildServiceProvider;
  W.Mediator := W.RootProvider.GetRequiredService<IMediatorImpl>;
end;

procedure TMediatRSteps.SendUnhandledVoidRequest(W: TMediatRWorld);
begin
  W.Mediator.Send(TOrphanRequest.Create);
end;

procedure TMediatRSteps.SendUnhandledResponseRequest(W: TMediatRWorld);
begin
  W.Mediator.Send<string, TOrphanQuery>(TOrphanQuery.Create);
end;

procedure TMediatRSteps.PublishUnsubscribed(W: TMediatRWorld);
begin
  W.Mediator.Publish(TOrphanNotification.Create);
end;

procedure TMediatRSteps.SendPingViaAfterRaising(W: TMediatRWorld);
begin
  W.Mediator.Send<string, TPing>(TPing.Create);
end;

procedure TMediatRSteps.VerifyVoidBeforeMarker(W: TMediatRWorld);
begin
  Expect(TPipelineState.Trace.Contains('void-before')).ToBeTrue;
end;

procedure TMediatRSteps.VerifyVoidBeforeMarkerAbsent(W: TMediatRWorld);
begin
  Expect(TPipelineState.Trace.Contains('void-before')).ToBeFalse;
end;

procedure TMediatRSteps.VerifyJingNotInvoked(W: TMediatRWorld);
begin
  Expect(TJingHandler.Done).ToBeFalse;
end;

procedure TMediatRSteps.VerifyNoException(W: TMediatRWorld);
begin
  ExpectException(Raised).ToBeNone;
end;

procedure TMediatRSteps.VerifyTwoPingBehaviorOrder(W: TMediatRWorld);
begin
  Expect(TPipelineState.Trace).ToEqual('ping-before>ping2-before>ping2-after>ping-after');
end;

initialization
  Bindings.RegisterSteps<TMediatRSteps>;

end.
