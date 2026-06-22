unit MediatR.Feat;

interface

uses
  System.SysUtils,
  System.Rtti,
  Daf.MediatR.Contracts,
  Daf.Extensions.DependencyInjection;

type
  TPipelineState = class
  public
    class var Trace: string;
    class procedure Reset;
    class procedure Add(const Value: string);
  end;

  IDependencyMock = interface(IInterface)
    ['{DDBE3375-7995-4983-8683-A4E52529C623}']
    procedure Visit;
    function Visites: Integer;
  end;

  TDependencyMock = class(TInterfacedObject, IDependencyMock)
  private
    FVisites: Integer;
  public
    procedure Visit;
    function Visites: Integer;
  end;

  TPing = class(TInterfacedObject, IRequest<string>)
  end;

  TPingHandler = class(TInterfacedObject, IRequestHandler<string, TPing>)
  private
    FDependency: IDependencyMock;
  public
    class var Invoked: Boolean;
    constructor Create(const Dependency: IDependencyMock);
    procedure Handle(TRequest: TPing; out Result: string);
  end;

  TJing = class(TInterfacedObject, IRequest)
  end;

  TJingHandler = class(TInterfacedObject, IRequestHandler<TJing>)
  public
    class var Done: Boolean;
    class var Count: Integer;
    procedure Handle(TRequest: TJing);
  end;

  TOuterBehavior = class(TPipelineBehavior)
  public
    function Handle(Request: TObject; Next: TFunc<TValue>): TValue; override;
  end;

  TInnerBehavior = class(TPipelineBehavior)
  public
    function Handle(Request: TObject; Next: TFunc<TValue>): TValue; override;
  end;

  [MediatorAbstract]
  TShortCircuitBehavior = class(TPipelineBehavior)
  public
    function Handle(Request: TObject; Next: TFunc<TValue>): TValue; override;
  end;

  // Typed behavior: applies only to TPing requests
  TPingBehavior = class(TPipelineBehavior<string, TPing>)
  public
    function Handle(Request: TPing; Next: TNext<string>): string; override;
  end;

  // Descendant of TPing (for polymorphic typed behavior test)
  TPingChild = class(TPing)
  end;

  TPingChildHandler = class(TInterfacedObject, IRequestHandler<string, TPingChild>)
  public
    procedure Handle(TRequest: TPingChild; out Result: string);
  end;

  [MediatorAbstract]
  TResponseShortCircuitBehavior = class(TPipelineBehavior<string, TPing>)
  public
    function Handle(Request: TPing; Next: TNext<string>): string; override;
  end;

  [MediatorAbstract]
  TResponseModifyingBehavior = class(TPipelineBehavior<string, TPing>)
  public
    function Handle(Request: TPing; Next: TNext<string>): string; override;
  end;

  [MediatorAbstract]
  TExceptionCatchingBehavior = class(TPipelineBehavior)
  public
    function Handle(Request: TObject; Next: TFunc<TValue>): TValue; override;
  end;

  [MediatorAbstract]
  TDependencyAwareBehavior = class(TPipelineBehavior)
  private
    FDependency: IDependencyMock;
  public
    constructor Create(const Dependency: IDependencyMock);
    function Handle(Request: TObject; Next: TFunc<TValue>): TValue; override;
  end;

  // Request that raises an exception in its handler (for scenario 5.5)
  TBoomRequest = class(TInterfacedObject, IRequest)
  end;

  TBoomHandler = class(TInterfacedObject, IRequestHandler<TBoomRequest>)
  public
    procedure Handle(TRequest: TBoomRequest);
  end;

  TPonged = class(TInterfacedObject, INotification)
  end;

  TPongedHandler1 = class(TInterfacedObject, INotificationHandler<TPonged>)
  public
    class var Done: Boolean;
    procedure Handle(Notification: TPonged);
  end;

  TPongedHandler2 = class(TInterfacedObject, INotificationHandler<TPonged>)
  public
    class var Done: Boolean;
    procedure Handle(Notification: TPonged);
  end;

  // 1.1 Typed void behavior — traces before+after, calls Next (covers pipeline-typed-void-behavior)
  [MediatorAbstract]
  TVoidTracingBehavior<TRequest: class, IRequest> = class(TPipelineBehavior<TRequest>)
  public
    procedure Handle(Request: TRequest; Next: TNext); override;
  end;

  // 1.2 Typed void behavior — does NOT call Next (short-circuit)
  [MediatorAbstract]
  TVoidShortCircuitBehavior<TRequest: class, IRequest> = class(TPipelineBehavior<TRequest>)
  public
    procedure Handle(Request: TRequest; Next: TNext); override;
  end;

  // 1.3 Global behavior — raises before calling Next (pipeline-error-handling)
  [MediatorAbstract]
  TBeforeRaisingBehavior = class(TPipelineBehavior)
  public
    function Handle(Request: TObject; Next: TFunc<TValue>): TValue; override;
  end;

  // 1.4 Global behavior — raises after Next() returns
  [MediatorAbstract]
  TAfterRaisingBehavior = class(TPipelineBehavior)
  public
    function Handle(Request: TObject; Next: TFunc<TValue>): TValue; override;
  end;

  // 1.5 Second typed ping behavior with distinct markers for composition ordering test
  [MediatorAbstract]
  TSecondPingBehavior = class(TPipelineBehavior<string, TPing>)
  public
    function Handle(Request: TPing; Next: TNext<string>): string; override;
  end;

  // 1.6 Typed ping behavior that performs a nested Send<TJing> before calling Next
  [MediatorAbstract]
  TNestedSendBehavior = class(TPipelineBehavior<string, TPing>)
  private
    FMediator: IMediator;
  public
    constructor Create(const Mediator: IMediator);
    function Handle(Request: TPing; Next: TNext<string>): string; override;
  end;

  // Request/notification types with no registered handlers (for error/silent scenarios)
  TOrphanRequest = class(TInterfacedObject, IRequest)
  end;

  TOrphanQuery = class(TInterfacedObject, IRequest<string>)
  end;

  TOrphanNotification = class(TInterfacedObject, INotification)
  end;

  TMediatRWorld = class
  public
    ServiceCollection: IServiceCollection;
    RootProvider: IServiceProvider;
    Mediator: IMediator;
    LastStringResponse: string;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  Daf.MiniSpec,
  Daf.Rtti,
  Daf.DependencyInjection,
  Daf.MediatR.DependencyInjection;

{ TDependencyMock }

class procedure TPipelineState.Reset;
begin
  Trace := '';
end;

class procedure TPipelineState.Add(const Value: string);
begin
  if Trace.IsEmpty then
    Trace := Value
  else
    Trace := Trace + '>' + Value;
end;

procedure TDependencyMock.Visit;
begin
  Inc(FVisites);
end;

function TDependencyMock.Visites: Integer;
begin
  Result := FVisites;
end;

{ TPingHandler }

constructor TPingHandler.Create(const Dependency: IDependencyMock);
begin
  inherited Create;
  FDependency := Dependency;
end;

procedure TPingHandler.Handle(TRequest: TPing; out Result: string);
begin
  Invoked := True;
  FDependency.Visit;
  Result := 'Pong' + FDependency.Visites.ToString;
end;

{ TJingHandler }

procedure TJingHandler.Handle(TRequest: TJing);
begin
  Inc(Count);
  TPipelineState.Add('handler');
  Done := True;
end;

{ TOuterBehavior }

function TOuterBehavior.Handle(Request: TObject; Next: TFunc<TValue>): TValue;
begin
  TPipelineState.Add('outer-before');
  Result := Next();
  TPipelineState.Add('outer-after');
end;

{ TInnerBehavior }

function TInnerBehavior.Handle(Request: TObject; Next: TFunc<TValue>): TValue;
begin
  TPipelineState.Add('inner-before');
  Result := Next();
  TPipelineState.Add('inner-after');
end;

{ TShortCircuitBehavior }

function TShortCircuitBehavior.Handle(Request: TObject; Next: TFunc<TValue>): TValue;
begin
  TPipelineState.Add('short');
  Result := Default(TValue);
end;

{ TPingBehavior }

function TPingBehavior.Handle(Request: TPing; Next: TNext<string>): string;
begin
  TPipelineState.Add('ping-before');
  Result := Next.Call;
  TPipelineState.Add('ping-after');
end;

{ TPingChildHandler }

procedure TPingChildHandler.Handle(TRequest: TPingChild; out Result: string);
begin
  Result := 'PingChild';
end;

{ TResponseShortCircuitBehavior }

function TResponseShortCircuitBehavior.Handle(Request: TPing; Next: TNext<string>): string;
begin
  Result := 'Short';
end;

{ TResponseModifyingBehavior }

function TResponseModifyingBehavior.Handle(Request: TPing; Next: TNext<string>): string;
begin
  Result := '[' + Next.Call + ']';
end;

{ TExceptionCatchingBehavior }

function TExceptionCatchingBehavior.Handle(Request: TObject; Next: TFunc<TValue>): TValue;
begin
  try
    Result := Next();
  except
    TPipelineState.Add('caught');
    Result := Default(TValue);
  end;
end;

{ TDependencyAwareBehavior }

constructor TDependencyAwareBehavior.Create(const Dependency: IDependencyMock);
begin
  inherited Create;
  FDependency := Dependency;
end;

function TDependencyAwareBehavior.Handle(Request: TObject; Next: TFunc<TValue>): TValue;
begin
  FDependency.Visit;
  TPipelineState.Add('dep-visited');
  Result := Next();
end;

{ TBoomHandler }

procedure TBoomHandler.Handle(TRequest: TBoomRequest);
begin
  raise Exception.Create('Boom!');
end;

{ TPongedHandler1 }

procedure TPongedHandler1.Handle(Notification: TPonged);
begin
  TPongedHandler1.Done := True;
end;

{ TPongedHandler2 }

procedure TPongedHandler2.Handle(Notification: TPonged);
begin
  TPongedHandler2.Done := True;
end;

{ TVoidTracingBehavior }

procedure TVoidTracingBehavior<TRequest>.Handle(Request: TRequest; Next: TNext);
begin
  TPipelineState.Add('void-before');
  Next.Call;
  TPipelineState.Add('void-after');
end;

{ TVoidShortCircuitBehavior }

procedure TVoidShortCircuitBehavior<TRequest>.Handle(Request: TRequest; Next: TNext);
begin
  TPipelineState.Add('void-sc');
end;

{ TBeforeRaisingBehavior }

function TBeforeRaisingBehavior.Handle(Request: TObject; Next: TFunc<TValue>): TValue;
begin
  raise Exception.Create('BeforeRaising');
end;

{ TAfterRaisingBehavior }

function TAfterRaisingBehavior.Handle(Request: TObject; Next: TFunc<TValue>): TValue;
begin
  Result := Next();
  raise Exception.Create('AfterRaising');
end;

{ TSecondPingBehavior }

function TSecondPingBehavior.Handle(Request: TPing; Next: TNext<string>): string;
begin
  TPipelineState.Add('ping2-before');
  Result := Next.Call;
  TPipelineState.Add('ping2-after');
end;

{ TNestedSendBehavior }

constructor TNestedSendBehavior.Create(const Mediator: IMediator);
begin
  inherited Create;
  FMediator := Mediator;
end;

function TNestedSendBehavior.Handle(Request: TPing; Next: TNext<string>): string;
begin
  FMediator.Send(TJing.Create);
  Result := Next.Call;
end;

{ TMediatRWorld }

constructor TMediatRWorld.Create;
begin
  inherited;
  ServiceCollection := TServiceCollection.Create;
  ServiceCollection.AddMediatR;
  ServiceCollection.AddMediatRClasses(_T.PackageOf<TMediatRWorld>);
  ServiceCollection.AddScoped<IDependencyMock, TDependencyMock>;
  RootProvider := ServiceCollection.BuildServiceProvider;
end;

destructor TMediatRWorld.Destroy;
begin
  Mediator := nil;
  if RootProvider <> nil then
    RootProvider.ShutDown;
  RootProvider := nil;
  ServiceCollection := nil;
  inherited;
end;

// --- Feature definition ---

initialization

Feature('''
Feature MediatR @mediatr

  As a developer
  I want to use the Mediator pattern for in-process messaging
  So I can decouple request handlers from callers
''')

.UseWorld<TMediatRWorld>

// --- Send (void) ---

.Rule('Send requests without response')

  .Scenario('Can send a simple request')
    .Given('a configured mediator')
    .When('I send a TJing request')
    .&Then('the TJing handler should have been invoked')

// --- Send with Response ---

.Rule('Send requests with response')

  .Scenario('Can send a request and receive a response')
    .Given('a configured mediator')
    .When('I send a TPing request and get Pong1')
    .&Then('the dependency mock should have been visited once')

// --- Publish Notifications ---

.Rule('Publish notifications')

  .Scenario('Can publish a notification to multiple handlers')
    .Given('a configured mediator')
    .When('I publish a TPonged notification')
    .&Then('handler 1 should have been invoked')
    .&And('handler 2 should have been invoked')

// --- Scoped Mediation ---

.Rule('Scoped mediation')

  .Scenario('Scoped mediators use scoped dependencies')
    .Given('a configured root provider').NoAction
    .When('I send requests from different scopes')
    .&Then('each scope has its own dependency instance')

// --- Pipeline behaviors ---

.Rule('Request pipeline behaviors')

  .Scenario('Behaviors can be discovered automatically via package scan')
    .Given('a configured mediator with discovered pipeline behaviors')
    .When('I send a TJing request')
    .&Then('the pipeline trace should be outer then inner then handler')

  .Scenario('Behaviors execute around handler in registration order')
    .Given('a configured mediator with ordered pipeline behaviors')
    .When('I send a TJing request')
    .&Then('the pipeline trace should be outer then inner then handler')

  .Scenario('Behavior can short-circuit request execution')
    .Given('a configured mediator with short-circuit behavior')
    .When('I send a TJing request')
    .&Then('the pipeline trace should show short-circuit')

  .Scenario('Publish notifications bypass request pipeline behaviors')
    .Given('a configured mediator with ordered pipeline behaviors')
    .When('I publish a TPonged notification')
    .&Then('handler 1 should have been invoked')

  .Scenario('Unhandled exception from handler propagates to caller')
    .Given('a configured mediator')
    .When('I send a TBoomRequest')
    .&Then('an exception should have been raised')

  .Scenario('Typed behavior does not execute for non-matching request')
    .Given('a configured mediator with typed ping behavior')
    .When('I send a TJing request')
    .&Then('the pipeline trace should not contain typed behavior')

  .Scenario('Typed behavior executes for matching request type')
    .Given('a configured mediator with typed ping behavior')
    .When('I send a TPing request and get Pong1')
    .&Then('the pipeline trace should contain typed behavior')

  .Scenario('Global behavior executes for response requests')
    .Given('a configured mediator with global outer behavior')
    .When('I send a TPing request and get Pong1')
    .&Then('the pipeline trace should contain outer behavior')

  .Scenario('Typed behavior applies to descendant request type')
    .Given('a configured mediator with typed ping behavior for ping descendants')
    .When('I send a TPingChild request')
    .&Then('the pipeline trace should contain typed behavior')

  .Scenario('Response behavior can short-circuit and return custom value')
    .Given('a configured mediator with response short-circuit behavior')
    .When('I send a TPing request via response short-circuit')
    .&Then('the response short-circuit value should be returned')

  .Scenario('Response behavior can modify the value returned by handler')
    .Given('a configured mediator with response-modifying behavior')
    .When('I send a TPing request via response-modifying behavior')
    .&Then('the response should be the modified value')

  .Scenario('Behavior can catch exception thrown by handler')
    .Given('a configured mediator with exception-catching behavior')
    .When('I send a TBoomRequest without exception propagating')
    .&Then('the pipeline trace should show the exception was caught')

  .Scenario('Global and typed behaviors both execute for matching request')
    .Given('a configured mediator with global and typed behaviors')
    .When('I send a TPing request and get Pong1')
    .&Then('both behaviors should appear in the pipeline trace')

  .Scenario('Behavior with injected dependency receives it from the container')
    .Given('a configured mediator with dependency-aware behavior')
    .When('I send a TJing request')
    .&Then('the behavior dependency should appear in the pipeline trace')

  .Scenario('Two typed response behaviors execute in order around handler')
    .Given('a configured mediator with two typed ping behaviors')
    .When('I send a TPing request and get Pong1')
    .&Then('the pipeline trace should show both ping behaviors in order')

  .Scenario('Nested Send inside a behavior invokes handler for nested request')
    .Given('a configured mediator with nested-send behavior')
    .When('I send a TPing request and get Pong1')
    .&Then('the TJing handler should have been invoked')

// --- Typed void pipeline behaviors ---

.Rule('Typed void pipeline behaviors')

  .Scenario('Typed void behavior executes for matching void request')
    .Given('a configured mediator with typed void tracing behavior')
    .When('I send a TJing request')
    .&Then('the pipeline trace should contain the void-before marker')

  .Scenario('Typed void behavior can call Next to continue the chain')
    .Given('a configured mediator with typed void tracing behavior')
    .When('I send a TJing request')
    .&Then('the TJing handler should have been invoked')

  .Scenario('Typed void behavior is skipped for a different void request type')
    .Given('a configured mediator with void tracing behavior for non-matching request')
    .When('I send a TJing request')
    .&Then('the pipeline trace should not contain the void-before marker')

  .Scenario('Typed void behavior short-circuits the chain')
    .Given('a configured mediator with typed void short-circuit behavior')
    .When('I send a TJing request')
    .&Then('the TJing handler should not have been invoked')

// --- Pipeline error handling ---

.Rule('Pipeline error handling')

  .Scenario('No handler registered for void request raises exception')
    .Given('a configured mediator')
    .When('I send an unhandled void request')
    .&Then('an exception should have been raised')

  .Scenario('No handler registered for response request raises exception')
    .Given('a configured mediator')
    .When('I send an unhandled response request')
    .&Then('an exception should have been raised')

  .Scenario('Publish with no subscribers succeeds silently')
    .Given('a configured mediator')
    .When('I publish an unsubscribed notification')
    .&Then('no exception should have been raised')

  .Scenario('Behavior raises before Next — exception reaches caller')
    .Given('a configured mediator with before-raising behavior')
    .When('I send a TJing request')
    .&Then('an exception should have been raised')

  .Scenario('Behavior raises after Next — exception reaches caller')
    .Given('a configured mediator with after-raising behavior')
    .When('I send a TPing request via after-raising behavior')
    .&Then('an exception should have been raised');

end.
