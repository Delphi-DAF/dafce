unit MediatR.Feat;

interface

uses
  Daf.MediatR.Contracts,
  Daf.Extensions.DependencyInjection;

type
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
    constructor Create(const Dependency: IDependencyMock);
    procedure Handle(TRequest: TPing; out Result: string);
  end;

  TJing = class(TInterfacedObject, IRequest)
  end;

  TJingHandler = class(TInterfacedObject, IRequestHandler<TJing>)
  public
    class var Done: Boolean;
    procedure Handle(TRequest: TJing);
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

  TMediatRWorld = class
  public
    ServiceCollection: IServiceCollection;
    RootProvider: IServiceProvider;
    Mediator: IMediator;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,
  System.Rtti,
  Daf.MiniSpec,
  Daf.MiniSpec.Types,
  Daf.Rtti,
  Daf.DependencyInjection,
  Daf.MediatR.DependencyInjection;

{ TDependencyMock }

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
  FDependency.Visit;
  Result := 'Pong' + FDependency.Visites.ToString;
end;

{ TJingHandler }

procedure TJingHandler.Handle(TRequest: TJing);
begin
  Done := True;
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

// --- Named Step Procedures ---

procedure Step_GetMediator(W: TMediatRWorld);
begin
  W.Mediator := W.RootProvider.GetRequiredService<IMediatorImpl>;
end;

procedure Step_GetMediator_ResetJing(W: TMediatRWorld);
begin
  W.Mediator := W.RootProvider.GetRequiredService<IMediatorImpl>;
  TJingHandler.Done := False;
end;

procedure Step_SendJing(W: TMediatRWorld);
begin
  W.Mediator.Send(TJing.Create);
end;

procedure Step_VerifyJingHandled(W: TMediatRWorld);
begin
  Expect(TJingHandler.Done).ToBeTrue;
end;

procedure Step_SendPingAndVerifyResponse(W: TMediatRWorld);
var
  Response: string;
begin
  Response := W.Mediator.Send<string, TPing>(TPing.Create);
  Expect(Response).ToEqual('Pong1');
end;

procedure Step_VerifyDependencyVisited(W: TMediatRWorld);
var
  D: IDependencyMock;
begin
  D := W.RootProvider.GetRequiredService<IDependencyMock>;
  Expect(D.Visites).ToEqual(1);
end;

procedure Step_GetMediator_ResetPonged(W: TMediatRWorld);
begin
  W.Mediator := W.RootProvider.GetRequiredService<IMediatorImpl>;
  TPongedHandler1.Done := False;
  TPongedHandler2.Done := False;
end;

procedure Step_PublishPonged(W: TMediatRWorld);
begin
  W.Mediator.Publish(TPonged.Create);
end;

procedure Step_VerifyPongedHandler1(W: TMediatRWorld);
begin
  Expect(TPongedHandler1.Done).ToBeTrue;
end;

procedure Step_VerifyPongedHandler2(W: TMediatRWorld);
begin
  Expect(TPongedHandler2.Done).ToBeTrue;
end;

procedure Step_Noop(W: TMediatRWorld);
begin
  // World constructor already configured the root provider
end;

procedure Step_SendPingFromScopes(W: TMediatRWorld);
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

procedure Step_VerifyScopedDependencies(W: TMediatRWorld);
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
    .Given('a configured mediator', Step_GetMediator_ResetJing)
    .When('I send a TJing request', Step_SendJing)
    .&Then('the handler should have been invoked', Step_VerifyJingHandled)

// --- Send with Response ---

.Rule('Send requests with response')

  .Scenario('Can send a request and receive a response')
    .Given('a configured mediator', Step_GetMediator)
    .When('I send a TPing request', Step_SendPingAndVerifyResponse)
    .&Then('the dependency mock should have been visited once', Step_VerifyDependencyVisited)

// --- Publish Notifications ---

.Rule('Publish notifications')

  .Scenario('Can publish a notification to multiple handlers')
    .Given('a configured mediator', Step_GetMediator_ResetPonged)
    .When('I publish a TPonged notification', Step_PublishPonged)
    .&Then('handler 1 should have been invoked', Step_VerifyPongedHandler1)
    .&And('handler 2 should have been invoked', Step_VerifyPongedHandler2)

// --- Scoped Mediation ---

.Rule('Scoped mediation')

  .Scenario('Scoped mediators use scoped dependencies')
    .Given('a configured root provider', Step_Noop)
    .When('I send requests from different scopes', Step_SendPingFromScopes)
    .&Then('each scope has its own dependency instance', Step_VerifyScopedDependencies);

end.
