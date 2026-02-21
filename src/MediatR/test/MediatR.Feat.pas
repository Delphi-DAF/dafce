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
    .&Then('each scope has its own dependency instance');

end.
