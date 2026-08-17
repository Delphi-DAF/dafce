unit Routing.Feat;

{
  Attribute routing is the module's actual value and the part that needs no
  socket: a request reaches a method because the RTTI says so, and the
  controller is built by the container like anything else.

  The controllers below are declared in the interface section on purpose. The
  router finds them by scanning RTTI, and the linker drops types nothing
  references — which is also why the runner turns STRONGLINKTYPES on.
}

interface

uses
  Web.HTTPApp,
  Daf.Extensions.DependencyInjection,
  Daf.DependencyInjection,
  Daf.Web.Router,
  Daf.Web.Controller,
  Doubles.Feat;

type
  IGreeter = interface(IInvokable)
    ['{7A2A5A6D-4B18-4C0E-9B2E-2C9C0E1B7F41}']
    function Greet(const Name: string): string;
  end;

  TGreeter = class(TInterfacedObject, IGreeter)
  public
    function Greet(const Name: string): string;
  end;

  [WebRoutePrefix('/spec/greet')]
  TGreetController = class(TWebController)
  private
    FGreeter: IGreeter;
  public
    constructor Create(const Greeter: IGreeter); reintroduce;
    [WebRoute(mtGet, '/{Name}')]
    function Hello(const Name: string): IActionResult;
  end;

  [WebRoutePrefix('/spec/orders')]
  TOrdersController = class(TWebController)
  public
    [WebRoute(mtPost, '/{Id}')]
    function Place(const Id: string): IActionResult;
  end;

  TRoutingWorld = class
  public
    Services: IServiceCollection;
    Provider: IServiceProvider;
    RouterRef: IWebRouter;
    Router: TWebRouter;
    Request: TSpecWebRequest;
    Response: TSpecWebResponse;
    Handled: Boolean;
    Controller: TGreetController;
    constructor Create;
    destructor Destroy; override;
    procedure DispatchRequest(const AMethod, APath: string);
  end;

var
  /// What the last action to run was, or '' when none did. The router builds
  /// and frees the controller itself, so this is the only seam a spec has to
  /// tell "the action did not run" from "it ran and answered nothing".
  InvokedAction: string = '';

implementation

uses
  System.SysUtils,
  System.IOUtils,
  Daf.MiniSpec;

{ TGreeter }

function TGreeter.Greet(const Name: string): string;
begin
  Result := 'greeted:' + Name;
end;

{ TGreetController }

constructor TGreetController.Create(const Greeter: IGreeter);
begin
  inherited Create;
  FGreeter := Greeter;
end;

function TGreetController.Hello(const Name: string): IActionResult;
begin
  InvokedAction := 'TGreetController.Hello';
  Result := Ok(FGreeter.Greet(Name));
end;

{ TOrdersController }

function TOrdersController.Place(const Id: string): IActionResult;
begin
  InvokedAction := 'TOrdersController.Place';
  Result := Created('/spec/orders/' + Id);
end;

{ TRoutingWorld }

constructor TRoutingWorld.Create;
begin
  inherited;
  Services := TServiceCollection.Create;
  Services.AddSingleton<IGreeter, TGreeter>;
  Provider := Services.BuildServiceProvider;
  Router := TWebRouter.Create(Provider.GetRequiredService<IServiceScopeFactory>);
  RouterRef := Router;
  RouterRef.RootDirectory := TPath.GetTempPath;
  InvokedAction := '';
end;

destructor TRoutingWorld.Destroy;
begin
  Controller.Free;
  Response.Free;
  Request.Free;
  // The router holds the scope factory, and the factory is registered in its
  // own container: released in this order, and shut down, nothing survives the
  // world. ReportMemoryLeaksOnShutdown makes the mistake visible either way.
  RouterRef := nil;
  Provider.ShutDown;
  Services := nil;
  inherited;
end;

procedure TRoutingWorld.DispatchRequest(const AMethod, APath: string);
begin
  Response.Free;
  Request.Free;
  Request := TSpecWebRequest.Create(AMethod, APath);
  Response := TSpecWebResponse.Create(Request);
  Handled := False;
  Router.WebDispatch(nil, Request, Response, Handled);
end;

initialization

Feature('''
Feature Attribute routing @routing

  As an application serving HTTP
  I want a request to reach a controller method by attribute, built by the container
  So actions read as plain methods and take their collaborators like anything else
''')

.UseWorld<TRoutingWorld>

.Rule('A request reaches a controller method by attribute, resolved through the container')

  .Scenario('A decorated action answers its route')
    .When('a GET arrives on the prefix and path the action declares',
      procedure(W: TRoutingWorld)
      begin
        W.DispatchRequest('GET', '/spec/greet/World');
      end)
    .&Then('that action is invoked and its result becomes the response',
      procedure(W: TRoutingWorld)
      begin
        Expect(InvokedAction).ToEqual('TGreetController.Hello');
        Expect(W.Handled).ToEqual(True);
        Expect(W.Response.StatusCode).ToEqual(200);
      end)

  .Scenario('The verb is part of the match')
    .When('a GET arrives on a path only a POST action declares',
      procedure(W: TRoutingWorld)
      begin
        W.DispatchRequest('GET', '/spec/orders/7');
      end)
    .&Then('no action is invoked, rather than falling through to the other verb',
      procedure(W: TRoutingWorld)
      begin
        Expect(InvokedAction).ToEqual('');
        Expect(W.Handled).ToEqual(False);
      end)

    .&And('the same path with its own verb does reach the action',
      procedure(W: TRoutingWorld)
      begin
        W.DispatchRequest('POST', '/spec/orders/7');
        Expect(InvokedAction).ToEqual('TOrdersController.Place');
        Expect(W.Response.StatusCode).ToEqual(201);
      end)

  .Scenario('A controller receives its dependencies')
    .Given('a greeter registered in the container',
      procedure(W: TRoutingWorld)
      begin
        Expect(W.Provider.GetService<IGreeter>.Greet('X')).ToEqual('greeted:X');
      end)
    .When('a request reaches an action of a controller that takes it as a constructor parameter',
      procedure(W: TRoutingWorld)
      begin
        W.DispatchRequest('GET', '/spec/greet/Ada');
      end)
    .&Then('the answer is the one only the resolved collaborator could give',
      procedure(W: TRoutingWorld)
      begin
        Expect(W.Response.Content).ToEqual('greeted:Ada');
      end)

  .Scenario('An unmatched path is reported, not swallowed')
    .When('a request arrives on a path no action declares',
      procedure(W: TRoutingWorld)
      begin
        W.DispatchRequest('GET', '/spec/nothing-declared-here');
      end)
    .&Then('nothing is invoked and the request is handed back unhandled, not raised',
      procedure(W: TRoutingWorld)
      begin
        // Handed back is what "reported" means here: the router declines, and
        // the not-found response is WebBroker's to produce. What must not
        // happen is an exception reaching the server's error handler.
        Expect(InvokedAction).ToEqual('');
        Expect(W.Handled).ToEqual(False);
        Expect(W.Response.Content).ToEqual('');
      end)

.Rule('A controller is constructible')

  .Scenario('Constructing a controller does not raise')
    .When('a TWebController descendant is constructed',
      procedure(W: TRoutingWorld)
      begin
        // Constructed, not merely referenced. TWebController descends from
        // TDataModule and constructs with an owner, so this is the only shape
        // of spec that reaches the missing resource — the compiler cannot.
        W.Controller := TGreetController.Create(TGreeter.Create);
      end)
    .&Then('it constructs, rather than raising because its resource is not linked',
      procedure(W: TRoutingWorld)
      begin
        Expect(Assigned(W.Controller)).ToEqual(True);
      end);

end.
