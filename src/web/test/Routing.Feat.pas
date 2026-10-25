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
  Web.Doubles;

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

  // The parameter route is declared before the literal one: what the rule below asserts is
  // that the literal wins whatever order the two were declared in.
  [WebRoutePrefix('/spec/catalog')]
  TCatalogController = class(TWebController)
  public
    [WebRoute(mtGet, '/{Id}')]
    function ById(const Id: string): IActionResult;
    [WebRoute(mtGet, '/search')]
    function Search: IActionResult;
  end;

  /// Waits, up to five seconds, for a second request to reach the same action: only two requests
  /// served at once both answer with their own value.
  [WebRoutePrefix('/spec/meet')]
  TMeetingController = class(TWebController)
  public
    [WebRoute(mtGet, '/{Tag}')]
    function Meet(const Tag: string): IActionResult;
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
    /// What each of two concurrent requests answered.
    FirstReport: string;
    SecondReport: string;
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
  System.Classes,
  System.SyncObjs,
  Daf.MiniSpec;

var
  Arrivals: Integer;
  BothArrived: TEvent;

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

{ TCatalogController }

function TCatalogController.ById(const Id: string): IActionResult;
begin
  InvokedAction := 'TCatalogController.ById';
  Result := Ok(Id);
end;

function TCatalogController.Search: IActionResult;
begin
  InvokedAction := 'TCatalogController.Search';
  Result := Ok('search');
end;

{ TMeetingController }

function TMeetingController.Meet(const Tag: string): IActionResult;
begin
  if TInterlocked.Increment(Arrivals) = 2 then
    BothArrived.SetEvent;
  if BothArrived.WaitFor(5000) = wrSignaled then
    Result := Ok(Tag)
  else
    Result := Ok('alone:' + Tag);
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
  FreeAndNil(BothArrived);
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

.Rule('A literal segment wins over a parameter, whatever the order they were declared in')

  .Scenario('A literal route wins over a parameter route declared before it')
    .When('a GET arrives on a path both routes of a controller accept',
      procedure(W: TRoutingWorld)
      begin
        W.DispatchRequest('GET', '/spec/catalog/search');
      end)
    .&Then('the literal route is the one invoked',
      procedure(W: TRoutingWorld)
      begin
        Expect(InvokedAction).ToEqual('TCatalogController.Search');
      end)

  .Scenario('The parameter still takes what no literal claims')
    .When('a GET arrives on a path only the parameter route accepts',
      procedure(W: TRoutingWorld)
      begin
        W.DispatchRequest('GET', '/spec/catalog/42');
      end)
    .&Then('the parameter route is invoked with the segment',
      procedure(W: TRoutingWorld)
      begin
        Expect(InvokedAction).ToEqual('TCatalogController.ById');
        Expect(W.Response.Content).ToEqual('42');
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
      end)

.Rule('Requests are served at once')

  .Scenario('Two requests at once')
    .Given('an action that waits, with a deadline, for another request to reach it',
      procedure(W: TRoutingWorld)
      begin
        Arrivals := 0;
        BothArrived := TEvent.Create(nil, True, False, '');
      end)
    .When('two requests arrive from two threads, each with its own route value',
      procedure(W: TRoutingWorld)
      begin
        var Serve :=
          function(const Tag: string): TProc
          begin
            Result :=
              procedure
              begin
                var Request := TSpecWebRequest.Create('GET', '/spec/meet/' + Tag);
                var Response := TSpecWebResponse.Create(Request);
                try
                  var Handled := False;
                  W.Router.WebDispatch(nil, Request, Response, Handled);
                  var Answer := Format('answered %d [%s]', [Response.StatusCode, Response.Content]);
                  if Tag = 'a' then
                    W.FirstReport := Answer
                  else
                    W.SecondReport := Answer;
                finally
                  Response.Free;
                  Request.Free;
                end;
              end;
          end;
        var First := TThread.CreateAnonymousThread(Serve('a'));
        var Second := TThread.CreateAnonymousThread(Serve('b'));
        First.FreeOnTerminate := False;
        Second.FreeOnTerminate := False;
        try
          First.Start;
          Second.Start;
          First.WaitFor;
          Second.WaitFor;
        finally
          First.Free;
          Second.Free;
        end;
      end)
    .&Then('both answer 200 within the deadline, each with its own route value',
      procedure(W: TRoutingWorld)
      begin
        Expect(W.FirstReport + ' | ' + W.SecondReport).ToEqual('answered 200 [a] | answered 200 [b]');
      end);

end.
