unit Binding.Feat;

{
  How an action parameter gets its value when it is not a route segment. The controllers are
  in the interface section for the same reason as in Routing.Feat: the router finds them by
  RTTI.
}

interface

uses
  System.Classes,
  Web.HTTPApp,
  Daf.Extensions.DependencyInjection,
  Daf.Web.Router,
  Daf.Web.Controller,
  Web.Doubles;

type
  ISpecClock = interface(IInvokable)
    ['{EB9C213C-4DE6-4A5D-A307-1155B0AE35F6}']
    function Tick: string;
  end;

  TSpecClock = class(TInterfacedObject, ISpecClock)
  public
    function Tick: string;
  end;

  /// Never registered: what a [FromServices] parameter meets when the composition forgot it.
  ISpecUnregistered = interface(IInvokable)
    ['{17025591-6126-4E7D-9B57-F2CB763AB091}']
    function Name: string;
  end;

  /// What a [FromBody] parameter is built into. Its constructor sets Origin, which no JSON in the
  /// specs carries, and it counts its live instances whichever constructor builds it.
  TSpecOrder = class
  private
    FProduct: string;
    FQuantity: Integer;
    FOrigin: string;
  public
    class var Alive: Integer;
    constructor Create;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property Product: string read FProduct;
    property Quantity: Integer read FQuantity;
    property Origin: string read FOrigin;
  end;

  [WebRoutePrefix('/spec/bind')]
  TBindingController = class(TWebController)
  public
    /// The calls the actions below received, and how many TSpecOrder were alive during the last
    /// call to PlaceOrder.
    class var Calls: Integer;
    class var AliveDuringCall: Integer;
    [WebRoute(mtGet, '/clock')]
    function ReadClock([FromServices] Clock: ISpecClock): IActionResult;
    [WebRoute(mtGet, '/unregistered')]
    function Unregistered([FromServices] Missing: ISpecUnregistered): IActionResult;
    [WebRoute(mtGet, '/find')]
    function Find([FromQuery] Q: string): IActionResult;
    [WebRoute(mtPost, '/order')]
    function PlaceOrder([FromBody] Order: TSpecOrder): IActionResult;
    [WebRoute(mtGet, '/price/{Amount}')]
    function Price(Amount: Double): IActionResult;
    [WebRoute(mtGet, '/items/{Id}')]
    function Item(Id: Integer): IActionResult;
    [WebRoute(mtGet, '/paged')]
    function Paged([FromQuery] Page: Integer): IActionResult;
    [WebRoute(mtGet, '/align/{Side}')]
    function Align(Side: TAlignment): IActionResult;
  end;

  TBindingWorld = class
  public
    Provider: IServiceProvider;
    RouterRef: IWebRouter;
    Router: TWebRouter;
    Request: TSpecWebRequest;
    Response: TSpecWebResponse;
    /// 'answered <code> [<content>]', or 'raised <class>: <message>'.
    Report: string;
    /// The process decimal separator before a scenario changed it, restored by the destructor.
    SavedDecimalSeparator: Char;
    constructor Create;
    destructor Destroy; override;
    procedure DispatchRequest(const APath: string; const AQuery: string = '');
    procedure PostBody(const APath, ABody: string);
    /// 'status=<code> calls=<n>': what the response said and whether the action ran.
    function Outcome: string;
  end;

implementation

uses
  System.SysUtils,
  System.TypInfo,
  System.IOUtils,
  Daf.DependencyInjection,
  Daf.MiniSpec;

{ TSpecClock }

function TSpecClock.Tick: string;
begin
  Result := 'tick';
end;

{ TSpecOrder }

constructor TSpecOrder.Create;
begin
  inherited Create;
  FOrigin := 'constructor';
end;

procedure TSpecOrder.AfterConstruction;
begin
  inherited;
  Inc(Alive);
end;

procedure TSpecOrder.BeforeDestruction;
begin
  Dec(Alive);
  inherited;
end;

{ TBindingController }

function TBindingController.Price(Amount: Double): IActionResult;
begin
  Inc(Calls);
  Result := Ok(Format('%.2f', [Amount], TFormatSettings.Invariant));
end;

function TBindingController.Item(Id: Integer): IActionResult;
begin
  Inc(Calls);
  Result := Ok(IntToStr(Id));
end;

function TBindingController.Align(Side: TAlignment): IActionResult;
begin
  Inc(Calls);
  Result := Ok(GetEnumName(TypeInfo(TAlignment), Ord(Side)));
end;

function TBindingController.Paged(Page: Integer): IActionResult;
begin
  Inc(Calls);
  Result := Ok(IntToStr(Page));
end;

function TBindingController.PlaceOrder(Order: TSpecOrder): IActionResult;
begin
  Inc(Calls);
  AliveDuringCall := TSpecOrder.Alive;
  Result := Ok(Format('%s x%d from %s', [Order.Product, Order.Quantity, Order.Origin]));
end;

function TBindingController.ReadClock(Clock: ISpecClock): IActionResult;
begin
  Result := Ok(Clock.Tick);
end;

function TBindingController.Unregistered(Missing: ISpecUnregistered): IActionResult;
begin
  Result := Ok('assigned=' + BoolToStr(Assigned(Missing), True));
end;

function TBindingController.Find(Q: string): IActionResult;
begin
  Result := Ok(Q);
end;

{ TBindingWorld }

constructor TBindingWorld.Create;
begin
  inherited;
  SavedDecimalSeparator := FormatSettings.DecimalSeparator;
  TSpecOrder.Alive := 0;
  TBindingController.Calls := 0;
  TBindingController.AliveDuringCall := 0;
  var Services: IServiceCollection := TServiceCollection.Create;
  Services.AddSingleton<ISpecClock, TSpecClock>;
  Provider := Services.BuildServiceProvider;
  Router := TWebRouter.Create(Provider.GetRequiredService<IServiceScopeFactory>);
  RouterRef := Router;
  RouterRef.RootDirectory := TPath.GetTempPath;
end;

destructor TBindingWorld.Destroy;
begin
  FormatSettings.DecimalSeparator := SavedDecimalSeparator;
  Response.Free;
  Request.Free;
  RouterRef := nil;
  Provider.ShutDown;
  inherited;
end;

procedure TBindingWorld.PostBody(const APath, ABody: string);
begin
  Request := TSpecWebRequest.Create('POST', APath, '', ABody);
  Response := TSpecWebResponse.Create(Request);
  var Handled := False;
  try
    Router.WebDispatch(nil, Request, Response, Handled);
    Report := Format('answered %d [%s]', [Response.StatusCode, Response.Content]);
  except
    on E: Exception do
      Report := 'raised ' + E.ClassName + ': ' + E.Message;
  end;
end;

function TBindingWorld.Outcome: string;
begin
  Result := Format('status=%d calls=%d', [Response.StatusCode, TBindingController.Calls]);
end;

procedure TBindingWorld.DispatchRequest(const APath, AQuery: string);
begin
  Request := TSpecWebRequest.Create('GET', APath, AQuery);
  Response := TSpecWebResponse.Create(Request);
  var Handled := False;
  try
    Router.WebDispatch(nil, Request, Response, Handled);
    Report := Format('answered %d [%s]', [Response.StatusCode, Response.Content]);
  except
    on E: Exception do
      Report := 'raised ' + E.ClassName + ': ' + E.Message;
  end;
end;

initialization

Feature('''
Feature Binding action parameters @routing @binding

  As an application serving HTTP
  I want an action parameter to say where its value comes from
  So an action takes a service or a query value the way it takes a route segment
''')

.UseWorld<TBindingWorld>

.Rule('A [FromServices] parameter is resolved from the request''s services')

  .Scenario('A registered service reaches the action')
    .When('a request reaches an action that takes a registered service by [FromServices]',
      procedure(W: TBindingWorld)
      begin
        W.DispatchRequest('/spec/bind/clock');
      end)
    .&Then('the action answers with what only that service could give',
      procedure(W: TBindingWorld)
      begin
        Expect(W.Report).ToEqual('answered 200 [tick]');
      end)

  .Scenario('A service nobody registered fails naming it')
    .When('a request reaches an action that takes an unregistered service by [FromServices]',
      procedure(W: TBindingWorld)
      begin
        W.DispatchRequest('/spec/bind/unregistered');
      end)
    .&Then('the request fails naming the service, and the action never sees a nil',
      procedure(W: TBindingWorld)
      begin
        Expect(W.Report).ToContain('raised ');
        Expect(W.Report).ToContain('ISpecUnregistered');
      end)

.Rule('A [FromQuery] parameter is read from the query string by its name')

  .Scenario('A query value reaches the action')
    .When('a request carries the value in its query string',
      procedure(W: TBindingWorld)
      begin
        W.DispatchRequest('/spec/bind/find', 'q=drill');
      end)
    .&Then('the parameter receives it',
      procedure(W: TBindingWorld)
      begin
        Expect(W.Report).ToEqual('answered 200 [drill]');
      end)

  .Scenario('An absent query value arrives empty')
    .When('a request carries no such value',
      procedure(W: TBindingWorld)
      begin
        W.DispatchRequest('/spec/bind/find');
      end)
    .&Then('the parameter is empty, and deciding whether that is an error is the action''s',
      procedure(W: TBindingWorld)
      begin
        Expect(W.Report).ToEqual('answered 200 []');
      end)

.Rule('A [FromBody] parameter goes through its constructor and lives as long as the request')

  .Scenario('The object goes through its constructor')
    .Given('an action whose [FromBody] class sets in its constructor a field the JSON does not carry',
      procedure(W: TBindingWorld)
      begin
        var Probe := TSpecOrder.Create;
        try
          Expect(Probe.Origin).ToEqual('constructor');
        finally
          Probe.Free;
        end;
      end)
    .When('a request arrives with a JSON body that fills the other fields',
      procedure(W: TBindingWorld)
      begin
        W.PostBody('/spec/bind/order', '{"product":"drill","quantity":3}');
      end)
    .&Then('the action receives the JSON values, and in the absent field the one the constructor set',
      procedure(W: TBindingWorld)
      begin
        Expect(W.Report).ToEqual('answered 200 [drill x3 from constructor]');
      end)

  .Scenario('Nothing bound outlives the request')
    .Given('an action with a [FromBody] parameter whose class counts its live instances',
      procedure(W: TBindingWorld)
      begin
        Expect(TSpecOrder.Alive).ToEqual(0);
      end)
    .When('a request with a JSON body is served',
      procedure(W: TBindingWorld)
      begin
        W.PostBody('/spec/bind/order', '{"product":"drill","quantity":3}');
      end)
    .&Then('the action received one instance, and none is alive once the request is over',
      procedure(W: TBindingWorld)
      begin
        Expect(Format('during=%d after=%d', [TBindingController.AliveDuringCall, TSpecOrder.Alive]))
          .ToEqual('during=1 after=0');
      end)

.Rule('A body that is not a JSON object is rejected with 400')

  .Scenario('A malformed body')
    .When('a request whose body is not valid JSON reaches an action with [FromBody]',
      procedure(W: TBindingWorld)
      begin
        W.PostBody('/spec/bind/order', 'this is not json');
      end)
    .&Then('the response is 400, and the action was not invoked',
      procedure(W: TBindingWorld)
      begin
        Expect(W.Outcome).ToEqual('status=400 calls=0');
      end)

  .Scenario('An empty body')
    .When('a request with no body reaches an action with [FromBody]',
      procedure(W: TBindingWorld)
      begin
        W.PostBody('/spec/bind/order', '');
      end)
    .&Then('the response is 400, and the action was not invoked',
      procedure(W: TBindingWorld)
      begin
        Expect(W.Outcome).ToEqual('status=400 calls=0');
      end)

.Rule('Route and query values convert with the invariant culture')

  .Scenario('A route decimal in a process with a decimal comma')
    .Given('a process whose format uses a comma as the decimal separator',
      procedure(W: TBindingWorld)
      begin
        FormatSettings.DecimalSeparator := ',';
        Expect(FormatFloat('0.0', 1.5)).ToEqual('1,5');
      end)
    .When('/price/1.5 reaches an action whose route parameter is a Double',
      procedure(W: TBindingWorld)
      begin
        W.DispatchRequest('/spec/bind/price/1.5');
      end)
    .&Then('the action receives 1.5 and the answer is 200',
      procedure(W: TBindingWorld)
      begin
        Expect(W.Report).ToEqual('answered 200 [1.50]');
      end)

  .Scenario('A query integer')
    .When('a request with page=3 reaches an action with [FromQuery] Page: Integer',
      procedure(W: TBindingWorld)
      begin
        W.DispatchRequest('/spec/bind/paged', 'page=3');
      end)
    .&Then('the action receives 3',
      procedure(W: TBindingWorld)
      begin
        Expect(W.Report).ToEqual('answered 200 [3]');
      end)

.Rule('A route or query value that does not convert is rejected with 400')

  .Scenario('A route integer that is not a number')
    .When('/items/abc reaches an action whose route parameter is an Integer',
      procedure(W: TBindingWorld)
      begin
        W.DispatchRequest('/spec/bind/items/abc');
      end)
    .&Then('the answer is 400 and names the parameter, and the action was not invoked',
      procedure(W: TBindingWorld)
      begin
        Expect(W.Outcome).ToEqual('status=400 calls=0');
        Expect(W.Response.Content).ToContain('"Id"');
      end)

  .Scenario('A route enumeration that is neither a name nor a number')
    .When('/align/Middle reaches an action whose route parameter is a TAlignment',
      procedure(W: TBindingWorld)
      begin
        W.DispatchRequest('/spec/bind/align/Middle');
      end)
    .&Then('the answer is 400 and names the parameter, and the action was not invoked',
      procedure(W: TBindingWorld)
      begin
        Expect(W.Outcome).ToEqual('status=400 calls=0');
        Expect(W.Response.Content).ToContain('"Side"');
      end)

  .Scenario('A query integer that is not a number')
    .When('a request with page=abc reaches an action with [FromQuery] Page: Integer',
      procedure(W: TBindingWorld)
      begin
        W.DispatchRequest('/spec/bind/paged', 'page=abc');
      end)
    .&Then('the answer is 400 and names the parameter, and the action was not invoked',
      procedure(W: TBindingWorld)
      begin
        Expect(W.Outcome).ToEqual('status=400 calls=0');
        Expect(W.Response.Content).ToContain('"Page"');
      end)

.Rule('An absent query value arrives as its type''s default')

  .Scenario('An absent query integer')
    .When('a request without page reaches an action with [FromQuery] Page: Integer',
      procedure(W: TBindingWorld)
      begin
        W.DispatchRequest('/spec/bind/paged');
      end)
    .&Then('the action receives 0 and the answer is 200',
      procedure(W: TBindingWorld)
      begin
        Expect(W.Report).ToEqual('answered 200 [0]');
      end);

end.
