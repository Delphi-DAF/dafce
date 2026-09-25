unit Binding.Feat;

{
  How an action parameter gets its value when it is not a route segment. The controllers are
  in the interface section for the same reason as in Routing.Feat: the router finds them by
  RTTI.
}

interface

uses
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

  [WebRoutePrefix('/spec/bind')]
  TBindingController = class(TWebController)
  public
    [WebRoute(mtGet, '/clock')]
    function ReadClock([FromServices] Clock: ISpecClock): IActionResult;
    [WebRoute(mtGet, '/unregistered')]
    function Unregistered([FromServices] Missing: ISpecUnregistered): IActionResult;
    [WebRoute(mtGet, '/find')]
    function Find([FromQuery] Q: string): IActionResult;
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
    constructor Create;
    destructor Destroy; override;
    procedure DispatchRequest(const APath: string; const AQuery: string = '');
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  Daf.DependencyInjection,
  Daf.MiniSpec;

{ TSpecClock }

function TSpecClock.Tick: string;
begin
  Result := 'tick';
end;

{ TBindingController }

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
  var Services: IServiceCollection := TServiceCollection.Create;
  Services.AddSingleton<ISpecClock, TSpecClock>;
  Provider := Services.BuildServiceProvider;
  Router := TWebRouter.Create(Provider.GetRequiredService<IServiceScopeFactory>);
  RouterRef := Router;
  RouterRef.RootDirectory := TPath.GetTempPath;
end;

destructor TBindingWorld.Destroy;
begin
  Response.Free;
  Request.Free;
  RouterRef := nil;
  Provider.ShutDown;
  inherited;
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
      end);

end.
