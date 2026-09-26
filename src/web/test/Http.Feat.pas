unit Http.Feat;

{
  The request path end to end: a real socket, the variant's own web module and the router
  behind it. Routing.Feat dispatches to the router directly and stops where the router hands a
  request back; what the server then answers is only visible from here.
}

interface

uses
  Web.HTTPApp,
  Daf.Web.Controller,
  Daf.Web.Application;

type
  [WebRoutePrefix('/spec/http')]
  THttpSpecController = class(TWebController)
  public
    [WebRoute(mtGet, '/ping')]
    function Ping: IActionResult;
  end;

  THttpWorld = class
  public
    App: IWebApplication;
    Port: Word;
    /// The status code and the body of the last answer, as '<code> <body>'.
    Report: string;
    destructor Destroy; override;
    procedure Serve;
    function Get(const Path: string): string;
  end;

implementation

uses
  System.SysUtils,
  IdHTTP,
  Daf.MiniSpec,
  Web.Doubles;

{ THttpSpecController }

function THttpSpecController.Ping: IActionResult;
begin
  Result := Ok('pong');
end;

{ THttpWorld }

destructor THttpWorld.Destroy;
begin
  if Assigned(App) then
    App.Server.Stop;
  App := nil;
  inherited;
end;

procedure THttpWorld.Serve;
begin
  Port := FindFreePort;
  var Builder := TWebApplication.CreateBuilder(['--Web:Port=' + Port.ToString]);
  WebApp.AddHttpServer(Builder.Services);
  App := Builder.Build;
  App.MapControllers;
  App.Server.Start;
end;

function THttpWorld.Get(const Path: string): string;
begin
  var Http := TIdHTTP.Create(nil);
  try
    // Without these TIdHTTP raises on any code outside 2xx, and the code is what is asserted.
    Http.HTTPOptions := Http.HTTPOptions + [hoNoProtocolErrorException, hoWantProtocolErrorContent];
    var Body := Http.Get(Format('http://127.0.0.1:%d%s', [Port, Path]));
    Result := Format('%d %s', [Http.ResponseCode, Body.Trim]);
  finally
    Http.Free;
  end;
end;

initialization

Feature('''
Feature Serving requests over HTTP @http

  As an application serving controllers
  I want every request to get an HTTP answer that says what happened
  So a client can tell a missing route from a failure of the server
''')

.UseWorld<THttpWorld>

.Rule('A path no action declares is answered 404 @socket')

  .Scenario('As the first request the server receives')
    .Given('a web application serving its controllers',
      procedure(W: THttpWorld)
      begin
        W.Serve;
      end)
    .When('the first request asks for a path no action declares',
      procedure(W: THttpWorld)
      begin
        W.Report := W.Get('/spec/http/nothing-declared-here');
      end)
    .&Then('the answer is a 404, not a server error',
      procedure(W: THttpWorld)
      begin
        Expect(W.Report).ToEqual('404 Not found');
      end)

  .Scenario('After a request that matched')
    .Given('a web application that has already answered a declared route',
      procedure(W: THttpWorld)
      begin
        W.Serve;
        Expect(W.Get('/spec/http/ping')).ToEqual('200 pong');
      end)
    .When('a request asks for a path no action declares',
      procedure(W: THttpWorld)
      begin
        W.Report := W.Get('/spec/http/nothing-declared-here');
      end)
    .&Then('the answer is a 404, not a server error',
      procedure(W: THttpWorld)
      begin
        Expect(W.Report).ToEqual('404 Not found');
      end)

.Rule('A declared route answers through the same path @socket')

  .Scenario('A declared route answers 200 with its result')
    .Given('a web application serving its controllers',
      procedure(W: THttpWorld)
      begin
        W.Serve;
      end)
    .When('a request asks for a path an action declares',
      procedure(W: THttpWorld)
      begin
        W.Report := W.Get('/spec/http/ping');
      end)
    .&Then('the action answers',
      procedure(W: THttpWorld)
      begin
        Expect(W.Report).ToEqual('200 pong');
      end);

end.
