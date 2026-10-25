# Daf.Web — Usage Guide

**🌍 Language: English | [Español](GUIDE.es.md)**

---

## Table of Contents

1. [Architecture](#architecture)
2. [Controllers](#controllers)
3. [Routing attributes](#routing-attributes)
4. [Parameter binding](#parameter-binding)
5. [Action results reference](#action-results-reference)
6. [The Web application variant](#the-web-application-variant)
7. [TDafHttpServer](#tdafhttpserver)
8. [DI integration](#di-integration)
9. [Custom action results](#custom-action-results)

---

## Architecture

```
HTTP Request
     │
     ▼
TDafHttpServer  (Indy TIdHTTPWebBrokerBridge)
     │
     ▼
TWebModule      (WebBroker dispatcher)
     │
     ▼
TWebRouter      (BeforeDispatch — matches [WebRoute] by RTTI)
     │
     ▼
TWebController  (built for this request from the service scope)
     │
     ▼
Action method → IActionResult → HTTP Response
```

---

## Controllers

### TWebController

A controller descends from `TWebController`. This is the class the router builds, and the one to use.

```pascal
uses Web.HTTPApp, Daf.Web.Controller;

type
  [WebRoutePrefix('/api/users')]
  TUserController = class(TWebController)
  private
    FService: IUserService;
  public
    constructor Create(const Service: IUserService); reintroduce;
    [WebRoute(mtGet, '/{id}')]
    function GetUser(Id: Integer): IActionResult;
  end;

constructor TUserController.Create(const Service: IUserService);
begin
  inherited Create;
  FService := Service;
end;

function TUserController.GetUser(Id: Integer): IActionResult;
var User: TUserDTO;
begin
  User := FService.FindById(Id);
  if User = nil then
    Result := NotFound
  else
    Result := Ok(User);
end;
```

`TWebController` extends `TDataModule`, which is what makes it usable from the IDE designer alongside WebBroker action items. Action methods must be `public` or `published`, or RTTI will not see them.

### TWebControllerBase

A plain-class variant that does not descend from `TDataModule`. **The router does not build it**: it constructs the matched class and treats it as a `TWebController`, so routing a `TWebControllerBase` descendant is not supported. Use it only outside the router, or not at all.

---

## Routing attributes

### [WebRoutePrefix]

Applied to the controller class. All route paths in the class are prefixed.

```pascal
[WebRoutePrefix('/api/users')]
TUserController = class(TWebController)
```

### [WebRoute]

Applied to an action method. The verb is a `TMethodType`, not a string — the constants live in `Web.HTTPApp`.

```pascal
[WebRoute(mtGet,    '/')]          // GET /api/users/
[WebRoute(mtGet,    '/{id}')]      // GET /api/users/42
[WebRoute(mtPost,   '/')]          // POST /api/users/
[WebRoute(mtPut,    '/{id}')]      // PUT /api/users/42
[WebRoute(mtDelete, '/{id}')]      // DELETE /api/users/42
[WebRoute(mtAny,    '/health')]    // any verb
```

Route segments written `{name}` are path parameters — matched into method parameters by name.

A literal segment is tried before a parameter in the same position, whatever order the actions are
declared in: with `/{id}` and `/me` on the same prefix, `/api/users/me` reaches the `/me` action. Two
routes a path matches alike — `/{id}` and `/{name}` on the same verb — are not an error: the first one
the router discovered takes the request.

A request whose path matches but whose verb no action declares is **not** dispatched to an action of another verb: the router leaves it unhandled, and the variant's web module answers `404 Not found`, as it does for a path no action declares. A web module of your own passed to `WebApp.AddHttpServer` answers those requests as it decides.

---

## Parameter binding

| Attribute | Source | Example |
|-----------|--------|---------|
| *(none)* | Path segment | `[WebRoute(mtGet,'/{id}')] function Get(Id: Integer)` |
| `[FromQuery]` | Query string | `function Search([FromQuery] Q: string)` |
| `[FromBody]` | Request body (JSON) | `function Create([FromBody] Cmd: TCreateUserCmd)` |
| `[FromServices]` | DI container | `function Ping([FromServices] Clock: IClock)` |

A parameter typed `TWebRequest` or `TWebResponse` receives the request or the response itself, with no attribute needed.

A `[FromServices]` parameter is an interface, resolved from the request's scope. A service nobody
registered fails the request naming the service: the action never receives a `nil`.

A `[FromBody]` parameter is a class, built with its parameterless constructor and filled from the
body, which has to be a JSON object. An empty body, one that does not parse, another JSON type, or one
that does not fill the class, answers `400 Bad Request` naming the parameter, and the action is not
invoked. The object lives as long as the request: the router frees it after executing the action's
result, so an action that has to keep something copies it.

A route or `[FromQuery]` value converts to its parameter's type with the invariant culture, as in
ASP.NET Core: integers, decimals with a point whatever the process locale, dates in ISO 8601 or the
invariant format, booleans as `true` or `false` in any case, and enumerations by name or by number. A
value that does not convert answers `400 Bad Request` naming the parameter, and the action is not
invoked. A query key the request does not carry arrives as its type's default: empty, zero or false.
A route value arrives decoded, except `%2F`, which stays encoded and inside its segment.

---

## Action results reference

All helpers are protected methods of the controller. Every status helper takes an optional message, which becomes the response **body** — none of them sets a `Location` header.

```pascal
// 2xx
Ok(const Value: TObject): IActionResult;   overload;   // serialized as JSON
Ok(const Value: string): IActionResult;    overload;
Ok(const Value: Boolean): IActionResult;   overload;
Created(const Msg: string = 'Created'): IActionResult;
Accepted(const Msg: string = 'Accepted'): IActionResult;
NoContent(const Msg: string = 'No content'): IActionResult;

// 3xx
MovedPermanently(const Msg: string = 'Moved permanently'): IActionResult;
MovedTemporarily(const Msg: string = 'Moved temporarily'): IActionResult;
NotModified(const Msg: string = 'Not modified'): IActionResult;

// Content
Html(const HtmlText: string): IActionResult;
Page(const FileName: string): IActionResult;      // <root>/pages/<name>.html
Partial(const FileName: string): IActionResult;   // <root>/partials/<name>.html

// 4xx / 5xx
BadRequest(const Msg: string = 'Bad request'): IActionResult;
Unauthorized(const Msg: string = 'Unauthorized'): IActionResult;
Forbidden(const Msg: string = 'Forbidden'): IActionResult;
NotFound(const Msg: string = 'Not found'): IActionResult;
NotAcceptable(const Msg: string = 'Not acceptable'): IActionResult;
Conflict(const Msg: string = 'Conflict'): IActionResult;
UnprocessableEntity(const Msg: string = 'Unprocessable entity'): IActionResult;
InternalServerError(const Msg: string = 'Internal server error'): IActionResult;
```

An action may also return a plain `string`, which is sent as `text/plain`.

---

## The Web application variant

`Daf.Web` is one of the application family, and its entry point is `TWebApplication`. Composing the
host, the server and the routing is four lines:

```pascal
uses
  Daf.Extensions.DependencyInjection,   // IServiceCollection, AddScoped
  Daf.Web.Application;                  // TWebApplication, WebApp

begin
  var Builder := TWebApplication.CreateBuilder;
  Builder.Services.AddScoped<IUserService, TUserService>;

  WebApp.AddHttpServer(Builder.Services);

  var App := Builder.Build;
  App.MapControllers;
  ExitCode := App.Run;
end.
```

- **`WebApp.AddHttpServer`** registers the router and the server. It is a facade of `Daf.Web` rather
  than a member of the shared builder, so the generic package names no variant and the package graph
  stays one-way.
- **`MapControllers`** resolves the router, and resolving it **is** the discovery: the router scans
  the RTTI for `[WebRoute]` as it is built.
- **`Run`** starts the host, starts the server, waits for shutdown and stops both, and answers
  `TAppExit.Ok` when it ends well. The loop belongs to DAF here, so the `.dpr` ends when `Run` ends —
  unlike VCL and FMX, where the loop is the platform's.

### What escapes

`Builder.OnException(Policy)` declares what happens to what escapes, and with which exit code;
without it, `ExitPolicy.LogAndFail`. It is the application family's policy: the same `TAppFailure`
and `ExitPolicy` as the CLI variant, and `Daf.Web.Application` exposes them.

```pascal
begin
  var Builder := TWebApplication.CreateBuilder;
  Builder.OnException(ExitPolicy.LogAndFail());
  try
    Builder.Services.AddScoped<IUserService, TUserService>;
    WebApp.AddHttpServer(Builder.Services);
    var App := Builder.Build;
    App.MapControllers;
    ExitCode := App.Run;
  except
    on E: Exception do
      ExitCode := Builder.HandleException(E);
  end;
end.
```

- What escapes the host's startup, the server's startup or the shutdown goes to the policy, and
  `Run` answers the code it gave the first failure. A startup failure arrives with
  `Failure.Reported` as `True`; the server does not start, and the host is stopped anyway. A
  shutdown failure arrives with `Failure.Services` as `nil`.
- What escapes before `Run` — the composition, `Build` — reaches the same policy when the `.dpr`'s
  `except` hands it to `HandleException`, which answers the exit code. Before `OnException` is
  declared, the default policy decides.
- An `EAbort` does not reach the policy: `Run` answers `TAppExit.Failure` and nothing is reported.
- What fails inside a request does not reach the policy: the server answers it with its error
  response. The RTL's `ApplicationHandleException` hook stays the one WebBroker's request handler
  sets.

### The port

The variant **seeds** `Web:Port` with `8080` before handing the configuration over, so an argument
or an environment variable read afterwards wins:

```
MyApp --Web:Port=9000
```

`AddHttpServer` hands the server the `Web` **section**, which is why the key is `Web:Port` from
outside and plain `Port` inside `TDafHttpServer.Create`.

See [The application family](../../Hosting/docs/APPLICATION-FAMILY.md) for what the four variants share.

---

## TDafHttpServer

```pascal
TDafHttpServer = class(TInterfacedObject, IDafHttpServer)
  constructor Create(const WebModuleClass: TComponentClass;
                     const Config: IConfiguration;
                     const LoggerFactory: ILoggerFactory = nil);
  procedure Start;
  procedure Stop;
  function CanBindPort: Boolean; overload;
  function CanBindPort(const APort: Integer): Boolean; overload;
  property Port      : Word;
  property Active    : Boolean;      // read-only
  property SessionID : string;       // read-only
  // Events: OnStarting, OnStarted, OnStopping, OnStopped, OnError, OnDestroy
end;
```

`Create` reads the port from `Config['Port']`. `Start` refuses to bind a port already in use: it fires `OnError` and then raises an exception that names the port, before touching the socket. `CanBindPort` answers the same question ahead of time, and releases whatever it bound.

### Driving it yourself

`WebApp.AddHttpServer` registers it for you, and that is the route to take. Building one by hand is for
a process that is not a DAF application — a spec harness, an existing program taking DAF in piece by
piece:

```pascal
uses
  Daf.Extensions.HttpServer,   // IDafHttpServer
  Daf.HttpServer;              // TDafHttpServer

var Srv: IDafHttpServer := TDafHttpServer.Create(TMyWebModule, AppConfig, LogFactory);
Srv.Port := 8080;
Srv.Start;
```

⚠️ **The variable is an `IDafHttpServer` and not the class.** `TDafHttpServer` is reference counted,
so holding it by its class type leaves nobody owning it.

⚠️ **What you pass as `AppConfig` is read with the key `Port`, not `Web:Port`.** The facade hands
the server `GetSection('Web')`; a configuration passed straight in is read at its root.

The server is not an `IHostedService` yet — see the note in the [README](../README.md).

---

## DI integration

Each request gets its own scope, and the controller is built inside it from `IServiceProvider`, called, and freed once the response is written. Anything scoped therefore lives and dies with the request.

The controller class itself does **not** need registering: it is constructed by `TActivatorUtilities`, which picks the constructor whose parameters the container can satisfy. What must be registered is what the controller asks for.

```pascal
Services.AddScoped<IUserService, TUserService>;
```

---

## Custom action results

The helpers above cover the usual cases. For a content type they do not produce, inherit `TActionResult` — it implements `IActionResult` and leaves `Execute` abstract:

```pascal
type
  TCsvResult = class(TActionResult)
  private
    FRows: TArray<string>;
  public
    constructor Create(const Rows: TArray<string>);
    procedure Execute(Response: TWebResponse); override;
  end;

constructor TCsvResult.Create(const Rows: TArray<string>);
begin
  inherited Create;
  FRows := Rows;
end;

procedure TCsvResult.Execute(Response: TWebResponse);
begin
  Response.StatusCode  := 200;
  Response.ContentType := 'text/csv; charset=utf-8';
  Response.Content     := string.Join(sLineBreak, FRows);
end;
```

Use it in any controller:

```pascal
Result := TCsvResult.Create(Rows);
```
