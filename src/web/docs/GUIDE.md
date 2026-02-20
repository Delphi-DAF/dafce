# web — Usage Guide

**🌍 Language: English | [Español](GUIDE.es.md)**

---

## Table of Contents

1. [Architecture](#architecture)
2. [Controllers](#controllers)
3. [Routing attributes](#routing-attributes)
4. [Parameter binding](#parameter-binding)
5. [Action results reference](#action-results-reference)
6. [TDafHttpServer](#tdafhttpserver)
7. [DI integration](#di-integration)
8. [Custom action results](#custom-action-results)

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
TWebControllerBase / TWebController
     │  (routing by [WebRoute] attributes)
     ▼
Action method → IActionResult → HTTP Response
```

---

## Controllers

### TWebControllerBase

Plain class — DI-friendly, no VCL / DataModule dependency. Preferred for new controllers.

```pascal
uses Daf.Web.Controller;

type
  TUserController = class(TWebControllerBase)
  private
    FService: IUserService;
  public
    constructor Create(Service: IUserService);
    [WebRoute('GET', '/{id}')]
    function GetUser(Id: Integer): IActionResult;
  end;

constructor TUserController.Create(Service: IUserService);
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

### TWebController

Extends `TDataModule` — compatible with WebBroker's action items and legacy code.

---

## Routing attributes

### [WebRoutePrefix]

Applied to the controller class. All route paths in the class are prefixed.

```pascal
[WebRoutePrefix('/api/users')]
TUserController = class(TWebControllerBase)
```

### [WebRoute]

Applied to an action method.

```pascal
[WebRoute('GET',    '/')]          // GET /api/users/
[WebRoute('GET',    '/{id}')]      // GET /api/users/42
[WebRoute('POST',   '/')]          // POST /api/users/
[WebRoute('PUT',    '/{id}')]      // PUT /api/users/42
[WebRoute('DELETE', '/{id}')]      // DELETE /api/users/42
```

Route segments starting with `{name}` are path parameters — matched into method parameters by name.

---

## Parameter binding

| Attribute | Source | Example |
|-----------|--------|---------|
| *(none)* | Path segment | `[WebRoute('GET','/{id}')] function Get(Id: Integer)` |
| `[FromQuery]` | Query string | `function Search([FromQuery] Q: string)` |
| `[FromBody]` | Request body (JSON) | `function Create([FromBody] Cmd: TCreateUserCmd)` |
| `[FromServices]` | DI container | `function Ping([FromServices] Logger: ILogger)` |

---

## Action results reference

All helpers are methods of `TWebControllerBase`:

```pascal
// 2xx
Ok(Value: TObject): IActionResult;       overload;
Ok(const S: string): IActionResult;      overload;
Ok(B: Boolean): IActionResult;           overload;
Created(const Location: string): IActionResult;
Accepted: IActionResult;
NoContent: IActionResult;
NotModified: IActionResult;

// Redirect
Html(const Body: string): IActionResult;
Page(const FileName: string): IActionResult;
Partial(const FileName: string): IActionResult;

// 4xx / 5xx
BadRequest: IActionResult;               overload;
BadRequest(const Msg: string): IActionResult; overload;
Unauthorized: IActionResult;
Forbidden: IActionResult;
NotFound: IActionResult;
Conflict: IActionResult;
InternalServerError: IActionResult;      overload;
InternalServerError(const Msg: string): IActionResult; overload;
```

---

## TDafHttpServer

```pascal
TDafHttpServer = class
  constructor Create(WebModuleClass: TComponentClass;
                     Config: IConfiguration;
                     LoggerFactory: ILoggerFactory);
  property Port   : Integer;
  property Active : Boolean;  // read-only
  procedure Start;
  procedure Stop;
  // Events: OnBeforeDispatch, OnAfterDispatch, OnException
end;
```

### Minimal setup

```pascal
var Srv := TDafHttpServer.Create(TMyWebModule, AppConfig, LogFactory);
Srv.Port := 8080;
Srv.Start;
```

### With Hosting (recommended)

```pascal
// In ConfigureServices:
Services.AddSingleton<IDafHttpServer>(
  function(P: IServiceProvider): IDafHttpServer
  begin
    Result := TDafHttpServer.Create(TMyWebModule,
                P.GetService<IConfiguration>,
                P.GetService<ILoggerFactory>);
  end);

// In hosted service:
FServer.Port := 8080;
FServer.Start;
```

---

## DI integration

Controllers are resolved from the DI container on each request. Register them as transient:

```pascal
Services.AddTransient<TUserController>;
Services.AddTransient<TOrdersController>;
```

The routing dispatcher will resolve the matching controller from `IServiceProvider`, inject its dependencies, call the action, and dispose the controller after the response is sent.

---

## Custom action results

Implement `IActionResult`:

```pascal
type
  TJsonResult<T> = class(TInterfacedObject, IActionResult)
  private
    FData: T;
    FStatus: Integer;
  public
    constructor Create(Data: T; Status: Integer = 200);
    procedure Execute(Response: TWebResponse);
  end;

procedure TJsonResult<T>.Execute(Response: TWebResponse);
begin
  Response.StatusCode   := FStatus;
  Response.ContentType  := 'application/json; charset=utf-8';
  Response.Content      := TJsonSerializer.Serialize<T>(FData);
end;
```

Use it in any controller:

```pascal
Result := TJsonResult<TMyRecord>.Create(MyData, 201);
```
