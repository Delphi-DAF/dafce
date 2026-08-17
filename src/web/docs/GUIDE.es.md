# Daf.Web — Guía de uso

**🌍 Idioma: [English](GUIDE.md) | Español**

---

## Tabla de contenidos

1. [Arquitectura](#arquitectura)
2. [Controladores](#controladores)
3. [Atributos de enrutamiento](#atributos-de-enrutamiento)
4. [Binding de parámetros](#binding-de-parámetros)
5. [Referencia de action results](#referencia-de-action-results)
6. [TDafHttpServer](#tdafhttpserver)
7. [Integración con DI](#integración-con-di)
8. [Action results personalizados](#action-results-personalizados)

---

## Arquitectura

```
Petición HTTP
     │
     ▼
TDafHttpServer  (Indy TIdHTTPWebBrokerBridge)
     │
     ▼
TWebModule      (dispatcher WebBroker)
     │
     ▼
TWebRouter      (BeforeDispatch — casa [WebRoute] por RTTI)
     │
     ▼
TWebController  (construido para esta petición desde el scope de servicios)
     │
     ▼
Método de acción → IActionResult → Respuesta HTTP
```

---

## Controladores

### TWebController

Un controlador desciende de `TWebController`. Es la clase que construye el router, y la que hay que usar.

```pascal
uses Web.HTTPApp, Daf.Web.Controller;

type
  [WebRoutePrefix('/api/usuarios')]
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

`TWebController` extiende `TDataModule`, que es lo que lo hace usable desde el diseñador del IDE junto a los action items de WebBroker. Los métodos de acción deben ser `public` o `published`, o el RTTI no los verá.

### TWebControllerBase

Variante de clase plana que no desciende de `TDataModule`. **El router no la construye**: construye la clase que ha casado y la trata como un `TWebController`, así que enrutar un descendiente de `TWebControllerBase` no está soportado. Úsala solo fuera del router, o no la uses.

---

## Atributos de enrutamiento

### [WebRoutePrefix]

Se aplica a la clase del controlador. Prefija todas las rutas de la clase.

```pascal
[WebRoutePrefix('/api/usuarios')]
TUserController = class(TWebController)
```

### [WebRoute]

Se aplica a un método de acción. El verbo es un `TMethodType`, no una cadena — las constantes están en `Web.HTTPApp`.

```pascal
[WebRoute(mtGet,    '/')]           // GET /api/usuarios/
[WebRoute(mtGet,    '/{id}')]       // GET /api/usuarios/42
[WebRoute(mtPost,   '/')]           // POST /api/usuarios/
[WebRoute(mtPut,    '/{id}')]       // PUT /api/usuarios/42
[WebRoute(mtDelete, '/{id}')]       // DELETE /api/usuarios/42
[WebRoute(mtAny,    '/health')]     // cualquier verbo
```

Los segmentos escritos `{nombre}` son parámetros de ruta — se casan por nombre con los parámetros del método.

Una petición cuyo path casa pero cuyo verbo no declara ninguna acción **no** se despacha a una acción de otro verbo: el router la deja sin atender y responde WebBroker.

---

## Binding de parámetros

| Atributo | Origen | Ejemplo |
|-----------|--------|---------|
| *(ninguno)* | Segmento de path | `[WebRoute(mtGet,'/{id}')] function Get(Id: Integer)` |
| `[FromQuery]` | Query string | `function Search([FromQuery] Q: string)` |
| `[FromBody]` | Cuerpo de la petición (JSON) | `function Create([FromBody] Cmd: TCreateUserCmd)` |
| `[FromServices]` | Contenedor DI | `function Ping([FromServices] Logger: ILogger)` |

Un parámetro de tipo `TWebRequest` o `TWebResponse` recibe la propia petición o respuesta, sin necesidad de atributo.

---

## Referencia de action results

Todos los helpers son métodos protected del controlador. Cada helper de estado acepta un mensaje opcional, que pasa a ser el **cuerpo** de la respuesta — ninguno pone cabecera `Location`.

```pascal
// 2xx
Ok(const Value: TObject): IActionResult;   overload;   // serializado como JSON
Ok(const Value: string): IActionResult;    overload;
Ok(const Value: Boolean): IActionResult;   overload;
Created(const Msg: string = 'Created'): IActionResult;
Accepted(const Msg: string = 'Accepted'): IActionResult;
NoContent(const Msg: string = 'No content'): IActionResult;

// 3xx
MovedPermanently(const Msg: string = 'Moved permanently'): IActionResult;
MovedTemporarily(const Msg: string = 'Moved temporarily'): IActionResult;
NotModified(const Msg: string = 'Not modified'): IActionResult;

// Contenido
Html(const HtmlText: string): IActionResult;
Page(const FileName: string): IActionResult;      // <raíz>/pages/<nombre>.html
Partial(const FileName: string): IActionResult;   // <raíz>/partials/<nombre>.html

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

Una acción también puede devolver un `string` a secas, que se envía como `text/plain`.

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
  property Active    : Boolean;      // solo lectura
  property SessionID : string;       // solo lectura
  // Eventos: OnStarting, OnStarted, OnStopping, OnStopped, OnError, OnDestroy
end;
```

`Create` lee el puerto de `Config['Port']`. `Start` se niega a arrancar sobre un puerto ocupado: dispara `OnError` con un mensaje que nombra el puerto y aborta, en vez de dejar escapar un error de socket desde dentro del servidor. `CanBindPort` responde a lo mismo por adelantado, y suelta lo que haya reservado.

### Montaje mínimo

```pascal
uses Daf.HttpServer;

var Srv := TDafHttpServer.Create(TMyWebModule, AppConfig, LogFactory);
Srv.Port := 8080;
Srv.Start;
```

### Con Hosting

```pascal
// En ConfigureServices:
Services.AddSingleton<IDafHttpServer>(
  function(P: IServiceProvider): IDafHttpServer
  begin
    Result := TDafHttpServer.Create(TMyWebModule,
                P.GetService<IConfiguration>,
                P.GetService<ILoggerFactory>);
  end);

// En un hosted service:
FServer.Port := 8080;
FServer.Start;
```

El servidor todavía no es un `IHostedService` — ver la nota del [README](../README.es.md).

---

## Integración con DI

Cada petición tiene su propio scope, y el controlador se construye dentro de él a partir de `IServiceProvider`, se invoca y se libera una vez escrita la respuesta. Todo lo scoped vive y muere, por tanto, con la petición.

La clase del controlador **no** hace falta registrarla: la construye `TActivatorUtilities`, que elige el constructor cuyos parámetros puede satisfacer el contenedor. Lo que sí hay que registrar es lo que el controlador pide.

```pascal
Services.AddScoped<IUserService, TUserService>;
```

---

## Action results personalizados

Implementa `IActionResult`:

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
  Response.Content      := TJson.ObjectToJsonString(FData);
end;
```

Úsalo en cualquier controlador:

```pascal
Result := TJsonResult<TMyRecord>.Create(MyData, 201);
```
