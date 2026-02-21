# web — Guía de uso

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
TWebControllerBase / TWebController
     │  (enrutamiento por atributos [WebRoute])
     ▼
Método de acción → IActionResult → Respuesta HTTP
```

---

## Controladores

### TWebControllerBase

Clase plana — compatible con DI, sin dependencia de VCL / DataModule. Preferido para nuevos controladores.

```pascal
uses Daf.Web.Controller;

type
  TUserController = class(TWebControllerBase)
  private
    FService: IUserService;
  public
    constructor Create(Service: IUserService);
    [WebRoute('GET', '/{id}')]
    function ObtenerUsuario(Id: Integer): IActionResult;
  end;

constructor TUserController.Create(Service: IUserService);
begin
  inherited Create;
  FService := Service;
end;

function TUserController.ObtenerUsuario(Id: Integer): IActionResult;
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

Extiende `TDataModule` — compatible con los action items de WebBroker y código heredado.

---

## Atributos de enrutamiento

### [WebRoutePrefix]

Se aplica a la clase del controlador. Todos los paths de rutas de la clase llevan este prefijo.

```pascal
[WebRoutePrefix('/api/usuarios')]
TUserController = class(TWebControllerBase)
```

### [WebRoute]

Se aplica a un método de acción.

```pascal
[WebRoute('GET',    '/')]           // GET /api/usuarios/
[WebRoute('GET',    '/{id}')]       // GET /api/usuarios/42
[WebRoute('POST',   '/')]           // POST /api/usuarios/
[WebRoute('PUT',    '/{id}')]       // PUT /api/usuarios/42
[WebRoute('DELETE', '/{id}')]       // DELETE /api/usuarios/42
```

Los segmentos de ruta que comienzan con `{nombre}` son parámetros de path — se mapean a los parámetros del método por nombre.

---

## Binding de parámetros

| Atributo | Fuente | Ejemplo |
|----------|--------|---------|
| *(ninguno)* | Segmento de path | `[WebRoute('GET','/{id}')] function Get(Id: Integer)` |
| `[FromQuery]` | Query string | `function Buscar([FromQuery] Q: string)` |
| `[FromBody]` | Cuerpo de la petición (JSON) | `function Crear([FromBody] Cmd: TCreateUserCmd)` |
| `[FromServices]` | Contenedor DI | `function Ping([FromServices] Logger: ILogger)` |

---

## Referencia de action results

Todos los helpers son métodos de `TWebControllerBase`:

```pascal
// 2xx
Ok(Value: TObject): IActionResult;       overload;
Ok(const S: string): IActionResult;      overload;
Ok(B: Boolean): IActionResult;           overload;
Created(const Location: string): IActionResult;
Accepted: IActionResult;
NoContent: IActionResult;
NotModified: IActionResult;

// HTML
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
  property Active : Boolean;  // solo lectura
  procedure Start;
  procedure Stop;
  // Eventos: OnBeforeDispatch, OnAfterDispatch, OnException
end;
```

### Configuración mínima

```pascal
var Srv := TDafHttpServer.Create(TMyWebModule, AppConfig, LogFactory);
Srv.Port := 8080;
Srv.Start;
```

### Con Hosting (recomendado)

```pascal
// En ConfigureServices:
Services.AddSingleton<IDafHttpServer>(
  function(P: IServiceProvider): IDafHttpServer
  begin
    Result := TDafHttpServer.Create(TMyWebModule,
                P.GetService<IConfiguration>,
                P.GetService<ILoggerFactory>);
  end);

// En el hosted service:
FServer.Port := 8080;
FServer.Start;
```

---

## Integración con DI

Los controladores se resuelven desde el contenedor DI en cada petición. Regístralos como transitorios:

```pascal
Services.AddTransient<TUserController>;
Services.AddTransient<TOrdersController>;
```

El dispatcher de enrutamiento resolverá el controlador coincidente desde `IServiceProvider`, inyectará sus dependencias, llamará a la acción y dispondrá el controlador después de que se envíe la respuesta.

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
  Response.Content      := TJsonSerializer.Serialize<T>(FData);
end;
```

Úsalo en cualquier controlador:

```pascal
Result := TJsonResult<TMyRecord>.Create(MisDatos, 201);
```
