# Daf.Web — Servidor HTTP ligero y enrutamiento por atributos

**🌍 Idioma: [English](README.md) | Español**

Servidor HTTP y capa de controladores para aplicaciones DAFce. Construido sobre `TIdHTTPWebBrokerBridge` de Indy y Delphi WebBroker, añade controladores compatibles con DI, enrutamiento basado en atributos y helpers de action-result.

No enlaza **ni DataSnap ni DBX**: un módulo que sirve HTTP no tiene por qué meter una pila de remoting y una capa de base de datos en tu aplicación, y este no lo hace.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![Licencia](https://img.shields.io/badge/licencia-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Inicio rápido

```pascal
uses Web.HTTPApp, Daf.Web.Controller;

type
  [WebRoutePrefix('/api/pedidos')]
  TPedidosController = class(TWebController)
    [WebRoute(mtGet, '/')]
    function Listar: IActionResult;

    [WebRoute(mtPost, '/')]
    function Crear([FromBody] Command: TCreateOrderCommand): IActionResult;
  end;

function TPedidosController.Listar: IActionResult;
begin
  Result := Ok(FOrderService.GetAll);
end;

function TPedidosController.Crear(Command: TCreateOrderCommand): IActionResult;
begin
  FMediator.Send<TCreateOrderCommand>(Command);
  Result := Created('/api/pedidos/' + IntToStr(Command.OrderId));
end;
```

`Web.HTTPApp` está por las constantes de verbo — `mtGet`, `mtPost` y las demás. Todo lo demás que necesita un controlador, atributos incluidos, sale de `Daf.Web.Controller`.

El router construye `Command` a partir del cuerpo JSON con su constructor sin parámetros, y lo libera
cuando se ha ejecutado el resultado de la acción. Un cuerpo que no es un objeto JSON responde
`400 Bad Request`.

---

## Arrancar el servidor

```pascal
uses
  Daf.Extensions.HttpServer,   // IDafHttpServer
  Daf.HttpServer;              // TDafHttpServer

var Server: IDafHttpServer := TDafHttpServer.Create(TWebModule1, Config, LoggerFactory);
Server.Port := 8080;
Server.Start;
WriteLn('Escuchando en :8080');
ReadLn;
Server.Stop;
```

⚠️ **Sostenlo en una `IDafHttpServer`.** `TDafHttpServer` lleva contador de referencias, así que una
variable del tipo de la clase deja el servidor sin dueño y se fuga.

`Config['Port']` fija el puerto al construir y es **obligatoria**: si la clave falta, el indexador
devuelve la cadena vacía y el constructor falla al convertirla. Asignar `Port` después, como arriba,
lo sustituye. Componer por la variante de aplicación en vez de a mano siembra el puerto por ti —ver
la [guía](docs/GUIDE.es.md#la-variante-de-aplicación-web)—.

Con el puerto ocupado, `Start` dispara `OnError` y después lanza una excepción que nombra el puerto:
`Cannot start: port 8080 is in use`. `OnError` es un `TServerErrorEvent`, `procedure(Sender: TObject;
const Error: Exception) of object`, con el servidor como `Sender`: el manejador lee la excepción, que
`Start` lanza a continuación, y ni la libera ni la guarda. ASP.NET Core no tiene este evento.

---

## Helpers de respuesta

| Método | Status |
|--------|--------|
| `Ok(valor)` | 200 |
| `Created(msg)` | 201 |
| `Accepted` | 202 |
| `NoContent` | 204 |
| `BadRequest(msg)` | 400 |
| `Unauthorized` | 401 |
| `Forbidden` | 403 |
| `NotFound` | 404 |
| `Conflict` | 409 |
| `InternalServerError(msg)` | 500 |
| `Html(texto)` | 200 text/html |
| `Page(fichero)` | 200 página renderizada en servidor |

El mensaje que recibe un helper de estado pasa a ser el **cuerpo** de la respuesta. Ninguno pone cabecera `Location`.

---

## El ciclo de vida va a cambiar

El servidor se gobierna a sí mismo: `Start`, `Stop` y los eventos `OnStarting` / `OnStarted` / `OnStopping` / `OnStopped` / `OnError`. DAF ya tiene un modelo de ciclo de vida para lo que gobierna un host —`IHostedService` y `AddHostedService<T>`— y este servidor todavía no lo usa.

Lo usará. La forma prevista es que `TDafHttpServer` implemente `IHostedService` **además**, con `Start` y `Stop` pasando a ser el detalle interno en vez de la API que se maneja; los eventos son más finos que nada que exponga `IHostedService`, así que se quedan. Queda escrito aquí en vez de dejarlo para que lo descubra el primero que llegue: quien componga un host hoy debe contar con que esa costura se mueve.

---

## Documentación

- 📖 [Guía de uso](docs/GUIDE.es.md) — atributos de enrutamiento, binding de parámetros, DI, TDafHttpServer, action results personalizados
