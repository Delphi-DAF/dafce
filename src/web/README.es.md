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

---

## Arrancar el servidor

```pascal
uses Daf.HttpServer;

var Server := TDafHttpServer.Create(TWebModule1, Config, LoggerFactory);
Server.Port := 8080;
Server.Start;
WriteLn('Escuchando en :8080');
ReadLn;
Server.Stop;
```

`Config['Port']` fija el puerto al construir; asignar `Port` después, como arriba, lo sustituye.

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
