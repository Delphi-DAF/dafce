# web

**🌍 Idioma: [English](README.md) | Español**

Servidor HTTP y capa de controladores para aplicaciones DAFce. Construido sobre `TIdHTTPWebBrokerBridge` de Indy y Delphi WebBroker, añade controladores compatibles con DI, enrutamiento basado en atributos y helpers de action-result.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![Licencia](https://img.shields.io/badge/licencia-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Inicio rápido

```pascal
uses Daf.Web.Controller, Daf.Web.HttpServer;

type
  [WebRoutePrefix('/api/pedidos')]
  TPedidosController = class(TWebControllerBase)
    [WebRoute('GET', '/')]
    function Listar: IActionResult;

    [WebRoute('POST', '/')]
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

---

## Arrancar el servidor

```pascal
uses Daf.Web.HttpServer;

var Server := TDafHttpServer.Create(TWebModule1, Config, LoggerFactory);
Server.Port := 8080;
Server.Start;
WriteLn('Escuchando en :8080');
ReadLn;
Server.Stop;
```

---

## Helpers de respuesta

| Método | Status |
|--------|--------|
| `Ok(valor)` | 200 |
| `Created(urlRecurso)` | 201 |
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

---

## Documentación

- 📖 [Guía de uso](docs/GUIDE.es.md) — atributos de enrutamiento, binding de parámetros, DI, TDafHttpServer, action results personalizados
