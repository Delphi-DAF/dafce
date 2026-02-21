# web

**🌍 Language: English | [Español](README.es.md)**

HTTP server and controller layer for DAFce applications. Built on top of Indy's `TIdHTTPWebBrokerBridge` and Delphi WebBroker, it adds DI-friendly controllers, attribute-based routing, and action-result helpers.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Quick Start

```pascal
uses Daf.Web.Controller, Daf.Web.HttpServer;

type
  [WebRoutePrefix('/api/orders')]
  TOrdersController = class(TWebControllerBase)
    [WebRoute('GET', '/')]
    function List: IActionResult;

    [WebRoute('POST', '/')]
    function Create([FromBody] Command: TCreateOrderCommand): IActionResult;
  end;

function TOrdersController.List: IActionResult;
begin
  Result := Ok(FOrderService.GetAll);
end;

function TOrdersController.Create(Command: TCreateOrderCommand): IActionResult;
begin
  FMediator.Send<TCreateOrderCommand>(Command);
  Result := Created('/api/orders/' + IntToStr(Command.OrderId));
end;
```

---

## Starting the server

```pascal
uses Daf.Web.HttpServer;

var Server := TDafHttpServer.Create(TWebModule1, Config, LoggerFactory);
Server.Port := 8080;
Server.Start;
WriteLn('Listening on :8080');
ReadLn;
Server.Stop;
```

---

## Response helpers

| Method | Status |
|--------|--------|
| `Ok(value)` | 200 |
| `Created(location)` | 201 |
| `Accepted` | 202 |
| `NoContent` | 204 |
| `BadRequest(msg)` | 400 |
| `Unauthorized` | 401 |
| `Forbidden` | 403 |
| `NotFound` | 404 |
| `Conflict` | 409 |
| `InternalServerError(msg)` | 500 |
| `Html(text)` | 200 text/html |
| `Page(filename)` | 200 server-rendered page |

---

## Documentation

- 📖 [Usage Guide](docs/GUIDE.md) — routing attributes, parameter binding, DI, TDafHttpServer, custom action results
