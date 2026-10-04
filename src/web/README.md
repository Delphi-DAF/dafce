# Daf.Web — Lightweight HTTP Server and Attribute Routing

**🌍 Language: English | [Español](README.es.md)**

HTTP server and controller layer for DAFce applications. Built on top of Indy's `TIdHTTPWebBrokerBridge` and Delphi WebBroker, it adds DI-friendly controllers, attribute-based routing, and action-result helpers.

It links **no DataSnap and no DBX**: a module that serves HTTP should not put a remoting stack and a database layer into your application, and this one does not.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Quick Start

```pascal
uses Web.HTTPApp, Daf.Web.Controller;

type
  [WebRoutePrefix('/api/orders')]
  TOrdersController = class(TWebController)
    [WebRoute(mtGet, '/')]
    function List: IActionResult;

    [WebRoute(mtPost, '/')]
    function Place([FromBody] Command: TCreateOrderCommand): IActionResult;
  end;

function TOrdersController.List: IActionResult;
begin
  Result := Ok(FOrderService.GetAll);
end;

function TOrdersController.Place(Command: TCreateOrderCommand): IActionResult;
begin
  FMediator.Send<TCreateOrderCommand>(Command);
  Result := Created('/api/orders/' + IntToStr(Command.OrderId));
end;
```

`Web.HTTPApp` is there for the verb constants — `mtGet`, `mtPost` and the rest. Everything else a controller needs, including the attributes, comes from `Daf.Web.Controller`.

---

## Starting the server

```pascal
uses
  Daf.Extensions.HttpServer,   // IDafHttpServer
  Daf.HttpServer;              // TDafHttpServer

var Server: IDafHttpServer := TDafHttpServer.Create(TWebModule1, Config, LoggerFactory);
Server.Port := 8080;
Server.Start;
WriteLn('Listening on :8080');
ReadLn;
Server.Stop;
```

⚠️ **Hold it in an `IDafHttpServer`.** `TDafHttpServer` is reference counted, so a variable of the
class type leaves nobody owning it and the server leaks.

`Config['Port']` sets the port at construction and is **required**: with the key missing the indexer
returns the empty string and the constructor fails converting it. Assigning `Port` afterwards, as
above, overrides it. Composing through the application variant instead of by hand seeds the port for
you — see the [guide](docs/GUIDE.md#the-web-application-variant).

A port already in use makes `Start` fire `OnError` and then raise an exception that names the port:
`Cannot start: port 8080 is in use`.

---

## Response helpers

| Method | Status |
|--------|--------|
| `Ok(value)` | 200 |
| `Created(msg)` | 201 |
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

The message a status helper takes becomes the response **body**. None of them sets a `Location` header.

---

## The lifecycle will change

The server drives itself: `Start`, `Stop`, and the `OnStarting` / `OnStarted` / `OnStopping` / `OnStopped` / `OnError` events. DAF already has a lifecycle model for anything a host owns — `IHostedService` and `AddHostedService<T>` — and this server does not use it yet.

It will. The intended shape is `TDafHttpServer` implementing `IHostedService` **as well**, with `Start` and `Stop` becoming the inner detail rather than the API you drive; the events are finer-grained than anything `IHostedService` exposes, so they stay. This is written here rather than left to be discovered: a consumer composing a host today should expect that seam to move.

---

## Documentation

- 📖 [Usage Guide](docs/GUIDE.md) — routing attributes, parameter binding, DI, TDafHttpServer, custom action results
