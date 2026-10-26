program MiniApi;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  System.SysUtils,
  System.Classes,
  IdHTTP,
  IdException,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.HttpServer,
  Daf.Web.Application,
  Daf.Web.Controller,
  MiniApi.Catalog in 'MiniApi.Catalog.pas';

const
  Port = '8099';

procedure Section(const Title: string);
begin
  Writeln;
  Writeln('== ', Title);
end;

/// Una peticion contra el propio servidor, escribiendo el codigo y el cuerpo. Es lo que
/// convierte al sample en algo que se puede mirar en vez de algo que hay que creerse.
procedure Call(const Verb, Path: string);
begin
  var Http := TIdHTTP.Create(nil);
  try
    // Sin esto, TIdHTTP lanza en cualquier codigo que no sea 2xx y el sample no podria
    // enseñar el 404 ni el 400, que son la mitad de lo que hay que ver.
    Http.HTTPOptions := Http.HTTPOptions + [hoNoProtocolErrorException, hoWantProtocolErrorContent];
    var Body: string;
    if Verb = 'DELETE' then
      Body := Http.Delete('http://127.0.0.1:' + Port + Path)
    else
      Body := Http.Get('http://127.0.0.1:' + Port + Path);
    Writeln(Format('  %-6s %-28s -> %d %s', [Verb, Path, Http.ResponseCode, Body.Trim]));
  finally
    Http.Free;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;

  var Builder := TWebApplication.CreateBuilder(['--Web:Port=' + Port]);
  Builder.Services.AddSingleton<ICatalog, TCatalog>;

  // Registra el router y el servidor. Es una fachada de Daf.Web y no un miembro del
  // componedor comun, para que el paquete generico no nombre a ninguna variante.
  WebApp.AddHttpServer(Builder.Services);

  var App := Builder.Build;
  // Resolver el router ES descubrir los controladores: los busca por RTTI al construirse.
  App.MapControllers;

  // Una aplicacion de verdad llama a App.Run, que arranca el host, arranca el servidor y
  // se queda esperando la parada. Aqui se arranca el servidor a mano para poder hacerle
  // unas peticiones y salir.
  App.Server.Start;
  try
    Writeln('escuchando en http://127.0.0.1:', Port);

    Section('Un segmento de la ruta entra por nombre en el parametro');
    Call('GET', '/api/products/42');

    Section('Lo que no existe contesta 404, y lo dice');
    Call('GET', '/api/products/99');
    Call('GET', '/api/nothing-declared');

    Section('De la cadena de consulta, con FromQuery');
    Call('GET', '/api/products/search?q=taladro');
    Call('GET', '/api/products/search');

    Section('mtAny contesta a cualquier verbo');
    Call('GET', '/api/products/health');
    Call('DELETE', '/api/products/health');

    Section('La misma ruta, otro verbo, otra accion');
    Call('DELETE', '/api/products/42');
  finally
    App.Server.Stop;
  end;

  Writeln;
end.
