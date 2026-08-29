unit WebVariant.Feat;

interface

uses
  System.SysUtils,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Hosting;

type
  // En la seccion de interfaz por obligacion: el contenedor resuelve por RTTI.
  IShared = interface(IInvokable)
    ['{667A3A79-281D-4414-BE9F-AD09B6D0EA75}']
    function Name: string;
  end;

  TShared = class(TInterfacedObject, IShared)
  public
    function Name: string;
  end;

  /// Un paquete de terceros que aporta registros de arranque. No sabe --ni puede saber-- que
  /// variante lo va a consumir: solo ve IServiceCollection.
  TSharedModule = class
  public
    class procedure AddTo(const Services: IServiceCollection);
  end;

  TWebVariantWorld = class
  public
    Report: string;
  end;

implementation

uses
  Daf.MiniSpec,
  Daf.Extensions.Configuration,
  Daf.Extensions.HttpServer,
  Daf.Web.Router,
  Daf.Web.Application,
  Daf.Cli;

function TShared.Name: string;
begin
  Result := 'shared';
end;

class procedure TSharedModule.AddTo(const Services: IServiceCollection);
begin
  Services.AddSingleton<IShared, TShared>;
end;

// --- Feature definition ---

initialization

Feature('''
Feature La variante Web de la familia @web @application-family

  Como quien escribe una aplicacion web
  Quiero que el arranque se exprese igual que el de una CLI
  Para que solo difieran donde las variantes difieren de verdad
''')

.UseWorld<TWebVariantWorld>

.Rule('Dos variantes se arrancan igual hasta donde pueden')

  .Scenario('El registro, la configuracion y el entorno se expresan igual en las dos')
    .Given('el arranque de una CLI y el de una web, escritos con las mismas llamadas',
      procedure(W: TWebVariantWorld)
      begin
        var Cli := TCliApplication.CreateBuilder;
        Cli.Services.AddSingleton<IShared, TShared>;
        Cli.Configuration['App:Name'] := 'cli';

        var Web := TWebApplication.CreateBuilder;
        Web.Services.AddSingleton<IShared, TShared>;
        Web.Configuration['App:Name'] := 'web';

        W.Report := Format('%s/%s env=%s',
          [Cli.Configuration['App:Name'], Web.Configuration['App:Name'],
           BoolToStr(Cli.Environment.EnvironmentName = Web.Environment.EnvironmentName, True)]);
      end)
    .When('se comparan').NoAction
    .&Then('las tres cosas se escriben igual y responden igual',
      procedure(W: TWebVariantWorld)
      begin
        Expect(W.Report).ToEqual('cli/web env=True');
      end)

  // La razon de que el contrato comun exista: sin el, un paquete tendria que ofrecer una via
  // por variante, o el consumidor elegir la suya.
  .Scenario('Un paquete que aporta registros de arranque sirve a cualquier variante')
    .Given('un paquete que solo conoce IServiceCollection',
      procedure(W: TWebVariantWorld)
      begin
        var Cli := TCliApplication.CreateBuilder;
        TSharedModule.AddTo(Cli.Services);

        var Web := TWebApplication.CreateBuilder;
        TSharedModule.AddTo(Web.Services);

        // Y por el contrato comun, sin saber cual es cual.
        var AsCommon: IHostApplicationBuilder := Web;
        TSharedModule.AddTo(AsCommon.Services);

        W.Report := 'registrado en las dos sin preguntar cual es';
      end)
    .When('lo consume cada una').NoAction
    .&Then('no necesita saber cual es',
      procedure(W: TWebVariantWorld)
      begin
        Expect(W.Report).ToEqual('registrado en las dos sin preguntar cual es');
      end)

.Rule('Lo que la variante Web aporta')

  // El puerto se SIEMBRA en el manager antes de entregarlo, asi que un argumento posterior lo
  // pisa. Si la variante lo anadiera despues, llegaria tarde.
  .Scenario('El puerto por defecto se puede pisar por linea de comandos')
    .Given('una aplicacion web arrancada con el puerto por argumento',
      procedure(W: TWebVariantWorld)
      begin
        var Plain := TWebApplication.CreateBuilder;
        var Given := TWebApplication.CreateBuilder(['--Web:Port=9000']);
        W.Report := Format('defecto=%s dado=%s',
          [Plain.Configuration[TWebDefaults.PortKey], Given.Configuration[TWebDefaults.PortKey]]);
      end)
    .When('se lee la configuracion').NoAction
    .&Then('el argumento gana al valor sembrado',
      procedure(W: TWebVariantWorld)
      begin
        Expect(W.Report).ToEqual('defecto=8080 dado=9000');
      end)

  // La fachada la aporta Daf.Web, y el componedor generico no la nombra: es lo que evita que
  // el grafo de paquetes se invierta.
  .Scenario('El servidor se aporta por fachada, no por miembro del componedor generico')
    .Given('un arranque web que anade el servidor http',
      procedure(W: TWebVariantWorld)
      begin
        var B := TWebApplication.CreateBuilder;
        Web.AddHttpServer(B.Services);
        var App := B.Build;
        App.MapControllers;

        // Assigned pide una variable, no el resultado de una llamada: sobre la llamada da E2036
        // y el error sale en la linea siguiente, lejos de la causa.
        var Server: IDafHttpServer := App.Server;
        var Router: IWebRouter := App.Services.GetService<IWebRouter>;
        W.Report := Format('server=%s router=%s',
          [BoolToStr(Assigned(Server), True), BoolToStr(Assigned(Router), True)]);
      end)
    .When('se construye').NoAction
    .&Then('el servidor y el enrutado quedan resueltos por el contenedor',
      procedure(W: TWebVariantWorld)
      begin
        Expect(W.Report).ToEqual('server=True router=True');
      end)

  // src/web no tiene un solo .dpr fuera de sus specs: este escenario es lo unico que se daria
  // cuenta si su arranque dejara de arrancar.
  .Scenario('El arranque web llega hasta un servidor escuchando')
    .Given('una aplicacion web compuesta de punta a punta',
      procedure(W: TWebVariantWorld)
      begin
        var B := TWebApplication.CreateBuilder(['--Web:Port=8099']);
        Web.AddHttpServer(B.Services);
        var App := B.Build;
        App.MapControllers;

        App.Server.Start;
        try
          W.Report := Format('port=%d active=%s',
            [App.Server.Port, BoolToStr(App.Server.Active, True)]);
        finally
          App.Server.Stop;
        end;
      end)
    .When('se arranca el servidor y se para').NoAction
    .&Then('escucho en el puerto que dijo la configuracion',
      procedure(W: TWebVariantWorld)
      begin
        Expect(W.Report).ToEqual('port=8099 active=True');
      end);

end.
