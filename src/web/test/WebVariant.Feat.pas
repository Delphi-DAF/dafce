unit WebVariant.Feat;

interface

uses
  System.SysUtils,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Hosting,
  Daf.Cli,
  Daf.Web.Application;

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

  /// El estado que el Given deja montado y el When usa. Sostenerlo aqui es lo que permite que
  /// el When haga LA ACCION: con un World de solo texto, el Given tiene que ejecutarla para
  /// poder dejar escrito su resultado, y entonces el escenario ya no comprueba comportamiento
  /// sino la cadena que el propio Given acaba de componer.
  TWebVariantWorld = class
  public
    CliBuilder: ICliApplicationBuilder;
    WebBuilder: IWebApplicationBuilder;
    WebBuilderWithArg: IWebApplicationBuilder;
    CliServices: IServiceProvider;
    WebServices: IServiceProvider;
    App: IWebApplication;
    Report: string;
    destructor Destroy; override;
  end;

implementation

uses
  Daf.MiniSpec,
  Daf.Extensions.Configuration,
  Daf.Extensions.HttpServer,
  Daf.Web.Router;

destructor TWebVariantWorld.Destroy;
begin
  // Un proveedor construido a mano retiene sus singletons hasta que se le cierra. Sin esto el
  // runner acaba con una fuga que no es del codigo medido sino del montaje del escenario, y una
  // fuga en el informe se lee igual la ponga quien la ponga.
  if CliServices <> nil then
    CliServices.ShutDown;
  if WebServices <> nil then
    WebServices.ShutDown;
  inherited;
end;

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
        W.CliBuilder := TCliApplication.CreateBuilder;
        W.CliBuilder.Services.AddSingleton<IShared, TShared>;
        W.CliBuilder.Configuration['App:Name'] := 'cli';

        W.WebBuilder := TWebApplication.CreateBuilder;
        W.WebBuilder.Services.AddSingleton<IShared, TShared>;
        W.WebBuilder.Configuration['App:Name'] := 'web';
      end)
    .When('se le pregunta a cada uno por lo que se le escribio',
      procedure(W: TWebVariantWorld)
      begin
        W.Report := Format('%s/%s env=%s',
          [W.CliBuilder.Configuration['App:Name'], W.WebBuilder.Configuration['App:Name'],
           BoolToStr(W.CliBuilder.Environment.EnvironmentName =
                     W.WebBuilder.Environment.EnvironmentName, True)]);
      end)
    .&Then('las tres cosas se escriben igual y responden igual',
      procedure(W: TWebVariantWorld)
      begin
        Expect(W.Report).ToEqual('cli/web env=True');
      end)

  // La razon de que el contrato comun exista: sin el, un paquete tendria que ofrecer una via
  // por variante, o el consumidor elegir la suya.
  .Scenario('Un paquete que aporta registros de arranque sirve a cualquier variante')
    .Given('el arranque de dos variantes distintas',
      procedure(W: TWebVariantWorld)
      begin
        W.CliBuilder := TCliApplication.CreateBuilder;
        W.WebBuilder := TWebApplication.CreateBuilder;
      end)
    .When('un paquete que solo conoce IServiceCollection registra en los dos',
      procedure(W: TWebVariantWorld)
      begin
        // Por el contrato comun y sin preguntar cual es cual, que es lo que hace de las dos
        // variantes una familia.
        for var Builder in TArray<IHostApplicationBuilder>.Create(W.CliBuilder, W.WebBuilder) do
          TSharedModule.AddTo(Builder.Services);

        W.CliServices := W.CliBuilder.Services.BuildServiceProvider;
        W.WebServices := W.WebBuilder.Services.BuildServiceProvider;
      end)
    .&Then('lo que registro se resuelve en las dos',
      procedure(W: TWebVariantWorld)
      begin
        Expect(W.CliServices.GetRequiredService<IShared>.Name + '/' +
               W.WebServices.GetRequiredService<IShared>.Name).ToEqual('shared/shared');
      end)

.Rule('Lo que la variante Web aporta')

  // El puerto se SIEMBRA en el manager antes de entregarlo, asi que un argumento posterior lo
  // pisa. Si la variante lo anadiera despues, llegaria tarde.
  .Scenario('El puerto por defecto se puede pisar por linea de comandos')
    .Given('dos arranques web, uno tal cual y otro con el puerto por argumento',
      procedure(W: TWebVariantWorld)
      begin
        W.WebBuilder := TWebApplication.CreateBuilder;
        W.WebBuilderWithArg := TWebApplication.CreateBuilder(['--Web:Port=9000']);
      end)
    .When('se lee el puerto de la configuracion de cada uno',
      procedure(W: TWebVariantWorld)
      begin
        W.Report := Format('defecto=%s dado=%s',
          [W.WebBuilder.Configuration[TWebDefaults.PortKey],
           W.WebBuilderWithArg.Configuration[TWebDefaults.PortKey]]);
      end)
    .&Then('el argumento gana al valor sembrado',
      procedure(W: TWebVariantWorld)
      begin
        Expect(W.Report).ToEqual('defecto=8080 dado=9000');
      end)

  // La fachada la aporta Daf.Web, y el componedor generico no la nombra: es lo que evita que
  // el grafo de paquetes se invierta.
  .Scenario('El servidor se aporta por fachada, no por miembro del componedor generico')
    .Given('un arranque web con el servidor http anadido por la fachada',
      procedure(W: TWebVariantWorld)
      begin
        W.WebBuilder := TWebApplication.CreateBuilder;
        Web.AddHttpServer(W.WebBuilder.Services);
      end)
    .When('se construye la aplicacion y se mapean los controladores',
      procedure(W: TWebVariantWorld)
      begin
        W.App := W.WebBuilder.Build;
        W.App.MapControllers;
      end)
    .&Then('el servidor y el enrutado quedan resueltos por el contenedor',
      procedure(W: TWebVariantWorld)
      begin
        // Assigned pide una variable, no el resultado de una llamada: sobre la llamada da E2036
        // y el error sale en la linea siguiente, lejos de la causa.
        var Server: IDafHttpServer := W.App.Server;
        var Router: IWebRouter := W.App.Services.GetService<IWebRouter>;
        Expect(Format('server=%s router=%s',
          [BoolToStr(Assigned(Server), True), BoolToStr(Assigned(Router), True)]))
          .ToEqual('server=True router=True');
      end)

  // src/web no tiene un solo .dpr fuera de sus specs: este escenario es lo unico que se daria
  // cuenta si su arranque dejara de arrancar.
  .Scenario('El arranque web llega hasta un servidor escuchando')
    .Given('una aplicacion web compuesta de punta a punta',
      procedure(W: TWebVariantWorld)
      begin
        W.WebBuilder := TWebApplication.CreateBuilder(['--Web:Port=8099']);
        Web.AddHttpServer(W.WebBuilder.Services);
        W.App := W.WebBuilder.Build;
        W.App.MapControllers;
      end)
    .When('se arranca el servidor, se mira, y se para',
      procedure(W: TWebVariantWorld)
      begin
        W.App.Server.Start;
        try
          W.Report := Format('port=%d active=%s',
            [W.App.Server.Port, BoolToStr(W.App.Server.Active, True)]);
        finally
          W.App.Server.Stop;
        end;
      end)
    .&Then('escucho en el puerto que dijo la configuracion',
      procedure(W: TWebVariantWorld)
      begin
        Expect(W.Report).ToEqual('port=8099 active=True');
      end);

end.
