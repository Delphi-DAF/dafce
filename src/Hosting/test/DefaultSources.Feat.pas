unit DefaultSources.Feat;

// Las fuentes de configuracion que el host carga sin que nadie las pida, en el orden de
// Host.CreateApplicationBuilder: appsettings.json, appsettings.{Entorno}.json, todas las variables
// de entorno y, al final, los argumentos. Cada escenario usa su propia carpeta como content root y
// una clave con nombre propio, para no chocar con el entorno de la maquina que ejecuta la suite.

interface

uses
  Daf.Hosting;

type
  TDefaultSourcesWorld = class
  strict private
    FEnvVar: string;
  public
    Folder: string;
    Builder: IGenericHostBuilder;
    /// Lo que el host acabo leyendo para la clave de la spec, o que no la tiene.
    Report: string;
    /// El nombre de entorno del host y sus respuestas a las preguntas por los entornos conocidos.
    EnvReport: string;
    constructor Create;
    destructor Destroy; override;
    procedure WriteJson(const FileName, Server: string);
    procedure WriteBrokenJson(const FileName: string);
    procedure SetEnvVar(const Value: string);
    procedure CreateBuilder(const ExtraArgs: TArray<string>);
    procedure BuildCreated;
    procedure BuildHost(const ExtraArgs: TArray<string>);
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  Winapi.Windows,
  Daf.MiniSpec,
  Daf.Extensions.Configuration,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Hosting;

const
  Section = 'DafDefaultSourcesSpec';
  Key = Section + ':Server';

{ TDefaultSourcesWorld }

constructor TDefaultSourcesWorld.Create;
begin
  inherited;
  Folder := TPath.Combine(TPath.GetTempPath, 'daf-default-sources-' + TGuid.NewGuid.ToString);
  TDirectory.CreateDirectory(Folder);
end;

destructor TDefaultSourcesWorld.Destroy;
begin
  if FEnvVar <> '' then
    SetEnvironmentVariable(PChar(FEnvVar), nil);
  TDirectory.Delete(Folder, True);
  inherited;
end;

procedure TDefaultSourcesWorld.WriteJson(const FileName, Server: string);
begin
  TFile.WriteAllText(TPath.Combine(Folder, FileName),
    Format('{ "%s": { "Server": "%s" } }', [Section, Server]));
end;

procedure TDefaultSourcesWorld.WriteBrokenJson(const FileName: string);
begin
  TFile.WriteAllText(TPath.Combine(Folder, FileName), '{ this is not json');
end;

procedure TDefaultSourcesWorld.SetEnvVar(const Value: string);
begin
  FEnvVar := Section + '__Server';
  SetEnvironmentVariable(PChar(FEnvVar), PChar(Value));
end;

procedure TDefaultSourcesWorld.CreateBuilder(const ExtraArgs: TArray<string>);
begin
  Builder := THostBuilder.Create(['--contentRoot=' + Folder] + ExtraArgs);
end;

procedure TDefaultSourcesWorld.BuildHost(const ExtraArgs: TArray<string>);
begin
  CreateBuilder(ExtraArgs);
  BuildCreated;
end;

procedure TDefaultSourcesWorld.BuildCreated;
begin
  try
    // Se lee del host: al construirlo, el builder suelta su configuracion.
    var Host := Builder.Build;
    try
      var Config := Host.Services.GetRequiredService<IConfiguration>;
      if Config.Exists(Key) then
        Report := Config[Key]
      else
        Report := '<ausente>';
      var Env := Host.Services.GetRequiredService<IHostEnvironment>;
      EnvReport := Format('%s production=%s staging=%s development=%s', [Env.EnvironmentName,
        BoolToStr(Env.IsProduction, True), BoolToStr(Env.IsStaging, True), BoolToStr(Env.IsDevelopment, True)]);
    finally
      // Parar es lo que suelta lo que el host retiene; sin esto se queda vivo.
      Host.Stop;
    end;
  except
    on E: Exception do Report := E.ClassName + ': ' + E.Message;
  end;
end;

initialization

Feature('''
Feature Fuentes de configuracion por defecto @hosting @defaultsources

  Como quien escribe una aplicacion sobre el host
  Quiero que el host cargue por su cuenta las fuentes que carga el de .NET
  Para que appsettings.json y el entorno funcionen sin tener que anadirlos a mano
''')

.UseWorld<TDefaultSourcesWorld>

.Rule('El host carga appsettings.json y el del entorno desde su content root')

  .Scenario('appsettings.json se carga sin pedirlo')
    .Given('un appsettings.json en el content root',
      procedure(W: TDefaultSourcesWorld)
      begin
        W.WriteJson('appsettings.json', 'from-json');
      end)
    .When('se construye el host',
      procedure(W: TDefaultSourcesWorld)
      begin
        W.BuildHost([]);
      end)
    .&Then('la configuracion tiene su clave',
      procedure(W: TDefaultSourcesWorld)
      begin
        Expect(W.Report).ToEqual('from-json');
      end)

  .Scenario('appsettings del entorno gana a appsettings.json')
    .Given('un appsettings.json y un appsettings.Staging.json en el content root',
      procedure(W: TDefaultSourcesWorld)
      begin
        W.WriteJson('appsettings.json', 'from-json');
        W.WriteJson('appsettings.Staging.json', 'from-staging');
      end)
    .When('se construye el host en el entorno Staging',
      procedure(W: TDefaultSourcesWorld)
      begin
        W.BuildHost(['--environment=Staging']);
      end)
    .&Then('la clave es la del fichero del entorno',
      procedure(W: TDefaultSourcesWorld)
      begin
        Expect(W.Report).ToEqual('from-staging');
      end)

  .Scenario('Sin ficheros, el host se construye igual')
    .Given('un content root vacio', procedure(W: TDefaultSourcesWorld) begin end)
    .When('se construye el host',
      procedure(W: TDefaultSourcesWorld)
      begin
        W.BuildHost([]);
      end)
    .&Then('se construye, y la clave no esta',
      procedure(W: TDefaultSourcesWorld)
      begin
        Expect(W.Report).ToEqual('<ausente>');
      end)

.Rule('El host carga tambien los ficheros de la aplicacion, y ganan a appsettings')

  .Scenario('El fichero de la aplicacion gana a appsettings.json')
    .Given('un appsettings.json y un SpecApp.settings.json en el content root',
      procedure(W: TDefaultSourcesWorld)
      begin
        W.WriteJson('appsettings.json', 'from-json');
        W.WriteJson('SpecApp.settings.json', 'from-app');
      end)
    .When('se construye el host de la aplicacion SpecApp',
      procedure(W: TDefaultSourcesWorld)
      begin
        W.BuildHost(['--applicationName=SpecApp']);
      end)
    .&Then('la clave es la del fichero de la aplicacion',
      procedure(W: TDefaultSourcesWorld)
      begin
        Expect(W.Report).ToEqual('from-app');
      end)

  .Scenario('El fichero de la aplicacion gana tambien al appsettings del entorno')
    .Given('un appsettings.Staging.json y un SpecApp.settings.json en el content root',
      procedure(W: TDefaultSourcesWorld)
      begin
        W.WriteJson('appsettings.Staging.json', 'from-staging');
        W.WriteJson('SpecApp.settings.json', 'from-app');
      end)
    .When('se construye el host de SpecApp en el entorno Staging',
      procedure(W: TDefaultSourcesWorld)
      begin
        W.BuildHost(['--applicationName=SpecApp', '--environment=Staging']);
      end)
    .&Then('la clave es la del fichero de la aplicacion',
      procedure(W: TDefaultSourcesWorld)
      begin
        Expect(W.Report).ToEqual('from-app');
      end)

  .Scenario('El fichero de la aplicacion y su entorno gana a todos los ficheros')
    .Given('un SpecApp.settings.json y un SpecApp.settings.Staging.json en el content root',
      procedure(W: TDefaultSourcesWorld)
      begin
        W.WriteJson('SpecApp.settings.json', 'from-app');
        W.WriteJson('SpecApp.settings.Staging.json', 'from-app-staging');
      end)
    .When('se construye el host de SpecApp en el entorno Staging',
      procedure(W: TDefaultSourcesWorld)
      begin
        W.BuildHost(['--applicationName=SpecApp', '--environment=Staging']);
      end)
    .&Then('la clave es la del fichero de la aplicacion para ese entorno',
      procedure(W: TDefaultSourcesWorld)
      begin
        Expect(W.Report).ToEqual('from-app-staging');
      end)

  .Scenario('Un separador de ruta en el nombre de la aplicacion pasa a guion bajo')
    .Given('un Suite_Tool.settings.json en el content root',
      procedure(W: TDefaultSourcesWorld)
      begin
        W.WriteJson('Suite_Tool.settings.json', 'from-sanitized');
      end)
    .When('se construye el host de la aplicacion Suite/Tool',
      procedure(W: TDefaultSourcesWorld)
      begin
        W.BuildHost(['--applicationName=Suite/Tool']);
      end)
    .&Then('lee el fichero con el separador cambiado',
      procedure(W: TDefaultSourcesWorld)
      begin
        Expect(W.Report).ToEqual('from-sanitized');
      end)

.Rule('El entorno gana a los ficheros, y los argumentos a todo')

  .Scenario('Una variable de entorno gana a appsettings.json')
    .Given('un appsettings.json y una variable de entorno con la misma clave',
      procedure(W: TDefaultSourcesWorld)
      begin
        W.WriteJson('appsettings.json', 'from-json');
        W.SetEnvVar('from-env');
      end)
    .When('se construye el host',
      procedure(W: TDefaultSourcesWorld)
      begin
        W.BuildHost([]);
      end)
    .&Then('la clave es la de la variable, con __ como separador de seccion',
      procedure(W: TDefaultSourcesWorld)
      begin
        Expect(W.Report).ToEqual('from-env');
      end)

  .Scenario('Un argumento gana al entorno y a los ficheros')
    .Given('la clave en appsettings.json y en una variable de entorno',
      procedure(W: TDefaultSourcesWorld)
      begin
        W.WriteJson('appsettings.json', 'from-json');
        W.SetEnvVar('from-env');
      end)
    .When('se construye el host con la clave tambien en los argumentos',
      procedure(W: TDefaultSourcesWorld)
      begin
        W.BuildHost(['--' + Key + '=from-args']);
      end)
    .&Then('la clave es la del argumento',
      procedure(W: TDefaultSourcesWorld)
      begin
        Expect(W.Report).ToEqual('from-args');
      end)

.Rule('El host carga sus fuentes en la primera lectura de su configuracion, o al construirse')

  .Scenario('Crear el builder no abre ningun fichero')
    .Given('un appsettings.json que no se puede leer en el content root',
      procedure(W: TDefaultSourcesWorld)
      begin
        W.WriteBrokenJson('appsettings.json');
      end)
    .When('se crea el builder',
      procedure(W: TDefaultSourcesWorld)
      begin
        try
          W.CreateBuilder([]);
          W.Report := '<creado>';
        except
          on E: Exception do W.Report := E.ClassName + ': ' + E.Message;
        end;
      end)
    .&Then('no falla',
      procedure(W: TDefaultSourcesWorld)
      begin
        Expect(W.Report).ToEqual('<creado>');
      end)

  // El fichero se escribe con el builder ya creado: si se cargara al crearlo, la clave no estaria.
  .Scenario('Una composicion que no lee la configuracion la recibe al construir el host')
    .Given('un builder recien creado, y despues un appsettings.json en su content root',
      procedure(W: TDefaultSourcesWorld)
      begin
        W.CreateBuilder([]);
        W.WriteJson('appsettings.json', 'from-json');
      end)
    .When('se construye el host sin haber leido la configuracion',
      procedure(W: TDefaultSourcesWorld)
      begin
        W.BuildCreated;
      end)
    .&Then('la configuracion del host lleva la clave del fichero',
      procedure(W: TDefaultSourcesWorld)
      begin
        Expect(W.Report).ToEqual('from-json');
      end)

.Rule('El nombre de entorno se conserva tal cual')

  .Scenario('Un entorno QA carga su propio fichero')
    .Given('un appsettings.QA.json y un appsettings.Production.json en el content root, con valores distintos para la misma clave',
      procedure(W: TDefaultSourcesWorld)
      begin
        W.WriteJson('appsettings.QA.json', 'from-qa');
        W.WriteJson('appsettings.Production.json', 'from-production');
      end)
    .When('se construye el host con el entorno QA',
      procedure(W: TDefaultSourcesWorld)
      begin
        W.BuildHost(['--environment=QA']);
      end)
    .&Then('el entorno es QA, y no es Production',
      procedure(W: TDefaultSourcesWorld)
      begin
        Expect(W.EnvReport).ToEqual('QA production=False staging=False development=False');
      end)
    .&And('la clave tiene el valor de appsettings.QA.json',
      procedure(W: TDefaultSourcesWorld)
      begin
        Expect(W.Report).ToEqual('from-qa');
      end)

  .Scenario('Una abreviatura es un nombre como otro')
    .When('se construye el host con el entorno stag',
      procedure(W: TDefaultSourcesWorld)
      begin
        W.BuildHost(['--environment=stag']);
      end)
    .&Then('el entorno es stag, y no es Staging',
      procedure(W: TDefaultSourcesWorld)
      begin
        Expect(W.EnvReport).ToEqual('stag production=False staging=False development=False');
      end)

  .Scenario('Un nombre conocido en otras mayusculas')
    .When('se construye el host con el entorno development',
      procedure(W: TDefaultSourcesWorld)
      begin
        W.BuildHost(['--environment=development']);
      end)
    .&Then('el entorno es development, y es Development',
      procedure(W: TDefaultSourcesWorld)
      begin
        Expect(W.EnvReport).ToEqual('development production=False staging=False development=True');
      end);

end.
