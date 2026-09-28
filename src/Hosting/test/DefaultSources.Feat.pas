unit DefaultSources.Feat;

// Las fuentes de configuracion que el host carga sin que nadie las pida, en el orden de
// Host.CreateApplicationBuilder: appsettings.json, appsettings.{Entorno}.json, todas las variables
// de entorno y, al final, los argumentos. Cada escenario usa su propia carpeta como content root y
// una clave con nombre propio, para no chocar con el entorno de la maquina que ejecuta la suite.

interface

type
  TDefaultSourcesWorld = class
  strict private
    FEnvVar: string;
  public
    Folder: string;
    /// Lo que el host acabo leyendo para la clave de la spec, o que no la tiene.
    Report: string;
    constructor Create;
    destructor Destroy; override;
    procedure WriteJson(const FileName, Server: string);
    procedure SetEnvVar(const Value: string);
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
  Daf.Extensions.Hosting,
  Daf.Hosting;

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

procedure TDefaultSourcesWorld.SetEnvVar(const Value: string);
begin
  FEnvVar := Section + '__Server';
  SetEnvironmentVariable(PChar(FEnvVar), PChar(Value));
end;

procedure TDefaultSourcesWorld.BuildHost(const ExtraArgs: TArray<string>);
begin
  var Builder := THostBuilder.Create(['--DAF_CONTENT_ROOT=' + Folder] + ExtraArgs);
  var Owner: IHostApplicationBuilder := Builder;
  try
    // Se lee del host: al construirlo, el builder suelta su configuracion.
    var Host := Builder.Build;
    try
      var Config := Host.Services.GetRequiredService<IConfiguration>;
      if Config.Exists(Key) then
        Report := Config[Key]
      else
        Report := '<ausente>';
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
        W.BuildHost(['--DAF_APP_ENV=Staging']);
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
      end);

end.
