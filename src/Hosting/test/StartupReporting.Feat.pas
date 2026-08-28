unit StartupReporting.Feat;

interface

uses
  System.SysUtils,
  Daf.Extensions.Hosting,
  DAF.Extensions.Logging;

type
  /// Donde acaba cada informe. El canal de arranque y el pipeline de ILogger escriben fuera del
  /// proceso, asi que el relevo solo se puede medir poniendo un destino en cada uno y mirando cual
  /// de los dos recibio el informe.
  TProbe = class
  public
    class var Bootstrap: string;
    class var Pipeline: string;
    class procedure Reset;
  end;

  TReportingWorld = class
  strict private
    FSaved: TArray<IBootstrapSink>;
    FFolder: string;
  public
    Host: IHost;
    Failure: string;
    ConfigFile: string;
    constructor Create;
    destructor Destroy; override;
    /// Un fichero de configuracion que no se puede leer: el fallo tipico de la fase en la que
    /// todavia no hay proveedor.
    procedure BuildWithBrokenConfig;
    procedure BuildWithFailingStartup(const WithLogging: Boolean);
    procedure StartHost;
  end;

implementation

uses
  System.IOUtils,
  Daf.Extensions.Configuration,
  Daf.Extensions.DependencyInjection,
  Daf.Configuration.Json,
  Daf.Hosting,
  Daf.Logging,
  DAF.Logging.Builder,
  Daf.MiniSpec,
  StartupTasks.Feat;

type
  TProbeSink = class(TInterfacedObject, IBootstrapSink)
  public
    function IsAvailable: Boolean;
    procedure Write(const ALine: string);
  end;

  TProbeLogger = class(TLogger)
  public
    procedure Log(const Entry: TLogEntry); override;
  end;

  TProbeLoggerProvider = class(TInterfacedObject, ILoggerProvider)
  public
    function CreateLogger(const Category: string): ILogger;
  end;

{ TProbe }

class procedure TProbe.Reset;
begin
  Bootstrap := '';
  Pipeline := '';
end;

{ TProbeSink }

function TProbeSink.IsAvailable: Boolean;
begin
  Result := True;
end;

procedure TProbeSink.Write(const ALine: string);
begin
  TProbe.Bootstrap := TProbe.Bootstrap + ALine + sLineBreak;
end;

{ TProbeLogger }

procedure TProbeLogger.Log(const Entry: TLogEntry);
begin
  var Reason := '';
  if Assigned(Entry.Exception) then
    Reason := ' Exception: ' + Entry.Exception.Message;
  TProbe.Pipeline := TProbe.Pipeline + Format('[%s] %s | %s%s',
    [Entry.Level.ToString, Entry.Category, Entry.Message, Reason]) + sLineBreak;
end;

{ TProbeLoggerProvider }

function TProbeLoggerProvider.CreateLogger(const Category: string): ILogger;
begin
  Result := TProbeLogger.Create(Category);
end;

{ TReportingWorld }

constructor TReportingWorld.Create;
begin
  inherited Create;
  FSaved := BootstrapLog.Sinks;
  BootstrapLog.ClearSinks;
  BootstrapLog.AddSink(TProbeSink.Create);
  TProbe.Reset;
end;

destructor TReportingWorld.Destroy;
begin
  if Host <> nil then
    Host.Stop;
  BootstrapLog.ClearSinks;
  for var Sink in FSaved do
    BootstrapLog.AddSink(Sink);
  if (FFolder <> '') and TDirectory.Exists(FFolder) then
    TDirectory.Delete(FFolder, True);
  inherited;
end;

procedure TReportingWorld.BuildWithBrokenConfig;
begin
  FFolder := TPath.Combine(TPath.GetTempPath, 'daf-hostcfg-' + TGuid.NewGuid.ToString);
  TDirectory.CreateDirectory(FFolder);
  ConfigFile := TPath.Combine(FFolder, 'appsettings.json');
  TFile.WriteAllText(ConfigFile, '{ this is not json');
  var FileName := ConfigFile;

  // THostBuilder es maquinaria, no entrada publica: se sostiene por el contrato comun --que es
  // quien lo refcuenta-- y Build se llama por la clase. Un usuario nunca escribe esto: escribe
  // la variante de su tipo de aplicacion.
  var Builder := THostBuilder.Create;
  var Owner: IHostApplicationBuilder := Builder;
  try
    // El fichero se abre AQUI, al anadir la fuente: en el estilo
    // inmediato la fuente se carga cuando se anade.
    JsonConfig.AddFile(Builder.Configuration, FileName);
    Host := Builder.Build;
  except
    on E: Exception do Failure := E.Message;
  end;
end;

procedure TReportingWorld.BuildWithFailingStartup(const WithLogging: Boolean);
begin
  var Builder := THostBuilder.Create;
  var Owner: IHostApplicationBuilder := Builder;
  if WithLogging then
    // Pedir la propiedad ES tocarla, y solo entonces se compone el pipeline en Build.
    Builder.Logging.AddProvider(TProbeLoggerProvider.Create);
  Builder.Services.AddStartupTask<TFailingTask>;
  Host := Builder.Build;
end;

procedure TReportingWorld.StartHost;
begin
  Failure := '';
  try
    Host.Start;
  except
    on E: Exception do Failure := E.Message;
  end;
end;

initialization

Feature('''
Feature The startup report changes hands when the provider exists @hosting @bootstrap @logging

  As an application author
  I want to know who reports before BuildServiceProvider and who reports after
  So that once there is a pipeline, my providers and my configuration are the ones being used
''')

.UseWorld<TReportingWorld>

.Rule('Before the provider exists, the bootstrap channel reports')

  .Scenario('A failure reading the configuration is reported and then propagates')
    .Given('a host whose application configuration points at a json file that cannot be parsed',
      procedure(W: TReportingWorld)
      begin
        W.BuildWithBrokenConfig;
      end)
    .&Then('the exception reaches whoever composed the configuration',
      procedure(W: TReportingWorld)
      begin
        Expect(W.Failure).ToContain('Cannot parse json');
      end)
    .&And('the message names the file, and not only what went wrong',
      procedure(W: TReportingWorld)
      begin
        Expect(W.Failure).ToContain(W.ConfigFile);
      end)
    .&And('the bootstrap channel had already reported it before it propagated',
      procedure(W: TReportingWorld)
      begin
        Expect(TProbe.Bootstrap).ToContain('Cannot parse json');
      end)
    .&And('the framework asked for its logger the same way it offers to the application',
      procedure(W: TReportingWorld)
      begin
        // La categoria dice quien y el mensaje dice que, igual que al otro lado del relevo: no hay
        // atajo global con la categoria del framework ya clavada.
        Expect(TProbe.Bootstrap).ToContain('] TConfigurationManager |');
        Expect(TProbe.Bootstrap).ToContain('a configuration source could not be added');
      end)

.Rule('Once the provider is built, the pipeline reports')

  .Scenario('A failure after the provider is built comes out through the pipeline')
    .Given('a host with logging configured and a startup task that raises',
      procedure(W: TReportingWorld)
      begin
        W.BuildWithFailingStartup(True);
      end)
    .When('the host starts',
      procedure(W: TReportingWorld)
      begin
        W.StartHost;
      end)
    .&Then('the report came out through the configured pipeline',
      procedure(W: TReportingWorld)
      begin
        Expect(W.Failure).ToContain('refused to initialize');
        Expect(TProbe.Pipeline).ToContain('refused to initialize');
        Expect(TProbe.Pipeline).ToContain('THost');
      end)
    .&And('the bootstrap channel did not take over what the pipeline already covers',
      procedure(W: TReportingWorld)
      begin
        Expect(TProbe.Bootstrap).ToBeEmpty;
      end)

  .Scenario('If the logger cannot be resolved, the bootstrap channel is the backstop')
    .Given('a host with no logging registered and a startup task that raises',
      procedure(W: TReportingWorld)
      begin
        W.BuildWithFailingStartup(False);
      end)
    .When('the host starts',
      procedure(W: TReportingWorld)
      begin
        W.StartHost;
      end)
    .&Then('the report came out through the bootstrap channel instead of being lost',
      procedure(W: TReportingWorld)
      begin
        Expect(W.Failure).ToContain('refused to initialize');
        Expect(TProbe.Bootstrap).ToContain('refused to initialize');
        Expect(TProbe.Bootstrap).ToContain('] THost |');
      end);

end.
