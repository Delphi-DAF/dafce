unit CodeConfig.Feat;

// NNLog compone su pipeline ANTES de que exista contenedor --ILoggingBuilder.AddProvider recibe un
// proveedor ya construido-- asi que sus ajustes no pueden llegarle por inyeccion. Lo que si puede
// es no obligar a que vengan de un fichero.
//
// Los dos escenarios afirman lo que un atajo no podria: el primero compara las DOS vias contra el
// mismo destino de prueba, porque "el de codigo escribio algo" lo cumpliria tambien un proveedor
// mal montado; y el segundo libera los ajustes antes de escribir, que es la unica forma de ver que
// el proveedor no se los quedo.

interface

uses
  Daf.Extensions.Logging,
  Daf.Extensions.Hosting,
  DAF.NNLog.Configuration,
  DAF.NNLog.Targets.Probe;

type
  TCodeConfigWorld = class
  strict private
    FEnv: IHostEnvironment;
    FProvider: ILoggerProvider;
    FSettings: TNNLogConfiguration;
    function BuildEnv: IHostEnvironment;
  public
    FromCode: string;
    FromSection: string;
    Failure: string;
    constructor Create;
    destructor Destroy; override;
    /// Los mismos ajustes que ComposeFromSection, escritos a mano en vez de leidos.
    procedure ComposeFromCode;
    procedure ComposeFromSection;
    /// Libera los ajustes que escribio el escenario. Solo tiene sentido tras ComposeFromCode: es
    /// la comprobacion de que el proveedor no se quedo con ellos.
    procedure ReleaseSettings;
    procedure WriteInfoFrom(const Category: string);
    function Recorded: string;
    procedure ClearProbe;
  end;

implementation

uses
  System.SysUtils,
  System.Generics.Collections,
  Daf.Extensions.Configuration,
  Daf.Configuration.Builder,
  Daf.Configuration.Memory,
  Daf.Hosting,
  DAF.NNLog.Loader,
  Daf.MiniSpec;

{ TCodeConfigWorld }

constructor TCodeConfigWorld.Create;
begin
  inherited;
  FromCode := '<sin componer>';
  FromSection := '<sin componer>';
  NNLogProbe.Clear;
end;

destructor TCodeConfigWorld.Destroy;
begin
  FProvider := nil;
  FEnv := nil;
  FSettings.Free;
  inherited;
end;

function TCodeConfigWorld.BuildEnv: IHostEnvironment;
begin
  if FEnv = nil then
  begin
    var CB: IConfigurationBuilder := TConfigurationBuilder.Create;
    var EmptyConfig: IConfiguration := CB.Build as IConfiguration;
    FEnv := THostEnvironment.Create(EmptyConfig) as IHostEnvironment;
  end;
  Result := FEnv;
end;

procedure TCodeConfigWorld.ComposeFromCode;
begin
  FSettings := TNNLogConfiguration.Create;

  var Target := TTargetConfig.Create;
  Target.AddOrSetValue('TargetType', 'Probe');
  Target.AddOrSetValue('Name', 'probe');
  FSettings.Targets.Add('probe', Target);

  var Rule := TLogRuleConfig.Create;
  Rule.Name := 'Alpha.*';
  Rule.Enabled := 'true';
  Rule.MinLevel := 'Trace';
  Rule.WriteTo := 'probe';
  FSettings.Rules := [Rule];

  FProvider := TNNLogLoader.BuildProvider(BuildEnv, FSettings);
end;

procedure TCodeConfigWorld.ComposeFromSection;
begin
  var Dict := TDictionary<string, string>.Create;
  Dict.AddOrSetValue('Targets:probe:TargetType', 'Probe');
  Dict.AddOrSetValue('Targets:probe:Name', 'probe');
  Dict.AddOrSetValue('Rules:0:Name', 'Alpha.*');
  Dict.AddOrSetValue('Rules:0:Enabled', 'true');
  Dict.AddOrSetValue('Rules:0:MinLevel', 'Trace');
  Dict.AddOrSetValue('Rules:0:WriteTo', 'probe');

  var Builder: IConfigurationBuilder := TConfigurationBuilder.Create;
  // La fuente en memoria pasa a POSEER el diccionario; no volver a liberarlo.
  MemoryConfig.AddCollection(Builder, Dict);
  var Config: IConfiguration := Builder.Build as IConfiguration;
  FProvider := TNNLogLoader.BuildProvider(BuildEnv, Config);
end;

procedure TCodeConfigWorld.ReleaseSettings;
begin
  FreeAndNil(FSettings);
end;

procedure TCodeConfigWorld.WriteInfoFrom(const Category: string);
begin
  try
    FProvider.CreateLogger(Category).LogInformation('linea');
  except
    on E: Exception do Failure := E.ClassName + ': ' + E.Message;
  end;
end;

function TCodeConfigWorld.Recorded: string;
begin
  Result := NNLogProbe.Text;
end;

procedure TCodeConfigWorld.ClearProbe;
begin
  NNLogProbe.Clear;
end;

initialization

Feature('''
Feature NNLog se configura con código igual que desde un fichero @nnlog @config

  Como quien arranca una aplicación
  Quiero poder darle a NNLog unos ajustes que he montado yo
  Para no tener que escribir un fichero de configuración cuando los ajustes los decide el código
''')

.UseWorld<TCodeConfigWorld>

.Rule('Unos ajustes escritos con código valen lo mismo que los leídos de una sección')

  .Scenario('Las dos vías anotan lo mismo en el destino')
    .Given('los mismos ajustes por las dos vías, cada una contra el mismo destino de prueba',
      procedure(W: TCodeConfigWorld)
      begin
        W.ComposeFromCode;
        W.WriteInfoFrom('Alpha.Uno');
        W.FromCode := W.Recorded;
        W.ClearProbe;
      end)
    .When('se compone otra vez leyendo esos mismos ajustes de una sección',
      procedure(W: TCodeConfigWorld)
      begin
        W.ComposeFromSection;
        W.WriteInfoFrom('Alpha.Uno');
        W.FromSection := W.Recorded;
      end)
    .&Then('la vía por código anotó la línea, y no un silencio que pasaría igual',
      procedure(W: TCodeConfigWorld)
      begin
        // El control de la comparación de abajo: dos silencios idénticos también son iguales.
        Expect(W.FromCode).ToContain('Alpha.Uno');
      end)
    .&And('y las dos vías anotaron exactamente lo mismo',
      procedure(W: TCodeConfigWorld)
      begin
        Expect(W.FromCode).ToEqual(W.FromSection);
      end)

.Rule('El objeto de ajustes sigue siendo de quien lo trajo')

  .Scenario('El proveedor no se queda con los ajustes')
    .Given('un proveedor construido con unos ajustes escritos con código',
      procedure(W: TCodeConfigWorld)
      begin
        W.ComposeFromCode;
      end)
    .When('quien los escribió los libera, y después se registra una línea',
      procedure(W: TCodeConfigWorld)
      begin
        W.ReleaseSettings;
        W.ClearProbe;
        W.WriteInfoFrom('Alpha.Dos');
      end)
    .&Then('la línea llega al destino igual',
      procedure(W: TCodeConfigWorld)
      begin
        // Si el proveedor se hubiera quedado una referencia a los ajustes, aquí habría un puntero
        // colgando. Se afirma sobre lo que anotó, no sobre que no lanzara: un acceso a memoria
        // liberada no siempre lanza.
        Expect(W.Recorded).ToContain('Alpha.Dos');
      end)
    .&And('y no hubo ningún fallo por el camino',
      procedure(W: TCodeConfigWorld)
      begin
        Expect(W.Failure).ToEqual('');
      end);

end.
