unit RuleSelection.Feat;

// Los dos defectos que cubre fallan en silencio y en direcciones
// opuestas —una regla sin `enabled` no escribe nada; una regla con `name` distinto de `*` lo
// escribe todo—, así que ningún escenario afirma «se escribió» o «no se lanzó»: cada uno afirma
// QUÉ anotó el destino de prueba (categoría por categoría), que es lo único que el código roto no
// puede fingir.
//
// El proveedor se construye SIEMPRE desde una configuración en memoria y por el loader real
// (TNNLogLoader.BuildProvider), no a mano: así el bind de `enabled`, la compilación del patrón y el
// filtrado por logger se ejercitan como en producción.

interface

uses
  System.Generics.Collections,
  Daf.Extensions.Logging,
  Daf.Extensions.Hosting,
  Daf.NNLog.Targets.Probe;

type
  /// El mundo de cada escenario: acumula targets y reglas como pares de configuración, arma la
  /// config en memoria y pide al loader el proveedor. Un escenario nuevo se lee como su config.
  TRuleWorld = class
  strict private
    FDict: TDictionary<string, string>;
    FRuleCount: Integer;
    FProvider: ILoggerProvider;
    FEnv: IHostEnvironment;
    function BuildEnv: IHostEnvironment;
    procedure PutRule(const NamePattern, WriteTo, Enabled: string; const HasEnabled: Boolean);
  public
    Failure: string;
    FailureClass: string;
    constructor Create;
    destructor Destroy; override;
    procedure AddTarget(const Key: string);
    procedure AddRuleActive(const NamePattern, WriteTo: string);
    procedure AddRuleNoEnabled(const NamePattern, WriteTo: string);
    procedure AddRuleDisabled(const NamePattern, WriteTo: string);
    procedure Compose;
    procedure WriteInfoFrom(const Category: string);
    function Probe: TProbeRecorder;
  end;

implementation

uses
  System.SysUtils,
  Daf.Extensions.Configuration,
  Daf.Configuration.Builder,
  Daf.Configuration.Memory,
  Daf.Hosting,
  Daf.NNLog.Loader,
  Daf.MiniSpec;

{ TRuleWorld }

constructor TRuleWorld.Create;
begin
  inherited Create;
  FDict := TDictionary<string, string>.Create;
  FRuleCount := 0;
  NNLogProbe.Clear;
end;

destructor TRuleWorld.Destroy;
begin
  FProvider := nil;
  FEnv := nil;
  // Tras Compose, la propiedad del diccionario pasa a la fuente de configuración (FDict := nil);
  // si un escenario no llegó a componer, lo liberamos aquí.
  if Assigned(FDict) then
    FDict.Free;
  inherited;
end;

procedure TRuleWorld.AddTarget(const Key: string);
begin
  FDict.AddOrSetValue('Targets:' + Key + ':TargetType', 'Probe');
  FDict.AddOrSetValue('Targets:' + Key + ':Name', Key);
end;

procedure TRuleWorld.PutRule(const NamePattern, WriteTo, Enabled: string; const HasEnabled: Boolean);
begin
  var P := 'Rules:' + IntToStr(FRuleCount) + ':';
  FDict.AddOrSetValue(P + 'Name', NamePattern);
  FDict.AddOrSetValue(P + 'WriteTo', WriteTo);
  FDict.AddOrSetValue(P + 'MinLevel', 'Trace');
  if HasEnabled then
    FDict.AddOrSetValue(P + 'Enabled', Enabled);
  Inc(FRuleCount);
end;

procedure TRuleWorld.AddRuleActive(const NamePattern, WriteTo: string);
begin
  PutRule(NamePattern, WriteTo, 'true', True);
end;

procedure TRuleWorld.AddRuleNoEnabled(const NamePattern, WriteTo: string);
begin
  PutRule(NamePattern, WriteTo, '', False);
end;

procedure TRuleWorld.AddRuleDisabled(const NamePattern, WriteTo: string);
begin
  PutRule(NamePattern, WriteTo, 'false', True);
end;

function TRuleWorld.BuildEnv: IHostEnvironment;
begin
  if FEnv = nil then
  begin
    var CB: IConfigurationBuilder := TConfigurationBuilder.Create;
    var EmptyConfig: IConfiguration := CB.Build as IConfiguration;
    FEnv := THostEnvironment.Create(EmptyConfig) as IHostEnvironment;
  end;
  Result := FEnv;
end;

procedure TRuleWorld.Compose;
begin
  Failure := '';
  FailureClass := '';
  var Builder: IConfigurationBuilder := TConfigurationBuilder.Create;
  // La fuente en memoria pasa a POSEER el diccionario; no volver a liberarlo.
  MemoryConfig.AddCollection(Builder, FDict);
  FDict := nil;
  var Config: IConfiguration := Builder.Build as IConfiguration;
  try
    FProvider := TNNLogLoader.BuildProvider(BuildEnv, Config);
  except
    on E: Exception do
    begin
      Failure := E.Message;
      FailureClass := E.ClassName;
    end;
  end;
end;

procedure TRuleWorld.WriteInfoFrom(const Category: string);
begin
  FProvider.CreateLogger(Category).LogInformation('linea');
end;

function TRuleWorld.Probe: TProbeRecorder;
begin
  Result := NNLogProbe;
end;

initialization

Feature('''
Feature Una regla de NNLog hace lo que su configuración dice @nnlog @rules

  Como quien configura el logging de una aplicación
  Quiero que una regla se aplique a las categorías que su patrón nombra y esté activa salvo que lo
  diga lo contrario
  Para que nadie depure un incidente sin registros que la configuración prometía, ni con registros
  de más en un destino que creía acotado
''')

.UseWorld<TRuleWorld>

.Rule('Una regla se aplica a las categorías que su patrón nombra')

  .Scenario('El patrón que nombra una familia deja fuera a las demás')
    .Given('una regla cuyo patrón nombra la familia Alpha y un destino que anota lo que recibe',
      procedure(W: TRuleWorld)
      begin
        W.AddTarget('probe');
        W.AddRuleActive('Alpha.*', 'probe');
        W.Compose;
        Expect(W.Failure).ToEqual('');
      end)
    .When('se escriben líneas desde una categoría de la familia y desde otra que no lo es',
      procedure(W: TRuleWorld)
      begin
        W.WriteInfoFrom('Alpha.One');
        W.WriteInfoFrom('Beta.One');
      end)
    .&Then('el destino recibe la de la familia que el patrón nombra',
      procedure(W: TRuleWorld)
      begin
        Expect(W.Probe.ReceivedFrom('probe', 'Alpha.One')).ToBeTrue;
      end)
    .&And('y no la que queda fuera',
      procedure(W: TRuleWorld)
      begin
        Expect(W.Probe.ReceivedFrom('probe', 'Beta.One')).ToBeFalse;
      end)

  .Scenario('El patrón que lo nombra todo sigue nombrándolo todo')
    .When('una regla usa el patrón que significa todas las categorías',
      procedure(W: TRuleWorld)
      begin
        W.AddTarget('probe');
        W.AddRuleActive('*', 'probe');
        W.Compose;
        W.WriteInfoFrom('Cualquier.Cosa');
      end)
    .&Then('recibe líneas de cualquier categoría, igual que antes de este cambio',
      procedure(W: TRuleWorld)
      begin
        Expect(W.Probe.ReceivedFrom('probe', 'Cualquier.Cosa')).ToBeTrue;
      end)

  .Scenario('Un patrón corto no se aplica a lo que solo lo contiene')
    .Given('una regla cuyo patrón nombra exactamente la categoría Acme',
      procedure(W: TRuleWorld)
      begin
        W.AddTarget('probe');
        W.AddRuleActive('Acme', 'probe');
        W.Compose;
      end)
    .When('se escribe desde Acme y desde otra que la contiene como parte del suyo',
      procedure(W: TRuleWorld)
      begin
        W.WriteInfoFrom('Acme');
        W.WriteInfoFrom('Acme.Sub');
      end)
    .&Then('recibe la categoría exacta',
      procedure(W: TRuleWorld)
      begin
        Expect(W.Probe.ReceivedFrom('probe', 'Acme')).ToBeTrue;
      end)
    .&And('y no la que solo la contiene',
      procedure(W: TRuleWorld)
      begin
        Expect(W.Probe.ReceivedFrom('probe', 'Acme.Sub')).ToBeFalse;
      end)

  .Scenario('Dos loggers de categorías distintas ven cada uno sus reglas')
    .Given('dos reglas, cada una a una familia y a su propio destino',
      procedure(W: TRuleWorld)
      begin
        W.AddTarget('probeA');
        W.AddTarget('probeB');
        W.AddRuleActive('Alpha.*', 'probeA');
        W.AddRuleActive('Beta.*', 'probeB');
        W.Compose;
      end)
    .When('se piden dos loggers al mismo proveedor y cada uno escribe desde su categoría',
      procedure(W: TRuleWorld)
      begin
        W.WriteInfoFrom('Alpha.One');
        W.WriteInfoFrom('Beta.One');
      end)
    .&Then('cada destino recibe la familia de su regla',
      procedure(W: TRuleWorld)
      begin
        Expect(W.Probe.ReceivedFrom('probeA', 'Alpha.One')).ToBeTrue;
        Expect(W.Probe.ReceivedFrom('probeB', 'Beta.One')).ToBeTrue;
      end)

.Rule('Una regla está activa salvo que su configuración diga lo contrario')

  .Scenario('La regla sin declarar nada escribe')
    .Given('una configuración con una regla que no dice si está activa',
      procedure(W: TRuleWorld)
      begin
        W.AddTarget('probe');
        W.AddRuleNoEnabled('*', 'probe');
        W.Compose;
      end)
    .When('se escribe una línea que esa regla acepta por nivel y por categoría',
      procedure(W: TRuleWorld)
      begin
        W.WriteInfoFrom('Cualquiera');
      end)
    .&Then('el destino la recibe',
      procedure(W: TRuleWorld)
      begin
        Expect(W.Probe.ReceivedFrom('probe', 'Cualquiera')).ToBeTrue;
      end)

  .Scenario('La regla desactivada a propósito no escribe')
    .Given('una configuración con una regla declarada como no activa',
      procedure(W: TRuleWorld)
      begin
        W.AddTarget('probe');
        W.AddRuleDisabled('*', 'probe');
        W.Compose;
      end)
    .When('se escribe una línea que esa regla aceptaría por nivel y por categoría',
      procedure(W: TRuleWorld)
      begin
        W.WriteInfoFrom('Cualquiera');
      end)
    .&Then('el destino no recibe nada por esa regla',
      procedure(W: TRuleWorld)
      begin
        Expect(W.Probe.Count).ToEqual(0);
      end)

.Rule('Un patrón que no se puede cumplir se rechaza al cargar la configuración')

  .Scenario('El patrón imposible se dice al cargar')
    .When('se carga una configuración cuya regla lleva un patrón de categoría que no se puede interpretar',
      procedure(W: TRuleWorld)
      begin
        W.AddTarget('probe');
        W.AddRuleActive('[', 'probe');
        W.Compose;
      end)
    .&Then('la carga falla nombrando el patrón',
      procedure(W: TRuleWorld)
      begin
        Expect(W.Failure).ToContain('[');
      end)
    .&And('y nombrando la regla',
      procedure(W: TRuleWorld)
      begin
        Expect(W.Failure).ToContain('#0');
      end);

end.
