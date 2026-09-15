unit BinderAbsence.Feat;

{
  Que escribe el enlazador para lo que la configuracion NO dice.

  Los defectos de partida de estas clases --'por-defecto', 8080, 'localhost'-- no son el cero de
  su tipo, y eso es lo que hace que los escenarios midan algo: sobre un defecto de '' o 0, "no se
  escribio" y "se escribio el cero" dan el mismo verde y el mismo fallo.
}

interface

uses
  System.Generics.Collections,
  Daf.Extensions.Configuration;

type
  {$M+}
  /// <summary>
  ///   La seccion anidada. Su constructor pone los valores que un enlace sin datos tiene que
  ///   respetar.
  /// </summary>
  TEndpointSettings = class
  private
    FHost: string;
    FPort: Integer;
  public
    constructor Create;
    property Host: string read FHost write FHost;
    property Port: Integer read FPort write FPort;
  end;

  /// <summary>
  ///   Unos ajustes con las cuatro formas de miembro que el enlazador reparte: planos, una clase
  ///   anidada, una coleccion y un array. La clase y la coleccion arrancan en nil, que es la
  ///   condicion para afirmar que el enlace los construye; el array arranca con un elemento,
  ///   porque lo que se mide de el es si sobrevive.
  /// </summary>
  TServerSettings = class
  private
    FName: string;
    FPort: Integer;
    FEndpoint: TEndpointSettings;
    FTags: TList<string>;
    FAliases: TArray<string>;
  public
    constructor Create;
    destructor Destroy; override;
    property Name: string read FName write FName;
    property Port: Integer read FPort write FPort;
    property Endpoint: TEndpointSettings read FEndpoint write FEndpoint;
    property Tags: TList<string> read FTags write FTags;
    property Aliases: TArray<string> read FAliases write FAliases;
  end;
  {$M-}

  TAbsenceWorld = class
  public
    Config: IConfigurationRoot;
    Second: IConfigurationRoot;
    Settings: TServerSettings;
    Report: string;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.Configuration.Builder,
  Daf.Configuration.Memory,
  Daf.Configuration.Binder;

{ TEndpointSettings }

constructor TEndpointSettings.Create;
begin
  inherited Create;
  FHost := 'localhost';
  FPort := 5432;
end;

{ TServerSettings }

constructor TServerSettings.Create;
begin
  inherited Create;
  FName := 'por-defecto';
  FPort := 8080;
  FAliases := ['alfa'];
end;

destructor TServerSettings.Destroy;
begin
  FEndpoint.Free;
  FTags.Free;
  inherited;
end;

{ TAbsenceWorld }

destructor TAbsenceWorld.Destroy;
begin
  Settings.Free;
  inherited;
end;

function ConfigOf(const Pairs: array of string): IConfigurationRoot;
begin
  var Builder: IConfigurationBuilder := TConfigurationBuilder.Create;
  var Dic := TDictionary<string, string>.Create;
  var I := 0;
  while I < Length(Pairs) do
  begin
    Dic.Add(Pairs[I], Pairs[I + 1]);
    Inc(I, 2);
  end;
  MemoryConfig.AddCollection(Builder, Dic);
  Result := Builder.Build;
end;

// Entre corchetes porque una cadena vacia tiene que VERSE en el informe: es la mitad que
// distingue "no se escribio" de "se escribio lo que la configuracion decia".
function Show(const S: string): string;
begin
  Result := '[' + S + ']';
end;

// --- Feature definition ---

initialization

Feature('''
Feature Configuration binder absence @configuration @binder @absence

  As a developer binding settings
  I want binding to write what the configuration says and not what it does not say
  So that a default put in the constructor survives a key no source mentions
''')

.UseWorld<TAbsenceWorld>

.Rule('A plain member whose key is absent is not written')

  .Scenario('The constructor default survives')
    .Given('settings whose constructor fills both plain members',
      procedure(W: TAbsenceWorld)
      begin
        W.Config := ConfigOf(['Unrelated', 'value']);
      end)
    .When('a configuration that mentions neither key is bound',
      procedure(W: TAbsenceWorld)
      begin
        W.Settings := TConfigurationBinder.Bind<TServerSettings>(W.Config);
        W.Report := Format('Name=%s Port=%d', [Show(W.Settings.Name), W.Settings.Port]);
      end)
    .&Then('both members keep what the constructor put there',
      procedure(W: TAbsenceWorld)
      begin
        Expect(W.Report).ToEqual('Name=[por-defecto] Port=8080');
      end)

  .Scenario('A key that is present with an empty value is written')
    .Given('a configuration where Name is present and empty, and Port is not there at all',
      procedure(W: TAbsenceWorld)
      begin
        W.Config := ConfigOf(['Name', '']);
      end)
    .When('the configuration is bound',
      procedure(W: TAbsenceWorld)
      begin
        W.Settings := TConfigurationBinder.Bind<TServerSettings>(W.Config);
        W.Report := Format('Name=%s Port=%d', [Show(W.Settings.Name), W.Settings.Port]);
      end)
    .&Then('Name is empty because the configuration said so, and Port is untouched',
      procedure(W: TAbsenceWorld)
      begin
        // Las dos mitades en la misma asercion: si el enlace escribiera siempre, Port seria 0;
        // si no escribiera nunca, Name seguiria siendo 'por-defecto'.
        Expect(W.Report).ToEqual('Name=[] Port=8080');
      end)

  .Scenario('A second bind does not erase what the first one brought')
    .Given('settings already bound from a source that carried both keys',
      procedure(W: TAbsenceWorld)
      begin
        W.Config := ConfigOf(['Name', 'de-la-primera', 'Port', '1234']);
        W.Settings := TConfigurationBinder.Bind<TServerSettings>(W.Config);
      end)
    .When('a second configuration that mentions neither key is bound over the same object',
      procedure(W: TAbsenceWorld)
      begin
        W.Second := ConfigOf(['Unrelated', 'value']);
        TConfigurationBinder.Bind(W.Second, '', W.Settings, boProperties);
        W.Report := Format('Name=%s Port=%d', [Show(W.Settings.Name), W.Settings.Port]);
      end)
    .&Then('the values of the first bind are still there',
      procedure(W: TAbsenceWorld)
      begin
        Expect(W.Report).ToEqual('Name=[de-la-primera] Port=1234');
      end)

.Rule('A plain member is answered by the value of its own key')

  // Esta es la medida que elige la consulta. Exists responde por "valor propio O hijos", asi que
  // una clave que solo agrupa existe y no tiene nada que escribir en un Integer.
  .Scenario('A key that only groups has nothing to write into a plain member')
    .Given('a configuration where Port only groups: Port:Http has a value and Port has none of its own',
      procedure(W: TAbsenceWorld)
      begin
        W.Config := ConfigOf(['Port:Http', '80']);
      end)
    .When('the two ways of asking for its presence are compared',
      procedure(W: TAbsenceWorld)
      begin
        W.Report := Format('Exists=%s Find=%s',
          [BoolToStr(W.Config.Exists('Port'), True), BoolToStr(W.Config.Find('Port').HasValue, True)]);
      end)
    .&Then('Exists says yes and Find says no',
      procedure(W: TAbsenceWorld)
      begin
        Expect(W.Report).ToEqual('Exists=True Find=False');
      end)
    .&And('binding leaves the constructor default, because the plain member goes by the own value',
      procedure(W: TAbsenceWorld)
      begin
        W.Settings := TConfigurationBinder.Bind<TServerSettings>(W.Config);
        Expect(W.Settings.Port).ToEqual(8080);
      end)

.Rule('A composite member is built even when its section is not there')

  .Scenario('An absent section leaves the child object with its own defaults')
    .Given('a configuration that does not mention the Endpoint section',
      procedure(W: TAbsenceWorld)
      begin
        W.Config := ConfigOf(['Name', 'con-nombre']);
      end)
    .When('the configuration is bound',
      procedure(W: TAbsenceWorld)
      begin
        W.Settings := TConfigurationBinder.Bind<TServerSettings>(W.Config);
      end)
    .&Then('the member is not nil',
      procedure(W: TAbsenceWorld)
      begin
        Expect(Assigned(W.Settings.Endpoint)).ToBeTrue;
      end)
    .&And('and it carries the values its own constructor put there',
      procedure(W: TAbsenceWorld)
      begin
        Expect(Format('%s/%d', [W.Settings.Endpoint.Host, W.Settings.Endpoint.Port]))
          .ToEqual('localhost/5432');
      end)

  // La diferencia entre las dos formas de compuesto, medida y no descubierta: la coleccion se
  // REUTILIZA --el enlace anade sobre la que ya habia-- y el array se SUSTITUYE, porque no hay
  // array que reutilizar y el enlace devuelve uno nuevo por construccion.
  .Scenario('An array member is replaced where a collection is kept')
    .Given('settings bound from a source that filled both the collection and the array',
      procedure(W: TAbsenceWorld)
      begin
        W.Config := ConfigOf(['Tags:0', 'uno', 'Aliases:0', 'alfa']);
        W.Settings := TConfigurationBinder.Bind<TServerSettings>(W.Config);
        W.Report := Format('primero Tags=%d Aliases=%d',
          [W.Settings.Tags.Count, Length(W.Settings.Aliases)]);
      end)
    .When('a second configuration that mentions neither section is bound over the same object',
      procedure(W: TAbsenceWorld)
      begin
        W.Second := ConfigOf(['Unrelated', 'value']);
        TConfigurationBinder.Bind(W.Second, '', W.Settings, boProperties);
        W.Report := W.Report + Format(' | despues Tags=%d Aliases=%d',
          [W.Settings.Tags.Count, Length(W.Settings.Aliases)]);
      end)
    .&Then('the collection keeps its elements and the array comes back empty',
      procedure(W: TAbsenceWorld)
      begin
        Expect(W.Report).ToEqual('primero Tags=1 Aliases=1 | despues Tags=1 Aliases=0');
      end)

  .Scenario('A collection whose section is not there ends up empty and not nil')
    .Given('a configuration that does not mention the Tags section',
      procedure(W: TAbsenceWorld)
      begin
        W.Config := ConfigOf(['Name', 'con-nombre']);
      end)
    .When('the configuration is bound',
      procedure(W: TAbsenceWorld)
      begin
        W.Settings := TConfigurationBinder.Bind<TServerSettings>(W.Config);
        if Assigned(W.Settings.Tags) then
          W.Report := Format('Count=%d', [W.Settings.Tags.Count])
        else
          W.Report := 'nil';
      end)
    .&Then('the member is a collection with no elements',
      procedure(W: TAbsenceWorld)
      begin
        Expect(W.Report).ToEqual('Count=0');
      end)

.Rule('The absence is checked against a value other than the zero of the type')

  // El escenario que hace que los demas valgan: el informe NOMBRA el valor que llego, asi que si
  // el enlazador volviera a escribir lo ausente se leeria 'tras-enlazar=[]/0' y no un silencio.
  .Scenario('The scenario fails if the rule is withdrawn')
    .Given('settings whose defaults are not the zero of their type',
      procedure(W: TAbsenceWorld)
      begin
        W.Settings := TServerSettings.Create;
        W.Report := Format('defecto=%s/%d', [Show(W.Settings.Name), W.Settings.Port]);
        W.Config := ConfigOf(['Unrelated', 'value']);
      end)
    .When('a configuration that mentions neither key is bound',
      procedure(W: TAbsenceWorld)
      begin
        TConfigurationBinder.Bind(W.Config, '', W.Settings, boProperties);
        W.Report := W.Report + Format(' tras-enlazar=%s/%d', [Show(W.Settings.Name), W.Settings.Port]);
      end)
    .&Then('the report names the value that arrived, so a zero would give it away',
      procedure(W: TAbsenceWorld)
      begin
        Expect(W.Report).ToEqual('defecto=[por-defecto]/8080 tras-enlazar=[por-defecto]/8080');
      end);

end.
