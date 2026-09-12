unit Options.Feat;

{
  El patron de opciones: unos ajustes que el contenedor construye, valida y POSEE.

  Casi ninguna afirmacion de aqui es un recuento. Lo que este patron viene a quitar es una fuga,
  y una fuga no lanza: se afirma sobre CUANTOS objetos siguen vivos despues de cerrar, y sobre
  QUE dice el fallo cuando algo no vale. Un contenedor que no liberase nada pasaria cualquier
  spec que solo comprobara que el valor llega.

  Por eso cada contador de vida viene con su control de que el objeto llego a construirse: sin
  el, "no queda ninguno vivo" y "no se construyo ninguno" imprimen la misma cifra.
}

interface

uses
  System.SysUtils,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Options;

type
  {$M+}
  /// <summary>
  ///   Unos ajustes corrientes. Las class var son el instrumento de medida del spec, no estado
  ///   de los ajustes: publicas a proposito.
  /// </summary>
  TCacheSettings = class
  private
    FMaxItems: Integer;
    FHost: string;
  public
    class var Built: Integer;
    class var Alive: Integer;
    constructor Create;
    destructor Destroy; override;
    property MaxItems: Integer read FMaxItems write FMaxItems;
    property Host: string read FHost write FHost;
  end;

  /// <summary>
  ///   Un SEGUNDO tipo de ajustes, y no una segunda instancia del primero: todas las
  ///   instanciaciones de una interfaz generica comparten el GUID declarado, asi que "cada tipo
  ///   devuelve lo suyo" es justo lo que no se puede dar por hecho.
  /// </summary>
  TDbSettings = class
  private
    FConnection: string;
  public
    property Connection: string read FConnection write FConnection;
  end;

  /// <summary>Nadie lo registra en ningun escenario. Ese es su papel entero.</summary>
  TOrphanSettings = class
  private
    FTag: string;
  public
    property Tag: string read FTag write FTag;
  end;

  /// <summary>
  ///   El miembro de tipo objeto. Lleva su propio contador de vivos porque la regla que se
  ///   afirma es que lo libera SU DUEÑO --los ajustes que lo contienen-- y no el patron.
  /// </summary>
  TDbSection = class
  private
    FServer: string;
  public
    class var Built: Integer;
    class var Alive: Integer;
    constructor Create;
    destructor Destroy; override;
    property Server: string read FServer write FServer;
  end;

  /// <summary>
  ///   Unos ajustes con un miembro objeto que SI libera lo suyo. Quitarle el destructor es
  ///   exactamente el fallo que su escenario existe para ver.
  /// </summary>
  TNestedSettings = class
  private
    FDb: TDbSection;
  public
    constructor Create;
    destructor Destroy; override;
    property Db: TDbSection read FDb write FDb;
  end;
  {$M-}

  /// <summary>
  ///   El servicio que pide sus ajustes por constructor. COPIA lo que necesita y no se guarda el
  ///   objeto: Value devuelve un objeto crudo cuyo dueño es la opcion, asi que quedarselo y
  ///   soltar la opcion dejaria un puntero colgando. La otra forma correcta es sostener la
  ///   opcion en un campo, que es lo que hace THolder mas abajo.
  /// </summary>
  ICache = interface(IInvokable)
    ['{0DE4D28B-5E5B-4B9A-8B0D-4E9E5D4E9A01}']
    function Describe: string;
  end;

  TCache = class(TInterfacedObject, ICache)
  strict private
    FMaxItems: Integer;
    FHost: string;
  public
    constructor Create(const Options: IOptions<TCacheSettings>);
    function Describe: string;
  end;

  /// <summary>
  ///   Dos contratos distintos servidos por la misma clase: hacen falta DOS servicios para poder
  ///   afirmar que los dos ven el MISMO objeto de ajustes. Con uno solo, "compartido" y "uno por
  ///   servicio" darian el mismo resultado.
  /// </summary>
  ISettingsHolder = interface(IInvokable)
    ['{0DE4D28B-5E5B-4B9A-8B0D-4E9E5D4E9A02}']
    function Settings: TCacheSettings;
  end;

  IOtherHolder = interface(IInvokable)
    ['{0DE4D28B-5E5B-4B9A-8B0D-4E9E5D4E9A03}']
    function Settings: TCacheSettings;
  end;

  THolder = class(TInterfacedObject, ISettingsHolder, IOtherHolder)
  strict private
    FOptions: IOptions<TCacheSettings>;
  public
    constructor Create(const AOptions: IOptions<TCacheSettings>);
    function Settings: TCacheSettings;
  end;

  TOptionsWorld = class
  public
    Services: IServiceCollection;
    Provider: IServiceProvider;
    Told: string;
    Outcome: string;
    Validations: Integer;
    SameObject: Boolean;
    Closed: Boolean;
    BuiltAfterCompose: Integer;
    BuiltAfterRead: Integer;
    AliveAfterClose: Integer;
    SectionBuilt: Integer;
    SectionAliveAfterClose: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure Build;
    /// Lee el servidor de los ajustes anidados y SUELTA la opcion antes de volver. Va en un
    /// metodo propio, y no escrito en el escenario, porque Delphi finaliza las referencias de
    /// interfaz temporales al salir del METODO y no al acabar la sentencia: leida alli, la
    /// opcion seguiria sujeta por un temporal invisible durante el cierre, y el escenario
    /// estaria midiendo ese temporal en vez de al contenedor.
    procedure ReadNestedServer;
    /// Cierra el contenedor a proposito y DENTRO del escenario: lo que se afirma pasa despues
    /// del cierre, asi que dejarlo al teardown lo dejaria fuera de toda asercion.
    procedure Close;
    /// Captura clase y mensaje del fallo, o un centinela con texto si no lo hubo. Afirmar solo
    /// que "fallo" da por bueno cualquier fallo, incluido uno que no viene al caso.
    procedure Attempt(const Action: TProc);
  end;

const
  SinFallo = '<sin fallo>';
  // Centinela con texto, y no la cadena vacia: la cadena vacia es tambien lo que se lee cuando
  // nadie escribio nada, asi que no distingue "no contesto" de "contesto vacio".
  SinRespuesta = '<sin respuesta>';

implementation

uses
  Daf.MiniSpec,
  Daf.Options,
  Daf.DependencyInjection;

{ TCacheSettings }

constructor TCacheSettings.Create;
begin
  inherited Create;
  Inc(Built);
  Inc(Alive);
end;

destructor TCacheSettings.Destroy;
begin
  Dec(Alive);
  inherited;
end;

{ TDbSection }

constructor TDbSection.Create;
begin
  inherited Create;
  Inc(Built);
  Inc(Alive);
end;

destructor TDbSection.Destroy;
begin
  Dec(Alive);
  inherited;
end;

{ TNestedSettings }

constructor TNestedSettings.Create;
begin
  inherited Create;
  FDb := TDbSection.Create;
end;

destructor TNestedSettings.Destroy;
begin
  // La regla entera, en una linea: el POCO libera lo suyo. Quitar este destructor es la forma
  // de ver caer el escenario de los anidados.
  FDb.Free;
  inherited;
end;

{ TCache }

constructor TCache.Create(const Options: IOptions<TCacheSettings>);
begin
  inherited Create;
  FHost := Options.Value.Host;
  FMaxItems := Options.Value.MaxItems;
end;

function TCache.Describe: string;
begin
  Result := Format('%s/%d', [FHost, FMaxItems]);
end;

{ THolder }

constructor THolder.Create(const AOptions: IOptions<TCacheSettings>);
begin
  inherited Create;
  FOptions := AOptions;
end;

function THolder.Settings: TCacheSettings;
begin
  Result := FOptions.Value;
end;

{ TOptionsWorld }

constructor TOptionsWorld.Create;
begin
  inherited;
  TCacheSettings.Built := 0;
  TCacheSettings.Alive := 0;
  TDbSection.Built := 0;
  TDbSection.Alive := 0;
  Told := SinRespuesta;
  Outcome := '<sin intentar>';
  Services := TServiceCollection.Create;
end;

destructor TOptionsWorld.Destroy;
begin
  if (Provider <> nil) and not Closed then
    Provider.ShutDown;
  inherited;
end;

procedure TOptionsWorld.Build;
begin
  if Provider = nil then
    Provider := Services.BuildServiceProvider;
end;

procedure TOptionsWorld.ReadNestedServer;
begin
  Told := Provider.GetRequiredService<IOptions<TNestedSettings>>.Value.Db.Server;
end;

procedure TOptionsWorld.Close;
begin
  if Closed then Exit;
  Provider.ShutDown;
  Closed := True;
end;

procedure TOptionsWorld.Attempt(const Action: TProc);
begin
  Outcome := SinFallo;
  try
    Action;
  except
    on E: Exception do Outcome := E.ClassName + ': ' + E.Message;
  end;
end;

// --- Feature definition ---

initialization

Feature('''
Feature A service asks for its settings and owns nothing @di @options

  As an application author
  I want a service to declare in its constructor the settings it needs
  So that reading configuration stops being a lambda that ends in Free
''')

.UseWorld<TOptionsWorld>

.Rule('The container owns the settings object')

  .Scenario('Nobody frees the settings object')
    .Given('settings registered, and a service that receives them by constructor',
      procedure(W: TOptionsWorld)
      begin
        AppOptions.Configure<TCacheSettings>(W.Services,
          procedure(S: TCacheSettings)
          begin
            S.Host := 'cache-host';
            S.MaxItems := 500;
          end);
        W.Services.AddSingleton<ICache, TCache>;
      end)
    .When('the service is used and the container is closed',
      procedure(W: TOptionsWorld)
      begin
        W.Build;
        W.Told := W.Provider.GetRequiredService<ICache>.Describe;
        W.BuiltAfterRead := TCacheSettings.Built;
        W.Close;
        W.AliveAfterClose := TCacheSettings.Alive;
      end)
    .&Then('the settings object had really been built',
      procedure(W: TOptionsWorld)
      begin
        // El control del contador de vivos: sin el, "no queda ninguno vivo" y "no se construyo
        // ninguno" son la misma cifra.
        Expect(W.BuiltAfterRead).ToEqual(1);
      end)
    .&And('and the service got its values without freeing anything',
      procedure(W: TOptionsWorld)
      begin
        Expect(W.Told).ToEqual('cache-host/500');
      end)
    .&And('and no settings object is left alive once the container closes',
      procedure(W: TOptionsWorld)
      begin
        Expect(W.AliveAfterClose).ToEqual(0);
      end)

  .Scenario('The object stays alive as long as its registration does')
    .Given('two different services that receive the same settings',
      procedure(W: TOptionsWorld)
      begin
        AppOptions.Configure<TCacheSettings>(W.Services,
          procedure(S: TCacheSettings)
          begin
            S.Host := 'shared-host';
            S.MaxItems := 10;
          end);
        W.Services.AddSingleton<ISettingsHolder, THolder>;
        W.Services.AddSingleton<IOtherHolder, THolder>;
      end)
    .When('both are resolved and both read their settings',
      procedure(W: TOptionsWorld)
      begin
        W.Build;
        var One := W.Provider.GetRequiredService<ISettingsHolder>.Settings;
        var Other := W.Provider.GetRequiredService<IOtherHolder>.Settings;
        W.SameObject := One = Other;
        W.Told := One.Host + '|' + Other.Host;
        W.BuiltAfterRead := TCacheSettings.Built;
      end)
    .&Then('both see literally the same object',
      procedure(W: TOptionsWorld)
      begin
        Expect(W.SameObject).ToBeTrue;
      end)
    .&And('and it is one object, not one per service',
      procedure(W: TOptionsWorld)
      begin
        Expect(W.BuiltAfterRead).ToEqual(1);
      end)
    .&And('and it is still valid while the container is open',
      procedure(W: TOptionsWorld)
      begin
        Expect(W.Told).ToEqual('shared-host|shared-host');
      end)

.Rule('The value is built once, and on first use')

  .Scenario('Registering does not build')
    .Given('settings registered',
      procedure(W: TOptionsWorld)
      begin
        AppOptions.Configure<TCacheSettings>(W.Services,
          procedure(S: TCacheSettings)
          begin
            S.Host := 'lazy-host';
          end);
      end)
    .When('the container is built, nobody asks, and only then somebody asks',
      procedure(W: TOptionsWorld)
      begin
        W.Build;
        W.BuiltAfterCompose := TCacheSettings.Built;
        W.Told := W.Provider.GetRequiredService<IOptions<TCacheSettings>>.Value.Host;
        W.BuiltAfterRead := TCacheSettings.Built;
      end)
    .&Then('composing built no settings object',
      procedure(W: TOptionsWorld)
      begin
        Expect(W.BuiltAfterCompose).ToEqual(0);
      end)
    .&And('and asking for it does build it, so that zero was not for want of a registration',
      procedure(W: TOptionsWorld)
      begin
        // El control positivo del cero de arriba: sin el, un registro que no registrase nada en
        // absoluto pasaria igual.
        Expect(W.BuiltAfterRead).ToEqual(1);
      end)
    .&And('and what it gives back is what was configured',
      procedure(W: TOptionsWorld)
      begin
        Expect(W.Told).ToEqual('lazy-host');
      end)

  .Scenario('Two reads, one object')
    .Given('settings registered',
      procedure(W: TOptionsWorld)
      begin
        AppOptions.Configure<TCacheSettings>(W.Services,
          procedure(S: TCacheSettings)
          begin
            S.Host := 'once-host';
          end);
      end)
    .When('the value is asked for twice',
      procedure(W: TOptionsWorld)
      begin
        W.Build;
        var Options := W.Provider.GetRequiredService<IOptions<TCacheSettings>>;
        var One := Options.Value;
        var Other := Options.Value;
        W.SameObject := One = Other;
        W.BuiltAfterRead := TCacheSettings.Built;
      end)
    .&Then('both reads gave the same object',
      procedure(W: TOptionsWorld)
      begin
        Expect(W.SameObject).ToBeTrue;
      end)
    .&And('and it was built once',
      procedure(W: TOptionsWorld)
      begin
        Expect(W.BuiltAfterRead).ToEqual(1);
      end)

.Rule('Settings can be configured with no external source')

  .Scenario('Settings set with code reach the service')
    .Given('settings filled in by a delegate, with no configuration involved',
      procedure(W: TOptionsWorld)
      begin
        AppOptions.Configure<TCacheSettings>(W.Services,
          procedure(S: TCacheSettings)
          begin
            S.Host := 'coded-host';
            S.MaxItems := 42;
          end);
        W.Services.AddSingleton<ICache, TCache>;
      end)
    .When('the service is resolved',
      procedure(W: TOptionsWorld)
      begin
        W.Build;
        W.Told := W.Provider.GetRequiredService<ICache>.Describe;
      end)
    .&Then('it received those values',
      procedure(W: TOptionsWorld)
      begin
        Expect(W.Told).ToEqual('coded-host/42');
      end)

  .Scenario('A test substitutes the settings without touching the service')
    .Given('the same service, with settings a test wrote',
      procedure(W: TOptionsWorld)
      begin
        AppOptions.Configure<TCacheSettings>(W.Services,
          procedure(S: TCacheSettings)
          begin
            S.Host := 'from-the-test';
            S.MaxItems := 7;
          end);
        W.Services.AddSingleton<ICache, TCache>;
      end)
    .When('the service under test is resolved',
      procedure(W: TOptionsWorld)
      begin
        W.Build;
        W.Told := W.Provider.GetRequiredService<ICache>.Describe;
      end)
    .&Then('it uses them without knowing they did not come from a file',
      procedure(W: TOptionsWorld)
      begin
        Expect(W.Told).ToEqual('from-the-test/7');
      end)

.Rule('Validation runs when the value is built, and names the setting')

  .Scenario('An invalid setting aborts on first use, naming it')
    .Given('settings with a validator, filled in with a value the validator rejects',
      procedure(W: TOptionsWorld)
      begin
        AppOptions.Configure<TCacheSettings>(W.Services,
          procedure(S: TCacheSettings)
          begin
            S.Host := 'invalid-host';
            S.MaxItems := 0;
          end,
          function(const S: TCacheSettings; out Error: string): Boolean
          begin
            Inc(W.Validations);
            Result := S.MaxItems > 0;
            if not Result then
              Error := Format('MaxItems must be greater than zero, and it is %d', [S.MaxItems]);
          end);
      end)
    .When('the value is asked for',
      procedure(W: TOptionsWorld)
      begin
        W.Build;
        W.Attempt(
          procedure
          begin
            W.Told := 'gave back ' + W.Provider.GetRequiredService<IOptions<TCacheSettings>>.Value.Host;
          end);
      end)
    .&Then('it aborts naming the setting that failed',
      procedure(W: TOptionsWorld)
      begin
        // Sobre el mensaje, y no sobre que lanzara: "configuracion invalida" a secas obliga a
        // leerse el fichero entero, que es el trabajo que este patron viene a quitar.
        Expect(W.Outcome).ToContain('MaxItems');
      end)
    .&And('and it is the validation failure, not some other one',
      procedure(W: TOptionsWorld)
      begin
        Expect(W.Outcome).ToContain('EOptionsValidationError');
      end)
    .&And('and nothing was handed back',
      procedure(W: TOptionsWorld)
      begin
        Expect(W.Told).ToEqual(SinRespuesta);
      end)

  .Scenario('Valid settings do not pay for validation twice')
    .Given('settings with a validator, filled in with a value it accepts',
      procedure(W: TOptionsWorld)
      begin
        AppOptions.Configure<TCacheSettings>(W.Services,
          procedure(S: TCacheSettings)
          begin
            S.Host := 'valid-host';
            S.MaxItems := 500;
          end,
          function(const S: TCacheSettings; out Error: string): Boolean
          begin
            Inc(W.Validations);
            Result := S.MaxItems > 0;
            if not Result then
              Error := 'MaxItems must be greater than zero';
          end);
      end)
    .When('the value is asked for twice',
      procedure(W: TOptionsWorld)
      begin
        W.Build;
        var Options := W.Provider.GetRequiredService<IOptions<TCacheSettings>>;
        W.Told := Options.Value.Host + '|' + Options.Value.Host;
      end)
    .&Then('the validation ran once',
      procedure(W: TOptionsWorld)
      begin
        Expect(W.Validations).ToEqual(1);
      end)
    .&And('and both reads gave the value',
      procedure(W: TOptionsWorld)
      begin
        Expect(W.Told).ToEqual('valid-host|valid-host');
      end)

.Rule('Each settings type is a service of its own')

  .Scenario('Two settings types live together')
    .Given('options registered for two different settings types',
      procedure(W: TOptionsWorld)
      begin
        AppOptions.Configure<TCacheSettings>(W.Services,
          procedure(S: TCacheSettings)
          begin
            S.Host := 'cache-host';
          end);
        AppOptions.Configure<TDbSettings>(W.Services,
          procedure(S: TDbSettings)
          begin
            S.Connection := 'db-conn';
          end);
      end)
    .When('each one is asked for',
      procedure(W: TOptionsWorld)
      begin
        W.Build;
        W.Told := W.Provider.GetRequiredService<IOptions<TCacheSettings>>.Value.Host + '|' +
                  W.Provider.GetRequiredService<IOptions<TDbSettings>>.Value.Connection;
      end)
    .&Then('each one gives back its own values',
      procedure(W: TOptionsWorld)
      begin
        // Es el control positivo del GUID compartido: todas las instanciaciones de la interfaz
        // comparten el GUID declarado, asi que "funciona" y "se confunden" solo se distinguen
        // mirando QUE trae cada una.
        Expect(W.Told).ToEqual('cache-host|db-conn');
      end)

.Rule('Settings nobody registered are not invented')

  .Scenario('What nobody registered is reported, not filled in')
    .Given('a composition where those settings are never registered',
      procedure(W: TOptionsWorld)
      begin
        AppOptions.Configure<TCacheSettings>(W.Services,
          procedure(S: TCacheSettings)
          begin
            S.Host := 'the-registered-one';
          end);
      end)
    .When('the unregistered settings are asked for',
      procedure(W: TOptionsWorld)
      begin
        W.Build;
        W.Attempt(
          procedure
          begin
            var Orphan := W.Provider.GetRequiredService<IOptions<TOrphanSettings>>;
            W.Told := 'gave back ' + Orphan.Value.ClassName;
          end);
      end)
    .&Then('the failure names the type that was asked for',
      procedure(W: TOptionsWorld)
      begin
        // Sobre el nombre, y no sobre que fallara: cualquier fallo pasaria lo segundo.
        Expect(W.Outcome).ToContain('TOrphanSettings');
      end)
    .&And('and no object with default values was handed back',
      procedure(W: TOptionsWorld)
      begin
        // La divergencia con .NET, afirmada: alli esto devuelve un objeto vacio en silencio.
        Expect(W.Told).ToEqual(SinRespuesta);
      end)

.Rule('Settings with object members free what is theirs')

  .Scenario('Nested settings do not leak')
    .Given('settings with an object member that frees what is its own',
      procedure(W: TOptionsWorld)
      begin
        AppOptions.Configure<TNestedSettings>(W.Services,
          procedure(S: TNestedSettings)
          begin
            S.Db.Server := 'localhost';
          end);
      end)
    .When('the value is used and the container is closed',
      procedure(W: TOptionsWorld)
      begin
        W.Build;
        W.ReadNestedServer;
        W.SectionBuilt := TDbSection.Built;
        W.Close;
        W.SectionAliveAfterClose := TDbSection.Alive;
      end)
    .&Then('the nested member had really been built',
      procedure(W: TOptionsWorld)
      begin
        // Sin este control, un anidado que nunca llegara a construirse dejaria el contador de
        // vivos en cero y el escenario diria que no hay fuga.
        Expect(W.SectionBuilt).ToEqual(1);
      end)
    .&And('and it was reachable through the settings',
      procedure(W: TOptionsWorld)
      begin
        Expect(W.Told).ToEqual('localhost');
      end)
    .&And('and none is left alive once the container closes',
      procedure(W: TOptionsWorld)
      begin
        Expect(W.SectionAliveAfterClose).ToEqual(0);
      end);

end.
