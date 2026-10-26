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
  System.SyncObjs,
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

  /// Una configuracion registrada directamente como contrato: anade su marca al Host.
  TMarkConfigure = class(TInterfacedObject, IConfigureOptions<TCacheSettings>)
  strict private
    FMark: string;
  public
    constructor Create(const AMark: string);
    procedure Configure(const Options: TCacheSettings);
  end;

  /// Un validador registrado directamente como contrato, que da un motivo por ajuste que rechaza.
  TCacheRulesValidator = class(TInterfacedObject, IValidateOptions<TCacheSettings>)
  public
    function Validate(const Name: string; const Options: TCacheSettings): TValidateOptionsResult;
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
    Report: string;
    Runs: Integer;
    FirstSaw: TCacheSettings;
    SecondSaw: TCacheSettings;
    // La cita de una configuracion: avisa de que esta dentro y espera a la puerta.
    InConfigure: TEvent;
    ConfigureGate: TEvent;
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
    /// Lo leido, cuantas configuraciones corrieron, si vieron el mismo objeto, y el fallo si lo hubo.
    function AccumulationReport: string;
    /// Una configuracion de TCacheSettings que cumple la cita, y una validacion que cuenta.
    procedure RegisterConfigureAtGate;
    /// Un hilo pide el valor y, con su configuracion dentro, otro hilo lo pide tambien; despues
    /// se abre la puerta. Deja en Report cuantas veces corrieron, si los dos recibieron el mismo
    /// objeto y si el segundo pidio con la puerta cerrada.
    procedure ReadFromTwoThreads;
    function ReadCacheSettings: TCacheSettings;
  end;

const
  SinFallo = '<sin fallo>';
  // Centinela con texto, y no la cadena vacia: la cadena vacia es tambien lo que se lee cuando
  // nadie escribio nada, asi que no distingue "no contesto" de "contesto vacio".
  SinRespuesta = '<sin respuesta>';

implementation

uses
  System.Classes,
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

{ TMarkConfigure }

constructor TMarkConfigure.Create(const AMark: string);
begin
  inherited Create;
  FMark := AMark;
end;

procedure TMarkConfigure.Configure(const Options: TCacheSettings);
begin
  Options.Host := Options.Host + FMark;
end;

{ TCacheRulesValidator }

function TCacheRulesValidator.Validate(const Name: string; const Options: TCacheSettings): TValidateOptionsResult;
begin
  var Failures: TArray<string> := [];
  if Options.Host = '' then
    Failures := Failures + ['Host must not be empty'];
  if Options.MaxItems <= 0 then
    Failures := Failures + ['MaxItems must be greater than zero'];
  if Length(Failures) = 0 then
    Result := TValidateOptionsResult.Success
  else
    Result := TValidateOptionsResult.Fail(Failures);
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
  InConfigure := TEvent.Create(nil, True, False, '');
  ConfigureGate := TEvent.Create(nil, True, False, '');
  Services := TServiceCollection.Create;
end;

destructor TOptionsWorld.Destroy;
begin
  if (Provider <> nil) and not Closed then
    Provider.ShutDown;
  InConfigure.Free;
  ConfigureGate.Free;
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

function TOptionsWorld.AccumulationReport: string;
begin
  Result := Format('%s, runs=%d, same object=%s',
    [Told, Runs, BoolToStr((FirstSaw <> nil) and (FirstSaw = SecondSaw), True)]);
  if Outcome <> SinFallo then
    Result := Result + ' | ' + Outcome;
end;

procedure TOptionsWorld.RegisterConfigureAtGate;
begin
  var Entered := InConfigure;
  var Gate := ConfigureGate;
  AppOptions.Configure<TCacheSettings>(Services,
    procedure(S: TCacheSettings)
    begin
      TInterlocked.Increment(Runs);
      Entered.SetEvent;
      Gate.WaitFor(10000);
      S.Host := 'shared-host';
      S.MaxItems := 1;
    end,
    function(const S: TCacheSettings; out Error: string): Boolean
    begin
      TInterlocked.Increment(Validations);
      Result := True;
    end);
end;

// Aparte para que el temporal de la opcion muera al volver, y no al final del hilo.
function TOptionsWorld.ReadCacheSettings: TCacheSettings;
begin
  Result := Provider.GetRequiredService<IOptions<TCacheSettings>>.Value;
end;

procedure TOptionsWorld.ReadFromTwoThreads;
var
  FirstDone, SecondDone: TEvent;
  AskedWhileConfiguring: Boolean;
begin
  AskedWhileConfiguring := False;
  FirstDone := TEvent.Create(nil, True, False, '');
  SecondDone := TEvent.Create(nil, True, False, '');
  try
    TThread.CreateAnonymousThread(
      procedure
      begin
        FirstSaw := ReadCacheSettings;
        FirstDone.SetEvent;
      end).Start;
    if InConfigure.WaitFor(10000) <> wrSignaled then
    begin
      ConfigureGate.SetEvent;
      FirstDone.WaitFor(10000);
      Report := 'the configuration never started';
      Exit;
    end;
    TThread.CreateAnonymousThread(
      procedure
      begin
        AskedWhileConfiguring := ConfigureGate.WaitFor(0) <> wrSignaled;
        SecondSaw := ReadCacheSettings;
        SecondDone.SetEvent;
      end).Start;
    // Quien espera al valor no termina antes del plazo; quien lo construye por su cuenta se para
    // en la misma puerta. Se abre en los dos casos.
    SecondDone.WaitFor(1500);
    ConfigureGate.SetEvent;
    FirstDone.WaitFor(10000);
    SecondDone.WaitFor(10000);
    Report := Format('configure=%d, validate=%d, same object=%s, asked while configuring=%s',
      [Runs, Validations, BoolToStr((FirstSaw <> nil) and (FirstSaw = SecondSaw), True),
       BoolToStr(AskedWhileConfiguring, True)]);
  finally
    SecondDone.Free;
    FirstDone.Free;
  end;
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
      end)

.Rule('The configurations of one settings type accumulate')

  .Scenario('Two configurations with code accumulate in order')
    .Given('two configurations of the same settings: the first sets Host and MaxItems, the second only MaxItems',
      procedure(W: TOptionsWorld)
      begin
        AppOptions.Configure<TCacheSettings>(W.Services,
          procedure(S: TCacheSettings)
          begin
            Inc(W.Runs);
            W.FirstSaw := S;
            S.Host := 'first-host';
            S.MaxItems := 1;
          end);
        AppOptions.Configure<TCacheSettings>(W.Services,
          procedure(S: TCacheSettings)
          begin
            Inc(W.Runs);
            W.SecondSaw := S;
            S.MaxItems := 2;
          end);
      end)
    .When('the value is asked for',
      procedure(W: TOptionsWorld)
      begin
        W.Build;
        W.Attempt(
          procedure
          begin
            var Value := W.Provider.GetRequiredService<IOptions<TCacheSettings>>.Value;
            W.Told := Format('host=%s, max=%d', [Value.Host, Value.MaxItems]);
          end);
      end)
    .&Then('Host has the value of the first and MaxItems that of the second, and each ran once on the same object',
      procedure(W: TOptionsWorld)
      begin
        Expect(W.AccumulationReport).ToEqual('host=first-host, max=2, runs=2, same object=True');
      end)

  .Scenario('A configuration registered as a contract is applied in its turn')
    .Given('a configuration with code, then one registered as a contract, then another with code, each marking Host',
      procedure(W: TOptionsWorld)
      begin
        AppOptions.Configure<TCacheSettings>(W.Services,
          procedure(S: TCacheSettings)
          begin
            S.Host := S.Host + 'a';
          end);
        W.Services.AddSingleton<IConfigureOptions<TCacheSettings>>(
          function(Provider: IServiceProvider): IConfigureOptions<TCacheSettings>
          begin
            Result := TMarkConfigure.Create('b');
          end);
        AppOptions.Configure<TCacheSettings>(W.Services,
          procedure(S: TCacheSettings)
          begin
            S.Host := S.Host + 'c';
          end);
      end)
    .When('the value is asked for',
      procedure(W: TOptionsWorld)
      begin
        W.Build;
        W.Attempt(
          procedure
          begin
            W.Told := W.Provider.GetRequiredService<IOptions<TCacheSettings>>.Value.Host;
          end);
      end)
    .&Then('Host shows the three marks in the order they were registered',
      procedure(W: TOptionsWorld)
      begin
        Expect(W.Told + ' ' + W.Outcome).ToEqual('abc ' + SinFallo);
      end)

.Rule('Every validation of a settings type runs, and the failure gathers them')

  .Scenario('A validator registered as a contract gives several reasons')
    .Given('a configuration with code, and a validator registered as a contract that rejects two settings',
      procedure(W: TOptionsWorld)
      begin
        AppOptions.Configure<TCacheSettings>(W.Services,
          procedure(S: TCacheSettings)
          begin
            S.Host := '';
            S.MaxItems := 0;
          end);
        W.Services.AddSingleton<IValidateOptions<TCacheSettings>>(
          function(Provider: IServiceProvider): IValidateOptions<TCacheSettings>
          begin
            Result := TCacheRulesValidator.Create;
          end);
      end)
    .When('the value is asked for',
      procedure(W: TOptionsWorld)
      begin
        W.Build;
        W.Attempt(
          procedure
          begin
            W.Told := W.Provider.GetRequiredService<IOptions<TCacheSettings>>.Value.Host;
          end);
      end)
    .&Then('it fails with a single validation error that gives both reasons',
      procedure(W: TOptionsWorld)
      begin
        Expect(W.Outcome).ToContain('EOptionsValidationError: ');
        Expect(W.Outcome).ToContain('Host must not be empty; MaxItems must be greater than zero');
      end)

  .Scenario('Two failing validations are reported together')
    .Given('two configurations of the same settings, each with a validation that rejects a different setting',
      procedure(W: TOptionsWorld)
      begin
        AppOptions.Configure<TCacheSettings>(W.Services,
          procedure(S: TCacheSettings)
          begin
            S.Host := '';
          end,
          function(const S: TCacheSettings; out Error: string): Boolean
          begin
            Result := S.Host <> '';
            if not Result then
              Error := 'Host must not be empty';
          end);
        AppOptions.Configure<TCacheSettings>(W.Services,
          procedure(S: TCacheSettings)
          begin
            S.MaxItems := 0;
          end,
          function(const S: TCacheSettings; out Error: string): Boolean
          begin
            Result := S.MaxItems > 0;
            if not Result then
              Error := 'MaxItems must be greater than zero';
          end);
      end)
    .When('the value is asked for',
      procedure(W: TOptionsWorld)
      begin
        W.Build;
        W.Attempt(
          procedure
          begin
            W.Told := W.Provider.GetRequiredService<IOptions<TCacheSettings>>.Value.Host;
          end);
      end)
    .&Then('it fails with a single validation error that names both settings',
      procedure(W: TOptionsWorld)
      begin
        Expect(W.Outcome).ToContain('EOptionsValidationError: ');
        Expect(W.Outcome).ToContain('Host must not be empty');
        Expect(W.Outcome).ToContain('MaxItems must be greater than zero');
      end)

  .Scenario('A validation sees every configuration applied')
    .Given('a configuration whose validation needs a setting that only a later configuration sets',
      procedure(W: TOptionsWorld)
      begin
        AppOptions.Configure<TCacheSettings>(W.Services,
          procedure(S: TCacheSettings)
          begin
            S.Host := 'host';
          end,
          function(const S: TCacheSettings; out Error: string): Boolean
          begin
            Inc(W.Validations);
            Result := S.MaxItems > 0;
            if not Result then
              Error := 'MaxItems must be greater than zero';
          end);
        AppOptions.Configure<TCacheSettings>(W.Services,
          procedure(S: TCacheSettings)
          begin
            S.MaxItems := 5;
          end);
      end)
    .When('the value is asked for',
      procedure(W: TOptionsWorld)
      begin
        W.Build;
        W.Attempt(
          procedure
          begin
            W.Told := IntToStr(W.Provider.GetRequiredService<IOptions<TCacheSettings>>.Value.MaxItems);
          end);
      end)
    .&Then('the validation ran once and passed, and the value is handed back',
      procedure(W: TOptionsWorld)
      begin
        Expect(Format('validations=%d, value=%s, %s', [W.Validations, W.Told, W.Outcome]))
          .ToEqual('validations=1, value=5, ' + SinFallo);
      end)

.Rule('Reading the value from several threads at once builds it once')

  .Scenario('Two first reads at the same time')
    .Given('settings whose configuration is under way in a thread that asked for the value',
      procedure(W: TOptionsWorld)
      begin
        W.RegisterConfigureAtGate;
        W.Build;
      end)
    .When('another thread asks for the value',
      procedure(W: TOptionsWorld)
      begin
        W.ReadFromTwoThreads;
      end)
    .&Then('configuration and validation ran once, both threads got the same object, and the second asked meanwhile',
      procedure(W: TOptionsWorld)
      begin
        Expect(W.Report).ToEqual('configure=1, validate=1, same object=True, asked while configuring=True');
      end);

end.
