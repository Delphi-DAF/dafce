unit OptionsBinding.Feat;

{
  La costura entre la configuracion y el patron de opciones.

  Lo que se afirma aqui no es que el enlazador funcione --eso ya tiene sus specs-- sino que el
  servicio deja de nombrar su seccion y deja de heredar el deber de liberar nada. Por eso los
  escenarios miran QUE recibe el servicio y CUANTOS objetos quedan vivos, y no si el enlace
  convierte bien un entero.

  Y hay un escenario que afirma lo contrario de lo que espera quien viene de .NET: el valor es una
  FOTO. Cambiar la configuracion despues de haberlo leido no lo cambia. Eso no se puede dejar sin
  decir, asi que va escrito con su control de que la configuracion cambio de verdad.
}

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Daf.Extensions.Configuration,
  Daf.Extensions.DependencyInjection,
  Daf.Extensions.Options;

type
  {$M+}
  /// <summary>
  ///   Unos ajustes corrientes. El contador de vivos es el instrumento de medida del spec: sin
  ///   el, "no se fuga" no se distingue de "no se construyo".
  /// </summary>
  TAppSettings = class
  private
    FTitle: string;
    FPort: Integer;
  public
    class var Built: Integer;
    class var Alive: Integer;
    constructor Create;
    destructor Destroy; override;
    property Title: string read FTitle write FTitle;
    property Port: Integer read FPort write FPort;
  end;
  {$M-}

  /// <summary>
  ///   El servicio. En ninguna parte de su codigo aparece el nombre de la seccion de la que salen
  ///   sus valores: la decide quien compone. Y copia lo que necesita en el constructor, porque el
  ///   objeto que Value devuelve es de la opcion.
  /// </summary>
  IGreeter = interface(IInvokable)
    ['{9C36F5B0-6D1E-4F58-9C43-1A2B3C4D5E60}']
    function Describe: string;
  end;

  TGreeter = class(TInterfacedObject, IGreeter)
  strict private
    FTitle: string;
    FPort: Integer;
  public
    constructor Create(const Options: IOptions<TAppSettings>);
    function Describe: string;
  end;

  TBindingWorld = class
  public
    Data: TDictionary<string, string>;
    Config: IConfigurationRoot;
    Services: IServiceCollection;
    Provider: IServiceProvider;
    Told: string;
    ConfigSaysAfter: string;
    AliveWhileHeld: Integer;
    AliveAfterFree: Integer;
    Built: Integer;
    Closed: Boolean;
    ConfigBuilt: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Configure(const Key, Value: string);
    procedure BuildConfig;
    procedure Build;
    procedure Close;
    /// Lee el titulo y SUELTA la opcion antes de volver. En un metodo propio porque Delphi
    /// finaliza las referencias de interfaz temporales al salir del METODO, no al acabar la
    /// sentencia: leido dentro del escenario, un temporal invisible seguiria sujetando la opcion
    /// y el cierre no podria liberar nada.
    function ReadTitle: string;
  end;

implementation

uses
  Daf.MiniSpec,
  Daf.Configuration.Builder,
  Daf.Configuration.Memory,
  Daf.Configuration.Binder,
  Daf.Options.Configuration,
  Daf.DependencyInjection;

{ TAppSettings }

constructor TAppSettings.Create;
begin
  inherited Create;
  Inc(Built);
  Inc(Alive);
end;

destructor TAppSettings.Destroy;
begin
  Dec(Alive);
  inherited;
end;

{ TGreeter }

constructor TGreeter.Create(const Options: IOptions<TAppSettings>);
begin
  inherited Create;
  FTitle := Options.Value.Title;
  FPort := Options.Value.Port;
end;

function TGreeter.Describe: string;
begin
  Result := Format('%s/%d', [FTitle, FPort]);
end;

{ TBindingWorld }

constructor TBindingWorld.Create;
begin
  inherited;
  TAppSettings.Built := 0;
  TAppSettings.Alive := 0;
  Told := '<sin respuesta>';
  ConfigSaysAfter := '<sin leer>';
  Data := TDictionary<string, string>.Create;
  Services := TServiceCollection.Create;
end;

destructor TBindingWorld.Destroy;
begin
  if (Provider <> nil) and not Closed then
    Provider.ShutDown;
  Config := nil;
  // El diccionario pasa a ser del origen en memoria en cuanto se compone la configuracion --
  // TMemoryConfigurationSource lo libera-- asi que solo hay que liberarlo aqui si nunca llego
  // a componerse.
  if not ConfigBuilt then
    Data.Free;
  inherited;
end;

procedure TBindingWorld.Configure(const Key, Value: string);
begin
  Data.AddOrSetValue(Key, Value);
end;

procedure TBindingWorld.BuildConfig;
begin
  if ConfigBuilt then Exit;
  var Builder: IConfigurationBuilder := TConfigurationBuilder.Create;
  MemoryConfig.AddCollection(Builder, Data);
  Config := Builder.Build;
  ConfigBuilt := True;
end;

procedure TBindingWorld.Build;
begin
  BuildConfig;
  if Provider = nil then
    Provider := Services.BuildServiceProvider;
end;

procedure TBindingWorld.Close;
begin
  if Closed then Exit;
  Provider.ShutDown;
  Closed := True;
end;

function TBindingWorld.ReadTitle: string;
begin
  Result := Provider.GetRequiredService<IOptions<TAppSettings>>.Value.Title;
end;

// --- Feature definition ---

initialization

Feature('''
Feature A configuration section arrives as settings with an owner @configuration @options

  As an application author
  I want a service to receive a configuration section as a typed object it does not own
  So that composing decides where the values come from and the service never names a section
''')

.UseWorld<TBindingWorld>

.Rule('A configuration section is handed over as settings with an owner')

  .Scenario('The service does not name its section')
    .Given('a configuration with a section, bound to settings at composition time',
      procedure(W: TBindingWorld)
      begin
        W.Configure('App:Title', 'from-app');
        W.Configure('App:Port', '8080');
        W.BuildConfig;
        ConfigOptions.Bind<TAppSettings>(W.Services, W.Config, 'App');
        W.Services.AddSingleton<IGreeter, TGreeter>;
      end)
    .When('the service is resolved',
      procedure(W: TBindingWorld)
      begin
        W.Build;
        W.Told := W.Provider.GetRequiredService<IGreeter>.Describe;
      end)
    .&Then('it got the values of the section it never names',
      procedure(W: TBindingWorld)
      begin
        Expect(W.Told).ToEqual('from-app/8080');
      end)

  .Scenario('Changing where the values come from does not touch the service')
    .Given('the very same service, bound to a different section',
      procedure(W: TBindingWorld)
      begin
        // Mismos ajustes, mismo servicio, misma clase: lo unico que cambia entre este escenario
        // y el anterior es la ruta que se le pasa a la fachada.
        W.Configure('App:Title', 'from-app');
        W.Configure('App:Port', '8080');
        W.Configure('Other:Title', 'from-other');
        W.Configure('Other:Port', '9090');
        W.BuildConfig;
        ConfigOptions.Bind<TAppSettings>(W.Services, W.Config, 'Other');
        W.Services.AddSingleton<IGreeter, TGreeter>;
      end)
    .When('the service is resolved',
      procedure(W: TBindingWorld)
      begin
        W.Build;
        W.Told := W.Provider.GetRequiredService<IGreeter>.Describe;
      end)
    .&Then('it now carries the other section, with no change of its own',
      procedure(W: TBindingWorld)
      begin
        Expect(W.Told).ToEqual('from-other/9090');
      end)

  .Scenario('The consumer frees nothing')
    .Given('settings bound from a section, and a service that consumes them',
      procedure(W: TBindingWorld)
      begin
        W.Configure('App:Title', 'owned-by-the-container');
        W.Configure('App:Port', '1');
        W.BuildConfig;
        ConfigOptions.Bind<TAppSettings>(W.Services, W.Config, 'App');
        W.Services.AddSingleton<IGreeter, TGreeter>;
      end)
    .When('the service is used and the container is closed',
      procedure(W: TBindingWorld)
      begin
        W.Build;
        W.Told := W.Provider.GetRequiredService<IGreeter>.Describe;
        W.Built := TAppSettings.Built;
        W.Close;
        W.AliveAfterFree := TAppSettings.Alive;
      end)
    .&Then('the settings object had really been built',
      procedure(W: TBindingWorld)
      begin
        // Control del contador de vivos: sin el, "no queda ninguno" y "no se construyo ninguno"
        // son la misma cifra.
        Expect(W.Built).ToEqual(1);
      end)
    .&And('and the service saw its values',
      procedure(W: TBindingWorld)
      begin
        Expect(W.Told).ToEqual('owned-by-the-container/1');
      end)
    .&And('and none is left alive once the container closes, with nobody having freed it',
      procedure(W: TBindingWorld)
      begin
        Expect(W.AliveAfterFree).ToEqual(0);
      end)

.Rule('The configuration is read once, when the settings are built')

  .Scenario('A photograph, not a mirror')
    .Given('settings already read from a section',
      procedure(W: TBindingWorld)
      begin
        W.Configure('App:Title', 'as-composed');
        W.BuildConfig;
        ConfigOptions.Bind<TAppSettings>(W.Services, W.Config, 'App');
      end)
    .When('the configuration is changed after the value was read',
      procedure(W: TBindingWorld)
      begin
        W.Build;
        var Before := W.ReadTitle;
        W.Config['App:Title'] := 'changed-afterwards';
        W.ConfigSaysAfter := W.Config['App:Title'];
        W.Told := Before + '|' + W.ReadTitle;
      end)
    .&Then('the configuration really did change, which is what makes the next line mean something',
      procedure(W: TBindingWorld)
      begin
        // Sin este control, un cambio que no llegase a la configuracion daria el mismo verde que
        // un valor que no la sigue -- y el escenario diria lo que quiere oir.
        Expect(W.ConfigSaysAfter).ToEqual('changed-afterwards');
      end)
    .&And('and the value still shows what was there when it was built',
      procedure(W: TBindingWorld)
      begin
        Expect(W.Told).ToEqual('as-composed|as-composed');
      end)

.Rule('Binding by hand is still available, unchanged')

  .Scenario('What already compiled still compiles, and the object is still the callers')
    .Given('a configuration with a section',
      procedure(W: TBindingWorld)
      begin
        W.Configure('App:Title', 'bound-by-hand');
        W.Configure('App:Port', '5432');
        W.BuildConfig;
      end)
    .When('a consumer binds it to an object of its own, as it used to',
      procedure(W: TBindingWorld)
      begin
        var Settings := TConfigurationBinder.Bind<TAppSettings>(W.Config, 'App');
        try
          W.Told := Format('%s/%d', [Settings.Title, Settings.Port]);
          W.AliveWhileHeld := TAppSettings.Alive;
        finally
          // Y sigue teniendo que liberarlo: eso es lo que significa que el objeto sea suyo, y es
          // justo la via que este trabajo NO deprecia.
          Settings.Free;
        end;
        W.AliveAfterFree := TAppSettings.Alive;
      end)
    .&Then('it got the same object, filled the same way',
      procedure(W: TBindingWorld)
      begin
        Expect(W.Told).ToEqual('bound-by-hand/5432');
      end)
    .&And('and it was the callers while it held it',
      procedure(W: TBindingWorld)
      begin
        Expect(W.AliveWhileHeld).ToEqual(1);
      end)
    .&And('and freeing it was the callers to do, and nobody else had done it',
      procedure(W: TBindingWorld)
      begin
        Expect(W.AliveAfterFree).ToEqual(0);
      end);

end.
