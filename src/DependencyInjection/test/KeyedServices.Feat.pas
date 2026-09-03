unit KeyedServices.Feat;

{
  Varias implementaciones del mismo contrato distinguidas por una CLAVE, con la superficie de
  Microsoft.Extensions.DependencyInjection.

  Dos modos de fallo de aqui se leen como acierto, y por eso casi ninguna afirmacion es un
  recuento -- se afirma sobre QUE implementacion trae cada resolucion:

    - el cruce entre los dos espacios: pedir sin clave y recibir un registro con clave, o al
      reves. Hay objeto, responde, y es el de otro contrato
    - una clave que no empareja con la suya: se lee como "ese servicio no esta registrado", que
      manda a buscar en la composicion en vez de en la clave
}

interface

uses
  System.SysUtils,
  System.Rtti,
  System.TypInfo,
  Daf.Rtti,
  Daf.Extensions.DependencyInjection,
  KeyedServices.Marks;

type
  /// Servicio de juguete: lo unico que se le pregunta es su nombre, que es lo que distingue una
  /// implementacion de otra -- y por tanto "gano la clave que pedi" de "gano otra".
  IStorage = interface(IInvokable)
    ['{6E4A0B71-9C82-4D35-8F16-3A7B5C2D9E04}']
    function Name: string;
  end;

  /// Un segundo contrato, para que el aislamiento tenga algo con lo que confundirse.
  IClock = interface(IInvokable)
    ['{1F5C8D26-4B70-4E93-A2D8-7C0E6B1A5493}']
    function Name: string;
  end;

  TNamedStorage = class(TInterfacedObject, IStorage)
  strict private
    FName: string;
  public
    constructor Create(const AName: string);
    function Name: string;
  end;

  TBlobStorage = class(TInterfacedObject, IStorage)
  public
    function Name: string;
  end;

  TFileStorage = class(TInterfacedObject, IStorage)
  public
    function Name: string;
  end;

  TPlainStorage = class(TInterfacedObject, IStorage)
  public
    function Name: string;
  end;

  TClock = class(TInterfacedObject, IClock)
  public
    function Name: string;
  end;

  /// Decorador escrito a mano: envuelve otro IStorage y le anade una marca.
  TWrappedStorage = class(TInterfacedObject, IStorage)
  strict private
    FInner: IStorage;
  public
    constructor Create(const Inner: IStorage);
    function Name: string;
  end;

  /// Cuenta sus construcciones: es lo que separa "la misma instancia" de "otra igual".
  TCountedStorage = class(TInterfacedObject, IStorage)
  public
    class var Built: Integer;
    constructor Create;
    function Name: string;
  end;

  // --- Dos enumerados DISTINTOS que comparten ordinal ---------------------------------------
  TTransport = (trSms, trEmail);
  TPriority = (prLow, prHigh);
  // --- Inyeccion por constructor -------------------------------------------------------------
  IUsesFile = interface(IInvokable)
    ['{2D9E7A40-5C13-4F86-B0A7-9E4C1D6B3852}']
    function StorageName: string;
  end;

  TUsesFile = class(TInterfacedObject, IUsesFile)
  strict private
    FStorage: IStorage;
  public
    constructor Create([DafInject('file')] const Storage: IStorage);
    function StorageName: string;
  end;

  IUsesPlain = interface(IInvokable)
    ['{7B0C4E15-3A69-4D72-8E51-6F2A9C0B4D37}']
    function StorageName: string;
  end;

  TUsesPlain = class(TInterfacedObject, IUsesPlain)
  strict private
    FStorage: IStorage;
  public
    constructor Create(const Storage: IStorage);
    function StorageName: string;
  end;

  IUsesUnknownKey = interface(IInvokable)
    ['{4C1D8B93-0E27-4A56-9F3B-8D5A2E7C6014}']
    function StorageName: string;
  end;

  TUsesUnknownKey = class(TInterfacedObject, IUsesUnknownKey)
  strict private
    FStorage: IStorage;
  public
    constructor Create([DafInject('nobody')] const Storage: IStorage);
    function StorageName: string;
  end;

  /// Dos constructores: uno pide una clave que nadie registra y otro no pide nada. El activador
  /// tiene que descartar el primero y elegir el segundo, en vez de elegirlo y fallar despues.
  IPicksCtor = interface(IInvokable)
    ['{8A3F6D02-1B54-4C89-A7E0-2D9B5F1C4736}']
    function Chosen: string;
  end;

  TPicksCtor = class(TInterfacedObject, IPicksCtor)
  strict private
    FChosen: string;
  public
    constructor Create([DafInject('nobody')] const Storage: IStorage); overload;
    constructor Create(const Clock: IClock); overload;
    function Chosen: string;
  end;

  // --- Inyeccion por propiedad ---------------------------------------------------------------
  IPropByKey = interface(IInvokable)
    ['{0B6E2C58-7D41-4930-85AF-1C3D8B7A6E29}']
    function StorageName: string;
  end;

  [DafInjectProperties]
  TPropByKey = class(TInterfacedObject, IPropByKey)
  strict private
    FStorage: IStorage;
  public
    [DafInject('file')]
    property Storage: IStorage read FStorage write FStorage;
    function StorageName: string;
  end;

  IPropNoKey = interface(IInvokable)
    ['{5E1A9C74-2F86-4B03-9D5C-7A0B4E6D8153}']
    function StorageName: string;
  end;

  [DafInjectProperties]
  TPropNoKey = class(TInterfacedObject, IPropNoKey)
  strict private
    FStorage: IStorage;
  public
    [DafInject]
    property Storage: IStorage read FStorage write FStorage;
    function StorageName: string;
  end;

  IPropUnknownKey = interface(IInvokable)
    ['{9C7B3E10-6A25-4D48-B1F9-0E5C2A8D7346}']
    function StorageName: string;
  end;

  [DafInjectProperties]
  TPropUnknownKey = class(TInterfacedObject, IPropUnknownKey)
  strict private
    FStorage: IStorage;
  public
    [DafInject('nobody')]
    property Storage: IStorage read FStorage write FStorage;
    function StorageName: string;
  end;

  /// La clave por el marcador propio de la aplicacion, tipado a su enumerado.
  IPropByOwnMark = interface(IInvokable)
    ['{3A8D5F62-4C09-4E71-8B26-9D1F7C0A5E48}']
    function StorageName: string;
  end;

  [DafInjectProperties]
  TPropByOwnMark = class(TInterfacedObject, IPropByOwnMark)
  strict private
    FStorage: IStorage;
  public
    [StorageInject(skFile)]
    property Storage: IStorage read FStorage write FStorage;
    function StorageName: string;
  end;

  /// El ordinal escrito a mano, fuera del rango de su tipo.
  IPropBadOrdinal = interface(IInvokable)
    ['{6D2C0A97-8B53-4F14-A9E7-5C3B1D8F2069}']
    function StorageName: string;
  end;

  [DafInjectProperties]
  TPropBadOrdinal = class(TInterfacedObject, IPropBadOrdinal)
  strict private
    FStorage: IStorage;
  public
    [DafInject(TypeInfo(TStorageKind), 7)]
    property Storage: IStorage read FStorage write FStorage;
    function StorageName: string;
  end;

  /// Una instancia que el contenedor NO construye: la cablea su dueno con InjectInto.
  [DafInjectProperties]
  TGuest = class
  strict private
    FStorage: IStorage;
  public
    [DafInject('file')]
    property Storage: IStorage read FStorage write FStorage;
    function StorageName: string;
  end;

  // --- Validacion ----------------------------------------------------------------------------
  /// Nadie lo registra nunca.
  IMissingDep = interface(IInvokable)
    ['{7F4B1E38-0D69-4A52-B8C3-2E6A9D5F0417}']
    function Name: string;
  end;

  IKeyedHolder = interface(IInvokable)
    ['{2C9A6B45-5E18-4073-91DF-8B0C4A7E3D62}']
    function Name: string;
  end;

  TKeyedHolder = class(TInterfacedObject, IKeyedHolder)
  public
    constructor Create(const Dep: IMissingDep);
    function Name: string;
  end;

  IScopedDep = interface(IInvokable)
    ['{4E7D2A61-9B03-4C58-86F1-3A5C0E8B7924}']
    function Name: string;
  end;

  TScopedDep = class(TInterfacedObject, IScopedDep)
  public
    function Name: string;
  end;

  IKeyedCaptor = interface(IInvokable)
    ['{8B5F0C29-1A74-4D36-B0E8-6C2D9A4F1573}']
    function Name: string;
  end;

  TKeyedCaptor = class(TInterfacedObject, IKeyedCaptor)
  public
    constructor Create(const Dep: IScopedDep);
    function Name: string;
  end;

  IHealthy = interface(IInvokable)
    ['{1D6C8A03-7E45-4B92-A3F0-5B9E2C7D6148}']
    function Name: string;
  end;

  THealthy = class(TInterfacedObject, IHealthy)
  public
    constructor Create([DafInject('blob')] const Storage: IStorage);
    function Name: string;
  end;

  // --- Genericos sin fijar -------------------------------------------------------------------
  TPayload = class
  end;

  TAlphaPayload = class(TPayload)
  end;

  IBox<T: class> = interface(IInvokable)
    ['{5C0B9E74-3D12-4A86-97F5-0E8A6B2C4D31}']
    function Tag: string;
  end;

  TBoxArgs = record
    T1: TClass;
  end;

  TBox<T: class> = class(TInterfacedObject, IBox<T>)
  private
    _ClosedArgs: TBoxArgs;
  public
    function Tag: string;
  end;

  TKeyedWorld = class
  public
    Services: IServiceCollection;
    Provider: IServiceProvider;
    Guest: TGuest;
    Report: string;
    Outcome: string;
    Failure: string;
    Names: TArray<string>;
    First: IStorage;
    Second: IStorage;
    constructor Create;
    destructor Destroy; override;
    procedure Build;
    procedure BuildWith(const Options: TServiceProviderOptions);
    procedure Attempt(const Action: TProc);
    function NameOfKeyed(const Key: TValue): string;
  end;

const
  // Centinela con texto: la cadena vacia es tambien lo que se lee cuando nadie escribio nada.
  NoFailure = '<sin error>';
  NotFound = '<no encontrado>';

implementation

uses
  Daf.MiniSpec,
  Daf.DependencyInjection,
  Daf.DependencyInjection.ActivatorUtilities;

{ TNamedStorage }

constructor TNamedStorage.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

function TNamedStorage.Name: string;
begin
  Result := FName;
end;

{ Los demas dobles }

function TBlobStorage.Name: string;
begin
  Result := 'blob';
end;

function TFileStorage.Name: string;
begin
  Result := 'file';
end;

function TPlainStorage.Name: string;
begin
  Result := 'plain';
end;

function TClock.Name: string;
begin
  Result := 'clock';
end;

constructor TWrappedStorage.Create(const Inner: IStorage);
begin
  inherited Create;
  FInner := Inner;
end;

function TWrappedStorage.Name: string;
begin
  Result := 'wrapped(' + FInner.Name + ')';
end;

constructor TCountedStorage.Create;
begin
  inherited Create;
  Inc(Built);
end;

function TCountedStorage.Name: string;
begin
  Result := 'counted';
end;

{ TUsesFile }

constructor TUsesFile.Create(const Storage: IStorage);
begin
  inherited Create;
  FStorage := Storage;
end;

function TUsesFile.StorageName: string;
begin
  if FStorage = nil then Exit('<nil>');
  Result := FStorage.Name;
end;

{ TUsesPlain }

constructor TUsesPlain.Create(const Storage: IStorage);
begin
  inherited Create;
  FStorage := Storage;
end;

function TUsesPlain.StorageName: string;
begin
  if FStorage = nil then Exit('<nil>');
  Result := FStorage.Name;
end;

{ TUsesUnknownKey }

constructor TUsesUnknownKey.Create(const Storage: IStorage);
begin
  inherited Create;
  FStorage := Storage;
end;

function TUsesUnknownKey.StorageName: string;
begin
  if FStorage = nil then Exit('<nil>');
  Result := FStorage.Name;
end;

{ TPicksCtor }

constructor TPicksCtor.Create(const Storage: IStorage);
begin
  inherited Create;
  FChosen := 'storage';
end;

constructor TPicksCtor.Create(const Clock: IClock);
begin
  inherited Create;
  FChosen := 'clock';
end;

function TPicksCtor.Chosen: string;
begin
  Result := FChosen;
end;

{ Las clases con propiedad marcada }

function TPropByKey.StorageName: string;
begin
  if FStorage = nil then Exit('<nil>');
  Result := FStorage.Name;
end;

function TPropNoKey.StorageName: string;
begin
  if FStorage = nil then Exit('<nil>');
  Result := FStorage.Name;
end;

function TPropUnknownKey.StorageName: string;
begin
  if FStorage = nil then Exit('<nil>');
  Result := FStorage.Name;
end;

function TPropByOwnMark.StorageName: string;
begin
  if FStorage = nil then Exit('<nil>');
  Result := FStorage.Name;
end;

function TPropBadOrdinal.StorageName: string;
begin
  if FStorage = nil then Exit('<nil>');
  Result := FStorage.Name;
end;

function TGuest.StorageName: string;
begin
  if FStorage = nil then Exit('<nil>');
  Result := FStorage.Name;
end;

{ Los dobles de validacion }

constructor TKeyedHolder.Create(const Dep: IMissingDep);
begin
  inherited Create;
end;

function TKeyedHolder.Name: string;
begin
  Result := 'holder';
end;

function TScopedDep.Name: string;
begin
  Result := 'scoped';
end;

constructor TKeyedCaptor.Create(const Dep: IScopedDep);
begin
  inherited Create;
end;

function TKeyedCaptor.Name: string;
begin
  Result := 'captor';
end;

constructor THealthy.Create(const Storage: IStorage);
begin
  inherited Create;
end;

function THealthy.Name: string;
begin
  Result := 'healthy';
end;

{ TBox<T> }

function TBox<T>.Tag: string;
begin
  Result := _ClosedArgs.T1.ClassName;
end;

{ TKeyedWorld }

constructor TKeyedWorld.Create;
begin
  inherited;
  TCountedStorage.Built := 0;
  Outcome := '<sin intentar>';
  Failure := '';
  Services := TServiceCollection.Create;
end;

destructor TKeyedWorld.Destroy;
begin
  First := nil;
  Second := nil;
  Guest.Free;
  if Provider <> nil then
    Provider.ShutDown;
  inherited;
end;

procedure TKeyedWorld.Build;
begin
  if Provider = nil then
    Provider := Services.BuildServiceProvider;
end;

procedure TKeyedWorld.BuildWith(const Options: TServiceProviderOptions);
begin
  Failure := NoFailure;
  try
    Provider := Services.BuildServiceProvider(Options);
  except
    on E: Exception do Failure := E.Message;
  end;
end;

procedure TKeyedWorld.Attempt(const Action: TProc);
begin
  Outcome := NoFailure;
  try
    Action();
  except
    on E: Exception do Outcome := E.ClassName + ': ' + E.Message;
  end;
end;

function TKeyedWorld.NameOfKeyed(const Key: TValue): string;
begin
  // Un centinela con texto, y no la cadena vacia: "no encontrado" y "lo encontro y se llama ''"
  // se leen igual, que es justo el fallo que estos escenarios miran.
  var Svc := Provider.GetKeyedService<IStorage>(Key);
  if Svc = nil then Exit(NotFound);
  Result := Svc.Name;
end;

initialization

Feature('''
Feature Keyed services @di @keyed

  As an application author
  I want two implementations of the same contract told apart by a key
  So I stop splitting a contract in two just to give each one a name, or picking by `is` at the
  call site
''')

.UseWorld<TKeyedWorld>

.Rule('A service can be registered and resolved with a key')

  .Scenario('Two implementations of the same contract, told apart by their key')
    .Given('IStorage registered as singleton with the keys blob and file',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddKeyedSingleton<IStorage, TBlobStorage>('blob');
        W.Services.AddKeyedSingleton<IStorage, TFileStorage>('file');
      end)
    .When('each one is resolved by its key',
      procedure(W: TKeyedWorld)
      begin
        W.Build;
        W.First := W.Provider.GetRequiredKeyedService<IStorage>('blob');
        W.Second := W.Provider.GetRequiredKeyedService<IStorage>('file');
      end)
    .&Then('each one brings its own implementation',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.First.Name + '|' + W.Second.Name).ToEqual('blob|file');
      end)
    .&And('they are two different instances',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.First = W.Second).ToBeFalse;
      end)

  .Scenario('A key nobody registered')
    .Given('IStorage registered only with the key blob',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddKeyedSingleton<IStorage, TBlobStorage>('blob');
      end)
    .When('a service is asked for with a key that is not registered',
      procedure(W: TKeyedWorld)
      begin
        W.Build;
        W.Report := W.NameOfKeyed('nobody');
        W.Attempt(procedure begin W.Provider.GetRequiredKeyedService<IStorage>('nobody') end);
      end)
    .&Then('GetKeyedService gives back nil',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Report).ToEqual(NotFound);
      end)
    .&And('GetRequiredKeyedService fails naming the service and the key',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Outcome).ToContain('IStorage');
        Expect(W.Outcome).ToContain('nobody');
      end)

  .Scenario('The lifetime belongs to the registration, not to the type')
    .Given('IStorage as singleton with the key blob and as transient with the key file',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddKeyedSingleton<IStorage>('blob',
          function(P: IServiceProvider): IStorage begin Result := TCountedStorage.Create end);
        W.Services.AddKeyedTransient<IStorage>('file',
          function(P: IServiceProvider): IStorage begin Result := TCountedStorage.Create end);
      end)
    .When('each one is resolved twice',
      procedure(W: TKeyedWorld)
      begin
        W.Build;
        var A1 := W.Provider.GetRequiredKeyedService<IStorage>('blob');
        var A2 := W.Provider.GetRequiredKeyedService<IStorage>('blob');
        var B1 := W.Provider.GetRequiredKeyedService<IStorage>('file');
        var B2 := W.Provider.GetRequiredKeyedService<IStorage>('file');
        W.Report := Format('blob=%s file=%s built=%d',
          [BoolToStr(A1 = A2, True), BoolToStr(B1 = B2, True), TCountedStorage.Built]);
      end)
    .&Then('blob gives the same instance twice and file a different one each time',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Report).ToEqual('blob=True file=False built=3');
      end)

  .Scenario('All the ones under a key')
    .Given('two registrations of IStorage under the same key blob',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddKeyedSingleton<IStorage, TBlobStorage>('blob');
        W.Services.AddKeyedSingleton<IStorage, TFileStorage>('blob');
      end)
    .When('every service under that key is asked for',
      procedure(W: TKeyedWorld)
      begin
        W.Build;
        W.Report := '';
        for var Svc in W.Provider.GetKeyedServices<IStorage>('blob') do
          W.Report := W.Report + Svc.Name + ';';
      end)
    .&Then('both come back',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Report).ToEqual('blob;file;');
      end)

.Rule('A key is a string, an ordinal, an enumerated value or a type')

  .Scenario('A string, an integer, a boolean and an enumerated value live together as keys')
    .Given('registrations of the same service type under four keys of four kinds',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddKeyedSingleton<IStorage>('sms',
          function(P: IServiceProvider): IStorage begin Result := TNamedStorage.Create('by-string') end);
        W.Services.AddKeyedSingleton<IStorage>(42,
          function(P: IServiceProvider): IStorage begin Result := TNamedStorage.Create('by-integer') end);
        W.Services.AddKeyedSingleton<IStorage>(TValue.From<Boolean>(True),
          function(P: IServiceProvider): IStorage begin Result := TNamedStorage.Create('by-boolean') end);
        W.Services.AddKeyedSingleton<IStorage>(TValue.From<TTransport>(trSms),
          function(P: IServiceProvider): IStorage begin Result := TNamedStorage.Create('by-enum') end);
      end)
    .When('each one is resolved by its key',
      procedure(W: TKeyedWorld)
      begin
        W.Build;
        W.Report := W.NameOfKeyed('sms') + '|' + W.NameOfKeyed(42) + '|' +
          W.NameOfKeyed(TValue.From<Boolean>(True)) + '|' + W.NameOfKeyed(TValue.From<TTransport>(trSms));
      end)
    .&Then('each one brings its own',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Report).ToEqual('by-string|by-integer|by-boolean|by-enum');
      end)

  .Scenario('A boolean is not the integer that matches it')
    .Given('one registration under the key True and another under the integer key 1',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddKeyedSingleton<IStorage>(TValue.From<Boolean>(True),
          function(P: IServiceProvider): IStorage begin Result := TNamedStorage.Create('boolean') end);
        W.Services.AddKeyedSingleton<IStorage>(1,
          function(P: IServiceProvider): IStorage begin Result := TNamedStorage.Create('integer') end);
      end)
    .When('each one is resolved',
      procedure(W: TKeyedWorld)
      begin
        W.Build;
        W.Report := W.NameOfKeyed(TValue.From<Boolean>(True)) + '|' + W.NameOfKeyed(1);
      end)
    .&Then('each one brings its own, because Boolean is a type of its own',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Report).ToEqual('boolean|integer');
      end)

  .Scenario('A character as a key is rejected')
    .Given('nothing registered',
      procedure(W: TKeyedWorld)
      begin
      end)
    .When('a service is registered with a key holding a character',
      procedure(W: TKeyedWorld)
      begin
        W.Attempt(procedure
          begin
            W.Services.AddKeyedSingleton<IStorage, TBlobStorage>(TValue.From<Char>('a'));
          end);
      end)
    .&Then('it fails saying a character cannot be a key, and that a string is what that is for',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Outcome).ToContain('character');
        Expect(W.Outcome).ToContain('string');
      end)

  .Scenario('Two different enumerated types sharing an ordinal are not confused')
    .Given('two registrations, one under the first value of an enumerated type and one under the first value of another',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddKeyedSingleton<IStorage>(TValue.From<TTransport>(trSms),
          function(P: IServiceProvider): IStorage begin Result := TNamedStorage.Create('transport') end);
        W.Services.AddKeyedSingleton<IStorage>(TValue.From<TPriority>(prLow),
          function(P: IServiceProvider): IStorage begin Result := TNamedStorage.Create('priority') end);
      end)
    .When('each one is resolved by its key',
      procedure(W: TKeyedWorld)
      begin
        W.Build;
        W.Report := W.NameOfKeyed(TValue.From<TTransport>(trSms)) + '|' +
          W.NameOfKeyed(TValue.From<TPriority>(prLow));
      end)
    .&Then('each one brings its own',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Report).ToEqual('transport|priority');
      end)

  .Scenario('A string is not the number it spells')
    .Given('a registration under the integer key 42',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddKeyedSingleton<IStorage, TBlobStorage>(42);
      end)
    .When('it is asked for with the string key 42',
      procedure(W: TKeyedWorld)
      begin
        W.Build;
        W.Report := W.NameOfKeyed('42');
      end)
    .&Then('it is not found',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Report).ToEqual(NotFound);
      end)

  .Scenario('A metaclass and a PTypeInfo work as keys')
    .Given('registrations of the same service type under two different metaclasses and a PTypeInfo',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddKeyedSingleton<IStorage>(TValue.From<TClass>(TBlobStorage),
          function(P: IServiceProvider): IStorage begin Result := TNamedStorage.Create('by-blob-class') end);
        W.Services.AddKeyedSingleton<IStorage>(TValue.From<TClass>(TFileStorage),
          function(P: IServiceProvider): IStorage begin Result := TNamedStorage.Create('by-file-class') end);
        W.Services.AddKeyedSingleton<IStorage>(TValue.From<PTypeInfo>(TypeInfo(IClock)),
          function(P: IServiceProvider): IStorage begin Result := TNamedStorage.Create('by-typeinfo') end);
      end)
    .When('each one is resolved by its key',
      procedure(W: TKeyedWorld)
      begin
        W.Build;
        W.Report := W.NameOfKeyed(TValue.From<TClass>(TBlobStorage)) + '|' +
          W.NameOfKeyed(TValue.From<TClass>(TFileStorage)) + '|' +
          W.NameOfKeyed(TValue.From<PTypeInfo>(TypeInfo(IClock)));
      end)
    .&Then('each one brings its own, and none of the three reaches the others',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Report).ToEqual('by-blob-class|by-file-class|by-typeinfo');
      end)
    .&And('a PTypeInfo nobody registered is not found',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.NameOfKeyed(TValue.From<PTypeInfo>(TypeInfo(IStorage)))).ToEqual(NotFound);
      end)

  .Scenario('A type that cannot be a key is rejected at registration')
    .Given('nothing registered',
      procedure(W: TKeyedWorld)
      begin
      end)
    .When('a service is registered with a key holding a record',
      procedure(W: TKeyedWorld)
      begin
        W.Attempt(procedure
          begin
            W.Services.AddKeyedSingleton<IStorage, TBlobStorage>(TValue.From<TGUID>(StringToGUID('{0F1E2D3C-4B5A-6978-8796-A5B4C3D2E1F0}')));
          end);
      end)
    .&Then('it fails naming the type that was used as a key',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Outcome).ToContain('TGUID');
      end)

  .Scenario('And it is rejected when resolving too')
    .Given('IStorage registered with the key blob',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddKeyedSingleton<IStorage, TBlobStorage>('blob');
      end)
    .When('it is asked for with a key of a type that cannot be one',
      procedure(W: TKeyedWorld)
      begin
        W.Build;
        W.Attempt(procedure
          begin
            W.Provider.GetKeyedService<IStorage>(TValue.From<TGUID>(StringToGUID('{0F1E2D3C-4B5A-6978-8796-A5B4C3D2E1F0}')));
          end);
      end)
    .&Then('it fails naming the type, instead of answering not found',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Outcome).ToContain('TGUID');
      end)

.Rule('The keyed space and the keyless space are isolated from each other')

  .Scenario('A registration made with a key does not show up without one')
    .Given('IStorage registered only with the key blob',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddKeyedSingleton<IStorage, TBlobStorage>('blob');
      end)
    .When('IStorage is asked for with no key',
      procedure(W: TKeyedWorld)
      begin
        W.Build;
        var Svc := W.Provider.GetService<IStorage>;
        if Svc = nil then W.Report := NotFound else W.Report := Svc.Name;
      end)
    .&Then('it is not found, exactly as if nothing were registered',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Report).ToEqual(NotFound);
      end)
    .&And('CanResolve with no key says no as well',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Provider.CanResolve(TypeInfo(IStorage))).ToBeFalse;
      end)

  .Scenario('A registration made without a key is not reached with one')
    .Given('IStorage registered with no key',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddSingleton<IStorage, TPlainStorage>;
      end)
    .When('IStorage is asked for with any key',
      procedure(W: TKeyedWorld)
      begin
        W.Build;
        W.Report := W.NameOfKeyed('blob');
      end)
    .&Then('it is not found',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Report).ToEqual(NotFound);
      end)

  .Scenario('Asking for all of them does not drag in the keyed ones')
    .Given('IStorage with two keyless registrations and three under different keys',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddSingleton<IStorage>(
          function(P: IServiceProvider): IStorage begin Result := TNamedStorage.Create('plain-1') end);
        W.Services.AddSingleton<IStorage>(
          function(P: IServiceProvider): IStorage begin Result := TNamedStorage.Create('plain-2') end);
        W.Services.AddKeyedSingleton<IStorage, TBlobStorage>('blob');
        W.Services.AddKeyedSingleton<IStorage, TFileStorage>('file');
        W.Services.AddKeyedSingleton<IStorage, TPlainStorage>(7);
      end)
    .When('every keyless IStorage is asked for',
      procedure(W: TKeyedWorld)
      begin
        W.Build;
        W.Report := '';
        for var Svc in W.Provider.GetServices<IStorage> do
          W.Report := W.Report + Svc.Name + ';';
      end)
    .&Then('exactly the two keyless ones come back',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Report).ToEqual('plain-1;plain-2;');
      end)

  .Scenario('Both spaces live together for the same type')
    .Given('IStorage registered with no key and also with the key blob',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddSingleton<IStorage, TPlainStorage>;
        W.Services.AddKeyedSingleton<IStorage, TBlobStorage>('blob');
      end)
    .When('it is resolved both ways',
      procedure(W: TKeyedWorld)
      begin
        W.Build;
        W.First := W.Provider.GetRequiredService<IStorage>;
        W.Second := W.Provider.GetRequiredKeyedService<IStorage>('blob');
        W.Report := W.First.Name + '|' + W.Second.Name;
      end)
    .&Then('each way brings its own',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Report).ToEqual('plain|blob');
      end)
    .&And('they are different instances',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.First = W.Second).ToBeFalse;
      end)

.Rule('A constructor can ask for a dependency by key')

  .Scenario('A constructor receives the implementation of its key')
    .Given('two IStorage under the keys blob and file, and a class whose constructor declares file',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddKeyedSingleton<IStorage, TBlobStorage>('blob');
        W.Services.AddKeyedSingleton<IStorage, TFileStorage>('file');
        W.Services.AddSingleton<IUsesFile, TUsesFile>;
      end)
    .When('the container builds that class',
      procedure(W: TKeyedWorld)
      begin
        W.Build;
        W.Report := W.Provider.GetRequiredService<IUsesFile>.StorageName;
      end)
    .&Then('the parameter got the implementation of file',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Report).ToEqual('file');
      end)

  .Scenario('A parameter with no key behaves as it did before')
    .Given('a class whose constructor asks for IStorage with no key, and a keyless IStorage',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddSingleton<IStorage, TPlainStorage>;
        W.Services.AddSingleton<IUsesPlain, TUsesPlain>;
      end)
    .When('the container builds it',
      procedure(W: TKeyedWorld)
      begin
        W.Build;
        W.Report := W.Provider.GetRequiredService<IUsesPlain>.StorageName;
      end)
    .&Then('it gets the keyless registration, as before this capability existed',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Report).ToEqual('plain');
      end)

  .Scenario('A parameter with a key nobody registered leaves the dependency empty at resolve time')
    // Y NO aborta, que es lo que se esperaria. El constructor con la clave insatisfacible queda
    // descartado --eso es D6 funcionando-- y el activador cae al del ancestro, que si puede
    // llamar. Quien nombra el servicio, la clave, el parametro y la clase es ValidateOnBuild, y
    // ese escenario esta abajo. El de aqui existe para que el silencio quede escrito.
    .Given('a class whose constructor declares a key nobody registered',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddKeyedSingleton<IStorage, TBlobStorage>('blob');
        W.Services.AddSingleton<IUsesUnknownKey, TUsesUnknownKey>;
      end)
    .When('the container tries to build it',
      procedure(W: TKeyedWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Report := W.Provider.GetRequiredService<IUsesUnknownKey>.StorageName end);
      end)
    .&Then('nothing is raised, and the dependency is left empty',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Outcome + ' / ' + W.Report).ToEqual(NoFailure + ' / <nil>');
      end)

  .Scenario('CONTROL: the same shape with no key at all and nothing registered')
    // El control de lo de arriba, y es lo que lo hace una medida: esto tambien queda mudo, asi
    // que lo que se ve arriba no es la clave -- es que el activador cae al constructor del
    // ancestro cuando ninguno propio se puede satisfacer, y entrega la instancia a medias.
    .Given('a class whose constructor asks for IStorage with no key, and nothing registered',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddSingleton<IUsesPlain, TUsesPlain>;
      end)
    .When('the container tries to build it',
      procedure(W: TKeyedWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Report := W.Provider.GetRequiredService<IUsesPlain>.StorageName end);
      end)
    .&Then('nothing is raised either, and the dependency is left empty',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Outcome + ' / ' + W.Report).ToEqual(NoFailure + ' / <nil>');
      end)

  .Scenario('A constructor whose keyed parameter cannot be satisfied is not the one chosen')
    // CanResolveArgs es lo que decide QUE constructor se elige. Si solo TryResolveArgs leyera la
    // clave, este se elegiria --tiene mas parametros que ninguno-- y fallaria despues.
    .Given('a class with two constructors, one asking for an unregistered key and one for IClock',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddKeyedSingleton<IStorage, TBlobStorage>('blob');
        W.Services.AddSingleton<IClock, TClock>;
        W.Services.AddSingleton<IPicksCtor, TPicksCtor>;
      end)
    .When('the container builds it',
      procedure(W: TKeyedWorld)
      begin
        W.Build;
        W.Report := W.Provider.GetRequiredService<IPicksCtor>.Chosen;
      end)
    .&Then('the one it can satisfy is chosen',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Report).ToEqual('clock');
      end)

.Rule('A marked property can ask for its dependency by key')

  .Scenario('A marked property receives the implementation of its key')
    .Given('two IStorage under blob and file, and a class with a marked property asking for file',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddKeyedSingleton<IStorage, TBlobStorage>('blob');
        W.Services.AddKeyedSingleton<IStorage, TFileStorage>('file');
        W.Services.AddSingleton<IPropByKey, TPropByKey>;
      end)
    .When('the container resolves it',
      procedure(W: TKeyedWorld)
      begin
        W.Build;
        W.Report := W.Provider.GetRequiredService<IPropByKey>.StorageName;
      end)
    .&Then('the property holds the implementation of file',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Report).ToEqual('file');
      end)

  .Scenario('A marked property with no key does not reach a keyed registration')
    .Given('IStorage registered only with the key blob, and a class with a marked property with no key',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddKeyedSingleton<IStorage, TBlobStorage>('blob');
        W.Services.AddSingleton<IPropNoKey, TPropNoKey>;
      end)
    .When('the container resolves it',
      procedure(W: TKeyedWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Provider.GetRequiredService<IPropNoKey> end);
      end)
    .&Then('it fails naming the property and its type, exactly as if nothing were registered',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Outcome).ToContain('TPropNoKey');
        Expect(W.Outcome).ToContain('Storage');
        Expect(W.Outcome).ToContain('IStorage');
      end)

  .Scenario('A key nobody registered, asked for from a property')
    .Given('a class with a marked property asking for a key nobody registered',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddKeyedSingleton<IStorage, TBlobStorage>('blob');
        W.Services.AddSingleton<IPropUnknownKey, TPropUnknownKey>;
      end)
    .When('the container resolves it',
      procedure(W: TKeyedWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Provider.GetRequiredService<IPropUnknownKey> end);
      end)
    .&Then('it fails naming the property, its type, the key and the class',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Outcome).ToContain('TPropUnknownKey');
        Expect(W.Outcome).ToContain('Storage');
        Expect(W.Outcome).ToContain('IStorage');
        Expect(W.Outcome).ToContain('nobody');
      end)

  .Scenario('A mark of the application itself is recognised the same way')
    // Una palabra en el barrido decide esto: con `Attr is` el marcador de la aplicacion existe;
    // comparando el tipo exacto no existe, y sus propiedades quedan sin inyectar EN SILENCIO.
    .Given('an application that declares its own mark as a descendant of the framework mark, typed to its own enumeration',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddKeyedSingleton<IStorage>(TValue.From<TStorageKind>(skFile),
          function(P: IServiceProvider): IStorage begin Result := TNamedStorage.Create('own-file') end);
        W.Services.AddSingleton<IPropByOwnMark, TPropByOwnMark>;
      end)
    .When('the container resolves a class with a property marked that way',
      procedure(W: TKeyedWorld)
      begin
        W.Build;
        W.Report := W.Provider.GetRequiredService<IPropByOwnMark>.StorageName;
      end)
    .&Then('the property receives the implementation of that key',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Report).ToEqual('own-file');
      end)

  .Scenario('The type of the enumeration is kept, so another one with the same ordinal does not reach it')
    .Given('the same class, with the key registered under an enumeration of another type but the same ordinal',
      procedure(W: TKeyedWorld)
      begin
        // skFile y prHigh comparten ordinal. Si el tipo se perdiera por el camino del marcador
        // propio, esta registro atenderia la propiedad -- y traeria otra implementacion.
        W.Services.AddKeyedSingleton<IStorage>(TValue.From<TPriority>(prHigh),
          function(P: IServiceProvider): IStorage begin Result := TNamedStorage.Create('wrong-enum') end);
        W.Services.AddSingleton<IPropByOwnMark, TPropByOwnMark>;
      end)
    .When('the container resolves it',
      procedure(W: TKeyedWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Provider.GetRequiredService<IPropByOwnMark> end);
      end)
    .&Then('it is not reached: the resolution fails instead of bringing the other one',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Outcome).ToContain('TPropByOwnMark');
        Expect(W.Outcome).ToContain('IStorage');
      end)

  .Scenario('An ordinal outside the range of its type is rejected')
    .Given('a marked property whose key is written as the type of an enumeration and an ordinal it does not have',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddKeyedSingleton<IStorage>(TValue.From<TStorageKind>(skFile),
          function(P: IServiceProvider): IStorage begin Result := TNamedStorage.Create('own-file') end);
        W.Services.AddSingleton<IPropBadOrdinal, TPropBadOrdinal>;
      end)
    .When('the container resolves it',
      procedure(W: TKeyedWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Provider.GetRequiredService<IPropBadOrdinal> end);
      end)
    .&Then('it fails saying the ordinal does not belong to that type, and naming the property',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Outcome).ToContain('TStorageKind');
        Expect(W.Outcome).ToContain('Storage');
      end)
    .&And('it does not behave as if the service were not registered',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Outcome).ToContain('7');
      end)

  .Scenario('Wiring an instance the container did not build respects the key')
    .Given('an object built outside the container, with a marked property asking for a key',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddKeyedSingleton<IStorage, TBlobStorage>('blob');
        W.Services.AddKeyedSingleton<IStorage, TFileStorage>('file');
        W.Guest := TGuest.Create;
      end)
    .When('its owner asks for it to be wired against the provider',
      procedure(W: TKeyedWorld)
      begin
        W.Build;
        TActivatorUtilities.InjectInto(W.Provider, W.Guest);
        W.Report := W.Guest.StorageName;
      end)
    .&Then('the property receives the implementation of that key',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Report).ToEqual('file');
      end)

.Rule('One registration can serve any key')

  .Scenario('The wildcard serves a key nobody registered, and is told which one')
    .Given('a wildcard registration for IStorage',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddKeyedSingleton<IStorage>(ServiceKey.Any,
          function(P: IServiceProvider; const Key: TValue): IStorage
          begin
            Result := TNamedStorage.Create('wild:' + Key.ToString);
          end);
      end)
    .When('IStorage is asked for with the key anything',
      procedure(W: TKeyedWorld)
      begin
        W.Build;
        W.Report := W.NameOfKeyed('anything');
      end)
    .&Then('what the wildcard produced comes back, and its factory was told the key',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Report).ToEqual('wild:anything');
      end)

  .Scenario('A concrete registration beats the wildcard')
    .Given('a wildcard registration for IStorage and a concrete one under the key blob',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddKeyedSingleton<IStorage>(ServiceKey.Any,
          function(P: IServiceProvider; const Key: TValue): IStorage
          begin
            Result := TNamedStorage.Create('wild:' + Key.ToString);
          end);
        W.Services.AddKeyedSingleton<IStorage, TBlobStorage>('blob');
      end)
    .When('IStorage is asked for with the key blob',
      procedure(W: TKeyedWorld)
      begin
        W.Build;
        W.Report := W.NameOfKeyed('blob');
      end)
    .&Then('the concrete one comes back',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Report).ToEqual('blob');
      end)

  .Scenario('Two keys served by the wildcard are two instances')
    .Given('a singleton wildcard registration for IStorage',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddKeyedSingleton<IStorage>(ServiceKey.Any,
          function(P: IServiceProvider; const Key: TValue): IStorage
          begin
            Result := TNamedStorage.Create('wild:' + Key.ToString);
          end);
      end)
    .When('it is asked for with the key a and with the key b, and then a again',
      procedure(W: TKeyedWorld)
      begin
        W.Build;
        W.First := W.Provider.GetRequiredKeyedService<IStorage>('a');
        W.Second := W.Provider.GetRequiredKeyedService<IStorage>('b');
        var Again := W.Provider.GetRequiredKeyedService<IStorage>('a');
        W.Report := Format('same-key=%s other-key=%s',
          [BoolToStr(W.First = Again, True), BoolToStr(W.First = W.Second, True)]);
      end)
    .&Then('two keys are two instances, and the same key twice is one',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Report).ToEqual('same-key=True other-key=False');
      end)

  .Scenario('The wildcard key cannot be mistaken for one of the user')
    .Given('a wildcard for IStorage and a concrete one under the first value of an application enumeration',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddKeyedSingleton<IStorage>(ServiceKey.Any,
          function(P: IServiceProvider; const Key: TValue): IStorage
          begin
            Result := TNamedStorage.Create('wild');
          end);
        W.Services.AddKeyedSingleton<IStorage>(TValue.From<TTransport>(trSms),
          function(P: IServiceProvider): IStorage begin Result := TNamedStorage.Create('concrete') end);
      end)
    .When('it is resolved by that enumeration key',
      procedure(W: TKeyedWorld)
      begin
        W.Build;
        W.Report := W.NameOfKeyed(TValue.From<TTransport>(trSms));
      end)
    .&Then('the concrete one comes back and not the wildcard',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Report).ToEqual('concrete');
      end)
    .&And('the integer 0 and the empty string are served by the wildcard, not mistaken for it',
      procedure(W: TKeyedWorld)
      begin
        // Los dos los ATIENDE el comodin, que es lo correcto: son claves que nadie registro. Lo
        // que no puede pasar es que sean EL centinela -- y eso se ve en el escenario siguiente,
        // donde un registro concreto bajo cada uno de ellos gana.
        Expect(W.NameOfKeyed(0) + '|' + W.NameOfKeyed('')).ToEqual('wild|wild');
      end)

  .Scenario('Nothing the user can write is the wildcard key itself')
    .Given('a wildcard for IStorage and concrete registrations under the integer 0 and the empty string',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddKeyedSingleton<IStorage>(ServiceKey.Any,
          function(P: IServiceProvider; const Key: TValue): IStorage
          begin
            Result := TNamedStorage.Create('wild');
          end);
        W.Services.AddKeyedSingleton<IStorage>(0,
          function(P: IServiceProvider): IStorage begin Result := TNamedStorage.Create('zero') end);
        W.Services.AddKeyedSingleton<IStorage>('',
          function(P: IServiceProvider): IStorage begin Result := TNamedStorage.Create('empty') end);
      end)
    .When('each of those keys is resolved',
      procedure(W: TKeyedWorld)
      begin
        W.Build;
        W.Report := W.NameOfKeyed(0) + '|' + W.NameOfKeyed('');
      end)
    .&Then('each brings its own registration: neither collided with the sentinel',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Report).ToEqual('zero|empty');
      end)

  .Scenario('The wildcard does not cross into the keyless space')
    .Given('a wildcard registration for IStorage and no keyless one',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddKeyedSingleton<IStorage>(ServiceKey.Any,
          function(P: IServiceProvider; const Key: TValue): IStorage
          begin
            Result := TNamedStorage.Create('wild');
          end);
      end)
    .When('IStorage is asked for with no key',
      procedure(W: TKeyedWorld)
      begin
        W.Build;
        var Svc := W.Provider.GetService<IStorage>;
        if Svc = nil then W.Report := NotFound else W.Report := Svc.Name;
      end)
    .&Then('it is not found',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Report).ToEqual(NotFound);
      end)

.Rule('A keyed registration with unbound generic arguments is rejected at registration')

  .Scenario('Registering an unbound generic with a key')
    .Given('nothing registered',
      procedure(W: TKeyedWorld)
      begin
      end)
    .When('a generic contract is registered without binding its arguments and with a key',
      procedure(W: TKeyedWorld)
      begin
        W.Attempt(procedure
          begin
            W.Services.AddKeyedSingleton<IBox<_>, TBox<_>>('blob');
          end);
      end)
    .&Then('the registration fails saying the combination is not supported',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Outcome).ToContain('not supported');
        Expect(W.Outcome).ToContain('blob');
      end)

  .Scenario('Each mechanism on its own keeps working')
    .Given('a generic contract registered unbound and keyless, and a non generic one registered with a key',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddSingleton<IBox<_>, TBox<_>>;
        W.Services.AddKeyedSingleton<IStorage, TBlobStorage>('blob');
      end)
    .When('both are resolved',
      procedure(W: TKeyedWorld)
      begin
        W.Build;
        W.Report := W.Provider.GetRequiredService<IBox<TAlphaPayload>>.Tag + '|' + W.NameOfKeyed('blob');
      end)
    .&Then('each one works as its own capability describes',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Report).ToEqual('TAlphaPayload|blob');
      end)

.Rule('Decoration works on the keyless space, and says so when there is none')

  .Scenario('Decorating a type wraps only its keyless registrations')
    // La pregunta abierta del diseno, contestada donde se ve: Decorate lee los descriptores SIN
    // clave y escribe SIN clave, asi que decorar por tipo sigue significando lo que significaba.
    .Given('IStorage registered with no key and also under blob and file',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddSingleton<IStorage, TPlainStorage>;
        W.Services.AddKeyedSingleton<IStorage, TBlobStorage>('blob');
        W.Services.AddKeyedSingleton<IStorage, TFileStorage>('file');
      end)
    .When('IStorage is decorated',
      procedure(W: TKeyedWorld)
      begin
        W.Services.Decorate<IStorage>(
          function(const Inner: IStorage; const Provider: IServiceProvider): IStorage
          begin
            Result := TWrappedStorage.Create(Inner);
          end);
        W.Build;
        W.Report := W.Provider.GetRequiredService<IStorage>.Name + '|' +
          W.NameOfKeyed('blob') + '|' + W.NameOfKeyed('file');
      end)
    .&Then('the keyless one comes back wrapped and the keyed ones untouched',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Report).ToEqual('wrapped(plain)|blob|file');
      end)

  .Scenario('Decorating a type that only has keyed registrations fails saying nothing is registered')
    .Given('IStorage registered only under blob',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddKeyedSingleton<IStorage, TBlobStorage>('blob');
      end)
    .When('IStorage is decorated',
      procedure(W: TKeyedWorld)
      begin
        W.Attempt(procedure
          begin
            W.Services.Decorate<IStorage>(
              function(const Inner: IStorage; const Provider: IServiceProvider): IStorage
              begin
                Result := TWrappedStorage.Create(Inner);
              end);
          end);
      end)
    .&Then('it fails, because from the keyless space there is nothing to wrap',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Outcome).ToContain('nothing is registered');
      end)

.Rule('Idempotent registration asks about the pair, not about the type')

  .Scenario('One key does not block another')
    .Given('a collection with IStorage registered under the key blob',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddKeyedSingleton<IStorage, TBlobStorage>('blob');
      end)
    .When('TryAddKeyedSingleton is called for IStorage with the key file',
      procedure(W: TKeyedWorld)
      begin
        W.Services.TryAddKeyedSingleton<IStorage, TFileStorage>('file');
        W.Build;
        W.Report := W.NameOfKeyed('file') + '|' + W.NameOfKeyed('blob');
      end)
    .&Then('the file registration goes in, and blob keeps its own',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Report).ToEqual('file|blob');
      end)

  .Scenario('The same key is respected')
    .Given('a collection with IStorage registered under the key blob',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddKeyedSingleton<IStorage, TBlobStorage>('blob');
      end)
    .When('TryAddKeyedSingleton is called for IStorage with that same key',
      procedure(W: TKeyedWorld)
      begin
        W.Services.TryAddKeyedSingleton<IStorage, TFileStorage>('blob');
        W.Build;
        W.Report := W.NameOfKeyed('blob') + '|' + IntToStr(Length(W.Services.KeyedDescriptors<IStorage>('blob')));
      end)
    .&Then('nothing is replaced, and the first one is still there',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Report).ToEqual('blob|1');
      end)

  .Scenario('A keyed registration does not block the keyless one')
    .Given('a collection with IStorage registered under the key blob',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddKeyedSingleton<IStorage, TBlobStorage>('blob');
      end)
    .When('TryAddSingleton is called for IStorage with no key',
      procedure(W: TKeyedWorld)
      begin
        W.Services.TryAddSingleton(TypeInfo(IStorage), TPlainStorage);
        W.Build;
        W.Report := W.Provider.GetRequiredService<IStorage>.Name;
      end)
    .&Then('the keyless registration goes in',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Report).ToEqual('plain');
      end)

.Rule('Removing reaches only what the caller named')

  .Scenario('Removing the keyless ones does not touch the keyed ones')
    .Given('a collection with IStorage registered with no key and under blob and file',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddSingleton<IStorage, TPlainStorage>;
        W.Services.AddKeyedSingleton<IStorage, TBlobStorage>('blob');
        W.Services.AddKeyedSingleton<IStorage, TFileStorage>('file');
      end)
    .When('RemoveAll is called for IStorage',
      procedure(W: TKeyedWorld)
      begin
        W.Services.RemoveAll<IStorage>;
        W.Build;
        var Svc := W.Provider.GetService<IStorage>;
        var Plain := NotFound;
        if Svc <> nil then Plain := Svc.Name;
        W.Report := Plain + '|' + W.NameOfKeyed('blob') + '|' + W.NameOfKeyed('file');
      end)
    .&Then('the keyless registration is gone and blob and file still resolve',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Report).ToEqual(NotFound + '|blob|file');
      end)

  .Scenario('Removing one key touches neither the others nor the keyless one')
    .Given('the same collection',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddSingleton<IStorage, TPlainStorage>;
        W.Services.AddKeyedSingleton<IStorage, TBlobStorage>('blob');
        W.Services.AddKeyedSingleton<IStorage, TFileStorage>('file');
      end)
    .When('RemoveAllKeyed is called for IStorage with the key blob',
      procedure(W: TKeyedWorld)
      begin
        W.Services.RemoveAllKeyed<IStorage>('blob');
        W.Build;
        W.Report := W.Provider.GetRequiredService<IStorage>.Name + '|' +
          W.NameOfKeyed('blob') + '|' + W.NameOfKeyed('file');
      end)
    .&Then('only the blob one is gone',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Report).ToEqual('plain|' + NotFound + '|file');
      end)

.Rule('Validation covers the keyed registrations and names the key')

  .Scenario('A keyed dependency nobody registered is caught at build time')
    .Given('a registered class whose constructor asks for IStorage with the key blob, with only file registered',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddKeyedSingleton<IStorage, TFileStorage>('file');
        W.Services.AddSingleton<IHealthy, THealthy>;
      end)
    .When('the provider is built with ValidateOnBuild',
      procedure(W: TKeyedWorld)
      begin
        W.BuildWith(TServiceProviderOptions.Create(True, False));
      end)
    .&Then('it fails naming the service, the key blob, the parameter and the class',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Failure).ToContain('IStorage');
        Expect(W.Failure).ToContain('blob');
        Expect(W.Failure).ToContain('Storage');
        Expect(W.Failure).ToContain('THealthy');
      end)

  .Scenario('A keyed registration whose own dependency is unresolved')
    .Given('a class registered under the key blob whose constructor asks for a service nobody registered',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddKeyedSingleton<IKeyedHolder, TKeyedHolder>('blob');
      end)
    .When('the provider is built with ValidateOnBuild',
      procedure(W: TKeyedWorld)
      begin
        W.BuildWith(TServiceProviderOptions.Create(True, False));
      end)
    .&Then('it fails, and the message identifies the registration by its type and its key',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Failure).ToContain('IKeyedHolder');
        Expect(W.Failure).ToContain('blob');
        Expect(W.Failure).ToContain('IMissingDep');
      end)

  .Scenario('A keyed singleton that captures a scoped one')
    .Given('a singleton registered with a key whose constructor asks for a scoped service',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddScoped<IScopedDep, TScopedDep>;
        W.Services.AddKeyedSingleton<IKeyedCaptor, TKeyedCaptor>('blob');
      end)
    .When('the provider is built with ValidateScopes',
      procedure(W: TKeyedWorld)
      begin
        W.BuildWith(TServiceProviderOptions.Create(False, True));
      end)
    .&Then('it fails naming both services, the key and their lifetimes',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Failure).ToContain('IKeyedCaptor');
        Expect(W.Failure).ToContain('blob');
        Expect(W.Failure).ToContain('IScopedDep');
        Expect(W.Failure).ToContain('singleton');
        Expect(W.Failure).ToContain('scoped');
      end)

  .Scenario('A healthy graph with keys is not blocked')
    .Given('a composition with several keyed registrations, every dependency registered and lifetimes consistent',
      procedure(W: TKeyedWorld)
      begin
        W.Services.AddKeyedSingleton<IStorage, TBlobStorage>('blob');
        W.Services.AddKeyedSingleton<IStorage, TFileStorage>('file');
        W.Services.AddKeyedSingleton<IHealthy, THealthy>('main');
      end)
    .When('it is built with ValidateOnBuild and ValidateScopes',
      procedure(W: TKeyedWorld)
      begin
        W.BuildWith(TServiceProviderOptions.Create(True, True));
      end)
    .&Then('the provider is built without flagging anything',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Failure).ToEqual(NoFailure);
      end)
    .&And('the keyed registrations resolve',
      procedure(W: TKeyedWorld)
      begin
        Expect(W.Provider.GetRequiredKeyedService<IHealthy>('main').Name).ToEqual('healthy');
      end);

end.
