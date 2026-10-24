unit PropertyInjection.Feat;

interface

uses
  System.SysUtils,
  Daf.Extensions.DependencyInjection;

type
  /// Servicios de juguete: lo unico que se les pregunta es su nombre, que es lo que distingue
  /// "la propiedad se relleno" de "la propiedad sigue en nil".
  IStorage = interface(IInvokable)
    ['{7820E325-2683-49C5-93DF-2D52AA090328}']
    function Name: string;
  end;

  IClock = interface(IInvokable)
    ['{5DC60BC3-4C0E-4D1E-8203-C5E58F39158D}']
    function Name: string;
  end;

  /// Nadie lo registra nunca.
  IUnregistered = interface(IInvokable)
    ['{0A6D6E3F-6E1C-4F2E-9E58-4B1F6D2A77C1}']
    function Name: string;
  end;

  /// Cuenta sus destrucciones: es lo que separa "sigue vivo" de "quedo una interfaz sobre
  /// memoria liberada", que responden igual hasta que dejan de hacerlo.
  ICounted = interface(IInvokable)
    ['{2F5B6A8D-4C31-4A0E-9D77-8E0C1A5B4E22}']
    function Name: string;
  end;

  TStorage = class(TInterfacedObject, IStorage)
  public
    function Name: string;
  end;

  TClock = class(TInterfacedObject, IClock)
  public
    function Name: string;
  end;

  TCounted = class(TInterfacedObject, ICounted)
  public
    class var Destroyed: Integer;
    destructor Destroy; override;
    function Name: string;
  end;

  // --- Las cuatro formas de la propiedad marcada ---

  IByField = interface(IInvokable)
    ['{9B1C4E77-5A2D-4C0B-8D33-6E7A1B9F4C10}']
    function StorageName: string;
  end;

  TByField = class(TInterfacedObject, IByField)
  private
    FStorage: IStorage;
  public
    [DafInject]
    property Storage: IStorage read FStorage write FStorage;
    function StorageName: string;
  end;

  IBySetter = interface(IInvokable)
    ['{4D8E2A16-7B0C-4F35-9A61-2C5D8E3F0B44}']
    function StorageName: string;
  end;

  TBySetter = class(TInterfacedObject, IBySetter)
  private
    FStorage: IStorage;
    procedure SetStorage(const Value: IStorage);
  public
    class var SetterRuns: Integer;
    [DafInject]
    property Storage: IStorage read FStorage write SetStorage;
    function StorageName: string;
  end;

  IByStrictField = interface(IInvokable)
    ['{6C3A9F52-1E4B-4D08-B7A9-3F8D2C6E5A71}']
    function StorageName: string;
  end;

  TByStrictField = class(TInterfacedObject, IByStrictField)
  strict private
    FStorage: IStorage;
  public
    [DafInject]
    property Storage: IStorage read FStorage write FStorage;
    function StorageName: string;
  end;

  IInherited = interface(IInvokable)
    ['{1E7B5D04-8C6A-4239-95FB-7A2E4C1D8B36}']
    function StorageName: string;
  end;

  /// La marca la lleva la propiedad de la base; la declaracion de participar, la hija.
  TInjectableBase = class(TInterfacedObject)
  private
    FStorage: IStorage;
  public
    [DafInject]
    property Storage: IStorage read FStorage write FStorage;
  end;

  TInheritedProp = class(TInjectableBase, IInherited)
  public
    function StorageName: string;
  end;

  IWithUnmarked = interface(IInvokable)
    ['{3A9C6E18-0D5F-4B72-A84E-1B6C7D2F5093}']
    function StorageName: string;
    function ClockIsNil: Boolean;
  end;

  TWithUnmarked = class(TInterfacedObject, IWithUnmarked)
  private
    FStorage: IStorage;
    FClock: IClock;
  public
    [DafInject]
    property Storage: IStorage read FStorage write FStorage;
    property Clock: IClock read FClock write FClock;
    function StorageName: string;
    function ClockIsNil: Boolean;
  end;

  // --- El constructor y la propiedad ---

  ICtorReader = interface(IInvokable)
    ['{5B2D8F41-6A03-4C97-BE15-9D4A3E7C8021}']
    function StorageName: string;
  end;

  TCtorReader = class(TInterfacedObject, ICtorReader)
  private
    FStorage: IStorage;
  public
    class var StorageInCtorWasNil: Boolean;
    constructor Create;
    [DafInject]
    property Storage: IStorage read FStorage write FStorage;
    function StorageName: string;
  end;

  IBoth = interface(IInvokable)
    ['{8E4F1C60-2B7D-4A38-91C5-6F0A2D9B7E34}']
    function StorageName: string;
  end;

  TBoth = class(TInterfacedObject, IBoth)
  private
    FClock: IClock;
    FStorage: IStorage;
  public
    class var ClockInCtor: string;
    constructor Create(const Clock: IClock);
    [DafInject]
    property Storage: IStorage read FStorage write FStorage;
    function StorageName: string;
  end;

  // --- Los tres fallos que abortan ---

  INeedsUnregistered = interface(IInvokable)
    ['{7C1A5E29-3F84-4D60-B2E7-0C8B6A4F1D53}']
  end;

  TNeedsUnregistered = class(TInterfacedObject, INeedsUnregistered)
  private
    FMissing: IUnregistered;
  public
    [DafInject]
    property Missing: IUnregistered read FMissing write FMissing;
  end;

  IReadOnlyMark = interface(IInvokable)
    ['{2D6F8B37-5C10-4E9A-8B43-7A1E5D0C2F68}']
  end;

  TReadOnlyMark = class(TInterfacedObject, IReadOnlyMark)
  private
    FStorage: IStorage;
  public
    [DafInject]
    property Storage: IStorage read FStorage;
  end;

  INonInterfaceMark = interface(IInvokable)
    ['{6A0E3C74-8D25-4F1B-93A6-2E7C5B9D0184}']
  end;

  TNonInterfaceMark = class(TInterfacedObject, INonInterfaceMark)
  private
    FRetries: Integer;
  public
    [DafInject]
    property Retries: Integer read FRetries write FRetries;
  end;

  /// No marca ninguna propiedad, teniendo una que el contenedor sabria rellenar. Participar se
  /// deduce de marcar, asi que esta clase no participa -- y esa es la unica lectura posible.
  INothingMarked = interface(IInvokable)
    ['{4F8C2A05-7E63-4B19-A0D8-5C3B1F6E9247}']
    function StorageName: string;
  end;

  TNothingMarked = class(TInterfacedObject, INothingMarked)
  private
    FStorage: IStorage;
  public
    property Storage: IStorage read FStorage write FStorage;
    function StorageName: string;
  end;

  /// La marca esta escrita DENTRO de un tipo generico. Existe como control: si el compilador
  /// dejase de emitir esos atributos, la propiedad se quedaria sin marcar y el sintoma seria el
  /// mismo que el de un recorte de RTTI, en otra unit y sin directivas que mirar.
  IGenericMarked = interface(IInvokable)
    ['{9D5B4E82-1A70-4C36-88F2-6B0D3A7C5E19}']
    function StorageName: string;
  end;

  TGenericBase<T: IInterface> = class(TInterfacedObject)
  private
    FStorage: IStorage;
  public
    [DafInject]
    property Storage: IStorage read FStorage write FStorage;
    function StorageName: string;
  end;

  TGenericMarked = class(TGenericBase<IClock>, IGenericMarked)
  end;

  /// Los contratos de las dos clases que viven en la unit con la RTTI recortada. Viven aqui, con
  /// la RTTI completa, porque lo que aquel escenario recorta son las PROPIEDADES de sus clases.
  ITrimmedParticipant = interface(IInvokable)
    ['{7B4D0A96-2C58-4E31-B9A7-5D1F3C8E6042}']
    function StorageName: string;
  end;

  ITrimmedOutsider = interface(IInvokable)
    ['{1C6E9B40-8A37-4D52-96F4-2B5D0E7A3C81}']
    function StorageName: string;
  end;

  // --- Ciclos ---

  IAlphaSvc = interface;
  IBetaSvc = interface;

  IAlphaSvc = interface(IInvokable)
    ['{0B7E1D46-9C58-4A23-B6F0-3D8A2C5E7149}']
    function Serial: Integer;
    function Peer: IBetaSvc;
  end;

  IBetaSvc = interface(IInvokable)
    ['{5E9A0C73-4B16-4D82-97C5-1F6B8D3A2E50}']
    function Serial: Integer;
    function Peer: IAlphaSvc;
  end;

  TAlphaSvc = class(TInterfacedObject, IAlphaSvc)
  private
    FSerial: Integer;
    FBeta: IBetaSvc;
  public
    class var Created: Integer;
    class var Destroyed: Integer;
    constructor Create;
    destructor Destroy; override;
    [DafInject]
    property Beta: IBetaSvc read FBeta write FBeta;
    function Serial: Integer;
    function Peer: IBetaSvc;
  end;

  TBetaSvc = class(TInterfacedObject, IBetaSvc)
  private
    FSerial: Integer;
    FAlpha: IAlphaSvc;
  public
    class var Created: Integer;
    class var Destroyed: Integer;
    constructor Create;
    destructor Destroy; override;
    [DafInject]
    property Alpha: IAlphaSvc read FAlpha write FAlpha;
    function Serial: Integer;
    function Peer: IAlphaSvc;
  end;

  /// Un ciclo de tres, para que "romper el ciclo" no pueda salir bien por casualidad con dos.
  IRed = interface;
  IGreen = interface;
  IBlue = interface;

  IRed = interface(IInvokable)
    ['{6E0C4A85-3D71-4926-B84F-1C5A7E2D9B03}']
    function Next: IGreen;
  end;

  IGreen = interface(IInvokable)
    ['{2A7F5C13-9B48-4E60-A2D5-7F3C0B6E8A14}']
    function Next: IBlue;
  end;

  IBlue = interface(IInvokable)
    ['{8D3B1E70-5C29-4A46-91B8-0E6F2A4D7C35}']
    function Next: IRed;
  end;

  TRed = class(TInterfacedObject, IRed)
  private
    FNext: IGreen;
  public
    class var Created: Integer;
    class var Alive: Integer;
    constructor Create;
    destructor Destroy; override;
    [DafInject]
    property Green: IGreen read FNext write FNext;
    function Next: IGreen;
  end;

  TGreen = class(TInterfacedObject, IGreen)
  private
    FNext: IBlue;
  public
    class var Created: Integer;
    class var Alive: Integer;
    constructor Create;
    destructor Destroy; override;
    [DafInject]
    property Blue: IBlue read FNext write FNext;
    function Next: IBlue;
  end;

  TBlue = class(TInterfacedObject, IBlue)
  private
    FNext: IRed;
  public
    class var Created: Integer;
    class var Alive: Integer;
    constructor Create;
    destructor Destroy; override;
    [DafInject]
    property Red: IRed read FNext write FNext;
    function Next: IRed;
  end;

  /// Se pide a si mismo por propiedad.
  ITwin = interface(IInvokable)
    ['{3C8D5F91-6E24-4B07-A153-9D2F7A0C4B68}']
    function Serial: Integer;
    function Peer: ITwin;
  end;

  TTwin = class(TInterfacedObject, ITwin)
  private
    FSerial: Integer;
    FTwin: ITwin;
  public
    class var Created: Integer;
    constructor Create;
    [DafInject]
    property Twin: ITwin read FTwin write FTwin;
    function Serial: Integer;
    function Peer: ITwin;
  end;

  /// Ciclo por CONSTRUCTOR: inconstruible, y hoy mata el proceso sin decir nada.
  ICtorLeft = interface(IInvokable)
    ['{8A2E6B15-0F73-4C49-92D6-5B1C8E3A7F02}']
  end;

  ICtorRight = interface(IInvokable)
    ['{1F7C4A83-2D69-4E50-B8A1-6C0E5D9B3247}']
  end;

  TCtorLeft = class(TInterfacedObject, ICtorLeft)
  public
    constructor Create(const Right: ICtorRight);
  end;

  TCtorRight = class(TInterfacedObject, ICtorRight)
  public
    constructor Create(const Left: ICtorLeft);
  end;

  /// Ciclo que pasa por un Transient: se puede resolver y no se puede deshacer, asi que se
  /// rechaza.
  ITransientPeer = interface;

  ISingletonPeer = interface(IInvokable)
    ['{6D0B3E58-9A47-4F21-8C35-2E7A1D4B6039}']
  end;

  ITransientPeer = interface(IInvokable)
    ['{4B9F1C60-3E85-4A72-96D0-8A2C5F7E1B43}']
  end;

  TSingletonPeer = class(TInterfacedObject, ISingletonPeer)
  private
    FPeer: ITransientPeer;
  public
    [DafInject]
    property Peer: ITransientPeer read FPeer write FPeer;
  end;

  TTransientPeer = class(TInterfacedObject, ITransientPeer)
  private
    FPeer: ISingletonPeer;
  public
    [DafInject]
    property Peer: ISingletonPeer read FPeer write FPeer;
  end;

  // --- Instancia ajena, la que el contenedor no construye ---

  /// Ni interfaz ni registro: lo construye y lo destruye su dueño, como haria la plataforma con
  /// un formulario.
  TGuest = class
  private
    FStorage: IStorage;
    FCounted: ICounted;
  public
    [DafInject]
    property Storage: IStorage read FStorage write FStorage;
    [DafInject]
    property Counted: ICounted read FCounted write FCounted;
  end;

  // --- Cableado por codigo ---

  IPlain = interface(IInvokable)
    ['{2E5A8D14-7C36-4B90-A6F2-0D9B4E1C8375}']
    function StorageName: string;
  end;

  /// Ni una marca: lo suyo lo escribe quien compone.
  TPlain = class(TInterfacedObject, IPlain)
  private
    FStorage: IStorage;
  public
    property Storage: IStorage read FStorage write FStorage;
    function StorageName: string;
  end;

  IMarkedAndWired = interface(IInvokable)
    ['{7F1D6B29-4A80-4E53-B1C7-3A6E9D2F5048}']
    function Note: string;
  end;

  TMarkedAndWired = class(TInterfacedObject, IMarkedAndWired)
  private
    FStorage: IStorage;
    FNote: string;
  public
    [DafInject]
    property Storage: IStorage read FStorage write FStorage;
    procedure SetNote(const Value: string);
    function Note: string;
  end;

  IFirstHeir = interface(IInvokable)
    ['{0C4E7A93-6B15-4D28-95F0-8E2A1C6D3B74}']
    function Mark: string;
  end;

  ISecondHeir = interface(IInvokable)
    ['{5A8C2F07-1D64-4B39-86E1-4F0B7D9A2C56}']
    function Mark: string;
  end;

  TWiredBase = class(TInterfacedObject)
  private
    FTag: string;
  public
    property Tag: string read FTag write FTag;
    function Mark: string;
  end;

  TFirstHeir = class(TWiredBase, IFirstHeir)
  end;

  TSecondHeir = class(TWiredBase, ISecondHeir)
  end;

  IOrdered = interface(IInvokable)
    ['{3D7B0E62-8F41-4A95-B0C6-2A5D9E1F7483}']
    function Trace: string;
  end;

  TOrdered = class(TInterfacedObject, IOrdered)
  private
    FTrace: string;
  public
    procedure Append(const Step: string);
    function Trace: string;
  end;

  IThrower = interface(IInvokable)
    ['{9E2A5C38-0B74-4F16-A853-7D1C6B4E0925}']
  end;

  TThrower = class(TInterfacedObject, IThrower)
  end;

  ISelfResolving = interface(IInvokable)
    ['{D85F2A31-6C07-4B94-8E2D-1A9C3F7B0E62}']
  end;

  /// Su constructor pide su propio servicio al proveedor.
  TSelfResolving = class(TInterfacedObject, ISelfResolving)
  public
    constructor Create(const Provider: IServiceProvider);
  end;

  IFlakyWired = interface(IInvokable)
    ['{A2C47E19-5B83-4D06-9E71-3F8B0D6C2A45}']
    function Describe: string;
  end;

  /// Se numera al construirse y cuenta construcciones y destrucciones: lo que distingue «se volvio a
  /// construir» de «se entrego la misma».
  TFlakyWired = class(TInterfacedObject, IFlakyWired)
  private
    FNumber: Integer;
    FWired: string;
  public
    class var Created: Integer;
    class var Destroyed: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure MarkWired;
    function Describe: string;
  end;

  // --- Decoracion ---

  IDecoratedSvc = interface(IInvokable)
    ['{6B3E9A17-2D50-4C84-9F16-0A7C5E3B8D21}']
    function Name: string;
  end;

  /// El original que el contenedor construye por clase: recibe un ICounted por propiedad.
  TDecoratedOriginal = class(TInterfacedObject, IDecoratedSvc)
  private
    FCounted: ICounted;
  public
    class var Destroyed: Integer;
    destructor Destroy; override;
    [DafInject]
    property Counted: ICounted read FCounted write FCounted;
    function Name: string;
  end;

  TDecoratorSvc = class(TInterfacedObject, IDecoratedSvc)
  private
    FInner: IDecoratedSvc;
  public
    class var Destroyed: Integer;
    constructor Create(const Inner: IDecoratedSvc);
    destructor Destroy; override;
    function Name: string;
  end;

  TInjectionWorld = class
  public
    Services: IServiceCollection;
    Provider: IServiceProvider;
    // Lo que el dueno de una instancia registrada le pone en su propiedad marcada.
    OwnerStorage: IStorage;
    Guest: TGuest;
    Outcome: string;
    Report: string;
    Held: IInterface;
    Numbers: TArray<Integer>;
    Closed: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Build;
    procedure Close;
    procedure Attempt(const Action: TProc);
    function Owned: Integer;
  end;

const
  // Centinela con texto: la cadena vacia es tambien lo que se lee cuando nadie escribio nada.
  NoFailure = '<sin error>';

implementation

uses
  System.TypInfo,
  Daf.MiniSpec,
  Daf.DependencyInjection,
  Daf.DependencyInjection.ActivatorUtilities;

{ TStorage }

function TStorage.Name: string;
begin
  Result := 'storage';
end;

{ TClock }

function TClock.Name: string;
begin
  Result := 'clock';
end;

{ TCounted }

destructor TCounted.Destroy;
begin
  Inc(Destroyed);
  inherited;
end;

function TCounted.Name: string;
begin
  Result := 'counted';
end;

{ TByField }

function TByField.StorageName: string;
begin
  if FStorage = nil then Exit('<nil>');
  Result := FStorage.Name;
end;

{ TBySetter }

procedure TBySetter.SetStorage(const Value: IStorage);
begin
  Inc(SetterRuns);
  FStorage := Value;
end;

function TBySetter.StorageName: string;
begin
  if FStorage = nil then Exit('<nil>');
  Result := FStorage.Name;
end;

{ TByStrictField }

function TByStrictField.StorageName: string;
begin
  if FStorage = nil then Exit('<nil>');
  Result := FStorage.Name;
end;

{ TInheritedProp }

function TInheritedProp.StorageName: string;
begin
  if Storage = nil then Exit('<nil>');
  Result := Storage.Name;
end;

{ TNothingMarked }

function TNothingMarked.StorageName: string;
begin
  if FStorage = nil then Exit('<nil>');
  Result := FStorage.Name;
end;

{ TWithUnmarked }

function TWithUnmarked.StorageName: string;
begin
  if FStorage = nil then Exit('<nil>');
  Result := FStorage.Name;
end;

function TWithUnmarked.ClockIsNil: Boolean;
begin
  Result := FClock = nil;
end;

{ TCtorReader }

constructor TCtorReader.Create;
begin
  inherited Create;
  StorageInCtorWasNil := FStorage = nil;
end;

function TCtorReader.StorageName: string;
begin
  if FStorage = nil then Exit('<nil>');
  Result := FStorage.Name;
end;

{ TBoth }

constructor TBoth.Create(const Clock: IClock);
begin
  inherited Create;
  FClock := Clock;
  if Clock = nil then
    ClockInCtor := '<nil>'
  else
    ClockInCtor := Clock.Name;
end;

function TBoth.StorageName: string;
begin
  if FStorage = nil then Exit('<nil>');
  Result := FStorage.Name;
end;

{ TGenericBase<T> }

function TGenericBase<T>.StorageName: string;
begin
  if FStorage = nil then Exit('<nil>');
  Result := FStorage.Name;
end;

{ TAlphaSvc }

constructor TAlphaSvc.Create;
begin
  inherited Create;
  Inc(Created);
  FSerial := Created;
end;

destructor TAlphaSvc.Destroy;
begin
  Inc(Destroyed);
  inherited;
end;

function TAlphaSvc.Serial: Integer;
begin
  Result := FSerial;
end;

function TAlphaSvc.Peer: IBetaSvc;
begin
  Result := FBeta;
end;

{ TBetaSvc }

constructor TBetaSvc.Create;
begin
  inherited Create;
  Inc(Created);
  FSerial := Created;
end;

destructor TBetaSvc.Destroy;
begin
  Inc(Destroyed);
  inherited;
end;

function TBetaSvc.Serial: Integer;
begin
  Result := FSerial;
end;

function TBetaSvc.Peer: IAlphaSvc;
begin
  Result := FAlpha;
end;

{ TRed }

constructor TRed.Create;
begin
  inherited Create;
  Inc(Created);
  Inc(Alive);
end;

destructor TRed.Destroy;
begin
  Dec(Alive);
  inherited;
end;

function TRed.Next: IGreen;
begin
  Result := FNext;
end;

{ TGreen }

constructor TGreen.Create;
begin
  inherited Create;
  Inc(Created);
  Inc(Alive);
end;

destructor TGreen.Destroy;
begin
  Dec(Alive);
  inherited;
end;

function TGreen.Next: IBlue;
begin
  Result := FNext;
end;

{ TBlue }

constructor TBlue.Create;
begin
  inherited Create;
  Inc(Created);
  Inc(Alive);
end;

destructor TBlue.Destroy;
begin
  Dec(Alive);
  inherited;
end;

function TBlue.Next: IRed;
begin
  Result := FNext;
end;

{ TTwin }

constructor TTwin.Create;
begin
  inherited Create;
  Inc(Created);
  FSerial := Created;
end;

function TTwin.Serial: Integer;
begin
  Result := FSerial;
end;

function TTwin.Peer: ITwin;
begin
  Result := FTwin;
end;

{ TCtorLeft }

constructor TCtorLeft.Create(const Right: ICtorRight);
begin
  inherited Create;
end;

{ TCtorRight }

constructor TCtorRight.Create(const Left: ICtorLeft);
begin
  inherited Create;
end;

{ TPlain }

function TPlain.StorageName: string;
begin
  if FStorage = nil then Exit('<nil>');
  Result := FStorage.Name;
end;

{ TMarkedAndWired }

procedure TMarkedAndWired.SetNote(const Value: string);
begin
  FNote := Value;
end;

function TMarkedAndWired.Note: string;
begin
  Result := FNote;
end;

{ TWiredBase }

function TWiredBase.Mark: string;
begin
  Result := FTag;
end;

{ TOrdered }

procedure TOrdered.Append(const Step: string);
begin
  FTrace := FTrace + Step;
end;

function TOrdered.Trace: string;
begin
  Result := FTrace;
end;

{ TInjectionWorld }

{ TSelfResolving }

constructor TSelfResolving.Create(const Provider: IServiceProvider);
begin
  inherited Create;
  Provider.GetRequiredService<ISelfResolving>;
end;

{ TFlakyWired }

constructor TFlakyWired.Create;
begin
  inherited Create;
  Inc(Created);
  FNumber := Created;
end;

destructor TFlakyWired.Destroy;
begin
  Inc(Destroyed);
  inherited;
end;

procedure TFlakyWired.MarkWired;
begin
  FWired := 'wired';
end;

function TFlakyWired.Describe: string;
begin
  Result := Format('#%d %s', [FNumber, FWired]);
end;

{ TDecoratedOriginal }

destructor TDecoratedOriginal.Destroy;
begin
  Inc(Destroyed);
  inherited;
end;

function TDecoratedOriginal.Name: string;
begin
  Result := 'original';
end;

{ TDecoratorSvc }

constructor TDecoratorSvc.Create(const Inner: IDecoratedSvc);
begin
  inherited Create;
  FInner := Inner;
end;

destructor TDecoratorSvc.Destroy;
begin
  Inc(Destroyed);
  inherited;
end;

function TDecoratorSvc.Name: string;
begin
  Result := 'decorated(' + FInner.Name + ')';
end;

constructor TInjectionWorld.Create;
begin
  inherited;
  TDecoratedOriginal.Destroyed := 0;
  TDecoratorSvc.Destroyed := 0;
  TFlakyWired.Created := 0;
  TFlakyWired.Destroyed := 0;
  TBySetter.SetterRuns := 0;
  TCounted.Destroyed := 0;
  TCtorReader.StorageInCtorWasNil := False;
  TBoth.ClockInCtor := '';
  TAlphaSvc.Created := 0;
  TAlphaSvc.Destroyed := 0;
  TBetaSvc.Created := 0;
  TBetaSvc.Destroyed := 0;
  TTwin.Created := 0;
  TRed.Created := 0;
  TRed.Alive := 0;
  TGreen.Created := 0;
  TGreen.Alive := 0;
  TBlue.Created := 0;
  TBlue.Alive := 0;
  Outcome := '<sin intentar>';
  Services := TServiceCollection.Create;
end;

destructor TInjectionWorld.Destroy;
begin
  Held := nil;
  Guest.Free;
  if (Provider <> nil) and not Closed then
    Provider.ShutDown;
  inherited;
end;

procedure TInjectionWorld.Build;
begin
  if Provider = nil then
    Provider := Services.BuildServiceProvider;
end;

procedure TInjectionWorld.Close;
begin
  Provider.ShutDown;
  Closed := True;
end;

procedure TInjectionWorld.Attempt(const Action: TProc);
begin
  Outcome := NoFailure;
  try
    Action();
  except
    on E: Exception do Outcome := E.ClassName + ': ' + E.Message;
  end;
end;

function TInjectionWorld.Owned: Integer;
begin
  var Access: IContainerAccess;
  if not Supports(Provider, IContainerAccess, Access) then
    raise Exception.Create('the provider does not expose IContainerAccess');
  Result := Access.OwnedInstanceCount;
end;

initialization

Feature('''
Feature Property injection @di @injection

  As an application author
  I want a property declared as a dependency to be filled by the container
  So I can reach where the constructor cannot: an instance the container did not build, and two
  services that need each other
''')

.UseWorld<TInjectionWorld>

.Rule('A marked property receives the service its type declares')

  .Scenario('A property backed by a field is filled')
    .Given('a class that declares participation, with a marked property backed by a field',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<IStorage, TStorage>;
        W.Services.AddSingleton<IByField, TByField>;
      end)
    .When('the container resolves it',
      procedure(W: TInjectionWorld)
      begin
        W.Build;
        W.Report := W.Provider.GetRequiredService<IByField>.StorageName;
      end)
    .&Then('the property holds the instance registered for that type',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Report).ToEqual('storage');
      end)

  .Scenario('A property backed by a method is filled the same way')
    .Given('the same class but with the property written through a method',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<IStorage, TStorage>;
        W.Services.AddSingleton<IBySetter, TBySetter>;
      end)
    .When('the container resolves it',
      procedure(W: TInjectionWorld)
      begin
        W.Build;
        W.Report := W.Provider.GetRequiredService<IBySetter>.StorageName;
      end)
    .&Then('the property holds the instance',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Report).ToEqual('storage');
      end)
    .&And('the writing method has run',
      procedure(W: TInjectionWorld)
      begin
        Expect(TBySetter.SetterRuns).ToEqual(1);
      end)

  .Scenario('A property over a strict private field is filled')
    .Given('a class whose marked property is backed by a strict private field',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<IStorage, TStorage>;
        W.Services.AddSingleton<IByStrictField, TByStrictField>;
      end)
    .When('the container resolves it',
      procedure(W: TInjectionWorld)
      begin
        W.Build;
        W.Report := W.Provider.GetRequiredService<IByStrictField>.StorageName;
      end)
    .&Then('the property holds the instance',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Report).ToEqual('storage');
      end)

  .Scenario('An inherited property is filled')
    .Given('a class that declares participation and whose marked property is declared by its base',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<IStorage, TStorage>;
        W.Services.AddSingleton<IInherited, TInheritedProp>;
      end)
    .When('the container resolves it',
      procedure(W: TInjectionWorld)
      begin
        W.Build;
        W.Report := W.Provider.GetRequiredService<IInherited>.StorageName;
      end)
    .&Then('the inherited property is filled',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Report).ToEqual('storage');
      end)

  .Scenario('An unmarked property is left alone')
    .Given('a class with an unmarked interface property whose type the container could resolve',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<IStorage, TStorage>;
        W.Services.AddSingleton<IClock, TClock>;
        W.Services.AddSingleton<IWithUnmarked, TWithUnmarked>;
      end)
    .When('the container resolves it',
      procedure(W: TInjectionWorld)
      begin
        W.Build;
        W.Held := W.Provider.GetRequiredService<IWithUnmarked>;
      end)
    .&Then('the marked one is filled',
      procedure(W: TInjectionWorld)
      begin
        Expect((W.Held as IWithUnmarked).StorageName).ToEqual('storage');
      end)
    .&And('the unmarked one is as the constructor left it',
      procedure(W: TInjectionWorld)
      begin
        Expect((W.Held as IWithUnmarked).ClockIsNil).ToBeTrue;
      end)

  .Scenario('A marker declared inside a generic type is emitted, and works')
    .Given('a class that declares participation and inherits its marked property from a generic base',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<IStorage, TStorage>;
        W.Services.AddSingleton<IGenericMarked, TGenericMarked>;
      end)
    .When('the container resolves it',
      procedure(W: TInjectionWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Report := W.Provider.GetRequiredService<IGenericMarked>.StorageName; end);
      end)
    .&Then('the property is filled, so the marker written inside the generic type did reach the RTTI',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Outcome).ToEqual(NoFailure);
        Expect(W.Report).ToEqual('storage');
      end)

.Rule('The injected instance keeps alive the reference it receives')

  .Scenario('The service outlives the object it was injected into')
    .Given('a service injected into the property of an object, and another live reference to it',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<IStorage, TStorage>;
        W.Services.AddTransient<ICounted, TCounted>;
        W.Build;
        W.Guest := TGuest.Create;
        TActivatorUtilities.InjectInto(W.Provider, W.Guest);
        W.Held := W.Guest.Counted;
      end)
    .When('that object is destroyed',
      procedure(W: TInjectionWorld)
      begin
        W.Guest.Free;
        W.Guest := nil;
      end)
    .&Then('the service is still alive and answers',
      procedure(W: TInjectionWorld)
      begin
        Expect((W.Held as ICounted).Name).ToEqual('counted');
      end)
    .&And('it has not been destroyed',
      procedure(W: TInjectionWorld)
      begin
        Expect(TCounted.Destroyed).ToEqual(0);
      end)

  .Scenario('The service is destroyed when the last reference is released')
    .Given('that same service, with the object that received it already destroyed',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<IStorage, TStorage>;
        W.Services.AddTransient<ICounted, TCounted>;
        W.Build;
        W.Guest := TGuest.Create;
        TActivatorUtilities.InjectInto(W.Provider, W.Guest);
        W.Held := W.Guest.Counted;
        W.Guest.Free;
        W.Guest := nil;
      end)
    .When('the last remaining reference is released',
      procedure(W: TInjectionWorld)
      begin
        W.Held := nil;
      end)
    .&Then('it is destroyed then, and exactly once',
      procedure(W: TInjectionWorld)
      begin
        Expect(TCounted.Destroyed).ToEqual(1);
      end)

.Rule('The constructor does not see the injected properties')

  .Scenario('The constructor reads its own marked property')
    .Given('a class whose constructor reads a marked property',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<IStorage, TStorage>;
        W.Services.AddSingleton<ICtorReader, TCtorReader>;
      end)
    .When('the container resolves it',
      procedure(W: TInjectionWorld)
      begin
        W.Build;
        W.Report := W.Provider.GetRequiredService<ICtorReader>.StorageName;
      end)
    .&Then('the constructor saw it nil',
      procedure(W: TInjectionWorld)
      begin
        Expect(TCtorReader.StorageInCtorWasNil).ToBeTrue;
      end)
    .&And('and by the end of the resolution it is filled',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Report).ToEqual('storage');
      end)

  .Scenario('Constructor and property in the same class')
    .Given('a class asking for one service by constructor and another by marked property',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<IStorage, TStorage>;
        W.Services.AddSingleton<IClock, TClock>;
        W.Services.AddSingleton<IBoth, TBoth>;
      end)
    .When('the container resolves it',
      procedure(W: TInjectionWorld)
      begin
        W.Build;
        W.Report := W.Provider.GetRequiredService<IBoth>.StorageName;
      end)
    .&Then('the constructor one was available inside the constructor',
      procedure(W: TInjectionWorld)
      begin
        Expect(TBoth.ClockInCtor).ToEqual('clock');
      end)
    .&And('and the property one is available when the resolution ends',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Report).ToEqual('storage');
      end)

.Rule('Two services can need each other')

  .Scenario('A cycle of two services resolves')
    .Given('two Singleton services, each with a marked property of the other type',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<IAlphaSvc, TAlphaSvc>;
        W.Services.AddSingleton<IBetaSvc, TBetaSvc>;
      end)
    .When('either of them is resolved',
      procedure(W: TInjectionWorld)
      begin
        W.Build;
        W.Held := W.Provider.GetRequiredService<IAlphaSvc>;
      end)
    .&Then('the instance comes back with its property filled',
      procedure(W: TInjectionWorld)
      begin
        Expect((W.Held as IAlphaSvc).Peer <> nil).ToBeTrue;
      end)
    .&And('and the other one points back to the first instance, not to a second one',
      procedure(W: TInjectionWorld)
      begin
        var Alpha := W.Held as IAlphaSvc;
        Expect(Alpha.Peer.Peer.Serial).ToEqual(Alpha.Serial);
        Expect(TAlphaSvc.Created).ToEqual(1);
        Expect(TBetaSvc.Created).ToEqual(1);
      end)

  .Scenario('A longer cycle resolves the same way')
    .Given('three Scoped services chained in a cycle by marked properties',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddScoped<IRed, TRed>;
        W.Services.AddScoped<IGreen, TGreen>;
        W.Services.AddScoped<IBlue, TBlue>;
      end)
    .When('one of them is resolved',
      procedure(W: TInjectionWorld)
      begin
        W.Build;
        W.Held := W.Provider.GetRequiredService<IRed>;
      end)
    .&Then('the three of them end up with their property filled',
      procedure(W: TInjectionWorld)
      begin
        var Red := W.Held as IRed;
        Expect(Red.Next <> nil).ToBeTrue;
        Expect(Red.Next.Next <> nil).ToBeTrue;
        Expect(Red.Next.Next.Next <> nil).ToBeTrue;
      end)
    .&And('and they are three instances, not more',
      procedure(W: TInjectionWorld)
      begin
        Expect(TRed.Created + TGreen.Created + TBlue.Created).ToEqual(3);
      end)

  .Scenario('Resolving twice does not duplicate')
    .Given('a cycle of two Singleton services already resolved',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<IAlphaSvc, TAlphaSvc>;
        W.Services.AddSingleton<IBetaSvc, TBetaSvc>;
        W.Build;
        W.Numbers := [W.Provider.GetRequiredService<IAlphaSvc>.Serial];
      end)
    .When('either of them is resolved again',
      procedure(W: TInjectionWorld)
      begin
        W.Numbers := W.Numbers + [W.Provider.GetRequiredService<IAlphaSvc>.Serial];
      end)
    .&Then('the same instance as the first time comes back',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Numbers[1]).ToEqual(W.Numbers[0]);
        Expect(TAlphaSvc.Created).ToEqual(1);
      end)

  .Scenario('A service that asks for itself by property')
    .Given('a Singleton with a marked property of its own service type',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<ITwin, TTwin>;
      end)
    .When('it is resolved',
      procedure(W: TInjectionWorld)
      begin
        W.Build;
        W.Held := W.Provider.GetRequiredService<ITwin>;
      end)
    .&Then('it receives its own instance',
      procedure(W: TInjectionWorld)
      begin
        var Twin := W.Held as ITwin;
        Expect(Twin.Peer <> nil).ToBeTrue;
        Expect(Twin.Peer.Serial).ToEqual(Twin.Serial);
        Expect(TTwin.Created).ToEqual(1);
      end)

.Rule('An instance the container did not build can be wired on request')

  .Scenario('An object built by its owner gets wired')
    .Given('an object built outside the container, with a marked property',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<IStorage, TStorage>;
        W.Services.AddSingleton<ICounted, TCounted>;
        W.Build;
        W.Guest := TGuest.Create;
      end)
    .When('its owner asks for it to be wired against the provider',
      procedure(W: TInjectionWorld)
      begin
        TActivatorUtilities.InjectInto(W.Provider, W.Guest);
      end)
    .&Then('the property holds the registered service',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Guest.Storage.Name).ToEqual('storage');
      end)

  .Scenario('The container does not keep the wired object')
    .Given('an object wired that way, with its services already resolved',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<IStorage, TStorage>;
        W.Services.AddSingleton<ICounted, TCounted>;
        W.Build;
        W.Provider.GetRequiredService<IStorage>;
        W.Provider.GetRequiredService<ICounted>;
        W.Guest := TGuest.Create;
      end)
    .When('what the container owns is read before and after wiring it',
      procedure(W: TInjectionWorld)
      begin
        W.Numbers := [W.Owned];
        TActivatorUtilities.InjectInto(W.Provider, W.Guest);
        W.Numbers := W.Numbers + [W.Owned];
      end)
    .&Then('the number does not change',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Numbers[1]).ToEqual(W.Numbers[0]);
      end)

  .Scenario('Closing the container does not destroy what it does not own')
    .Given('a wired object whose owner keeps it alive',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<IStorage, TStorage>;
        W.Services.AddSingleton<ICounted, TCounted>;
        W.Build;
        W.Guest := TGuest.Create;
        TActivatorUtilities.InjectInto(W.Provider, W.Guest);
      end)
    .When('the container is closed',
      procedure(W: TInjectionWorld)
      begin
        W.Close;
      end)
    .&Then('the object is still alive and its owner can still destroy it',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Guest.Storage.Name).ToEqual('storage');
      end)

  .Scenario('The services it holds outlive the shutdown')
    .Given('a wired object holding a Singleton service of the container',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<IStorage, TStorage>;
        W.Services.AddSingleton<ICounted, TCounted>;
        W.Build;
        W.Guest := TGuest.Create;
        TActivatorUtilities.InjectInto(W.Provider, W.Guest);
      end)
    .When('the container is closed while the object stays alive',
      procedure(W: TInjectionWorld)
      begin
        W.Close;
      end)
    .&Then('the service it holds is alive as long as it holds it',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Guest.Counted.Name).ToEqual('counted');
        Expect(TCounted.Destroyed).ToEqual(0);
      end)
    .&And('and the container reports it owns nothing',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Owned).ToEqual(0);
      end)

.Rule('A dependency can be wired by code, with nothing marked')

  .Scenario('A code wiring fills what nobody marked')
    .Given('a class with no marked property and a code wiring registered for its type',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<IStorage, TStorage>;
        W.Services.AddSingleton<IPlain, TPlain>;
        W.Services.AddInjector<TPlain>(
          procedure(const Instance: TPlain; const Provider: IServiceProvider)
          begin
            Instance.Storage := Provider.GetRequiredService<IStorage>;
          end);
      end)
    .When('the container resolves it',
      procedure(W: TInjectionWorld)
      begin
        W.Build;
        W.Report := W.Provider.GetRequiredService<IPlain>.StorageName;
      end)
    .&Then('the property the wiring assigns is filled',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Report).ToEqual('storage');
      end)

  .Scenario('It runs after the constructor and after the attribute injection')
    .Given('a class with a marked property and a code wiring that reads that same property',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<IStorage, TStorage>;
        W.Services.AddSingleton<IMarkedAndWired, TMarkedAndWired>;
        W.Services.AddInjector<TMarkedAndWired>(
          procedure(const Instance: TMarkedAndWired; const Provider: IServiceProvider)
          begin
            if Instance.Storage = nil then
              Instance.SetNote('<nil>')
            else
              Instance.SetNote(Instance.Storage.Name);
          end);
      end)
    .When('the container resolves it',
      procedure(W: TInjectionWorld)
      begin
        W.Build;
        W.Report := W.Provider.GetRequiredService<IMarkedAndWired>.Note;
      end)
    .&Then('the wiring saw the marked property already filled',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Report).ToEqual('storage');
      end)

  .Scenario('It reaches the descendants of the declared type')
    .Given('a wiring registered for a base class and two implementations descending from it',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<IFirstHeir, TFirstHeir>;
        W.Services.AddSingleton<ISecondHeir, TSecondHeir>;
        W.Services.AddInjector<TWiredBase>(
          procedure(const Instance: TWiredBase; const Provider: IServiceProvider)
          begin
            Instance.Tag := 'wired';
          end);
      end)
    .When('the container resolves both',
      procedure(W: TInjectionWorld)
      begin
        W.Build;
        W.Report := W.Provider.GetRequiredService<IFirstHeir>.Mark + '|' +
                    W.Provider.GetRequiredService<ISecondHeir>.Mark;
      end)
    .&Then('the wiring ran on each of them',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Report).ToEqual('wired|wired');
      end)

  .Scenario('Several wirings run in the order they were registered')
    .Given('two wirings for the same type, each leaving a trace',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<IOrdered, TOrdered>;
        W.Services.AddInjector<TOrdered>(
          procedure(const Instance: TOrdered; const Provider: IServiceProvider)
          begin
            Instance.Append('first');
          end);
        W.Services.AddInjector<TOrdered>(
          procedure(const Instance: TOrdered; const Provider: IServiceProvider)
          begin
            Instance.Append('-second');
          end);
      end)
    .When('the container resolves that type',
      procedure(W: TInjectionWorld)
      begin
        W.Build;
        W.Report := W.Provider.GetRequiredService<IOrdered>.Trace;
      end)
    .&Then('they ran in the order they were registered',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Report).ToEqual('first-second');
      end)

  .Scenario('It does not reach what the container did not build')
    .Given('a wiring registered for a type, and an instance of it built outside the container',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<IStorage, TStorage>;
        W.Services.AddSingleton<ICounted, TCounted>;
        W.Services.AddInjector<TGuest>(
          procedure(const Instance: TGuest; const Provider: IServiceProvider)
          begin
            raise Exception.Create('the code wiring must not run on a foreign instance');
          end);
        W.Build;
        W.Guest := TGuest.Create;
      end)
    .When('its owner wires it on request',
      procedure(W: TInjectionWorld)
      begin
        W.Attempt(procedure begin TActivatorUtilities.InjectInto(W.Provider, W.Guest); end);
      end)
    .&Then('the marked properties are filled',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Guest.Storage.Name).ToEqual('storage');
      end)
    .&And('and the code wiring has not run, because the container did not build that instance',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Outcome).ToEqual(NoFailure);
      end)

  .Scenario('A failure inside the wiring is not swallowed')
    .Given('a code wiring that raises',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<IThrower, TThrower>;
        W.Services.AddInjector<TThrower>(
          procedure(const Instance: TThrower; const Provider: IServiceProvider)
          begin
            raise Exception.Create('wiring refused');
          end);
      end)
    .When('the container resolves that type',
      procedure(W: TInjectionWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Held := W.Provider.GetRequiredService<IThrower>; end);
      end)
    .&Then('the resolution fails propagating that error',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Outcome).ToContain('wiring refused');
      end)
    .&And('and the half wired instance is not handed out',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Held = nil).ToBeTrue;
      end)

.Rule('An instance whose wiring fails is not left published')

  .Scenario('The second resolution builds again')
    .Given('a Singleton whose code wiring always raises',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<IFlakyWired, TFlakyWired>;
        W.Services.AddInjector<TFlakyWired>(
          procedure(const Instance: TFlakyWired; const Provider: IServiceProvider)
          begin
            raise Exception.Create('wiring refused');
          end);
      end)
    .When('it is resolved twice',
      procedure(W: TInjectionWorld)
      begin
        W.Build;
        W.Report := '';
        for var Round := 1 to 2 do
          try
            W.Report := W.Report + W.Provider.GetRequiredService<IFlakyWired>.Describe + '; ';
          except
            on E: Exception do
              W.Report := W.Report + E.Message + '; ';
          end;
      end)
    .&Then('both resolutions fail with that error',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Report).ToEqual('wiring refused; wiring refused; ');
      end)
    .&And('two instances were built, and none is alive',
      procedure(W: TInjectionWorld)
      begin
        Expect(Format('built %d, destroyed %d', [TFlakyWired.Created, TFlakyWired.Destroyed])).ToEqual('built 2, destroyed 2');
      end)

  .Scenario('A failure that does not repeat leaves no trace')
    .Given('a Singleton whose code wiring raises only the first time',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<IFlakyWired, TFlakyWired>;
        W.Services.AddInjector<TFlakyWired>(
          procedure(const Instance: TFlakyWired; const Provider: IServiceProvider)
          begin
            if TFlakyWired.Created = 1 then
              raise Exception.Create('wiring refused');
            Instance.MarkWired;
          end);
      end)
    .When('it is resolved twice',
      procedure(W: TInjectionWorld)
      begin
        W.Build;
        W.Report := '';
        for var Round := 1 to 2 do
          try
            W.Report := W.Report + W.Provider.GetRequiredService<IFlakyWired>.Describe + '; ';
          except
            on E: Exception do
              W.Report := W.Report + E.Message + '; ';
          end;
      end)
    .&Then('the second resolution hands over an instance with its wiring complete, and not the discarded one',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Report).ToEqual('wiring refused; #2 wired; ');
      end)

.Rule('The container wires what it builds from a class, and nothing else')

  .Scenario('What a factory returns is not wired')
    .Given('a factory that returns an instance with an unfilled marked property, and a code wiring registered for its class',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<IStorage, TStorage>;
        W.Services.AddSingleton<IMarkedAndWired>(
          function(Provider: IServiceProvider): IMarkedAndWired
          begin
            Result := TMarkedAndWired.Create;
          end);
        W.Services.AddInjector<TMarkedAndWired>(
          procedure(const Instance: TMarkedAndWired; const Provider: IServiceProvider)
          begin
            Instance.SetNote('wired');
          end);
      end)
    .When('it is resolved',
      procedure(W: TInjectionWorld)
      begin
        W.Build;
        var Svc := W.Provider.GetRequiredService<IMarkedAndWired>;
        var Inst := TMarkedAndWired(Svc as TObject);
        W.Report := Format('storage=%s note=%s', [BoolToStr(Inst.Storage <> nil, True), Svc.Note]);
      end)
    .&Then('the property is still nil, and the code wiring has not run',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Report).ToEqual('storage=False note=');
      end)

  .Scenario('A registered instance is neither wired nor cleared')
    .Given('a registered instance whose marked property its owner filled with an object of its own',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<IStorage, TStorage>;
        var Inst := TMarkedAndWired.Create;
        W.OwnerStorage := TStorage.Create;
        Inst.Storage := W.OwnerStorage;
        W.Held := Inst as IMarkedAndWired;
        W.Services.AddSingleton<IMarkedAndWired>(Inst as IMarkedAndWired);
      end)
    .When('it is resolved, and then the container is closed',
      procedure(W: TInjectionWorld)
      begin
        W.Build;
        var Inst := TMarkedAndWired(W.Provider.GetRequiredService<IMarkedAndWired> as TObject);
        W.Report := 'resolved: ' + BoolToStr(Inst.Storage = W.OwnerStorage, True);
        W.Close;
        W.Report := W.Report + ', closed: ' + BoolToStr(Inst.Storage = W.OwnerStorage, True);
      end)
    .&Then('at both moments the property still points to the object its owner put there',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Report).ToEqual('resolved: True, closed: True');
      end)

.Rule('The original of a decoration reaches the decorator wired')

  .Scenario('The decorator receives the original already wired')
    .Given('a service registered by class with a marked property, and a decorator that reads that property from the original',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<ICounted, TCounted>;
        W.Services.AddSingleton<IDecoratedSvc, TDecoratedOriginal>;
        W.Services.Decorate<IDecoratedSvc>(
          function(const Inner: IDecoratedSvc; const Provider: IServiceProvider): IDecoratedSvc
          begin
            W.Report := 'counted=' + BoolToStr(TDecoratedOriginal(Inner as TObject).Counted <> nil, True);
            Result := TDecoratorSvc.Create(Inner);
          end);
      end)
    .When('the service is resolved',
      procedure(W: TInjectionWorld)
      begin
        W.Build;
        W.Provider.GetRequiredService<IDecoratedSvc>;
      end)
    .&Then('the decorator saw the property filled',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Report).ToEqual('counted=True');
      end)

  .Scenario('Closing destroys the wired original')
    .Given('a decorated Singleton whose original, registered by class, received another Singleton by property',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<ICounted, TCounted>;
        W.Services.AddSingleton<IDecoratedSvc, TDecoratedOriginal>;
        W.Services.Decorate<IDecoratedSvc>(
          function(const Inner: IDecoratedSvc; const Provider: IServiceProvider): IDecoratedSvc
          begin
            W.Report := 'counted=' + BoolToStr(TDecoratedOriginal(Inner as TObject).Counted <> nil, True);
            Result := TDecoratorSvc.Create(Inner);
          end);
        W.Build;
        W.Provider.GetRequiredService<IDecoratedSvc>;
      end)
    .When('the container is closed',
      procedure(W: TInjectionWorld)
      begin
        W.Close;
      end)
    .&Then('the original had received the other Singleton',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Report).ToEqual('counted=True');
      end)
    .&And('the original, the decorator and the other Singleton are destroyed',
      procedure(W: TInjectionWorld)
      begin
        Expect(Format('original %d, decorator %d, counted %d',
          [TDecoratedOriginal.Destroyed, TDecoratorSvc.Destroyed, TCounted.Destroyed]))
          .ToEqual('original 1, decorator 1, counted 1');
      end)

.Rule('A marked property that cannot be filled aborts naming it')

  .Scenario('Nobody registers the requested type')
    .Given('a class with a marked property whose type nobody registered',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<INeedsUnregistered, TNeedsUnregistered>;
      end)
    .When('it is filled',
      procedure(W: TInjectionWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Held := W.Provider.GetRequiredService<INeedsUnregistered>; end);
      end)
    .&Then('it fails naming the property, its type and the class',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Outcome).ToContain('Missing');
        Expect(W.Outcome).ToContain('IUnregistered');
        Expect(W.Outcome).ToContain('TNeedsUnregistered');
      end)

  .Scenario('The marked property is read only')
    .Given('a class with a marked property that has no way to be written',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<IStorage, TStorage>;
        W.Services.AddSingleton<IReadOnlyMark, TReadOnlyMark>;
      end)
    .When('it is filled',
      procedure(W: TInjectionWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Held := W.Provider.GetRequiredService<IReadOnlyMark>; end);
      end)
    .&Then('it says it is marked and not writable, and names the property and the class',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Outcome).ToContain('not writable');
        Expect(W.Outcome).ToContain('Storage');
        Expect(W.Outcome).ToContain('TReadOnlyMark');
      end)

  .Scenario('The marked property is not of an interface type')
    .Given('a class with a marked property whose type is not an interface',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<INonInterfaceMark, TNonInterfaceMark>;
      end)
    .When('it is filled',
      procedure(W: TInjectionWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Held := W.Provider.GetRequiredService<INonInterfaceMark>; end);
      end)
    .&Then('it says so, naming the property and the class',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Outcome).ToContain('not an interface');
        Expect(W.Outcome).ToContain('Retries');
        Expect(W.Outcome).ToContain('TNonInterfaceMark');
      end)

.Rule('Resolution rejects the cycle it cannot break, instead of dying')

  .Scenario('A cycle by constructor fails naming itself')
    .Given('two services registered by class whose constructors ask for each other',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<ICtorLeft, TCtorLeft>;
        W.Services.AddSingleton<ICtorRight, TCtorRight>;
      end)
    .When('one of the two is resolved, with build time validation off',
      procedure(W: TInjectionWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Held := W.Provider.GetRequiredService<ICtorLeft>; end);
      end)
    .&Then('it fails with an error naming the path of the cycle',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Outcome).ToContain('cycle');
        Expect(W.Outcome).ToContain('ICtorLeft');
        Expect(W.Outcome).ToContain('ICtorRight');
      end)
    .&And('and the process is still alive',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Provider <> nil).ToBeTrue;
      end)

  .Scenario('A constructor that resolves its own service fails with the cycle error')
    .Given('a Singleton whose constructor asks the provider for its own service',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<ISelfResolving, TSelfResolving>;
      end)
    .When('it is resolved',
      procedure(W: TInjectionWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Held := W.Provider.GetRequiredService<ISelfResolving>; end);
      end)
    .&Then('it fails with an error naming the cycle, and hands nothing over',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Outcome).ToContain('cycle');
        Expect(W.Outcome).ToContain('ISelfResolving');
        Expect(W.Held = nil).ToBeTrue;
      end)

  .Scenario('A cycle going through a transient fails naming itself')
    .Given('a cycle by marked properties where one of the services is Transient',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<ISingletonPeer, TSingletonPeer>;
        W.Services.AddTransient<ITransientPeer, TTransientPeer>;
      end)
    .When('it is resolved, with build time validation off',
      procedure(W: TInjectionWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Held := W.Provider.GetRequiredService<ITransientPeer>; end);
      end)
    .&Then('it fails naming the path and the Transient service',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Outcome).ToContain('cycle');
        Expect(W.Outcome).ToContain('ITransientPeer');
        Expect(W.Outcome).ToContain('transient');
      end)
    .&And('and the process is still alive',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Provider <> nil).ToBeTrue;
      end)

  .Scenario('A breakable cycle is left alone')
    .Given('a cycle of two Singleton services by marked properties',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<IAlphaSvc, TAlphaSvc>;
        W.Services.AddSingleton<IBetaSvc, TBetaSvc>;
      end)
    .When('it is resolved, with build time validation off',
      procedure(W: TInjectionWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Held := W.Provider.GetRequiredService<IAlphaSvc>; end);
      end)
    .&Then('the instance comes back with its property filled, with no error',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Outcome).ToEqual(NoFailure);
        Expect((W.Held as IAlphaSvc).Peer <> nil).ToBeTrue;
      end)

  .Scenario('A failed resolution does not leave the container half done')
    .Given('an unbreakable cycle that has just failed to resolve',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<ICtorLeft, TCtorLeft>;
        W.Services.AddSingleton<ICtorRight, TCtorRight>;
        W.Services.AddSingleton<IStorage, TStorage>;
        W.Build;
        W.Attempt(procedure begin W.Held := W.Provider.GetRequiredService<ICtorLeft>; end);
      end)
    .When('a healthy service that is not part of the cycle is resolved afterwards',
      procedure(W: TInjectionWorld)
      begin
        W.Attempt(procedure begin W.Report := W.Provider.GetRequiredService<IStorage>.Name; end);
      end)
    .&Then('it is served as usual',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Outcome).ToEqual(NoFailure);
        Expect(W.Report).ToEqual('storage');
      end)

.Rule('Taking part is deduced from marking, so a class that marks nothing does not take part')

  .Scenario('A class with no marked property is resolved and nothing is injected')
    // Es el reverso del primer escenario del fichero, y hace falta escrito: sin el, «no inyecta
    // nada» y «no habia nada que inyectar» dan el mismo verde.
    .Given('a registered class with a writable interface property that nobody marked, and its type registered',
      procedure(W: TInjectionWorld)
      begin
        W.Services.AddSingleton<IStorage, TStorage>;
        W.Services.AddSingleton<INothingMarked, TNothingMarked>;
      end)
    .When('the container resolves it',
      procedure(W: TInjectionWorld)
      begin
        W.Build;
        W.Attempt(procedure begin W.Report := W.Provider.GetRequiredService<INothingMarked>.StorageName; end);
      end)
    .&Then('it resolves with no error, and the property is left as the constructor left it',
      procedure(W: TInjectionWorld)
      begin
        Expect(W.Outcome + ' / ' + W.Report).ToEqual(NoFailure + ' / <nil>');
      end)

;

end.
