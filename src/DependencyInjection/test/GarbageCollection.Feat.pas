unit GarbageCollection.Feat;

interface

uses
  Daf.Extensions.DependencyInjection;

type
  /// <summary>
  /// Servicio de juguete que lleva la cuenta de sus instancias vivas y recuerda la ultima
  /// construida. Es lo unico que distingue "sigue ahi" de "se destruyo bajo los pies del
  /// llamante", que es justo lo que el recolector del ambito puede hacer mal.
  /// </summary>
  ITracked = interface(IInvokable)
    ['{DC5E82F9-FE0F-4582-B8C3-9459725E17B8}']
    function Name: string;
  end;

  /// <summary>
  /// Depende del anterior, para que el ambito tenga mas de una entrada y el bucle del
  /// recolector visite las dos. Es la forma que importa: mientras el titular siga vivo,
  /// el servicio interno no puede desaparecer.
  /// </summary>
  IHolder = interface(IInvokable)
    ['{4FE95CA0-FECC-4689-B36B-14F1238F3EF1}']
    function Inner: ITracked;
  end;

  TTracked = class(TInterfacedObject, ITracked)
  public
    // Publicas a proposito: son el instrumento de medida del spec, no estado del servicio.
    class var Alive: Integer;
    class var LastCreated: TTracked;
    constructor Create;
    destructor Destroy; override;
    function Name: string;
  end;

  THolder = class(TInterfacedObject, IHolder)
  strict private
    FInner: ITracked;
  public
    class var Alive: Integer;
    class var LastCreated: THolder;
    constructor Create(const Tracked: ITracked);
    destructor Destroy; override;
    function Inner: ITracked;
  end;

  /// <summary>
  /// Retiene CINCO referencias propias, y es a proposito. El defecto que este spec vigila
  /// deja el contador uno por debajo del numero de referencias vivas; con una sola
  /// referencia el objeto llegaria a cero dentro del propio recolector y el runner moriria
  /// alli, y un proceso que muere no dice en que asercion fallo. Con cinco, el defecto se
  /// MIDE en vez de tumbar la suite.
  /// </summary>
  TGarbageWorld = class
  public
    Services: IServiceCollection;
    Provider: IServiceProvider;
    T1, T2, T3, T4, T5: ITracked;
    H1, H2, H3, H4, H5: IHolder;
    RefsBefore, RefsAfter: Integer;
    AliveAfter: Integer;
    Closed: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure ResolveTracked;
    procedure ResolveHolder;
    procedure CloseScope;
    function SafeToRelease: Boolean;
  end;

const
  // Las que sujeta el World. Nombrarla evita que el "5" de la asercion y el del teardown
  // se separen el dia que alguien cambie uno de los dos.
  HeldByCaller = 5;

implementation

uses
  System.SysUtils,
  Daf.MiniSpec,
  Daf.DependencyInjection;

{ TTracked }

constructor TTracked.Create;
begin
  inherited Create;
  Inc(Alive);
  LastCreated := Self;
end;

destructor TTracked.Destroy;
begin
  Dec(Alive);
  // Que LastCreated se anule solo es lo que hace seguro leerlo despues: un puntero a
  // memoria liberada se lee igual que uno valido.
  if LastCreated = Self then LastCreated := nil;
  inherited;
end;

function TTracked.Name: string;
begin
  Result := 'tracked';
end;

{ THolder }

constructor THolder.Create(const Tracked: ITracked);
begin
  inherited Create;
  FInner := Tracked;
  Inc(Alive);
  LastCreated := Self;
end;

destructor THolder.Destroy;
begin
  Dec(Alive);
  if LastCreated = Self then LastCreated := nil;
  inherited;
end;

function THolder.Inner: ITracked;
begin
  Result := FInner;
end;

{ TGarbageWorld }

constructor TGarbageWorld.Create;
begin
  inherited;
  TTracked.Alive := 0;
  TTracked.LastCreated := nil;
  THolder.Alive := 0;
  THolder.LastCreated := nil;
  Services := TServiceCollection.Create;
end;

function TGarbageWorld.SafeToRelease: Boolean;
begin
  // El contador tiene que cubrir las referencias que seguimos sujetando. Si va por debajo,
  // miente: soltarlas destruiria el objeto antes de la ultima y la ultima aterrizaria en
  // memoria liberada.
  var Watched: TInterfacedObject := THolder.LastCreated;
  if Watched = nil then
    Watched := TTracked.LastCreated;
  Result := (Watched <> nil) and (Watched.RefCount >= HeldByCaller);
end;

destructor TGarbageWorld.Destroy;
begin
  { Con el defecto presente el contador queda corto, y soltar las cinco referencias mata al
    runner en el teardown: el spec habria medido el fallo y nadie llegaria a leerlo. En ese
    caso se anulan SIN soltar — una fuga en un test que ya esta rojo es mejor que un proceso
    muerto. En verde este camino no se toma y no hay fuga. }
  if SafeToRelease then
  begin
    T1 := nil; T2 := nil; T3 := nil; T4 := nil; T5 := nil;
    H1 := nil; H2 := nil; H3 := nil; H4 := nil; H5 := nil;
  end
  else
  begin
    Pointer(T1) := nil; Pointer(T2) := nil; Pointer(T3) := nil; Pointer(T4) := nil; Pointer(T5) := nil;
    Pointer(H1) := nil; Pointer(H2) := nil; Pointer(H3) := nil; Pointer(H4) := nil; Pointer(H5) := nil;
  end;
  // El escenario ya cerro el ambito: cerrarlo dos veces no es lo que este spec mide.
  if (not Closed) and (Provider <> nil) then
    Provider.ShutDown;
  inherited;
end;

procedure TGarbageWorld.ResolveTracked;
begin
  Services.AddScoped<ITracked, TTracked>;
  Provider := Services.BuildServiceProvider;
  T1 := Provider.GetRequiredService<ITracked>;
  T2 := T1; T3 := T1; T4 := T1; T5 := T1;
end;

procedure TGarbageWorld.ResolveHolder;
begin
  Services.AddScoped<ITracked, TTracked>;
  Services.AddScoped<IHolder, THolder>;
  Provider := Services.BuildServiceProvider;
  H1 := Provider.GetRequiredService<IHolder>;
  H2 := H1; H3 := H1; H4 := H1; H5 := H1;
end;

procedure TGarbageWorld.CloseScope;
begin
  Provider.ShutDown;
  Closed := True;
end;

initialization

Feature('''
Feature Scope shutdown reference accounting @di @lifetime

  As an application author
  I want closing a scope to release exactly the references the scope held
  So a service I am still using is not destroyed under my feet
''')

.UseWorld<TGarbageWorld>

.Rule('The scope gives back what it took, and nothing else')

  .Scenario('Closing the scope releases one reference per service it created')
    .Given('a scoped service resolved, with five references held by the caller',
      procedure(W: TGarbageWorld)
      begin
        W.ResolveTracked;
        W.RefsBefore := TTracked.LastCreated.RefCount;
      end)
    .When('I close the scope', procedure(W: TGarbageWorld)
      begin
        W.CloseScope;
        W.RefsAfter := TTracked.LastCreated.RefCount;
      end)
    .&Then('the count drops by one, which is all the scope ever held',
      procedure(W: TGarbageWorld)
      begin
        // Lo contractual es el delta, no el valor absoluto: el spec no decide cuantas
        // referencias sujeta el llamante, solo cuantas devuelve el ambito.
        Expect(W.RefsBefore - W.RefsAfter).ToEqual(1);
      end)

  .Scenario('Closing the scope does not destroy what a surviving service still holds')
    .Given('a scoped service that depends on another, held by the caller',
      procedure(W: TGarbageWorld)
      begin
        W.ResolveHolder;
      end)
    .When('I close the scope while I keep the holder alive',
      procedure(W: TGarbageWorld)
      begin
        W.CloseScope;
        W.AliveAfter := TTracked.Alive;
      end)
    .&Then('the inner service is still alive, because the holder still references it',
      procedure(W: TGarbageWorld)
      begin
        Expect(W.AliveAfter).ToEqual(1);
      end)
    .&And('the holder can still reach it',
      procedure(W: TGarbageWorld)
      begin
        Expect(W.H1.Inner.Name).ToEqual('tracked');
      end)

;

end.
