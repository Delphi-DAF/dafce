unit Daf.DependencyInjection;

interface

uses
  System.TypInfo,
  System.Rtti,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  Daf.Enumerable,
  Daf.Extensions.DependencyInjection;

type
{$REGION 'from unit System.Rtti implem;'}
  PPVtable = ^PVtable;
  PVtable = ^TVtable;
  TVtable = array [0 .. MaxInt div SizeOf(Pointer) - 1] of Pointer;
{$ENDREGION}

  // Indexado por el PAR (tipo, clave), no por el tipo. Es lo que hace el aislamiento entre los dos
  // espacios estructural en vez de condicional: un registro con clave no esta donde busca una
  // consulta sin clave, asi que no hay nada que cada consulta tenga que acordarse de filtrar.
  TDescriptorsDictionary = TObjectDictionary<TServiceSlot, TServiceDescriptors>;
  TServiceScope = class;

  // Es un singleton. Contiene la lista de todos los Scopes creados asi como
  // el scope para singletons
  TServiceScopeFactory = class(TInterfacedObject, IServiceScopeFactory, IContainerAccess)
  strict private
    FServiceCollection: IServiceCollection;
    FSingletonScope: IServiceScope;
    FShutDonwnDone: Boolean;
    // Apuntados SIN contar referencia, a proposito: sostenerlos es justo lo que impediria que
    // murieran cuando su creador los suelta. La baja la da el destructor del ambito, que es lo
    // unico que ocurre exactamente cuando muere. Alcanzar sin sostener.
    FScopes: TList<TServiceScope>;
    // Guarda FScopes y FShutDonwnDone, y nada mas. Es una HOJA: no se sostiene mientras se pide
    // ningun otro cerrojo, asi que la jerarquia no puede tener ciclos y no hay orden que
    // recordar. Lo que se hace bajo el es tomar una foto --la lista de ambitos, con referencias
    // propias-- y el trabajo va fuera. Separado del de los ambitos porque crear un ambito no
    // debe esperar a que otro hilo construya un servicio.
    // Reentrante porque el desmontaje llega a destructores, y un destructor puede volver a
    // llamar aqui.
    FLock: TCriticalSection;
    function GetSingletonsScope: IServiceScope;
    procedure CheckNotShutDown;
  private
    // La lee la resolucion de cualquier ambito, y la fabrica es lo unico que comparten todos.
    FValidateScopes: Boolean;
    procedure AddToSingletonScope;
    procedure RemoveFromSingletonScope;
    procedure RemoveScope(const Scope: TServiceScope);
    function LiveScopes: TArray<IServiceScope>;
  protected
    procedure Shutdown;
    function OwnedInstanceCount: Integer;
  public
    constructor Create(const ServiceCollection: IServiceCollection);
    destructor Destroy;override;
    function CreateScope: IServiceScope;
    /// <summary>El ambito que devuelve BuildServiceProvider: el proveedor raiz.</summary>
    function CreateRootScope(const ValidateScopes: Boolean): IServiceScope;
    property ServiceCollection: IServiceCollection read FServiceCollection;
    property SingletonsScope: IServiceScope read GetSingletonsScope;
    property ValidateScopes: Boolean read FValidateScopes;
  end;

  IServiceResolver = interface(IInvokable)
    ['{8B521187-079B-44FE-9124-5A1E6771ECCE}']
    function CreateInstance(const Descriptor: TServiceDescriptor; const Provider: IServiceProvider): IInterface;
    function GetOrCreate(const Descriptor: TServiceDescriptor; const Provider: IServiceProvider): IInterface;overload;
    /// <summary>El proveedor que reciben los servicios que construye este ambito.</summary>
    function HandedProvider: IServiceProvider;
  end;

  // Lo que el contenedor entrega a lo que construye: responde como su ambito, pero no lo sostiene,
  // asi que un servicio que guarda su proveedor no mantiene vivo el ambito. Apunta sin contar
  // referencia, como la fabrica, y la baja se la da el destructor del ambito.
  TScopeProvider = class(TInterfacedObject, IServiceProviderImpl)
  private
    FScope: TServiceScope;
    // Guarda FScope: leerlo y tomar referencia no puede cruzarse con la baja, o se tocaria un
    // ambito ya liberado.
    FLock: TLightweightMREW;
    function Live(out Ref: IServiceScope; const ServiceType: PTypeInfo): TServiceScope;
    procedure Detach;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
  public
    constructor Create(const Scope: TServiceScope);
    function CanResolve(const Slot: TServiceSlot): Boolean;
    function TryGet(const Slot: TServiceSlot; out Service): Boolean;
    function TryGetAll(const Slot: TServiceSlot; out Services: IInterfaceList): Boolean;
    function CreateScope: IServiceScope;
  end;

  TServiceScope = class(TInterfacedObject, IServiceScope, IServiceProviderImpl, IServiceResolver,
    IContainerAccess, IServiceInjectors)
  private
    // Apunta a la fabrica SIN sostenerla --sostenerla cerraria el ciclo con FScopes-- y no lleva
    // [Weak]: ese atributo solo actua sobre referencias a interfaz, y sobre una a objeto no hace
    // nada. Quien lo pone a nil es la propia fabrica al morir, que es la unica que sabe cuando.
    FScopeFactory: TServiceScopeFactory;
    [Weak]
    FServiceCollection: IServiceCollectionImpl;
    FProviderObj: TScopeProvider;
    FProvider: IServiceProviderImpl;
    // Ordenado a proposito: CollectGarbage lo recorre de atras hacia
    // delante para liberar en orden inverso al de creacion.
    FServiceInstances : TOrderedDictionary<TServiceIdentity, IInterface>;
    // Las instancias que este ambito cableo, las unicas a las que vacia las propiedades al cerrar.
    // Se lee y se escribe bajo FInstancesLock.
    FWired: TDictionary<Pointer, Byte>;
    // Los dos cerrojos del ambito. La regla: FInstancesLock se toma y se suelta en linea recta
    // --para leer, o para anadir-- y nunca se sostiene mientras se pide FBuildLock ni mientras
    // corre un constructor. Anidar dos tomas suyas, o leer teniendo la escritura, BLOQUEA sin
    // lanzar. FBuildLock si es reentrante, que es lo que pide un grafo: construir A reentra la
    // resolucion para B en el mismo hilo. Orden de los tres: fabrica, construccion, diccionario.
    FInstancesLock: TLightweightMREW;
    FBuildLock: TCriticalSection;
    FIsSingletonsScope: Boolean;
    FIsRoot: Boolean;
    FShutDonwnDone: Boolean;
    function GetServiceIdentity(const Descriptor: TServiceDescriptor): TServiceIdentity;
    function CreateInstance(const Descriptor: TServiceDescriptor; const Provider: IServiceProvider): IInterface;
    function GetOrCreate(const Descriptor: TServiceDescriptor; const Provider: IServiceProvider): IInterface;overload;
    function GetImplementor(const Descriptor: TServiceDescriptor): IInterface;
    function TryGet(const Slot: TServiceSlot; out Service): Boolean;
    function TryGetAll(const Slot: TServiceSlot; out Services: IInterfaceList): Boolean;
    function TryGetDescriptors(const Slot: TServiceSlot; out Descriptors: TServiceDescriptors;
      out MatchedOpen, MatchedAnyKey: Boolean): Boolean;
    function DescriptorFor(const Slot: TServiceSlot; const Descriptor: TServiceDescriptor;
      const MatchedOpen, MatchedAnyKey: Boolean): TServiceDescriptor;
    function GetSingletonsScope: IServiceResolver;
    procedure AddServiceInstance(const Descriptor: TServiceDescriptor; const Service: IInterface);
    function TryGetInstance(const Identity: TServiceIdentity; out Service: IInterface): Boolean;
    procedure RemoveFirstInstance;
    function InstanceCount: Integer;
    function CanResolve(const Slot: TServiceSlot): Boolean;
    procedure CheckNotShutDown(const ServiceType: PTypeInfo);
    procedure RaiseShutDown(const ServiceType: PTypeInfo);
    procedure RaiseScopedFromRoot(const Descriptor: TServiceDescriptor);
    procedure CollectGarbage;
    function InjectorsFor(const AClass: TClass): TArray<TServiceInjector>;
    class function TryAcquire(const Scope: TServiceScope; out Ref: IServiceScope): Boolean; static;
  protected
    property IsSingletonsScope: Boolean read FIsSingletonsScope;
    property ScopeFactory: TServiceScopeFactory read FScopeFactory;
    // Clausula de resolucion porque Shutdown ya esta cogido en esta clase y significa otra cosa:
    // el de abajo recoge la basura de ESTE ambito, y este de aqui cierra el contenedor entero.
    procedure IContainerAccess.Shutdown = ShutdownContainer;
    procedure ShutdownContainer;
    function OwnedInstanceCount: Integer;
  public
    constructor Create(const Factory: TServiceScopeFactory; const SingletonsScope: Boolean = False);
    destructor Destroy; override;
    procedure Shutdown;
    function CreateScope: IServiceScope;
    function ServiceProvider: IServiceProvider;
    function HandedProvider: IServiceProvider;
    property ServiceCollection: IServiceCollectionImpl read FServiceCollection;
    property SingletonsScope: IServiceResolver read GetSingletonsScope;
  end;

  TServiceCollection = class(TInterfacedObject, IServiceCollectionImpl)
  strict private
    // Componer es de un solo hilo, y eso es el contrato, no una carencia: la coleccion se compone
    // en un hilo, se congela al construir el proveedor, y desde entonces solo se lee. Por eso
    // FEntries no lleva cerrojo -- la garantia viene de arriba, no del codigo -- y por eso
    // FIsFrozen se puede escribir una vez y leer despues sin proteger: la escritura ocurre antes
    // de que exista el segundo hilo. Lo unico que se escribe al RESOLVER es FOpenMatches, y por
    // eso es lo unico que va con cerrojo.
    FEntries: TDescriptorsDictionary;
    // Estado de la coleccion, no del proceso: componer dos veces en un mismo proceso
    // tiene que dar el mismo resultado, y con una global de unit no lo daba.
    FVisited: TDictionary<string, Boolean>;
    // Que registro sin fijar sirve a cada cierre, incluido el "ninguno" -- por eso guarda nil
    // en vez de no guardar nada. Sin esto, cada resolucion de un cierre no registrado
    // recorreria todas las claves de la coleccion. Se vacia en cuanto la coleccion cambia:
    // un "ninguno" cacheado antes de un registro posterior seria una respuesta caducada.
    FOpenMatches: TDictionary<PTypeInfo, PTypeInfo>;
    // FOpenMatches es el UNICO estado de la coleccion que se escribe al RESOLVER, y por eso es
    // el unico que lleva cerrojo: lo demas se compone en un hilo, se congela, y luego solo se
    // lee -- que es el contrato, no una casualidad. Sin este cerrojo, dos hilos resolviendo
    // cierres distintos por primera vez escriben el mismo TDictionary a la vez.
    // Toda lectura y toda escritura de FOpenMatches pasa por aqui, sin excepciones que recordar.
    FOpenMatchesLock: TLightweightMREW;
    // Politica de la coleccion, con el mismo contrato que FEntries: se escribe al componer, se
    // congela, y desde ahi solo se lee. Ordenada porque varios cableados del mismo tipo corren
    // en el orden en que se registraron.
    FInjectors: TList<TPair<TClass, TServiceInjector>>;
    FIsFrozen: Boolean;
    function DescribeUnresolvable(const Descriptor: TServiceDescriptor; const Failed: TRttiParameter): string;
    procedure CollectCapturedScopes(const Descriptor: TServiceDescriptor; const Ctor: TRttiMethod;
      var Errors: TArray<string>);
    procedure CollectPropertyFailures(const Descriptor: TServiceDescriptor; const Provider: IServiceProvider;
      var Errors: TArray<string>);
    procedure CollectCapturedScopesByProperty(const Descriptor: TServiceDescriptor; var Errors: TArray<string>);
    procedure Validate(const Provider: IServiceProvider; const Options: TServiceProviderOptions);
    procedure ClearOpenMatches;
  protected
    function CreateScopeFactory: IServiceScopeFactory;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CheckNotFrozen;
    function GetDescriptors(const Slot: TServiceSlot): TServiceDescriptors;
    function SetDescriptors(const Slot: TServiceSlot; const NewDescriptors: TArray<TServiceDescriptor>): IServiceCollectionImpl;
    function RemoveAll(const Slot: TServiceSlot): IServiceCollectionImpl;
    function TryFindOpenGeneric(const ClosedType: PTypeInfo; out OpenType: PTypeInfo): Boolean;
    function TryMarkVisited(const Category, Key: string): Boolean;
    function AddInjector(const AClass: TClass; const Injector: TServiceInjector): IServiceCollectionImpl;
    function InjectorsFor(const AClass: TClass): TArray<TServiceInjector>;
    function GetCount: Integer;
    function Add<ServiceType: IInterface>(const LifeTime: TServiceLifeTime; const Factory: TServiceFactory): IServiceCollectionImpl;overload;
    function Add(const Slot: TServiceSlot; const LifeTime: TServiceLifeTime; const Factory: TServiceFactory;
      const ImplClass: TClass = nil; const RequestedFactory: TServiceFactoryEx = nil;
      const KeyedFactory: TServiceKeyedFactory = nil): IServiceCollectionImpl;overload;
    function BuildServiceProvider: IServiceProviderImpl;overload;
    function BuildServiceProvider(const Options: TServiceProviderOptions): IServiceProviderImpl;overload;
    function Contains<T: IInterface>: Boolean;overload;
    function Contains(const Slot: TServiceSlot): Boolean;overload;
    property Descriptors[const Slot: TServiceSlot]: TServiceDescriptors read GetDescriptors;default;
  end;


implementation

uses
  System.SysUtils,
  System.Generics.Defaults,
  Daf.Types,
  Daf.Rtti,
  Daf.DependencyInjection.ActivatorUtilities,
  Daf.DependencyInjection.OpenGenerics;

type
  PServiceDescriptor = ^TServiceDescriptor;

type
  PCreationFrame = ^TCreationFrame;
  /// <summary>
  ///   Un eslabon de la cadena de construcciones en curso de este hilo. Vive en la pila del
  ///   metodo que construye, asi que vale exactamente mientras esa construccion esta en curso y
  ///   no hay nada que liberar.
  /// </summary>
  TCreationFrame = record
    Descriptor: PServiceDescriptor;
    Identity: TServiceIdentity;
    Outer: PCreationFrame;
  end;

threadvar
  /// <summary>
  ///   Lo que este hilo esta construyendo ahora mismo, del ultimo al primero. Contesta dos
  ///   preguntas: si hay un singleton en curso --lo que convierte en captura pedir un scoped-- y
  ///   si una identidad ya esta en la cadena, que es un ciclo que la resolucion no puede romper.
  ///   Es POR HILO y no por ambito porque compartir un ambito entre hilos esta soportado: con la
  ///   cadena en el ambito, la construccion en curso de un hilo le parece un ciclo al otro, y la
  ///   guarda rechazaria grafos sanos de forma intermitente. Un rechazo que no se reproduce es
  ///   peor que ninguno.
  ///   Un puntero y no el registro: un threadvar de tipo gestionado no se finaliza al terminar
  ///   el hilo.
  /// </summary>
  GCreating: PCreationFrame;

{ TServiceCollection }

constructor TServiceCollection.Create;
begin
  inherited Create;
  // Comparador explicito: la clave del indice lleva un TValue, que el comparador binario por
  // defecto no sabe comparar -- y con el par sin comparar, dos claves iguales serian dos entradas.
  FEntries := TDescriptorsDictionary.Create([doOwnsValues], TServiceSlot.Comparer);
  FVisited := TDictionary<string, Boolean>.Create;
  FOpenMatches := TDictionary<PTypeInfo, PTypeInfo>.Create;
  FInjectors := TList<TPair<TClass, TServiceInjector>>.Create;
end;

destructor TServiceCollection.Destroy;
begin
  FInjectors.Free;
  FOpenMatches.Free;
  FVisited.Free;
  FEntries.Free;
  inherited;
end;

function TServiceCollection.AddInjector(const AClass: TClass; const Injector: TServiceInjector): IServiceCollectionImpl;
begin
  CheckNotFrozen;
  FInjectors.Add(TPair<TClass, TServiceInjector>.Create(AClass, Injector));
  Result := Self;
end;

function TServiceCollection.InjectorsFor(const AClass: TClass): TArray<TServiceInjector>;
begin
  Result := [];
  if AClass = nil then Exit;
  // Descendientes incluidos: es politica de un tipo, asi que declararla para una clase base
  // alcanza a todas sus hijas.
  for var Entry in FInjectors do
    if AClass.InheritsFrom(Entry.Key) then
      Result := Result + [Entry.Value];
end;

function TServiceCollection.TryFindOpenGeneric(const ClosedType: PTypeInfo; out OpenType: PTypeInfo): Boolean;
begin
  OpenType := nil;
  if (ClosedType = nil) or not _T.IsGeneric(ClosedType) then Exit(False);

  FOpenMatchesLock.BeginRead;
  try
    if FOpenMatches.TryGetValue(ClosedType, OpenType) then Exit(OpenType <> nil);
  finally
    FOpenMatchesLock.EndRead;
  end;

  // El emparejamiento se calcula FUERA del cerrojo: solo lee FEntries, que a estas alturas esta
  // congelada. Dos hilos pueden calcular el mismo y no importa -- el calculo no tiene efectos y
  // la respuesta es la misma.
  //
  // Un registro sin fijar no se empareja consigo mismo: IX<_> es un tipo cerrado legitimo y
  // se resuelve por la via directa, como cualquier otro.
  //
  // Y solo mira el espacio SIN clave, que es donde puede haber un registro sin fijar: registrar
  // uno CON clave se rechaza al registrar, asi que una consulta con clave no tiene aqui nada que
  // encontrar -- y encontrarlo seria el cruce entre los dos espacios.
  if not TOpenGeneric.IsOpen(ClosedType) then
    for var Entry in FEntries do
      if (not Entry.Key.IsKeyed) and TOpenGeneric.IsOpen(Entry.Key.ServiceType) and
         TOpenGeneric.SameForm(Entry.Key.ServiceType, ClosedType) then
      begin
        OpenType := Entry.Key.ServiceType;
        Break;
      end;

  FOpenMatchesLock.BeginWrite;
  try
    // AddOrSetValue y no Add, y aqui eso importa: dos hilos que llegaron a la vez guardan la
    // misma respuesta, y con Add el segundo chocaria con el primero.
    FOpenMatches.AddOrSetValue(ClosedType, OpenType);
  finally
    FOpenMatchesLock.EndWrite;
  end;
  Result := OpenType <> nil;
end;

procedure TServiceCollection.ClearOpenMatches;
begin
  FOpenMatchesLock.BeginWrite;
  try
    FOpenMatches.Clear;
  finally
    FOpenMatchesLock.EndWrite;
  end;
end;

function TServiceCollection.TryMarkVisited(const Category, Key: string): Boolean;
begin
  var Id := Category + '|' + Key;
  Result := not FVisited.ContainsKey(Id);
  if Result then
    FVisited.Add(Id, True);
end;

function TServiceCollection.Contains<T>: Boolean;
begin
  Result := Contains(TServiceSlot.Create(TypeInfo(T)));
end;

procedure TServiceCollection.CheckNotFrozen;
begin
  if FIsFrozen then
    raise Exception.Create('This ServiceCollection is frozen and can no longer be modified.');
end;

function TServiceCollection.Contains(const Slot: TServiceSlot): Boolean;
begin
  Result := FEntries.ContainsKey(Slot);
end;

function TServiceCollection.CreateScopeFactory: IServiceScopeFactory;
begin
  Result := TServiceScopeFactory.Create(Self);
end;

function TServiceCollection.BuildServiceProvider: IServiceProviderImpl;
begin
  Result := BuildServiceProvider(TServiceProviderOptions.None);
end;

function TServiceCollection.BuildServiceProvider(const Options: TServiceProviderOptions): IServiceProviderImpl;
begin
 if FIsFrozen then
    raise Exception.Create('BuildServiceProvider can only be called once per ServiceCollection.');
  var Factory := CreateScopeFactory;
  var Scope := TServiceScopeFactory(Factory as TObject).CreateRootScope(Options.ValidateScopes);
  var Provider: IServiceProvider := Scope.ServiceProvider;
  try
    Validate(Provider, Options);
  except
    // Un grafo invalido no debe dejar atras un contenedor a medio montar.
    (Factory as IContainerAccess).Shutdown;
    raise;
  end;
  Result := Provider;
  FIsFrozen := True;
end;

function LifeTimeName(const LifeTime: TServiceLifeTime): string;
begin
  Result := LowerCase(TEnum.ToString<TServiceLifeTime>(LifeTime));
end;

// Nombre de un par (tipo, clave) para un mensaje. Sin clave da exactamente el texto de siempre.
function SlotName(const Slot: TServiceSlot): string;
begin
  Result := ServiceRequestName(Slot.ServiceType, Slot.Key);
end;

type
  // El grafo de dependencias tal como lo ve la validacion: un nodo por REGISTRO --por par (tipo,
  // clave), no por tipo-- y una arista por cada dependencia que se pueda seguir. La arista
  // recuerda POR DONDE entra, porque eso es lo que decide si un ciclo se puede romper.
  TDepEdge = record
    Target: TServiceSlot;
    ViaConstructor: Boolean;
    Member: string;
  end;

  TDepNode = record
    Slot: TServiceSlot;
    LifeTime: TServiceLifeTime;
    ImplName: string;
    Edges: TArray<TDepEdge>;
  end;

function UnbreakableCycles(const Entries: TDescriptorsDictionary; const Provider: IServiceProvider): TArray<string>;
var
  Nodes: TArray<TDepNode>;
  IndexOf: TDictionary<TServiceSlot, Integer>;
  Color: TArray<Byte>;
  Stack: TArray<Integer>;
  Edges: TArray<TDepEdge>;
  Seen: TDictionary<string, Boolean>;
  Found: TArray<string>;

  procedure BuildGraph;
  begin
    for var Entry in Entries do
    begin
      if Entry.Value.Count = 0 then Continue;
      // El ultimo descriptor es el que resuelve TryGet, asi que es su grafo el que se recorre.
      var Descriptor := Entry.Value.Last;
      var Node: TDepNode;
      Node.Slot := Entry.Key;
      Node.LifeTime := Descriptor.LifeTime;
      Node.Edges := [];
      Node.ImplName := '';
      if Descriptor.ImplementationClass <> nil then
        Node.ImplName := Descriptor.ImplementationClass.ClassName;
      IndexOf.Add(Entry.Key, Length(Nodes));
      Nodes := Nodes + [Node];
    end;

    for var Idx := 0 to High(Nodes) do
    begin
      var Descriptor := Entries[Nodes[Idx].Slot].Last;
      var AClass := Descriptor.ImplementationClass;
      // Una factory es opaca: no aporta aristas, y por eso un ciclo que pase por ella no se ve.
      if AClass = nil then Continue;

      var Ctor: TRttiMethod;
      var Failed: TRttiParameter;
      // Dos constructores que compiten ya los informa Validate; aqui la clase no aporta aristas.
      var Found: Boolean;
      try
        Found := TActivatorUtilities.TryFindConstructor(Provider, AClass, ccContainer, Ctor, Failed);
      except
        on EServiceProviderError do
          Found := False;
      end;
      if Found and (Ctor <> nil) then
        for var Param in Ctor.GetParameters do
        begin
          if Param.ParamType = nil then Continue;
          if Param.ParamType.Handle.Kind <> tkInterface then Continue;
          var Edge: TDepEdge;
          // La arista apunta al PAR, no al tipo: una dependencia con clave sale de un registro y
          // llega a otro, y con el tipo a secas dos claves del mismo contrato serian un nodo.
          Edge.Target := TServiceSlot.Create(Param.ParamType.Handle, TActivatorUtilities.KeyOfParameter(Param));
          Edge.ViaConstructor := True;
          Edge.Member := Param.Name;
          Nodes[Idx].Edges := Nodes[Idx].Edges + [Edge];
        end;

      // Sin guarda de participacion: una clase que no marca nada devuelve una lista vacia.
      for var Entry in TActivatorUtilities.InjectableProperties(AClass) do
      begin
        var Prop := Entry.Prop;
        if (Prop.PropertyType = nil) or (Prop.PropertyType.TypeKind <> tkInterface) then Continue;
        var Edge: TDepEdge;
        Edge.Target := TServiceSlot.Create(Prop.PropertyType.Handle, Entry.Key);
        Edge.ViaConstructor := False;
        Edge.Member := Prop.Name;
        Nodes[Idx].Edges := Nodes[Idx].Edges + [Edge];
      end;
    end;
  end;

  procedure Report(const Target: Integer);
  begin
    var First := -1;
    for var Pos := 0 to High(Stack) do
      if Stack[Pos] = Target then
      begin
        First := Pos;
        Break;
      end;
    if First < 0 then Exit;

    var Path := '';
    var Key := '';
    var Reason := '';
    for var Pos := First to High(Stack) do
    begin
      var Node := Nodes[Stack[Pos]];
      if Path <> '' then Path := Path + ' -> ';
      Path := Path + Format('%s (%s)', [SlotName(Node.Slot), LifeTimeName(Node.LifeTime)]);
      Key := Key + Format('%p|%s|', [Pointer(Node.Slot.ServiceType), ServiceKey.Text(Node.Slot.Key)]);
      if (Reason = '') and Edges[Pos].ViaConstructor then
        Reason := Format('%s needs %s through constructor parameter "%s", and a constructor cannot run before its argument exists',
          [Node.ImplName, SlotName(Edges[Pos].Target), Edges[Pos].Member]);
      if (Reason = '') and (Node.LifeTime = Transient) then
        Reason := Format('%s is transient and the container does not retain transients, so nothing can undo the cycle',
          [SlotName(Node.Slot)]);
    end;
    Path := Path + ' -> ' + Format('%s (%s)', [SlotName(Nodes[Target].Slot),
      LifeTimeName(Nodes[Target].LifeTime)]);
    if Reason = '' then Exit;
    if Seen.ContainsKey(Key) then Exit;
    Seen.Add(Key, True);
    Found := Found + [Format('dependency cycle the container cannot break: %s; %s', [Path, Reason])];
  end;

  procedure Visit(const Idx: Integer);
  begin
    Color[Idx] := 1;
    Stack := Stack + [Idx];
    SetLength(Edges, Length(Stack));
    for var Edge in Nodes[Idx].Edges do
    begin
      var Target: Integer;
      if not IndexOf.TryGetValue(Edge.Target, Target) then Continue;
      Edges[High(Edges)] := Edge;
      if Color[Target] = 1 then
        Report(Target)
      else if Color[Target] = 0 then
        Visit(Target);
    end;
    SetLength(Edges, Length(Stack) - 1);
    SetLength(Stack, Length(Stack) - 1);
    Color[Idx] := 2;
  end;

begin
  Result := [];
  Nodes := [];
  Found := [];
  IndexOf := TDictionary<TServiceSlot, Integer>.Create(TServiceSlot.Comparer);
  Seen := TDictionary<string, Boolean>.Create;
  try
    BuildGraph;
    SetLength(Color, Length(Nodes));
    for var Idx := 0 to High(Nodes) do
      if Color[Idx] = 0 then
      begin
        Stack := [];
        Edges := [];
        Visit(Idx);
      end;
    Result := Found;
  finally
    Seen.Free;
    IndexOf.Free;
  end;
end;

procedure TServiceCollection.Validate(const Provider: IServiceProvider; const Options: TServiceProviderOptions);
begin
  if not (Options.ValidateOnBuild or Options.ValidateScopes) then Exit;

  var Errors: TArray<string> := [];
  for var Entry in FEntries do
    for var Descriptor in Entry.Value do
    begin
      // Los registros por factory son opacos: validarlos exigiria ejecutar la lambda, y
      // ejecutarla al construir tendria efectos. Se dice, y no se finge que se cubren.
      if Descriptor.ImplementationClass = nil then Continue;

      var Ctor: TRttiMethod;
      var Failed: TRttiParameter;
      var Found: Boolean;
      try
        Found := TActivatorUtilities.TryFindConstructor(Provider, Descriptor.ImplementationClass, ccContainer, Ctor, Failed);
      except
        on E: EServiceProviderError do
        begin
          if Options.ValidateOnBuild then
            Errors := Errors + [Format('%s: %s', [Descriptor.ServiceName, E.Message])];
          Continue;
        end;
      end;
      if not Found then
      begin
        if Options.ValidateOnBuild then
          Errors := Errors + [DescribeUnresolvable(Descriptor, Failed)];
        Continue;
      end;

      // Una clase cuya unica dependencia insatisfecha esta en una propiedad marcada tiene un
      // constructor que el contenedor satisface sin mas: mirar solo constructores la declara
      // sana y la entrega con la propiedad en nil.
      if Options.ValidateOnBuild then
        CollectPropertyFailures(Descriptor, Provider, Errors);

      if Options.ValidateScopes and (Descriptor.LifeTime = Singleton) then
      begin
        if Ctor <> nil then
          CollectCapturedScopes(Descriptor, Ctor, Errors);
        CollectCapturedScopesByProperty(Descriptor, Errors);
      end;
    end;

  if Options.ValidateOnBuild then
    Errors := Errors + UnbreakableCycles(FEntries, Provider);

  if Length(Errors) = 0 then Exit;
  // Todos los fallos juntos: un arranque mal cableado suele tener varios, y descubrirlos
  // de uno en uno cuesta un ciclo de compilacion por cada uno.
  raise EServiceProviderError.Create('Service provider validation failed:' + sLineBreak +
    string.Join(sLineBreak, Errors));
end;

function TServiceCollection.DescribeUnresolvable(const Descriptor: TServiceDescriptor;
  const Failed: TRttiParameter): string;
begin
  if Failed = nil then
    Exit(Format('%s: %s has no public constructor the container can call',
      [Descriptor.ServiceName, Descriptor.ImplementationClass.ClassName]));

  // El texto del parametro lo compone el activador, para que la validacion y la construccion
  // digan lo mismo: el tipo, la clave que el parametro pide, y su nombre. El tipo del parametro
  // tampoco tiene por que ser nombrable.
  Result := Format('%s: %s', [Descriptor.ServiceName,
    TActivatorUtilities.DescribeUnresolvedParameter(Descriptor.ImplementationClass, Failed)]);
end;

procedure TServiceCollection.CollectCapturedScopes(const Descriptor: TServiceDescriptor; const Ctor: TRttiMethod;
  var Errors: TArray<string>);
begin
  for var Param in Ctor.GetParameters do
  begin
    if Param.ParamType = nil then Continue;
    var PInfo := Param.ParamType.Handle;
    if PInfo.Kind <> tkInterface then Continue;

    var Dependencies: TServiceDescriptors;
    // Por el PAR: un Singleton con clave que captura un Scoped con clave es la misma captura, y
    // buscar por el tipo a secas no encontraria ninguno de los dos.
    if not FEntries.TryGetValue(TServiceSlot.Create(PInfo, TActivatorUtilities.KeyOfParameter(Param)),
      Dependencies) then Continue;
    if Dependencies.Count = 0 then Continue;
    // El ultimo descriptor es el que resuelve TryGet, asi que es su ciclo de vida el que cuenta.
    if Dependencies.Last.LifeTime <> Scoped then Continue;

    Errors := Errors + [Format('captured dependency: singleton %s (%s) depends on scoped %s, via parameter "%s"',
      [Descriptor.ServiceName, Descriptor.ImplementationClass.ClassName, Dependencies.Last.ServiceName, Param.Name])];
  end;
end;

procedure TServiceCollection.CollectPropertyFailures(const Descriptor: TServiceDescriptor;
  const Provider: IServiceProvider; var Errors: TArray<string>);
begin
  var AClass := Descriptor.ImplementationClass;
  var Props := TActivatorUtilities.InjectableProperties(AClass);

  for var Entry in Props do
  begin
    var Prop := Entry.Prop;
    if Entry.KeyError <> '' then
    begin
      Errors := Errors + [Format('%s: %s.%s carries an unusable key: %s',
        [Descriptor.ServiceName, AClass.ClassName, Prop.Name, Entry.KeyError])];
      Continue;
    end;
    if not Prop.IsWritable then
    begin
      Errors := Errors + [Format('%s: %s.%s is marked with DafInject but is not writable',
        [Descriptor.ServiceName, AClass.ClassName, Prop.Name])];
      Continue;
    end;
    if (Prop.PropertyType = nil) or (Prop.PropertyType.TypeKind <> tkInterface) then
    begin
      Errors := Errors + [Format('%s: %s.%s is marked with DafInject but its type is not an interface',
        [Descriptor.ServiceName, AClass.ClassName, Prop.Name])];
      Continue;
    end;
    var PInfo := Prop.PropertyType.Handle;
    if not Provider.CanResolveKeyed(PInfo, Entry.Key) then
      Errors := Errors + [Format('%s: %s needs %s for property "%s", and nothing registers it',
        [Descriptor.ServiceName, AClass.ClassName, ServiceRequestName(PInfo, Entry.Key), Prop.Name])];
  end;
end;

procedure TServiceCollection.CollectCapturedScopesByProperty(const Descriptor: TServiceDescriptor;
  var Errors: TArray<string>);
begin
  var AClass := Descriptor.ImplementationClass;
  for var Entry in TActivatorUtilities.InjectableProperties(AClass) do
  begin
    var Prop := Entry.Prop;
    if (Prop.PropertyType = nil) or (Prop.PropertyType.TypeKind <> tkInterface) then Continue;

    var Dependencies: TServiceDescriptors;
    if not FEntries.TryGetValue(TServiceSlot.Create(Prop.PropertyType.Handle, Entry.Key), Dependencies) then Continue;
    if Dependencies.Count = 0 then Continue;
    if Dependencies.Last.LifeTime <> Scoped then Continue;

    // La captura no depende de por donde entre la dependencia: un singleton que sostiene un
    // scoped lo sostiene igual desde una propiedad que desde un campo del constructor.
    Errors := Errors + [Format('captured dependency: singleton %s (%s) depends on scoped %s, via property "%s"',
      [Descriptor.ServiceName, AClass.ClassName, Dependencies.Last.ServiceName, Prop.Name])];
  end;
end;

function TServiceCollection.GetCount: Integer;
begin
  Result := FEntries.Count;
end;

function TServiceCollection.GetDescriptors(const Slot: TServiceSlot): TServiceDescriptors;
begin
  Result := FEntries[Slot];
end;

function TServiceCollection.SetDescriptors(const Slot: TServiceSlot;
  const NewDescriptors: TArray<TServiceDescriptor>): IServiceCollectionImpl;
begin
  CheckNotFrozen;
  Result := Self;
  ClearOpenMatches;
  FEntries.Remove(Slot);
  if Length(NewDescriptors) = 0 then Exit;

  var List := TServiceDescriptors.Create;
  FEntries.AddOrSetValue(Slot, List);
  List.AddRange(NewDescriptors);
end;

function TServiceCollection.RemoveAll(const Slot: TServiceSlot): IServiceCollectionImpl;
begin
  CheckNotFrozen;
  Result := Self;
  ClearOpenMatches;
  // Se borra la entrada entera, no se vacia la lista: si quedara una clave con lista vacia,
  // Contains seguiria diciendo que si y TryAdd no repondria el registro.
  // Y se borra SOLO la del par que se nombro: quitar los registros sin clave no toca los que
  // tienen clave, ni al reves. Es la interpretacion que no toca lo que el llamante no nombro.
  FEntries.Remove(Slot);
end;

function TServiceCollection.Add<ServiceType>(const LifeTime: TServiceLifeTime;
  const Factory: TServiceFactory): IServiceCollectionImpl;
begin
  Result := Add(TServiceSlot.Create(TypeInfo(ServiceType)), LifeTime, Factory);
end;

function TServiceCollection.Add(const Slot: TServiceSlot; const LifeTime: TServiceLifeTime;
  const Factory: TServiceFactory; const ImplClass: TClass; const RequestedFactory: TServiceFactoryEx;
  const KeyedFactory: TServiceKeyedFactory): IServiceCollectionImpl;
begin
  CheckNotFrozen;

  Assert(Assigned(Factory), 'ServiceFactory can''t be empty');
  Result := Self;
  ClearOpenMatches;
  var
    Descriptors: TServiceDescriptors;

  if not FEntries.TryGetValue(Slot, Descriptors) then
  begin
    Descriptors := TServiceDescriptors.Create;
    FEntries.AddOrSetValue(Slot, Descriptors);
  end;
  var Descriptor := TServiceDescriptor.Create(Factory, LifeTime, Slot, ImplClass, RequestedFactory, KeyedFactory);
  Descriptors.Add(Descriptor);
end;

{ TServiceScopeFactory }

constructor TServiceScopeFactory.Create(const ServiceCollection: IServiceCollection);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FServiceCollection := ServiceCollection;
  // Note FSingletonScope is not in FScopes !!
  FSingletonScope := TServiceScope.Create(Self, True);
  FScopes := TList<TServiceScope>.Create;
  AddToSingletonScope;
end;

destructor TServiceScopeFactory.Destroy;
begin
  // Los ambitos que sigan vivos apuntan aqui sin sostener, asi que al morir la fabrica hay que
  // dejarles el puntero en nil: su destructor se da de baja aqui, y entrar en memoria liberada
  // es un Access Violation leyendo el cerrojo. Los que ya murieron se dieron de baja solos, asi
  // que la lista es exactamente los que quedan.
  FLock.Acquire;
  try
    for var Scope in FScopes do
      Scope.FScopeFactory := nil;
  finally
    FLock.Release;
  end;
  FreeAndNil(FScopes);
  FreeAndNil(FLock);
  inherited;
end;

procedure TServiceScopeFactory.AddToSingletonScope;
begin
  // Force Self avaliable as Singleton
  ServiceCollection.AddSingleton<IServiceScopeFactory>(Self);
  FSingletonScope.ServiceProvider.GetRequiredService<IServiceScopeFactory>;
end;

procedure TServiceScopeFactory.RemoveFromSingletonScope;
begin
  TServiceScope(FSingletonScope).RemoveFirstInstance;
end;

procedure TServiceScopeFactory.RemoveScope(const Scope: TServiceScope);
begin
  FLock.Acquire;
  try
    // Que el ambito no este en la lista no es un error: el de singletons nunca estuvo, y uno
    // creado con el contenedor ya cerrado se descarta sin llegar a apuntarse.
    if Assigned(FScopes) then
      FScopes.Remove(Scope);
  finally
    FLock.Release;
  end;
end;

function TServiceScopeFactory.LiveScopes: TArray<IServiceScope>;
begin
  // Una referencia propia por cada ambito que SIGA vivo. El que ya esta muriendo la rechaza, y
  // no hace falta desmontarlo desde aqui: su destructor llama a su propio Shutdown.
  // Se llama con FLock tomado, y no toma ningun otro: ninguno de los contadores que toca puede
  // llegar a cero --TryAcquire lo sube a dos antes de compensar-- asi que no corre ningun
  // destructor y el cerrojo de la fabrica sigue siendo una hoja.
  for var Scope in FScopes do
  begin
    var Ref: IServiceScope;
    if TServiceScope.TryAcquire(Scope, Ref) then
      Result := Result + [Ref];
  end;
end;

procedure TServiceScopeFactory.Shutdown;
begin
  // Reclamar el cierre y tomar la foto de los ambitos vivos es lo unico que pasa bajo el
  // cerrojo. Quien llegue despues encuentra la marca puesta y se va; quien pida un ambito, el
  // error de contenedor cerrado. Asi no hay un tercer resultado: o la marca esta o no esta.
  var Scopes: TArray<IServiceScope>;
  FLock.Acquire;
  try
    if FShutDonwnDone then Exit;
    FShutDonwnDone := True;
    // La lista NO se vacia: los ambitos que el llamante siga sosteniendo tienen que seguir
    // siendo alcanzables desde aqui hasta que mueran, porque es como se les limpia el puntero a
    // la fabrica. Los que mueran se daran de baja solos, y FShutDonwnDone ya rechaza todo lo
    // demas, asi que la lista no vuelve a crecer.
    Scopes := LiveScopes;
  finally
    FLock.Release;
  end;

  _AddRef;
  try
    RemoveFromSingletonScope;
    // Cerrar en orden inverso al de creacion es el orden correcto.
    for var idx := High(Scopes) downto Low(Scopes) do
    begin
      TServiceScope(Scopes[idx]).Shutdown;
      Scopes[idx] := nil;
    end;
    TServiceScope(FSingletonScope).Shutdown;

    // El ambito de singletons se suelta al FINAL y con una referencia propia: mientras exista un
    // ambito sin marcar, una resolucion suya puede pedirle un Singleton, y encontrarlo en nil
    // seria el Access Violation. Todos los ambitos alcanzables estan en
    // FScopes --incluido el que creo BuildServiceProvider-- asi que a esta linea ya llevan todos
    // la marca puesta.
    var Singletons: IServiceScope;
    FLock.Acquire;
    try
      Singletons := FSingletonScope;
      FSingletonScope := nil;
      FServiceCollection := nil;
    finally
      FLock.Release;
    end;
    Singletons := nil;
  finally
    _Release;
  end;
end;

function TServiceScopeFactory.OwnedInstanceCount: Integer;
begin
  Result := 0;
  // La foto se toma bajo el cerrojo y la cuenta se hace fuera, sobre referencias propias: asi el
  // recorrido no se rompe si otro hilo crea o cierra un ambito, ninguno de los contados puede
  // morir mientras se cuenta, y el cerrojo de la fabrica sigue siendo una hoja.
  var Singletons: IServiceScope;
  var Scopes: TArray<IServiceScope>;
  FLock.Acquire;
  try
    Singletons := FSingletonScope;
    if Assigned(FScopes) then
      Scopes := LiveScopes;
  finally
    FLock.Release;
  end;

  // FSingletonScope no esta en FScopes, asi que se suma aparte: lo dice la nota de su creacion.
  if Assigned(Singletons) then
    Inc(Result, TServiceScope(Singletons).InstanceCount);
  for var Scope in Scopes do
    Inc(Result, TServiceScope(Scope).InstanceCount);
end;

function TServiceScopeFactory.CreateRootScope(const ValidateScopes: Boolean): IServiceScope;
begin
  FValidateScopes := ValidateScopes;
  Result := CreateScope;
  TServiceScope(Result as TObject).FIsRoot := True;
end;

function TServiceScopeFactory.CreateScope: IServiceScope;
begin
  var Scope := TServiceScope.Create(Self);
  Result := Scope;
  FLock.Acquire;
  try
    // Comprobar y apuntar, en el mismo cerrojo: separados, un ambito creado justo despues de que
    // el cierre vaciara la lista no lo alcanzaria nadie.
    if FShutDonwnDone then
    begin
      // Se descarta el que se acaba de crear en vez de devolverlo: un ambito que no esta en la
      // lista no lo cierra nadie.
      Result := nil;
      CheckNotShutDown;
    end;
    FScopes.Add(Scope);
  finally
    FLock.Release;
  end;
end;

procedure TServiceScopeFactory.CheckNotShutDown;
begin
  if not FShutDonwnDone then Exit;
  raise EContainerShutDownError.CreateFmt('Cannot resolve %s: the container has been shut down',
    [_T.NameOf(TypeInfo(IServiceScope))]);
end;

function TServiceScopeFactory.GetSingletonsScope: IServiceScope;
begin
  Result := FSingletonScope;
end;

{ TScopeProvider }

constructor TScopeProvider.Create(const Scope: TServiceScope);
begin
  inherited Create;
  FScope := Scope;
end;

procedure TScopeProvider.Detach;
begin
  FLock.BeginWrite;
  try
    FScope := nil;
  finally
    FLock.EndWrite;
  end;
end;

function TScopeProvider.Live(out Ref: IServiceScope; const ServiceType: PTypeInfo): TServiceScope;
begin
  FLock.BeginRead;
  try
    if TServiceScope.TryAcquire(FScope, Ref) then
      Result := FScope
    else
      Result := nil;
  finally
    FLock.EndRead;
  end;
  if Result = nil then
    raise EContainerShutDownError.CreateFmt(
      'Cannot resolve %s: the scope that built this service has been released', [_T.NameOf(ServiceType)]);
end;

function TScopeProvider.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  // Lo suyo primero; lo demas --los cableados, el acceso al contenedor, AsType-- es del ambito, y
  // un ambito ya soltado no responde a nada.
  Result := inherited QueryInterface(IID, Obj);
  if Result = S_OK then
    Exit;
  var Ref: IServiceScope;
  FLock.BeginRead;
  try
    if not TServiceScope.TryAcquire(FScope, Ref) then
      Exit(E_NOINTERFACE);
  finally
    FLock.EndRead;
  end;
  Result := Ref.QueryInterface(IID, Obj);
end;

function TScopeProvider.CanResolve(const Slot: TServiceSlot): Boolean;
var
  Ref: IServiceScope;
begin
  Result := Live(Ref, Slot.ServiceType).CanResolve(Slot);
end;

function TScopeProvider.TryGet(const Slot: TServiceSlot; out Service): Boolean;
var
  Ref: IServiceScope;
begin
  Result := Live(Ref, Slot.ServiceType).TryGet(Slot, Service);
end;

function TScopeProvider.TryGetAll(const Slot: TServiceSlot; out Services: IInterfaceList): Boolean;
var
  Ref: IServiceScope;
begin
  Result := Live(Ref, Slot.ServiceType).TryGetAll(Slot, Services);
end;

function TScopeProvider.CreateScope: IServiceScope;
var
  Ref: IServiceScope;
begin
  Result := Live(Ref, TypeInfo(IServiceScope)).CreateScope;
end;

{ TServiceScope }

constructor TServiceScope.Create(const Factory: TServiceScopeFactory; const SingletonsScope: Boolean = False);
begin
  inherited Create;
  FScopeFactory := Factory;
  // Comparador explicito y no el binario por defecto: TServiceIdentity tiene dos campos de
  // tamaños distintos, asi que en 64 bits lleva relleno, y el relleno de un record sin
  // inicializar no vale como parte de una clave.
  FServiceInstances := TOrderedDictionary<TServiceIdentity, IInterface>.Create(
    TEqualityComparer<TServiceIdentity>.Construct(
      function(const A, B: TServiceIdentity): Boolean
      begin
        Result := A = B;
      end,
      function(const Value: TServiceIdentity): Integer
      begin
        Result := Integer(Value.Registration) xor Integer(NativeInt(Value.ClosedType)) xor
          ServiceKey.HashOf(Value.ServedKey);
      end));
  FWired := TDictionary<Pointer, Byte>.Create;
  FIsSingletonsScope := SingletonsScope;
  FBuildLock := TCriticalSection.Create;
  FServiceCollection := Factory.ServiceCollection;
  FProviderObj := TScopeProvider.Create(Self);
  FProvider := FProviderObj;
end;

class function TServiceScope.TryAcquire(const Scope: TServiceScope; out Ref: IServiceScope): Boolean;
begin
  Ref := nil;
  if Scope = nil then Exit(False);

  // Tomar referencia SOLO si sigue vivo. Hace falta porque el contador llega a cero fuera de
  // todo cerrojo: entre ese cero y la primera linea del destructor --la baja-- la fabrica
  // todavia ve el ambito en la lista.
  // ⚠️ La guarda mira <= 0 y no = 0 porque el RTL marca al que muere poniendo el contador en
  // Integer.MinValue, de forma atomica y ANTES de llamar al destructor. Si esa marca dejara de
  // ser negativa, esta guarda dejaria de proteger sin que nada fallase.
  var Current: Integer;
  repeat
    Current := Scope.FRefCount;
    if Current <= 0 then Exit(False);
  until AtomicCmpExchange(Scope.FRefCount, Current + 1, Current) = Current;

  // El incremento de arriba ya es la referencia que se entrega; asignar la interfaz suma otra,
  // asi que se compensa. Entre las dos lineas el contador vale dos, nunca cero, de modo que
  // esta compensacion no puede destruir nada.
  Ref := Scope;
  Scope._Release;
  Result := True;
end;

destructor TServiceScope.Destroy;
begin
  // La baja es lo PRIMERO: desde aqui ni la fabrica ni el proveedor que tienen los servicios lo
  // alcanzan. Al reves --desmontar y darse de baja despues-- deja una ventana en la que alcanzan
  // un ambito a medio destruir.
  FProviderObj.Detach;
  if FScopeFactory <> nil then
    FScopeFactory.RemoveScope(Self);
  // Just in case this Scope is temporal, and we not are in Shutdown
  Shutdown;
  FreeAndNil(FServiceInstances);
  FreeAndNil(FWired);
  FreeAndNil(FBuildLock);
  inherited;
end;

procedure TServiceScope.AddServiceInstance(const Descriptor: TServiceDescriptor; const Service: IInterface);
begin
  FInstancesLock.BeginWrite;
  try
    FServiceInstances.Add(GetServiceIdentity(Descriptor), Service);
  finally
    FInstancesLock.EndWrite;
  end;
end;

function TServiceScope.TryGetInstance(const Identity: TServiceIdentity; out Service: IInterface): Boolean;
begin
  FInstancesLock.BeginRead;
  try
    Result := FServiceInstances.TryGetValue(Identity, Service);
  finally
    FInstancesLock.EndRead;
  end;
end;

procedure TServiceScope.RemoveFirstInstance;
begin
  FInstancesLock.BeginWrite;
  try
    FServiceInstances.Delete(0);
  finally
    FInstancesLock.EndWrite;
  end;
end;

function TServiceScope.InstanceCount: Integer;
begin
  if not Assigned(FServiceInstances) then Exit(0);
  FInstancesLock.BeginRead;
  try
    Result := FServiceInstances.Count;
  finally
    FInstancesLock.EndRead;
  end;
end;

function InstanceOf(const Service: IInterface): TObject;
begin
  if Service = nil then Exit(nil);
  Result := Service as TObject;
end;

// La cadena, del primero al ultimo, con el ciclo que cierra Descriptor al final.
function DescribeCycle(const Descriptor: TServiceDescriptor): string;
begin
  var Path: TArray<string> := [];
  var Frame := GCreating;
  while Frame <> nil do
  begin
    Path := [Format('%s (%s)', [Frame.Descriptor.ServiceName, LifeTimeName(Frame.Descriptor.LifeTime)])] + Path;
    if Frame.Identity = Descriptor.Identity then Break;
    Frame := Frame.Outer;
  end;
  Path := Path + [Format('%s (%s)', [Descriptor.ServiceName, LifeTimeName(Descriptor.LifeTime)])];
  Result := 'dependency cycle the container cannot break: ' + string.Join(' -> ', Path);
end;

// Anota que este hilo empieza a construir Descriptor, y falla si ya lo estaba construyendo. Lo
// que llega aqui es un ciclo irrompible por construccion: uno rompible se satisface desde las
// instancias ya publicadas y nunca vuelve a entrar a construir.
procedure BeginCreate(var Frame: TCreationFrame; const Descriptor: TServiceDescriptor);
begin
  var Existing := GCreating;
  while Existing <> nil do
  begin
    if Existing.Identity = Descriptor.Identity then
      raise EServiceProviderError.Create(DescribeCycle(Descriptor));
    Existing := Existing.Outer;
  end;
  Frame.Descriptor := @Descriptor;
  Frame.Identity := Descriptor.Identity;
  Frame.Outer := GCreating;
  GCreating := @Frame;
end;

procedure EndCreate(var Frame: TCreationFrame);
begin
  GCreating := Frame.Outer;
end;

// El singleton mas cercano que este hilo esta construyendo, o nil.
function CreatingSingleton: PServiceDescriptor;
begin
  var Frame := GCreating;
  while Frame <> nil do
  begin
    if Frame.Descriptor.LifeTime = Singleton then Exit(Frame.Descriptor);
    Frame := Frame.Outer;
  end;
  Result := nil;
end;

function TServiceScope.InjectorsFor(const AClass: TClass): TArray<TServiceInjector>;
begin
  if ServiceCollection = nil then Exit(nil);
  Result := ServiceCollection.InjectorsFor(AClass);
end;

function TServiceScope.GetOrCreate(const Descriptor: TServiceDescriptor; const Provider: IServiceProvider): IInterface;
begin
  // El cierre se comprueba ANTES de tocar el diccionario, y del ambito de destino: un Singleton
  // se guarda en el de singletons, que no es el que resuelve, asi que la guarda de
  // TryGetDescriptors no lo cubre. Y va primero porque el desmontaje recorre el diccionario con
  // el cerrojo de escritura tomado: un destructor que resuelva mientras eso pasa encuentra aqui
  // el error de contenedor cerrado en vez de quedarse esperando ese cerrojo.
  CheckNotShutDown(Descriptor.TypeInfo);

  var ServiceIdentity := GetServiceIdentity(Descriptor);
  if TryGetInstance(ServiceIdentity, Result) then Exit;

  FBuildLock.Acquire;
  try
    // La segunda comprobacion no es una precaucion opcional: entre la lectura de arriba y este
    // cerrojo otro hilo pudo construirlo. Sin ella la ventana sigue abierta, mas estrecha -- y
    // una carrera mas estrecha es peor, porque aparece en produccion y no en los tests.
    if TryGetInstance(ServiceIdentity, Result) then Exit;

    var Frame: TCreationFrame;
    BeginCreate(Frame, Descriptor);
    try
      // El constructor corre con la construccion cerrada, no con el diccionario cerrado. Que
      // corra dentro es lo que hace cierto "se construye una vez": construir fuera y quedarse con
      // el ganador ejecuta el constructor una vez por hilo, y los efectos de un constructor no se
      // deshacen por perder la carrera.
      Result := CreateInstance(Descriptor, Provider);

      if FShutDonwnDone then
      begin
        // Perdio la carrera con el cierre despues de construir. Soltar la referencia aqui destruye
        // la instancia: entregarla seria una fuga --el contenedor ya no la va a desmontar-- y no
        // entregarla pero dejarla viva, otra.
        Result := nil;
        CheckNotShutDown(Descriptor.TypeInfo);
      end;
      // Un nil de la factory no se cablea, y un Singleton no lo retiene: la siguiente resolucion
      // vuelve a llamar a la factory. Un Scoped si lo guarda en su ambito.
      if Result = nil then
      begin
        if Descriptor.LifeTime <> Singleton then
          AddServiceInstance(Descriptor, nil);
        Exit;
      end;
      // Publicar y DESPUES cablear, en ese orden. Mientras la instancia no este en el diccionario,
      // quien la vuelva a pedir --ella misma, o alguien a quien ella pide-- entra otra vez a
      // construirla, y ahi no hay fondo. Solo se cablea lo construido por clase: lo que devuelve una
      // factory, o una instancia registrada, es de quien lo construyo.
      AddServiceInstance(Descriptor, Result);
      if Descriptor.ImplementationClass <> nil then
      begin
        FInstancesLock.BeginWrite;
        try
          FWired.AddOrSetValue(InstanceOf(Result), 0);
        finally
          FInstancesLock.EndWrite;
        end;
        TActivatorUtilities.Wire(Provider, InstanceOf(Result));
      end;
    finally
      EndCreate(Frame);
    end;
  finally
    FBuildLock.Release;
  end;
end;

// La clase de lo que devolvio una factory, para un mensaje de error: no puede fallar, porque lo
// devuelto puede no ser un objeto de Delphi.
function ImplementorName(const Intf: IInterface): string;
begin
  try
    Result := (Intf as TObject).QualifiedClassName;
  except
    on EInvalidCast do
      Result := 'an object that is not a Delphi object';
  end;
end;

function TServiceScope.CreateInstance(const Descriptor: TServiceDescriptor; const Provider: IServiceProvider): IInterface;
begin
  var Intf := Descriptor.Factory(Provider);
  if Intf = nil then
    Exit(nil);
  if not Supports(Intf, Descriptor.TypeInfo.TypeData.GUID, Result) then
    raise EIntfCastError.CreateFmt('The factory of %s returned %s, which does not implement it',
      [Descriptor.ServiceName, ImplementorName(Intf)]);
end;

function TServiceScope.GetServiceIdentity(const Descriptor: TServiceDescriptor): TServiceIdentity;
begin
  // El registro y, cuando se llego a el emparejando, tambien el tipo cerrado. Con el nombre
  // del tipo el ambito no podia guardar mas de una instancia por tipo de servicio; con el
  // registro a secas, un registro sin fijar --que es UNO para todos sus cierres-- serviria
  // una sola instancia a todos ellos. Los dos ejes vienen ya puestos en la identidad.
  Result := Descriptor.Identity;
end;

function TServiceScope.GetSingletonsScope: IServiceResolver;
begin
  if IsSingletonsScope  then Exit(Self);
  if FScopeFactory = nil then Exit(nil);
  // Devuelve nil en vez de reventar, y quien llama decide con que error: el cierre suelta el
  // ambito de singletons al final, y una resolucion que ya paso la guarda de SU ambito puede
  // llegar aqui justo despues. Comprobar y usar no es atomico y no se puede hacer que lo sea sin
  // un cerrojo en el camino caliente, asi que lo que se arregla es el modo de fallo: sin esto,
  // "nil as IServiceResolver" daba un Access Violation leyendo la direccion 0.
  var Scope := FScopeFactory.SingletonsScope;
  if Scope = nil then Exit(nil);
  Result := Scope as IServiceResolver;
end;

procedure TServiceScope.CollectGarbage;
begin
  // El recorrido entero bajo el cerrojo de escritura, no cada borrado: el algoritmo depende de
  // que los recuentos de referencia bajen en cascada mientras itera, y con el cerrojo suelto
  // entre vueltas otro hilo podria anadir una instancia a un ambito que se esta desmontando.
  FInstancesLock.BeginWrite;
  try
    // Vaciar las propiedades inyectadas de TODAS antes de soltar la primera. Dos servicios que
    // se inyectaron mutuamente se sostienen entre ellos: ninguno baja de dos referencias, el
    // bucle de abajo termina sin liberar nada y el Clear final los suelta a la vez, cada uno
    // sosteniendo al otro. Y todas antes que ninguna, porque alternando podria morir uno
    // mientras otro lo tiene en una propiedad sin vaciar.
    // Solo las que el ambito cableo: lo que devuelve una factory, o una instancia registrada, es de
    // quien lo construyo, y sus propiedades tambien.
    for var idx := 0 to FServiceInstances.Count - 1 do
    begin
      // A una local, como el bucle de abajo. Pasar ValueList[idx] directo a una llamada deja
      // viva hasta el final del metodo la referencia del temporal que emite el compilador, y esa
      // referencia impide que su instancia baje a uno: se libera fuera de orden, la ultima.
      var Svc: IInterface := FServiceInstances.ValueList[idx];
      var Instance := InstanceOf(Svc);
      if (Instance <> nil) and FWired.ContainsKey(Instance) then
        TActivatorUtilities.ClearInjected(Instance);
    end;

    var UnusedFound: Boolean;
    repeat
      UnusedFound := False;
      for var idx := FServiceInstances.Count - 1 downto 0 do
      begin
        // ValueList[idx] hands back the IInterface BY VALUE, so the compiler emits an
        // _AddRef for the temporary AND the matching _Release when that temporary dies.
        // Reading into a local and restoring the count keeps the read balanced. The
        // threshold does not move: with the local alive the count is dictionary + local
        // = 2, and _Release leaves it at 1. The local also keeps the
        // entry alive across the Delete below.
        var Svc: IInterface := FServiceInstances.ValueList[idx];
        // El nil que un Scoped guarda de su factory no sostiene nada.
        if Svc = nil then
        begin
          FServiceInstances.Delete(idx);
          UnusedFound := True;
          Break;
        end;
        var RefCount := Svc._Release;
        Svc._AddRef;
        if (RefCount = 1) then
        // it is only referenced by FServiceInstances, so it will be released
        // and that will reduce the RefCount of other services in this
        // scope
        begin
          FServiceInstances.Delete(idx);
          UnusedFound := True;
          Break
        end;
      end;
    until not UnusedFound;
    FServiceInstances.Clear;
    FWired.Clear;
  finally
    FInstancesLock.EndWrite;
  end;
end;

procedure TServiceScope.ShutdownContainer;
begin
  // El ambito alcanza la fabrica por FScopeFactory, que la ve sin poseerla. Esta es la costura
  // por la que IServiceProvider.ShutDown llega al contenedor sin resolver nada.
  if FScopeFactory <> nil then
    FScopeFactory.Shutdown;
end;

function TServiceScope.OwnedInstanceCount: Integer;
begin
  // La misma costura por la que ShutdownContainer llega al contenedor entero: FScopeFactory la
  // ve sin poseerla.
  if FScopeFactory <> nil then
    Result := FScopeFactory.OwnedInstanceCount
  else
    Result := 0;
end;

procedure TServiceScope.Shutdown;
begin
  if FShutDonwnDone then Exit;
  FShutDonwnDone := True;
  CollectGarbage;
end;

function TServiceScope.ServiceProvider: IServiceProvider;
begin
  Result := Self;
end;

function TServiceScope.HandedProvider: IServiceProvider;
begin
  Result := FProvider;
end;

function TServiceScope.CanResolve(const Slot: TServiceSlot): Boolean;
begin
  // Antes que la coleccion: el ambito la apunta sin sostenerla, y cerrado puede no existir ya.
  CheckNotShutDown(Slot.ServiceType);
  Result := ServiceCollection.Contains(Slot);
  if Result then Exit;
  // Y esto no es un adorno: TActivatorUtilities pregunta aqui ANTES de elegir constructor. Si
  // el emparejamiento sin fijar no se reflejase, el constructor que pide un cierre no se
  // elegiria, la clase se construiria con el de TObject y quedaria a medias -- en verde y sin
  // excepcion, que es el peor sitio donde dejar esto sin contestar. Lo mismo vale para el comodin.
  if Slot.IsKeyed then
    Exit(ServiceCollection.Contains(Slot.AnyKeySlot));

  var OpenType: PTypeInfo;
  Result := ServiceCollection.TryFindOpenGeneric(Slot.ServiceType, OpenType);
end;

function TServiceScope.TryGetDescriptors(const Slot: TServiceSlot; out Descriptors: TServiceDescriptors;
  out MatchedOpen, MatchedAnyKey: Boolean): Boolean;
begin
  // El embudo por el que pasan TryGet y TryGetAll, y por tanto toda resolucion. La guarda vive
  // aqui y no en las firmas publicas del record: alli habria que escribirla ocho veces. Y la clave
  // llega hasta aqui y no mas lejos: GetImplementor, GetOrCreate y CollectGarbage siguen
  // trabajando con descriptores y no se enteran de que existe.
  CheckNotShutDown(Slot.ServiceType);
  Descriptors := nil;
  MatchedOpen := False;
  MatchedAnyKey := False;

  if ServiceCollection.Contains(Slot) then
  begin
    Descriptors := ServiceCollection.GetDescriptors(Slot);
    Exit(True);
  end;

  // Los dos emparejamientos entran al FALLAR la busqueda directa, y ese orden ES el requisito: un
  // registro concreto gana al general, tanto sobre el eje del cierre generico como sobre el de la
  // clave.
  if Slot.IsKeyed then
  begin
    // Y el comodin no cruza al espacio sin clave: solo se pregunta por el cuando habia clave.
    Result := ServiceCollection.Contains(Slot.AnyKeySlot);
    if not Result then Exit;
    MatchedAnyKey := True;
    Descriptors := ServiceCollection.GetDescriptors(Slot.AnyKeySlot);
    Exit;
  end;

  var OpenType: PTypeInfo;
  Result := ServiceCollection.TryFindOpenGeneric(Slot.ServiceType, OpenType);
  if not Result then Exit;
  MatchedOpen := True;
  Descriptors := ServiceCollection.GetDescriptors(TServiceSlot.Create(OpenType));
end;

function TServiceScope.DescriptorFor(const Slot: TServiceSlot; const Descriptor: TServiceDescriptor;
  const MatchedOpen, MatchedAnyKey: Boolean): TServiceDescriptor;
begin
  if MatchedOpen then
    Result := Descriptor.ForClosedType(Slot.ServiceType)
  else if MatchedAnyKey then
    Result := Descriptor.ForServedKey(Slot.Key)
  else
    Result := Descriptor;
end;

function TServiceScope.TryGet(const Slot: TServiceSlot; out Service): Boolean;
begin
  Pointer(Service) := nil;
  var
    Descriptors: TServiceDescriptors;
  var MatchedOpen, MatchedAnyKey: Boolean;
  Result := TryGetDescriptors(Slot, Descriptors, MatchedOpen, MatchedAnyKey);
  if not Result then
    Exit;
  var
  Descriptor := DescriptorFor(Slot, Descriptors.Last, MatchedOpen, MatchedAnyKey);
  IInterface(Service) := GetImplementor(Descriptor);
end;

function TServiceScope.TryGetAll(const Slot: TServiceSlot; out Services: IInterfaceList): Boolean;
begin
  Services := TInterfaceList<IInterface>.Create;

  var Descriptors: TServiceDescriptors;
  var MatchedOpen, MatchedAnyKey: Boolean;
  Result := TryGetDescriptors(Slot, Descriptors, MatchedOpen, MatchedAnyKey);
  if not Result then
    Exit;

  for var Descriptor in Descriptors do
    Services.Add(GetImplementor(DescriptorFor(Slot, Descriptor, MatchedOpen, MatchedAnyKey)));
end;

function TServiceScope.CreateScope: IServiceScope;
begin
  // Un contenedor cerrado que repartiera ambitos estaria repoblando el arbol que se acaba de
  // desmontar, y quien lo recibiera solo lo descubriria al resolver.
  CheckNotShutDown(TypeInfo(IServiceScope));
  Result := FScopeFactory.CreateScope;
end;

procedure TServiceScope.CheckNotShutDown(const ServiceType: PTypeInfo);
begin
  if not FShutDonwnDone then Exit;
  RaiseShutDown(ServiceType);
end;

procedure TServiceScope.RaiseScopedFromRoot(const Descriptor: TServiceDescriptor);
begin
  // Lo que se pidio a la raiz es el marco mas externo de lo que este hilo construye; sin ninguno,
  // el Scoped se pidio directamente.
  var Requested := GCreating;
  while (Requested <> nil) and (Requested.Outer <> nil) do
    Requested := Requested.Outer;
  if Requested = nil then
    raise EServiceProviderError.CreateFmt('Cannot resolve scoped service %s from root provider',
      [Descriptor.ServiceName]);
  raise EServiceProviderError.CreateFmt('Cannot resolve %s from root provider because it requires scoped service %s',
    [Requested.Descriptor.ServiceName, Descriptor.ServiceName]);
end;

procedure TServiceScope.RaiseShutDown(const ServiceType: PTypeInfo);
begin
  // El mensaje nombra lo que se pidio: sin depurador comodo, un "esta cerrado" a secas obliga
  // a adivinar quien preguntaba.
  raise EContainerShutDownError.CreateFmt('Cannot resolve %s: the container has been shut down',
    [_T.NameOf(ServiceType)]);
end;

function TServiceScope.GetImplementor(const Descriptor: TServiceDescriptor): IInterface;
begin
  case Descriptor.LifeTime of
    Singleton: begin
      var Singletons := SingletonsScope;
      if Singletons = nil then
        RaiseShutDown(Descriptor.TypeInfo);
      // Con el proveedor del contenedor y no con el de quien lo pidio: el singleton vive mas que
      // cualquier ambito hijo.
      Result := Singletons.GetOrCreate(Descriptor, Singletons.HandedProvider);
    end;
    Scoped: begin
      var Captor := CreatingSingleton;
      if Captor <> nil then
        raise Exception.CreateFmt('singleton %s cannot depends on scoped %s ',[Captor.ServiceName, Descriptor.ServiceName]);
      if FIsRoot and Assigned(FScopeFactory) and FScopeFactory.ValidateScopes then
        RaiseScopedFromRoot(Descriptor);
      Result := Self.GetOrCreate(Descriptor, FProvider);
    end;
    Transient: begin
      // El contenedor no retiene los transient, asi que no hay nada que publicar y el cableado
      // corre aqui. La cadena si se anota: dos transient que se inyectan mutuamente no tienen
      // ninguna instancia publicada desde la que satisfacerse, y sin la anotacion la resolucion
      // construiria uno nuevo en cada vuelta.
      var Frame: TCreationFrame;
      BeginCreate(Frame, Descriptor);
      try
        Result := Self.CreateInstance(Descriptor, FProvider);
        if (Result <> nil) and (Descriptor.ImplementationClass <> nil) then
          TActivatorUtilities.Wire(FProvider, InstanceOf(Result));
      finally
        EndCreate(Frame);
      end;
    end;
  end;
end;

end.
