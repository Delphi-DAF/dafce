unit Daf.DependencyInjection;

interface

uses
  System.TypInfo,
  System.Rtti,
  System.Classes,
  System.Generics.Collections,
  Daf.Enumerable,
  DAF.Extensions.DependencyInjection;

type

  TDIObject<T: class> = class(TInterfacedObject)
  {$IFOPT D+}
  strict private
    class var FInstanceCount: Integer;
    class var FInstances: TArray<T>;
  private
    FID: string;
  public
    class property InstanceCount: Integer read FInstanceCount;
    class property Instances: TArray<T> read FInstances;
    property _ID: string read FID;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  {$ENDIF}
  end;

type
{$REGION 'from unit System.Rtti implem;'}
  PPVtable = ^PVtable;
  PVtable = ^TVtable;
  TVtable = array [0 .. MaxInt div SizeOf(Pointer) - 1] of Pointer;
{$ENDREGION}

  TDescriptorsDictionary = TObjectDictionary<PTypeInfo, TServiceDescriptors>;
  TServiceScope = class;

  // Es un singleton. Contiene la lista de todos los Scopes creados asi como
  // el scope para singletons
  TServiceScopeFactory = class(TDIObject<TServiceScopeFactory>, IServiceScopeFactory, IContainerAccess)
  strict private
    FServiceCollection: IServiceCollection;
    FSingletonScope: IServiceScope;
    FShutDonwnDone: Boolean;
    FScopes: TList<IServiceScope>;
    function GetSingletonsScope: IServiceScope;
  private
    procedure AddToSingletonScope;
    procedure RemoveFromSingletonScope;
  protected
    procedure Shutdown;
  public
    constructor Create(const ServiceCollection: IServiceCollection);
    destructor Destroy;override;
    function CreateScope: IServiceScope;
    property ServiceCollection: IServiceCollection read FServiceCollection;
    property SingletonsScope: IServiceScope read GetSingletonsScope;
  end;

  IServiceResolver = interface(IInvokable)
    ['{8B521187-079B-44FE-9124-5A1E6771ECCE}']
    function CreateInstance(const Descriptor: TServiceDescriptor; const Provider: IServiceProvider): IInterface;
    function GetOrCreate(const Descriptor: TServiceDescriptor; const Provider: IServiceProvider): IInterface;overload;
  end;

  TServiceScope = class(TDIObject<TServiceScope>, IServiceScope, IServiceProviderImpl, IServiceResolver,
    IContainerAccess)
  private
    [Weak]
    FScopeFactory: TServiceScopeFactory;
    [Weak]
    FServiceCollection: IServiceCollectionImpl;
    // Ordenado a proposito, y sigue siendolo: CollectGarbage lo recorre de atras hacia
    // delante para liberar en orden inverso al de creacion. Lo que cambio aqui es la
    // clave, no la estructura.
    FServiceInstances : TOrderedDictionary<TServiceIdentity, IInterface>;
    FCreatingSingletons: TStack<TServiceDescriptor>;
    FIsSingletonsScope: Boolean;
    FShutDonwnDone: Boolean;
    function GetServiceIdentity(const Descriptor: TServiceDescriptor): TServiceIdentity;
    function CreateInstance(const Descriptor: TServiceDescriptor; const Provider: IServiceProvider): IInterface;
    function GetOrCreate(const Descriptor: TServiceDescriptor; const Provider: IServiceProvider): IInterface;overload;
    procedure BeginCreateSingleton(const Descriptor: TServiceDescriptor);
    procedure EndCreateSingleton;
    function GetImplementor(const Descriptor: TServiceDescriptor): IInterface;
    function TryGet(const ServiceType: PTypeInfo; out Service): Boolean;
    function TryGetAll(const ServiceType: PTypeInfo; out Services: IInterfaceList): Boolean;
    function TryGetDescriptors(const ServiceType: PTypeInfo; out Descriptors: TServiceDescriptors;
      out MatchedOpen: Boolean): Boolean;
    function DescriptorFor(const ServiceType: PTypeInfo; const Descriptor: TServiceDescriptor;
      const MatchedOpen: Boolean): TServiceDescriptor;
    function GetSingletonsScope: IServiceResolver;
    procedure AddServiceInstance(const Descriptor: TServiceDescriptor; const Service: IInterface);
    function CanResolve(const ServiceType: PTypeInfo): Boolean;
    procedure CheckNotShutDown(const ServiceType: PTypeInfo);
    procedure CollectGarbage;
  protected
    property IsSingletonsScope: Boolean read FIsSingletonsScope;
    property ScopeFactory: TServiceScopeFactory read FScopeFactory;
    // Clausula de resolucion porque Shutdown ya esta cogido en esta clase y significa otra cosa:
    // el de abajo recoge la basura de ESTE ambito, y este de aqui cierra el contenedor entero.
    procedure IContainerAccess.Shutdown = ShutdownContainer;
    procedure ShutdownContainer;
  public
    constructor Create(const Factory: TServiceScopeFactory; const SingletonsScope: Boolean = False);
    destructor Destroy; override;
    procedure Shutdown;
    function CreateScope: IServiceScope;
    function ServiceProvider: IServiceProvider;
    property ServiceCollection: IServiceCollectionImpl read FServiceCollection;
    property SingletonsScope: IServiceResolver read GetSingletonsScope;
  end;

  TServiceCollection = class(TDIObject<TServiceCollection>, IServiceCollectionImpl)
  strict private
    FEntries: TDescriptorsDictionary;
    // Estado de la coleccion, no del proceso: componer dos veces en un mismo proceso
    // tiene que dar el mismo resultado, y con una global de unit no lo daba.
    FVisited: TDictionary<string, Boolean>;
    // Que registro sin fijar sirve a cada cierre, incluido el "ninguno" -- por eso guarda nil
    // en vez de no guardar nada. Sin esto, cada resolucion de un cierre no registrado
    // recorreria todas las claves de la coleccion. Se vacia en cuanto la coleccion cambia:
    // un "ninguno" cacheado antes de un registro posterior seria una respuesta caducada.
    FOpenMatches: TDictionary<PTypeInfo, PTypeInfo>;
    FIsFrozen: Boolean;
    function DescribeUnresolvable(const Descriptor: TServiceDescriptor; const Failed: TRttiParameter): string;
    procedure CollectCapturedScopes(const Descriptor: TServiceDescriptor; const Ctor: TRttiMethod;
      var Errors: TArray<string>);
    procedure Validate(const Provider: IServiceProvider; const Options: TServiceProviderOptions);
  protected
    function CreateScopeFactory: IServiceScopeFactory;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CheckNotFrozen;
    function GetDescriptors(ServiceType: PTypeInfo): TServiceDescriptors;
    function SetDescriptors(const ServiceType: PTypeInfo; const NewDescriptors: TArray<TServiceDescriptor>): IServiceCollectionImpl;
    function RemoveAll(const ServiceType: PTypeInfo): IServiceCollectionImpl;
    function TryFindOpenGeneric(const ClosedType: PTypeInfo; out OpenType: PTypeInfo): Boolean;
    function TryMarkVisited(const Category, Key: string): Boolean;
    function GetCount: Integer;
    function Add<ServiceType: IInterface>(const LifeTime: TServiceLifeTime; const Factory: TServiceFactory): IServiceCollectionImpl;overload;
    function Add(const ServiceType: PTypeInfo; const LifeTime: TServiceLifeTime; const Factory: TServiceFactory;
      const ImplClass: TClass = nil): IServiceCollectionImpl;overload;
    function BuildServiceProvider: IServiceProviderImpl;overload;
    function BuildServiceProvider(const Options: TServiceProviderOptions): IServiceProviderImpl;overload;
    function Contains<T: IInterface>: Boolean;overload;
    function Contains(const ServiceType: PTypeInfo): Boolean;overload;
    property Descriptors[ServiceType: PTypeInfo]: TServiceDescriptors read GetDescriptors;default;
  end;

{$IFOPT D+}
var
  GlobalInstanceCount: Integer;
{$ENDIF}

implementation

uses
  System.SysUtils,
  System.Generics.Defaults,
  Daf.Types,
  Daf.Rtti,
  Daf.DependencyInjection.ActivatorUtilities,
  Daf.DependencyInjection.OpenGenerics;

{ TDIObject }

{$IFOPT D+}
procedure TDIObject<T>.AfterConstruction;
begin
  inherited;
  FID := TGUID.NewGuid.ToString;
  Setlength(Finstances, 1 + Length(Finstances));
  FInstances[FinstanceCount] := Self as T;
  AtomicIncrement(FInstanceCount);
  AtomicIncrement(GlobalInstanceCount);
end;

procedure TDIObject<T>.BeforeDestruction;
begin
  var FoundIdx := TArray.IndexOf<T>(Finstances, Self as T);
  if FoundIdx > -1 then
  begin
    FInstances[FoundIdx] := nil;
    AtomicDecrement(FInstanceCount);
    AtomicDecrement(GlobalInstanceCount);
    if FinstanceCount = 0 then
      FInstances := nil;
  end;
  inherited;
end;
{$ENDIF}

{ TServiceCollection }

constructor TServiceCollection.Create;
begin
  inherited Create;
  FEntries := TDescriptorsDictionary.Create([doOwnsValues]);
  FVisited := TDictionary<string, Boolean>.Create;
  FOpenMatches := TDictionary<PTypeInfo, PTypeInfo>.Create;
end;

destructor TServiceCollection.Destroy;
begin
  FOpenMatches.Free;
  FVisited.Free;
  FEntries.Free;
  inherited;
end;

function TServiceCollection.TryFindOpenGeneric(const ClosedType: PTypeInfo; out OpenType: PTypeInfo): Boolean;
begin
  OpenType := nil;
  if (ClosedType = nil) or not _T.IsGeneric(ClosedType) then Exit(False);
  if FOpenMatches.TryGetValue(ClosedType, OpenType) then Exit(OpenType <> nil);

  // Un registro sin fijar no se empareja consigo mismo: IX<_> es un tipo cerrado legitimo y
  // se resuelve por la via directa, como cualquier otro.
  if not TOpenGeneric.IsOpen(ClosedType) then
    for var Entry in FEntries do
      if TOpenGeneric.IsOpen(Entry.Key) and TOpenGeneric.SameForm(Entry.Key, ClosedType) then
      begin
        OpenType := Entry.Key;
        Break;
      end;

  FOpenMatches.AddOrSetValue(ClosedType, OpenType);
  Result := OpenType <> nil;
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
  Result := Contains(TypeInfo(T));
end;

procedure TServiceCollection.CheckNotFrozen;
begin
  if FIsFrozen then
    raise Exception.Create('This ServiceCollection is frozen and can no longer be modified.');
end;

function TServiceCollection.Contains(const ServiceType: PTypeInfo): Boolean;
begin
  Result := FEntries.ContainsKey(ServiceType);
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
  var Scope := Factory.CreateScope;
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
      if not TActivatorUtilities.TryFindConstructor(Provider, Descriptor.ImplementationClass, Ctor, Failed) then
      begin
        if Options.ValidateOnBuild then
          Errors := Errors + [DescribeUnresolvable(Descriptor, Failed)];
        Continue;
      end;

      if Options.ValidateScopes and (Descriptor.LifeTime = Singleton) and (Ctor <> nil) then
        CollectCapturedScopes(Descriptor, Ctor, Errors);
    end;

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

  Result := Format('%s: %s needs %s for parameter "%s", and nothing registers it',
    // El tipo del parametro tampoco tiene por que ser nombrable: QualifiedName aqui abortaba
    // la validacion con un error de visibilidad en lugar de decir que falta el registro.
    [Descriptor.ServiceName, Descriptor.ImplementationClass.ClassName,
     ServiceTypeName(Failed.ParamType.Handle), Failed.Name]);
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
    if not FEntries.TryGetValue(PInfo, Dependencies) then Continue;
    if Dependencies.Count = 0 then Continue;
    // El ultimo descriptor es el que resuelve TryGet, asi que es su ciclo de vida el que cuenta.
    if Dependencies.Last.LifeTime <> Scoped then Continue;

    Errors := Errors + [Format('captured dependency: singleton %s (%s) depends on scoped %s, via parameter "%s"',
      [Descriptor.ServiceName, Descriptor.ImplementationClass.ClassName, Dependencies.Last.ServiceName, Param.Name])];
  end;
end;

function TServiceCollection.GetCount: Integer;
begin
  Result := FEntries.Count;
end;

function TServiceCollection.GetDescriptors(ServiceType: PTypeInfo): TServiceDescriptors;
begin
  Result := FEntries[ServiceType];
end;

function TServiceCollection.SetDescriptors(const ServiceType: PTypeInfo;
  const NewDescriptors: TArray<TServiceDescriptor>): IServiceCollectionImpl;
begin
  CheckNotFrozen;
  Result := Self;
  FOpenMatches.Clear;
  FEntries.Remove(ServiceType);
  if Length(NewDescriptors) = 0 then Exit;

  var List := TServiceDescriptors.Create;
  FEntries.AddOrSetValue(ServiceType, List);
  List.AddRange(NewDescriptors);
end;

function TServiceCollection.RemoveAll(const ServiceType: PTypeInfo): IServiceCollectionImpl;
begin
  CheckNotFrozen;
  Result := Self;
  FOpenMatches.Clear;
  // Se borra la entrada entera, no se vacia la lista: si quedara una clave con lista vacia,
  // Contains seguiria diciendo que si y TryAdd no repondria el registro.
  FEntries.Remove(ServiceType);
end;

function TServiceCollection.Add<ServiceType>(const LifeTime: TServiceLifeTime;
  const Factory: TServiceFactory): IServiceCollectionImpl;
begin
  Result := Add(TypeInfo(ServiceType), LifeTime, Factory);
end;

function TServiceCollection.Add(const ServiceType: PTypeInfo; const LifeTime: TServiceLifeTime;
  const Factory: TServiceFactory; const ImplClass: TClass): IServiceCollectionImpl;
begin
  CheckNotFrozen;

  Assert(Assigned(Factory), 'ServiceFactory can''t be empty');
  Result := Self;
  FOpenMatches.Clear;
  var
    Descriptors: TServiceDescriptors;

  if not FEntries.TryGetValue(ServiceType, Descriptors) then
  begin
    Descriptors := TServiceDescriptors.Create;
    FEntries.AddOrSetValue(ServiceType, Descriptors);
  end;
  var Descriptor := TServiceDescriptor.Create(Factory, LifeTime, ServiceType, ImplClass);
  Descriptors.Add(Descriptor);
end;

{ TServiceScopeFactory }

constructor TServiceScopeFactory.Create(const ServiceCollection: IServiceCollection);
begin
  inherited Create;
  FServiceCollection := ServiceCollection;
  // Note FSingletonScope is not in FScopes !!
  FSingletonScope := TServiceScope.Create(Self, True);
  FScopes := TList<IServiceScope>.Create;
  AddToSingletonScope;
end;

destructor TServiceScopeFactory.Destroy;
begin
  FreeAndNil(FScopes);
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
  with TServiceScope(FSingletonScope).FServiceInstances do
    Delete(0);
end;

procedure TServiceScopeFactory.Shutdown;
begin
  if FShutDonwnDone then Exit;
  FShutDonwnDone := True;
  _AddRef;
  try
    RemoveFromSingletonScope;
  var Scopes := FScopes.ToArray;
  FSCopes.Clear;
  // downto, no to: ascendente iba de High a Low, asi que corria UNA vez con un solo ambito y
  // NINGUNA con dos o mas -- que es cuando de verdad hacia falta. Cerrar en orden inverso al de
  // creacion es ademas el orden correcto.
  for var idx := High(Scopes) downto Low(Scopes) do
    begin
       TServiceScope(Scopes[idx]).Shutdown;
    Scopes[idx] := nil;
    end;
  TServiceScope(FSingletonScope).Shutdown;
  FSingletonScope := nil;
    FServiceCollection := nil;
  finally
    _Release;
  end;
end;

function TServiceScopeFactory.CreateScope: IServiceScope;
begin
  Result := TServiceScope.Create(Self);
  FScopes.Add(Result);
end;

function TServiceScopeFactory.GetSingletonsScope: IServiceScope;
begin
  Result := FSingletonScope;
end;

{ TServiceScope }

constructor TServiceScope.Create(const Factory: TServiceScopeFactory; const SingletonsScope: Boolean = False);
begin
  inherited Create;
  FScopeFactory := Factory;
  // Comparador explicito y no el binario por defecto: TServiceIdentity tiene dos campos de
  // tamanos distintos, asi que en 64 bits lleva relleno, y el relleno de un record sin
  // inicializar no vale como parte de una clave.
  FServiceInstances := TOrderedDictionary<TServiceIdentity, IInterface>.Create(
    TEqualityComparer<TServiceIdentity>.Construct(
      function(const A, B: TServiceIdentity): Boolean
      begin
        Result := A = B;
      end,
      function(const Value: TServiceIdentity): Integer
      begin
        Result := Integer(Value.Registration) xor Integer(NativeInt(Value.ClosedType));
      end));
  FIsSingletonsScope := SingletonsScope;
  FCreatingSingletons := TStack<TServiceDescriptor>.Create;
  FServiceCollection := Factory.ServiceCollection;
end;

destructor TServiceScope.Destroy;
begin
  // Just in case this Scope is temporal, and we not are in Shutdown
  Shutdown;
  FreeAndNil(FServiceInstances);
  FreeAndNil(FCreatingSingletons);
  inherited;
end;

procedure TServiceScope.AddServiceInstance(const Descriptor: TServiceDescriptor; const Service: IInterface);
begin
  FServiceInstances.Add(GetServiceIdentity(Descriptor), Service);
end;

function TServiceScope.GetOrCreate(const Descriptor: TServiceDescriptor; const Provider: IServiceProvider): IInterface;
begin
  var ServiceIdentity := GetServiceIdentity(Descriptor);
  if FServiceInstances.TryGetValue(ServiceIdentity, Result) then Exit;
  Result := CreateInstance(Descriptor, Provider);
  AddServiceInstance(Descriptor, Result);
end;

function TServiceScope.CreateInstance(const Descriptor: TServiceDescriptor; const Provider: IServiceProvider): IInterface;
begin
  var
  Intf := Descriptor.Factory(Provider);

  if not Supports(Intf, Descriptor.TypeInfo.TypeData.GUID, Result) then
    raise Exception.CreateFmt('% don''t implements %s', [TObject(Intf).QualifiedClassName, Descriptor.ServiceName]);
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
  Result := FScopeFactory.SingletonsScope as IServiceResolver;
end;

procedure TServiceScope.CollectGarbage;
begin
  var UnusedFound: Boolean;
  repeat
    UnusedFound := False;
    for var idx := FServiceInstances.Count - 1 downto 0 do
    begin
      // ValueList[idx] hands back the IInterface BY VALUE, so the compiler emits an
      // _AddRef for the temporary AND the matching _Release when that temporary dies.
      // Calling _Release on it was therefore one release the scope never took: every
      // entry visited lost a count it never gained, and the Clear below then dropped
      // the one the scope did hold -- so a service referenced once by the scope was
      // released twice and destroyed while its caller was still using it.
      // Reading into a local and restoring the count keeps the read balanced. The
      // threshold does not move: with the local alive the count is dictionary + local
      // = 2, and _Release leaves it at 1, exactly as before. The local also keeps the
      // entry alive across the Delete below, so the last reference can no longer drop
      // while the dictionary is still holding it.
      var Svc: IInterface := FServiceInstances.ValueList[idx];
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
end;

procedure TServiceScope.ShutdownContainer;
begin
  // El ambito alcanza la fabrica por FScopeFactory, que es [Weak]: la ve sin poseerla. Esta es
  // la costura por la que IServiceProvider.ShutDown llega al contenedor sin resolver nada.
  if FScopeFactory <> nil then
    FScopeFactory.Shutdown;
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

function TServiceScope.CanResolve(const ServiceType: PTypeInfo): Boolean;
begin
  Result := ServiceCollection.Contains(ServiceType);
  if Result then Exit;
  // Y esto no es un adorno: TActivatorUtilities pregunta aqui ANTES de elegir constructor. Si
  // el emparejamiento sin fijar no se reflejase, el constructor que pide un cierre no se
  // elegiria, la clase se construiria con el de TObject y quedaria a medias -- en verde y sin
  // excepcion, que es el peor sitio donde dejar esto sin contestar.
  var OpenType: PTypeInfo;
  Result := ServiceCollection.TryFindOpenGeneric(ServiceType, OpenType);
end;

function TServiceScope.TryGetDescriptors(const ServiceType: PTypeInfo; out Descriptors: TServiceDescriptors;
  out MatchedOpen: Boolean): Boolean;
begin
  // El embudo por el que pasan TryGet y TryGetAll, y por tanto toda resolucion. La guarda vive
  // aqui y no en las firmas publicas del record: alli habria que escribirla ocho veces.
  CheckNotShutDown(ServiceType);
  Descriptors := nil;
  MatchedOpen := False;

  if ServiceCollection.Contains(ServiceType) then
  begin
    Descriptors := ServiceCollection.GetDescriptors(ServiceType);
    Exit(True);
  end;

  // El emparejamiento entra al FALLAR la busqueda directa, y ese orden ES el requisito: un
  // registro concreto para un cierre gana al registro sin fijar.
  var OpenType: PTypeInfo;
  Result := ServiceCollection.TryFindOpenGeneric(ServiceType, OpenType);
  if not Result then Exit;
  MatchedOpen := True;
  Descriptors := ServiceCollection.GetDescriptors(OpenType);
end;

function TServiceScope.DescriptorFor(const ServiceType: PTypeInfo; const Descriptor: TServiceDescriptor;
  const MatchedOpen: Boolean): TServiceDescriptor;
begin
  if MatchedOpen then
    Result := Descriptor.ForClosedType(ServiceType)
  else
    Result := Descriptor;
end;

function TServiceScope.TryGet(const ServiceType: PTypeInfo; out Service): Boolean;
begin
  Pointer(Service) := nil;
  var
    Descriptors: TServiceDescriptors;
  var MatchedOpen: Boolean;
  Result := TryGetDescriptors(ServiceType, Descriptors, MatchedOpen);
  if not Result then
    Exit;
  var
  Descriptor := DescriptorFor(ServiceType, Descriptors.Last, MatchedOpen);
  IInterface(Service) := GetImplementor(Descriptor);
end;

function TServiceScope.TryGetAll(const ServiceType: PTypeInfo; out Services: IInterfaceList): Boolean;
begin
  Services := TInterfaceList<IInterface>.Create;

  var Descriptors: TServiceDescriptors;
  var MatchedOpen: Boolean;
  Result := TryGetDescriptors(ServiceType, Descriptors, MatchedOpen);
  if not Result then
    Exit;

  for var Descriptor in Descriptors do
    Services.Add(GetImplementor(DescriptorFor(ServiceType, Descriptor, MatchedOpen)));
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
  // El mensaje nombra lo que se pidio: sin depurador comodo, un "esta cerrado" a secas obliga
  // a adivinar quien preguntaba.
  raise EContainerShutDownError.CreateFmt('Cannot resolve %s: the container has been shut down',
    [_T.NameOf(ServiceType)]);
end;

procedure TServiceScope.BeginCreateSingleton(const Descriptor: TServiceDescriptor);
begin
  FCreatingSingletons.Push(Descriptor);
end;

procedure TServiceScope.EndCreateSingleton;
begin
  FCreatingSingletons.Pop;
end;

function TServiceScope.GetImplementor(const Descriptor: TServiceDescriptor): IInterface;
begin
  case Descriptor.LifeTime of
    Singleton: try
      BeginCreateSingleton(Descriptor);
      Result := SingletonsScope.GetOrCreate(Descriptor, Self);
    finally
      EndCreateSingleton;
    end;
    Scoped: begin
      if not FCreatingSingletons.IsEmpty then
        raise Exception.CreateFmt('singleton %s cannot depends on scoped %s ',[FCreatingSingletons.Peek.ServiceName, Descriptor.ServiceName]);
      Result := Self.GetOrCreate(Descriptor, Self);
    end;
    Transient: Result := Self.CreateInstance(Descriptor, Self);
  end;
end;

end.
