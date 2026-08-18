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

  TServiceScope = class(TDIObject<TServiceScope>, IServiceScope, IServiceProviderImpl, IServiceResolver)
  private
    [Weak]
    FScopeFactory: TServiceScopeFactory;
    [Weak]
    FServiceCollection: IServiceCollectionImpl;
    FServiceInstances : TOrderedDictionary<string, IInterface>;
    FCreatingSingletons: TStack<TServiceDescriptor>;
    FIsSingletonsScope: Boolean;
    FShutDonwnDone: Boolean;
    function GetServiceIdentity(Descriptor: TServiceDescriptor): TServiceIdentity;
    function CreateInstance(const Descriptor: TServiceDescriptor; const Provider: IServiceProvider): IInterface;
    function GetOrCreate(const Descriptor: TServiceDescriptor; const Provider: IServiceProvider): IInterface;overload;
    procedure BeginCreateSingleton(const Descriptor: TServiceDescriptor);
    procedure EndCreateSingleton;
    function GetImplementor(const Descriptor: TServiceDescriptor): IInterface;
    function TryGet(const ServiceType: PTypeInfo; out Service): Boolean;
    function TryGetAll(const ServiceType: PTypeInfo; out Services: IInterfaceList): Boolean;
    function TryGetDescriptors(const ServiceType: PTypeInfo; out Descriptors: TServiceDescriptors): Boolean;
    function GetSingletonsScope: IServiceResolver;
    procedure AddServiceInstance(const Descriptor: TServiceDescriptor; const Service: IInterface);
    function CanResolve(const ServiceType: PTypeInfo): Boolean;
    procedure CollectGarbage;
  protected
    property IsSingletonsScope: Boolean read FIsSingletonsScope;
    property ScopeFactory: TServiceScopeFactory read FScopeFactory;
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
  Daf.Types,
  Daf.Rtti,
  Daf.DependencyInjection.ActivatorUtilities;

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
end;

destructor TServiceCollection.Destroy;
begin
  FVisited.Free;
  FEntries.Free;
  inherited;
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
    [Descriptor.ServiceName, Descriptor.ImplementationClass.ClassName, Failed.ParamType.QualifiedName, Failed.Name]);
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
  for var idx := High(Scopes) to Low(Scopes) do
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
  FServiceInstances := TOrderedDictionary<string, IInterface>.Create;
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

function TServiceScope.GetServiceIdentity(Descriptor: TServiceDescriptor): TServiceIdentity;
begin
  Result := Descriptor.ServiceName;
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
      // there are an implicit call to _AddRef in ValueList access
      var RefCount := FServiceInstances.ValueList[idx]._Release;
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
end;

function TServiceScope.TryGetDescriptors(const ServiceType: PTypeInfo; out Descriptors: TServiceDescriptors): Boolean;
begin
  Descriptors := nil;
  Result := CanResolve(ServiceType);
  if not Result then
    Exit;
  Descriptors := ServiceCollection.GetDescriptors(ServiceType);
end;

function TServiceScope.TryGet(const ServiceType: PTypeInfo; out Service): Boolean;
begin
  Pointer(Service) := nil;
  var
    Descriptors: TServiceDescriptors;
  Result := TryGetDescriptors(ServiceType, Descriptors);
  if not Result then
    Exit;
  var
  Descriptor := ServiceCollection.GetDescriptors(ServiceType).Last;
  IInterface(Service) := GetImplementor(Descriptor);
end;

function TServiceScope.TryGetAll(const ServiceType: PTypeInfo; out Services: IInterfaceList): Boolean;
begin
  Services := TInterfaceList<IInterface>.Create;

  var Descriptors: TServiceDescriptors;
  Result := TryGetDescriptors(ServiceType, Descriptors);
  if not Result then
    Exit;

  for var Descriptor in Descriptors do
    Services.Add(GetImplementor(Descriptor));
end;

function TServiceScope.CreateScope: IServiceScope;
begin
  Result := FScopeFactory.CreateScope;
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
