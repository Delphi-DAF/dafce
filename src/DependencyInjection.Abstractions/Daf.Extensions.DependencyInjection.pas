unit Daf.Extensions.DependencyInjection;

interface

uses
  System.SysUtils,
  System.Rtti,
  System.Generics.Collections,
  System.TypInfo,
  Daf.Enumerable;

type
  IServiceProviderImpl = interface;
  IServiceScope = interface;
  IAppModule = interface;

  EServiceProviderError = class(Exception)
  end;

  // Pedirle algo a un contenedor ya cerrado. Deriva de EServiceProviderError para no romper a
  // quien ya captura el padre, y es un tipo propio porque el llamante necesita separar "esto no
  // esta registrado" -- que se arregla en la composicion -- de "esto ya esta cerrado", que se
  // arregla en el ciclo de vida. Es el ObjectDisposedException de .NET.
  EContainerShutDownError = class(EServiceProviderError)
  end;

  // Interfaces don't support generic overload, so we model our desired interface using a record
  // that will delegate behavior in a true interface that implementor classes will support
  IServiceProvider = record
  strict private
    FImplementor: IServiceProviderImpl;
    class function Error(const Fmt: string; const Args: array of const): EServiceProviderError; static;
  private
    function GetFromInterfaceOps(const ServiceType: PTypeInfo): TArray<TRttiMethod>;
  public
    class operator Equal(S: IServiceProvider; P: Pointer): Boolean;
    class operator NotEqual(S: IServiceProvider; P: Pointer): Boolean;
    class operator Implicit(const Implementor: IServiceProviderImpl): IServiceProvider;
    class operator Implicit(const Provider: IServiceProvider): IServiceProviderImpl;
    function AsType<ServiceType: IInterface>: ServiceType;

    function CanResolve(const ServiceType: PTypeInfo): Boolean;

    procedure GetService(const ServiceType: PTypeInfo; out Service); overload;
    function GetService<ServiceType: IInterface>: ServiceType; overload;

    procedure GetRequiredService(const ServiceType: PTypeInfo; out Service);overload;
    function GetRequiredService<ServiceType>: ServiceType;overload;

    function GetServices(const ServiceType: PTypeInfo): IInterfaceList<IInterface>;overload;
    function GetServices<ServiceType: IInterface>: IInterfaceList<ServiceType>; overload;

    function TryGet(const ServiceType: PTypeInfo; out Service): Boolean;overload;
    function TryGet<ServiceType: IInterface>(out Service): Boolean;overload;
    function CreateScope: IServiceScope;
    procedure ShutDown;
  end;

  TServiceFactory<ServiceType: IInterface> = reference to function(Provider: IServiceProvider): ServiceType;
  TServiceFactory = TServiceFactory<IInterface>;
  // La misma factoria, pero enterada de que se le esta pidiendo: recibe, ademas del proveedor, el
  // tipo concreto que se resuelve -- el cierre en un registro sin fijar, el tipo del servicio en
  // uno corriente. Es un tipo APARTE y no un parametro opcional del de arriba: un valor por defecto
  // sirve al sitio de llamada, no a la compatibilidad de asignacion, y una lambda de un parametro
  // asignada a una de dos da E2010. Asi que la via es una sobrecarga, no un parametro mas.
  TServiceFactoryEx<ServiceType: IInterface> = reference to function(Provider: IServiceProvider;
    RequestedType: PTypeInfo): ServiceType;
  TServiceFactoryEx = TServiceFactoryEx<IInterface>;
  Factory = class
  public
    class function From(const ServiceType: PTypeInfo; const TImpl: TClass): TServiceFactory;overload;
    class function From(const ServiceType: PTypeInfo; const Implementor: IInterface): TServiceFactory;overload;
    /// <summary>
    ///   La fabrica POR DEFECTO de un registro sin fijar hecho por clase, ya atada a un cierre
    ///   concreto. No se puede crear al registrar --entonces todavia no se sabe que cierres van
    ///   a pedirse-- sino al emparejar, que es cuando el tipo cerrado esta delante.
    /// </summary>
    class function FromOpen(const OpenType, ClosedType: PTypeInfo; const TImpl: TClass): TServiceFactory;
    /// <summary>
    ///   Cualquier fabrica alcanzada por emparejamiento, con la comprobacion de que entregar lo
    ///   que produce para ese cierre es seguro. Envuelve por igual la fabrica por defecto que
    ///   calcula un registro por clase y una escrita a mano: registrar por clase es la forma
    ///   abreviada de registrar por factory, asi que una comprobacion que solo cubra la primera
    ///   deja pasar la mitad de los casos.
    /// </summary>
    class function ForClosedType(const OpenType, ClosedType: PTypeInfo;
      const Inner: TServiceFactory): TServiceFactory;
    /// <summary>
    ///   Baja una factoria consciente del tipo pedido a una de un solo parametro, capturando el
    ///   tipo que se le entregara. Es la UNICA forma de que una consciente llegue a llamarse: nunca
    ///   se invoca directa, siempre envuelta asi. Se llama con el tipo del servicio al registrar uno
    ///   corriente (D4), y con el cierre al cerrar un registro sin fijar (D3).
    /// </summary>
    class function ForRequestedType(const RequestedType: PTypeInfo;
      const Aware: TServiceFactoryEx): TServiceFactory;
  end;

  // Envuelve el servicio que produce el registro previo. Es Scrutor: en .NET esto no
  // esta en el framework, lo trae una libreria encima de IServiceCollection.
  TServiceDecorator<ServiceType: IInterface> = reference to function(const Inner: ServiceType;
    const Provider: IServiceProvider): ServiceType;
  TServiceDecorator = TServiceDecorator<IInterface>;

  TServiceLifeTime = (Singleton, Scoped, Transient);

  // Lo que hace que dos resoluciones devuelvan la misma instancia, y son DOS ejes porque
  // hacen falta los dos.
  //
  // El REGISTRO, y no el nombre del tipo: dos registros del mismo tipo de servicio son dos
  // instancias, que es lo que hace util pedirlos todos.
  //
  // Y el TIPO CERRADO, para un registro que dejo sus argumentos genericos sin fijar: ese es
  // UN SOLO registro para todos sus cierres, asi que con el primer eje a secas todos
  // compartirian objeto. Y el fallo se lee como el acierto -- hay instancia, responde, y
  // trae los datos de otro cierre. Vale nil en un registro corriente.
  TServiceIdentity = record
    Registration: NativeInt;
    ClosedType: PTypeInfo;
    class operator Equal(const A, B: TServiceIdentity): Boolean;
    class operator NotEqual(const A, B: TServiceIdentity): Boolean;
  end;

  // Calcado de ServiceProviderOptions de .NET. Ambas comprobaciones son opt-in: una
  // validacion que bloquea un arranque que funcionaba cuesta mas de lo que ahorra.
  TServiceProviderOptions = record
  public
    ValidateOnBuild: Boolean;
    ValidateScopes: Boolean;
    constructor Create(const AValidateOnBuild, AValidateScopes: Boolean);
    class function None: TServiceProviderOptions; static;
  end;

  IContainerAccess = interface(IInvokable)
    ['{606B658C-C5EB-48D6-B447-D9A19CBB31F5}']
    procedure Shutdown;
  end;

  TServiceDescriptor = record
  strict private
    FFactory: TServiceFactory;
    FLifeTime: TServiceLifeTime;
    FTypeInfo: PTypeInfo;
    FImplementationClass: TClass;
    FIdentity: TServiceIdentity;
    // La factoria consciente del tipo pedido, si el registro se hizo con una; nil si no.
    // Es lo que ForClosedType necesita para volver a bajarla con el cierre concreto (D3).
    FRequestedFactory: TServiceFactoryEx;
  public
    constructor Create(const Factory: TServiceFactory; const LifeTime: TServiceLifeTime; const TypeInfo: PTypeInfo;
      const ImplementationClass: TClass = nil; const RequestedFactory: TServiceFactoryEx = nil);
    // Nombre legible del tipo de servicio, y para nada mas que un mensaje de error.
    // No decide nada y no lanza: ver la implementacion.
    function ServiceName: string;
    /// <summary>
    ///   Este mismo registro, atado al cierre que se acaba de pedir. Es lo que convierte UN
    ///   registro sin fijar en un registro por tipo cerrado: identidad propia --y con ella
    ///   ciclo de vida propio-- y una fabrica que sabe para que cierre construye.
    /// </summary>
    function ForClosedType(const AClosedType: PTypeInfo): TServiceDescriptor;
    // El cierre por el que se alcanzo este registro, o nil si se alcanzo directamente.
    function ClosedType: PTypeInfo;
    // Que registro es este. Se asigna al construirse y viaja con las copias, porque
    // Descriptors reparte copias y todas tienen que seguir siendo el mismo registro.
    // Es unico dentro del proceso; no es estable entre ejecuciones ni es una clave de
    // servicio con nombre, asi que no sirve para buscar nada.
    property Identity: TServiceIdentity read FIdentity;
    property Factory: TServiceFactory read FFactory;
    property LifeTime: TServiceLifeTime read FLifeTime;
    property TypeInfo: PTypeInfo read FTypeInfo;
    // Clase de implementacion si el registro se hizo por clase; nil si se hizo por factory.
    // Es lo unico que la validacion del grafo puede inspeccionar: una lambda es opaca.
    property ImplementationClass: TClass read FImplementationClass;
  end;
  TServiceDescriptors = TList<TServiceDescriptor>;

  // misteriosamente si uso interface(IInvokable) se produce E2134 Type'<void>' has no type info
{$M+}
  IServiceProviderImpl = interface
    ['{D2043968-561D-4666-8A36-D2714BC262C4}']
    function CanResolve(const ServiceType: PTypeInfo): Boolean;
    function TryGet(const ServiceType: PTypeInfo; out Service): Boolean;
    function TryGetAll(const ServiceType: PTypeInfo; out Services: IInterfaceList): Boolean;
    function CreateScope: IServiceScope;
  end;
{$M-}

  IServiceCollectionImpl = interface(IInvokable)
    ['{C767E76F-F5AE-438D-9725-B624E704127B}']
    function GetCount: Integer;
    procedure CheckNotFrozen;
    function Add(const ServiceType: PTypeInfo; const LifeTime: TServiceLifeTime; const ServiceFactory: TServiceFactory;
      const ImplClass: TClass = nil; const RequestedFactory: TServiceFactoryEx = nil): IServiceCollectionImpl; overload;
    function Contains(const ServiceType: PTypeInfo): Boolean;
    function BuildServiceProvider: IServiceProviderImpl; overload;
    function BuildServiceProvider(const Options: TServiceProviderOptions): IServiceProviderImpl; overload;
    function GetDescriptors(ServiceType: PTypeInfo): TServiceDescriptors;
    function SetDescriptors(const ServiceType: PTypeInfo; const NewDescriptors: TArray<TServiceDescriptor>): IServiceCollectionImpl;
    function RemoveAll(const ServiceType: PTypeInfo): IServiceCollectionImpl;
    // El registro sin fijar que puede servir a este cierre, si lo hay. Se pregunta al FALLAR
    // la busqueda directa y nunca antes: un registro concreto para un cierre tiene que seguir
    // mandando sobre el registro sin fijar.
    function TryFindOpenGeneric(const ClosedType: PTypeInfo; out OpenType: PTypeInfo): Boolean;
    // Contabilidad de "esto ya se proceso", por coleccion y no por proceso: componer
    // dos veces en un proceso tiene que dar el mismo resultado.
    function TryMarkVisited(const Category, Key: string): Boolean;
  end;

  IServiceScope = interface(IInvokable)
    ['{E0B2F2D2-BAF3-4C6F-9B22-4A9B2B3A5C4D}']
    function ServiceProvider: IServiceProvider;
  end;

  IServiceScopeFactory = interface(IInvokable)
    ['{C3F2E1D4-7A6C-4B8E-9B12-5D8A6A2D9F3E}']
    function CreateScope: IServiceScope;
  end;

  IServiceCollection = record
  strict private
    FImplementor: IServiceCollectionImpl;
    function GetCount: Integer;
  private
    function AddDescriptor(const LifeTime: TServiceLifeTime; const ServiceType: PTypeInfo;
      const ServiceFactory: TServiceFactory; const ImplClass: TClass;
      const RequestedFactory: TServiceFactoryEx = nil): IServiceCollection;
    function AddAwareDescriptor(const LifeTime: TServiceLifeTime; const ServiceType: PTypeInfo;
      const Aware: TServiceFactoryEx): IServiceCollection;
    function CanTryAdd(const ServiceType: PTypeInfo): Boolean;
  public
    class operator Implicit(const Implementor: IServiceCollectionImpl): IServiceCollection;
    class operator Implicit(const ServiceCollection: IServiceCollection): IServiceCollectionImpl;
    function BuildServiceProvider: IServiceProvider; overload;
    function BuildServiceProvider(const Options: TServiceProviderOptions): IServiceProvider; overload;

    function Contains<ServiceType: IInterface>: Boolean;overload;
    function Contains(const ServiceType: PTypeInfo): Boolean;overload;

    // Solo lectura: es una copia, no la lista viva que posee la coleccion. Habilita
    // escribir composicion propia (TryAdd, Replace, Decorate) desde fuera de DAF.
    function Descriptors(const ServiceType: PTypeInfo): TArray<TServiceDescriptor>; overload;
    function Descriptors<ServiceType: IInterface>: TArray<TServiceDescriptor>; overload;

    function Add(const LifeTime: TServiceLifeTime; const ServiceType: PTypeInfo; const Factory: TServiceFactory): IServiceCollection; overload;
    function Add<ServiceType: IInterface>(const LifeTime: TServiceLifeTime; const Factory: TServiceFactory<ServiceType>)
      : IServiceCollection; overload;
    function Add<ServiceType: IInterface>(const LifeTime: TServiceLifeTime;
      const Factory: TServiceFactoryEx<ServiceType>): IServiceCollection; overload;

    function AddSingleton(const ServiceType: PTypeInfo; const Factory: TServiceFactory): IServiceCollection; overload;
    function AddSingleton<ServiceType: IInterface>(const Factory: TServiceFactory<ServiceType>): IServiceCollection; overload;
    function AddSingleton<ServiceType: IInterface>(const Factory: TServiceFactoryEx<ServiceType>): IServiceCollection; overload;

    function AddSingleton(const ServiceType: PTypeInfo; const TImpl: TClass): IServiceCollection;overload;
    function AddSingleton<ServiceType: IInterface; TImpl: ServiceType>: IServiceCollection; overload;

    function AddSingleton(const ServiceType: PTypeInfo; const Implementor: IInterface): IServiceCollection; overload;
    function AddSingleton<ServiceType: IInterface>(const Implementor: ServiceType): IServiceCollection; overload;

    function AddTransient(const ServiceType: PTypeInfo; const Factory: TServiceFactory): IServiceCollection; overload;
    function AddTransient(const ServiceType: PTypeInfo; const TImpl: TClass): IServiceCollection;overload;
    function AddTransient<ServiceType: IInterface>(const Factory: TServiceFactory<ServiceType>): IServiceCollection; overload;
    function AddTransient<ServiceType: IInterface>(const Factory: TServiceFactoryEx<ServiceType>): IServiceCollection; overload;
    function AddTransient<ServiceType: IInterface; TImpl: ServiceType>: IServiceCollection; overload;

    function AddScoped(const ServiceType: PTypeInfo; const Factory: TServiceFactory): IServiceCollection; overload;
    function AddScoped(const ServiceType: PTypeInfo; const TImpl: TClass): IServiceCollection;overload;
    function AddScoped<ServiceType: IInterface>(const Factory: TServiceFactory<ServiceType>): IServiceCollection; overload;
    function AddScoped<ServiceType: IInterface>(const Factory: TServiceFactoryEx<ServiceType>): IServiceCollection; overload;
    function AddScoped<ServiceType: IInterface; TImpl: ServiceType>: IServiceCollection; overload;

    // TryAdd*: registra solo si nadie registro ya ese tipo de servicio. Es el contrato
    // "la libreria pone el default, la aplicacion manda" de ServiceCollectionDescriptorExtensions.
    function TryAddSingleton(const ServiceType: PTypeInfo; const Factory: TServiceFactory): IServiceCollection; overload;
    function TryAddSingleton(const ServiceType: PTypeInfo; const TImpl: TClass): IServiceCollection; overload;
    function TryAddSingleton(const ServiceType: PTypeInfo; const Implementor: IInterface): IServiceCollection; overload;
    function TryAddSingleton<ServiceType: IInterface>(const Factory: TServiceFactory<ServiceType>): IServiceCollection; overload;
    function TryAddSingleton<ServiceType: IInterface>(const Factory: TServiceFactoryEx<ServiceType>): IServiceCollection; overload;
    function TryAddSingleton<ServiceType: IInterface>(const Implementor: ServiceType): IServiceCollection; overload;
    function TryAddSingleton<ServiceType: IInterface; TImpl: ServiceType>: IServiceCollection; overload;

    function TryAddScoped(const ServiceType: PTypeInfo; const Factory: TServiceFactory): IServiceCollection; overload;
    function TryAddScoped(const ServiceType: PTypeInfo; const TImpl: TClass): IServiceCollection; overload;
    function TryAddScoped<ServiceType: IInterface>(const Factory: TServiceFactory<ServiceType>): IServiceCollection; overload;
    function TryAddScoped<ServiceType: IInterface>(const Factory: TServiceFactoryEx<ServiceType>): IServiceCollection; overload;
    function TryAddScoped<ServiceType: IInterface; TImpl: ServiceType>: IServiceCollection; overload;

    function TryAddTransient(const ServiceType: PTypeInfo; const Factory: TServiceFactory): IServiceCollection; overload;
    function TryAddTransient(const ServiceType: PTypeInfo; const TImpl: TClass): IServiceCollection; overload;
    function TryAddTransient<ServiceType: IInterface>(const Factory: TServiceFactory<ServiceType>): IServiceCollection; overload;
    function TryAddTransient<ServiceType: IInterface>(const Factory: TServiceFactoryEx<ServiceType>): IServiceCollection; overload;
    function TryAddTransient<ServiceType: IInterface; TImpl: ServiceType>: IServiceCollection; overload;

    // Replace quita los registros previos del tipo y anade el nuevo; RemoveAll solo quita.
    function Replace(const LifeTime: TServiceLifeTime; const ServiceType: PTypeInfo; const Factory: TServiceFactory): IServiceCollection; overload;
    function Replace(const LifeTime: TServiceLifeTime; const ServiceType: PTypeInfo; const TImpl: TClass): IServiceCollection; overload;
    function Replace<ServiceType: IInterface>(const LifeTime: TServiceLifeTime; const Factory: TServiceFactory<ServiceType>): IServiceCollection; overload;
    function Replace<ServiceType: IInterface; TImpl: ServiceType>(const LifeTime: TServiceLifeTime): IServiceCollection; overload;

    function RemoveAll(const ServiceType: PTypeInfo): IServiceCollection; overload;
    function RemoveAll<ServiceType: IInterface>: IServiceCollection; overload;

    // Envuelve los registros existentes del tipo conservando su ciclo de vida. Para envolver
    // comportamiento (logging, cache, reintentos): los efectos laterales de arranque son IStartupTask.
    function Decorate(const ServiceType: PTypeInfo; const Decorator: TServiceDecorator): IServiceCollection; overload;
    function Decorate<ServiceType: IInterface>(const Decorator: TServiceDecorator<ServiceType>): IServiceCollection; overload;

    // Aplica un modulo en el acto, no de forma diferida: la composicion es una sola
    // secuencia legible, y un TryAdd escrito despues de un modulo se ejecuta despues.
    function AddModule<T: class, IAppModule, constructor>: IServiceCollection; overload;
    function AddModule(const Module: IAppModule): IServiceCollection; overload;

    function TryMarkVisited(const Category, Key: string): Boolean;
    property Count: Integer read GetCount;
  end;

  // El modulo no es invencion de DAF: es Autofac.Module, IWindsorInstaller de Castle Windsor,
  // Registry de StructureMap. Vive aqui, y no en su propia unit, porque AddModule<T> es un
  // metodo del record y no puede depender de una unit que a su vez depende de esta.
  IAppModule = interface(IInvokable)
    ['{3FA56942-21D1-4B08-ABD4-F4F82F48F36C}']
    procedure AddServices(const ServiceCollection: IServiceCollection);
  end;

  TLazy<T: IInvokable> = class(TVirtualInterface)
  private
    FServiceProvider: IServiceProvider;
    FDelegated: T;
    FType: PTypeInfo;
    FFactory: TServiceFactory<T>;
    procedure CallInterceptor(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue);
    procedure CallDelegated(Method: TRttiMethod; const Args: TArray<TValue>; var Result: TValue);
    procedure CheckDelegatedReady;
  public
    constructor Create(ServiceProvider: IServiceProvider; const Factory: TServiceFactory<T>);
  end;

  Lazy = record
    class function From<T: IInvokable>(Provider: IServiceProvider; const Factory: TServiceFactory<T>): T;static;
  end;

  TDuckDecorator<T: IInvokable> = class(TVirtualInterface)
  private
    FDecorated: T;
    FDecoratedType: PTypeInfo;
    procedure CallInterceptor(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue);
  protected
    procedure CallDecorated(Method: TRttiMethod; const Args: TArray<TValue>; var Result: TValue);
    function CallByNameTo(const Instance: TObject; MethodName: string; const Args: TArray<TValue>; var CallResult: TValue): Boolean;
    procedure BeforeCall(Method: TRttiMethod; const Args: TArray<TValue>; var Result: TValue); virtual;
    procedure AfterCall(Method: TRttiMethod; const Args: TArray<TValue>; var Result: TValue); virtual;
    constructor InternalCreate(const DecoratedType: PTypeInfo; const Decorated: T);
  public
    constructor Create(Decorated: T); overload;
    constructor Create; overload;
    destructor Destroy; override;
    property Decorated: T read FDecorated write FDecorated;
    property DecoratedType: PTypeInfo read FDecoratedType;
  end;

  TDuckDecorator = class(TDuckDecorator<IInvokable>)
  public
    constructor Create(const DecoratedType: PTypeInfo; Decorated: IInvokable = nil);
  end;

// Nombre de un tipo de servicio PARA UN MENSAJE DE ERROR, y por eso no puede lanzar. Se
// compone dentro de la ruta de error, donde una excepcion no es un fallo mas: sustituye el
// que se estaba contando y el original se pierde.
function ServiceTypeName(const ServiceType: PTypeInfo): string;

implementation

uses
  Daf.Rtti,
  Daf.MemUtils,
  Daf.Activator,
  Daf.DependencyInjection.ActivatorUtilities,
  Daf.DependencyInjection.OpenGenerics;

var
  // Contador de proceso, y aqui si vale: la identidad de un registro solo tiene que ser
  // UNICA, no reproducible entre ejecuciones ni entre colecciones -- al reves que la
  // contabilidad de "esto ya se proceso", que por eso vive en la coleccion. Empieza en 1
  // para que el 0 de un TServiceDescriptor sin construir no se confunda con un registro.
  GLastServiceIdentity: Integer = 0;

{ TServiceProviderOptions }

constructor TServiceProviderOptions.Create(const AValidateOnBuild, AValidateScopes: Boolean);
begin
  ValidateOnBuild := AValidateOnBuild;
  ValidateScopes := AValidateScopes;
end;

class function TServiceProviderOptions.None: TServiceProviderOptions;
begin
  Result := Default(TServiceProviderOptions);
end;

{ TServiceIdentity }

class operator TServiceIdentity.Equal(const A, B: TServiceIdentity): Boolean;
begin
  Result := (A.Registration = B.Registration) and (A.ClosedType = B.ClosedType);
end;

class operator TServiceIdentity.NotEqual(const A, B: TServiceIdentity): Boolean;
begin
  Result := not (A = B);
end;

{ TServiceDescriptor }

constructor TServiceDescriptor.Create(const Factory: TServiceFactory; const LifeTime: TServiceLifeTime;
  const TypeInfo: PTypeInfo; const ImplementationClass: TClass; const RequestedFactory: TServiceFactoryEx);
begin
  FFactory := Factory;
  FLifeTime := LifeTime;
  FTypeInfo := TypeInfo;
  FImplementationClass := ImplementationClass;
  FIdentity.Registration := AtomicIncrement(GLastServiceIdentity);
  FIdentity.ClosedType := nil;
  FRequestedFactory := RequestedFactory;
 end;

function TServiceDescriptor.ClosedType: PTypeInfo;
begin
  Result := FIdentity.ClosedType;
end;

function TServiceDescriptor.ForClosedType(const AClosedType: PTypeInfo): TServiceDescriptor;
begin
  Result := Self;
  // El registro sigue siendo el mismo -- Registration no cambia -- y lo que se le anade es el
  // eje que le faltaba. Dos cierres del mismo registro son dos identidades, y por tanto dos
  // instancias; el mismo cierre dos veces es una.
  Result.FIdentity.ClosedType := AClosedType;
  // Una fabrica escrita a mano no puede saber para que cierre construye, asi que se queda como
  // esta; lo que se reemplaza es la fabrica POR DEFECTO que calculo el registro por clase, que
  // si puede -- ahi es donde entra el relleno de _ClosedArgs.
  // El tipo de Inner va escrito y no inferido: `var Inner := FFactory` sobre una referencia a
  // funcion la LLAMA en vez de copiarla, y el error que sale es E2035 por numero de parametros.
  var Inner: TServiceFactory := FFactory;
  // Y el nombre de unit delante tampoco sobra: dentro de este record, Factory es su propia
  // propiedad --la fabrica de ESTE registro-- y tapa la clase del mismo nombre.
  if FImplementationClass <> nil then
    Inner := Daf.Extensions.DependencyInjection.Factory.FromOpen(
      FTypeInfo, AClosedType, FImplementationClass)
  else if Assigned(FRequestedFactory) then
    // D3: una factoria consciente se baja aqui, el unico punto que conoce el cierre, a una de las
    // de siempre que le pasa ESE cierre como tipo pedido. FFactory llevaba la bajada con el tipo del
    // registro (el abierto); esta la sustituye con el cierre concreto. Es un brazo mas de la
    // bifurcacion que ya estaba aqui, no un if en el camino de resolucion.
    Inner := Daf.Extensions.DependencyInjection.Factory.ForRequestedType(AClosedType, FRequestedFactory);
  // Y la comprobacion del emparejamiento, sobre la que haya de las dos: es una propiedad del
  // par contrato-abierto/cierre, no de quien fabrique el objeto.
  Result.FFactory := Daf.Extensions.DependencyInjection.Factory.ForClosedType(
    FTypeInfo, AClosedType, Inner);
end;


function ServiceTypeName(const ServiceType: PTypeInfo): string;
begin
  // Se llega aqui porque algo ya iba mal, asi que el nombre cualificado se INTENTA, no se
  // exige: el simple queda puesto de entrada y solo lo sustituye el cualificado si sale.
  Result := string(ServiceType.Name);
  var RC := TRttiContext.Create;
  try
    try
      var RType := RC.GetType(ServiceType);
      if RType <> nil then
      begin
        // QualifiedName lanza ENonPublicType si el tipo no esta declarado en la interface de
        // una unit -- el caso de una instanciacion cerrada que solo vive dentro del cuerpo de
        // un metodo. Name si funciona sobre ese mismo tipo.
        // A una variable local y NO a Result: Delphi pasa Result como parametro oculto por
        // referencia, y una funcion que lanza ya lo ha vaciado antes de hacerlo. Escribiendo
        // directamente en Result, el nombre simple de arriba desaparece y el mensaje de error
        // acaba sin nombrar el servicio -- medido, y con la excepcion capturada igualmente.
        var Qualified := RType.QualifiedName;
        Result := Qualified;
      end;
    except
      // Se queda el nombre simple, que ya esta puesto. Tragarse la excepcion es el punto:
      // nombrar un servicio para un mensaje no puede abortar.
    end;
  finally
    RC.Free;
  end;
end;

function TServiceDescriptor.ServiceName: string;
begin
  Result := ServiceTypeName(FTypeInfo);
end;

{ IServiceProvider }

class operator IServiceProvider.Implicit(const Implementor: IServiceProviderImpl): IServiceProvider;
begin
  Result.FImplementor := Implementor;
end;

class operator IServiceProvider.Implicit(const Provider: IServiceProvider): IServiceProviderImpl;
begin
  Result := Provider.FImplementor;
end;

class function IServiceProvider.Error(const Fmt: string; const Args: array of const): EServiceProviderError;
begin
  Result := EServiceProviderError.CreateFmt(Fmt, Args);
end;

function IServiceProvider.TryGet(const ServiceType: PTypeInfo; out Service): Boolean;
begin
  Result := FImplementor.TryGet(ServiceType, Service);
  if Result then Exit;
  var Conversors := GetFromInterfaceOps(ServiceType);
  for var Conversor in Conversors do
  begin
    Result := FImplementor.TryGet(Conversor.GetParameters[0].ParamType.Handle, Service);
    if Result then
    begin
      var Arg: TValue;
      TValue.Make(@Service, Conversor.GetParameters[0].ParamType.Handle, Arg);
      var Instance: TValue;
      TValue.Make(nil, ServiceType, Instance);
      Instance := Conversor.Invoke(Instance, [Arg]);
      Instance.ExtractRawData(@Service);
      Exit;
    end;
  end;
end;

function IServiceProvider.TryGet<ServiceType>(out Service): Boolean;
begin
  Result := TryGet(TypeInfo(ServiceType), Service);
end;

procedure IServiceProvider.GetService(const ServiceType: PTypeInfo; out Service);
begin
  if not TryGet(ServiceType, Service) then
    IInterface(Service) := nil;
end;

function IServiceProvider.GetService<ServiceType>: ServiceType;
begin
  GetService(TypeInfo(ServiceType), Result);
end;

procedure IServiceProvider.GetRequiredService(const ServiceType: PTypeInfo; out Service);
begin
  if not TryGet(ServiceType, Service) then
    raise Error('Cannot resolve service: %s', [ServiceType.Name]);
end;

function IServiceProvider.GetRequiredService<ServiceType>: ServiceType;
begin
  GetRequiredService(TypeInfo(ServiceType), Result);
end;

function IServiceProvider.GetServices<ServiceType>: IInterfaceList<ServiceType>;
begin
  Result := IInterfaceList<ServiceType>(GetServices(TypeInfo(ServiceType)))
end;

function IServiceProvider.GetServices(const ServiceType: PTypeInfo): IInterfaceList<IInterface>;
begin
  FImplementor.TryGetAll(ServiceType, IInterfaceList(Result));
end;

function IServiceProvider.AsType<ServiceType>: ServiceType;
begin
  var PInfo: PTypeInfo :=  TypeInfo(ServiceType);
  Supports(FImplementor, PInfo.TypeData.Guid, Result);
end;

function IServiceProvider.GetFromInterfaceOps(const ServiceType: PTypeInfo): TArray<TRttiMethod>;
begin
  var FImpl := FImplementor;
  Result :=_T.GetConverFromOps(ServiceType,  function(M: TRttiMethod): Boolean
              begin
                var FromType := M.GetParameters[0].ParamType;
                Result := (FromType.TypeKind = tkInterface) and FImpl.CanResolve(FromType.Handle);
              end);
end;

function IServiceProvider.CanResolve(const ServiceType: PTypeInfo): Boolean;
begin
  Result := FImplementor.CanResolve(ServiceType) or (GetFromInterfaceOps(ServiceType) <> nil);
end;

function IServiceProvider.CreateScope: IServiceScope;
begin
  Result :=  FImplementor.CreateScope;
end;

class operator IServiceProvider.Equal(S: IServiceProvider; P: Pointer): Boolean;
begin
  Result := Pointer(S.FImplementor) = P;
end;

class operator IServiceProvider.NotEqual(S: IServiceProvider; P: Pointer): Boolean;
begin
  Result := Pointer(S.FImplementor) <> P;
end;

procedure IServiceProvider.ShutDown;
begin
  // Se llega al contenedor por QueryInterface, NO resolviendo.
  var Container: IContainerAccess;
  if Supports(FImplementor, IContainerAccess, Container) then
    Container.ShutDown;
end;

{ Factory }

class function Factory.From(const ServiceType: PTypeInfo; const TImpl: TClass): TServiceFactory;
begin
  Assert(Supports(TImpl, _T.GUID(ServiceType)), TImpl.Classname + 'no supports ' + _T.NameOf(ServiceType));
  // Un registro que deja sus argumentos sin fijar promete algo que la implementacion tiene que
  // poder cumplir, y si no puede eso es un error de composicion: se dice AQUI, al registrar, y
  // no cuando alguien resuelva un cierre en otro punto del programa. Es un Assert lo de arriba
  // y una excepcion lo de abajo a proposito -- aquello comprueba al programador, esto describe
  // un registro imposible, y tiene que seguir hablando en release.
  if TOpenGeneric.IsOpen(ServiceType) then
    TOpenGeneric.CheckImplementation(ServiceType, TImpl);
  Result := function(Provider: IServiceProvider): IInterface
            begin
              var Aux := TActivatorUtilities.CreateInstance(Provider, TImpl);
              Supports(Aux, _T.GUID(ServiceType), Result);
            end;
end;

class function Factory.ForClosedType(const OpenType, ClosedType: PTypeInfo;
  const Inner: TServiceFactory): TServiceFactory;
begin
  Result := function(Provider: IServiceProvider): IInterface
            begin
              TOpenGeneric.CheckMatch(OpenType, ClosedType);
              Result := Inner(Provider);
            end;
end;

class function Factory.ForRequestedType(const RequestedType: PTypeInfo;
  const Aware: TServiceFactoryEx): TServiceFactory;
begin
  Result := function(Provider: IServiceProvider): IInterface
            begin
              Result := Aware(Provider, RequestedType);
            end;
end;

class function Factory.FromOpen(const OpenType, ClosedType: PTypeInfo; const TImpl: TClass): TServiceFactory;
begin
  Result := function(Provider: IServiceProvider): IInterface
            begin
              var Aux := TOpenGeneric.Activate(Provider, TImpl, OpenType, ClosedType);
              // El GUID declarado lo comparten TODAS las instanciaciones de una familia, asi
              // que la unica clase compilada -- la del marcador -- satisface tambien el cierre.
              // Que eso sea seguro es lo que Activate acaba de comprobar metodo a metodo.
              if not Supports(Aux, _T.GUID(ClosedType), Result) then
              begin
                Aux.Free;
                raise EServiceProviderError.CreateFmt('%s does not implement %s',
                  [TImpl.ClassName, ServiceTypeName(ClosedType)]);
              end;
            end;
end;

class function Factory.From(const ServiceType: PTypeInfo;  const Implementor: IInterface): TServiceFactory;
begin
  Assert(Supports(Implementor, _T.GUID(ServiceType)));
  Result := function(Provider: IServiceProvider): IInterface
            begin
              Result := Implementor;
            end;
end;

// Funcion aparte, y no un closure dentro del bucle de Decorate: Delphi captura la
// variable, no su valor, asi que un closure creado dentro del bucle veria el ultimo
// descriptor. Los parametros de esta funcion si son un marco por llamada.
function DecoratedFactory(const ServiceType: PTypeInfo; const Inner: TServiceFactory;
  const Decorator: TServiceDecorator): TServiceFactory;
begin
  Result := function(Provider: IServiceProvider): IInterface
            begin
              var Original: IInterface;
              if not Supports(Inner(Provider), ServiceType.TypeData.GUID, Original) then
                raise EServiceProviderError.CreateFmt('Cannot decorate %s: the registered factory did not produce it',
                  [_T.NameOf(ServiceType)]);
              Result := Decorator(Original, Provider);
            end;
end;

{ IServiceCollection }

class operator IServiceCollection.Implicit(const Implementor: IServiceCollectionImpl): IServiceCollection;
begin
  Result.FImplementor := Implementor;
end;

class operator IServiceCollection.Implicit(const ServiceCollection: IServiceCollection): IServiceCollectionImpl;
begin
  Result := ServiceCollection.FImplementor;
end;

function IServiceCollection.GetCount: Integer;
begin
  Result := FImplementor.GetCount;
end;

function IServiceCollection.BuildServiceProvider: IServiceProvider;
begin
  Result := FImplementor.BuildServiceProvider;
end;

function IServiceCollection.BuildServiceProvider(const Options: TServiceProviderOptions): IServiceProvider;
begin
  Result := FImplementor.BuildServiceProvider(Options);
end;

function IServiceCollection.AddDescriptor(const LifeTime: TServiceLifeTime; const ServiceType: PTypeInfo;
  const ServiceFactory: TServiceFactory; const ImplClass: TClass;
  const RequestedFactory: TServiceFactoryEx): IServiceCollection;
begin
  FImplementor.Add(ServiceType, LifeTime, ServiceFactory, ImplClass, RequestedFactory);
  Result := Self;
end;

function IServiceCollection.AddAwareDescriptor(const LifeTime: TServiceLifeTime; const ServiceType: PTypeInfo;
  const Aware: TServiceFactoryEx): IServiceCollection;
begin
  // D4: la factoria consciente se baja AQUI, con el tipo del servicio ya conocido, a una de un solo
  // parametro; y se guarda aparte para que ForClosedType pueda volver a bajarla con el cierre (D3).
  // Asi hay una sola forma de que una consciente llegue a llamarse: envuelta, nunca directa.
  Result := AddDescriptor(LifeTime, ServiceType, Factory.ForRequestedType(ServiceType, Aware), nil, Aware);
end;

function IServiceCollection.Add<ServiceType>(const LifeTime: TServiceLifeTime;
  const Factory: TServiceFactoryEx<ServiceType>): IServiceCollection;
begin
  Result := AddAwareDescriptor(LifeTime, TypeInfo(ServiceType), TServiceFactoryEx(Factory));
end;

function IServiceCollection.AddSingleton<ServiceType>(const Factory: TServiceFactoryEx<ServiceType>): IServiceCollection;
begin
  Result := Add<ServiceType>(Singleton, Factory);
end;

function IServiceCollection.AddScoped<ServiceType>(const Factory: TServiceFactoryEx<ServiceType>): IServiceCollection;
begin
  Result := Add<ServiceType>(Scoped, Factory);
end;

function IServiceCollection.AddTransient<ServiceType>(const Factory: TServiceFactoryEx<ServiceType>): IServiceCollection;
begin
  Result := Add<ServiceType>(Transient, Factory);
end;

function IServiceCollection.TryAddSingleton<ServiceType>(const Factory: TServiceFactoryEx<ServiceType>): IServiceCollection;
begin
  Result := Self;
  if not CanTryAdd(TypeInfo(ServiceType)) then Exit;
  Result := AddAwareDescriptor(Singleton, TypeInfo(ServiceType), TServiceFactoryEx(Factory));
end;

function IServiceCollection.TryAddScoped<ServiceType>(const Factory: TServiceFactoryEx<ServiceType>): IServiceCollection;
begin
  Result := Self;
  if not CanTryAdd(TypeInfo(ServiceType)) then Exit;
  Result := AddAwareDescriptor(Scoped, TypeInfo(ServiceType), TServiceFactoryEx(Factory));
end;

function IServiceCollection.TryAddTransient<ServiceType>(const Factory: TServiceFactoryEx<ServiceType>): IServiceCollection;
begin
  Result := Self;
  if not CanTryAdd(TypeInfo(ServiceType)) then Exit;
  Result := AddAwareDescriptor(Transient, TypeInfo(ServiceType), TServiceFactoryEx(Factory));
end;

function IServiceCollection.AddScoped(const ServiceType: PTypeInfo; const TImpl: TClass): IServiceCollection;
begin
  Result := AddDescriptor(Scoped, ServiceType, Factory.From(ServiceType, Timpl), TImpl);
end;

function IServiceCollection.AddSingleton(const ServiceType: PTypeInfo; const TImpl: TClass): IServiceCollection;
begin
  Result := AddDescriptor(Singleton, ServiceType, Factory.From(ServiceType, Timpl), TImpl);
end;

function IServiceCollection.AddTransient(const ServiceType: PTypeInfo; const TImpl: TClass): IServiceCollection;
begin
  Result := AddDescriptor(Transient, ServiceType, Factory.From(ServiceType, Timpl), TImpl);
end;

function IServiceCollection.AddSingleton(const ServiceType: PTypeInfo; const Implementor: IInterface): IServiceCollection;
begin
  Result := AddSingleton(ServiceType, Factory.From(ServiceType, Implementor));
end;

function IServiceCollection.Add(const LifeTime: TServiceLifeTime; const ServiceType: PTypeInfo; const Factory: TServiceFactory): IServiceCollection;
begin
  Result := AddDescriptor(LifeTime, ServiceType, Factory, nil);
end;

function IServiceCollection.Add<ServiceType>(const LifeTime: TServiceLifeTime; const Factory: TServiceFactory<ServiceType>): IServiceCollection;
begin
  Result := Add(LifeTime, TypeInfo(ServiceType), TServiceFactory(Factory));
end;

function IServiceCollection.AddSingleton(const ServiceType: PTypeInfo; const Factory: TServiceFactory): IServiceCollection;
begin
  Result := Add(Singleton, ServiceType, Factory);
end;

function IServiceCollection.AddScoped(const ServiceType: PTypeInfo; const Factory: TServiceFactory): IServiceCollection;
begin
  Result := Add(Scoped, ServiceType, Factory);
end;

function IServiceCollection.AddScoped<ServiceType, TImpl>: IServiceCollection;
begin
  Result := AddScoped(TypeInfo(ServiceType), PTypeInfo(TypeInfo(TImpl)).TypeData.ClassType);
end;

function IServiceCollection.AddScoped<ServiceType>(const Factory: TServiceFactory<ServiceType>): IServiceCollection;
begin
  Result := Add<ServiceType>(Scoped, Factory);
end;

function IServiceCollection.AddSingleton<ServiceType>(const Factory: TServiceFactory<ServiceType>): IServiceCollection;
begin
  Result := Add<ServiceType>(Singleton, Factory);
end;

function IServiceCollection.AddSingleton<ServiceType, TImpl>: IServiceCollection;
begin
  Result := AddSingleton(TypeInfo(ServiceType), PTypeInfo(TypeInfo(TImpl)).TypeData.ClassType);
end;

function IServiceCollection.AddSingleton<ServiceType>(const Implementor: ServiceType): IServiceCollection;
begin
  Result := AddSingleton(TypeInfo(ServiceType), Implementor);
end;

function IServiceCollection.AddTransient(const ServiceType: PTypeInfo; const Factory: TServiceFactory): IServiceCollection;
begin
  Result := Add(Transient, ServiceType, Factory);
end;

function IServiceCollection.AddTransient<ServiceType>(const Factory: TServiceFactory<ServiceType>): IServiceCollection;
begin
  Result := Add<ServiceType>(Transient, Factory);
end;

function IServiceCollection.AddTransient<ServiceType, TImpl>: IServiceCollection;
begin
  Result := AddTransient(TypeInfo(ServiceType), PTypeInfo(TypeInfo(TImpl)).TypeData.ClassType);
end;

function IServiceCollection.Contains<ServiceType>: Boolean;
begin
  Result := Contains(TypeInfo(ServiceType));
end;

function IServiceCollection.Contains(const ServiceType: PTypeInfo): Boolean;
begin
  Result := FImplementor.Contains(ServiceType)
end;

function IServiceCollection.Descriptors(const ServiceType: PTypeInfo): TArray<TServiceDescriptor>;
begin
  Result := nil;
  if not FImplementor.Contains(ServiceType) then Exit;
  // Copia: la lista viva la posee el diccionario interno (doOwnsValues), y repartirla
  // dejaria mutar la coleccion sin pasar por CheckNotFrozen.
  Result := FImplementor.GetDescriptors(ServiceType).ToArray;
end;

function IServiceCollection.Descriptors<ServiceType>: TArray<TServiceDescriptor>;
begin
  Result := Descriptors(TypeInfo(ServiceType));
end;

function IServiceCollection.CanTryAdd(const ServiceType: PTypeInfo): Boolean;
begin
  // Un TryAdd que no anade sigue siendo una modificacion intentada: si la coleccion ya
  // esta congelada, quien llama tiene un error, no un no-op.
  FImplementor.CheckNotFrozen;
  Result := not FImplementor.Contains(ServiceType);
end;

function IServiceCollection.TryAddSingleton(const ServiceType: PTypeInfo; const Factory: TServiceFactory): IServiceCollection;
begin
  Result := Self;
  if not CanTryAdd(ServiceType) then Exit;
  Result := AddDescriptor(Singleton, ServiceType, Factory, nil);
end;

function IServiceCollection.TryAddSingleton(const ServiceType: PTypeInfo; const TImpl: TClass): IServiceCollection;
begin
  Result := Self;
  if not CanTryAdd(ServiceType) then Exit;
  Result := AddDescriptor(Singleton, ServiceType, Factory.From(ServiceType, TImpl), TImpl);
end;

function IServiceCollection.TryAddSingleton(const ServiceType: PTypeInfo; const Implementor: IInterface): IServiceCollection;
begin
  Result := Self;
  if not CanTryAdd(ServiceType) then Exit;
  Result := AddDescriptor(Singleton, ServiceType, Factory.From(ServiceType, Implementor), nil);
end;

function IServiceCollection.TryAddSingleton<ServiceType>(const Factory: TServiceFactory<ServiceType>): IServiceCollection;
begin
  Result := TryAddSingleton(TypeInfo(ServiceType), TServiceFactory(Factory));
end;

function IServiceCollection.TryAddSingleton<ServiceType>(const Implementor: ServiceType): IServiceCollection;
begin
  Result := TryAddSingleton(TypeInfo(ServiceType), IInterface(Implementor));
end;

function IServiceCollection.TryAddSingleton<ServiceType, TImpl>: IServiceCollection;
begin
  Result := TryAddSingleton(TypeInfo(ServiceType), PTypeInfo(TypeInfo(TImpl)).TypeData.ClassType);
end;

function IServiceCollection.TryAddScoped(const ServiceType: PTypeInfo; const Factory: TServiceFactory): IServiceCollection;
begin
  Result := Self;
  if not CanTryAdd(ServiceType) then Exit;
  Result := AddDescriptor(Scoped, ServiceType, Factory, nil);
end;

function IServiceCollection.TryAddScoped(const ServiceType: PTypeInfo; const TImpl: TClass): IServiceCollection;
begin
  Result := Self;
  if not CanTryAdd(ServiceType) then Exit;
  Result := AddDescriptor(Scoped, ServiceType, Factory.From(ServiceType, TImpl), TImpl);
end;

function IServiceCollection.TryAddScoped<ServiceType>(const Factory: TServiceFactory<ServiceType>): IServiceCollection;
begin
  Result := TryAddScoped(TypeInfo(ServiceType), TServiceFactory(Factory));
end;

function IServiceCollection.TryAddScoped<ServiceType, TImpl>: IServiceCollection;
begin
  Result := TryAddScoped(TypeInfo(ServiceType), PTypeInfo(TypeInfo(TImpl)).TypeData.ClassType);
end;

function IServiceCollection.TryAddTransient(const ServiceType: PTypeInfo; const Factory: TServiceFactory): IServiceCollection;
begin
  Result := Self;
  if not CanTryAdd(ServiceType) then Exit;
  Result := AddDescriptor(Transient, ServiceType, Factory, nil);
end;

function IServiceCollection.TryAddTransient(const ServiceType: PTypeInfo; const TImpl: TClass): IServiceCollection;
begin
  Result := Self;
  if not CanTryAdd(ServiceType) then Exit;
  Result := AddDescriptor(Transient, ServiceType, Factory.From(ServiceType, TImpl), TImpl);
end;

function IServiceCollection.TryAddTransient<ServiceType>(const Factory: TServiceFactory<ServiceType>): IServiceCollection;
begin
  Result := TryAddTransient(TypeInfo(ServiceType), TServiceFactory(Factory));
end;

function IServiceCollection.TryAddTransient<ServiceType, TImpl>: IServiceCollection;
begin
  Result := TryAddTransient(TypeInfo(ServiceType), PTypeInfo(TypeInfo(TImpl)).TypeData.ClassType);
end;

function IServiceCollection.Replace(const LifeTime: TServiceLifeTime; const ServiceType: PTypeInfo;
  const Factory: TServiceFactory): IServiceCollection;
begin
  FImplementor.RemoveAll(ServiceType);
  Result := AddDescriptor(LifeTime, ServiceType, Factory, nil);
end;

function IServiceCollection.Replace(const LifeTime: TServiceLifeTime; const ServiceType: PTypeInfo;
  const TImpl: TClass): IServiceCollection;
begin
  FImplementor.RemoveAll(ServiceType);
  Result := AddDescriptor(LifeTime, ServiceType, Factory.From(ServiceType, TImpl), TImpl);
end;

function IServiceCollection.Replace<ServiceType>(const LifeTime: TServiceLifeTime;
  const Factory: TServiceFactory<ServiceType>): IServiceCollection;
begin
  Result := Replace(LifeTime, TypeInfo(ServiceType), TServiceFactory(Factory));
end;

function IServiceCollection.Replace<ServiceType, TImpl>(const LifeTime: TServiceLifeTime): IServiceCollection;
begin
  Result := Replace(LifeTime, TypeInfo(ServiceType), PTypeInfo(TypeInfo(TImpl)).TypeData.ClassType);
end;

function IServiceCollection.RemoveAll(const ServiceType: PTypeInfo): IServiceCollection;
begin
  FImplementor.RemoveAll(ServiceType);
  Result := Self;
end;

function IServiceCollection.RemoveAll<ServiceType>: IServiceCollection;
begin
  Result := RemoveAll(TypeInfo(ServiceType));
end;

function IServiceCollection.Decorate(const ServiceType: PTypeInfo; const Decorator: TServiceDecorator): IServiceCollection;
begin
  FImplementor.CheckNotFrozen;
  Result := Self;
  var Current := Descriptors(ServiceType);
  if Length(Current) = 0 then
    raise EServiceProviderError.CreateFmt('Cannot decorate %s: nothing is registered for it', [_T.NameOf(ServiceType)]);

  var Wrapped: TArray<TServiceDescriptor>;
  SetLength(Wrapped, Length(Current));
  for var Idx := 0 to High(Current) do
    // El descriptor decorado pasa a ser un registro por factory: su implementacion ya no
    // es una clase inspeccionable, y la validacion del grafo no debe fingir que lo es.
    Wrapped[Idx] := TServiceDescriptor.Create(DecoratedFactory(ServiceType, Current[Idx].Factory, Decorator),
      Current[Idx].LifeTime, ServiceType);

  FImplementor.SetDescriptors(ServiceType, Wrapped);
end;

function IServiceCollection.Decorate<ServiceType>(const Decorator: TServiceDecorator<ServiceType>): IServiceCollection;
begin
  Result := Decorate(TypeInfo(ServiceType), TServiceDecorator(Decorator));
end;

function IServiceCollection.AddModule(const Module: IAppModule): IServiceCollection;
begin
  FImplementor.CheckNotFrozen;
  Result := Self;
  if Assigned(Module) then
    Module.AddServices(Self);
end;

function IServiceCollection.AddModule<T>: IServiceCollection;
begin
  // La restriccion `constructor` llama al constructor propio de T y sin RTTI. El
  // descubrimiento hacia MetaclassType.Create, que se lo saltaba en silencio.
  var Instance := T.Create;
  var Module: IAppModule;
  if not Supports(Instance, IAppModule, Module) then
  begin
    Instance.Free;
    raise EServiceProviderError.CreateFmt('%s does not support IAppModule', [T.ClassName]);
  end;
  Result := AddModule(Module);
end;

function IServiceCollection.TryMarkVisited(const Category, Key: string): Boolean;
begin
  Result := FImplementor.TryMarkVisited(Category, Key);
end;

{ TDuckDecorator }

constructor TDuckDecorator<T>.InternalCreate(const DecoratedType: PTypeInfo; const Decorated: T);
begin
  if Assigned(Decorated) and not HasRTTI(DecoratedType) then
    raise Exception.CreateFmt('Type %s not supports RTTI', [DecoratedType.Name]);
  inherited Create(DecoratedType, CallInterceptor);
  FDecoratedType := DecoratedType;
  FDecorated := Decorated;
end;

constructor TDuckDecorator<T>.Create(Decorated: T);
begin
  InternalCreate(TypeInfo(T), Decorated);
end;

constructor TDuckDecorator<T>.Create;
begin
  InternalCreate(TypeInfo(T), nil);
end;

destructor TDuckDecorator<T>.Destroy;
begin
  OnInvoke := nil;
  FDecorated := nil;
  inherited;
end;

procedure TDuckDecorator<T>.BeforeCall(Method: TRttiMethod; const Args: TArray<TValue>; var Result: TValue);
begin

end;

procedure TDuckDecorator<T>.AfterCall(Method: TRttiMethod; const Args: TArray<TValue>; var Result: TValue);
begin

end;

function TDuckDecorator<T>.CallByNameTo(const Instance: TObject; MethodName: string; const Args: TArray<TValue>; var CallResult: TValue): Boolean;
begin
  if not Assigned(Instance) then Exit(False);
  var
    RC := TRttiContext.Create;
  try
    var
    RType := RC.GetType(Instance.ClassType);
    var
    Method := RType.GetMethod(MethodName);
    Result := (Method <> nil);
    if not Result then Exit;
    // Remove original Self parameter
    var
    NewArgs := Args;
    System.Delete(NewArgs, 0, 1);
    CallResult := Method.Invoke(Instance, NewArgs);
  finally
    RC.Free;
  end;

end;

procedure TDuckDecorator<T>.CallDecorated(Method: TRttiMethod; const Args: TArray<TValue>; var Result: TValue);
var
  VTable: PPVtable;
  Code: Pointer;
  ArgList: TArray<TValue>;
  CallConv: TCallConv;
begin
  if not Assigned(FDecorated) then Exit;
  var Intf: IInterface;
  if not Supports(Decorated, DecoratedType.TypeData.GUID, Intf) then Exit;
  VTable := PPVtable(Intf);
  Code := VTable^^[Method.VirtualIndex];
  ArgList := Args;
  CallConv := Method.CallingConvention;
  ArgList[0] := TValue.From(Intf);
  Intf := nil;

  if Method.ReturnType <> nil then Result := System.Rtti.Invoke(Code, ArgList, CallConv, Method.ReturnType.Handle)
  else Result := System.Rtti.Invoke(Code, ArgList, CallConv, nil);
  ArgList[0] := nil;
end;

procedure TDuckDecorator<T>.CallInterceptor(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue);
begin
  Result := TValue.Empty;
  BeforeCall(Method, Args, Result);

  if not CallByNameTo(Self, Method.Name, Args, Result) then
    CallDecorated(Method, Args, Result);

  AfterCall(Method, Args, Result);
end;

{ TDuckDecorator }

constructor TDuckDecorator.Create(const DecoratedType: PTypeInfo; Decorated: IInvokable);
begin
  if Assigned(Decorated) and not Supports(Decorated, DecoratedType.TypeData.GUID) then
      raise Exception.CreateFmt('Decorated argument not supports interface type %s', [DecoratedType.Name]);

  InternalCreate(DecoratedType, Decorated);
end;

{ TLazy<T> }

constructor TLazy<T>.Create(ServiceProvider: IServiceProvider; const Factory: TServiceFactory<T>);
begin
  FType := TypeInfo(T);
  inherited Create(FType, CallInterceptor);
  FServiceProvider := ServiceProvider;
  FFactory := Factory;
end;

procedure TLazy<T>.CheckDelegatedReady;
begin
  if Assigned(FDelegated) then Exit;
  FDelegated := FFactory(FServiceProvider);
end;

procedure TLazy<T>.CallDelegated(Method: TRttiMethod; const Args: TArray<TValue>; var Result: TValue);
var
  VTable: PPVtable;
  Code: Pointer;
  ArgList: TArray<TValue>;
  CallConv: TCallConv;
begin
  CheckDelegatedReady;
  if not Assigned(FDelegated) then Exit;
  var Intf: Pointer;
  if not Supports(FDelegated, FType.TypeData.GUID, Intf) then Exit;

  VTable := PPVtable(IInterface(Intf));
  Code := VTable^^[Method.VirtualIndex];
  ArgList := Args;
  CallConv := Method.CallingConvention;
  ArgList[0] := TValue.From(IInterface(Intf));

  if Method.ReturnType <> nil then Result := System.Rtti.Invoke(Code, ArgList, CallConv, Method.ReturnType.Handle)
  else Result := System.Rtti.Invoke(Code, ArgList, CallConv, nil);
end;

procedure TLazy<T>.CallInterceptor(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue);
begin
  Result := TValue.Empty;
  CallDelegated(Method, Args, Result);
end;

{ Lazy }

class function Lazy.From<T>(Provider: IServiceProvider; const Factory: TServiceFactory<T>): T;
begin
  var Implementor := TLazy<T>.Create(Provider, Factory);
  var FType: PTypeInfo := TypeInfo(T);
  Supports(Implementor, FType.TypeData.GUID, Result);
end;

end.
