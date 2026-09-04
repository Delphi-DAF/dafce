unit Daf.Extensions.DependencyInjection;

interface

uses
  System.SysUtils,
  System.Rtti,
  System.Generics.Collections,
  System.Generics.Defaults,
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

  // Una propiedad marcada que el contenedor no puede rellenar, o una clase que dice participar
  // y no expone ninguna. Deriva de EServiceProviderError porque es un fallo de composicion como
  // los demas, y es un tipo propio porque quien cablea a peticion una instancia suya necesita
  // separarlo de "esto no esta registrado".
  EPropertyInjectionError = class(EServiceProviderError)
  end;

  /// <summary>
  ///   Todo lo que el contenedor sabe de una clave de servicio: que puede serlo, como se compara
  ///   con otra, como se nombra en un mensaje, y cual es el comodin.
  /// </summary>
  /// <remarks>
  ///   La clave es un TValue porque en .NET es un object?, y ese es el equivalente fiel: admite
  ///   cadena, ordinal, enumerado --Boolean incluido--, metaclase y PTypeInfo, y dos valores de
  ///   tipos DISTINTOS con el mismo ordinal son claves distintas. El TValue vacio ES el espacio
  ///   sin clave, y no un tercer estado que cada consulta tenga que tratar aparte.
  /// </remarks>
  ServiceKey = record
  public
    // Si este TValue puede ser clave. El vacio lo es: significa "sin clave".
    class function IsValid(const Key: TValue): Boolean; static;
    /// <summary>
    ///   Aborta si no puede serlo, nombrando el tipo que se intento usar.
    /// </summary>
    /// <remarks>
    ///   Se llama donde la clave ENTRA --al registrar y al resolver-- y nunca al comparar dos
    ///   claves entre si: un diccionario solo consulta la comparacion cuando dos claves caen en el
    ///   mismo cubo, asi que un guardian puesto ahi no corre para un tipo que no colisiona con
    ///   nada, y la busqueda devuelve "no encontrado" en silencio.
    /// </remarks>
    class procedure Check(const Key: TValue); static;
    // Texto de una clave PARA UN MENSAJE, y para nada mas. No lanza.
    class function Text(const Key: TValue): string; static;
    // Dos claves son la misma cuando coinciden su TIPO y su valor. El tipo entra primero: es lo
    // que separa dos enumerados distintos con el mismo ordinal, y la cadena '42' del entero 42.
    class function Same(const A, B: TValue): Boolean; static;
    // El tipo entra tambien en el hash, o dos claves distintas caerian en el mismo cubo y solo la
    // comparacion las separaria: correcto, lento, y oculta el error.
    class function HashOf(const Key: TValue): Integer; static;
    /// <summary>
    ///   La clave comodin: el registro que la lleva atiende cualquier clave de su tipo de servicio
    ///   que no tenga registro propio. Es KeyedService.AnyKey de .NET.
    /// </summary>
    /// <remarks>
    ///   Es un valor de un enumerado declarado en el CUERPO de esta unit, y esa es toda su
    ///   seguridad: nadie de fuera puede nombrar el tipo para construirlo. Con una cadena
    ///   reservada, quien la registrase legitimamente se convertiria en el comodin sin enterarse
    ///   -- el registro funciona y lo que cambia es a quien atiende.
    /// </remarks>
    class function Any: TValue; static;
    class function IsAny(const Key: TValue): Boolean; static;
  end;

  /// <summary>
  ///   El par por el que el indice de la coleccion reparte descriptores: tipo de servicio y clave.
  /// </summary>
  /// <remarks>
  ///   Con el par en la clave del indice, el aislamiento entre el espacio con clave y el sin clave
  ///   es ESTRUCTURAL: un registro con clave no esta donde busca una consulta sin clave, y no hay
  ///   nada que cada consulta tenga que acordarse de filtrar. Un filtro condicional repartido falla
  ///   en la consulta que se olvide, y ese fallo no se nota porque las demas pasan igual.
  /// </remarks>
  TServiceSlot = record
    ServiceType: PTypeInfo;
    Key: TValue;
    constructor Create(const AServiceType: PTypeInfo); overload;
    constructor Create(const AServiceType: PTypeInfo; const AKey: TValue); overload;
    function IsKeyed: Boolean;
    // El slot comodin del mismo tipo de servicio: donde se busca al fallar la busqueda directa.
    function AnyKeySlot: TServiceSlot;
    class function Comparer: IEqualityComparer<TServiceSlot>; static;
  end;

  /// <summary>
  ///   Marca una propiedad como dependencia que el contenedor rellena. El tipo de la propiedad
  ///   es el contrato --ha de ser una interfaz-- y la propiedad ha de ser escribible.
  /// </summary>
  /// <remarks>
  ///   El prefijo no es decorativo: Daf.MiniSpec.Injection exporta un InjectAttribute propio, y
  ///   dos units que exporten el mismo nombre en el mismo uses no dan error -- gana la ultima, y
  ///   el barrido deja de ver las propiedades marcadas sin decir nada.
  ///
  ///   Sirve igual sobre un PARAMETRO DE CONSTRUCTOR, que es el [FromKeyedServices] de .NET. Un
  ///   solo atributo para las dos posiciones, para que el marcador propio de una aplicacion valga
  ///   en las dos sin declararlo dos veces.
  ///
  ///   La clave viaja DENTRO, y no en un atributo acompanante: es lo que hace Unity con
  ///   [Dependency("name")]. Un segundo atributo serian dos cosas que hay que poner juntas siempre
  ///   y que se pueden poner sueltas.
  ///
  ///   Las sobrecargas por primitivo no son una complicacion evitable: los argumentos de un
  ///   atributo en Delphi tienen que ser CONSTANTES, y un TValue en el sitio de uso da E2026 hasta
  ///   con el literal de cadena mas simple. El TValue se construye dentro del constructor, al que
  ///   llama la RTTI al leer el atributo -- ahi si se puede.
  /// </remarks>
  DafInjectAttribute = class(TCustomAttribute)
  strict private
    FKey: TValue;
    FKeyError: string;
  strict protected
    /// <summary>
    ///   La via de un descendiente que ya tiene su clave hecha. Un TValue SI puede ser parametro
    ///   de un constructor; lo que no puede es aparecer en el sitio de uso de un atributo.
    /// </summary>
    constructor CreateWithKey(const AKey: TValue);
  public
    constructor Create; overload;                                     // [DafInject]
    constructor Create(const AKey: string); overload;                 // [DafInject('blob')]
    constructor Create(const AKey: Integer); overload;                // [DafInject(42)]
    constructor Create(const AKey: Boolean); overload;                // [DafInject(True)]
    constructor Create(const AKey: TClass); overload;                 // [DafInject(TOrderCreated)]
    constructor Create(const AKeyType: PTypeInfo); overload;          // [DafInject(TypeInfo(IFoo))]
    /// <summary>
    ///   Escape para un enumerado suelto: [DafInject(TypeInfo(TTransport), Ord(trSms))]. NO es la
    ///   via del enumerado -- esa es un descendiente de una linea sobre DafInjectAttribute&lt;T&gt;.
    /// </summary>
    /// <remarks>
    ///   Ord(...) es un entero cualquiera, asi que el ordinal se comprueba contra el rango del
    ///   tipo. Sin eso, un ordinal que el tipo no tiene da una clave que no es la de nadie, y el
    ///   fallo se lee como "ese servicio no esta registrado" -- mandando a buscar donde no es.
    /// </remarks>
    constructor Create(const AKeyType: PTypeInfo; const AOrdinal: Integer); overload;
    property Key: TValue read FKey;
    // Vacio salvo que la clave escrita en el atributo sea imposible. Lo lee el barrido, que es
    // quien puede nombrar la propiedad o el parametro en el mensaje.
    property KeyError: string read FKeyError;
  end;

  /// <summary>
  ///   Base para el marcador tipado de una aplicacion, que se declara en una linea:
  ///   <c>StorageInject = class(DafInjectAttribute&lt;TStorageKind&gt;);</c>
  /// </summary>
  /// <remarks>
  ///   NUNCA en posicion de atributo. [DafInject&lt;TStorageKind&gt;(...)] COMPILA, con un
  ///   W1074 Unknown custom attribute que casi nadie lee, y en ejecucion GetAttributes devuelve
  ///   CERO: la inyeccion no ocurre y nada lo dice. Es lo que va a intentar quien vea el generico
  ///   declarado. Un constructor generico tampoco vale -- E2528, un constructor no puede llevar
  ///   parametros de tipo en Delphi.
  /// </remarks>
  DafInjectAttribute<T> = class(DafInjectAttribute)
  public
    constructor Create(const AKey: T);
  end;

  // Interfaces don't support generic overload, so we model our desired interface using a record
  // that will delegate behavior in a true interface that implementor classes will support
  IServiceProvider = record
  strict private
    FImplementor: IServiceProviderImpl;
    class function Error(const Fmt: string; const Args: array of const): EServiceProviderError; static;
  private
    function GetFromInterfaceOps(const Slot: TServiceSlot): TArray<TRttiMethod>;
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

    // Resolucion con clave. Los nombres son los de .NET a proposito: quien venga de alli no tiene
    // que aprender otra palabra. Un registro hecho SIN clave no se alcanza desde aqui, y uno hecho
    // CON clave no se alcanza desde los de arriba: un contrato con clave es un contrato distinto.
    function CanResolveKeyed(const ServiceType: PTypeInfo; const Key: TValue): Boolean;

    procedure GetKeyedService(const ServiceType: PTypeInfo; const Key: TValue; out Service); overload;
    function GetKeyedService<ServiceType: IInterface>(const Key: TValue): ServiceType; overload;

    procedure GetRequiredKeyedService(const ServiceType: PTypeInfo; const Key: TValue; out Service); overload;
    function GetRequiredKeyedService<ServiceType>(const Key: TValue): ServiceType; overload;

    function GetKeyedServices(const ServiceType: PTypeInfo; const Key: TValue): IInterfaceList<IInterface>; overload;
    function GetKeyedServices<ServiceType: IInterface>(const Key: TValue): IInterfaceList<ServiceType>; overload;

    function TryGetKeyed(const ServiceType: PTypeInfo; const Key: TValue; out Service): Boolean; overload;
    function TryGetKeyed<ServiceType: IInterface>(const Key: TValue; out Service): Boolean; overload;

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
  // La misma idea que la de arriba, sobre el otro eje: recibe la CLAVE que se esta pidiendo. Es lo
  // que hace util un registro comodin -- lo unico que puede hacer una factoria que atiende a todas
  // las claves es decidir en funcion de cual le toca. Tipo aparte y no parametro opcional, por la
  // razon que explica el comentario de TServiceFactoryEx.
  TServiceKeyedFactory<ServiceType: IInterface> = reference to function(Provider: IServiceProvider;
    const Key: TValue): ServiceType;
  TServiceKeyedFactory = TServiceKeyedFactory<IInterface>;
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
    /// <summary>
    ///   Lo mismo para el eje de la clave: baja una factoria consciente de la clave a una de un
    ///   solo parametro, capturando la clave que se le entregara. Se llama con la clave del
    ///   registro al registrar uno concreto, y con la clave PEDIDA al alcanzar el comodin.
    /// </summary>
    class function ForRequestedKey(const RequestedKey: TValue;
      const Aware: TServiceKeyedFactory): TServiceFactory;
  end;

  // Cableado por codigo de una instancia recien construida: la tercera via de la inyeccion por
  // propiedad, para lo que no se puede anotar --una clase de otra libreria, una unit con la RTTI
  // recortada, una clave de servicio que en un atributo se escribe con dolor--. No usa RTTI.
  TServiceInjector<T: class> = reference to procedure(const Instance: T; const Provider: IServiceProvider);
  TServiceInjector = TServiceInjector<TObject>;

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
  // Y la CLAVE SERVIDA, por la misma razon que el tipo cerrado y no por la de estar registrado con
  // clave: dos registros con claves distintas ya son dos registros, asi que Registration los
  // separa solo. Lo que necesita eje propio es el registro COMODIN, que es UNO para todas las
  // claves que atiende -- sin esto todas compartirian instancia, y el fallo se lee como el
  // acierto. Vacia en un registro corriente.
  TServiceIdentity = record
    Registration: NativeInt;
    ClosedType: PTypeInfo;
    ServedKey: TValue;
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
    ['{E0C55B47-03C5-4F25-98DC-F07C1AA27C01}']
    procedure Shutdown;
    // Cuantos objetos sigue poseyendo el contenedor: los del ambito de singletons mas los de
    // cada ambito vivo. NO cuenta los transient: el contenedor no los retiene, asi que para el
    // dejan de existir en cuanto los entrega.
    function OwnedInstanceCount: Integer;
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
    // La clave con la que se registro: vacia en el espacio sin clave (D4).
    FKey: TValue;
    // La factoria consciente de la clave, si el registro se hizo con una; nil si no. Es lo que
    // ForServedKey necesita para volver a bajarla con la clave que el comodin acaba de atender.
    FKeyedFactory: TServiceKeyedFactory;
  public
    constructor Create(const Factory: TServiceFactory; const LifeTime: TServiceLifeTime; const TypeInfo: PTypeInfo;
      const ImplementationClass: TClass = nil; const RequestedFactory: TServiceFactoryEx = nil); overload;
    constructor Create(const Factory: TServiceFactory; const LifeTime: TServiceLifeTime; const Slot: TServiceSlot;
      const ImplementationClass: TClass = nil; const RequestedFactory: TServiceFactoryEx = nil;
      const KeyedFactory: TServiceKeyedFactory = nil); overload;
    // Nombre legible del registro, y para nada mas que un mensaje de error. Nombra tambien la
    // clave cuando la tiene: "falta IStorage" no dice cual falta si hay tres registrados por
    // clave, y obliga a leer la composicion entera para averiguar de que se hablaba.
    // No decide nada y no lanza: ver la implementacion.
    function ServiceName: string;
    // El par (tipo, clave) por el que este registro esta indexado.
    function Slot: TServiceSlot;
    /// <summary>
    ///   Este mismo registro, atado al cierre que se acaba de pedir. Es lo que convierte UN
    ///   registro sin fijar en un registro por tipo cerrado: identidad propia --y con ella
    ///   ciclo de vida propio-- y una fabrica que sabe para que cierre construye.
    /// </summary>
    function ForClosedType(const AClosedType: PTypeInfo): TServiceDescriptor;
    /// <summary>
    ///   Este mismo registro, atado a la clave que el comodin acaba de atender. Es a la clave lo
    ///   que ForClosedType es al cierre generico: identidad propia --y con ella instancia propia--
    ///   y una fabrica que sabe para que clave construye.
    /// </summary>
    function ForServedKey(const AKey: TValue): TServiceDescriptor;
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
    // La clave del REGISTRO, vacia si se hizo sin clave. No es la clave servida: esa vive en la
    // identidad, y solo difiere cuando a este registro se llego por el comodin.
    property Key: TValue read FKey;
    // Clase de implementacion si el registro se hizo por clase; nil si se hizo por factory.
    // Es lo unico que la validacion del grafo puede inspeccionar: una lambda es opaca.
    property ImplementationClass: TClass read FImplementationClass;
  end;
  TServiceDescriptors = TList<TServiceDescriptor>;

  // misteriosamente si uso interface(IInvokable) se produce E2134 Type'<void>' has no type info
{$M+}
  IServiceProviderImpl = interface
    ['{D2043968-561D-4666-8A36-D2714BC262C4}']
    // Por el PAR y no por el tipo: la clave llega hasta el embudo de resolucion, y ahi se acaba.
    // GetImplementor, GetOrCreate y CollectGarbage siguen sin saber que existe.
    function CanResolve(const Slot: TServiceSlot): Boolean;
    function TryGet(const Slot: TServiceSlot; out Service): Boolean;
    function TryGetAll(const Slot: TServiceSlot; out Services: IInterfaceList): Boolean;
    function CreateScope: IServiceScope;
  end;
{$M-}

  IServiceCollectionImpl = interface(IInvokable)
    ['{C767E76F-F5AE-438D-9725-B624E704127B}']
    function GetCount: Integer;
    procedure CheckNotFrozen;
    function Add(const Slot: TServiceSlot; const LifeTime: TServiceLifeTime; const ServiceFactory: TServiceFactory;
      const ImplClass: TClass = nil; const RequestedFactory: TServiceFactoryEx = nil;
      const KeyedFactory: TServiceKeyedFactory = nil): IServiceCollectionImpl; overload;
    function Contains(const Slot: TServiceSlot): Boolean;
    function BuildServiceProvider: IServiceProviderImpl; overload;
    function BuildServiceProvider(const Options: TServiceProviderOptions): IServiceProviderImpl; overload;
    function GetDescriptors(const Slot: TServiceSlot): TServiceDescriptors;
    function SetDescriptors(const Slot: TServiceSlot; const NewDescriptors: TArray<TServiceDescriptor>): IServiceCollectionImpl;
    function RemoveAll(const Slot: TServiceSlot): IServiceCollectionImpl;
    // El registro sin fijar que puede servir a este cierre, si lo hay. Se pregunta al FALLAR
    // la busqueda directa y nunca antes: un registro concreto para un cierre tiene que seguir
    // mandando sobre el registro sin fijar.
    function TryFindOpenGeneric(const ClosedType: PTypeInfo; out OpenType: PTypeInfo): Boolean;
    // Contabilidad de "esto ya se proceso", por coleccion y no por proceso: componer
    // dos veces en un proceso tiene que dar el mismo resultado.
    function TryMarkVisited(const Category, Key: string): Boolean;
    // Los cableados por codigo son POLITICA de la coleccion y no propiedad de un registro:
    // alcanzan a toda instancia construida que sea del tipo o descienda de el, y corren en el
    // orden en que se registraron.
    function AddInjector(const AClass: TClass; const Injector: TServiceInjector): IServiceCollectionImpl;
    function InjectorsFor(const AClass: TClass): TArray<TServiceInjector>;
  end;

  IServiceScope = interface(IInvokable)
    ['{E0B2F2D2-BAF3-4C6F-9B22-4A9B2B3A5C4D}']
    function ServiceProvider: IServiceProvider;
  end;

  IServiceScopeFactory = interface(IInvokable)
    ['{C3F2E1D4-7A6C-4B8E-9B12-5D8A6A2D9F3E}']
    function CreateScope: IServiceScope;
  end;

  // La costura por la que el activador alcanza los cableados por codigo: el activador solo tiene
  // delante un proveedor, y los cableados viven en la coleccion.
  IServiceInjectors = interface(IInvokable)
    ['{CD6C10E8-D3C8-4A61-AEB0-38578B00CB72}']
    function InjectorsFor(const AClass: TClass): TArray<TServiceInjector>;
  end;

  IServiceCollection = record
  strict private
    FImplementor: IServiceCollectionImpl;
    function GetCount: Integer;
  private
    function AddDescriptor(const LifeTime: TServiceLifeTime; const Slot: TServiceSlot;
      const ServiceFactory: TServiceFactory; const ImplClass: TClass;
      const RequestedFactory: TServiceFactoryEx = nil;
      const KeyedFactory: TServiceKeyedFactory = nil): IServiceCollection;
    function AddAwareDescriptor(const LifeTime: TServiceLifeTime; const Slot: TServiceSlot;
      const Aware: TServiceFactoryEx): IServiceCollection;
    function AddKeyAwareDescriptor(const LifeTime: TServiceLifeTime; const Slot: TServiceSlot;
      const Aware: TServiceKeyedFactory): IServiceCollection;
    function CanTryAdd(const Slot: TServiceSlot): Boolean;
    // El slot de un registro CON clave: comprueba que la clave puede serlo y que el tipo no deja
    // sus argumentos genericos sin fijar. Las dos, donde la clave entra.
    function KeyedSlot(const ServiceType: PTypeInfo; const Key: TValue): TServiceSlot;
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

    // --- Registro con clave -------------------------------------------------------------------
    // Las mismas formas que sus Add* --por clase, por factory y por instancia--, cada una tomando
    // la clave. Un registro con clave NO bloquea ni sustituye a uno sin clave del mismo tipo, ni
    // al de otra clave: la unidad de la pregunta es el par, no el tipo.
    function AddKeyed(const LifeTime: TServiceLifeTime; const ServiceType: PTypeInfo; const Key: TValue;
      const Factory: TServiceFactory): IServiceCollection; overload;
    function AddKeyed<ServiceType: IInterface>(const LifeTime: TServiceLifeTime; const Key: TValue;
      const Factory: TServiceFactory<ServiceType>): IServiceCollection; overload;
    function AddKeyed<ServiceType: IInterface>(const LifeTime: TServiceLifeTime; const Key: TValue;
      const Factory: TServiceKeyedFactory<ServiceType>): IServiceCollection; overload;

    function AddKeyedSingleton(const ServiceType: PTypeInfo; const Key: TValue; const Factory: TServiceFactory): IServiceCollection; overload;
    function AddKeyedSingleton(const ServiceType: PTypeInfo; const Key: TValue; const TImpl: TClass): IServiceCollection; overload;
    function AddKeyedSingleton(const ServiceType: PTypeInfo; const Key: TValue; const Implementor: IInterface): IServiceCollection; overload;
    function AddKeyedSingleton<ServiceType: IInterface>(const Key: TValue; const Factory: TServiceFactory<ServiceType>): IServiceCollection; overload;
    function AddKeyedSingleton<ServiceType: IInterface>(const Key: TValue; const Factory: TServiceKeyedFactory<ServiceType>): IServiceCollection; overload;
    function AddKeyedSingleton<ServiceType: IInterface>(const Key: TValue; const Implementor: ServiceType): IServiceCollection; overload;
    function AddKeyedSingleton<ServiceType: IInterface; TImpl: ServiceType>(const Key: TValue): IServiceCollection; overload;

    function AddKeyedScoped(const ServiceType: PTypeInfo; const Key: TValue; const Factory: TServiceFactory): IServiceCollection; overload;
    function AddKeyedScoped(const ServiceType: PTypeInfo; const Key: TValue; const TImpl: TClass): IServiceCollection; overload;
    function AddKeyedScoped<ServiceType: IInterface>(const Key: TValue; const Factory: TServiceFactory<ServiceType>): IServiceCollection; overload;
    function AddKeyedScoped<ServiceType: IInterface>(const Key: TValue; const Factory: TServiceKeyedFactory<ServiceType>): IServiceCollection; overload;
    function AddKeyedScoped<ServiceType: IInterface; TImpl: ServiceType>(const Key: TValue): IServiceCollection; overload;

    function AddKeyedTransient(const ServiceType: PTypeInfo; const Key: TValue; const Factory: TServiceFactory): IServiceCollection; overload;
    function AddKeyedTransient(const ServiceType: PTypeInfo; const Key: TValue; const TImpl: TClass): IServiceCollection; overload;
    function AddKeyedTransient<ServiceType: IInterface>(const Key: TValue; const Factory: TServiceFactory<ServiceType>): IServiceCollection; overload;
    function AddKeyedTransient<ServiceType: IInterface>(const Key: TValue; const Factory: TServiceKeyedFactory<ServiceType>): IServiceCollection; overload;
    function AddKeyedTransient<ServiceType: IInterface; TImpl: ServiceType>(const Key: TValue): IServiceCollection; overload;

    function TryAddKeyedSingleton(const ServiceType: PTypeInfo; const Key: TValue; const Factory: TServiceFactory): IServiceCollection; overload;
    function TryAddKeyedSingleton(const ServiceType: PTypeInfo; const Key: TValue; const TImpl: TClass): IServiceCollection; overload;
    function TryAddKeyedSingleton(const ServiceType: PTypeInfo; const Key: TValue; const Implementor: IInterface): IServiceCollection; overload;
    function TryAddKeyedSingleton<ServiceType: IInterface>(const Key: TValue; const Factory: TServiceFactory<ServiceType>): IServiceCollection; overload;
    function TryAddKeyedSingleton<ServiceType: IInterface>(const Key: TValue; const Factory: TServiceKeyedFactory<ServiceType>): IServiceCollection; overload;
    function TryAddKeyedSingleton<ServiceType: IInterface>(const Key: TValue; const Implementor: ServiceType): IServiceCollection; overload;
    function TryAddKeyedSingleton<ServiceType: IInterface; TImpl: ServiceType>(const Key: TValue): IServiceCollection; overload;

    function TryAddKeyedScoped(const ServiceType: PTypeInfo; const Key: TValue; const Factory: TServiceFactory): IServiceCollection; overload;
    function TryAddKeyedScoped(const ServiceType: PTypeInfo; const Key: TValue; const TImpl: TClass): IServiceCollection; overload;
    function TryAddKeyedScoped<ServiceType: IInterface>(const Key: TValue; const Factory: TServiceFactory<ServiceType>): IServiceCollection; overload;
    function TryAddKeyedScoped<ServiceType: IInterface>(const Key: TValue; const Factory: TServiceKeyedFactory<ServiceType>): IServiceCollection; overload;
    function TryAddKeyedScoped<ServiceType: IInterface; TImpl: ServiceType>(const Key: TValue): IServiceCollection; overload;

    function TryAddKeyedTransient(const ServiceType: PTypeInfo; const Key: TValue; const Factory: TServiceFactory): IServiceCollection; overload;
    function TryAddKeyedTransient(const ServiceType: PTypeInfo; const Key: TValue; const TImpl: TClass): IServiceCollection; overload;
    function TryAddKeyedTransient<ServiceType: IInterface>(const Key: TValue; const Factory: TServiceFactory<ServiceType>): IServiceCollection; overload;
    function TryAddKeyedTransient<ServiceType: IInterface>(const Key: TValue; const Factory: TServiceKeyedFactory<ServiceType>): IServiceCollection; overload;
    function TryAddKeyedTransient<ServiceType: IInterface; TImpl: ServiceType>(const Key: TValue): IServiceCollection; overload;

    // Quita los registros de un tipo Y una clave. RemoveAll sin clave alcanza unicamente los del
    // espacio sin clave: la interpretacion que se conserva es la que no toca lo que el llamante no
    // nombro.
    function RemoveAllKeyed(const ServiceType: PTypeInfo; const Key: TValue): IServiceCollection; overload;
    function RemoveAllKeyed<ServiceType: IInterface>(const Key: TValue): IServiceCollection; overload;

    function ContainsKeyed(const ServiceType: PTypeInfo; const Key: TValue): Boolean; overload;
    function ContainsKeyed<ServiceType: IInterface>(const Key: TValue): Boolean; overload;

    function KeyedDescriptors(const ServiceType: PTypeInfo; const Key: TValue): TArray<TServiceDescriptor>; overload;
    function KeyedDescriptors<ServiceType: IInterface>(const Key: TValue): TArray<TServiceDescriptor>; overload;

    // Envuelve los registros existentes del tipo conservando su ciclo de vida. Para envolver
    // comportamiento (logging, cache, reintentos): los efectos laterales de arranque son IStartupTask.
    function Decorate(const ServiceType: PTypeInfo; const Decorator: TServiceDecorator): IServiceCollection; overload;
    function Decorate<ServiceType: IInterface>(const Decorator: TServiceDecorator<ServiceType>): IServiceCollection; overload;

    // Aplica un modulo en el acto, no de forma diferida: la composicion es una sola
    // secuencia legible, y un TryAdd escrito despues de un modulo se ejecuta despues.
    function AddModule<T: class, IAppModule, constructor>: IServiceCollection; overload;
    function AddModule(const Module: IAppModule): IServiceCollection; overload;

    /// <summary>
    ///   Cablea por codigo toda instancia que el contenedor construya de esa clase o de una que
    ///   descienda de ella, despues del constructor y despues de la inyeccion por atributo.
    /// </summary>
    /// <remarks>
    ///   Se llama AddInjector y no AddInitializer --que es como lo nombran Simple Injector,
    ///   Autofac y Windsor-- porque es la tercera via de UNA feature, y comparte raiz con
    ///   DafInject y con TActivatorUtilities.InjectInto. AddInitializer queda libre para un
    ///   gancho de post-construccion de proposito general, que es otra cosa.
    ///   No alcanza a lo que el contenedor no construyo: para eso esta InjectInto, y la llama
    ///   quien posee la instancia.
    /// </remarks>
    function AddInjector<T: class>(const Injector: TServiceInjector<T>): IServiceCollection;

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
// Lo mismo para un servicio PEDIDO con una clave: nombra las dos cosas. Sin clave da exactamente
// el texto de arriba, para que un mensaje que ya existia no cambie por existir esta capacidad.
function ServiceRequestName(const ServiceType: PTypeInfo; const Key: TValue): string;

implementation

uses
  System.Hash,
  Daf.Rtti,
  Daf.MemUtils,
  Daf.Activator,
  Daf.DependencyInjection.ActivatorUtilities,
  Daf.DependencyInjection.OpenGenerics;

type
  // El centinela del comodin, y toda su seguridad es vivir AQUI: nadie fuera de esta unit puede
  // nombrar el tipo, asi que la clave no se puede construir por accidente ni pisar. Es la misma
  // propiedad que en .NET da una instancia de object -- infalsificable por identidad-- conseguida
  // por otra via, y funciona porque dos enumerados de tipos distintos no chocan aunque compartan
  // ordinal.
  TDafServiceKeyMarker = (dskAnyKey);

var
  // Contador de proceso, y aqui si vale: la identidad de un registro solo tiene que ser
  // UNICA, no reproducible entre ejecuciones ni entre colecciones -- al reves que la
  // contabilidad de "esto ya se proceso", que por eso vive en la coleccion. Empieza en 1
  // para que el 0 de un TServiceDescriptor sin construir no se confunda con un registro.
  GLastServiceIdentity: Integer = 0;

{ ServiceKey }

class function ServiceKey.IsValid(const Key: TValue): Boolean;
begin
  // La vacia es valida: ES el espacio sin clave.
  if Key.IsEmpty then Exit(True);
  Result := Key.Kind in [tkUString, tkString, tkLString, tkWString,
                         tkInteger, tkInt64, tkEnumeration, tkClassRef, tkPointer];
end;

class procedure ServiceKey.Check(const Key: TValue);
begin
  if IsValid(Key) then Exit;
  if Key.Kind in [tkChar, tkWChar] then
    raise EServiceProviderError.Create(
      'A character cannot be a service key: a one character key is a string key, so use a string');
  raise EServiceProviderError.CreateFmt(
    'A %s cannot be a service key: only a string, an integer, an enumerated value, a metaclass or a PTypeInfo can',
    [string(Key.TypeInfo.Name)]);
end;

class function ServiceKey.Text(const Key: TValue): string;
begin
  if Key.IsEmpty then Exit('<none>');
  if IsAny(Key) then Exit('<any key>');
  case Key.Kind of
    tkUString, tkString, tkLString, tkWString: Result := '''' + Key.AsString + '''';
    tkInteger, tkInt64: Result := Key.ToString;
    tkEnumeration: Result := string(Key.TypeInfo.Name) + '.' + Key.ToString;
    tkClassRef: Result := Key.AsClass.ClassName;
    tkPointer:
      begin
        var Ptr := PPointer(Key.GetReferenceToRawData)^;
        Result := Format('$%p', [Ptr]);
        // Se INTENTA nombrarlo, no se exige: un tkPointer no tiene por que ser un PTypeInfo, y
        // esto se compone dentro de la ruta de error, donde una excepcion sustituye a la que se
        // estaba contando.
        try
          if Ptr <> nil then
            Result := string(PTypeInfo(Ptr).Name);
        except
        end;
      end;
  else
    Result := string(Key.TypeInfo.Name);
  end;
end;

class function ServiceKey.Same(const A, B: TValue): Boolean;
begin
  if A.IsEmpty or B.IsEmpty then Exit(A.IsEmpty and B.IsEmpty);
  // El tipo primero: es lo que separa dos enumerados distintos con el mismo ordinal, y la cadena
  // '42' del entero 42. Es la semantica de .NET, donde la clave es un object y dos valores de
  // tipos distintos no son iguales aunque el numero coincida.
  if A.TypeInfo <> B.TypeInfo then Exit(False);
  case A.Kind of
    tkUString, tkString, tkLString, tkWString: Result := A.AsString = B.AsString;
    tkInteger, tkInt64, tkEnumeration: Result := A.AsOrdinal = B.AsOrdinal;
    // Metaclase y PTypeInfo, por identidad de puntero: es lo que ya hace el indice de servicios
    // con PTypeInfo, y el enlazador unifica el del mismo tipo calculado en units distintas.
    tkClassRef: Result := A.AsClass = B.AsClass;
    tkPointer: Result := PPointer(A.GetReferenceToRawData)^ = PPointer(B.GetReferenceToRawData)^;
  else
    // Un tipo que no es clave no llega aqui, porque Check lo rechazo donde entraba. Y no se lanza
    // desde aqui a proposito: una comprobacion puesta en la comparacion solo corre cuando dos
    // claves caen en el mismo cubo -- medido -- asi que seria un guardian que salta a veces.
    Result := False;
  end;
end;

class function ServiceKey.HashOf(const Key: TValue): Integer;
begin
  if Key.IsEmpty then Exit(0);
  // El tipo entra tambien en el hash, o dos claves distintas caerian en el mismo cubo por
  // casualidad y solo la comparacion las separaria: correcto, lento, y oculta el error.
  var Seed := Integer(THashBobJenkins.GetHashValue(string(Key.TypeInfo.Name)));
  case Key.Kind of
    tkUString, tkString, tkLString, tkWString:
      Result := Seed xor Integer(THashBobJenkins.GetHashValue(Key.AsString));
    tkInteger, tkInt64, tkEnumeration: Result := Seed xor Integer(Key.AsOrdinal);
    tkClassRef: Result := Seed xor Integer(NativeInt(Key.AsClass));
    tkPointer: Result := Seed xor Integer(NativeInt(PPointer(Key.GetReferenceToRawData)^));
  else
    Result := Seed;
  end;
end;

class function ServiceKey.Any: TValue;
begin
  Result := TValue.From<TDafServiceKeyMarker>(dskAnyKey);
end;

class function ServiceKey.IsAny(const Key: TValue): Boolean;
begin
  Result := (not Key.IsEmpty) and (Key.TypeInfo = TypeInfo(TDafServiceKeyMarker));
end;

{ TServiceSlot }

constructor TServiceSlot.Create(const AServiceType: PTypeInfo);
begin
  ServiceType := AServiceType;
  Key := TValue.Empty;
end;

constructor TServiceSlot.Create(const AServiceType: PTypeInfo; const AKey: TValue);
begin
  ServiceType := AServiceType;
  Key := AKey;
end;

function TServiceSlot.IsKeyed: Boolean;
begin
  Result := not Key.IsEmpty;
end;

function TServiceSlot.AnyKeySlot: TServiceSlot;
begin
  Result := TServiceSlot.Create(ServiceType, ServiceKey.Any);
end;

class function TServiceSlot.Comparer: IEqualityComparer<TServiceSlot>;
begin
  Result := TEqualityComparer<TServiceSlot>.Construct(
    function(const A, B: TServiceSlot): Boolean
    begin
      Result := (A.ServiceType = B.ServiceType) and ServiceKey.Same(A.Key, B.Key);
    end,
    function(const Value: TServiceSlot): Integer
    begin
      Result := Integer(NativeInt(Value.ServiceType)) xor ServiceKey.HashOf(Value.Key);
    end);
end;

{ DafInjectAttribute }

constructor DafInjectAttribute.CreateWithKey(const AKey: TValue);
begin
  inherited Create;
  FKey := AKey;
end;

constructor DafInjectAttribute.Create;
begin
  CreateWithKey(TValue.Empty);
end;

constructor DafInjectAttribute.Create(const AKey: string);
begin
  CreateWithKey(TValue.From<string>(AKey));
end;

constructor DafInjectAttribute.Create(const AKey: Integer);
begin
  CreateWithKey(TValue.From<Integer>(AKey));
end;

constructor DafInjectAttribute.Create(const AKey: Boolean);
begin
  CreateWithKey(TValue.From<Boolean>(AKey));
end;

constructor DafInjectAttribute.Create(const AKey: TClass);
begin
  CreateWithKey(TValue.From<TClass>(AKey));
end;

constructor DafInjectAttribute.Create(const AKeyType: PTypeInfo);
begin
  CreateWithKey(TValue.From<PTypeInfo>(AKeyType));
end;

constructor DafInjectAttribute.Create(const AKeyType: PTypeInfo; const AOrdinal: Integer);
begin
  inherited Create;
  if (AKeyType = nil) or not (AKeyType.Kind in [tkEnumeration, tkInteger]) then
  begin
    FKeyError := 'the first argument of this mark has to be the TypeInfo of an ordinal type';
    Exit;
  end;
  var Data := GetTypeData(AKeyType);
  if (AOrdinal < Data.MinValue) or (AOrdinal > Data.MaxValue) then
  begin
    // No se lanza desde el constructor: lo llama la RTTI al leer el atributo, y una excepcion ahi
    // no puede nombrar la propiedad ni el parametro que la lleva. Lo hace el barrido, que si.
    FKeyError := Format('%d is not an ordinal of %s, whose range is %d..%d',
      [AOrdinal, string(AKeyType.Name), Data.MinValue, Data.MaxValue]);
    Exit;
  end;
  TValue.Make(@AOrdinal, AKeyType, FKey);
end;

{ DafInjectAttribute<T> }

constructor DafInjectAttribute<T>.Create(const AKey: T);
begin
  CreateWithKey(TValue.From<T>(AKey));
end;

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
  Result := (A.Registration = B.Registration) and (A.ClosedType = B.ClosedType) and
    ServiceKey.Same(A.ServedKey, B.ServedKey);
end;

class operator TServiceIdentity.NotEqual(const A, B: TServiceIdentity): Boolean;
begin
  Result := not (A = B);
end;

{ TServiceDescriptor }

constructor TServiceDescriptor.Create(const Factory: TServiceFactory; const LifeTime: TServiceLifeTime;
  const TypeInfo: PTypeInfo; const ImplementationClass: TClass; const RequestedFactory: TServiceFactoryEx);
begin
  Self := TServiceDescriptor.Create(Factory, LifeTime, TServiceSlot.Create(TypeInfo),
    ImplementationClass, RequestedFactory);
end;

constructor TServiceDescriptor.Create(const Factory: TServiceFactory; const LifeTime: TServiceLifeTime;
  const Slot: TServiceSlot; const ImplementationClass: TClass; const RequestedFactory: TServiceFactoryEx;
  const KeyedFactory: TServiceKeyedFactory);
begin
  FFactory := Factory;
  FLifeTime := LifeTime;
  FTypeInfo := Slot.ServiceType;
  FKey := Slot.Key;
  FImplementationClass := ImplementationClass;
  FIdentity.Registration := AtomicIncrement(GLastServiceIdentity);
  FIdentity.ClosedType := nil;
  FIdentity.ServedKey := TValue.Empty;
  FRequestedFactory := RequestedFactory;
  FKeyedFactory := KeyedFactory;
 end;

function TServiceDescriptor.Slot: TServiceSlot;
begin
  Result := TServiceSlot.Create(FTypeInfo, FKey);
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

function TServiceDescriptor.ForServedKey(const AKey: TValue): TServiceDescriptor;
begin
  Result := Self;
  // El registro sigue siendo el mismo -- Registration no cambia -- y lo que se le anade es el eje
  // que le faltaba. Dos claves atendidas por el mismo comodin son dos identidades, y por tanto dos
  // instancias; la misma clave dos veces es una.
  Result.FIdentity.ServedKey := AKey;
  // Una fabrica que no sabe de claves se queda como esta; la que si sabe se vuelve a bajar, ahora
  // con la clave que se pidio en vez de la del registro. Es el mismo brazo que ForClosedType tiene
  // para la factoria consciente del tipo.
  if Assigned(FKeyedFactory) then
    Result.FFactory := Daf.Extensions.DependencyInjection.Factory.ForRequestedKey(AKey, FKeyedFactory);
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

function ServiceRequestName(const ServiceType: PTypeInfo; const Key: TValue): string;
begin
  Result := ServiceTypeName(ServiceType);
  if Key.IsEmpty then Exit;
  Result := Format('%s (key %s)', [Result, ServiceKey.Text(Key)]);
end;

function TServiceDescriptor.ServiceName: string;
begin
  Result := ServiceTypeName(FTypeInfo);
  if FKey.IsEmpty then Exit;
  Result := Format('%s (key %s)', [Result, ServiceKey.Text(FKey)]);
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
  Result := TryGetKeyed(ServiceType, TValue.Empty, Service);
end;

function IServiceProvider.TryGetKeyed(const ServiceType: PTypeInfo; const Key: TValue; out Service): Boolean;
begin
  // La clave se comprueba DONDE ENTRA, que es aqui y en el registro. Un tipo que no puede ser
  // clave tiene que abortar nombrandolo, no devolver "no encontrado" en silencio.
  ServiceKey.Check(Key);
  var Slot := TServiceSlot.Create(ServiceType, Key);
  Result := FImplementor.TryGet(Slot, Service);
  if Result then Exit;
  // La conversion viaja con la clave: el servicio del que se convierte se busca con la MISMA, o
  // seria una puerta trasera del espacio con clave al espacio sin clave.
  var Conversors := GetFromInterfaceOps(Slot);
  for var Conversor in Conversors do
  begin
    Result := FImplementor.TryGet(TServiceSlot.Create(Conversor.GetParameters[0].ParamType.Handle, Key), Service);
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

function IServiceProvider.TryGetKeyed<ServiceType>(const Key: TValue; out Service): Boolean;
begin
  Result := TryGetKeyed(TypeInfo(ServiceType), Key, Service);
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

procedure IServiceProvider.GetKeyedService(const ServiceType: PTypeInfo; const Key: TValue; out Service);
begin
  if not TryGetKeyed(ServiceType, Key, Service) then
    IInterface(Service) := nil;
end;

function IServiceProvider.GetKeyedService<ServiceType>(const Key: TValue): ServiceType;
begin
  GetKeyedService(TypeInfo(ServiceType), Key, Result);
end;

procedure IServiceProvider.GetRequiredService(const ServiceType: PTypeInfo; out Service);
begin
  GetRequiredKeyedService(ServiceType, TValue.Empty, Service);
end;

function IServiceProvider.GetRequiredService<ServiceType>: ServiceType;
begin
  GetRequiredService(TypeInfo(ServiceType), Result);
end;

procedure IServiceProvider.GetRequiredKeyedService(const ServiceType: PTypeInfo; const Key: TValue; out Service);
begin
  if TryGetKeyed(ServiceType, Key, Service) then Exit;
  if Key.IsEmpty then
    raise Error('Cannot resolve service: %s', [ServiceType.Name]);
  raise Error('Cannot resolve service: %s (key %s)', [ServiceType.Name, ServiceKey.Text(Key)]);
end;

function IServiceProvider.GetRequiredKeyedService<ServiceType>(const Key: TValue): ServiceType;
begin
  GetRequiredKeyedService(TypeInfo(ServiceType), Key, Result);
end;

function IServiceProvider.GetServices<ServiceType>: IInterfaceList<ServiceType>;
begin
  Result := IInterfaceList<ServiceType>(GetServices(TypeInfo(ServiceType)))
end;

function IServiceProvider.GetServices(const ServiceType: PTypeInfo): IInterfaceList<IInterface>;
begin
  FImplementor.TryGetAll(TServiceSlot.Create(ServiceType), IInterfaceList(Result));
end;

function IServiceProvider.GetKeyedServices<ServiceType>(const Key: TValue): IInterfaceList<ServiceType>;
begin
  Result := IInterfaceList<ServiceType>(GetKeyedServices(TypeInfo(ServiceType), Key))
end;

function IServiceProvider.GetKeyedServices(const ServiceType: PTypeInfo; const Key: TValue): IInterfaceList<IInterface>;
begin
  ServiceKey.Check(Key);
  FImplementor.TryGetAll(TServiceSlot.Create(ServiceType, Key), IInterfaceList(Result));
end;

function IServiceProvider.AsType<ServiceType>: ServiceType;
begin
  var PInfo: PTypeInfo :=  TypeInfo(ServiceType);
  Supports(FImplementor, PInfo.TypeData.Guid, Result);
end;

function IServiceProvider.GetFromInterfaceOps(const Slot: TServiceSlot): TArray<TRttiMethod>;
begin
  var FImpl := FImplementor;
  var Key := Slot.Key;
  Result :=_T.GetConverFromOps(Slot.ServiceType,  function(M: TRttiMethod): Boolean
              begin
                var FromType := M.GetParameters[0].ParamType;
                Result := (FromType.TypeKind = tkInterface) and
                  FImpl.CanResolve(TServiceSlot.Create(FromType.Handle, Key));
              end);
end;

function IServiceProvider.CanResolve(const ServiceType: PTypeInfo): Boolean;
begin
  Result := CanResolveKeyed(ServiceType, TValue.Empty);
end;

function IServiceProvider.CanResolveKeyed(const ServiceType: PTypeInfo; const Key: TValue): Boolean;
begin
  ServiceKey.Check(Key);
  var Slot := TServiceSlot.Create(ServiceType, Key);
  Result := FImplementor.CanResolve(Slot) or (GetFromInterfaceOps(Slot) <> nil);
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
              // Sin cablear: quien resuelve publica la instancia y la cablea despues, que es
              // lo que hace resoluble un ciclo por propiedad.
              var Aux := TActivatorUtilities.CreateUnwired(Provider, TImpl);
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

class function Factory.ForRequestedKey(const RequestedKey: TValue;
  const Aware: TServiceKeyedFactory): TServiceFactory;
begin
  Result := function(Provider: IServiceProvider): IInterface
            begin
              Result := Aware(Provider, RequestedKey);
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

function IServiceCollection.AddDescriptor(const LifeTime: TServiceLifeTime; const Slot: TServiceSlot;
  const ServiceFactory: TServiceFactory; const ImplClass: TClass;
  const RequestedFactory: TServiceFactoryEx; const KeyedFactory: TServiceKeyedFactory): IServiceCollection;
begin
  FImplementor.Add(Slot, LifeTime, ServiceFactory, ImplClass, RequestedFactory, KeyedFactory);
  Result := Self;
end;

function IServiceCollection.AddAwareDescriptor(const LifeTime: TServiceLifeTime; const Slot: TServiceSlot;
  const Aware: TServiceFactoryEx): IServiceCollection;
begin
  // D4: la factoria consciente se baja AQUI, con el tipo del servicio ya conocido, a una de un solo
  // parametro; y se guarda aparte para que ForClosedType pueda volver a bajarla con el cierre (D3).
  // Asi hay una sola forma de que una consciente llegue a llamarse: envuelta, nunca directa.
  Result := AddDescriptor(LifeTime, Slot, Factory.ForRequestedType(Slot.ServiceType, Aware), nil, Aware);
end;

function IServiceCollection.AddKeyAwareDescriptor(const LifeTime: TServiceLifeTime; const Slot: TServiceSlot;
  const Aware: TServiceKeyedFactory): IServiceCollection;
begin
  // Lo mismo sobre el eje de la clave: se baja aqui con la clave del registro, y se guarda aparte
  // para que ForServedKey pueda volver a bajarla con la clave que el comodin acabe atendiendo.
  Result := AddDescriptor(LifeTime, Slot, Factory.ForRequestedKey(Slot.Key, Aware), nil, nil, Aware);
end;

function IServiceCollection.KeyedSlot(const ServiceType: PTypeInfo; const Key: TValue): TServiceSlot;
begin
  ServiceKey.Check(Key);
  // Clave y argumentos genericos sin fijar son dos mecanismos de emparejamiento, y cruzarlos
  // multiplica los casos de precedencia. Falla AL REGISTRAR y no al resolver, que es lo que lo
  // convierte en un limite y no en una trampa: la alternativa es un registro que se acepta y
  // luego no aparece nunca.
  if (not Key.IsEmpty) and TOpenGeneric.IsOpen(ServiceType) then
    raise EServiceProviderError.CreateFmt(
      'Cannot register %s with the key %s: a service key and unbound generic arguments are not supported together',
      [ServiceTypeName(ServiceType), ServiceKey.Text(Key)]);
  Result := TServiceSlot.Create(ServiceType, Key);
end;

function IServiceCollection.Add<ServiceType>(const LifeTime: TServiceLifeTime;
  const Factory: TServiceFactoryEx<ServiceType>): IServiceCollection;
begin
  Result := AddAwareDescriptor(LifeTime, TServiceSlot.Create(TypeInfo(ServiceType)), TServiceFactoryEx(Factory));
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
  if not CanTryAdd(TServiceSlot.Create(TypeInfo(ServiceType))) then Exit;
  Result := AddAwareDescriptor(Singleton, TServiceSlot.Create(TypeInfo(ServiceType)), TServiceFactoryEx(Factory));
end;

function IServiceCollection.TryAddScoped<ServiceType>(const Factory: TServiceFactoryEx<ServiceType>): IServiceCollection;
begin
  Result := Self;
  if not CanTryAdd(TServiceSlot.Create(TypeInfo(ServiceType))) then Exit;
  Result := AddAwareDescriptor(Scoped, TServiceSlot.Create(TypeInfo(ServiceType)), TServiceFactoryEx(Factory));
end;

function IServiceCollection.TryAddTransient<ServiceType>(const Factory: TServiceFactoryEx<ServiceType>): IServiceCollection;
begin
  Result := Self;
  if not CanTryAdd(TServiceSlot.Create(TypeInfo(ServiceType))) then Exit;
  Result := AddAwareDescriptor(Transient, TServiceSlot.Create(TypeInfo(ServiceType)), TServiceFactoryEx(Factory));
end;

function IServiceCollection.AddScoped(const ServiceType: PTypeInfo; const TImpl: TClass): IServiceCollection;
begin
  Result := AddDescriptor(Scoped, TServiceSlot.Create(ServiceType), Factory.From(ServiceType, Timpl), TImpl);
end;

function IServiceCollection.AddSingleton(const ServiceType: PTypeInfo; const TImpl: TClass): IServiceCollection;
begin
  Result := AddDescriptor(Singleton, TServiceSlot.Create(ServiceType), Factory.From(ServiceType, Timpl), TImpl);
end;

function IServiceCollection.AddTransient(const ServiceType: PTypeInfo; const TImpl: TClass): IServiceCollection;
begin
  Result := AddDescriptor(Transient, TServiceSlot.Create(ServiceType), Factory.From(ServiceType, Timpl), TImpl);
end;

function IServiceCollection.AddSingleton(const ServiceType: PTypeInfo; const Implementor: IInterface): IServiceCollection;
begin
  Result := AddSingleton(ServiceType, Factory.From(ServiceType, Implementor));
end;

function IServiceCollection.Add(const LifeTime: TServiceLifeTime; const ServiceType: PTypeInfo; const Factory: TServiceFactory): IServiceCollection;
begin
  Result := AddDescriptor(LifeTime, TServiceSlot.Create(ServiceType), Factory, nil);
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
  Result := FImplementor.Contains(TServiceSlot.Create(ServiceType))
end;

function IServiceCollection.Descriptors(const ServiceType: PTypeInfo): TArray<TServiceDescriptor>;
begin
  Result := nil;
  if not FImplementor.Contains(TServiceSlot.Create(ServiceType)) then Exit;
  // Copia: la lista viva la posee el diccionario interno (doOwnsValues), y repartirla
  // dejaria mutar la coleccion sin pasar por CheckNotFrozen.
  Result := FImplementor.GetDescriptors(TServiceSlot.Create(ServiceType)).ToArray;
end;

function IServiceCollection.Descriptors<ServiceType>: TArray<TServiceDescriptor>;
begin
  Result := Descriptors(TypeInfo(ServiceType));
end;

function IServiceCollection.CanTryAdd(const Slot: TServiceSlot): Boolean;
begin
  // Un TryAdd que no anade sigue siendo una modificacion intentada: si la coleccion ya
  // esta congelada, quien llama tiene un error, no un no-op.
  FImplementor.CheckNotFrozen;
  // La unidad de la pregunta es el PAR, no el tipo: con clave, un TryAdd de 'blob' no ve el
  // registro de 'file', y ninguno de los dos ve el registro sin clave. Si mirase solo el tipo, la
  // primera clave registrada bloquearia a todas las demas.
  Result := not FImplementor.Contains(Slot);
end;

function IServiceCollection.TryAddSingleton(const ServiceType: PTypeInfo; const Factory: TServiceFactory): IServiceCollection;
begin
  Result := Self;
  if not CanTryAdd(TServiceSlot.Create(ServiceType)) then Exit;
  Result := AddDescriptor(Singleton, TServiceSlot.Create(ServiceType), Factory, nil);
end;

function IServiceCollection.TryAddSingleton(const ServiceType: PTypeInfo; const TImpl: TClass): IServiceCollection;
begin
  Result := Self;
  if not CanTryAdd(TServiceSlot.Create(ServiceType)) then Exit;
  Result := AddDescriptor(Singleton, TServiceSlot.Create(ServiceType), Factory.From(ServiceType, TImpl), TImpl);
end;

function IServiceCollection.TryAddSingleton(const ServiceType: PTypeInfo; const Implementor: IInterface): IServiceCollection;
begin
  Result := Self;
  if not CanTryAdd(TServiceSlot.Create(ServiceType)) then Exit;
  Result := AddDescriptor(Singleton, TServiceSlot.Create(ServiceType), Factory.From(ServiceType, Implementor), nil);
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
  if not CanTryAdd(TServiceSlot.Create(ServiceType)) then Exit;
  Result := AddDescriptor(Scoped, TServiceSlot.Create(ServiceType), Factory, nil);
end;

function IServiceCollection.TryAddScoped(const ServiceType: PTypeInfo; const TImpl: TClass): IServiceCollection;
begin
  Result := Self;
  if not CanTryAdd(TServiceSlot.Create(ServiceType)) then Exit;
  Result := AddDescriptor(Scoped, TServiceSlot.Create(ServiceType), Factory.From(ServiceType, TImpl), TImpl);
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
  if not CanTryAdd(TServiceSlot.Create(ServiceType)) then Exit;
  Result := AddDescriptor(Transient, TServiceSlot.Create(ServiceType), Factory, nil);
end;

function IServiceCollection.TryAddTransient(const ServiceType: PTypeInfo; const TImpl: TClass): IServiceCollection;
begin
  Result := Self;
  if not CanTryAdd(TServiceSlot.Create(ServiceType)) then Exit;
  Result := AddDescriptor(Transient, TServiceSlot.Create(ServiceType), Factory.From(ServiceType, TImpl), TImpl);
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
  FImplementor.RemoveAll(TServiceSlot.Create(ServiceType));
  Result := AddDescriptor(LifeTime, TServiceSlot.Create(ServiceType), Factory, nil);
end;

function IServiceCollection.Replace(const LifeTime: TServiceLifeTime; const ServiceType: PTypeInfo;
  const TImpl: TClass): IServiceCollection;
begin
  FImplementor.RemoveAll(TServiceSlot.Create(ServiceType));
  Result := AddDescriptor(LifeTime, TServiceSlot.Create(ServiceType), Factory.From(ServiceType, TImpl), TImpl);
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
  FImplementor.RemoveAll(TServiceSlot.Create(ServiceType));
  Result := Self;
end;

function IServiceCollection.RemoveAll<ServiceType>: IServiceCollection;
begin
  Result := RemoveAll(TypeInfo(ServiceType));
end;

function IServiceCollection.AddKeyed(const LifeTime: TServiceLifeTime; const ServiceType: PTypeInfo;
  const Key: TValue; const Factory: TServiceFactory): IServiceCollection;
begin
  Result := AddDescriptor(LifeTime, KeyedSlot(ServiceType, Key), Factory, nil);
end;

function IServiceCollection.AddKeyed<ServiceType>(const LifeTime: TServiceLifeTime; const Key: TValue;
  const Factory: TServiceFactory<ServiceType>): IServiceCollection;
begin
  Result := AddKeyed(LifeTime, TypeInfo(ServiceType), Key, TServiceFactory(Factory));
end;

function IServiceCollection.AddKeyed<ServiceType>(const LifeTime: TServiceLifeTime; const Key: TValue;
  const Factory: TServiceKeyedFactory<ServiceType>): IServiceCollection;
begin
  Result := AddKeyAwareDescriptor(LifeTime, KeyedSlot(TypeInfo(ServiceType), Key), TServiceKeyedFactory(Factory));
end;

function IServiceCollection.AddKeyedSingleton(const ServiceType: PTypeInfo; const Key: TValue;
  const Factory: TServiceFactory): IServiceCollection;
begin
  Result := AddDescriptor(Singleton, KeyedSlot(ServiceType, Key), Factory, nil);
end;

function IServiceCollection.AddKeyedSingleton(const ServiceType: PTypeInfo; const Key: TValue;
  const TImpl: TClass): IServiceCollection;
begin
  Result := AddDescriptor(Singleton, KeyedSlot(ServiceType, Key), Factory.From(ServiceType, TImpl), TImpl);
end;

function IServiceCollection.AddKeyedSingleton<ServiceType>(const Key: TValue;
  const Factory: TServiceFactory<ServiceType>): IServiceCollection;
begin
  Result := AddKeyedSingleton(TypeInfo(ServiceType), Key, TServiceFactory(Factory));
end;

function IServiceCollection.AddKeyedSingleton<ServiceType>(const Key: TValue;
  const Factory: TServiceKeyedFactory<ServiceType>): IServiceCollection;
begin
  Result := AddKeyAwareDescriptor(Singleton, KeyedSlot(TypeInfo(ServiceType), Key), TServiceKeyedFactory(Factory));
end;

function IServiceCollection.AddKeyedSingleton<ServiceType, TImpl>(const Key: TValue): IServiceCollection;
begin
  Result := AddKeyedSingleton(TypeInfo(ServiceType), Key, PTypeInfo(TypeInfo(TImpl)).TypeData.ClassType);
end;

function IServiceCollection.AddKeyedScoped(const ServiceType: PTypeInfo; const Key: TValue;
  const Factory: TServiceFactory): IServiceCollection;
begin
  Result := AddDescriptor(Scoped, KeyedSlot(ServiceType, Key), Factory, nil);
end;

function IServiceCollection.AddKeyedScoped(const ServiceType: PTypeInfo; const Key: TValue;
  const TImpl: TClass): IServiceCollection;
begin
  Result := AddDescriptor(Scoped, KeyedSlot(ServiceType, Key), Factory.From(ServiceType, TImpl), TImpl);
end;

function IServiceCollection.AddKeyedScoped<ServiceType>(const Key: TValue;
  const Factory: TServiceFactory<ServiceType>): IServiceCollection;
begin
  Result := AddKeyedScoped(TypeInfo(ServiceType), Key, TServiceFactory(Factory));
end;

function IServiceCollection.AddKeyedScoped<ServiceType>(const Key: TValue;
  const Factory: TServiceKeyedFactory<ServiceType>): IServiceCollection;
begin
  Result := AddKeyAwareDescriptor(Scoped, KeyedSlot(TypeInfo(ServiceType), Key), TServiceKeyedFactory(Factory));
end;

function IServiceCollection.AddKeyedScoped<ServiceType, TImpl>(const Key: TValue): IServiceCollection;
begin
  Result := AddKeyedScoped(TypeInfo(ServiceType), Key, PTypeInfo(TypeInfo(TImpl)).TypeData.ClassType);
end;

function IServiceCollection.AddKeyedTransient(const ServiceType: PTypeInfo; const Key: TValue;
  const Factory: TServiceFactory): IServiceCollection;
begin
  Result := AddDescriptor(Transient, KeyedSlot(ServiceType, Key), Factory, nil);
end;

function IServiceCollection.AddKeyedTransient(const ServiceType: PTypeInfo; const Key: TValue;
  const TImpl: TClass): IServiceCollection;
begin
  Result := AddDescriptor(Transient, KeyedSlot(ServiceType, Key), Factory.From(ServiceType, TImpl), TImpl);
end;

function IServiceCollection.AddKeyedTransient<ServiceType>(const Key: TValue;
  const Factory: TServiceFactory<ServiceType>): IServiceCollection;
begin
  Result := AddKeyedTransient(TypeInfo(ServiceType), Key, TServiceFactory(Factory));
end;

function IServiceCollection.AddKeyedTransient<ServiceType>(const Key: TValue;
  const Factory: TServiceKeyedFactory<ServiceType>): IServiceCollection;
begin
  Result := AddKeyAwareDescriptor(Transient, KeyedSlot(TypeInfo(ServiceType), Key), TServiceKeyedFactory(Factory));
end;

function IServiceCollection.AddKeyedTransient<ServiceType, TImpl>(const Key: TValue): IServiceCollection;
begin
  Result := AddKeyedTransient(TypeInfo(ServiceType), Key, PTypeInfo(TypeInfo(TImpl)).TypeData.ClassType);
end;

function IServiceCollection.AddKeyedSingleton(const ServiceType: PTypeInfo; const Key: TValue;
  const Implementor: IInterface): IServiceCollection;
begin
  Result := AddDescriptor(Singleton, KeyedSlot(ServiceType, Key), Factory.From(ServiceType, Implementor), nil);
end;

function IServiceCollection.AddKeyedSingleton<ServiceType>(const Key: TValue;
  const Implementor: ServiceType): IServiceCollection;
begin
  Result := AddKeyedSingleton(TypeInfo(ServiceType), Key, IInterface(Implementor));
end;

function IServiceCollection.TryAddKeyedSingleton(const ServiceType: PTypeInfo; const Key: TValue;
  const Factory: TServiceFactory): IServiceCollection;
begin
  Result := Self;
  var Slot := KeyedSlot(ServiceType, Key);
  if not CanTryAdd(Slot) then Exit;
  Result := AddDescriptor(Singleton, Slot, Factory, nil);
end;

function IServiceCollection.TryAddKeyedSingleton(const ServiceType: PTypeInfo; const Key: TValue;
  const TImpl: TClass): IServiceCollection;
begin
  Result := Self;
  var Slot := KeyedSlot(ServiceType, Key);
  if not CanTryAdd(Slot) then Exit;
  Result := AddDescriptor(Singleton, Slot, Factory.From(ServiceType, TImpl), TImpl);
end;

function IServiceCollection.TryAddKeyedSingleton<ServiceType>(const Key: TValue;
  const Factory: TServiceFactory<ServiceType>): IServiceCollection;
begin
  Result := TryAddKeyedSingleton(TypeInfo(ServiceType), Key, TServiceFactory(Factory));
end;

function IServiceCollection.TryAddKeyedSingleton<ServiceType>(const Key: TValue;
  const Factory: TServiceKeyedFactory<ServiceType>): IServiceCollection;
begin
  Result := Self;
  var Slot := KeyedSlot(TypeInfo(ServiceType), Key);
  if not CanTryAdd(Slot) then Exit;
  Result := AddKeyAwareDescriptor(Singleton, Slot, TServiceKeyedFactory(Factory));
end;

function IServiceCollection.TryAddKeyedSingleton<ServiceType, TImpl>(const Key: TValue): IServiceCollection;
begin
  Result := TryAddKeyedSingleton(TypeInfo(ServiceType), Key, PTypeInfo(TypeInfo(TImpl)).TypeData.ClassType);
end;

function IServiceCollection.TryAddKeyedScoped(const ServiceType: PTypeInfo; const Key: TValue;
  const Factory: TServiceFactory): IServiceCollection;
begin
  Result := Self;
  var Slot := KeyedSlot(ServiceType, Key);
  if not CanTryAdd(Slot) then Exit;
  Result := AddDescriptor(Scoped, Slot, Factory, nil);
end;

function IServiceCollection.TryAddKeyedScoped(const ServiceType: PTypeInfo; const Key: TValue;
  const TImpl: TClass): IServiceCollection;
begin
  Result := Self;
  var Slot := KeyedSlot(ServiceType, Key);
  if not CanTryAdd(Slot) then Exit;
  Result := AddDescriptor(Scoped, Slot, Factory.From(ServiceType, TImpl), TImpl);
end;

function IServiceCollection.TryAddKeyedScoped<ServiceType>(const Key: TValue;
  const Factory: TServiceFactory<ServiceType>): IServiceCollection;
begin
  Result := TryAddKeyedScoped(TypeInfo(ServiceType), Key, TServiceFactory(Factory));
end;

function IServiceCollection.TryAddKeyedScoped<ServiceType>(const Key: TValue;
  const Factory: TServiceKeyedFactory<ServiceType>): IServiceCollection;
begin
  Result := Self;
  var Slot := KeyedSlot(TypeInfo(ServiceType), Key);
  if not CanTryAdd(Slot) then Exit;
  Result := AddKeyAwareDescriptor(Scoped, Slot, TServiceKeyedFactory(Factory));
end;

function IServiceCollection.TryAddKeyedScoped<ServiceType, TImpl>(const Key: TValue): IServiceCollection;
begin
  Result := TryAddKeyedScoped(TypeInfo(ServiceType), Key, PTypeInfo(TypeInfo(TImpl)).TypeData.ClassType);
end;

function IServiceCollection.TryAddKeyedTransient(const ServiceType: PTypeInfo; const Key: TValue;
  const Factory: TServiceFactory): IServiceCollection;
begin
  Result := Self;
  var Slot := KeyedSlot(ServiceType, Key);
  if not CanTryAdd(Slot) then Exit;
  Result := AddDescriptor(Transient, Slot, Factory, nil);
end;

function IServiceCollection.TryAddKeyedTransient(const ServiceType: PTypeInfo; const Key: TValue;
  const TImpl: TClass): IServiceCollection;
begin
  Result := Self;
  var Slot := KeyedSlot(ServiceType, Key);
  if not CanTryAdd(Slot) then Exit;
  Result := AddDescriptor(Transient, Slot, Factory.From(ServiceType, TImpl), TImpl);
end;

function IServiceCollection.TryAddKeyedTransient<ServiceType>(const Key: TValue;
  const Factory: TServiceFactory<ServiceType>): IServiceCollection;
begin
  Result := TryAddKeyedTransient(TypeInfo(ServiceType), Key, TServiceFactory(Factory));
end;

function IServiceCollection.TryAddKeyedTransient<ServiceType>(const Key: TValue;
  const Factory: TServiceKeyedFactory<ServiceType>): IServiceCollection;
begin
  Result := Self;
  var Slot := KeyedSlot(TypeInfo(ServiceType), Key);
  if not CanTryAdd(Slot) then Exit;
  Result := AddKeyAwareDescriptor(Transient, Slot, TServiceKeyedFactory(Factory));
end;

function IServiceCollection.TryAddKeyedTransient<ServiceType, TImpl>(const Key: TValue): IServiceCollection;
begin
  Result := TryAddKeyedTransient(TypeInfo(ServiceType), Key, PTypeInfo(TypeInfo(TImpl)).TypeData.ClassType);
end;

function IServiceCollection.TryAddKeyedSingleton(const ServiceType: PTypeInfo; const Key: TValue;
  const Implementor: IInterface): IServiceCollection;
begin
  Result := Self;
  var Slot := KeyedSlot(ServiceType, Key);
  if not CanTryAdd(Slot) then Exit;
  Result := AddDescriptor(Singleton, Slot, Factory.From(ServiceType, Implementor), nil);
end;

function IServiceCollection.TryAddKeyedSingleton<ServiceType>(const Key: TValue;
  const Implementor: ServiceType): IServiceCollection;
begin
  Result := TryAddKeyedSingleton(TypeInfo(ServiceType), Key, IInterface(Implementor));
end;

function IServiceCollection.RemoveAllKeyed(const ServiceType: PTypeInfo; const Key: TValue): IServiceCollection;
begin
  ServiceKey.Check(Key);
  FImplementor.RemoveAll(TServiceSlot.Create(ServiceType, Key));
  Result := Self;
end;

function IServiceCollection.RemoveAllKeyed<ServiceType>(const Key: TValue): IServiceCollection;
begin
  Result := RemoveAllKeyed(TypeInfo(ServiceType), Key);
end;

function IServiceCollection.ContainsKeyed(const ServiceType: PTypeInfo; const Key: TValue): Boolean;
begin
  ServiceKey.Check(Key);
  Result := FImplementor.Contains(TServiceSlot.Create(ServiceType, Key));
end;

function IServiceCollection.ContainsKeyed<ServiceType>(const Key: TValue): Boolean;
begin
  Result := ContainsKeyed(TypeInfo(ServiceType), Key);
end;

function IServiceCollection.KeyedDescriptors(const ServiceType: PTypeInfo;
  const Key: TValue): TArray<TServiceDescriptor>;
begin
  Result := nil;
  ServiceKey.Check(Key);
  var Slot := TServiceSlot.Create(ServiceType, Key);
  if not FImplementor.Contains(Slot) then Exit;
  Result := FImplementor.GetDescriptors(Slot).ToArray;
end;

function IServiceCollection.KeyedDescriptors<ServiceType>(const Key: TValue): TArray<TServiceDescriptor>;
begin
  Result := KeyedDescriptors(TypeInfo(ServiceType), Key);
end;

function IServiceCollection.Decorate(const ServiceType: PTypeInfo; const Decorator: TServiceDecorator): IServiceCollection;
begin
  FImplementor.CheckNotFrozen;
  Result := Self;
  // Lee y escribe el espacio SIN clave, y por tanto deja intactos los registros con clave del mismo
  // tipo. Decorar es por tipo, no por par: cruzarlo con las claves multiplicaria los casos y no se
  // acomete aqui. Consecuencia que hay que decir: un tipo que solo tiene registros con clave falla
  // abajo, porque desde aqui no hay nada que envolver.
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

  FImplementor.SetDescriptors(TServiceSlot.Create(ServiceType), Wrapped);
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

function IServiceCollection.AddInjector<T>(const Injector: TServiceInjector<T>): IServiceCollection;
begin
  FImplementor.CheckNotFrozen;
  // El envoltorio baja el cableado a TObject, que es como lo guarda la coleccion: dos
  // instanciaciones distintas de un tipo generico no son asignables entre si.
  FImplementor.AddInjector(T,
    procedure(const Instance: TObject; const Provider: IServiceProvider)
    begin
      Injector(Instance as T, Provider);
    end);
  Result := Self;
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
