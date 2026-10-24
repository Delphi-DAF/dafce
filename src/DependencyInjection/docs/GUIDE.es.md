# DependencyInjection — Guía de uso

**🌍 Idioma: [English](GUIDE.md) | Español**

---

## Contenido

1. [Conceptos](#conceptos)
2. [Registro de servicios](#registro-de-servicios)
3. [Construir el contenedor](#construir-el-contenedor)
4. [Resolver servicios](#resolver-servicios)
5. [Tiempos de vida](#tiempos-de-vida)
6. [Scopes](#scopes)
7. [Inyección por propiedad](#inyección-por-propiedad)
8. [Servicios con clave](#servicios-con-clave)
9. [Patrón AppModule](#patrón-appmodule)
10. [Resolución diferida (Lazy)](#resolución-diferida-lazy)
11. [Decorador Duck Typing](#decorador-duck-typing)
12. [Integración con Hosting](#integración-con-hosting)

---

## Conceptos

El contenedor DI se articula en torno a tres tipos:

| Tipo | Rol |
|------|-----|
| `IServiceCollection` | Registro — añade descriptores de servicio antes de construir |
| `IServiceProvider` | Resolutor — resuelve servicios en tiempo de ejecución |
| `IServiceScope` | Frontera de vida — agrupa instancias Scoped |

`TServiceCollection` es la implementación concreta de `IServiceCollection`.  
Llama a `BuildServiceProvider` una sola vez para congelar la colección y obtener el `IServiceProvider`.

---

## Registro de servicios

### Por clase de implementación

```pascal
Services.AddSingleton<IMyService, TMyServiceImpl>;
Services.AddTransient<IMyService, TMyServiceImpl>;
Services.AddScoped<IMyService, TMyServiceImpl>;
```

El contenedor construye `TMyServiceImpl` por RTTI y **resuelve los argumentos de su
constructor**. Eso es inyección por constructor, y es la vía por defecto:

```pascal
type
  TMyServiceImpl = class(TInterfacedObject, IMyService)
  public
    constructor Create(const Config: IConfig; const Audit: IAuditLog);
  end;

Services.AddSingleton<IConfig, TFileConfig>;
Services.AddSingleton<IAuditLog, TDbAuditLog>;
Services.AddSingleton<IMyService, TMyServiceImpl>;   // los dos argumentos se resuelven
```

Qué constructor elige:

- Los candidatos son los constructores **públicos** que la clase declara. La que no declara
  ninguno usa el de su ancestro.
- Entre los que el contenedor puede satisfacer gana el que toma **más parámetros**. Otro que pide un
  servicio que el ganador no pide compite con él, y la resolución falla nombrando la clase y los dos
  constructores.
- `[ActivatorUtilitiesConstructor]` solo cuenta en `TActivatorUtilities.CreateInstance`, que toma el
  constructor marcado como único candidato; el contenedor lo ignora. Vive en
  `Daf.DependencyInjection.ActivatorUtilities`. Sin marca, `CreateInstance` falla cuando dos
  constructores que puede satisfacer empatan en número de parámetros.
- Un parámetro que nadie registró descarta su constructor, y también un `ILazy<X>` cuyo `X` nadie
  registra. Cuando eso no deja ninguno, la resolución falla nombrando el parámetro, su tipo y la
  clase.

Un parámetro puede pedir una clave —ver [Keyed Services](#keyed-services)— y una dependencia que
cerraría un ciclo entra por propiedad; ver [Property Injection](#property-injection).

### Por lambda de factoría

Control total sobre la construcción:

```pascal
Services.AddTransient<IMyService>(
  function(P: IServiceProvider): IMyService
  begin
    Result := TMyServiceImpl.Create(
      P.GetRequiredService<IConfig>,
      P.GetRequiredService<ILogger>
    );
  end);
```

### Por instancia existente (solo Singleton)

```pascal
var Repo: IMyRepository := TMyRepository.Create(ConnectionString);
Services.AddSingleton<IMyRepository>(Repo);
```

### Registro de bajo nivel (PTypeInfo)

Cuando los genéricos no están disponibles:

```pascal
Services.Add(TServiceLifeTime.Transient, TypeInfo(IMyService),
  function(P: IServiceProvider): IInterface
  begin
    Result := TMyServiceImpl.Create;
  end);
```

### Varios registros para la misma interfaz

Puedes registrar la misma interfaz varias veces:

```pascal
Services.AddTransient<IPlugin, TPluginA>;
Services.AddTransient<IPlugin, TPluginB>;

// Resolver todos
var Plugins := Provider.GetServices<IPlugin>;
for var P in Plugins do
  P.Execute;
```

---

## Construir el contenedor

```pascal
var Provider := Services.BuildServiceProvider;
```

- `BuildServiceProvider` **congela** la colección — llamadas posteriores a `Add*` lanzan una excepción.
- Solo puede llamarse **una vez** por instancia de `TServiceCollection`.
- El `IServiceProvider` devuelto es el scope raíz (los Singletons viven aquí).

**Siempre llama a `ShutDown` al terminar:**

```pascal
Provider.ShutDown;
```

Esto libera todas las instancias singleton gestionadas en orden inverso de creación.

### Comprobar qué sigue poseyendo el contenedor

`IContainerAccess` publica cuántos objetos sigue poseyendo el contenedor: el ámbito de singletons
más cada ámbito vivo. Los transient no se cuentan — el contenedor no los retiene, así que para él
dejan de existir en cuanto entrega uno.

Se llega a él por `QueryInterface`, no resolviéndolo como servicio:

```pascal
uses System.SysUtils; // Supports

var Access: IContainerAccess;
if Supports(Provider, IContainerAccess, Access) then
  WriteLn(Access.OwnedInstanceCount);
```

Cero después de `ShutDown` convierte «se liberó todo» en un hecho que un test afirma sobre
cualquier servicio, sin un detector de fugas y sin nombrarlos uno a uno.

El recuento es del contenedor y no del handle por el que se pregunta: un ámbito informa del mismo
número que el proveedor del que salió, porque los dos llegan a la misma fábrica de ámbitos. Se
puede leer mientras otros hilos crean y cierran ámbitos.

---

## Resolver servicios

| Método | Comportamiento |
|--------|---------------|
| `GetService<T>` | Devuelve `nil` si no está registrado |
| `GetRequiredService<T>` | Lanza `EServiceProviderError` si no está registrado |
| `TryGet<T>(out Service)` | Devuelve `False` si no está registrado (sin excepción) |
| `GetServices<T>` | Devuelve todos los registros para `T` como `IInterfaceList<T>` |
| `CanResolve(TypeInfo)` | `True` si existe al menos un registro |

```pascal
// Resolución segura
var Svc: IMyService;
if Provider.TryGet<IMyService>(Svc) then
  Svc.DoWork;

// Resolución requerida — excepción si falta
var Svc := Provider.GetRequiredService<IMyService>;

// Resolver todos
for var P in Provider.GetServices<IPlugin> do
  P.Execute;
```

---

## Tiempos de vida

| Tiempo de vida | Se crea | Se destruye |
|----------------|---------|-------------|
| `Singleton` | Una vez (primer resolve) | En `Provider.ShutDown` |
| `Scoped` | Una vez por scope | Al hacer `nil` del scope / `ShutDown` |
| `Transient` | En cada resolve | Cuando se libera la última referencia |

```pascal
// Singleton — siempre la misma instancia
Services.AddSingleton<ICache, TMemoryCache>;

// Scoped — nueva por scope (p. ej. por petición HTTP, por unidad de trabajo)
Services.AddScoped<IUnitOfWork, TEFUnitOfWork>;

// Transient — nueva cada vez
Services.AddTransient<IEmailSender, TSmtpSender>;
```

> ⚠️ **Dependencia cautiva**: un servicio `Transient` o `Scoped` inyectado en un `Singleton` queda capturado durante la vida del Singleton. Con `ValidateScopes`, que el host activa en `Development`, un `Singleton` que resuelve un `Scoped` falla con `EServiceProviderError`: por constructor, por factory, a través de un `ILazy` o después, por el proveedor que guardó. Sin la opción, el `Scoped` que recibe es el que da el proveedor raíz. Un `Transient` capturado no se detecta.

---

## Scopes

Usa scopes para agrupar servicios Scoped relacionados — por ejemplo, por petición o por unidad de trabajo:

```pascal
var Scope := Provider.CreateScope;
try
  var SP := Scope.ServiceProvider;
  var UoW := SP.GetRequiredService<IUnitOfWork>;
  var Repo := SP.GetRequiredService<IRepository>;
  // UoW y Repo comparten el mismo scope; las deps Scoped se comparten entre ellos
  UoW.Commit;
finally
  Scope := nil; // libera todas las instancias Scoped de este scope
end;
```

`IServiceScope` solo expone `ServiceProvider: IServiceProvider` — úsalo para resolver.

### Un ámbito por petición, desde un pool de hilos

El patrón de un servidor: un ámbito por unidad de trabajo, abierto en el hilo que la atiende y
cerrado al terminar. Nada tiene que ser de un solo hilo para que esto sea correcto.

```pascal
// Un proveedor para todo el proceso, construido una vez al arrancar.
procedure THandler.Handle(const Request: TRequest);
begin
  var Scope := FProvider.CreateScope;
  try
    var SP := Scope.ServiceProvider;
    // Los servicios Scoped viven lo que dure esta petición; los singletons se comparten
    // con todas las demás, y el contenedor construyó cada uno exactamente una vez.
    SP.GetRequiredService<IRequestHandler>.Run(Request);
  finally
    Scope := nil;
  end;
end;
```

Dos cosas de esto que se cuentan mal a menudo:

- **El ámbito no es una frontera de hilo.** El ámbito de arriba podría crearse en un hilo y usarse
  en otro, y varios hilos podrían compartirlo. Lo que un ámbito acota es la **vida** de sus
  servicios Scoped, y nada más.
- **Un ámbito por petición no aísla los singletons.** El ámbito de singletons es uno para todo el
  proveedor, así que todas las peticiones comparten esas instancias. Si un singleton guarda estado
  mutable, ese estado está compartido — y no hay reparto de ámbitos que lo cambie.

Resolver en paralelo es seguro; los servicios que resuelves son tan seguros como los hayas escrito.
La garantía completa y sus límites, en [Hilos](../README.es.md#hilos).

---

## Inyección por propiedad

Una dependencia puede entrar por una propiedad marcada en vez de por el constructor. El constructor
sigue siendo lo normal —lo que recibe está garantizado— y esto cubre los dos casos que él no puede.

### Dos servicios que se necesitan mutuamente

Un ciclo por constructor es inconstruible: ninguno de los dos puede correr antes de que el otro
termine. Por propiedad se resuelve, porque el contenedor publica cada instancia antes de rellenar sus
propiedades.

```pascal
type
  TOrders = class(TInterfacedObject, IOrders)
  private
    FBilling: IBilling;
  public
    [DafInject]
    property Billing: IBilling read FBilling write FBilling;
  end;

  TBilling = class(TInterfacedObject, IBilling)
  private
    FOrders: IOrders;
  public
    [DafInject]
    property Orders: IOrders read FOrders write FOrders;
  end;

Services.AddSingleton<IOrders, TOrders>;
Services.AddSingleton<IBilling, TBilling>;

var Orders := Provider.GetRequiredService<IOrders>;
// Orders.Billing.Orders es la misma instancia que Orders
```

Los dos servicios han de ser `Singleton` o `Scoped`. Un ciclo que pasa por un `Transient` se rechaza:
el contenedor no retiene los transient, así que el par se sostiene a sí mismo y no hay quien lo
desmonte. Un ciclo con alguna arista por constructor se rechaza también: el constructor no puede
correr antes de que exista su argumento.

Los dos rechazos nombran el camino completo del ciclo. `ValidateOnBuild` informa de todos los ciclos
del grafo al arrancar; con la validación apagada, la resolución que se mete en uno falla nombrando el
camino que recorrió.

⚠️ **Durante la resolución de un ciclo, un participante recibe a otro que todavía no tiene sus propias
propiedades rellenas.** Un constructor no puede suponer nada de una propiedad marcada.

Cerrar el contenedor vacía las propiedades inyectadas de todo lo que cableó antes de soltar nada, así
que un ciclo se desmonta en vez de quedarse vivo.

**Solo se cablea lo que el contenedor construye a partir de una clase.** Lo que devuelve una factory,
y una instancia registrada como tal, son de quien los construyó: el contenedor no les rellena ninguna
propiedad, no les ejecuta ningún cableado por código y no les vacía las propiedades al cerrar. Su
dueño los cablea a petición con `InjectInto`. Cuando se decora un servicio registrado por clase, el
original llega al decorador ya cableado.

### Cablear una instancia que el contenedor no construyó

Un formulario lo construye la plataforma, así que no hay constructor que el contenedor pueda elegir:

```pascal
constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  TActivatorUtilities.InjectInto(Provider, Self);
end;
```

Las mismas marcas, las mismas exigencias y los mismos errores que una resolución. Lo cableado así
**no** es del contenedor: no lo cuenta entre sus instancias, cerrarlo no lo destruye ni le vacía las
propiedades, y los servicios que sostenga viven mientras él los sostenga.

### Cablear por código

```pascal
Services.AddInjector<TReportService>(
  procedure(const Instance: TReportService; const Provider: IServiceProvider)
  begin
    Instance.Storage := Provider.GetRequiredKeyedService<IStorage>(BlobKey);
  end);
```

Es política del tipo: alcanza a toda instancia que el contenedor construya de esa clase o de una que
descienda de ella, y varios cableados corren en el orden en que se registraron. Corre después del
constructor y después de la inyección por atributo, así que ve las propiedades marcadas ya rellenas.
**No** alcanza a una instancia que el contenedor no construyó — esa la cablea su dueño con
`InjectInto`.

### El límite: una RTTI de propiedades recortada

`{$RTTI EXPLICIT PROPERTIES([])}` deja las propiedades de una unit entera fuera de la RTTI. Para una
clase de esa unit, una propiedad marcada **no existe** para el barrido: participar se deduce de
marcar, así que esa clase no participa en nada. Se resuelve sin error, su dependencia se queda en
`nil`, y el fallo aparece en el primer uso como un access violation.

⚠️ **El contenedor no puede avisar de esto.** La causa es una directiva que alguien escribió a
conciencia, en la cabecera de la misma unit que contiene la propiedad marcada.

El cableado por código es la salida: no usa RTTI en absoluto, así que es la única vía inmune al
recorte.

---

## Servicios con clave

Dos implementaciones del mismo contrato, distinguidas por una clave. `IStorage` para blobs y para
ficheros, `INotifier` por SMS y por correo. Sin esto, la decisión se traslada al consumidor —pedirlas
todas y elegir por `is`— o el contrato se parte en dos interfaces que solo existen para tener nombres
distintos.

```pascal
type
  // En una unit propia -- ver «La marca propia de la aplicación», más abajo
  TStorageKind = (Blob, FileSystem);

Services.AddKeyedSingleton<IStorage, TBlobStorage>(TValue.From<TStorageKind>(Blob));
Services.AddKeyedSingleton<IStorage, TFileStorage>(TValue.From<TStorageKind>(FileSystem));

var Blobs := Provider.GetRequiredKeyedService<IStorage>(TValue.From<TStorageKind>(Blob));
var Files := Provider.GetKeyedService<IStorage>(TValue.From<TStorageKind>(FileSystem));
var Todos := Provider.GetKeyedServices<IStorage>(TValue.From<TStorageKind>(Blob));
```

Dos registros del mismo tipo con claves distintas son **dos registros**: cada uno con su instancia y
su ciclo de vida.

### Qué puede ser una clave

Una cadena, un entero, un booleano, un valor de un enumerado, una metaclase o un `PTypeInfo`. Dos
claves son la misma cuando coinciden **su tipo y su valor**, y de ahí salen tres consecuencias que
conviene decir:

```pascal
Provider.GetKeyedService<IStorage>('42');            // NO es la clave 42
Provider.GetKeyedService<IStorage>(TValue.From<Boolean>(True));  // NO es la clave 1
// y dos enumerados de tipos distintos que comparten ordinal son claves distintas
```

Cualquier otra cosa —un registro, un objeto— se rechaza donde entra, nombrando el tipo: el contenedor
no puede definir una igualdad que esos tipos no tienen. Un `TGUID` se escribe como su cadena y no
pierde nada. Y un **carácter** no es una clave: una clave de un carácter es una clave de cadena.

**La clave enumerada es la forma recomendada.** No por estilo: renombrar un miembro propaga a todos
sus usos, y un typo es un error de compilación. `'bolb'` compila.

### Los dos espacios están aislados, en los dos sentidos

Es la regla que decide si la feature ayuda o estorba:

```pascal
Services.AddKeyedSingleton<IStorage, TBlobStorage>('blob');

Provider.GetService<IStorage>;            // nil -- el que tiene clave no se alcanza sin ella
Provider.GetServices<IStorage>;           // vacío -- no arrastra los que tienen clave
Provider.CanResolve(TypeInfo(IStorage));  // False

Services.AddSingleton<IStorage, TPlainStorage>;
Provider.GetKeyedService<IStorage>('blob');   // el de blob
Provider.GetRequiredService<IStorage>;        // el sin clave -- los dos conviven
```

Alcanza también a las operaciones destructivas, que es donde más importa:

| Operación | Alcanza |
|---|---|
| `Replace`, `RemoveAll` | únicamente los registros sin clave |
| `RemoveAllKeyed(Tipo, Clave)` | únicamente los registros de esa clave |
| `TryAdd*` | pregunta por el par, así que una clave no bloquea a otra ni al registro sin clave |
| `Decorate` | envuelve los registros sin clave y deja intactos los que la tienen |

⚠️ **`RemoveAll` cambió de significado.** Antes no había claves, así que «todos los del tipo» y «todos
los del tipo sin clave» eran lo mismo. Un arnés de specs que lo usara para desmontar un registro
desmonta ahora menos de lo que cree — y lo descubre en verde.

⚠️ **Decorar un tipo que solo tiene registros con clave falla** diciendo que no hay nada registrado
para él. Está leyendo el espacio sin clave, y el espacio sin clave está vacío.

### Pedir una clave desde un constructor o una propiedad

La misma marca sirve a las dos posiciones, y la clave viaja dentro de ella —el modelo de
`[Dependency("name")]` de Unity—:

```pascal
type
  TReportService = class(TInterfacedObject, IReportService)
  private
    FStorage: IStorage;
  public
    constructor Create([DafInject('audit')] const Audit: IAuditLog);
    [DafInject('file')]
    property Storage: IStorage read FStorage write FStorage;
  end;
```

Un parámetro o una propiedad **sin** clave siguen resolviéndose sin ella, y por tanto no alcanzan un
registro con clave: el aislamiento no hace excepciones por venir de una propiedad.

### La marca propia de la aplicación

`[DafInject]` admite una cadena, un entero, un booleano, una metaclase o un `TypeInfo(...)`. Para una
clave **enumerada** —la recomendada— la aplicación declara su propia marca en una línea:

```pascal
unit MiApp.MarcasDeInyeccion;   // ⚠️ una unit propia

interface

uses Daf.Extensions.DependencyInjection;

type
  TStorageKind = (Blob, FileSystem);

  StorageInjectAttribute = class(DafInjectAttribute<TStorageKind>);

implementation
end.
```

```pascal
[StorageInject(FileSystem)]
property Storage: IStorage read FStorage write FStorage;
```

El contenedor la reconoce **por descendencia**, así que nunca necesita conocer ese enumerado, y el
tipo del enumerado se conserva: otro enumerado con el mismo ordinal no la alcanza.

⚠️ **La marca va en una unit propia, separada de las clases que la usan.** Declarada en la misma unit,
leer el atributo aborta con un access violation. El framework no puede detectarlo, y el fallo es
ruidoso pero ocurre en ejecución.

⚠️ **El genérico no se escribe nunca dentro de un `[ ]`.** `[DafInject<TStorageKind>(...)]`
**compila**, con un `W1074 Unknown custom attribute` que casi nadie lee, y en ejecución
`GetAttributes` devuelve **cero**: la inyección no ocurre y nada lo dice.

Para una clave enumerada suelta, sin declarar marca, queda un escape:

```pascal
[DafInject(TypeInfo(TStorageKind), Ord(FileSystem))]
```

El ordinal se comprueba contra el rango del tipo. Uno que no le pertenece falla nombrando la
propiedad.

### Un registro para cualquier clave

`ServiceKey.Any` es el comodín: atiende toda clave de su tipo de servicio que no tenga registro
propio, y su factoría recibe cuál se pidió.

```pascal
Services.AddKeyedSingleton<IStorage>(ServiceKey.Any,
  function(P: IServiceProvider; const Key: TValue): IStorage
  begin
    Result := TFolderStorage.Create(Key.ToString);
  end);
```

- **Un registro concreto le gana**: al comodín se le pregunta solo al fallar la búsqueda directa, el
  mismo orden que ya rige entre un tipo cerrado y uno sin fijar.
- **Cada clave que atiende recibe su propia instancia**, y pedir dos veces la misma da la misma.
- **No cruza al espacio sin clave**: con solo un comodín registrado, `GetService<IStorage>` no
  encuentra nada.

El centinela es un valor de un tipo declarado dentro del módulo de DI, así que nadie de fuera puede
nombrarlo.

### El límite: claves y genéricos sin fijar no se combinan

```pascal
Services.AddKeyedSingleton<IBox<_>, TBox<_>>('blob');   // falla al registrar
```

Falla **al registrar** y no al resolver.

---

## Patrón AppModule

Divide los registros en módulos lógicos usando `IAppModule`; `TAppModule` es la clase base que lo
implementa. Los servicios y los módulos viven en una unit y no en el fichero de programa: un tipo
declarado en un `.dpr` no está en la sección `interface` de ninguna unit, y resolverlo aborta con
`ENonPublicType`.

Los dos bloques de abajo son `samples/ModuleComposition/`, literal:

<!-- quickstart: src/DependencyInjection/samples/ModuleComposition/ModuleComposition.Services.pas -->

```pascal
unit ModuleComposition.Services;

// Los tipos van en una unit y no en el .dpr por obligacion del contenedor: un tipo declarado en el
// fichero de programa no esta en la seccion interface de ninguna unit, y resolverlo aborta en
// tiempo de ejecucion con ENonPublicType.

interface

uses
  Daf.Extensions.DependencyInjection,
  Daf.AppModule.DependencyInjection;

type
  IClock = interface(IInvokable)
    ['{0ED801D1-8529-4C2F-860F-472419E770B3}']
    function Today: string;
  end;

  TSystemClock = class(TInterfacedObject, IClock)
  public
    function Today: string;
  end;

  IGreeter = interface(IInvokable)
    ['{6803CD07-1D7D-4FA0-A890-65ED845860B1}']
    function Greet(const Name: string): string;
  end;

  TGreeter = class(TInterfacedObject, IGreeter)
  strict private
    FClock: IClock;
  public
    constructor Create(const AClock: IClock);
    function Greet(const Name: string): string;
  end;

  IBanner = interface(IInvokable)
    ['{E9A88893-3081-4181-8279-B189E6CF33DB}']
    function Text: string;
  end;

  TBanner = class(TInterfacedObject, IBanner)
  strict private
    FText: string;
  public
    constructor Create(const AText: string);
    function Text: string;
  end;

  // Los modulos se declaran en la interface para que el linker no los descarte: un tipo que solo
  // aparece en implementation y no se referencia se elimina como codigo muerto.
  TInfrastructureModule = class(TAppModule)
  public
    procedure AddServices(const ServiceCollection: IServiceCollection); override;
  end;

  // Su registro resuelve IClock cuando alguien pide el greeter, no al componer: para entonces
  // estan aplicados los tres modulos, asi que el orden entre este y el que aporta IClock da igual.
  TDomainModule = class(TAppModule)
  public
    procedure AddServices(const ServiceCollection: IServiceCollection); override;
  end;

  // Solo tiene constructor con parametro: es el caso que unicamente cubre la sobrecarga de
  // instancia, porque el modulo corre antes de que exista el proveedor.
  TBannerModule = class(TAppModule)
  strict private
    FText: string;
  public
    constructor Create(const AText: string);
    procedure AddServices(const ServiceCollection: IServiceCollection); override;
  end;

implementation

uses
  System.SysUtils;

{ TSystemClock }

function TSystemClock.Today: string;
begin
  Result := FormatDateTime('yyyy-mm-dd', Now);
end;

{ TGreeter }

constructor TGreeter.Create(const AClock: IClock);
begin
  inherited Create;
  FClock := AClock;
end;

function TGreeter.Greet(const Name: string): string;
begin
  Result := Format('Hola, %s. Hoy es %s', [Name, FClock.Today]);
end;

{ TBanner }

constructor TBanner.Create(const AText: string);
begin
  inherited Create;
  FText := AText;
end;

function TBanner.Text: string;
begin
  Result := FText;
end;

{ TInfrastructureModule }

procedure TInfrastructureModule.AddServices(const ServiceCollection: IServiceCollection);
begin
  ServiceCollection.AddSingleton<IClock, TSystemClock>;
end;

{ TDomainModule }

procedure TDomainModule.AddServices(const ServiceCollection: IServiceCollection);
begin
  ServiceCollection.AddSingleton<IGreeter>(
    function(Provider: IServiceProvider): IGreeter
    begin
      Result := TGreeter.Create(Provider.GetRequiredService<IClock>);
    end);
end;

{ TBannerModule }

constructor TBannerModule.Create(const AText: string);
begin
  inherited Create;
  FText := AText;
end;

procedure TBannerModule.AddServices(const ServiceCollection: IServiceCollection);
begin
  ServiceCollection.AddSingleton<IBanner>(TBanner.Create(FText) as IBanner);
end;

end.
```

Aplícalos desde el punto de entrada de la aplicación, un `AddModule<T>` por módulo:

<!-- quickstart: src/DependencyInjection/samples/ModuleComposition/ModuleComposition.dpr -->

```pascal
program ModuleComposition;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  Daf.Extensions.DependencyInjection,
  Daf.DependencyInjection,
  ModuleComposition.Services in 'ModuleComposition.Services.pas';

begin
  var Services: IServiceCollection := TServiceCollection.Create;

  // AddModule<T> instancia el modulo y llama a su AddServices en el acto: lo que se escribe
  // despues de un modulo se registra despues de el.
  Services
    .AddModule<TInfrastructureModule>
    .AddModule<TDomainModule>;

  // Un modulo con configuracion no la puede recibir del contenedor —corre antes de que exista el
  // proveedor—, asi que la trae el llamante por la sobrecarga de instancia. Entregarlo como
  // IAppModule es lo que le da un dueño.
  var Banner: IAppModule := TBannerModule.Create('Composicion por modulos');
  Services.AddModule(Banner);

  var Provider := Services.BuildServiceProvider;

  Writeln(Provider.GetRequiredService<IBanner>.Text);
  Writeln(Provider.GetRequiredService<IGreeter>.Greet('DAF'));
end.
```

`AddModule<T>` instancia el módulo, llama a su `AddServices` en el sitio y se hace cargo de su
vida. No llames a `TInfrastructureModule.Create.AddServices(Services)` a mano: el módulo es un
`TInterfacedObject` creado con contador de referencias a cero, así que nadie lo libera.

Un módulo que lleva configuración no la puede recibir del contenedor —corre antes de que exista el
proveedor—, así que la construye el llamante y la entrega como `IAppModule`, que es lo que le da
un dueño. Eso es `TBannerModule` de arriba.

**No hay descubrimiento automático**, ni con `Hosting` ni sin él: un módulo se registra donde está
escrito. El orden de registro, `TryAdd`/`Replace`/`Decorate` y la alternativa de agrupar con una
función, en [Composición del arranque](STARTUP-COMPOSITION.es.md).

---

## Resolución diferida (Lazy)

**Para qué está: dos servicios que se necesitan mutuamente.** Un ciclo por constructor no se puede
construir — ninguno de los dos puede correr antes de que el otro termine. Cuando uno de los dos
lados declara al otro como `ILazy<T>`, su constructor deja de esperar y el ciclo se cierra en el
primer uso.

**Lo pide el consumidor, y el registro no cambia.** Declara `ILazy<T>` en el constructor en vez de
`T` y el contenedor lo entrega. El servicio se registra como cualquier otro, y otro consumidor que
pida `T` lo sigue recibiendo construido.

```pascal
uses Daf.Extensions.DependencyInjection;

type
  TReportService = class(TInterfacedObject, IReportService)
  strict private
    FArchive: ILazy<IArchive>;
  public
    constructor Create(const AArchive: ILazy<IArchive>);
    procedure Publish;
  end;

constructor TReportService.Create(const AArchive: ILazy<IArchive>);
begin
  inherited Create;
  FArchive := AArchive;     // aquí no se construye nada
end;

procedure TReportService.Publish;
begin
  FArchive.Value.Store(BuildReport);   // aquí sí, en el primer uso
end;
```

```pascal
// El archivo se registra como cualquier otro servicio. Aquí no se dice nada de pereza.
Services.AddSingleton<IArchive, TArchive>;
Services.AddSingleton<IReportService, TReportService>;
```

**Qué hace `Value`: preguntarle al contenedor, cada vez.** Aquí no se guarda nada, así que el ciclo
de vida es exactamente el que declaró el registro — un `Singleton` devuelve la misma instancia en
cada `Value`, un `Transient` una nueva cada vez, un `Scoped` la de su ámbito. Guardar copia
sostendría además al servicio a través de quien sostiene el proxy, que es justo el ciclo que esto
existe para romper.

**Una ganancia colateral:** un servicio costoso que puede que no haga falta deja de construirse solo
para construir a quien lo tiene.

La otra vía para romper un ciclo, y la que conviene cuando la dependencia se usa en todos los
caminos, es la [inyección por propiedad](#inyección-por-propiedad).

⚠️ **Un servicio que no ha registrado nadie se rechaza donde se construye quien lo pide**, nombrando
el parámetro y su clase — no en el primer `Value`, lejos de la causa. Aplazar la construcción no es
aplazar la pregunta.

### `Lazy.From<T>`, la forma manual

`Lazy.From<T>` construye un proxy de la misma clase a partir de una factoría que escribes tú, y
devuelve la propia `T` en vez de un `ILazy<T>`. Es la vía para lo que no pasa por un constructor;
cuando hay constructor, `ILazy<T>` dice lo mismo con menos y no toca el registro.

⚠️ **Esta sí guarda lo que construyó**, al revés que la que entrega el contenedor: la factoría es
tuya y puede costar lo que sea, así que correrla en cada llamada a un método sería otra cosa. Corre
una sola vez aunque entren varios hilos, y una factoría que no devuelve nada falla nombrando el
contrato en vez de contestar el cero del tipo de retorno.

---

## Decorador Duck Typing

`TDuckDecorator<T>` envuelve cualquier objeto e intercepta todas las llamadas a métodos. Úsalo para añadir comportamiento transversal (logging, caché, métricas) sin herencia:

```pascal
uses System.SysUtils; // Supports

type
  TLoggingDecorator = class(TDuckDecorator<IMyService>)
  protected
    procedure BeforeCall(Method: TRttiMethod; const Args: TArray<TValue>;
      var Result: TValue); override;
    procedure AfterCall(Method: TRttiMethod; const Args: TArray<TValue>;
      var Result: TValue); override;
  end;

procedure TLoggingDecorator.BeforeCall(Method: TRttiMethod; ...);
begin
  Logger.Log('Llamando: ' + Method.Name);
end;

// Registrar como decorador. El decorador es un TVirtualInterface: implementa la
// interfaz en ejecución, así que se entrega con Supports y no se asigna.
Services.AddTransient<IMyService>(
  function(P: IServiceProvider): IMyService
  begin
    var Inner: IMyService := TMyServiceImpl.Create;
    Supports(TLoggingDecorator.Create(Inner), IMyService, Result);
  end);
```

---

## Integración con Hosting

Con el módulo `Hosting`, el `IServiceCollection` lo proporciona el builder de la aplicación. Nunca llamas a `BuildServiceProvider` directamente:

```pascal
uses Daf.Cli, Daf.CommandLine, Daf.Configuration.CommandLine, Daf.Extensions.DependencyInjection;

var B := TCliApplication.CreateBuilder(CommandLineConfig.ProcessArgs);

B.Services.AddSingleton<IMyService, TMyService>;

ExitCode := B.Build.Run;
```

El `IServiceProvider` queda accesible a través de `IHost.Services` o mediante inyección en los servicios alojados.

Consulta [Hosting](../../Hosting/docs/GUIDE.es.md) para la guía completa de integración.
