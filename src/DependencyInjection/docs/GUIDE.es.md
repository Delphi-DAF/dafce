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
8. [Patrón AppModule](#patrón-appmodule)
9. [Resolución diferida (Lazy)](#resolución-diferida-lazy)
10. [Decorador Duck Typing](#decorador-duck-typing)
11. [Integración con Hosting](#integración-con-hosting)

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

El contenedor usa RTTI para construir `TMyServiceImpl`. Debe tener un constructor público sin parámetros, o bien utiliza una factoría.

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
Services.Add(TypeInfo(IMyService), TServiceLifeTime.Transient,
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

> ⚠️ **Dependencia cautiva**: nunca inyectes un servicio `Transient` o `Scoped` en un `Singleton` — la instancia de vida más corta quedará capturada durante la vida del Singleton. El contenedor no detecta esto automáticamente.

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
  [DafInjectProperties]
  TOrders = class(TInterfacedObject, IOrders)
  private
    FBilling: IBilling;
  public
    [DafInject]
    property Billing: IBilling read FBilling write FBilling;
  end;

  [DafInjectProperties]
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

Los dos servicios han de ser `Singleton` o `Scoped`. Un ciclo que pasa por un `Transient` se rechaza
—no por no poder resolverlo, sino por no poder deshacerlo: el contenedor no retiene los transient, así
que el par se sostiene a sí mismo y no hay quien lo desmonte—. Un ciclo con alguna arista por
constructor se rechaza también, y por la razón de siempre: el constructor no puede correr antes de que
exista su argumento.

Los dos rechazos nombran el camino completo del ciclo. `ValidateOnBuild` informa de todos los ciclos
del grafo al arrancar; con la validación apagada, la resolución que se mete en uno falla nombrando el
camino que recorrió.

⚠️ **Durante la resolución de un ciclo, un participante recibe a otro que todavía no tiene sus propias
propiedades rellenas.** Es inevitable —el ciclo hay que romperlo por algún sitio— y es la razón de que
un constructor no pueda suponer nada de una propiedad marcada.

Cerrar el contenedor vacía las propiedades inyectadas de todo lo que posee antes de soltar nada, así
que un ciclo se desmonta en vez de quedarse vivo.

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
clase de esa unit, una propiedad marcada **no existe** para el barrido: no se inyectaría nada y nada
lo diría. Por eso participar se declara en la clase —un atributo de clase sobrevive al recorte— y por
eso una clase que lo declara y no expone ninguna propiedad marcada es un error que nombra la clase y
señala las directivas.

El cableado por código es la salida: no usa RTTI en absoluto, así que es la única vía inmune al
recorte.

---

## Patrón AppModule

Divide los registros en módulos lógicos usando `IAppModule`; `TAppModule` es la clase base que lo
implementa. Los servicios y los módulos viven en una unit y no en el fichero de programa: un tipo
declarado en un `.dpr` no está en la sección `interface` de ninguna unit, y resolverlo aborta con
`ENonPublicType`.

Los dos bloques de abajo son `samples/ModuleComposition/`, literal;
`eng/check-quickstart-snippet.py` los compara con los ficheros, así que esto es código que lee el
compilador:

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

  // Su registro resuelve IClock desde el proveedor, asi que necesita que el modulo que lo aporta
  // se haya aplicado antes.
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
  // IAppModule es lo que le da un dueno.
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

Usa `Lazy.From<T>` para diferir la construcción hasta el primer uso. Útil cuando:
- El servicio es costoso de crear y puede que no se necesite
- Hay que romper dependencias circulares

```pascal
uses Daf.Extensions.DependencyInjection;

Services.AddTransient<IExpensiveService>(
  function(P: IServiceProvider): IExpensiveService
  begin
    Result := Lazy.From<IExpensiveService>(P,
      function(P2: IServiceProvider): IExpensiveService
      begin
        Result := TExpensiveService.Create;
      end);
  end);
```

El proxy devuelto implementa `IExpensiveService` mediante `TVirtualInterface`. La instancia real se crea en la primera llamada a un método.

---

## Decorador Duck Typing

`TDuckDecorator<T>` envuelve cualquier objeto e intercepta todas las llamadas a métodos. Úsalo para añadir comportamiento transversal (logging, caché, métricas) sin herencia:

```pascal
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

// Registrar como decorador
Services.AddTransient<IMyService>(
  function(P: IServiceProvider): IMyService
  begin
    var Inner: IMyService := TMyServiceImpl.Create;
    Result := TLoggingDecorator.Create(Inner);
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
