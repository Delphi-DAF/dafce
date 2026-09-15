# DependencyInjection.Abstractions

**🌍 Idioma: [English](README.md) | Español**

Contratos principales (interfaces y tipos) del sistema de Inyección de Dependencias de DAF. Este módulo **no contiene implementación** — define la forma de la que dependen los consumidores y que `DependencyInjection` implementa.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![Licencia](https://img.shields.io/badge/licencia-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Qué hay en este módulo

| Unidad | Contenido |
|--------|-----------|
| `Daf.Extensions.DependencyInjection` | `IServiceCollection`, `IServiceProvider`, `IServiceScope`, `IAppModule`, tiempos de vida, `Factory`, `TDuckDecorator<T>`, `Lazy` |
| `Daf.Extensions.Options` | `IOptions<T>`, `TOptionsValidator<T>`, `EOptionsValidationError` — el contrato del patrón de opciones |
| `Daf.Options` | `TOptions<T>` y `AppOptions` — ajustes registrados con código |
| `Daf.AppModule` | **Alias de compatibilidad** de `IAppModule`: el contrato se mudó junto a `AddModule<T>`, que es método del record `IServiceCollection` y no puede depender de esta unit. Un `uses Daf.AppModule` que ya existiera sigue valiendo |
| `Daf.DependencyInjection.ActivatorUtilities` | `TActivatorUtilities`: construir un objeto con sus argumentos de constructor resueltos, e `InjectInto` para cablear una instancia que el contenedor no construyó |
| `Daf.DependencyInjection.OpenGenerics` | Emparejamiento, comprobaciones y activación de los registros que dejan sus argumentos de tipo sin fijar |

---

## Tipos clave

### `IServiceCollection`

Constructor fluido para registrar servicios antes de construir el contenedor.

```pascal
Services
  .AddSingleton<ICache, TMemoryCache>
  .AddTransient<IRepo, TRepoImpl>
  .AddScoped<IUoW, TDbUoW>;

var Provider := Services.BuildServiceProvider;
```

### `IServiceProvider`

El contenedor resuelto. Úsalo para obtener instancias de servicios.

```pascal
var Svc := Provider.GetRequiredService<IMyService>;
var Opt: IMyService;
if Provider.TryGet<IMyService>(Opt) then ...
var All := Provider.GetServices<IPlugin>;
```

### `TServiceLifeTime`

```pascal
type TServiceLifeTime = (Singleton, Scoped, Transient);
```

### `IServiceScope`

Frontera de tiempo de vida. Los servicios Scoped se comparten dentro de un scope y se liberan cuando este termina.

```pascal
var Scope := Provider.CreateScope;
var Svc := Scope.ServiceProvider.GetRequiredService<IScopedService>;
Scope := nil; // libera las instancias Scoped
```

### `IAppModule`

Convención para agrupar registros de servicios:

```pascal
type
  IAppModule = interface(IInvokable)
    procedure AddServices(const ServiceCollection: IServiceCollection);
  end;
```

### `ServiceKey` y `TServiceSlot`

`ServiceKey` es todo lo que el contenedor sabe de una clave de servicio: si un `TValue` puede serlo,
cómo se comparan dos, cómo se nombra una en un mensaje, y cuál es el comodín.

```pascal
ServiceKey.IsValid(Key);   // una cadena, un entero, un booleano, un enumerado, una metaclase
                           // o un PTypeInfo -- y la vacía, que significa "sin clave"
ServiceKey.Check(Key);     // aborta nombrando el tipo cuando no puede serlo
ServiceKey.Text(Key);      // para un mensaje, y para nada más
ServiceKey.Any;            // la clave comodín
```

Dos claves son la misma cuando coinciden **su tipo y su valor**, así que dos enumerados de tipos
distintos que compartan ordinal son claves distintas. El rechazo ocurre donde la clave **entra** —al
registrar y al resolver— y nunca al comparar dos claves.

`TServiceSlot` es el par `(tipo de servicio, clave)` por el que indexa la colección. Una clave vacía
**es** el espacio sin clave, no un tercer estado.

### `TServiceKeyedFactory<T>`

```pascal
TServiceKeyedFactory<ServiceType: IInterface> =
  reference to function(Provider: IServiceProvider; const Key: TValue): ServiceType;
```

Una factoría enterada de **qué clave** se le está pidiendo. Es lo que hace útil un registro comodín:
lo único que puede hacer una factoría que atiende a todas las claves es decidir en función de cuál le
toca.

### `DafInject`

La marca de la inyección por propiedad: va en cada propiedad que el contenedor rellena, y una clase
participa por tenerla. No hay nada que declarar en la clase.

```pascal
TReportService = class(TInterfacedObject, IReportService)
private
  FStorage: IStorage;
public
  [DafInject]
  property Storage: IStorage read FStorage write FStorage;
end;
```

La propiedad ha de ser de tipo interfaz y escribible. Una propiedad marcada que no se puede rellenar
levanta `EPropertyInjectionError` nombrando la propiedad, su tipo y la clase.

**Una marca y no dos.** Bajo una RTTI recortada la inyección está apagada y nada lo dice.

La clave viaja **dentro de esta marca**, y la misma marca sirve a un **parámetro de constructor**:

```pascal
constructor Create([DafInject('audit')] const Audit: IAuditLog);
```

`[DafInject]` admite una cadena, un entero, un booleano, una metaclase, un `TypeInfo(...)`, o un
`TypeInfo(...)` más un ordinal. No puede admitir un `TValue`: el argumento de un atributo tiene que
ser una constante, y un `TValue` ahí da `E2026` hasta con un literal de cadena — el `TValue` se
construye dentro del constructor, al que llama la RTTI al leer el atributo.

Para una clave **enumerada**, la aplicación declara su propia marca sobre la base genérica, en una
línea:

```pascal
type
  StorageInjectAttribute = class(DafInjectAttribute<TStorageKind>);
```

El barrido la reconoce **por descendencia** (`Attr is DafInjectAttribute`), así que el framework nunca
necesita conocer ese enumerado. ⚠️ **Esa marca va en una unit propia**, separada de las clases que la
usan: declarada en la misma unit, leer el atributo aborta con un access violation. ⚠️ **Y el genérico
no se escribe nunca dentro de un `[ ]`** — `[DafInject<TStorageKind>(...)]` compila con un `W1074` y
en ejecución la propiedad no tiene ningún atributo.

### `TServiceInjector<T>` y `AddInjector<T>`

El cableado por código, para lo que no se puede anotar:

```pascal
Services.AddInjector<TReportService>(
  procedure(const Instance: TReportService; const Provider: IServiceProvider)
  begin
    Instance.Storage := Provider.GetRequiredService<IStorage>;
  end);
```

Es política del tipo —alcanza también a sus descendientes— y corre después del constructor y después
de la inyección por atributo, sobre toda instancia que el contenedor construya. No usa RTTI, así que
es la única vía inmune a una RTTI de propiedades recortada.

### `Factory`

Helper para construir un `TServiceFactory` desde una clase o instancia:

```pascal
Services.Add(TypeInfo(IMyService), Singleton, Factory.From(TypeInfo(IMyService), TMyImpl));
```

### `TDuckDecorator<T>`

Clase base para envolver un servicio con lógica pre/post-llamada sin herencia. Sobreescribe `BeforeCall` / `AfterCall`.

### `Lazy`

```pascal
Result := Lazy.From<IHeavy>(Provider,
  function(P: IServiceProvider): IHeavy
  begin
    Result := THeavy.Create;
  end);
```

### `TActivatorUtilities`

Construir un objeto que el contenedor no tiene registrado, con sus argumentos de constructor
resueltos:

```pascal
uses Daf.DependencyInjection.ActivatorUtilities;

var Builder := TActivatorUtilities.CreateInstance<TReportBuilder>(Provider);
try
  Builder.Run;
finally
  Builder.Free;   // no es un servicio registrado: el dueño es quien llama
end;
```

De los constructores públicos cuyos argumentos el proveedor puede satisfacer se usa el de **más
parámetros**. Una marca lo decide:

```pascal
[ActivatorUtilitiesConstructor]
constructor Create(const Storage: IStorage);
```

⚠️ Un constructor marcado cuyos argumentos no se puedan resolver todos se salta como cualquier otro,
y la elección vuelve a los demás.

`InjectInto(Provider, Instance)` rellena las propiedades `[DafInject]` de una instancia que el
contenedor no construyó —un formulario que crea la plataforma, por ejemplo—. **No toma posesión**:
cerrar el contenedor ni destruye esa instancia ni le vacía las propiedades.

---

## Ajustes que un servicio pide por constructor

Un servicio declara en su constructor los ajustes que necesita; el contenedor los construye, es su
dueño y los libera. Nadie escribe un `Free`:

```pascal
uses Daf.Options;

AppOptions.Configure<TCacheSettings>(Services,
  procedure(S: TCacheSettings)
  begin
    S.Host := 'localhost';
    S.MaxItems := 500;
  end);

Services.AddSingleton<ICache, TCache>;
```

```pascal
constructor TCache.Create(const Options: IOptions<TCacheSettings>);
begin
  inherited Create;
  FHost := Options.Value.Host;
  FMaxItems := Options.Value.MaxItems;
end;
```

`AppOptions.Configure` rellena los ajustes **con código** — sin fichero, sin variable de entorno,
sin nada que leer. Unos valores calculados en el arranque, o escritos por una prueba, entran por la
misma puerta que los que vienen de un fichero, y el servicio que los recibe no distingue de dónde
salen. Llenarlos desde una sección de configuración es `ConfigOptions.Bind`, en
[`Configuration`](../Configuration/README.es.md).

El valor se construye en el **primer uso**, no al registrarse, y **una sola vez**: unos ajustes que la
ejecución no llega a usar no leen ni validan nada.

### El valor tiene dueño, y aquí es donde muerde

`Value` devuelve un objeto **crudo**, y su dueño es la opción. Delphi no tiene recolector, así que
quien se guarde el objeto y suelte la opción se queda con un puntero colgando — un riesgo que no
existe allí de donde se porta esto.

Dos formas correctas de consumirlo, y no hay una tercera:

```pascal
// sostener la opción en un campo, y leer Value cuando haga falta
FOptions := Options;

// o copiar lo que se necesite en el constructor, que es lo que quiere casi todo servicio
FHost := Options.Value.Host;
```

### La validación corre al construir el valor, y nombra el ajuste

```pascal
AppOptions.Configure<TCacheSettings>(Services,
  procedure(S: TCacheSettings)
  begin
    S.MaxItems := Cfg.MaxItems;
  end,
  function(const S: TCacheSettings; out Error: string): Boolean
  begin
    Result := S.MaxItems > 0;
    if not Result then
      Error := Format('MaxItems must be greater than zero, and it is %d', [S.MaxItems]);
  end);
```

Corre una vez, al construirse el valor, y un fallo levanta `EOptionsValidationError` **nombrando el
ajuste**.

### Lo que nadie registró no se inventa

Pedir unos ajustes que nadie registró **falla nombrando el tipo**. Es una divergencia con el original
que se porta: en .NET un único registro sin fijar sirve a todos los tipos de ajustes a la vez, así que
pedir unos que nadie configuró devuelve un objeto recién construido con todo a cero, sin decir nada.

**`IOptions<TSeccion>` no se deriva de una clase de ajustes padre que tenga una propiedad de ese
tipo.** .NET tampoco lo hace. La sección se registra contra su propia ruta, y si se registran las
dos, el objeto anidado del padre y el de la sección son **dos instancias distintas**.

### Unos ajustes con miembros objeto liberan lo suyo

Una clase de ajustes con miembros de tipo objeto **los libera ella**. La opción libera el objeto
raíz y nada más. Ver la
[guía de Configuration](../Configuration/docs/GUIDE.es.md), que es donde el enlazador construye esos
miembros.

---

## Registrar un contrato genérico una sola vez

Un contrato genérico se puede registrar **sin fijar sus argumentos de tipo**, y ese único registro
sirve a todos sus cierres:

```pascal
Services.AddSingleton<IRepo<_>, TRepo<_>>;

// no hay nada más registrado, y estos dos resuelven
var Users  := Provider.GetRequiredService<IRepo<TUser>>;
var Orders := Provider.GetRequiredService<IRepo<TOrder>>;
```

`_` es el marcador que declara `Daf.Rtti`. `IRepo<_>` es una instanciación cerrada corriente con
su propio tipo, así que registrarla no necesita nada nuevo — lo que el contenedor aprende es a
**emparejar** al resolver. De ahí salen tres reglas:

- el ciclo de vida cuenta **por tipo cerrado**: un singleton sin fijar es una instancia por cierre,
  no una en total
- un registro escrito para un cierre concreto gana siempre al registro sin fijar
- el nombre y la aridad identifican el contrato, así que uno del mismo nombre y otra aridad no
  empareja

### `_ClosedArgs`, o cómo se entera la implementación de cuál es su cierre

Delphi cierra los genéricos al compilar. Dentro de una implementación registrada sin fijar, `T`
**es** el marcador: `TypeInfo(T)` y `T.Create` no sirven de nada ahí — el constructor del marcador
lo dice en voz alta en vez de construir uno en silencio. Todo lo que la implementación necesite del
tipo de verdad le llega por un campo de nombre fijo que el contenedor rellena **antes de que corra
el constructor**, que es donde un servicio decide casi todo lo que es:

```pascal
type
  TRepoArgs = record
    T1: TEntityClass;                     // la cota, expresada por el tipo del campo
  end;

  TRepo<T: class> = class(TInterfacedObject, IRepo<T>)
  private
    _ClosedArgs: TRepoArgs;
  public
    constructor Create;
  end;

constructor TRepo<T>.Create;
begin
  inherited Create;
  FTable := _ClosedArgs.T1.ClassName;     // TUser, TOrder, ...
end;
```

**Declararlo es opcional.** Una implementación que no necesita nada del cierre —una implementación
nula, un contenedor que solo guarda— no declara el campo y no paga nada por ello. No declararlo es
exactamente igual que declararlo con una posición por argumento de tipo y todas de tipo `TClass`:
nada que rellenar, y ninguna cota que comprobar. Y fíjate en lo que eso *no* dice — el argumento no
pierde su clase real, el cierre la sigue nombrando. Lo que falta es la restricción.

Siete cosas que saber de ese campo, y cada una es una forma de equivocarse:

- **Se lee, y no se asigna nunca.** El prefijo `_` significa «esto lo opera otro», como en
  `_AddRef` — aquí con la flecha al revés: lo declaras tú, lo escribe el contenedor. Leerlo es
  gratis y es para lo que está.
- **Liga la posición; el nombre solo documenta.** Delphi no emite nada sobre los nombres de los
  parámetros de un genérico, así que el contenedor ata los campos del record a los argumentos de
  tipo **por orden de declaración**. Un record con los campos al revés compila y hace lo contrario.
- **El tipo del campo es la cota.** `class of TEntity` dice «solo entidades», y un cierre que no la
  satisface se rechaza nombrando el argumento y la cota que incumple.
- **`TClass` significa sin cota — y también sin despacho.** `UnTClass.Create` llama a
  `TObject.Create` y nunca al constructor propio de la clase, ni siquiera declarándolo `virtual`,
  porque `TClass` es `class of TObject` y `TObject.Create` no lo es. Devuelve un objeto de la clase
  correcta con los campos a cero, que se lee como un acierto. Tipa cada campo tan estrecho como
  puedas; donde de verdad no haya cota, construye con `TActivator.CreateInstance`.
- **El record va fuera de la clase.** Dentro, `T1` ya es el parámetro genérico:
  `E2004 Identifier redeclared`.
- **La visibilidad la eliges tú, y `private` es la que hay que elegir.** Lo escribe el contenedor y
  lo lee la propia clase; nadie de fuera tiene nada que hacer con él, así que ponerlo en la
  superficie del tipo es ensancharla a cambio de nada. Las directivas RTTI por defecto acotan a
  public y published los **métodos y las propiedades** —para los **campos** incluyen las cuatro
  visibilidades—, así que el contenedor lo encuentra en la más restrictiva que hay.
- **Cuidado con dónde cae el campo dentro de la clase.** En Delphi un bloque `class var` sigue
  abierto hasta el siguiente especificador de visibilidad, así que un campo escrito detrás es
  también un class var — que la RTTI no ve y que comparten todos los cierres. Esto es Delphi, no
  DAF: el contenedor no ve campo ninguno, no escribe nada, y `_ClosedArgs` sale a `nil` en el
  constructor. Pon un especificador de visibilidad entre los dos.

### Para qué sirve esto, y para qué no

Sirve para **reenviadores y contenedores agnósticos del tipo**. `ILogger<T>` es el caso canónico:
sostiene el registrador de verdad y su única relación con el argumento es usar su nombre como
categoría. No sirve para nada que necesite el tipo *como tipo* más allá de lo que `_ClosedArgs`
puede darle.

La prueba se hace con un dedo. **Tapa cada `T` del cuerpo** de la implementación: si el código
sigue en pie, es registrable sin fijar. Si algo necesitaba que `T` fuera `T` — un `T.Create`, un
`TypeInfo(T)`, una variable de tipo `T` con un valor de verdad dentro —, no lo es.

Y tres límites:

- **Todos los argumentos o ninguno.** `IFoo<TConocido, _>`, fijando unos y dejando otros sin fijar,
  no está soportado.
- **El argumento tiene que viajar como referencia.** Un contrato donde `T` aparezca en una posición
  cuya representación dependa de `T` —devuelto por valor, un record, un entero— no se puede servir
  por esta vía. El contenedor **compara los dos contratos método a método y lo rechaza**, nombrando
  el método. Lo rechaza sea lo que sea lo que declare la implementación, con `_ClosedArgs` o sin él —
  que un emparejamiento sea seguro es una propiedad de los dos contratos. Para saberlo de antemano,
  hazle la prueba del dedo al **contrato** en vez de al cuerpo de la implementación: lee las firmas, y
  si `T` aparece en algún sitio que no sea un objeto, una interfaz o una referencia de clase, el
  contrato no es registrable sin fijar.
- **El emparejamiento es por nombre y aridad, no por identidad.** Dos familias genéricas del mismo
  nombre simple y la misma aridad, declaradas en units distintas, emparejan. Lo que queda debajo no
  es la nada —esa misma comparación método a método rechaza el emparejamiento nombrando el primer
  método que no encaja— pero es una red, no una comprobación de identidad.

### Una factoría escrita a mano puede enterarse de qué le piden

La ruta por clase de arriba rellena `_ClosedArgs` antes de que corra el constructor, así que una
implementación registrada sin fijar conoce su cierre. Una **factoría escrita a mano** que recibe solo
el proveedor, no: una única factoría atiende todos los cierres y devuelve el mismo objeto a todos --
un `ILogger<TFacturas>` escribiendo bajo la categoría de otro cierre, sin que nada se queje.

Un segundo tipo de factoría recibe, además del proveedor, el tipo que se está resolviendo:

```pascal
Services.AddSingleton<IRepo<_>>(
  function(P: IServiceProvider; RequestedType: PTypeInfo): IRepo<_>
  begin
    Result := TRepo.Create(RequestedType);   // el cierre concreto IRepo<TUser>, IRepo<TOrder>, ...
  end);
```

La sobrecarga se elige por cuántos parámetros declara la lambda -- una factoría de un parámetro sigue
siendo de un parámetro, y no cambia nada más. `AddScoped`, `AddTransient` y los tres `TryAdd*` tienen
la misma sobrecarga.

**Es el tipo que se *pide*, no el cierre.** Para un registro sin fijar es el tipo cerrado que se
resuelve (`IRepo<TUser>`); para un registro corriente es el tipo del propio servicio (`IRepo`). El
dato significa lo mismo en los dos -- «esto es lo que se está resolviendo» -- así que una única
factoría registrada para varios contratos también puede distinguirlos.

**Las dos formas son tipos distintos, no uno con un parámetro opcional.** Un tipo procedural
exige una lista de parámetros idéntica, así que una lambda cuya aridad no coincide con el tipo
al que se asigna no compila -- `E2010 Incompatible types`--.

**Una factoría que no pregunta no es un caso especial.** La forma corta es la que usa casi todo
registro, y la mayoría no tienen nada que preguntar. Una que toma solo el proveedor se registra y
resuelve como siempre; registrada sin fijar, sirve el mismo objeto a todos los cierres.

---

## Dependencias

Solo `Daf.Commons`: `Daf.Rtti` por el marcador `_` y los helpers de RTTI, `Daf.Activator` por el
`TActivator.CreateInstance` que se nombra arriba, y `Daf.Enumerable` por el `IInterfaceList<T>` que
devuelve `GetServices`.

La implementación está en [`DependencyInjection`](../DependencyInjection/README.es.md).
