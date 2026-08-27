# DependencyInjection.Abstractions

**🌍 Idioma: [English](README.md) | Español**

Contratos principales (interfaces y tipos) del sistema de Inyección de Dependencias de DAF. Este módulo **no contiene implementación** — define la forma de la que dependen los consumidores y que `DependencyInjection` implementa.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![Licencia](https://img.shields.io/badge/licencia-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Qué hay en este módulo

| Unidad | Contenido |
|--------|-----------|
| `Daf.Extensions.DependencyInjection` | `IServiceCollection`, `IServiceProvider`, `IServiceScope`, tiempos de vida, `Factory`, `TDuckDecorator<T>`, `Lazy` |
| `Daf.AppModule` | `IAppModule` — patrón de registro por módulo |
| `Daf.DependencyInjection.ActivatorUtilities` | Helpers para construir objetos mediante RTTI con argumentos resueltos por DI |
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
    procedure AddServices(const Services: IServiceCollection);
  end;
```

### `Factory`

Helper para construir un `TServiceFactory` desde una clase o instancia:

```pascal
Services.Add(TypeInfo(IMyService), Singleton, Factory.From(TypeInfo(IMyService), TMyImpl));
```

### `TDuckDecorator<T>`

Clase base para envolver un servicio con lógica pre/post-llamada sin herencia. Sobreescribe `BeforeCall` / `AfterCall`.

### `Lazy`

```pascal
Result := Lazy.From<IHeavy>(Provider, function(P): IHeavy begin Result := THeavy.Create end);
```

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
  DAF, y ya no falla al registrar: ahora que declarar el campo es opcional, el contenedor no ve
  campo ninguno, no escribe nada, y `_ClosedArgs` sale a `nil` en el constructor. Pon un
  especificador de visibilidad entre los dos.

### Para qué sirve esto, y para qué no

Sirve para **reenviadores y contenedores agnósticos del tipo**. `ILogger<T>` es el caso canónico:
sostiene el registrador de verdad y su única relación con el argumento es usar su nombre como
categoría. No sirve para nada que necesite el tipo *como tipo* más allá de lo que `_ClosedArgs`
puede darle.

La prueba se hace con un dedo. **Tapa cada `T` del cuerpo** de la implementación: si el código
sigue en pie, es registrable sin fijar. Si algo necesitaba que `T` fuera `T` — un `T.Create`, un
`TypeInfo(T)`, una variable de tipo `T` con un valor de verdad dentro —, no lo es.

Y tres límites, dichos aquí en vez de dejados para que alguien tropiece con ellos:

- **Todos los argumentos o ninguno.** `IFoo<TConocido, _>`, fijando unos y dejando otros sin fijar,
  no está soportado.
- **El argumento tiene que viajar como referencia.** Un contrato donde `T` aparezca en una posición
  cuya representación dependa de `T` —devuelto por valor, un record, un entero— no se puede servir
  por esta vía. El contenedor **compara los dos contratos método a método y lo rechaza**, nombrando
  el método: entrar en ese emparejamiento corrompería la pila en vez de fallar. Lo rechaza sea lo
  que sea lo que declare la implementación, con `_ClosedArgs` o sin él — que un emparejamiento sea
  seguro es una propiedad de los dos contratos. Para saberlo de antemano, hazle la prueba del dedo
  al **contrato** en vez de al cuerpo de la implementación: lee las firmas, y si `T` aparece en
  algún sitio que no sea un objeto, una interfaz o una referencia de clase, el contrato no es
  registrable sin fijar.
- **El emparejamiento es por nombre y aridad, no por identidad.** Dos familias genéricas del mismo
  nombre simple y la misma aridad, declaradas en units distintas, emparejan. Lo que queda debajo no
  es la nada —esa misma comparación método a método rechaza el emparejamiento nombrando el primer
  método que no encaja— pero es una red, no una comprobación de identidad.

---

## Dependencias

Este módulo **no depende** de ningún otro módulo DAF. Solo requiere la RTL de Delphi (`System.Rtti`, `System.Generics.Collections`).

La implementación está en [`DependencyInjection`](../DependencyInjection/README.es.md).
