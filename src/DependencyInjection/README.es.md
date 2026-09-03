# DependencyInjection

**🌍 Idioma: [English](README.md) | Español**

Contenedor IoC/DI para Delphi con una API fluida inspirada en `Microsoft.Extensions.DependencyInjection` de .NET. Registra servicios una vez, resuélvelos en cualquier parte — con tiempos de vida `Singleton`, `Scoped` y `Transient`.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![Licencia](https://img.shields.io/badge/licencia-MIT-blue.svg)](../../legal/LICENSE.md)

---

## ¿Por qué usarlo?

- 🔌 **API familiar** — Los mismos métodos `AddSingleton` / `AddTransient` / `AddScoped` que en .NET
- 🎯 **Genérico y type-safe** — Verificación en tiempo de compilación mediante genéricos, sin tokens de cadena
- ⚡ **Tres modos de vida** — Singleton, Scoped (por scope) y Transient (nueva instancia siempre)
- 🏭 **Registro flexible** — Por clase, por lambda de factoría o por instancia existente
- 💤 **Resolución diferida** — `Lazy.From<T>()` pospone la creación hasta el primer uso
- 🦆 **Duck Typing** — `TDuckDecorator<T>` envuelve cualquier objeto que implemente la misma forma de interfaz
- 📦 **Sin dependencias externas** — Solo depende de `DependencyInjection.Abstractions`

---

## Inicio rápido

```pascal
uses
  Daf.Extensions.DependencyInjection,
  Daf.DependencyInjection;

var
  Services: IServiceCollection;
  Provider: IServiceProvider;
begin
  Services := TServiceCollection.Create;

  // Registrar
  Services.AddSingleton<IMyService, TMyService>;

  // Construir el contenedor (congela la colección)
  Provider := Services.BuildServiceProvider;

  // Resolver
  var Svc := Provider.GetRequiredService<IMyService>;
  Svc.DoWork;

  // Siempre hacer Shutdown al finalizar
  Provider.ShutDown;
end;
```

---

## Sobrecargas de registro

```pascal
// Por clase de implementación
Services.AddSingleton<IMyService, TMyServiceImpl>;

// Por lambda de factoría (control total)
Services.AddTransient<IMyService>(
  function(P: IServiceProvider): IMyService
  begin
    Result := TMyServiceImpl.Create(P.GetRequiredService<IConfig>);
  end);

// Por instancia existente (solo Singleton)
Services.AddSingleton<IMyService>(MyExistingInstance);
```


---

## Servicios con clave

Dos implementaciones del mismo contrato, distinguidas por una clave. Los mismos nombres que
`Microsoft.Extensions.DependencyInjection`: `AddKeyedSingleton`, `GetRequiredKeyedService`,
`RemoveAllKeyed`.

```pascal
Services.AddKeyedSingleton<IStorage, TBlobStorage>(TValue.From<TStorageKind>(Blob));
Services.AddKeyedSingleton<IStorage, TFileStorage>(TValue.From<TStorageKind>(FileSystem));

var Blobs := Provider.GetRequiredKeyedService<IStorage>(TValue.From<TStorageKind>(Blob));
var Missing := Provider.GetKeyedService<IStorage>('nadie');   // nil
```

`AddKeyedSingleton`, `AddKeyedScoped` y `AddKeyedTransient` vienen en las mismas formas que sus
`Add*` —por clase, por factoría y por instancia— y cada uno tiene su `TryAddKeyed*`.

**La clave enumerada es la forma recomendada**, y la razón es el compilador: renombrar un miembro
propaga a todos sus usos y un typo es un error. `'bolb'` compila.

Una clave puede ser una **cadena**, un **entero**, un **booleano**, un valor de un **enumerado**, una
**metaclase** o un **`PTypeInfo`**. Dos claves son la misma cuando coinciden **su tipo y su valor**,
así que dos enumerados de tipos distintos que compartan ordinal son claves distintas, y la cadena
`'42'` no es el entero `42`. Cualquier otra cosa —un registro, un objeto— se rechaza donde entra,
nombrando el tipo: el contenedor no puede definir una igualdad que esos tipos no tienen.

⚠️ **Un carácter no es una clave.** Una clave de un carácter es una clave de cadena, y admitir las dos
haría que dos claves que en el fuente se leen idénticas no se encontrasen.

### Los dos espacios están aislados

Un registro hecho **con** clave no se alcanza **sin** ella, y al revés. Un contrato con clave es un
contrato distinto:

- `GetServices<T>` no incluye los registros con clave, y `CanResolve` sin clave responde solo por los
  que no la tienen.
- `Replace` y `RemoveAll` alcanzan únicamente los registros sin clave. `RemoveAllKeyed` toma la clave.
- `TryAdd*` pregunta por el **par**, no por el tipo: una clave no bloquea a otra, y ninguna bloquea al
  registro sin clave.
- `Decorate` envuelve los registros sin clave y deja intactos los que la tienen. Decorar un tipo que
  *solo* tiene registros con clave falla diciendo que no hay nada registrado para él.

⚠️ **BREAKING** — `RemoveAll` quitaba todo lo registrado para el tipo, porque no había claves. Ahora
quita únicamente los registros sin clave.

### Pedir una clave desde un constructor o una propiedad

La misma marca sirve a las dos posiciones, y la clave viaja dentro de ella:

```pascal
type
  [DafInjectProperties]
  TReportService = class(TInterfacedObject, IReportService)
  private
    FStorage: IStorage;
  public
    constructor Create([DafInject('audit')] const Audit: IAuditLog);
    [StorageInject(FileSystem)]
    property Storage: IStorage read FStorage write FStorage;
  end;
```

`[DafInject]` admite una cadena, un entero, un booleano, una metaclase o un `TypeInfo(...)`. Para una
clave **enumerada**, la aplicación declara su propia marca en una línea:

```pascal
type
  StorageInjectAttribute = class(DafInjectAttribute<TStorageKind>);
```

El contenedor la reconoce **por descendencia**, así que nunca necesita conocer ese enumerado.

⚠️ **Esa marca va en una unit propia**, separada de las clases que la usan. Declarada en la misma
unit, leer el atributo aborta con un access violation — medido, y es un defecto del compilador, no una
regla de gusto.

⚠️ **Y el genérico no se escribe nunca dentro de un `[ ]`.** `[DafInject<TStorageKind>(...)]`
*compila*, con un `W1074 Unknown custom attribute` que casi nadie lee, y en ejecución la propiedad no
tiene ningún atributo: la inyección no ocurre y nada lo dice.

Para una clave enumerada suelta, sin declarar marca, queda el escape
`[DafInject(TypeInfo(TStorageKind), Ord(FileSystem))]`. El ordinal se comprueba contra el rango del
tipo, y uno que no le pertenece falla nombrando la propiedad.

### Un registro para cualquier clave

Un registro bajo `ServiceKey.Any` atiende toda clave de su tipo de servicio que no tenga registro
propio, y su factoría recibe cuál se pidió:

```pascal
Services.AddKeyedSingleton<IStorage>(ServiceKey.Any,
  function(P: IServiceProvider; const Key: TValue): IStorage
  begin
    Result := TStorage.Create(Key.ToString);
  end);
```

Un registro concreto le gana —al comodín se le pregunta solo al fallar la búsqueda directa— y cada
clave que atiende recibe su propia instancia. El centinela es un valor de un tipo declarado dentro del
módulo de DI, así que nadie de fuera puede construirlo por accidente: una cadena reservada convertiría
en comodín a quien la registrase, sin enterarse.

⚠️ **Un registro con clave no puede dejar sus argumentos genéricos sin fijar.** Los dos
emparejamientos no se combinan, y el registro se rechaza en el acto en vez de no aparecer nunca.

---

## Inyección por propiedad

**El constructor es lo normal, y esto no lo sustituye.** Una dependencia que entra por el constructor
la garantiza el compilador: el objeto no puede existir sin ella. Una que entra por una propiedad, no,
y eso se paga en cada lectura. Hay dos casos que justifican el precio, y solo dos:

- **Dos servicios que se necesitan mutuamente.** Un ciclo por constructor es inconstruible: ninguno de
  los dos puede correr antes de que el otro termine. Por propiedad se resuelve.
- **Una instancia que el contenedor no construye.** Un formulario lo construye la plataforma, así que
  no hay constructor que el contenedor pueda elegir.

La clase declara que participa, y cada propiedad se declara a sí misma:

```pascal
type
  [DafInjectProperties]
  TReportService = class(TInterfacedObject, IReportService)
  private
    FStorage: IStorage;
  public
    [DafInject]
    property Storage: IStorage read FStorage write FStorage;
  end;
```

La propiedad ha de ser de tipo interfaz y escribible; da igual si la respalda un campo o un método, y
da igual que ese campo sea `strict private`. Una propiedad marcada que no se puede rellenar aborta
nombrando la propiedad, su tipo y la clase — una dependencia declarada que se queda en `nil` traslada
el fallo a la primera vez que alguien la use, en otro sitio.

**El constructor no las ve.** La inyección ocurre cuando el constructor ya ha terminado, así que una
propiedad marcada vale `nil` dentro de él. Una dependencia que haga falta **durante** la construcción
va por el constructor; las dos vías conviven en la misma clase.

### Cablear una instancia que el contenedor no construyó

```pascal
TActivatorUtilities.InjectInto(Provider, Self);   // en el constructor del formulario
```

Las mismas marcas, las mismas exigencias y los mismos errores que una resolución. El contenedor **no**
toma posesión de lo que cablea: no lo cuenta, cerrarlo no lo destruye, y los servicios que sostenga
viven mientras él los sostenga.

### Cablear por código, sin marcar nada

```pascal
Services.AddInjector<TReportService>(
  procedure(const Instance: TReportService; const Provider: IServiceProvider)
  begin
    Instance.Storage := Provider.GetRequiredService<IStorage>;
  end);
```

Para lo que no se puede anotar: una clase de otra librería, una unit con la RTTI de propiedades
recortada —esta vía no usa RTTI en absoluto, así que es la única inmune— y una clave de servicio
incómoda de escribir dentro de un atributo. Es política del tipo, así que alcanza también a sus
descendientes, y varios cableados corren en el orden en que se registraron. Corre después del
constructor y después de la inyección por atributo, y solo sobre lo que el contenedor construye.

⚠️ **Una RTTI de propiedades recortada esconde las marcas.** `{$RTTI EXPLICIT PROPERTIES([])}` deja
las propiedades de una unit fuera de la RTTI, y entonces una marcada no existe para el barrido. Una
clase que declara participar y no expone ninguna propiedad marcada falla diciéndolo y señalando las
directivas — `ValidateOnBuild` lo informa al arrancar, y la resolución lo informa si la validación
está apagada.

---

## Scopes

```pascal
var Scope := Provider.CreateScope;
try
  var Svc := Scope.ServiceProvider.GetRequiredService<IScopedService>;
  // Las instancias Scoped son únicas dentro de este scope
finally
  Scope := nil; // dispara Shutdown + limpieza gestionada
end;
```

---

## Hilos

Una vez construido el proveedor, **resolver desde varios hilos a la vez es seguro**. Cada registro se
construye **una sola vez y en un solo hilo**, y todos los hilos que lo pidan reciben esa misma
instancia — así que un constructor no tiene por qué ser seguro entre hilos.

Lo que esa garantía cubre es el contenedor, no lo que sale de él: un servicio resuelto es tan seguro
entre hilos como lo haya hecho quien lo escribió.

**Componer es de un solo hilo.** Se registra en un hilo, se construye el proveedor, y desde entonces
la colección solo se lee — registrar después de construir falla diciendo que está congelada. No hay
garantía frente a componer desde dos hilos, y no se piensa dar: es el montaje de una aplicación, y
ocurre una vez.

**Un ámbito es una frontera de vida, no un hilo.** Un ámbito se puede usar desde cualquier hilo, y
desde varios a la vez, lo haya creado quien sea. Un ámbito por unidad de trabajo sigue siendo la
recomendación, pero el motivo son **los servicios** y no el contenedor: un servicio con estado
mutable no se vuelve concurrente por vivir en un ámbito. Y un ámbito por hilo **no** aísla los
singletons — el ámbito de singletons es uno para todos, así que dos hilos con ámbitos propios lo
siguen compartiendo.

**Cerrar es seguro frente a resoluciones en curso.** Un `ShutDown` concurrente con una resolución
termina de una de dos formas: la resolución se completa y entrega su servicio, o falla con
`EContainerShutDownError`. Si ya había construido su instancia cuando perdió la carrera, esa
instancia se destruye en vez de entregarse.

⚠️ **Un constructor que resuelve en OTRO hilo y espera el resultado se interbloquea.** La
construcción de un registro está serializada, así que el hilo que espera tiene tomado lo que el otro
necesita. Ningún contenedor previene esto; pide la dependencia por el constructor.

---

## Patrón AppModule

Organiza los registros por módulo:

```pascal
type
  TMyModule = class(TAppModule)
  public
    procedure AddServices(const Services: IServiceCollection); override;
  end;

procedure TMyModule.AddServices(const Services: IServiceCollection);
begin
  Services.AddSingleton<IMyService, TMyService>;
  Services.AddTransient<IOther, TOtherImpl>;
end;
```

Se aplican por nombre, en el orden escrito — `AddModule<T>` instancia el módulo y llama a su
`AddServices` en el acto, así que nadie llama a `AddServices` a mano:

```pascal
Services
  .AddModule<TMyModule>
  .AddModule<TOtherModule>;
```

Consulta [DependencyInjection.Abstractions](../DependencyInjection.Abstractions/README.es.md) para la interfaz `IAppModule` y todos los contratos clave.

---

## Documentación

- 📖 [Guía de uso](docs/GUIDE.es.md) — tiempos de vida, scopes, Lazy, DuckDecorator, integración con Hosting
- 📖 [Composición del arranque](docs/STARTUP-COMPOSITION.es.md) — módulos y funciones `AddXxx`, `TryAdd`/`Replace`/`Decorate`, `IStartupTask`, validación del grafo
- 📁 [ModuleComposition](samples/ModuleComposition/README.md) — tres módulos aplicados en orden, y el que solo puede entrar por la sobrecarga de instancia
