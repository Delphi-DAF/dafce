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
