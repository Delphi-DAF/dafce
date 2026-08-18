# Composición del arranque

**🌍 Idioma: [English](STARTUP-COMPOSITION.md) | Español**

---

## Contenido

1. [Dos fases, no una](#dos-fases-no-una)
2. [Agrupar registros](#agrupar-registros)
3. [Gobernar un registro ajeno](#gobernar-un-registro-ajeno)
4. [Decorar](#decorar)
5. [Tareas de arranque](#tareas-de-arranque)
6. [Validar el grafo](#validar-el-grafo)
7. [.NET ↔ DAF](#net--daf)

---

## Dos fases, no una

Casi todas las dudas sobre el arranque vienen de confundir dos cosas que ocurren en momentos
distintos:

```
  ┌───────────────────────────────────────┐        ┌──────────────────────────────────────┐
  │ FASE 1 · COMPOSICIÓN                  │        │ FASE 2 · ARRANQUE                    │
  │ todavía NO existe el IServiceProvider │        │ el IServiceProvider YA existe        │
  ├───────────────────────────────────────┤        ├──────────────────────────────────────┤
  │ ¿QUÉ se registra, y en qué orden?     │        │ ¿QUÉ se ejecuta, y en qué orden?     │
  │                                       │        │                                      │
  │   AddModule<T>                        │        │   IStartupTask.Run                   │
  │   funciones AddXxx                    │        │   IHostedService.Start               │
  │   TryAdd / Replace / Decorate         │        │   ApplicationStarted                 │
  └───────────────────────────────────────┘        └──────────────────────────────────────┘
                          └──── BuildServiceProvider ────┘
```

`IAppModule` es de la fase 1 y tiene **una sola** fase: `AddServices`, y nada más. Lo que
necesite el proveedor ya construido es asunto de la fase 2, y su contrato es `IStartupTask`
— no una segunda fase del módulo. Un objeto de composición muere al acabar la composición, y
mantenerlo vivo hasta el arranque sin que sea un servicio es el camino que ASP.NET Core
recorrió con la clase `Startup` antes de volverse atrás.

Las dos fases no compiten: un paquete suele necesitar las dos — declara sus registros en la
fase 1 y deja encargada una tarea de fase 2, de modo que el ejecutable escribe una línea.
Está en `src/Hosting/samples/PackageInit`.

## Agrupar registros

Dos formas, y el criterio cabe en una frase: **si el grupo necesita un tipo —para que la
aplicación lo herede y sobrescriba un registro, para tener variantes por subtipo, o para
llevar configuración— es un módulo; si es un puñado de registros sin más, basta una
función.**

### Un módulo

```pascal
type
  TEntornoModule = class(TAppModule)
  public
    procedure AddServices(const ServiceCollection: IServiceCollection); override;
  end;
```

Se aplica por su nombre, en el orden escrito, encadenando con los `Add*` que ya había:

```pascal
Services
  .AddSingleton<IClock, TClock>
  .AddModule<TDomainDaosModule>
  .AddModule<TEntornoModule>
  .AddModule<TEvaluationModule>;
```

`AddModule<T>` instancia el módulo y llama a su `AddServices` **en el acto**, no de forma
diferida: la composición es una sola secuencia, y lo que se escribe después de un módulo se
ejecuta después que él. Es lo que hace que un `TryAdd` posterior no pise lo que el módulo
registró.

Un módulo que lleva configuración no puede recibirla por inyección de dependencias —corre
antes de que exista el proveedor—, así que se la da quien llama, con la sobrecarga de
instancia:

```pascal
var Module: IAppModule := TEntornoModule.Create(NombreEntorno);
Services.AddModule(Module);
```

La sobrecarga genérica lleva restricción `constructor`, así que **se ejecuta el constructor
propio del módulo** y no interviene la RTTI.

`AddModule<T>` es un método del propio `IServiceCollection`, no un `record helper`: está
disponible siempre, sin depender del orden del `uses`, y cualquier paquete puede aportar su
módulo sin colisionar con otro.

**Linaje.** El módulo no es invención de DAF: es `Autofac.Module`, es `IWindsorInstaller` de
Castle Windsor, es `Registry` de StructureMap — agrupar registros en una clase es una
ampliación clásica de los contenedores completos. Lo que `Microsoft.Extensions` no tiene es
el concepto, porque allí esa función la cubren los métodos de extensión, que Delphi no tiene;
por eso aquí el módulo cuelga del propio `IServiceCollection`.

### Una función

```pascal
function AddEntorno(const Services: IServiceCollection): IServiceCollection;
```

Es el método de extensión del host genérico, y allí es la forma canónica. En Delphi hay una
pega que conviene decir sin adornos: **no encadena.** `Services.AddEntorno` es sintaxis de
método, y una función libre se llama `AddEntorno(Services)`. Un arranque escrito con
funciones se lee en secuencia, no en cadena:

```pascal
Services.AddSingleton<IClock, TClock>;
AddDomainDaos(Services);
AddEntorno(Services);
AddEvaluation(Services);
```

Sigue estando en un sitio y en orden, que es lo que importa. Quien llegue de C# esperando
`services.AddEntorno()` no lo va a tener.

### Lo que comparten

Las dos hacen desaparecer el `uses` con comentario `// force-link:`. Llamar a la función
enlaza su unit; nombrar el tipo en `AddModule<T>` enlaza la suya.

## Gobernar un registro ajeno

`Add*` siempre añade. Para escribir un `AddXxx` reutilizable hace falta la semántica que .NET
da por sentada: **la librería pone el default, la aplicación manda.**

| DAF | .NET |
|---|---|
| `TryAddSingleton` / `TryAddScoped` / `TryAddTransient` | `ServiceCollectionDescriptorExtensions.TryAdd*` |
| `Replace(LifeTime, ServiceType, …)` | `ServiceCollectionDescriptorExtensions.Replace` |
| `RemoveAll(ServiceType)` | `ServiceCollectionDescriptorExtensions.RemoveAll` |
| `Descriptors(ServiceType)` | indexar `IServiceCollection`, que **es** una `IList<ServiceDescriptor>` |

`TryAdd*` registra **solo si no existe ya** un registro para ese tipo de servicio, y nunca
sustituye al que hay:

```pascal
// En el paquete: este es su default
Services.TryAddSingleton<IClock, TSystemClock>;

// En el arranque, antes o después: este es el que manda
Services.AddSingleton<IClock, TFrozenClock>;
```

`Replace` quita los registros previos del tipo y añade el nuevo; `RemoveAll` los quita sin
añadir nada — que es lo que necesita un arnés de specs para desmontar un registro de
producción.

**Una diferencia deliberada con .NET.** Allí la colección *es* una lista mutable, y `TryAdd`,
`Replace` y Scrutor entero están construidos encima. En DAF los descriptores se exponen en
**sólo lectura**, como copia: la lista viva es propiedad del diccionario interno, y repartir
esa referencia dejaría mutar la colección sin pasar por sus guardias. Se conserva la
capacidad —leer para componer— y no se hereda la mutabilidad cruda, que allí es inocua porque
no hay una lista con dueño de por medio.

Ese acceso de lectura es lo que hace escribible todo lo demás, dentro de DAF y fuera. Un
tercero puede escribir su propio Scrutor sin tocar el framework:

```pascal
function AddShouting(const Services: IServiceCollection): IServiceCollection;
begin
  Result := Services;
  var Current := Services.Descriptors<IFoo>;
  if Length(Current) = 0 then Exit;

  var Inner := Current[High(Current)].Factory;
  Services.Replace(Current[High(Current)].LifeTime, TypeInfo(IFoo),
    function(Provider: IServiceProvider): IInterface
    begin
      var Original: IFoo;
      Supports(Inner(Provider), IFoo, Original);
      Result := TShoutingFoo.Create(Original) as IFoo;
    end);
end;
```

**Todo esto se acaba cuando se acaba la colección.** `BuildServiceProvider` la congela, y a
partir de ahí `TryAdd*`, `Replace`, `RemoveAll` y `Decorate` lanzan, igual que `Add`. Un
`TryAdd` que no añade sigue siendo una modificación intentada: sobre una colección congelada
es un error de quien llama, no un no-op silencioso.

## Decorar

```pascal
Services.Decorate<IRepository>(
  function(const Inner: IRepository; const Provider: IServiceProvider): IRepository
  begin
    Result := TLoggingRepository.Create(Inner, Provider.GetRequiredService<ILogger>);
  end);
```

Cada descriptor del servicio se sustituye por otro que construye el original con la factory
antigua y se lo pasa al decorador. **El ciclo de vida se conserva**, y dos `Decorate` anidan
en orden de registro: el segundo envuelve al primero, que envuelve al original.

Esto no está en el framework de .NET: es **Scrutor** (`services.Decorate<IFoo, Bar>()`), y
los contenedores completos lo traen (el `RegisterDecorator` de Autofac).

**Alcance.** La decoración es para envolver comportamiento — logging, caché, reintentos,
métricas. **No** es el sitio donde colgar efectos laterales de arranque: para eso está
`IStartupTask`. Un decorador que devuelve `Inner` sin envolver nada es un abuso del
mecanismo.

## Tareas de arranque

```pascal
IStartupTask = interface(IInvokable)
  procedure Run;   // una vez, al arrancar, con el proveedor construido. No hay Stop.
end;

Services.AddStartupTask<TSeedEngineTask>;
```

`THost.Start` ejecuta las tareas de arranque **en orden de registro**, **antes** que los
`IHostedService` y antes de notificar `ApplicationStarted`. El orden importa: una tarea de
arranque prepara estado del que un servicio alojado puede depender. Si una lanza, el arranque
falla y la excepción se propaga: no se arranca ningún servicio alojado y la aplicación nunca
se declara arrancada. Una inicialización que no se puede completar no debe dejar la app medio
arrancada.

**Es una divergencia deliberada de `Microsoft.Extensions.*`**, que no tiene el contrato. La
consecuencia conocida allí es que se usa `IHostedService` como inicializador, con un
`StopAsync` vacío que miente sobre lo que el objeto es. DAF clona .NET para heredar sus buenas
decisiones, no sus huecos.

**La regla, en una frase: si tiene algo que parar es `IHostedService`; si solo se ejecuta una
vez, es `IStartupTask`.**

`IHostApplicationLifetime.ApplicationStarted` no cubre este caso: corre *después* de los
servicios alojados y de que la aplicación se declare arrancada, así que no sirve para
preparar estado del que esos servicios dependan.

## Validar el grafo

```pascal
var Provider := Services.BuildServiceProvider(TServiceProviderOptions.Create(True, True));
//                                                                          │     └ ValidateScopes
//                                                                          └────── ValidateOnBuild
```

- `ValidateOnBuild` comprueba, por cada descriptor registrado **por clase**, que el
  constructor elegido se puede satisfacer. Reporta **todos** los fallos juntos, no el primero:
  un arranque mal cableado suele tener varios, y descubrirlos de uno en uno cuesta un ciclo de
  compilación por cada uno.
- `ValidateScopes` rechaza que un Singleton dependa de un Scoped (*captured dependency*),
  nombrando los dos servicios.

Ambos son calcados de `ServiceProviderOptions`, y ambos son **opt-in**. En Delphi esto vale
más que en .NET: sin stack traces cómodos, la diferencia está entre un mensaje al arrancar y
un Access Violation a mitad de una operación larga.

**El límite: solo alcanza a los descriptores registrados por clase**, donde el constructor es
inspeccionable por RTTI. Los registros por **factory** son opacos —validar lo que hace una
lambda exigiría ejecutarla, y ejecutarla en `Build` tendría efectos—. Así que un arranque que
valida sin quejarse no dice que el grafo entero esté bien: dice que lo inspeccionable lo está.

## .NET ↔ DAF

| .NET | DAF | Nota |
|---|---|---|
| `IServiceCollection` como `IList<ServiceDescriptor>` | `Descriptors(ServiceType)` | Copia de sólo lectura, a propósito |
| `TryAdd*` / `Replace` / `RemoveAll` | los mismos nombres | Misma semántica |
| `Decorate` de Scrutor | `Decorate<S>` | Tampoco está en el framework de .NET |
| `ServiceProviderOptions` | `TServiceProviderOptions` | Opt-in; solo alcanza a los registros por clase |
| métodos de extensión `services.AddXxx()` | `AddXxx(Services)` | Delphi no los tiene: **no encadena** |
| módulos de Autofac / Windsor / StructureMap | `AddModule<T>` | Divergencia de capa: en la colección, no en el contenedor |
| — | `IStartupTask` | Divergencia deliberada: .NET no tiene contrato para esto |

---

**Ver también:** [DependencyInjection — Guía de uso](GUIDE.es.md) ·
`src/Hosting/samples/PackageInit`
