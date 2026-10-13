# La familia de aplicaciones — Guía de uso

**🌍 Idioma: [English](APPLICATION-FAMILY.md) | Español**

Una aplicación DAF es una de cuatro variantes montadas sobre el [host genérico](GUIDE.es.md). Esta
guía cubre lo que comparten: cuál es la tuya, qué añade la capa sobre el host, a qué corresponde en
.NET y dónde fueron a parar los puntos de entrada que la precedieron.

---

## Índice

1. [Cuál es tu punto de entrada](#cuál-es-tu-punto-de-entrada)
2. [Qué se retiró, y dónde fue a parar](#qué-se-retiró-y-dónde-fue-a-parar)
3. [Qué añade la capa de aplicación sobre el host](#qué-añade-la-capa-de-aplicación-sobre-el-host)
4. [Qué es esta capa, en términos de .NET](#qué-es-esta-capa-en-términos-de-net)

---

## Cuál es tu punto de entrada

Hay **uno** por variante de aplicación, y elegir es cuestión de qué tipo de aplicación estás
escribiendo — no de cuál de dos capas usar. Una VCL entera, con sus propias specs, es
[TicTacToeApp](../../VCL/samples/TicTacToeApp/README.md):

| Tu aplicación | Punto de entrada | Vive en | `Build` devuelve |
|---|---|---|---|
| Consola / herramienta CLI | `TCliApplication.CreateBuilder(Args)` + `B.ParseArgs(Args)` | `Daf.Cli` | `ICliApplication` |
| Web | `TWebApplication.CreateBuilder(Args)` | `Daf.Web` | `IWebApplication` |
| Escritorio, VCL | `TVclApplication.CreateBuilder` | `Daf.VCL` | `IVclApplication` |
| Escritorio, FireMonkey | `TFmxApplication.CreateBuilder` | `Daf.FMX` | `IFmxApplication` |

Las cuatro comparten `Services`, `Configuration`, `Environment`, `Logging`, `Properties` y
`ServiceProviderOptions`, escritos
igual. **Ese contrato común —`IHostApplicationBuilder`— es lo que las hace una familia y no cuatro
cosas parecidas**, y es lo que permite que un paquete aporte registros de arranque sin saber qué
variante lo va a consumir.

Se parten en dos por **quién es dueño del bucle**: en CLI y Web, DAF es dueño del proceso y `Run`
arranca, trabaja y para, así que el `.dpr` acaba cuando `Run` acaba. En VCL y FMX el message loop ya
existe y es de `Vcl.Forms.Application` / `FMX.Forms.Application`; la variante crea el formulario
principal y delega. Son dos contratos de ciclo de vida, no cuatro variaciones de uno.

---

## Qué se retiró, y dónde fue a parar

| Antes | Ahora |
|---|---|
| `THostBuilder.Create.ConfigureServices(...)` | `B.Services.…` en el builder de cualquier variante — el estilo diferido se retiró; queda uno, el inmediato |
| `IHostBuilder` | `IHostApplicationBuilder`, el contrato común de la familia. `THostBuilder` sobrevive entero como **detalle de implementación** |
| `TDafApplication` / `TDafApplicationBuilder` | **retirados en 2.0.** Su relevo es la variante, de la tabla de arriba |
| `TDafApplication.CreateHostBuilder` | el `CreateBuilder` de tu variante |
| `IDafApplication` | la `I…Application` que devuelve el `Build` de tu variante |
| `TDafApplication.RunAsync` | `Run`. Era la misma llamada con otro nombre — Delphi no tiene `async`/`await` con el que hacerla otra cosa |
| `TDafApplication.VersionRequested` | `IParseResult.IsVersionRequested` — la gramática contesta `--version`, y la comprobación escrita a mano se retira en vez de mudarse |
| `TDafApplication.App` | `DafApp`, **solo en VCL y FMX**. CLI y Web no tienen global a propósito |
| `Daf.Application.Version` | `Daf.Commons`, sin tocar — las cuatro variantes alcanzan ahí `TVersionInfo` y `[Version]` |

---

## Qué añade la capa de aplicación sobre el host

El host te da el contenedor, la configuración, los servicios alojados y las tareas de arranque.
Encima de eso, las variantes añaden **el borde alrededor del `Run`** — que es la parte que cada
`.dpr` se escribía a mano:

| Qué | Dónde | Qué variantes |
|---|---|---|
| Decidir si el proceso llega a arrancar | `SingleInstance(Id)` | CLI, VCL, FMX |
| La política de lo que se escapa, y el código de salida | `OnException(Policy)` | CLI, Web |
| Pasarle a esa política lo que se escapa antes del `Run` | `HandleException(E)`, en el `except` del `.dpr` | CLI, Web |
| La acción del comando alcanzado, y su código de salida | `SetAction(Action)` / `SetAction(Command, Action)` | CLI |
| `--version`, `--help`, invocación inválida | `RootCommand`, contestado por el parse de lo que recibe `B.ParseArgs(Args)` | CLI |
| El servidor HTTP, cableado a DI y al host | `WebApp.AddHttpServer` + `MapControllers` | Web |
| Crear el formulario principal y entregar el bucle | `Run(TFormClass)` | VCL, FMX |
| Un global para el código que la plataforma construye sola | `DafApp` | VCL, FMX |

Dos miembros de la familia de .NET **no** se portan, y quedan declarados en vez de descubiertos:
`Metrics`, porque no hay pieza equivalente en DAF a la que apuntar, y `ConfigureContainer<T>`,
porque una interfaz de Delphi no admite métodos genéricos — no es que se descarte, es que no es
expresable. Las opciones del proveedor por defecto, que .NET fija a través de ese miembro, son la
`ServiceProviderOptions` del builder.

---

## Qué es esta capa, en términos de .NET

Casi todos los módulos de DAF portan algo de `Microsoft.Extensions.*`, y este parece no portar nada
—no existe `Microsoft.Extensions.Application`—. El equivalente sí existe, solo que en otra parte
del mismo ecosistema: es la **familia de constructores de aplicación montada sobre el host
genérico**: `HostApplicationBuilder`, `WebApplicationBuilder`, `MauiAppBuilder`. Cada uno empareja
un builder con un objeto de aplicación construido (`WebApplication`, `MauiApp`) que **posee** el
host en vez de serlo. El `CreateBuilder` de cada variante y la `I…Application` que construye son ese
par.

La familia tiene cuatro miembros donde .NET tiene tres, y el de más no es un concepto de más:
Delphi tiene dos stacks de interfaz, así que escritorio son VCL y FMX en vez de una sola entrada.

Donde **diverge**, y esto importa más que el parecido:

- **Delphi no tiene métodos de extensión, y en .NET toda la superficie de composición lo es.**
  `AddJsonFile`, `AddHostedService`, `UseSerilog`, `ConfigureKestrel`: todos son extensiones que
  aporta el paquete dueño de la funcionalidad, y así es como .NET deja crecer un builder sin que el
  builder sepa de nada. Delphi tiene class helpers, pero solo **uno** está en ámbito por tipo y
  ninguno se aplica a interfaces, así que el truco no se traslada. DAF compone con fachadas
  explícitas —`JsonConfig.AddFile(Builder, …)` en vez de `Builder.AddJsonFile(…)`— y con un
  `IServiceCollection` que es un record con métodos de verdad.
- **Las fuentes por defecto son las de .NET 10, con tres diferencias.** El builder carga
  `appsettings.json`, `appsettings.{Entorno}.json`, `{ApplicationName}.settings.json`,
  `{ApplicationName}.settings.{Entorno}.json`, todas las variables de entorno y la línea de comandos,
  como `Host.CreateApplicationBuilder`, y deriva el entorno y el content root de la misma manera —las
  variables del propio host llevan `DAF_` donde las de .NET llevan `DOTNET_`, así que
  `DAF_ENVIRONMENT` es `environment`—. Lo que difiere: no hay *user secrets*; los ficheros no se
  recargan cuando cambian; y una variable de entorno con un prefijo de cadena de conexión como
  `SQLCONNSTR_` no se convierte en una clave `ConnectionStrings:`.
- **Sin proveedor de log por defecto.** .NET escribe en la consola, en la salida de depuración y en el
  event source salvo que se le diga otra cosa; DAF no añade ninguno. Quien usa `Logging` añade un
  proveedor, o `AddNullProvider` para declarar que el silencio es a propósito: con `Logging` usado y
  sin proveedor, `Build` falla con `ELoggingCompositionError`.
- **`Properties` va por clave de cadena**, `TDictionary<string, TValue>`, donde .NET usa `object`.
- **El arranque es sincrónico de principio a fin.** El `RunAsync`, `StartAsync` y `StopAsync` de
  .NET no tienen contrapartida aquí; `Run`, `Start` y `Stop` vuelven cuando el trabajo está hecho.

---

## Ver también

- 📖 [Hosting — Guía de uso](GUIDE.es.md) — el host genérico sobre el que se montan las cuatro
  variantes
- 📖 [Composición del arranque](../../DependencyInjection/docs/STARTUP-COMPOSITION.es.md) — las dos
  fases y las reglas de orden
