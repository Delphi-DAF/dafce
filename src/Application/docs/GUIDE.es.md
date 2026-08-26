# Application — Guía de uso

**🌍 Idioma: [English](GUIDE.md) | Español**

---

## Índice

1. [Cuál es tu punto de entrada](#cuál-es-tu-punto-de-entrada)
2. [Qué es esta capa, en términos de .NET](#qué-es-esta-capa-en-términos-de-net)
3. [Inicio rápido](#inicio-rápido)
4. [TDafApplicationBuilder](#tdafapplicationbuilder)
5. [IDafApplication](#idafapplication)
6. [TDafApplication.App](#tdafapplicationapp)
7. [Métodos de ciclo de vida](#métodos-de-ciclo-de-vida)
8. [Metadatos de versión](#metadatos-de-versión)
9. [Servicios en ejecución](#servicios-en-ejecución)
10. [Patrón de subclase](#patrón-de-subclase)
11. [Integración con Hosting](#integración-con-hosting)

---

## Cuál es tu punto de entrada

Hay **uno** por variante de aplicación, y elegir es cuestión de qué tipo de aplicación estás
escribiendo — no de cuál de dos capas usar:

| Tu aplicación | Punto de entrada | Vive en | `Build` devuelve |
|---|---|---|---|
| Consola / herramienta CLI | `TCliApplication.CreateBuilder(Args)` | `Daf.Cli` | `ICliApplication` |
| Web | `TWebApplication.CreateBuilder(Args)` | `Daf.Web` | `IWebApplication` |
| Escritorio, VCL | `TVclApplication.CreateBuilder` | `Daf.VCL` | `IVclApplication` |
| Escritorio, FireMonkey | `TFmxApplication.CreateBuilder` | `Daf.FMX` | `IFmxApplication` |

Las cuatro comparten `Services`, `Configuration`, `Environment`, `Logging` y `Properties`, escritos
igual. **Ese contrato común —`IHostApplicationBuilder`— es lo que las hace una familia y no cuatro
cosas parecidas**, y es lo que permite que un paquete aporte registros de arranque sin saber qué
variante lo va a consumir.

Se parten en dos por **quién es dueño del bucle**: en CLI y Web, DAF es dueño del proceso y `Run`
arranca, trabaja y para, así que el `.dpr` acaba cuando `Run` acaba. En VCL y FMX el message loop ya
existe y es de `Vcl.Forms.Application` / `FMX.Forms.Application`; la variante crea el formulario
principal y delega. Son dos contratos de ciclo de vida, no cuatro variaciones de uno.

### Qué se retiró, y dónde fue a parar

| Antes | Ahora |
|---|---|
| `THostBuilder.Create.ConfigureServices(...)` | `B.Services.…` en el builder de cualquier variante — el estilo diferido se retiró; queda uno, el inmediato |
| `IHostBuilder` | `IHostApplicationBuilder`, el contrato común de la familia. `THostBuilder` sobrevive entero como **detalle de implementación** |
| `TDafApplication.CreateHostBuilder` | el `CreateBuilder` de tu variante, de la tabla de arriba |
| `TDafApplication` / `TDafApplicationBuilder` | **deprecados**, siguen compilando en 2.0. Su relevo es la variante |
| `TDafApplication.VersionRequested` | `ICommandLineParseResult.VersionRequested` — la gramática contesta `--version`, y la comprobación escrita a mano se retira en vez de mudarse |
| `TDafApplication.App` | `DafApp`, **solo en VCL y FMX**. CLI y Web no tienen global a propósito |
| `Daf.Application.Version` | mudado a `Daf.Commons`, sin tocar — las cuatro variantes lo alcanzan sin el paquete deprecado |

### Qué añade la capa de aplicación sobre el host

El host te da el contenedor, la configuración, los servicios alojados y las tareas de arranque.
Encima de eso, las variantes añaden **el borde alrededor del `Run`** — que es la parte que cada
`.dpr` se escribía a mano:

| Qué | Dónde | Qué variantes |
|---|---|---|
| Decidir si el proceso llega a arrancar | `SingleInstance(Id)` | CLI |
| La política de lo que se escapa, y el código de salida | `OnUnhandled(Policy)` | CLI |
| El trabajo, y su código de salida | `OnExecute` / un `ICliHandler` registrado | CLI |
| `--version`, `--help`, invocación inválida | `RootCommand`, contestado por el parse | CLI |
| El servidor HTTP, cableado a DI y al host | `Web.AddHttpServer` + `MapControllers` | Web |
| Crear el formulario principal y entregar el bucle | `Run(TFormClass)` | VCL, FMX |
| Un global para el código que la plataforma construye sola | `DafApp` | VCL, FMX |

Dos miembros de la familia de .NET **no** se portan, y quedan declarados en vez de descubiertos:
`Metrics`, porque no hay pieza equivalente en DAF a la que apuntar, y `ConfigureContainer<T>`,
porque una interfaz de Delphi no admite métodos genéricos — no es que se descarte, es que no es
expresable.

## Qué es esta capa, en términos de .NET

Casi todos los módulos de DAF portan algo de `Microsoft.Extensions.*`, y este parece no portar nada
—no existe `Microsoft.Extensions.Application`—. El equivalente sí existe, solo que en otra parte
del mismo ecosistema: es la **familia de constructores de aplicación montada sobre el host
genérico**: `HostApplicationBuilder`, `WebApplicationBuilder`, `MauiAppBuilder`. Cada uno empareja
un builder con un objeto de aplicación construido (`WebApplication`, `MauiApp`) que **posee** el
host en vez de serlo. `TDafApplicationBuilder` / `IDafApplication` es ese par, con un solo miembro
por ahora.

Donde **diverge**, y esto importa más que el parecido:

- **Delphi no tiene métodos de extensión, y en .NET toda la superficie de composición lo es.**
  `AddJsonFile`, `AddHostedService`, `UseSerilog`, `ConfigureKestrel`: todos son extensiones que
  aporta el paquete dueño de la funcionalidad, y así es como .NET deja crecer un builder sin que el
  builder sepa de nada. Delphi tiene class helpers, pero solo **uno** está en ámbito por tipo y
  ninguno se aplica a interfaces, así que el truco no se traslada. DAF compone con fachadas
  explícitas —`JsonConfig.AddFile(Builder, …)` en vez de `Builder.AddJsonFile(…)`— y con un
  `IServiceCollection` que es un record con métodos de verdad.
- **Hay un tipo de aplicación, no una familia.** .NET elige su builder según la clase de programa
  que escribes; aquí la clase de programa es lo que pongas en `Execute`.
- **`RunAsync` es sincrónico.** Existe por familiaridad sintáctica —lo dice el propio código— y
  Delphi no tiene `async`/`await` con el que hacerlo otra cosa. Es `Run` con otro nombre.

---

## Inicio rápido

Esto es `samples/QuickStart/QuickStart.dpr` tal cual; `eng/check-quickstart-snippet.py` compara los
dos, así que este bloque es un fichero que lee el compilador:

<!-- quickstart: src/Application/samples/QuickStart/QuickStart.dpr -->

```pascal
program QuickStart;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  System.SysUtils,
  Daf.Extensions.DependencyInjection,
  Daf.CommandLine,
  Daf.Configuration.CommandLine,
  Daf.Cli,
  QuickStart.Greeter in 'QuickStart.Greeter.pas';

// Lo mas corto que se puede escribir con la familia: una variante, sus registros donde estan
// escritos, y el codigo de salida que devuelve el trabajo.
begin
  var B := TCliApplication.CreateBuilder(CommandLineConfig.ProcessArgs);

  B.Services.AddSingleton<IGreeter, TGreeter>;

  B.OnExecute(
    function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
    begin
      Writeln(Services.GetRequiredService<IGreeter>.Greet);
      Result := TCliExit.Ok;
    end);

  ExitCode := B.Build.Run;
end.
```

Dos formas que conviene nombrar, porque las dos se equivocan con facilidad:

- **El builder se sostiene en una variable de su propia clase.** `CreateHostBuilder` devuelve un
  `TDafApplicationBuilder`, pero todos los `Configure*` devuelven `IHostBuilder`, y
  `IHostBuilder.Build` devuelve un `IHost`. Encadénalo todo en cascada y obtienes el host, no la
  aplicación. `TDafApplicationBuilder.Build` es la sobrecarga que devuelve `IDafApplication`, y
  solo se llega a ella por la clase.
- **El servicio se declara en una unit, no en el `.dpr`.** Un tipo del fichero de programa no está
  en la sección `interface` de ninguna unit, y el contenedor lo rechaza en ejecución con
  `ENonPublicType`.

---

> **De aquí en adelante, esta guía documenta la vía deprecada.** `TDafApplication` y
> `TDafApplicationBuilder` siguen compilando en 2.0, así que lo que sigue sigue siendo cierto — pero
> no es la recomendación. Tu punto de entrada está en la tabla de arriba.

## TDafApplicationBuilder

`TDafApplicationBuilder` extiende `THostBuilder`, así que hereda toda su superficie fluida:
`ConfigureHostConfiguration`, `ConfigureAppConfiguration`, `ConfigureServices`, `UseProperty`,
`AddHostedService<T>`. Lo que añade es un `Build` que devuelve la aplicación:

```pascal
var Builder := TDafApplication.CreateHostBuilder;

Builder.ConfigureAppConfiguration(
  procedure(Context: IHostBuilderContext; Config: IConfigurationBuilder)
  begin
    JsonConfig.AddFile(Config, 'appsettings.json');
  end);

Builder.ConfigureServices(
  procedure(Context: IHostBuilderContext; Services: IServiceCollection)
  begin
    Services.AddHostedService<TWorker>;
    Services.AddTransient<IMyService, TMyService>;
  end);

Builder.Build.Run(True);
```

No hay `UseEnvironment` ni `UseShutdownTimeout`. El entorno, el nombre de la aplicación y la raíz de
contenido salen de variables de entorno, leídas una vez al construir el host:

| Variable | Fija | Por defecto |
|---|---|---|
| `DAF_APP_ENV` | `IHostEnvironment.EnvironmentName` | `Production` |
| `DAF_APP_NAME` | `IHostEnvironment.ApplicationName` | el nombre del propio ejecutable |
| `DAF_CONTENT_ROOT` | `IHostEnvironment.ContentRootPath` | el directorio del ejecutable |

Para usar una clase de builder propia, `CreateHostBuilder<T>` la toma como argumento de tipo:

```pascal
var Builder := TDafApplication.CreateHostBuilder<TMyBuilder>;
```

---

## IDafApplication

Esta es la declaración, no una paráfrasis:

```pascal
IDafApplication = interface(IInvokable)
  ['{FAE0176F-FED1-42DF-92DF-C2A219853AC4}']
  function GetVersionInfo: TVersionInfo;
  function GetEnvironment: IHostEnvironment;
  function GetHost: IHost;
  function GetServices: IServiceProvider;
  procedure Start;
  procedure Stop;
  procedure WaitForShutdown;
  procedure Run(const WaitForShutdown: Boolean = False); overload;
  procedure Run(const Exec: TProc<IServiceProvider>;
                const WaitForShutdown: Boolean = False); overload;
  procedure RunAsync(const Exec: TProc<IServiceProvider> = nil;
                     const WaitForShutdown: Boolean = False);
  function GetConfiguration: IConfiguration;
  property VersionInfo: TVersionInfo read GetVersionInfo;
  property Host: IHost read GetHost;
  property Environment: IHostEnvironment read GetEnvironment;
  property Configuration: IConfiguration read GetConfiguration;
  property Services: IServiceProvider read GetServices;
end;
```

Fíjate en `interface(IInvokable)`, no `interface(IHost)`: la primera sección dice por qué importa.

---

## TDafApplication.App

El constructor deja la instancia en una variable de clase, así que después de `Build` se llega a
ella desde cualquier sitio:

```pascal
var App := TDafApplication.App;              // la clase, no IDafApplication
var Svc := TDafApplication.App.Services.GetService<IMyService>;
```

Útil en units que no están cableadas por DI. Es una referencia de clase pelada y no se limpia al
liberar la aplicación, así que no la uses cuando la aplicación ya no está.

---

## Métodos de ciclo de vida

| Llamada | Qué ocurre |
|---|---|
| `Run` | arranca el host, ejecuta `Execute`, para. **No** espera señal de apagado |
| `Run(True)` | lo mismo, y antes de parar se bloquea esperando la señal de apagado (Ctrl+C) |
| `Run(Exec)` | arranca el host, llama a `Exec(Services)` en lugar de `Execute`, para |
| `Run(Exec, True)` | lo mismo, bloqueándose en la señal de apagado antes de parar |
| `RunAsync(Exec, Wait)` | lo que llaman las dos sobrecargas de `Run`. Sincrónico, pese al nombre |
| `Start` | arranca el host: tareas de arranque y después servicios alojados. No bloquea |
| `Stop` | para los servicios alojados, suelta el contenedor y se deshace del host |
| `WaitForShutdown` | bloquea el hilo llamante hasta Ctrl+C |

`WaitForShutdown` vale **`False`** por defecto en todas las sobrecargas. Si `Exec` o `Execute`
lanzan, la aplicación para y la excepción se relanza.

Después de `Stop` la aplicación ya no tiene host: `App.Host` es `nil` y no se puede rearrancar.

---

## Metadatos de versión

`VersionInfo` **no** se lee de un fichero. Sale de un atributo `[Version]` en la clase de la
aplicación, encontrado por RTTI — así que una aplicación sin ese atributo tiene un `TVersionInfo`
vacío, que es la razón habitual de preguntarse dónde se fue la versión:

```pascal
type
  [Version('1.2.3-rc.2', 'MyApp', 'My Company')]
  TMyApp = class(TDafApplication)
  end;
```

```pascal
TVersionInfo = record
  property Tag            : string;   // '1.2.3' o '1.2.3-rc.2', tal como se dio
  property ApplicationName: string;
  property Company        : string;
  property Major          : Integer;
  property Minor          : Integer;
  property Patch          : Integer;
  property PreRelease     : string;   // 'alpha.1', 'rc.2', … vacío si es estable
  property Metadata       : string;   // metadatos de build
  property ArchBits       : Integer;  // 32 o 64, de la compilación
  property Platform       : string;   // 'Win32', 'Win64', …
  property Debug          : Boolean;

  function VersionTag(const WithCompiledMeta: Boolean = False): string;
end;
```

`TDafApplication.VersionRequested` es una función de clase que responde si el primer argumento de
línea de órdenes es `--version`, para imprimirla y salir antes de construir nada.

---

## Servicios en ejecución

```pascal
var App := TDafApplication.App;

var Logger := App.Services.GetRequiredService<ILoggerFactory>.CreateLogger('Main');
var Port := App.Configuration['server:port'];

if App.Environment.IsDevelopment then
  Logger.LogDebug('modo desarrollo', []);
```

`IConfiguration` devuelve la cadena vacía tanto para una clave ausente como para una cuyo valor
está vacío; solo el `TryGet` de un proveedor distingue las dos.

---

## Patrón de subclase

`CreateHostBuilder` es un método de clase que usa la clase sobre la que se llamó, y eso es lo que
decide el tipo que `Build` instancia. Llámalo sobre `TDafApplication` y obtienes un
`TDafApplication`:

```pascal
type
  TMyApp = class(TDafApplication)
  protected
    procedure Execute; override;
  end;

procedure TMyApp.Execute;
begin
  // Services, Configuration, Environment y VersionInfo son propiedades de este propio objeto.
  // No hay un Logger heredado ni un AppName: resuelve lo que necesites.
  var Logger := Services.GetRequiredService<ILoggerFactory>.CreateLogger('MyApp');
  Logger.LogInformation('en ejecución {V}', [VersionInfo.Tag]);
  Services.GetRequiredService<IWorkerService>.DoWork;
end;

// TMyApp aquí, no TDafApplication.
var Builder := TMyApp.CreateHostBuilder;
Builder.ConfigureServices(
  procedure(Context: IHostBuilderContext; Services: IServiceCollection)
  begin
    Services.AddTransient<IWorkerService, TWorkerService>;
  end);
Builder.Build.Run;
```

`Execute` es lo que `Run` llama cuando no se le pasa un `Exec`; son alternativas, no una secuencia.

---

## Integración con Hosting

`TDafApplicationBuilder` extiende `THostBuilder`, así que todo lo que ofrece el módulo Hosting
funciona igual:

- servicios alojados → `Services.AddHostedService<TWorker>`
- inicialización de una sola vez → `Services.AddStartupTask<TSeeder>`, que corre antes de los
  servicios alojados
- apagado cooperativo → `IHostApplicationLifetime`
- ámbitos → `IServiceScopeFactory.CreateScope`

Los detalles, en la [guía de Hosting](../../Hosting/docs/GUIDE.es.md).
