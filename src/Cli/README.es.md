# Daf.Cli

**🌍 Idioma: [English](README.md) | Español**

El **miembro CLI** de la familia de aplicaciones de DAF. Porta la forma de
[`Host.CreateApplicationBuilder`](https://learn.microsoft.com/dotnet/core/extensions/generic-host)
de .NET y añade lo que una herramienta de consola necesita de verdad y allí queda repartido con
`System.CommandLine`: una gramática, y el **borde alrededor del `Run`**.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Todo, de una vez

<!-- quickstart: src/Cli/samples/GreetTool/GreetTool.dpr -->

```pascal
program GreetTool;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  System.SysUtils,
  Daf.Extensions.DependencyInjection,
  Daf.CommandLine,
  Daf.Configuration.CommandLine,
  Daf.Cli;

// Una herramienta CLI de la familia, con gramatica de verdad. Lo que enseña es el BORDE del Run:
// --version y --help los contesta el parse, una invocacion que no encaja sale con su propio
// codigo, y lo que se escape --de la accion o de la composicion-- lo decide la politica declarada.
// El .dpr solo escribe el except que se la pasa.
//
//   GreetTool --name Jorge      -> 0, saluda
//   GreetTool --name Jorge --shout -> 0, saluda gritando
//   GreetTool --version         -> 0, la version
//   GreetTool --help            -> 0, la ayuda
//   GreetTool                   -> 2, falta un argumento obligatorio
//   GreetTool --name boom       -> 7, la politica traduce la excepcion
begin
  var B := TCliApplication.CreateBuilder(CommandLineConfig.ProcessArgs);

  // La politica es un callback, asi que una suite puede compartir la suya sin heredar nada mas. Va
  // lo primero porque decide tambien lo que se escape de la composicion.
  B.OnException(
    function(const Failure: TAppFailure): Integer
    begin
      Writeln(ErrOutput, 'greet: ', Failure.Error.Message);
      Result := 7;
    end);

  try
    var NameOpt := CommandLine.Option<string>('--name').Description('A quien saludar').Required;
    var ShoutOpt := CommandLine.Option<Boolean>('--shout', ['-s']).Description('En mayusculas');
    B.RootCommand.WithVersion('2.0.0').Add([NameOpt, ShoutOpt]);

    // Solo una instancia: la segunda no llega a construir el contenedor.
    B.SingleInstance('daf-greet-tool');

    B.SetAction(
      function(const Services: IServiceProvider; const Parsed: IParseResult): Integer
      begin
        var Who := NameOpt.ValueIn(Parsed);
        if Who = 'boom' then
          raise Exception.Create('ese nombre no me gusta');

        var Greeting := 'Hola, ' + Who;
        if ShoutOpt.ValueIn(Parsed) then
          Greeting := Greeting.ToUpper;

        Writeln(Greeting);
        Result := TCliExit.Ok;
      end);

    B.ParseArgs(CommandLineConfig.ProcessArgs);
    ExitCode := B.Build.Run;
  except
    // Lo que se escapa antes del Run --de la composicion o del Build-- va a la misma politica.
    on E: Exception do
      ExitCode := B.HandleException(E);
  end;
end.
```

`--version`, `--help`, una opción mal escrita y lo que se escape de la acción ya están resueltos: los
tres primeros los contesta la **gramática**, el último lo toma la política declarada, y `Run`
traduce cada desenlace a un código de salida. Ninguna de esas cuatro líneas se escribe en el
`.dpr`.

---

## Los samples

| Sample | Qué usa |
|---|---|
| [`QuickStart`](samples/QuickStart/) | el builder, la acción de la raíz y el código de salida, y nada más |
| [`ServiceWiring`](samples/ServiceWiring/) | lo mismo, más un servicio registrado y resuelto |
| [`GreetTool`](samples/GreetTool/) | la gramática, `--help`, códigos de salida por política — el bloque de arriba |
| [`ConsoleSample`](samples/ConsoleSample/) | el estilo inmediato: el entorno compone la ruta del `appsettings.json` antes de construir nada, y `IOptions<T>` llega a un servicio por constructor |
| [`PackageInit`](samples/PackageInit/) | un paquete que registra su propia `IStartupTask`, y el arranque no nombra ni la tarea ni lo que hace |

---

## El estilo inmediato

El builder es utilizable **en cuanto se crea**: `Configuration` y `Environment` responden en cuanto se
leen, así que se puede elegir una fuente a partir de lo ya configurado sin ningún callback diferido.
Se cargan en esa primera lectura, o en el `Build` si la composición no los lee: crear el builder no
abre ningún fichero.

```pascal
var B := TCliApplication.CreateBuilder(Args);

// El entorno ya responde -- este es el caso por el que existía el estilo por callback.
if B.Environment.IsDevelopment then
  JsonConfig.AddFile(B.Configuration,
    TPath.Combine(B.Environment.ContentRootPath, 'devtools.json'), [csoOptional]);

if B.Configuration.Find('Features:Beta').GetValueOrDefault('') = 'on' then
  B.Services.AddModule<TBetaModule>;
```

DAF tiene **un solo** estilo de builder, y es este. El diferido —registrar por callbacks de
`ConfigureServices`— se retiró: .NET lo conserva solo por compatibilidad hacia atrás, y dos formas de
expresar lo mismo son el defecto, no la comodidad.

---

## El borde alrededor del `Run`

Esto es lo que la capa añade sobre el host, y lo que cada herramienta de consola se reescribía a mano:

| Miembro | Qué decide | Cuándo |
|---|---|---|
| `SingleInstance(Id)` | Si el proceso llega a arrancar. Sin declararla no hay cerrojo, y pueden correr varias instancias a la vez | Tras el parse, **antes** de cargar la configuración y de construir el contenedor: `--help` y `--version` contestan sin él |
| `OnException(Policy)` | Qué pasa con lo que se escapa, y con qué código de salida. Sin ella, `ExitPolicy.LogAndFail` | Alrededor del `Run`, y en el `except` del `.dpr` |
| `HandleException(E)` | Pasa a la misma política lo que se escapa antes del `Run` —la composición, el `Build`— y devuelve el código de salida | En el `except` del `.dpr` |
| `SetAction(Action)` | Lo que hace la raíz | Dentro del `Run` |
| `SetAction(Command, Action)` | Lo que hace un comando cuando la invocación lo alcanza | Dentro del `Run` |
| `SetStandaloneAction(Command, Action)` | Lo que hace un comando sin host: sin cerrojo, sin tareas de arranque, sin servicios alojados, sin configuración y sin contenedor | Dentro del `Run` |
| `RootCommand` | La gramática | Se congela en el primer parse |
| `ParseArgs(Args)` | Lo que parsea la gramática. Sin llamarlo, el `Build` parsea una invocación vacía | En la composición, antes del `Build` |

`RootCommand` es una raíz de `Daf.CommandLine`: un banner encima de `--help` y de `--version` va en
su `HelpAction` y su `VersionAction`, como enseña el [README de Daf.CommandLine](../CommandLine/README.es.md).
Corren sin host: lo que se escapa de ellas va a la política de `OnException`, con
`Failure.Services` a `nil`.

### El parse va primero

`Build` parsea —una invocación vacía, salvo que ya lo hiciera `ParseArgs`— antes de tomar el cerrojo
de instancia única, de cargar la configuración y de construir el contenedor. `--version`, `--help` y una invocación que no encaja con la gramática se contestan
desde el parse, así que ni otra instancia en marcha ni un fichero de configuración del host que no se
puede leer les impiden contestar.

Lo que la composición ejecuta antes del `Build` es suyo, y corre antes del parse salvo que llame
primero a `ParseArgs`:

- leer `Configuration` o `Environment` carga la configuración en esa lectura;
- una fuente que añade la propia aplicación se abre al añadirla, y falla ahí.

Un fichero por defecto del host que no se puede leer —un `appsettings.json` mal formado— se informa
por el canal de arranque donde se abre, y hace fallar el `Build` cuando la invocación llega a una
acción.

### Cada comando, su acción

`Run` arranca el host, ejecuta la acción del comando que alcanzó la invocación y para el host; el
proceso termina con el código que devuelve la acción. La acción recibe el `IServiceProvider` por
parámetro, junto al resultado del parse:

```pascal
var Greet := CommandLine.Command('greet', 'Say hello');
var Farewell := CommandLine.Command('farewell', 'Say goodbye');
B.RootCommand.Add([Greet, Farewell]);

B.SetAction(Greet,
  function(const Services: IServiceProvider; const Parsed: IParseResult): Integer
  begin
    Writeln(Services.GetRequiredService<IGreeter>.Greet);
    Result := TCliExit.Ok;
  end);
```

- `SetAction(Action)` declara la acción de la raíz; `SetAction(Command, Action)`, la de un comando.
  La acción se instala en el comando en el acto, así que un comando con subcomandos y acción es una
  invocación por sí mismo.
- Declarar en el builder la acción de un comando que ya tiene una lanza `ECliCompositionError` en esa
  llamada. Un `Command.SetAction` escrito después la sustituye, como sustituye cualquier acción.
- Una aplicación que no declara ninguna acción es sus tareas de arranque y sus servicios alojados, y
  `Run` devuelve `TCliExit.Ok`.
- Lo que se escapa del arranque del host, de una acción o de la parada va a la política de
  `OnException`. Un fallo del arranque llega con `Failure.Reported` a `True`, porque el host ya lo
  informó; la acción no corre, y el host se para igual.
- Un fallo al parar el host llega con `Failure.Services` a `nil` y `Failure.Reported` a `True`: el
  contenedor ya se apagó, y el host ya informó del fallo. Si el arranque o la acción ya habían
  fallado, el código de salida es el que la política dio a ese primer fallo, y el de la parada solo
  se informa.
- Lo que se escapa antes del `Run` —la composición, o el `Build`— llega a la misma política cuando el
  `except` del `.dpr` se lo pasa a `HandleException`, como hace el sample de arriba. Llega con
  `Failure.Services` a `nil`, y con `Failure.Reported` a `True` si el host lo informó al
  construirse. Lo que la política ya dejó salir del `Run` se relanza sin pasar por ella una segunda
  vez. Antes de declarar `OnException`, decide la política por defecto.
- Un `EAbort` —lo que lanza `Abort`— no llega a la política: el proceso termina con
  `TCliExit.Failure` y no se informa nada.
- Mientras corre el `Run`, el gancho `ApplicationHandleException` del RTL apunta a la política, como
  en una aplicación VCL: lo que lanza un procedimiento encolado cuando lo procesa `CheckSynchronize`,
  y lo que lanzan el `OnCreate` o el `OnDestroy` de un módulo de datos, le llegan con el contenedor.
  Como en VCL, un `OnCreate` que lanza no impide que el módulo se cree.
- El código de salida es el que la política dio al primer fallo, aunque la acción termine bien
  después.

⚠️ Usa el parámetro `Services`. Una acción que captura la aplicación para leer `App.Services` forma
con ella un ciclo de referencias —la aplicación sostiene la gramática, la gramática la acción, la
acción la aplicación— y la aplicación no se libera nunca.

### Un comando sin host

`SetStandaloneAction(Command, Action)` declara la acción de un comando que corre sin host. Cuando la
invocación lo alcanza no hay cerrojo de instancia única, ni tareas de arranque, ni servicios alojados,
ni configuración, ni contenedor: la acción recibe solo el resultado del parse, como un
`TCommandAction` de `Daf.CommandLine`.

```pascal
var Init := CommandLine.Command('init', 'Escribe la configuración de partida');
B.RootCommand.Add(Init);
B.SetStandaloneAction(Init,
  function(const Parsed: IParseResult): Integer
  begin
    TFile.WriteAllText('appsettings.json', '{}');
    Result := TCliExit.Ok;
  end);
```

Es para lo que se contesta con los argumentos solos, como un `init` que escribe la configuración que
una ejecución normal exige. Como `SetAction`, lanza `ECliCompositionError` si el comando ya tiene
acción. Un `Command.SetAction` escrito después sustituye la acción, y el comando sigue sin host. Lo
que se escapa de ella va a la política de `OnException`, con `Failure.Services` a `nil`.

### Las políticas que vienen de serie

`OnException` acepta cualquier callback, y la política es la de la familia de aplicaciones: la
variante Web acepta la misma. Recibe un `TAppFailure` —la excepción en `Error`, el
contenedor en `Services`, `nil` si no lo hay, y `Reported` si el host ya la informó— y devuelve el
código de salida. Tres están ya escritas:

| | Qué hace |
|---|---|
| `ExitPolicy.LogAndFail` | informa por el canal de arranque y devuelve `TCliExit.Failure`; lo que el host ya informó (`Failure.Reported`) no lo vuelve a informar. **Es la que se aplica si no se llama a `OnException`** |
| `ExitPolicy.Rethrow` | deja salir la excepción del `Run`, para quien lo llamó. El host se para igual — la parada está en el `finally` |
| `ExitPolicy.Silent` | devuelve `TCliExit.Failure` y no dice nada |

`ExitPolicy` es una `class` y sus tres miembros son `virtual`, así que una suite de aplicaciones
puede derivar de ella para agrupar las suyas.

### Códigos de salida

| Código | Significado |
|---|---|
| `TCliExit.Ok` (0) | La acción terminó bien, no había ninguna, o contestaron `--version` / `--help` |
| `TCliExit.Failure` (1) | Lo que devuelve `ExitPolicy.LogAndFail` ante una excepción no capturada, y con lo que termina un `Abort` |
| `TCliExit.InvocationError` (2) | La invocación no encaja con la gramática declarada |
| `TCliExit.AlreadyRunning` (3) | Ya hay otra instancia con ese identificador |

`Run` devuelve un `Integer`, y no es adorno: una aplicación CLI que no puede decirle al sistema cómo
terminó no sirve en un script.

### El canal de arranque

Lo que falla antes de que exista un proveedor de log sale por el
[canal de arranque](../Logging.Abstractions/README.es.md#canal-de-arranque): un fichero de
configuración que no se puede leer, un `Build` del host que falla, y lo que informa
`ExitPolicy.LogAndFail`. De serie escribe en stderr y en la salida de depuración del sistema.

Un destino en fichero lo añade la aplicación, en las primeras líneas del `.dpr` y antes de
`CreateBuilder`:

```pascal
var Data := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), 'data');
TDirectory.CreateDirectory(Data);
BootstrapLog.AddSink(TFileBootstrapSink.Create(TPath.Combine(Data, 'startup.log')));

var B := TCliApplication.CreateBuilder(CommandLineConfig.ProcessArgs);
```

La ruta no puede salir de `Configuration` ni de `Environment`: leer cualquiera de los dos carga la
configuración, y un fichero que no se puede leer se informa en esa lectura, por los destinos que haya
entonces. El sample [`StartupReport`](../Hosting/samples/StartupReport/README.md) de Hosting hace lo
mismo con el host genérico.

Un error de la gramática no pasa por el canal: la gramática escribe el error en stderr, y las
sugerencias y la ayuda en stdout, y `Run` devuelve `TCliExit.InvocationError`. Para que quede también
en el fichero, la composición lo informa entre `ParseArgs` y `Build`:

```pascal
var Parsed := B.ParseArgs(CommandLineConfig.ProcessArgs);
if not Parsed.Succeeded then
begin
  for var Sink in BootstrapLog.Sinks do
    if (Sink as TObject) is TStdErrBootstrapSink then
      BootstrapLog.RemoveSink(Sink);
  var Log := BootstrapLog.LoggerFor('mytool');
  for var Error in Parsed.Errors do
    Log.LogWarning('{Error}', [TValue.From<string>(Error.Message)]);
end;
ExitCode := B.Build.Run;
```

El bucle sobre `Sinks` quita el destino de stderr del canal; con él, el error sale dos veces en la
consola. Sobre `mytool --verbsoe`, el fichero recibe:

```
<fecha y hora> #<proceso>:<hilo> [WARN] mytool | Unrecognized command or argument '--verbsoe'.
```

### Cuando no hay contenedor

`Services` es el contenedor, y responde **entre el `Build` y el `Run`**. Vale `nil` en dos casos:

- La invocación se resolvió sin construirlo: `--version`, `--help`, una invocación que no encaja con
  la gramática, un comando standalone, u otra instancia que ya sostiene el identificador de instancia
  única.
- Después de que `Run` haya vuelto, siempre — `Stop` libera el host.

Así que lo que un programa necesite del contenedor una vez corrida la acción lo sostiene desde
dentro de la acción, que recibe el `IServiceProvider`.

Ese `IServiceProvider` es el raíz: alrededor de la acción no se abre ningún ámbito. Un servicio
Scoped que la acción necesite sale de un ámbito que abre la propia acción —`Services.CreateScope`—;
con la validación activa, como en `Development`, pedírselo al proveedor raíz se rechaza.

### El resultado del parse, en el contenedor

Cuando la invocación llega a una acción, el resultado del parse está también en el contenedor, como
`IParseResult`: el mismo objeto que recibe la acción. Un servicio lo pide por constructor:

```pascal
constructor TExportJob.Create(const Parsed: IParseResult);
begin
  inherited Create;
  FParsed := Parsed;
end;
```

Y un registro puede depender de un argumento con una factory, que lee el resultado del parse al
resolver:

```pascal
var DryRun := CommandLine.Option<Boolean>('--dry-run').Description('Informa, no escribe');
B.RootCommand.Add(DryRun);

B.Services.AddSingleton<IWriter>(
  function(Provider: IServiceProvider): IWriter
  begin
    if DryRun.ValueIn(Provider.GetRequiredService<IParseResult>) then
      Result := TReportWriter.Create
    else
      Result := TFileWriter.Create;
  end);
```

### La composición decide con el parse

`B.ParseArgs(Args)` parsea `Args` con la gramática y la congela, así que la composición puede
decidir con lo parseado: qué fuente añadir, qué implementación registrar.
`B.ParseArgs(Args, Configuration)` los parsea con una `TParserConfiguration`, como
`rootCommand.Parse(args, configuration)`. Parsea una sola vez: llamado otra vez con los mismos
argumentos —elemento a elemento, distinguiendo mayúsculas— y la misma configuración —sus dos
interruptores, y si lee ficheros de respuesta— devuelve el mismo resultado, y el `Build` también; con
otros argumentos u otra configuración, lanza `ECliCompositionError`.

```pascal
var Parsed := B.ParseArgs(Args);
if Loud.ValueIn(Parsed) then
  B.Services.AddSingleton<IGreeter, TLoudGreeter>
else
  B.Services.AddSingleton<IGreeter, TGreeter>;
```

Después de `ParseArgs`, cambiar la gramática lanza `ECommandLine`; las acciones se siguen pudiendo
declarar.

Una composición que se salta el resto cuando el parse se contesta solo —ayuda, versión o una
invocación que no encaja— y va directa al `Build` deja contestar a la gramática aunque lo que iba a
añadir después no se pueda leer:

```pascal
var Parsed := B.ParseArgs(Args);
if Parsed.Succeeded and not Parsed.IsHelpRequested and not Parsed.IsVersionRequested then
  JsonConfig.AddFile(B.Configuration, 'web.json');
ExitCode := B.Build.Run;
```

Una validación que lee la configuración solo ve las fuentes añadidas antes de `ParseArgs`.

---

## Los argumentos, en la gramática y en la configuración

`CreateBuilder(Args)` carga los argumentos en `Configuration`, como hace
`Host.CreateApplicationBuilder(args)` en .NET. La gramática (`RootCommand`) parsea lo que recibe
`ParseArgs(Args)`, como `rootCommand.Parse(args)`: los argumentos del builder no le llegan, y sin
`ParseArgs` parsea una invocación vacía.

Una herramienta de consola les da los argumentos del proceso a los dos, y llama a `ParseArgs` una vez
declarada la gramática:

```pascal
var B := TCliApplication.CreateBuilder(CommandLineConfig.ProcessArgs);
// ...la gramática, los servicios y las acciones
B.ParseArgs(CommandLineConfig.ProcessArgs);
ExitCode := B.Build.Run;
```

Un proceso cuyos argumentos no son de fiar, como un CGI —que recibe como argumentos la consulta de
una URL sin `=`—, crea el builder sin ellos, y se los da solo a la gramática con los ficheros de
respuesta apagados: `--help` y `--version` contestan, la configuración no los ve, y un token que
empieza por `@` llega como valor, así que no se lee ningún fichero.

```pascal
var B := TCliApplication.CreateBuilder;
var Untrusted: TParserConfiguration;
Untrusted.ResponseFileTokenReplacer := nil;
B.ParseArgs(CommandLineConfig.ProcessArgs, Untrusted);
```

Cuando los dos reciben los mismos argumentos, la configuración los lee sin la gramática. Admite `--clave=valor` y `--clave valor`, ignora las
banderas sueltas y los posicionales, y una bandera seguida de un posicional la lee como clave y valor.
Sobre `mytool --verbose informe.txt`:

- la gramática ve la bandera `--verbose` y el argumento posicional `informe.txt`;
- la configuración ve la clave `verbose` con el valor `informe.txt`.

Lo que la gramática declara se lee del resultado del parseo, no de `Configuration`.

Pasada a los dos, una clave de configuración que la gramática no declara —`--Db:Server=localhost`— no
deja arrancar la aplicación: la gramática rechaza por defecto lo que no reconoce, y `Run` devuelve
`TCliExit.InvocationError`. Para admitirla, la raíz lo declara:

```pascal
B.RootCommand.AllowUnmatchedTokens;
```

---

## Dónde diverge de .NET

| .NET | Aquí | Por qué |
|---|---|---|
| Métodos de extensión (`AddX`, `UseX`) | Métodos del propio record, o fachada `class` | Delphi no tiene métodos de extensión, y solo hay **un** `record helper` visible por tipo — una API fluida repartida entre units se rompe en silencio en cuanto alguien reordena un `uses` |
| `IHostApplicationBuilder.Metrics` | **No se porta** | No hay pieza equivalente en DAF a la que apuntar |
| `ConfigureContainer<T>` | **No se porta** | Un método genérico no es expresable en una interfaz de Delphi — no es que se descarte, es que no cabe. Las opciones del proveedor por defecto, que .NET fija a través de él, son `ServiceProviderOptions` |
| `Command.SetAction`, cuya acción recibe el `ParseResult` | `B.SetAction(Command, Action)`, cuya acción recibe además el `IServiceProvider` | `System.CommandLine` no tiene host. Aquí el host arranca antes de que corra la acción, y su proveedor llega por parámetro |
| Sin global de aplicación | Igual | En consola no hay ningún `Application` global que el usuario espere: la acción recibe el `IServiceProvider` que necesita |

La última fila es la que separa esta variante de las de escritorio: **VCL y FMX sí ofrecen un
global**, porque su plataforma ya tiene uno.

---

## Dependencias del paquete

`Daf.Cli` requiere `Daf.Hosting` y `Daf.CommandLine`. Aquí el parser **no es opcional**: `--version`
y `--help` los contesta la gramática, así que toda aplicación CLI de la familia enlaza el parser —
el mismo precio que paga .NET.
