# Migrar a DAF 2.0

Esta versión es un **major**: hay cambios que rompen la compilación, y otros que cambian el
comportamiento en ejecución sin que el compilador diga nada. La lista de abajo cubre los dos; el paso
6 de «Cómo hacerlo» dice cuáles son de ejecución.

Se sube desde **1.6.0**. No hubo una 1.7.0 publicada —su contenido va aquí—, así que de 1.6 a 2.0 es
un solo paso.

## Lo que hay que tocar, por orden de probabilidad

### 1. Los módulos de aplicación se registran por su nombre

`AddModules(Package)` —el descubrimiento por RTTI de los `IAppModule`— **ya no existe**. Cada
módulo se aplica por su nombre, en el orden que la aplicación quiera:

```pascal
// antes
ServiceCollection.AddModules(_T.PackageOf<TAlgo>);

// ahora
ServiceCollection
  .AddModule<TEntornoModule>
  .AddModule<TEvaluationModule>
  .AddModule<TSuiteModule>;
```

Y se van con él los `uses` marcados `// force-link:` que solo estaban para que la RTTI viera el
módulo: nombrar el tipo en `AddModule<T>` ya enlaza su unit.

`IAppModule` y `TAppModule` **no cambian**, y el cuerpo de cada módulo —su `AddServices`— no se
toca una coma. Lo que desaparece es cómo se encontraban, no lo que son.

⚠️ **Si tu aplicación deriva de `TDafApplication`, esto te afecta aunque no hayas escrito nunca
`AddModules`.** Aquel builder descubría los módulos del paquete de la clase de aplicación por su
cuenta. `TDafApplication` se **retira** en 2.0 —tu punto de entrada es el builder de tu variante,
más abajo— y ninguna variante registra módulos por ti. Declara los tuyos donde compongas los
servicios:

```pascal
Builder.Services
  .AddModule<TEntornoModule>
  .AddModule<TEvaluationModule>;
```

**Por qué.** En compilación nativa el descubrimiento no ahorraba la declaración: la escondía. Para
que la RTTI vea una clase, su unit tiene que estar en la imagen, y el linker solo la deja si
alguien hizo `uses` — así que la declaración ya existía, disfrazada. A cambio de no ahorrar nada
añadía dos modos de fallo: una RTTI recortada dejaba el descubrimiento **ciego en silencio**, y el
orden de registro lo decidía el recorrido de la reflexión. `AddModule<T>` además respeta el
constructor del módulo, que el descubrimiento se saltaba sin decirlo.

### 2. `Daf.Commons` se ha partido

`Daf.Commons` se queda **solo con lo que extiende la biblioteca estándar**. Lo que sale, sale a un
paquete propio, y hay que añadirlo a los `requires` del paquete que lo use:

| Lo que usabas | Ahora vive en |
|---|---|
| `Daf.CmdLn.Parser` | a ningún sitio: retirado — ver la sección siguiente |
| `Daf.SystemProcess` | paquete `Daf.SystemProcess` |

`KillAfterTimeout` desaparece de `ISystemProcess` y de su builder: el plazo mata siempre el proceso.
Quita la llamada.

Un flujo sin destino declarado **hereda** ahora el del padre: lo que escribe el hijo sale donde
escribe tu proceso. Para que calle, como en 1.6.0, descarta los dos:

```pascal
  .StandardOutput(TProcessOutput.Discard)
  .StandardError(TProcessOutput.Discard)
```

Un flujo admite una sola declaración: `OnStdOut` dos veces, u `OnStdOut` y después `StandardOutput`,
lanza `EInvalidOpException`. Deja una por flujo.

`HideWindow(False)`, el valor por defecto, ya no abre una ventana de consola propia: el hijo corre
en tu consola. No hay opción que devuelva la ventana aparte.

`ExecuteAsync` lanza ahora al hijo antes de volver, en el hilo que lo llama, y solo la espera corre
en la tarea. Un lanzamiento que falla llama a `OnFailed` en ese mismo hilo, antes de que
`ExecuteAsync` vuelva: un manejador que contaba con correr en segundo plano ya no lo hace.

### 3. `Daf.CmdLn.Parser` ya no está

La unit que 1.6.0 traía dentro de `Daf.Commons` **ya no se distribuye**. Su sustituto es
`Daf.CommandLine`, el porting de
[`System.CommandLine`](https://learn.microsoft.com/dotnet/standard/commandline) de .NET
—la librería sobre la que está construido el propio `dotnet` CLI—. **No hay adaptador ni alias de
unidad**: son APIs distintas, y las cláusulas `uses` que existan no siguen compilando.

La misma gramática, declarada de las dos formas:

```pascal
// antes
uses Daf.CmdLn.Parser;

var Parser := TCmdLnParserBuilder.Create
  .Arg<string>('file')
  .Arg<Integer>('port')
  .Arg<Boolean>('verbose')
  .Build;

var Params := Parser.Parse(GetCommandLine);
if Params = nil then Exit;                    // nil era todo el parte de una invocación mala

var FilePath  := Params['file'].AsString;     // 'file' escrito por segunda vez, sin comprobar
var Port      := Params['port'].AsInteger;    // y el tipo elegido otra vez, en ejecución
var IsVerbose := Params['verbose'].AsBoolean;
```

```pascal
// ahora
uses Daf.CommandLine;

var FileOpt    := CommandLine.Option<string>('--file').Description('Fichero a compilar');
var PortOpt    := CommandLine.Option<Integer>('--port').Description('Puerto en el que escuchar');
var VerboseOpt := CommandLine.Option<Boolean>('--verbose').Description('Contar más');

var Root := CommandLine.Root('mytool', 'Una herramienta');
Root.Add([FileOpt, PortOpt, VerboseOpt]);

var Parsed := Root.Parse;                     // sin argumento: lee ParamStr
if Parsed.IsHelpRequested or Parsed.IsVersionRequested or not Parsed.Succeeded then
begin
  ExitCode := Parsed.Invoke;                  // la ayuda, la versión, o cada error nombrando la
  Exit;                                       // opción y el texto que recibió
end;

var FilePath  := FileOpt.ValueIn(Parsed);     // un string, estáticamente
var Port      := PortOpt.ValueIn(Parsed);     // un Integer, estáticamente
var IsVerbose := VerboseOpt.ValueIn(Parsed);
```

**Dónde desaparece el nombre repetido.** A la izquierda, `'file'` se escribe una vez al declarar y
otra al leer, y nadie comprueba que coincidan: una errata devuelve valor vacío, no un error. A la
derecha el objeto de la opción **es** la llave, así que el nombre existe una sola vez y pedir uno que
no se declaró no es representable. Leer un `IOption<Integer>` en un `string` es error **de
compilación**, donde `AsInteger` era una decisión que se volvía a tomar en cada lectura.

La parte mecánica, término a término:

| `Daf.CmdLn` | `Daf.CommandLine` |
|---|---|
| `TCmdLnParserBuilder.Create` … `.Build` | `CommandLine.Root(nombre, descripción)` |
| `.Arg<T>('name')` | `CommandLine.Option<T>('--name')`, añadida con `Root.Add` — los guiones se escriben ahora |
| `.Arg<string>('output\|o')` | `CommandLine.Option<string>('--output', ['-o'])` |
| `A.Default(V)` | `.DefaultValue(V)`, y `.DefaultValueFactory(…)` para el que haya que averiguar |
| `A.Required(True)` | `.Required` — en un `Boolean` no falla nunca: un `Boolean` tiene `false` por defecto |
| `A.Help('…')` | `.Description('…')` — y esta sí **se ve**: la ayuda se genera |
| `.Command('build')` … `.EndCommand` | `CommandLine.Command('build', descripción)`, colgada con `Parent.Add(Child)` |
| `Params['x'].AsString` / `.AsInteger` / `.AsBoolean` | `XOpt.ValueIn(Parsed)`, tipado por la propia opción |
| `Params.GetSection('build')['output']` | `OutputOpt.ValueIn(Parsed)`: no hay sección que navegar |
| `Params.Command = 'build'` | `Parsed.Command`, un `ICommand` — compáralo con el objeto que declaraste |
| `Parser.Parse(…) = nil` | `Parsed.Succeeded` en falso. `Parsed.Errors` lleva cada error con el resultado de su símbolo, y `Parsed.ErrorText` su texto |

**Lo que no pasa igual.** Se dice porque una migración que solo enseña la mitad fácil deja al lector
creyendo que ya está:

- **`Parse(string)` no es `Parser.Parse(GetCommandLine)`.** Para la línea de comandos del proceso,
  `Parse` sin argumentos lee `ParamStr`. `Parse(string)` parte una línea con
  `CommandLine.SplitCommandLine`, y toma su primer token por la raíz solo cuando es el nombre de la
  raíz o la ruta del ejecutable en marcha: una línea de `GetCommandLine` que empieza por `app.exe`, para
  una raíz llamada `app`, lee `app.exe` como un valor.
- **Los nombres distinguen mayúsculas.** `Daf.CmdLn` tomaba `--Verbose` por `--verbose`;
  `Daf.CommandLine` no, como .NET. Un parse configurado con
  `TParserConfiguration.EnableCaseInsensitiveNames` los compara como antes.
- **El orden dejó de importar, así que hay invocaciones que antes fallaban y ahora valen.** En
  `Daf.CmdLn` los argumentos de un comando se consumían *antes* de su subcomando, y
  `app build -o dir --flag` no parseaba. Una opción `Recursive` se lee desde la hoja, y esa
  invocación ahora sí parsea. Nada que funcionara deja de funcionar; un test que afirmaba el fallo,
  sí.
- **Dos limitaciones del motor de tipos viejo ya no están, y su resultado cambió.** No se podía
  pasar un número negativo, y `--port 80xyz` se aceptaba como `80`. Ahora `-5` es un valor, y
  `80xyz` se rechaza nombrando la opción, y leer la opción lanza ese error. Si algo se apoyaba en
  ese truncado, recibe un error donde antes recibía un número.
- **Dos valores para una opción que toma uno son un error.** `Daf.CmdLn` se quedaba con el último:
  `--port 1 --port 2` leía 2. Ahora el parse falla con
  `Option '--port' expects a single argument but 2 were provided.`, y leer la opción lo lanza.
  Nombrarla otra vez sin valor funciona como antes: `--port 1 --port` lee 1.
- **El despacho puede pasar a la gramática.** `Daf.CmdLn` devolvía los valores y dejaba el despacho a
  la aplicación. En `Daf.CommandLine` cada comando puede declarar su acción con `SetAction`, e
  `Invoke` sobre el resultado del parse ejecuta la del comando alcanzado y devuelve el código de
  salida. El código que despacha a mano sobre `Parsed.Command` sigue funcionando.

`Daf.Configuration.CommandLine` no se toca. Leer la línea de comandos como **fuente de
configuración** —`--Logging:LogLevel=Debug` pisando `appsettings.json`— es otro trabajo y no
comparte motor con ninguno de los dos módulos.

### 4. Tipos de `Daf.Commons` renombrados o mudados

| Antes | Ahora |
|---|---|
| `TExpresion<T>` | **`TDeferred<T>`**, en `Daf.Types` |
| `TFilterSpec<T>`, en `Daf.Expression` | misma clase, en `Daf.FilterSpec` |
| unidad `Daf.Expression` | disuelta en `Daf.FilterSpec` y `Daf.Types` |
| `TBooleanExpresion` | retirado |
| `&If.Exp<T>` | retirado — lo sustituye el operador ternario de Delphi 13, `if … then … else` como expresión, que evalúa solo la rama que toma |
| `TGenericRTTI.GetGenericArguments(TRttiType)` | **`_T.GenericArgs(PTypeInfo)`**, en `Daf.Rtti` — devuelve información de tipo en vez de tipos reflejados, y cuenta como uno un argumento que a su vez es genérico |
| `TGenericRTTI.MakeGenericType` | retirado sin sustituto — ver abajo |
| `TSmartArray<T>.Select(TDeferred<TMapper<T, TTarget>>)` | `Select(TFunc<T, TTarget>)` |

`Daf.Linq.Where` / `Filter` toman `TFilterSpec<T>`, el tipo que ya vivía en Commons: `Daf.Linq` no
introduce un vocabulario de filtrado propio, evalúa el que ya estaba nombrado.

`MakeGenericType` no tiene sustituto porque prometía algo que Delphi no hace: fabricar una
instanciación genérica en ejecución. Fallaba con violación de acceso incluso en el caso para el que
se escribió. Si la llamabas para obtener un genérico cerrado que el compilador nunca vio, no hay a
dónde migrar — pero si lo que necesitabas en realidad era un registro que sirviera a todos los
cierres de un contrato, eso es ahora
[registrar un contrato genérico una sola vez](src/DependencyInjection.Abstractions/README.es.md#registrar-un-contrato-genérico-una-sola-vez).

### 5. Módulos de consulta renombrados

| Antes | Ahora |
|---|---|
| `Daf.Query.Expressions` | `Daf.Specification` |
| `Daf.SQLCute.Expressions` | `Daf.SQLCute.Specification` |
| `Daf.SQLCute.Compilers` | disuelto en `Daf.SQLCute` |

Los nombres nuevos dicen el patrón que implementan. `Daf.SQLCute.Compilers` desaparece como
paquete: sus seis compiladores de dialecto viven en `Daf.SQLCute`, junto al ANSI que ya estaba
ahí, así que hay una línea menos en los `requires`.

### 6. Logging: el modo mudo ya no existe

- **`DAFInternalLogger` retirada.** Era la única vía mientras la categoría del canal estuvo
  clavada a `DAF`; sin ese filtro es una segunda puerta que además etiqueta como DAF a quien no
  lo es.
- **Componer el pipeline sin un solo proveedor falla al construir el host**, con
  `ELoggingCompositionError`, en vez de entregar un `ILogger` que acepta todo y no escribe nada.
  Si tu arranque componía logging sin proveedores y no te habías dado cuenta, esto es lo que te lo
  va a decir.
- Los informes de arranque salen bajo la categoría de la clase que informa y por el pipeline de
  `ILogger`. No es API, es salida: si filtrabas por la categoría `DAF`, revisa el filtro.
- **`ILogger` gana `IsEnabled(Level)`, y esta sí rompe en compilación.** Un registrador contesta ahora
  si una entrada de ese nivel llegaría a algún destino, de modo que quien llama puede saltarse la
  construcción de argumentos que cuestan más que la propia línea —un volcado de estado, una consulta—.
  Un descendiente de `TLogger` hereda un defecto que contesta `True`, así que un proveedor de logging
  escrito fuera de DAF sigue compilando sin tocarlo; solo un tipo que implemente `ILogger` **a mano**
  tiene que añadirlo. Contesta `True` si dudas — perder una línea por no saber es peor que componer una
  de más:

  ```pascal
  // añadir a cualquier implementador de ILogger escrito a mano
  function TMiLogger.IsEnabled(const Level: TLogLevel): Boolean;
  begin
    Result := True;  // o tu propia prueba de nivel, la misma que aplique tu Log
  end;
  ```

### 7. El contenedor cerrado ya no responde

- **Resolver de un contenedor cerrado lanza `EContainerShutDownError`**, y el mensaje nombra el
  servicio pedido. Antes hacía dos cosas distintas y ninguna era esa: un `EAccessViolation` si el
  servicio era Singleton, y servirlo en silencio si era Scoped —una instancia que ya nadie iba a
  recoger—. `EContainerShutDownError` deriva de `EServiceProviderError`, así que quien ya capture
  el padre sigue capturándolo.
- **Crear un ámbito desde un contenedor cerrado falla igual**, en vez de entregar un ámbito que no
  puede resolver.
- **Cerrar dos veces ya no rompe.** Si tu código cerraba en dos sitios —el cierre explícito de la
  aplicación y el desmontaje de quien posee el proveedor— y evitabas el segundo con una bandera,
  esa bandera ya no hace falta.

Lo único que hay que revisar es la última resolución antes del cierre:

```pascal
// Antes: funcionaba por accidente si el servicio era Scoped
Provider.ShutDown;
var Report := Provider.GetRequiredService<IReport>;   // ahora: EContainerShutDownError

// Ahora: resuelve mientras el contenedor está vivo
var Report := Provider.GetRequiredService<IReport>;
Provider.ShutDown;
```

### 8. La familia de aplicaciones, y un solo estilo de builder

Es el frente más grande de 2.0 para cualquier cosa que tuviera un `.dpr`.

- **`IHostBuilder` se retira, y con él el estilo diferido.** Registrar por callbacks de
  `ConfigureServices` / `ConfigureAppConfiguration` / `ConfigureHostConfiguration` deja de existir.
  Queda un estilo, el inmediato: el builder es utilizable en cuanto se crea, y lo que registras
  ocurre donde está escrito.

  ```pascal
  // Antes
  var Builder: IHostBuilder := THostBuilder.Create;
  Builder.ConfigureServices(
    procedure(Context: IHostBuilderContext; Services: IServiceCollection)
    begin
      Services.AddSingleton<IMyService, TMyService>;
    end);
  var Host := Builder.Build;

  // Ahora
  var B := TCliApplication.CreateBuilder(CommandLineConfig.ProcessArgs);
  B.Services.AddSingleton<IMyService, TMyService>;
  B.ParseArgs(CommandLineConfig.ProcessArgs);
  ExitCode := B.Build.Run;
  ```

  La maquinaria sobrevive entera — `THostBuilder` baja a detalle de implementación. Lo que cambia es
  quién la llama y cuándo.

- **Tu punto de entrada es ahora tu variante.** `TCliApplication` (`Daf.Cli`), `TWebApplication`
  (`Daf.Web`), `TVclApplication` (`Daf.VCL`) y `TFmxApplication` (`Daf.FMX`). Comparten `Services`,
  `Configuration`, `Environment`, `Logging`, `Properties` y `ServiceProviderOptions` por
  `IHostApplicationBuilder`.

- **`TDafApplication` y `TDafApplicationBuilder` se retiran**, y con ellos el paquete
  `Daf.Application`. Su relevo es la variante de la entrada de arriba, y cada miembro tiene
  destino: `CreateHostBuilder` pasa a ser el `CreateBuilder` de tu variante; `IDafApplication`,
  la `I…Application` que devuelve `Build`; `RunAsync` es `Run` con su propio nombre;
  `VersionRequested` es `IParseResult.IsVersionRequested`, contestado por la gramática;
  y `App` pasa a ser `DafApp`, que existe **solo en VCL y FMX** — en CLI y en Web no hay global
  de aplicación a propósito.

  La parte de método plantilla de `TDafApplication` —`AppClass`, el `Create(Host)` virtual,
  `Execute` y `VersionInfo`— no tiene equivalente en las variantes. En CLI el trabajo es la acción
  de la raíz, declarada con `B.SetAction`, que recibe el `IServiceProvider`; lo que corría a su
  alrededor es una tarea de arranque (`AddStartupTask<T>`) o un servicio alojado; y la versión es un
  atributo `[Version]` sobre cualquier clase.

- **El `try`/`except` alrededor del `Run` pasa a ser una política.** Capturar lo que se escapa,
  informarlo y elegir el código de salida es `B.OnException(Policy)` en las variantes CLI y Web,
  declarada antes que el resto de la composición. El `except` del `.dpr` solo pasa lo que se escapa
  antes del `Run` a `B.HandleException(E)`, que se lo da a esa misma política y devuelve el código de
  salida. El sample `GreetTool` de `Daf.Cli` enseña el `.dpr` entero.

- **`Daf.Application.Version` se muda a `Daf.Commons`.** Añade `Daf.Commons` a `requires` si usabas
  `TVersionInfo` y no lo tenías ya. Y cambian dos cosas:

  - `TVersionInfo.Debug` es de solo lectura, y lo declara el binario: un cuarto argumento del
    atributo o de `TVersionInfo.Create`. Sin él, `Debug` vale `False` — en 1.6 informaba de cómo se
    compiló el propio DAF, fuera cual fuera la configuración de tu binario.
  - `VersionTag` recibe una plantilla en lugar del flag `WithCompiledMeta`.

  ```pascal
  // before
  Info.Debug := {$IFDEF DEBUG}True{$ELSE}False{$ENDIF};
  Text := Info.VersionTag(True);

  // now
  [Version('2.9.0', 'MyApp', 'Acme', {$IFDEF DEBUG}True{$ELSE}False{$ENDIF})]
  TMyApp = class
  end;

  Text := TVersionInfo.GetFrom(TMyApp).VersionTag(VersionTemplateWithBuild);
  ```

- **`ILoggingBuilder` se muda a `Daf.Logging.Abstractions`**, donde .NET lo tiene. Componer logging
  deja de exigir el paquete de implementación. Y el builder lo expone como propiedad viva — el
  pipeline solo se compone **si la tocas**, así que una aplicación que nunca configura logging
  sigue arrancando.

- **El entorno pasa a derivarse de la configuración de host**, así que se puede fijar por línea de
  comandos además de por variable. Sus claves también cambian de nombre: ver la sección 20.

- **`IConfiguration` gana dos miembros**, para poder distinguir una clave ausente de una cuyo valor
  es vacío. Esto rompe a los **implementadores** de la interfaz, no a quien la llama: el indexador
  sigue devolviendo `string`, y toda expresión existente por él sigue compilando.

  ```pascal
  if Config.Exists('Features:Beta') then ;                   // presencia
  var Read := Config.Find('Features:Beta');                  // presencia Y valor, una sola lectura
  if Read.HasValue and (Read.Value = 'on') then ;
  var Value := Config['Features:Beta'];                      // '' si no está Y si está vacía
  ```

- **Un fallo leyendo un fichero de configuración se informa al añadir la fuente**, y ya no dentro de
  `Build` — porque es cuando el fichero se abre ahora. Sigue saliendo por el canal de arranque y
  sigue propagando; lo que cambia es qué categoría lo firma.

### 9. MiniSpec: la marca de inyección se renombra, y el inyector guarda los tres contextos

- **`[Inject]` pasa a ser `[MiniInject]`.** `Inject` a secas es un nombre que reclaman varias
  herramientas, y dos units que exporten el mismo en el mismo `uses` no dan error: gana la última,
  en silencio. El atributo viaja ahora en la facade, así que el `uses` de más sobra:

  ```pascal
  // Antes
  uses Daf.MiniSpec, Daf.MiniSpec.Injection;
  ...
    [Inject]
    property Shared: TSharedContext read FShared write FShared;

  // Ahora
  uses Daf.MiniSpec;
  ...
    [MiniInject]
    property Shared: TSharedContext read FShared write FShared;
  ```

  Dejar `[Inject]` **no** rompe la compilación: el compilador emite `W1074 Unknown custom attribute`
  y en ejecución el miembro no tiene atributo, así que no se inyecta nada y nada lo dice. Busca
  `[Inject]` en tus specs.

- **La marca funciona ahora también sobre un campo**, que es lo que la guía enseñaba desde siempre y
  nunca había funcionado. Un campo se alcanza sea cual sea su visibilidad; una propiedad, solo si es
  public o published.

- **`TInjectorService` deja de ser un registro de servicios sueltos a nivel de clase.** Es una
  instancia de la ejecución, y lo que guarda son los tres contextos vivos mientras corre un step.
  `Register` toma el ámbito, así que una suite que registraba mocks por su cuenta no compila:

  ```pascal
  // Antes: cualquier objeto, indexado por su clase
  TInjectorService.Register(TDatabaseMock.Create);

  // Ahora: no hay registro suelto. El colaborador se construye en el World, o se comparte
  // por el contexto de feature o de suite, que es lo que se inyecta.
  MiniSpec.UseSuiteContext<TTestResources>;   // TTestResources posee los mocks
  ```

- **Cuando más de un contexto satisface una propiedad `[MiniInject]`, el que gana está declarado**:
  primero el tipo exacto —escenario, luego feature, luego suite— y solo después un descendiente, en
  ese mismo orden. Antes contestaba el primero que encajara según el orden interno de un
  diccionario. Esta no rompe la compilación: el World que reciba un contexto distinto al de antes es
  uno que tenía dos encajando en la misma propiedad, y hasta ahora cuál llegaba no se podía prever.

### 10. Un ámbito destruye ahora lo que creó

- **Soltar un ámbito destruye los servicios Scoped que construyó.** El contenedor retenía todos los
  ámbitos que repartía y no soltaba nada hasta cerrarse, así que un proceso que abre un ámbito por
  petición —el patrón que recomienda la guía— acumulaba una instancia por petición durante toda su
  vida.
- **Qué rompe**: guardar una referencia a un servicio Scoped sin sostener su ámbito. Hasta ahora
  sobrevivía al ámbito del que salió; ya no.

```pascal
// Antes: la caché sobrevivía a su ámbito por accidente
var Scope := Provider.CreateScope;
FCache := Scope.ServiceProvider.GetRequiredService<ICache>;
Scope := nil;
FCache.Get('k');                       // ahora: la instancia ya no está

// Ahora: sostén el ámbito mientras sostengas lo que construyó
FScope := Provider.CreateScope;
FCache := FScope.ServiceProvider.GetRequiredService<ICache>;
```

### 11. La inyección por propiedad cambia cuándo se publica una instancia, y cuándo se vacía

- **La instancia se publica antes de rellenar sus propiedades**, para todo registro participe o no
  en la inyección por propiedad. Es lo que hace resoluble un ciclo: quien reentre a resolver el
  mismo servicio recibe la instancia ya publicada.
- **Qué rompe**: un constructor que resuelve su propio servicio. Antes recursaba sin fondo; ahora
  recibe la instancia a medio construir.
- **Cerrar el contenedor vacía las propiedades inyectadas** de lo que posee antes de soltar nada.
  Sin esa pasada, dos servicios que se inyectaron mutuamente se sostienen entre ellos y no se
  destruyen nunca. Un destructor que lea una propiedad inyectada lee ahora `nil`.

```pascal
// Ahora: lo que necesite el destructor, guárdalo en un campo propio
destructor TReportService.Destroy;
begin
  FLogger.Info('closing');             // FLogger es [DafInject]: a estas alturas, nil
  inherited;
end;
```

### 12. `Replace` y `RemoveAll` solo alcanzan los registros sin clave

Los servicios con clave introducen un segundo espacio, y estas dos operaciones se quedan en el que
no la tiene. Antes no había claves, así que «todos los del tipo» y «todos los del tipo sin clave»
eran lo mismo.

- **Qué rompe**: usarlos para desmontar un registro en un arnés de specs. Desmontan menos que antes,
  y se descubre en verde.

```pascal
Services.RemoveAll<IClock>;                                  // solo los registros sin clave
Services.RemoveAllKeyed<IClock>(TValue.From<string>('utc')); // el que la tiene
```

### 13. Un registro repetido da ahora dos instancias

Registrar el mismo par dos veces y pedirlos todos devolvía la primera instancia repetida: el segundo
registro se declaraba, se contaba y no se construía nunca.

- **Qué rompe**: una composición que registra un valor por defecto y vuelve a registrarlo en vez de
  sustituirlo. `TryAdd*` registra solo si no está, y `Replace` sustituye.
- Resolver un solo servicio no cambia —sigue ganando el último registrado— ni cambia el orden de
  cierre.

### 14. SQLCute: una consulta declara qué sentencia es

`IQuery` gana `Kind: TQueryKind`. Vale `Select` por defecto y lo fijan `AsInsert`, `AsUpdate` y sus
hermanos, en vez de deducirse de las cláusulas presentes.

- **Qué rompe**: implementar `IQuery` por tu cuenta. Hay que aportar el miembro nuevo.
- Declarar dos tipos en la misma consulta se queda ahora con el último, donde antes ganaba el
  primero. Una consulta de un solo tipo emite exactamente lo que emitía.

### 15. MiniSpec: qué contexto satisface un `[MiniInject]` pasa a estar declarado

- **La precedencia está declarada, y antes era el orden interno de un diccionario.** Dos pasadas:
  primero una coincidencia **exacta** de tipo, recorriendo escenario → feature → suite; y solo si no
  hay ninguna, un descendiente, en el mismo orden. Así que una coincidencia exacta en la suite gana a
  un descendiente más cercano en el escenario — pedir el tipo que escribiste no es una apuesta.
- **Qué rompe**: un step que inyecta un tipo base mientras dos ámbitos sostienen objetos
  compatibles. Compila, y recibe otra instancia que antes — el ganador anterior dependía del orden
  del hash, así que nunca fue una decisión. Nombra el tipo exacto que quieres, o deja uno de los dos.

### 16. `csoReloadOnChange` desaparece

`TConfigurationSourceOption` se queda con `csoOptional`. El valor retirado no lo leía ningún proveedor, así que no cambia nada de lo que una fuente hacía.

- **Qué rompe**: nombrarlo. Quítalo del conjunto — `[csoOptional, csoReloadOnChange]` pasa a `[csoOptional]`, y un conjunto que solo lo llevaba pasa a `[]`.

### 17. NNLog: un destino que no puede escribir ya no lanza

Un fallo al escribir dentro de un destino llegaba hasta quien registraba. Ahora `TTarget.Write` lo
captura y lo anota en el canal de arranque, bajo el nombre de clase del destino y nombrando el
destino, así que una línea que no llega a donde iba deja de abortar el código que solo intentaba
registrar.

- **Qué rompe**: un spec que afirme la propagación. Se pone rojo, y eso es el cambio llegando y no
  una regresión — no hay nada que restaurar de tu lado.
- **Dónde sale ahora el fallo**: en `BootstrapLog`, como un `LogError` con la excepción y el nombre
  del destino. Un destino cuyos fallos haya que ver se vigila ahí.
- **Un destino propio sobrescribe `DoWrite`, no `Write`**, y de esta mitad sí avisa el compilador:
  `Write` ya no es virtual. Pasa el cuerpo y quita tu propio candado — `TTarget.Write` toma una
  sección crítica propia de esa instancia antes de llamar a `DoWrite`, así que dos hilos que
  escriban por un mismo destino ya no se solapan.

```pascal
// before
procedure TMyTarget.Write(const Entry: TLogEntry);
begin
  Send(RenderEvent(Entry));   // un fallo aquí llegaba al llamante
end;

// now
procedure TMyTarget.DoWrite(const Entry: TLogEntry);
begin
  Send(RenderEvent(Entry));   // un fallo aquí se anota en el canal de arranque
end;
```

### 18. Los servicios alojados paran en orden inverso

Los `IHostedService` paran en el orden inverso al de su registro —el mismo en que el contenedor
libera—, así que un servicio arrancado después para antes que el que puede estar usando. En 1.6
paraban en el orden de registro.

- **Qué rompe**: un servicio cuyo `Stop` contara con que otro registrado después ya hubiera parado.
  Registra el servicio que se usa antes que el que lo usa.

### 19. El host carga `appsettings.json` y el entorno por defecto

El builder carga `appsettings.json`, `appsettings.{Entorno}.json`, `{ApplicationName}.settings.json`
y `{ApplicationName}.settings.{Entorno}.json` del content root, todos opcionales, y todas las
variables de entorno, con `__` como separador de sección, antes de la línea
de comandos: las fuentes de `Host.CreateApplicationBuilder`. En 1.6 el host no cargaba ninguna fuente
por su cuenta.

- **Qué rompe**: una variable de entorno cuyo nombre coincide con una clave de configuración pasa a
  pisarla —`Db__Server` pisa `Db:Server`—, y un `appsettings.json` en el content root se lee sin
  pedirlo. Quita las llamadas que cargaban esos dos ficheros a mano.

### 20. Los ajustes del host toman los nombres de .NET, y el content root es el directorio actual

El host lee sus propios ajustes con las claves del `HostDefaults` de .NET —`environment`,
`applicationName` y `contentRoot`—, como argumento o como variable con el prefijo `DAF_`, que se
quita: `--environment Staging` y `DAF_ENVIRONMENT=Staging` fijan la misma clave. `THostDefaults`
sustituye a `TDafEnvVars`. Y sin `contentRoot`, el content root es el directorio actual, como en
.NET, salvo en el directorio del sistema —donde arranca un servicio de Windows—, que conserva la
carpeta del ejecutable.

```pascal
// antes
TDafEnvVars.APP_ENV            // 'DAF_APP_ENV'

// ahora
THostDefaults.EnvironmentKey   // 'environment', y DAF_ENVIRONMENT como variable
```

- **Qué rompe**: `DAF_APP_ENV`, `DAF_APP_NAME` y `DAF_CONTENT_ROOT` dejan de leerse, y nada lo avisa:
  una aplicación arrancada con `DAF_APP_ENV=Development` corre como `Production`. Renómbralas a
  `DAF_ENVIRONMENT`, `DAF_APPLICATIONNAME` y `DAF_CONTENTROOT`. Una aplicación arrancada desde otro
  directorio ya no encuentra el `appsettings.json` junto a su ejecutable: fija `contentRoot`, o
  arráncala desde su carpeta. Y una configuración de log que escribe bajo `${contentRootPath}` sigue
  ahora al directorio actual; escribe `${binPath}` para que el log se quede junto al ejecutable.

### 21. Logging: el mensaje de una entrada ya no lleva el ámbito

`TLogEntry.Message` es el mensaje formateado y nada más; los ámbitos abiertos viajan solo en
`TLogEntry.Scope`, el más interno primero, cada uno precedido de ` | `. En 1.6 el mensaje los
llevaba pegados detrás, así que cada línea los enseñaba sin pedirlos.

- **Qué rompe**, y el compilador no lo dice: un layout de NNLog con `${message}` y sin `${scope}`
  deja de mostrar los ámbitos —el layout por defecto entre ellos—, y uno con los dos deja de
  escribirlos dos veces. `${message}${scope}` da la línea de 1.6; un destino que se apoyaba en el
  layout por defecto necesita uno propio.

```
antes  ${level} [${category}] ${message}
ahora  ${level} [${category}] ${message}${scope}
```

- **Un proveedor propio** que escribe `Entry.Message` deja de escribir los ámbitos. Añade
  `Entry.Scope`, que ya trae su separador:

```pascal
// before
procedure TMyLogger.Log(const Entry: TLogEntry);
begin
  Send(Entry.Message);                 // los ámbitos venían dentro del mensaje
end;

// now
procedure TMyLogger.Log(const Entry: TLogEntry);
begin
  Send(Entry.Message + Entry.Scope);   // vacío fuera de todo ámbito
end;
```

## Cómo hacerlo

1. Compila. Lo que falte sale como error del compilador, no como comportamiento raro en
   ejecución.
2. Sustituye cada `AddModules` por una llamada por módulo, y quita los `uses` de force-link.
3. Añade a los `requires` los paquetes nuevos que necesites —`Daf.SystemProcess`, y
   `Daf.CommandLine` si parseabas una línea de comandos— y quita `Daf.CmdLn` y
   `Daf.SQLCute.Compilers`.
4. Renombra los tipos y unidades de las tablas de arriba.
5. Vuelve a compilar y pasa tus tests. Después arranca la aplicación una vez con
   `DAF_ENVIRONMENT=Development`: el host valida el contenedor, y un registro que se haya perdido detiene
   la construcción nombrándolo. Un servicio Scoped pedido al proveedor raíz también se rechaza, al
   resolverlo: se resuelve desde un ámbito que abre el trabajo, `Services.CreateScope`.
6. Las secciones 10 a 13, la 15 y de la 17 a la 21 son cambios **de ejecución**: el compilador no dice nada de ellos. Léelas
   contra tu propia composición — la vida de los ámbitos, los registros repetidos, y lo que lea
   un destructor.
