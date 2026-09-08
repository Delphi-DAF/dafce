# Migrar a DAF 2.0

Esta versión es un **major**: hay cambios que rompen la compilación. Todos se detectan al
compilar —ninguno degrada en silencio— y la lista de abajo los cubre por completo.

Se sube desde **1.6.0**. No hubo una 1.7.0 publicada: se planificó, se decidió cortarla como
major y su contenido va aquí, así que un salto de 1.6 a 2.0 es un solo paso.

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
  .AddModule<TCGSuiteModule>;
```

Y se van con él los `uses` marcados `// force-link:` que solo estaban para que la RTTI viera el
módulo: nombrar el tipo en `AddModule<T>` ya enlaza su unit.

`IAppModule` y `TAppModule` **no cambian**, y el cuerpo de cada módulo —su `AddServices`— no se
toca una coma. Lo que desaparece es cómo se encontraban, no lo que son.

⚠️ **Si tu aplicación deriva de `TDafApplication`, esto te afecta aunque no hayas escrito nunca
`AddModules`.** `TDafApplicationBuilder` descubría los módulos del paquete de la clase de
aplicación por su cuenta; ahora no registra ninguno. Declara los tuyos donde compongas los
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

var FileOpt    := CommandLine.Option<string>('--file', 'Fichero a compilar');
var PortOpt    := CommandLine.Option<Integer>('--port', 'Puerto en el que escuchar');
var VerboseOpt := CommandLine.Option<Boolean>('--verbose', 'Contar más');

var Root := CommandLine.Root('mytool', 'Una herramienta');
Root.Add([FileOpt, PortOpt, VerboseOpt]);

var Parsed := Root.Parse;                     // sin argumento: lee ParamStr
if not Parsed.Succeeded then
begin
  Writeln(Parsed.ErrorText);                  // nombra la opción y el texto que recibió
  Exit;
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
| `.Arg<T>('name')` | `CommandLine.Option<T>('--name', descripción)`, añadida con `Root.Add` — los guiones se escriben ahora |
| `.Arg<string>('output\|o')` | `CommandLine.Option<string>('--output', …).WithAlias('-o')` |
| `A.Default(V)` | `.WithDefault(V)`, y una forma calculada para el que haya que averiguar |
| `A.Required(True)` | `.Required` |
| `A.Help('…')` | la descripción, segundo parámetro de `Option<T>` — y esta sí **se ve**: la ayuda se genera |
| `.Command('build')` … `.EndCommand` | `CommandLine.Command('build', descripción)`, colgada con `Parent.Add(Child)` |
| `Params['x'].AsString` / `.AsInteger` / `.AsBoolean` | `XOpt.ValueIn(Parsed)`, tipado por la propia opción |
| `Params.GetSection('build')['output']` | `OutputOpt.ValueIn(Parsed)`: no hay sección que navegar |
| `Params.Command = 'build'` | `Parsed.Command`, un `ICommand` — compáralo con el objeto que declaraste |
| `Parser.Parse(…) = nil` | `Parsed.Succeeded` en falso, con `Parsed.Errors` y `Parsed.ErrorText` |

**Lo que no pasa igual.** Se dice porque una migración que solo enseña la mitad fácil deja al lector
creyendo que ya está:

- **`Parse` ya no toma la línea de comandos como cadena.** `Parse` lee `ParamStr`, y `Parse(Args)`
  toma un `TArray<string>`. Quien le pasaba una cadena construida a mano —normalmente un test— pasa
  el array.
- **El orden dejó de importar, así que hay invocaciones que antes fallaban y ahora valen.** En
  `Daf.CmdLn` los argumentos de un comando se consumían *antes* de su subcomando, y
  `app build -o dir --flag` no parseaba. Una opción `Recursive` se lee desde la hoja, y esa
  invocación ahora sí parsea. Nada que funcionara deja de funcionar; un test que afirmaba el fallo,
  sí.
- **Dos limitaciones del motor de tipos viejo ya no están, y su resultado cambió.** No se podía
  pasar un número negativo, y `--port 80xyz` se aceptaba como `80`. Ahora `-5` es un valor, y
  `80xyz` se rechaza nombrando la opción. Si algo se apoyaba en ese truncado, recibe un error donde
  antes recibía un número.
- **No hay invocación.** Ninguno de los dos despacha a un handler: `Daf.CommandLine` parsea y
  devuelve valores tipados, y qué hacer con ellos sigue siendo decisión de la aplicación.

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
  ExitCode := B.Build.Run;
  ```

  La maquinaria sobrevive entera — `THostBuilder` baja a detalle de implementación. Lo que cambia es
  quién la llama y cuándo.

- **Tu punto de entrada es ahora tu variante.** `TCliApplication` (`Daf.Cli`), `TWebApplication`
  (`Daf.Web`), `TVclApplication` (`Daf.VCL`) y `TFmxApplication` (`Daf.FMX`). Comparten `Services`,
  `Configuration`, `Environment`, `Logging` y `Properties` por `IHostApplicationBuilder`.

- **`TDafApplication` y `TDafApplicationBuilder` quedan deprecados**, no borrados: siguen
  compilando. `TDafApplication.VersionRequested` **sí** se retira — la gramática contesta
  `--version` por `ICommandLineParseResult.VersionRequested`. Y `TDafApplication.App` pasa a ser
  `DafApp`, que existe **solo en VCL y FMX**: en CLI y en Web no hay global de aplicación a
  propósito.

- **`Daf.Application.Version` se muda a `Daf.Commons`**, sin tocar. Añade `Daf.Commons` a
  `requires` si usabas `TVersionInfo` y no lo tenías ya.

- **`ILoggingBuilder` se muda a `Daf.Logging.Abstractions`**, donde .NET lo tiene. Componer logging
  deja de exigir el paquete de implementación. Y el builder lo expone como propiedad viva — el
  pipeline solo se compone **si la tocas**, así que una aplicación que nunca configura logging
  sigue arrancando.

- **El entorno pasa a derivarse de la configuración de host.** Las tres claves conservan su nombre
  (`DAF_APP_ENV`, `DAF_APP_NAME`, `DAF_CONTENT_ROOT`), así que lo que fijaras por variable de
  entorno sigue funcionando — y ahora además funciona `--DAF_APP_ENV=Development`, que antes no
  había forma de escribir.

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

## Cómo hacerlo

1. Compila. Lo que falte sale como error del compilador, no como comportamiento raro en
   ejecución.
2. Sustituye cada `AddModules` por una llamada por módulo, y quita los `uses` de force-link.
3. Añade a los `requires` los paquetes nuevos que necesites —`Daf.SystemProcess`, y
   `Daf.CommandLine` si parseabas una línea de comandos— y quita `Daf.CmdLn` y
   `Daf.SQLCute.Compilers`.
4. Renombra los tipos y unidades de las tablas de arriba.
5. Vuelve a compilar y pasa tus tests. `ValidateOnBuild` ayuda a confirmar que la composición
   registra lo mismo que antes.
