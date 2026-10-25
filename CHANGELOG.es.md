# Changelog

**🌍 Idioma: [English](CHANGELOG.md) | Español**

Todos los cambios relevantes de este proyecto se documentan en este fichero.

El formato sigue [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), el proyecto se versiona
con [Semantic Versioning](https://semver.org/spec/v2.0.0.html) y los commits siguen
[Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/).

## [Unreleased]

Los cambios que rompen una aplicación, al compilar o en ejecución, están recogidos con su diff de ejemplo en `MIGRATION-2.0.md` (`MIGRATION-2.0.es.md` en español).

### Added

- **DependencyInjection**: Servicios con clave — dos implementaciones del mismo contrato distinguidas por una clave, con el juego `AddKeyed*` / `TryAddKeyed*` / `GetKeyed*` de .NET
- **DependencyInjection**: Inyección por propiedad — `[DafInject]` en la propiedad y el contenedor la rellena cuando el constructor ya ha terminado, así que **un ciclo entre dos servicios es resoluble**
- **DependencyInjection**: `TActivatorUtilities.InjectInto` — cablea una instancia que el contenedor no construyó, como un formulario que crea la plataforma, sin tomar posesión de ella
- **DependencyInjection**: `AddInjector<T>` — cablear por código, sin atributos y sin RTTI, así que llega a una clase de otra librería
- **DependencyInjection**: `IOptions<T>` — un servicio pide sus ajustes en el constructor y los recibe construidos y validados, de `AppOptions.Configure<T>` o de una sección de configuración. Las configuraciones de un tipo se acumulan y sus validaciones fallan juntas, con los contratos de .NET `IConfigureOptions<T>` e `IValidateOptions<T>`
- **DependencyInjection**: Genéricos abiertos — `AddSingleton<IRepo<_>, TRepo<_>>` sirve a todos los cierres del contrato. Qué cubre y qué no, en el README
- **DependencyInjection**: `ValidateOnBuild` y `ValidateScopes` en `BuildServiceProvider(TServiceProviderOptions)` — el grafo se comprueba al construir y reporta todos los fallos juntos. Los registros por factory quedan fuera. El host de la familia las activa en el entorno Development, y `ServiceProviderOptions` en el builder las fija a mano. Con `ValidateScopes`, un Scoped resuelto desde el proveedor raíz o desde un Singleton también se rechaza, al resolverlo
- **DependencyInjection**: Mejoras en la composición de la colección
  - `TryAddSingleton` / `TryAddScoped` / `TryAddTransient`, `Replace` y `RemoveAll`, con la semántica de .NET
  - `Descriptors` — los registros de un servicio, en sólo lectura
  - `Decorate<S>` — envuelve los registros existentes conservando su ciclo de vida
  - `AddModule<T>` y `AddModule(Module)` — registros agrupados en una clase, aplicados en el orden escrito
  - `IContainerAccess.OwnedInstanceCount` — cuántos objetos sigue poseyendo el contenedor, para afirmar «se liberó todo»
- **DependencyInjection**: `ILazy<T>` — rompe un ciclo entre dos servicios que se necesitan: uno de los dos declara al otro en su constructor como `ILazy<T>` y su constructor deja de esperarlo. El registro no cambia, y `Value` pregunta al contenedor en cada uso, así que el ciclo de vida sigue siendo el declarado
- **Logging**: `ILogger.IsEnabled(Level)` — preguntar si una entrada de ese nivel llegaría a algún destino antes de construir sus argumentos
- **Logging**: `ILogger<T>` — un servicio lo pide por constructor y recibe un registrador con su propia categoría
- **Logging**: Canal de arranque — un `ILogger` desde la primera línea del proceso, sin inyección de dependencias y sin configuración, para la fase anterior a `BuildServiceProvider`: informa con `BootstrapLog.LoggerFor` y añade destinos con `BootstrapLog.AddSink`. El catálogo de destinos, en el README
- **Logging**: `Builder.AddNullProvider` — descartar las entradas a propósito, dicho al componer
- **Configuration**: Un proveedor nuevo, la línea de comandos — `CommandLineConfig.AddArgs`, en `Daf.Configuration.CommandLine`, lee los argumentos del proceso como claves. Las formas admitidas, en el README
- **Configuration**: `ConfigOptions.Bind<T>` — una sección llega tipada y con dueño a quien la necesita, y el servicio deja de nombrar su sección: la decide el arranque
- ⚠️ **Configuration**: `IConfiguration.Exists` y `Find` — la presencia se **pregunta**, no se deduce de que el valor esté vacío; `Find` contesta presencia y valor en una sola consulta, como `TNullable<string>`. El indexador sigue siendo `string`. **Rompe** a quien implemente `IConfiguration` sin heredar de `TConfigurationRoot`
- **Configuration**: `IConfigurationManager` — constructor y lectura sobre el mismo objeto, así que la configuración se lee mientras se está componiendo. Cada `Add` construye y carga solo el proveedor nuevo
- **Configuration**: `ConfigurationKeyName` — un miembro nombra la clave de configuración por la que se lee, así que una clave que el fichero escribe como `connection-string` llega a una propiedad que Delphi no puede llamar así. Sustituye al nombre del miembro, renombra la sección entera sobre un miembro de tipo objeto, y vale igual para propiedades que para campos
- **Hosting**: `IStartupTask` y `AddStartupTask<T>` — inicialización que corre una vez al arrancar, con el proveedor ya construido y antes de los `IHostedService`. Si una falla, el arranque aborta
- **Hosting**: `THost.CreateBuilder` e `IGenericHostBuilder` — el punto de entrada del host genérico, en la forma de la familia: `var Builder := THost.CreateBuilder;`. `THostBuilder.Create` sigue existiendo, y encadenar sobre él sigue fugando la instancia
- **Hosting**: `StopAndNil` — para el host y vacía la referencia, que tras `Host.Stop` apuntaba a un host liberado
- **Application**: La familia de aplicaciones — `TCliApplication` (`Daf.Cli`), `TVclApplication` (`Daf.VCL`), `TFmxApplication` (`Daf.FMX`) y `TWebApplication` (`Daf.Web`): el mismo arranque hasta que la plataforma toma el bucle, y alrededor de `Run` el código de salida y los metadatos de versión. La variante CLI ejecuta la acción del comando alcanzado, declarada con `B.SetAction`, y un servicio pide el resultado del parse (`IParseResult`) por constructor; todo lo que se escapa, de la composición a la parada, lo decide una sola política, `OnException`; `B.ParseArgs(Args)` le da a la gramática su entrada y deja a la composición decidir con el resultado del parse, mientras los argumentos del builder van a la configuración; y `B.SetStandaloneAction` declara un comando que corre sin host. La variante Web decide lo que se escapa con la misma política, y su `Run` devuelve cómo acabó; VCL y FMX declaran instancia única con `SingleInstance`. FMX no añade targets: siguen siendo Win32 y Win64
- **MiniSpec**: `TSpecRun` — una ejecución con suite, contexto, inyector y runner propios, lanzable dentro de otra sin alterar sus recuentos ni su informe
- **MiniSpec**: `--allow-undefined` — un step undefined deja de fijar el código de salida; `MiniSpec.AllowUndefined(True)` lo declara en la suite. Por defecto rompe
- **MiniSpec**: `SpecContext.ScenarioContext` devuelve el World del escenario, y un `[MiniInject]` de su tipo lo recibe
- **MiniSpec**: `Arg.Matching<T>` — un matcher que recibe la condición misma, así que un rango o cualquier otra comprobación se escribe donde está el setup. El argumento llega al predicado ya convertido a `T`; uno de otro tipo no casa
- **NNLog**: `TNNLogLoader.BuildProvider` acepta un `TNNLogConfiguration` — los ajustes escritos con código, y no solo leídos de una sección
- **NNLog**: Token de layout `${scope}` — los ámbitos que abre `BeginScope` llegan a la línea escrita; se añade al `layout` de un destino
- **SQLCute**: Mejoras en el constructor de consultas
  - `WhereNot(Callback)` y `OrWhereNot(Callback)` — grupos WHERE negados como una unidad, `NOT (…)`
  - Orden de NULL por columna — `OrderBy` / `OrderByDesc` con `TNullsOrder`, y `NullsFirst` / `NullsLast`
  - Un alias escrito en el nombre de una columna —`Select('users.name as n')`— se separa y se entrecomilla, también en memoria
  - `OrHaving` — una condición HAVING unida a la anterior con OR
  - `EscapeChar` en la familia LIKE — hace literal el `%` o el `_` que precede, con `ESCAPE '<c>'` en el SQL
- **Commons**: Utilidades nuevas
  - `TNullable<T>` en `Daf.Nullable`
  - `TDeferredEnumerable<T>` y `TGeneratorEnumerator<T>` en `Daf.Enumerable`
  - `Daf.Comparers` — comparadores conscientes de NULL para `TValue` y `TNullable<T>`
- **Daf.CommandLine**: Módulo nuevo — porting de `System.CommandLine` de .NET: una gramática que se declara una vez y se lee con los objetos que la declararon, una acción por comando, que se declara con `SetAction` y ejecuta `Invoke`, y la ayuda, las reglas del parse y los textos de .NET, que `CommandLine.Texts` sustituye por los de la aplicación. Un error de parse sale con 2, no con 1; las demás divergencias, en su README
- **Daf.Linq**: Módulo nuevo — el equivalente de LINQ-to-Objects: secuencias perezosas sobre `System.IEnumerable<T>`, con `TQuery<T>` y `Linq.From<T>`
- **Daf.Specification**: Módulo nuevo — el patrón Specification: `TSpecification<T>` reúne filtro, orden y paginación en una regla reutilizable y afirmable sin base de datos, con `IRepository<T>` como puerto
- **SQLCute.InMemory**: Módulo nuevo — segundo back-end: `TInMemory` ejecuta un `IQuery` sobre colecciones cargadas —JOINs, GROUP BY, subconsultas, operaciones de conjunto— con semántica ANSI de NULL
- **SQLCute.Specification**: Módulo nuevo — baja una `Specification` entera a un `IQuery`, así que la misma regla va a la base de datos o a una colección cargada
- **Daf.Web**: Paquete nuevo — servidor HTTP ligero sobre Indy y WebBroker, con controladores enrutados por `[WebRoute]` y construidos por el contenedor. El paquete **no enlaza DataSnap ni DBX**
- **Samples**: `QuickStart`, `ServiceWiring` y `PackageInit` (Cli); `HostQuickStart`, `StartupReport` y `HostLifetime` (Hosting)
- **Samples**: `LinqPipeline` (Linq), `SpecAuthoring` (Specification), `CustomProvider` (Logging), `NNLogTargets` (NNLog), `MiniApi` (web) y `FmxQuickStart` (FMX)
- **Docs**: «Composición del arranque», el AST público de `SQLCute.Abstractions`, y el criterio de reparto de los paquetes de logging (EN/ES)
- **Commons**: `TVersionInfo.IsEmpty` e `IsSemVer` — «no se fijó versión», «el tag no se entendió» y «la versión es esta» dejan de compartir una sola representación que parecía una versión válida
- **Commons**: `VersionTag` toma una plantilla de marcadores `${...}`, con `VersionTemplate` y `VersionTemplateWithBuild` como sus dos plantillas por defecto. La lista de marcadores, en el GUIDE
- **Commons**: `TMacrosList.MissingMacroPolicy` y `OnMissingMacro` deciden en qué se convierte una referencia a una macro que no está registrada: se queda como está escrita, toma un valor de reserva o lanza `EMacroNotFound`. Sin configurar nada sigue resolviéndose como vacía
- **Commons**: `TShutdownHook.RequestShutdown` — pedir la parada como lo hace Ctrl+C. Una petición hecha sin ningún host escuchando la recibe el siguiente que arranca
- **SystemProcess**: `TProcessResult.TStatus.Killed` — un `Kill` explícito y un plazo vencido dejan de compartir el valor `Timeout`, así que un resultado dice cuál de los dos cortó la ejecución
- **SystemProcess**: `StandardOutput` y `StandardError` dan a cada flujo del hijo su destino, un `TProcessOutput`: capturado, heredado del padre, descartado o escrito en un fichero
- **SystemProcess**: `StandardOutputEncoding` y `StandardErrorEncoding` — la codificación de lo capturado
- **SystemProcess**: `Id` — el identificador que el sistema dio al hijo, desde que se lanza
- **Linq**: `Linq.FromOwned` — consultar una lista temporal que nadie más guarda: Linq la libera con la consulta, y cada objeto que devuelve un terminal pasa al llamante
- **Linq**: `MoveToList` y `MoveToArray` — terminales que sacan de la lista fuente los elementos seleccionados al entregarlos, así que cada objeto conserva un solo dueño

### Changed

- ⚠️ **DependencyInjection**: Soltar un ámbito **destruye los servicios Scoped que creó**. Antes los retenía hasta cerrarse, así que un ámbito por petición acumulaba una instancia por petición. **Rompe** a quien guardase una referencia a un Scoped sin sostenerla
- ⚠️ **DependencyInjection**: Un registro repetido **pasa a dar dos instancias**; pedirlos todos devolvía la primera dos veces. Resolver uno solo y el orden de cierre no cambian
- ⚠️ **DependencyInjection**: `CanResolve` sobre un contenedor cerrado lanza `EContainerShutDownError`, como resolver; antes contestaba como si siguiera abierto. **Rompe** a quien le preguntaba después de `ShutDown`
- ⚠️ **DependencyInjection**: Una clase cuyos constructores el contenedor no puede satisfacer falla con `EServiceProviderError` nombrando el parámetro, en vez de construirse con el constructor de un ancestro y la dependencia a `nil`. **Rompe** un registro que solo funcionaba gracias a esa caída
- ⚠️ **DependencyInjection**: `[ActivatorUtilitiesConstructor]` solo cuenta en `TActivatorUtilities.CreateInstance`, donde el constructor marcado es el único candidato; el contenedor lo ignora, como en .NET. **Rompe** un registro que contaba con que el contenedor eligiera el constructor marcado
- ⚠️ **DependencyInjection**: Dos constructores que compiten hacen fallar la elección con `EServiceProviderError` nombrando los dos, como en .NET; antes ganaba el primero de los más largos. **Rompe** una clase con dos constructores que el contenedor puede satisfacer y que no se contienen
- ⚠️ **DependencyInjection**: Un Singleton que resuelve un Scoped recibe, sin `ValidateScopes`, la instancia del proveedor raíz, como en .NET; con ella, la resolución falla con `EServiceProviderError` en cualquier momento, también por un proveedor que el Singleton guardó. Antes lanzaba una `Exception` mientras el Singleton se construía. **Rompe** a quien capturaba esa `Exception`
- ⚠️ **Logging**: Una línea que ningún destino aceptaría **deja de componerse**: el nivel se decide antes de analizar la plantilla. `ILogger` gana `IsEnabled`, y eso **rompe** a quien implemente el contrato sin heredar de `TLogger`
- ⚠️ **Logging**: El `Message` de una entrada **deja de llevar el ámbito**, que viaja solo en `TLogEntry.Scope`. **Rompe, sin error de compilación,** a un proveedor propio que escriba `Entry.Message`: deja de escribir los ámbitos hasta que escriba también `Entry.Scope`
- ⚠️ **SQLCute**: Una consulta **declara** qué sentencia es —`IQuery.Kind`, con `Select` por defecto— en vez de deducirse de sus cláusulas; declarar dos se queda con el último, donde antes ganaba el primero. El miembro nuevo **rompe** a quien implemente `IQuery` por su cuenta
- ⚠️ **SQLCute**: Los métodos protegidos de `TAnsiSqlCompiler` que emiten SQL reciben la compilación, un `TSqlCompilation`, como primer parámetro, y escriben cada marcador como `?`; `FormatPlaceholder(Index)` sustituye a `ParamPlaceholder`. **Rompe** un compilador derivado de `TAnsiSqlCompiler` que sobreescriba alguno
- ⚠️ **SQLCute**: `Having`, `WhereColumns` y `OrWhereColumns` rechazan `in` y `not in` con `EArgumentException`, como SqlKata; antes emitían `=`. En el AST, el `Op` de `TJoinClause` y `TDateWhereClause` es un `TWhereOp`. **Rompe** a quien pasaba `in` ahí, y a quien lee ese `Op` como texto
- ⚠️ **SQLCute**: `Where`, `OrWhere`, `WhereNot`, `OrWhereNot` y `Having` con el valor `Null` emiten `IS NULL` con `=` e `IS NOT NULL` con cualquier otro operador, sin enlace, como SqlKata; antes ligaban `Null` a `col = ?`, que no casa con ninguna fila. **Rompe** a quien contaba ese enlace
- ⚠️ **SQLCute**: `WhereTrue`, `WhereFalse`, y `Where`, `OrWhere`, `WhereNot` y `OrWhereNot` con un valor lógico comparan con el literal del dialecto —`true`, `1`, `cast(1 as bit)`— sin enlace, como SqlKata; antes ligaban el valor. Con un operador distinto de `=`, la comparación se niega. **Rompe** a quien contaba o tipaba ese enlace, y a Oracle anterior a 23
- ⚠️ **SQLCute**: La columna de `Having` se entrecomilla como cualquier otra, como SqlKata: nombra una columna agrupada o un alias de salida, y un agregado va por `HavingRaw`. **Rompe** `Having('count(*)', …)` en los dialectos que entrecomillan, donde emite `"count(*)"`
- ⚠️ **MiniSpec**: `[Inject]` pasa a llamarse **`[MiniInject]`** y viaja en la facade, así que `uses Daf.MiniSpec` basta para marcar
- ⚠️ **MiniSpec**: `TInjectorService` deja de admitir servicios sueltos: es una instancia de la ejecución con los tres ámbitos, `Register(Scope, Instance)`. El colaborador que necesite un escenario se construye en el World
- ⚠️ **MiniSpec**: Cuando más de un contexto satisface un `[MiniInject]`, gana el más cercano al step —escenario, feature, suite—, y una coincidencia exacta gana a cualquier descendiente
- **DependencyInjection**: Un tipo de servicio ya no necesita estar visible a la reflexión para resolverse: antes se registraba sin protestar y reventaba al resolver, lejos de su causa
- **Commons**: El módulo pasa a contener **solo lo que extiende la biblioteca estándar**
  - `Daf.SystemProcess` sale al paquete nuevo del mismo nombre, sin cambiar de unidad ni de tipos
  - `Daf.CmdLn.Parser` sale por el mismo criterio, y no continúa: la línea de comandos es `Daf.CommandLine`, con otro API
  - `TExpresion<T>` pasa a llamarse **`TDeferred<T>`** y se muda a `Daf.Types`; `TMacroExpression` sigue siendo el alias
  - `TFilterSpec<T>`, el filtro por clausuras, pasa a su propia unidad `Daf.FilterSpec`
  - `TSmartArray<T>.Select` toma `TFunc<T, TTarget>` en lugar de `TDeferred<TMapper<T, TTarget>>`
  - `Daf.Enumerable.ToArray` pasa de O(n²) a O(n)
- **SQLCute**: `Daf.SQLCute.Compilers` desaparece: los seis compiladores de dialecto se integran en `Daf.SQLCute`. Las unidades no cambian; quien lo referenciara en un `requires` o un `DCCReference` debe usar `Daf.SQLCute`
- **SQLCute**: `CompileWhere` y `CompileHaving` emiten paréntesis explícitos de precedencia
- **Hosting**: `THost.Start` y `THost.Stop` informan por el pipeline de `ILogger` —el canal de arranque queda de respaldo—, y los informes de DAF salen bajo la categoría de la clase que informa en vez de bajo `DAF`
- ⚠️ **Hosting**: Los `IHostedService` se paran en el **orden inverso al de su registro**, el mismo en que libera el contenedor: un servicio arrancado después puede estar usando lo que suelta al parar uno arrancado antes. Antes se paraban en orden de registro
- ⚠️ **Hosting**: El host carga `appsettings.json`, `appsettings.{Entorno}.json`, `{ApplicationName}.settings.json` y `{ApplicationName}.settings.{Entorno}.json` del content root, todos opcionales, y todas las variables de entorno, antes de la línea de comandos, como `Host.CreateApplicationBuilder`. **Rompe** a quien contara con que la configuración solo tuviera lo que añadió: una variable como `Db__Server` pasa a pisar `Db:Server`
- ⚠️ **Hosting**: Los ajustes del host toman los nombres de .NET: `environment`, `applicationName` y `contentRoot`, como argumento —`--environment Staging`— o como variable `DAF_` sin el prefijo —`DAF_ENVIRONMENT`—. `THostDefaults` sustituye a `TDafEnvVars`, y sin `contentRoot` el content root es el directorio actual, salvo en el directorio del sistema, donde arranca un servicio de Windows. **Rompe**, en silencio, a quien fije `DAF_APP_ENV`, `DAF_APP_NAME` o `DAF_CONTENT_ROOT`, que dejan de leerse, y a quien arranque la aplicación desde otro directorio contando con el `appsettings.json` junto al ejecutable
- ⚠️ **Web**: `TDafHttpServer.Start` sobre un puerto ocupado **lanza una excepción que nombra el puerto**, después del evento `OnError`. Antes terminaba con `Abort`: un `except on EAbort` alrededor de `Start` deja de capturarla
- **Logging**: Componer el pipeline sin un solo proveedor **falla al construir el host**, con `ELoggingCompositionError`, en vez de entregar un `ILogger` que acepta todo y no escribe nada. El silencio deliberado se declara con `AddNullProvider`
- **Samples**: Cada sample se archiva bajo el módulo que declara lo que usa: `ConsoleSample` a `src/Cli/samples/`, `TicTacToeApp` a `src/VCL/samples/`, y `HostedService` a `src/samples/`

- **MediatR**: La clase base de un handler de notificación es `TNotificationHandler<T>`. La grafía anterior, `TNotificacionHandler<T>`, sigue compilando

- ⚠️ **NNLog**: Un destino implementa `DoWrite` en vez de sobrescribir `Write`. `TTarget.Write` toma una sección crítica de esa instancia de destino y lo llama, así que las escrituras de varios hilos por un mismo destino dejan de solaparse — todos los loggers de un proceso comparten las mismas instancias de destino. **Rompe** a quien escribiera un destino propio
- ⚠️ **NNLog**: Un destino que no puede escribir deja de lanzar hacia quien registró; informa por el canal de arranque, nombrando el destino, una vez por racha de fallos, y cuando vuelve a escribir informa cuántas líneas perdió; un destino de fichero que no puede archivar su fichero sigue escribiendo en él, lo vuelve a intentar en la línea siguiente y lo informa una vez por racha. **Cambio observable**: un fallo del log deja de tumbar al código que solo quería registrar
- ⚠️ **NNLog**: `${message}` deja de llevar el ámbito, y `${scope}` es el único token que lo escribe. **Rompe, sin error de compilación,** un layout con `${message}` y sin `${scope}`, el de por defecto incluido: deja de mostrar los ámbitos. Uno con los dos deja de escribirlos dos veces
- **NNLog**: Un token de layout sin renderer detrás se queda sin sustituir en vez de convertirse en cadena vacía, así que un `${mesage}` mal escrito sale en la línea
- ⚠️ **Commons**: el flag `WithCompiledMeta` de `VersionTag` pasa a ser un parámetro de plantilla, y `TVersionInfo.Debug` es de solo lectura y lo declara el binario —un cuarto argumento de `[Version(...)]` o de `TVersionInfo.Create`, `False` si se omite—; antes informaba de cómo se compiló el propio DAF. **Rompe** a quien pasara el flag o asignara `Debug`
- ⚠️ **Commons**: `Cancel` ejecuta los callbacks **del último registrado al primero**, como .NET, y lo que lancen sale junto en una `EAggregateException` cuando han corrido todos, también si lanzó uno solo. **Rompe** a quien contara con el orden de registro, o capturase alrededor de `Cancel` la excepción propia de un callback
- ⚠️ **SystemProcess**: Un flujo sin destino declarado **hereda el del padre**, así que un hijo lanzado sin manejadores escribe donde escribe el proceso que lo lanza. `TProcessOutput.Discard` en los dos flujos lo calla
- ⚠️ **SystemProcess**: Un segundo destino para el mismo flujo —`OnStdOut` dos veces, u `OnStdOut` y `StandardOutput`— **lanza `EInvalidOpException`**; antes sustituía al primero
- ⚠️ **SystemProcess**: `HideWindow(False)`, el valor por defecto, **ya no abre una consola propia**: el hijo corre en la consola del padre
- ⚠️ **SystemProcess**: Un hijo que comparte la consola del padre **recibe su Ctrl+C**, y termina con él salvo que lo atienda, como en .NET. **Rompe** un hijo que tuviera que sobrevivir al Ctrl+C del usuario: se lanza con `HideWindow(True)`
- ⚠️ **SystemProcess**: `ExecuteAsync` **lanza al hijo antes de volver**, en el hilo de quien lo llama, y solo la espera va a la tarea. El `OnFailed` de un lanzamiento que falla corre en el hilo de quien llamó, ya no en la tarea
- ⚠️ **SystemProcess**: `Kill` sin un hijo lanzado —antes de ejecutar, o tras un lanzamiento que falló— **lanza `EInvalidOpException`**, como `Id`. **Rompe** un gancho de parada que mata sin mirar: se pregunta antes por `IsRunning`
- **SystemProcess**: Un hijo al que se mata —con `Kill`, el plazo o la cancelación— sale con el código `-1` (`$FFFFFFFF`), como en .NET, y `TProcessResult.ExitCode` lleva el código real del hijo siempre que terminó; antes era 0
- ⚠️ **SystemProcess**: Con plazo o con token, un descendiente que mantiene abierto un flujo capturado más allá de ellos termina la ejecución como `Timeout` o `Canceled`, con lo ya leído entregado; antes se le esperaba, y la ejecución volvía como `Completed`

### Deprecated

- **MediatR**: `TNotificacionHandler<T>`, en favor de `TNotificationHandler<T>`. Es una subclase suya, así que lo que ya hereda de ella sigue funcionando, y se va en el próximo major
- **SQLCute**: La `RegisterIQueryFactory` suelta, en favor de `TQuery.RegisterFactory`. Reenvía, así que las dos son indistinguibles, y se va en el próximo major

### Removed

- ⚠️ **Hosting**: `IHostBuilder`, y con él el estilo diferido: los callbacks `ConfigureServices`, `ConfigureAppConfiguration` y `ConfigureHostConfiguration`, más `TConfigureServicesAction`. Queda un solo estilo, el inmediato; el punto de entrada es `THost.CreateBuilder` o la variante de la familia
- ⚠️ **Application**: El paquete `Daf.Application`, con `TDafApplication`, `TDafApplicationBuilder` e `IDafApplication`. El relevo es la variante de la familia de aplicaciones —`TCliApplication`, `TWebApplication`, `TVclApplication`, `TFmxApplication`— y `MIGRATION-2.0.es.md` dice dónde fue a parar cada miembro. `TVersionInfo` y `[Version]` se mudan a `Daf.Commons`
- ⚠️ **DependencyInjection**: `AddModules(Package)`, el descubrimiento por RTTI de los `IAppModule`. El sustituto es `AddModule<T>`, una llamada por módulo y en el orden que la aplicación quiera; `IAppModule` y `TAppModule` no cambian
- **Logging**: La global `DAFInternalLogger`, sustituida por `BootstrapLog.LoggerFor(<categoría>)`
- ⚠️ **Configuration**: `csoReloadOnChange`, un valor de `TConfigurationSourceOption` que ningún proveedor leyó nunca. `csoOptional` no cambia
- **Hosting**: Restos del estilo fluido y del andamiaje del builder
  - `THostBuilder.AddHostedService<T>` — la vía es `Builder.Services.AddHostedService<T>`; la fachada `Hosting.AddHostedService(Services, TClass)` **se queda**
  - `UseProperty` — `Properties`, en `IHostApplicationBuilder`, hace lo mismo
  - `TSingleton<T>`, base de `THostEnvironment`, `THostBuilderContext` y `THostBuilder`, que pasan a derivar de `TInterfacedObject`
- **Commons**: Retirados
  - `TGenericRTTI` — su `MakeGenericType` prometía fabricar un genérico en ejecución, que es justo lo que Delphi no hace. Lo aprovechable vuelve como `_T.GenericArgs`
  - `&If.Exp<T>`, el ternario escrito como función: evaluaba las **dos** ramas, así que no cortocircuitaba. El operador ternario de Delphi 13 lo sustituye
  - `TBooleanExpresion`
  - La unidad `Daf.Expression`, disuelta en `Daf.FilterSpec` y `Daf.Types`
- ⚠️ **Commons**: `CreateLinkedCancellationTokenSource` y `TLinkedCancellationSource`. **Rompe** a quien la llamara: `MIGRATION-2.0.md` enseña cómo enlazar una fuente a varios tokens
- ⚠️ **SystemProcess**: `KillAfterTimeout`, de `ISystemProcess` y del builder. El plazo mata siempre, que es lo único que el flag llegó a hacer: `Execute` es dueño de la ejecución desde las tuberías hasta el desmontaje, así que es él quien lo hace valer. **Rompe** a quien llamara a `.KillAfterTimeout(...)`

### Fixed

- **DependencyInjection**: Correcciones del contenedor
  - Cerrar un ámbito soltaba **una referencia de más por cada servicio que había creado**, así que el servicio se destruía mientras su titular lo seguía usando
  - Resolver desde varios hilos entrega **una sola instancia por registro** y su constructor corre una vez; antes corría una vez por hilo y la mayoría fallaban con `EListError`. Componer la colección sigue siendo de un solo hilo
  - Un ciclo que el contenedor no puede romper **deja de matar el proceso**: la resolución falla nombrando el camino, en vez de agotar la pila sin excepción ni mensaje
  - Resolver después de `ShutDown` lanza `EContainerShutDownError` nombrando el servicio, en vez de `EAccessViolation`. Cerrar dos veces deja de romper
  - `Lazy.From<T>` construye su servicio una sola vez aunque varios hilos lleguen a la vez —ocho hilos construían ocho—, y una factoría que no devuelve nada falla nombrando el contrato en vez de contestar el cero del tipo de retorno
  - `Lazy.From<T>` libera lo que construyó junto con el lazy: cada llamada a través de él retenía una referencia más, y el servicio no se destruía nunca
  - Una factory que devuelve un objeto que no implementa su servicio falla con `EIntfCastError` nombrando los dos, y una que devuelve `nil` entrega `nil`, como en .NET: el propio mensaje de error fallaba, y `nil` era un `EAccessViolation`
- **MiniSpec**: Correcciones
  - La marca de inyección funciona **sobre un campo**: el barrido solo recorría propiedades, así que el campo se quedaba en `nil` sin aviso
  - `UseSuiteContext<T>` construye el contexto de suite: `SpecContext.SuiteContext` valía `nil` en todos los steps
  - Una feature que falla antes de sus escenarios sale en el informe y pone la suite roja; antes desaparecía y el proceso salía con 0
  - Un genérico en la descripción de un step —`AsType<T>`, `TList<T>`— se leía como placeholder de Scenario Outline y fallaba con `T not found in World`
  - El fallo de una excepción que escapa de un step When nombraba un API que no existe; ahora nombra `ExpectException(Raised)`
  - El informe JUnit da el mensaje de fallo de cada escenario que falla —salía siempre vacío— y un escenario que lanzó sale como fallido en vez de como pasado
  - Una fila de Scenario Outline que falla dice qué step falló y por qué, en los informes de consola, JSON, gherkin-results y live; solo enseñaba su aspa
  - Un paso de Background que falla hace fallar el escenario que prepara, que ya no se ejecuta; antes el escenario se ejecutaba y podía pasar
  - Un step Pending o Undefined de un Background deja el escenario Pending o Undefined, sin ejecutar sus steps: el escenario se ejecutaba, y podía contar como aprobado
  - Cada Background se ejecuta una vez por escenario: el de la feature se ejecutaba dos veces, y el de una regla sustituía al de la feature en todos sus escenarios
  - Un step declarado `.Pending` en un Scenario Outline queda pendiente en cada fila: las filas lo ejecutaban
  - Un Scenario Outline sin `.Examples` se ejecuta una vez como un escenario: se quedaba fuera de la ejecución y del informe
  - Un hook de la suite que lanza cuenta como fallo y sale en el informe, y un `Before` que lanza impide que se ejecuten las features: el error se perdía y la ejecución salía con 0
  - JUnit da un escenario Undefined como fallo y uno Pending como omitido, no lista ni un escenario filtrado ni un hook que pasó, y da un hook que lanzó como un caso fallido: Undefined y Pending salían como aprobados, y el hook que fallaba no aparecía
  - El informe JSON da a un escenario Pending o Undefined su propio estado, un `status` a cada step, y `undefinedCount` entre sus totales: Pending y Undefined salían como `fail`, y un step saltado con `success`
- **Hosting**: `IHostApplicationLifetime.StopApplication` para de verdad la aplicación: cancelaba `ApplicationStopping` en vez de pedir la parada, así que `WaitForShutdown` seguía bloqueado y los `IHostedService` no se paraban nunca
- **Hosting**: Un proceso puede construir más de un host: «Build already called» y el «ya parado» eran estado de clase, así que un arnés que monta y desmonta hosts solo podía montar el primero
- **Hosting**: Un servicio alojado cuyo `Stop` lanzaba dejaba sin parar a los que venían detrás, y el host sin liberar, contenedor incluido. La parada llega ahora hasta el final, y `Host.Stop` lanza lo que falló con el host ya liberado: la misma excepción, o una `EAggregateException` con todos los fallos si fallaron varios pasos
- **Hosting**: Liberar un host que no se había parado lo liberaba dos veces, con `EInvalidPointer`. Ahora lo para y lo libera una vez
- **Hosting**: un callback del ciclo de vida que lanzaba —en `ApplicationStarted`, `ApplicationStopping` o `ApplicationStopped`— hacía lanzar a `Start`, `Stop` o `StopApplication`; ahora se informa y el host sigue, como en .NET
- **NNLog**: `enabled` y `name` de una regla se ignoraban en silencio: `enabled` ausente vale **activada** y `name` filtra de verdad. Solo el literal `true`, con cualquier caja, mantiene activada una regla; cualquier otro valor la desactiva. **Cambio observable**: un config con alguna regla de `name` distinto de `*` deja de recibirlo todo
- **NNLog**: `TFileTarget` podía quedar en un estado donde toda escritura posterior fallaba el resto del proceso: una apertura fallida dejaba la ruta fijada sin stream, y la guarda ya no volvía a abrir. Ahora reintenta
- **NNLog**: El nombre del fichero rotado doblaba el punto antes de la extensión (`app_<marca>..log`) y llevaba el instante de rotar en vez de la fecha del contenido que archiva
- **NNLog**: Comprimir el archivo o limpiar el histórico ya no cuesta la línea que provocó la rotación, ni deja el destino inutilizable cuando falla
- **NNLog**: Con `${pid}` en su `FileName`, `TFileTarget` dejaba para siempre en la carpeta el fichero de cada ejecución terminada. Ahora esos ficheros pasan a `history` cuando vence su periodo de rotación
- **NNLog**: Limpiar el histórico contaba y borraba todos los ficheros de `history`, así que los destinos que comparten carpeta se borraban los archivos unos a otros. `MaxArchivedFiles` cuenta ahora solo los archivos del propio destino, de todas sus ejecuciones
- **NNLog**: Una regla construida en código llegaba desactivada, con su nivel máximo en el más bajo de todos, y reventaba al emparejar una categoría. `TLogRule.Create` fija ahora los mismos valores por defecto que escribe el cargador
- **MediatR**: Componer dos veces en un mismo proceso da el mismo resultado; la contabilidad de paquetes ya escaneados era estado global de unit
- **SQLCute**: `WhereIn` con un conjunto de valores **vacío** emitía `col IN ()`, inválido en casi cualquier motor; ahora no casa ninguna fila y `WhereNotIn` las casa todas. Afecta también a `OrWhereIn` / `OrWhereNotIn`
- **SQLCute**: Una instancia de compilador se puede reutilizar y compartir entre hilos: a través de `IQueryCompiler`, los compiladores de Postgres, SQL Server y Oracle numeraban desde donde había acabado la compilación anterior, y dos compilaciones a la vez mezclaban sus enlaces. Los `?` de `FromRaw`, `WithRaw` y las demás formas en bruto se numeran con los demás; `\?` escribe uno literal
- **SQLCute**: Un dialecto que entrecomilla entrecomilla por separado cada parte de un nombre cualificado —`"public"."users"`, `[users].[id]`— en vez del nombre entero con su punto, deja sin comillas un `*` final y duplica dentro de un nombre su carácter de cierre. Oracle escribe sus alias sin `AS`, que rechaza delante del alias de una tabla
- **SQLCute**: Un LIKE insensible a mayúsculas solo pasaba a minúsculas las letras ASCII del patrón, así que `WhereStarts('nombre', 'ÑAN')` enlazaba `'Ñan%'` y no casaba con ningún `Ñandú`; ahora el patrón se pliega en todo Unicode
- **Commons**: `TVersionInfo.Tag` estaba **siempre vacío**, y `GetFrom` sobre una clase sin atributo devolvía lo que hubiera en la memoria del destino
- **Configuration**: El enlace ya no escribe un miembro cuya clave no menciona ninguna fuente: un defecto puesto en el constructor sobrevive al enlace, y un segundo enlace no borra lo que trajo el primero. Una clave presente con valor vacío se sigue escribiendo
- **Configuration**: Una clave encadenada cuyo valor **es** la cadena vacía ya no desaparece al atravesar la cadena: `TChainedConfigurationProvider` pregunta la presencia en vez de deducirla del valor
- **Commons**: `TMacrosList.GetValueFromIndex` devolvía cadena vacía en toda lectura de macro
- **Docs**: El arranque que enseñaba la documentación **no compilaba**, y la guía de Hosting seguía prometiendo el descubrimiento automático de los `IAppModule`. Ahora enseña `AddModule<T>`
- **Docs**: Los ejemplos de Hosting esperaban la señal de parada y no llamaban nunca a `Host.Stop`: copiados tal cual, los servicios alojados no se paraban y el contenedor no se liberaba. Ahora paran el host en un `finally`
- **Docs**: El ejemplo de Scenario Outline de la guía de MiniSpec en inglés comparaba el resultado consigo mismo, así que copiado tal cual no podía fallar nunca
- **Commons**: un tag de versión que no es SemVer se informaba como `0.0.0`; ahora el texto declarado sobrevive
- **Commons**: un tag con sus propios metadatos de build producía dos secciones `+`, que SemVer no admite; lo que declara el tag y lo que vio el compilador comparten ya la única sección
- **Commons**: un callback que lanzaba durante `Cancel` dejaba sin ejecutar los siguientes, y cancelar otra vez volvía a ejecutar los anteriores
- **Commons**: liberar una fuente de `CreateCancellationTokenSourceWithTimeout` antes de su plazo dejaba vivo su hilo del temporizador hasta el plazo
- **SystemProcess**: un `Kill` explícito se informaba como `Timeout`, y mataba al hijo dos veces —una al pedirlo y otra cuando el bucle de espera se enteraba—. Ahora informa `Killed` y mata una vez
- **SystemProcess**: un hijo que escribía más de 4 KB en un flujo sin manejador se bloqueaba hasta su plazo y acababa como `Timeout`; sin plazo, `Execute` no volvía nunca
- **SystemProcess**: el texto capturado cambiaba los caracteres no ASCII por otros —una `ñ` de `cmd` llegaba como `¤`— y rompía un carácter partido entre dos lecturas; se decodifica con la página de la consola en que corre el hijo
- **SystemProcess**: `ExecuteAsyncAndFree` no llegaba a lanzar al hijo, ni llamaba a `OnCompleted` ni a `OnFailed`. Ahora ejecuta al hijo entero antes de liberarlo
- **SystemProcess**: `ExecuteAndFree` y `ExecuteAsyncAndFree` perdían su builder en cada llamada
- **SystemProcess**: Un `ExecuteAsync` cuyo `ISystemProcess` se soltaba antes de que terminara el hijo seguía usando la instancia liberada, y sus notificaciones no llegaban
- **SystemProcess**: Matar a un hijo que acababa de terminar lanzaba `EOSError`; un `Kill` sin hijo vivo cortaba la ejecución siguiente sin matar a nadie, y después del final mataba por un identificador que podía ser de otro proceso
- **SystemProcess**: El hijo de un lanzamiento podía heredar la tubería o el fichero de otro hecho a la vez, y retenerlos mientras vivía
- **SystemProcess**: El plazo y la cancelación no acotaban la espera de lo capturado, así que un descendiente que heredaba la tubería retenía `Execute` mientras vivía
- **SystemProcess**: `OnCompleted`, `OnKilled` y `OnCancelled` podían correr antes de que los manejadores de captura recibieran lo último de la salida
- **SystemProcess**: Un manejador de captura que lanzaba dejaba su tubería sin leer: el hijo se bloqueaba escribiendo hasta que el plazo lo mataba, y `Execute` lanzaba una `EAggregateException`. Ahora la ejecución se aborta en el acto y lanza la excepción del manejador
- **SystemProcess**: Un `OnIdle` que lanzaba retenía `Execute` hasta que el hijo cerraba sus flujos capturados, mientras el hijo viviera
- **SystemProcess**: `TProcess.HasExited` respondía `True` para un proceso vivo que no podía abrir con todos los permisos de consulta, y `False` para siempre para uno que salió con el código 259
- **Web**: Un segmento literal de ruta se prueba antes que un parámetro en la misma posición; antes ganaba la acción declarada primero, así que `/items/search` podía caer en `/items/{Id}`
- **Web**: Un parámetro `[FromServices]` recibe su servicio. Con el servicio registrado la petición fallaba con `EInvalidCast`, y sin él la acción recibía `nil` sin aviso; ese caso hace fallar ahora la petición nombrando el servicio
- **Web**: Destruir un `TDafHttpServer` libera los módulos web que WebBroker guardaba para reutilizarlos; un servidor posterior en el mismo proceso recibía los que se construyeron para el anterior
- **Web**: `Ok(Boolean)` ya no pierde el valor JSON que serializa, uno por respuesta
- **NNLog**: En NTFS, el destino de fichero ya no deja de rotar tras su primera rotación, y un archivo del mismo periodo en `history`, en claro o comprimido, recibe el fichero al final en vez de sobrescribirse
- **NNLog**: Cambiar en disco la fecha de creación del fichero que un destino de fichero tiene abierto ya no lo hace rotar

### Security

- **SQLCute**: `Where(col, 'in', V)` y `'not in'`, también por `OrWhere`, `WhereNot` y `OrWhereNot`, ligan sus valores como `WhereIn` en vez de escribir `V` en el SQL. Una cadena con una lista (`'1,2,3'`) es ahora un solo valor
- **SQLCute**: El operador de `WhereTime`, `OrWhereTime` y de una unión por columnas se valida contra la lista que acepta `Where`, y lo que se emite es el operador reconocido; antes se escribía en el SQL tal como llegaba
- **Web**: El router sirve ficheros estáticos solo cuando se declara `RootDirectory`, y solo de dentro de ella. Sin raíz servía cualquier fichero del directorio de trabajo, `appsettings.json` incluido; con ella, `..` y las rutas absolutas salían fuera

## [1.6.0] - 2026-07-20

### Added

- **MediatR**: Pipeline behaviors — `TPipelineBehavior<TRequest, TResponse>` con descubrimiento RTTI automático; soporte para behaviors globales y específicos por tipo de request
- **MediatR**: `TNext<TResponse>` y `TNextValue` — delegate tipado para invocar el siguiente eslabón de la cadena de behaviors
- **MediatR**: Sample rediseñado demostrando los cuatro conceptos clave (handler, behavior de logging, behavior de validación, pipeline completo)
- **SQLCute**: Módulo completo de query builder SQL fluent — `IQuery`, `TAnsiSqlCompiler`, soporte para WHERE, JOINs, GROUP BY/HAVING, UNION, CTE, subqueries, EXISTS, DML (INSERT/UPDATE/DELETE), paginación, set operations
- **SQLCute**: `SQLCute.Abstractions` — Core package con `TQueryImpl` y compilers detrás de factory pattern
- **SQLCute**: `SQLCuteSample` — sample completo con demostración de toda la API
- **SQLCute**: Documentación bilingüe (EN/ES) integrada en el site Quarto
- **MiniSpec**: `srrSkipped` y `srrUndefined` en `TSpecRunResult` — resultado explícito para steps saltados y sin definición
- **MiniSpec**: `MarkAsSkipped` / `MarkAsUndefined` en `ISpecItem`
- **MiniSpec**: `UndefinedCount` en el runner y en el pie de resumen
- **MiniSpec**: Flag `--strict-pending`: hace que specs pendientes cuenten como fallo del proceso
- **MiniSpec**: Console reporter progresivo — Feature y Scenario se emiten al encontrarse, steps con símbolos Gherkin (`✓ ✗ ~ P ?`) al completarse

### Changed

- **MiniSpec**: Pie de resumen renombrado de `Pass/Fail/Skip` a `Passed/Failed/Pending/Undefined/Skipped`

### Fixed

- **SQLCute**: `SqlServerCompiler` no emitía la cláusula `TOP` cuando se especificaba `LIMIT` sin `OFFSET`
- **MediatR**: Closure variable capture bug en la cadena de behaviors
- **MediatR**: Descubrimiento de behaviors globales con `StrictExtensions=False`
- **MediatR**: Warnings de ocultación en jerarquía `TPipelineBehavior`
- **MiniSpec**: Step sin lambda en `ScenarioOutline` se trataba como `Undefined`; ahora es no-op (display-only) porque `ExampleInit` inyecta los valores vía RTTI
- **MiniSpec**: Tabla de ejemplos de Outline: cabecera desalineada respecto a las filas de datos
- **MiniSpec**: Features sin escenarios ejecutados aparecían igualmente al filtrar con `-f`; ahora se suprimen en todos los reporters (console, JSON, Gherkin, JUnit)
- **MiniSpec**: Pie de resumen pegado a la última línea Gherkin; ahora lleva una línea en blanco antes

## [1.5.0] - 2026-03-20

### Added

- **Docs**: Sitio de documentación Quarto bilingüe (EN/ES) para todos los módulos

### Fixed

- **MiniSpec**: Eliminar `PendingStep` muerto de todos los builders
- **MiniSpec**: Renombrar `Expect(TProc)` → `ExpectException` para resolver ambigüedad con `TValue`
- **MiniSpec**: Fixes en step bindings — lazy binding, `NoAction` lskThen, aserciones DI en Then, memory leak
- **MiniSpec**: `CmdLnParser` usa `Build(False)` para evitar double-free con destructor

## [1.4.0] - 2026-02-16

### Added

- **Samples**: TicTacToe — aplicación de ejemplo BDD con MVVM + Clean Architecture (formulario VCL con tema oscuro, ViewModel, ViewPort)
- **Samples**: TicTacToe — specs de features: Movement, NewGame, UX (8 escenarios), con etiquetas `@e2e`/`@integration`
- **Samples**: TicTacToe — notación algebraica (`a1`–`c3`), DataTables con el tablero visual, ScenarioOutline con Examples
- **MiniSpec**: Enlace de DataTable en ScenarioOutline; varios reporters en la configuración INI

### Fixed

- **MiniSpec**: Enlace diferido en ScenarioOutline; render de Rule/DataTable en el Live Dashboard

## [1.3.0] - 2026-02-02

### Added

- **MiniSpec**: Test Doubles — `Stub<T>`, `Mock<T>`, `Spy<T>` para simular interfaces
- **MiniSpec**: API fluida para stubs con `.Setup().Returns()`, `.WithArgs()`, `.Raises()`
- **MiniSpec**: Expectativas de mock: `.Expects().Once`, `.AtLeastOnce`, `.Never`, `.Exactly(N)`
- **MiniSpec**: Matchers de argumentos: `Arg.Any<T>`, `Arg.Is<T>(predicate)`, `Arg.IsNil`
- **MiniSpec**: Seguimiento con spy mediante `.Track()` e inspección del historial de llamadas
- **MiniSpec**: Método fluido `.Pending` — marcar specs como no implementadas todavía
- **MiniSpec**: Método fluido `.NoAction` — steps descriptivos sin código
- **MiniSpec**: Etiqueta reservada `@skip` — siempre excluida de la ejecución (marcar tests roscos)
- **MiniSpec**: Reporter JUnit XML para integración con CI/CD
- **MiniSpec**: SpecSuite — nodo raíz que contiene todas las Features, con título y hooks
- **MiniSpec**: `Category(Title)` — API fluida para fijar el título de la Suite
- **MiniSpec**: `Before(Description, Hook)` / `After(Description, Hook)` — hooks a nivel de Suite
- **MiniSpec**: Interfaz `ISpecSuite` e implementación `TSpecSuite`
- **MiniSpec**: Eventos de Suite: `OnBeginSuite` / `OnEndSuite` para los listeners
- **MiniSpec**: Todos los reporters muestran el título de la Suite (Console, JSON, comentario Gherkin, cabecera del Live)
- **MiniSpec**: Soporte de DataTables — tablas de datos en línea para los steps (estándar Gherkin)
- **MiniSpec**: Tipo `TDataTable` y envoltorio `TDataTableObj` con acceso a Row/Cell/Headers
- **MiniSpec**: Todos los reporters muestran las DataTables (ASCII en consola, array en JSON, pipes en Gherkin)
- **MiniSpec**: Clase base `TFeatureWorld` para los Worlds que necesitan acceso al contexto de ejecución
- **MiniSpec**: Interfaz `ISpecContext` con `CurrentStep`, `CurrentScenario`, `CurrentRule`, `CurrentFeature`, `DataTable`
- **MiniSpec**: Sistema de filtros ampliado con los prefijos `Feat:`, `Scen:`, `Rule:`, `Cat:`
- **MiniSpec**: Método `EndRule` para volver al contexto de Feature desde una Rule
- **MiniSpec**: Reconexión automática del Live Dashboard para ejecuciones nuevas
- **MiniSpec**: Exportar e imprimir desde el Live Dashboard
- **MiniSpec**: API de prueba de excepciones más limpia — `Expect(Raised).ToBe<EException>`
- **MiniSpec**: Proyecto de tests propio para el framework (`src/MiniSpec/test/`)
- **MiniSpec**: Hooks `Before`/`After` de feature, para preparar y limpiar una vez por feature

### Changed

- **MiniSpec**: El concepto `InUnit` pasa a `Category`, para agrupar features con más libertad
- **MiniSpec**: Cambian los prefijos de filtro: `F:`→`Feat:`, `S:`→`Scen:`, `R:`→`Rule:`, `U:`→`Cat:`
- **MiniSpec**: El marcador `TSourceUnit` pasa a llamarse `TUnitMarker`
- **MiniSpec**: Los reporters salen a unidades propias, para modularizar
- **MiniSpec**: Usa la interfaz `ISpecReporter` en vez de una referencia de clase
- **Samples**: `.Feature.pas` pasa a `.Feat.pas`, por la convención de nombres

### Fixed

- **MiniSpec**: Navegación al padre en el AST y eliminación del World fantasma
- **MiniSpec**: Puntero colgado en `FCliOptions` de los listeners y lógica de configuración
- **MiniSpec**: Equilibrio de creación y destrucción del World, verificado con tests

### Removed

- **MiniSpec**: Reporter HTML (lo sustituye el Live Dashboard)
- **Tools**: Proyecto MiniSpecWatcher (lo sustituye el reporter Live)

### Documentation

- **MiniSpec**: README al día con la nueva API de prueba de excepciones
- **MiniSpec**: La unidad LiveDashboard entra en la lista de ficheros del README

## [1.2.0] - 2026-01-24

### Added

- **MiniSpec**: Palabras clave And/But en los steps, para escenarios más expresivos
- **MiniSpec**: Filtrado por etiquetas con parser de expresiones (`@tag`, `not @tag`, `@a and @b`)
- **MiniSpec**: Soporte de Rule, para organizar mejor los escenarios
- **MiniSpec**: ScenarioOutline con formato de tabla agrupada
- **MiniSpec**: Estado Skip e informes ampliados
- **MiniSpec**: Aserciones Expect ampliadas
- **MiniSpec**: TGherkinReporter — genera ficheros `.feature` desde las specs
- **MiniSpec**: TLiveReporter — reporter de panel en tiempo real
- **MiniSpec**: Ayuda por reporter con `-r <reporter>:help`
- **MiniSpec**: Configuración persistente en ficheros `.cfg`
- **MiniSpec**: El logo se muestra en la salida de consola

### Changed

- **MiniSpec**: Sintaxis nueva de reporters en la línea de comandos para la v1.2.0
- **MiniSpec**: TLiveReporter pasa a ser un reporter independiente
- **MiniSpec**: Limpieza de la línea de comandos — opciones DryRun, Pause, CompletedAt
- **BDS.bat**: Al día en la v4.1.0, con la opción `--project`

### Fixed

- **MiniSpec**: Problemas de codificación UTF-8 en los reporters
- **MiniSpec**: Añadir los paquetes de Indy al requires (elimina los avisos de import implícito)
- **MiniSpec**: Usar el nombre de configuración fijo `MiniSpec.ini`

### Documentation

- **MiniSpec**: README reescrito por completo para la v1.5.0

## [1.1.0] - 2026-01-12

### Added

- Primera versión pública de DAF Community Edition
- Módulos base: Commons, Configuration, DependencyInjection, Hosting, Logging, MediatR
- Framework de tests BDD MiniSpec
- Proyectos de ejemplo: Calculator, Config, Console, HostedService, Mastermind, MediatR, Ping

[Unreleased]: https://github.com/Delphi-DAF/dafce/compare/v1.6.0...HEAD
[1.6.0]: https://github.com/Delphi-DAF/dafce/compare/v1.5.0...v1.6.0
[1.5.0]: https://github.com/Delphi-DAF/dafce/compare/v1.4.0...v1.5.0
[1.4.0]: https://github.com/Delphi-DAF/dafce/compare/v1.3.0...v1.4.0
[1.3.0]: https://github.com/Delphi-DAF/dafce/compare/v1.2.0...v1.3.0
[1.2.0]: https://github.com/Delphi-DAF/dafce/compare/v1.1.0...v1.2.0
[1.1.0]: https://github.com/Delphi-DAF/dafce/releases/tag/v1.1.0
