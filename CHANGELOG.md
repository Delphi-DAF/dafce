# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html)
and [Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/).

## [Unreleased]

Los cambios que rompen compilación están recogidos, con su diff de ejemplo, en `MIGRATION-2.0.md` (`MIGRATION-2.0.es.md` en español).

### Added

- **Logging**: `ILogger.IsEnabled(Level)` — preguntar si una entrada de ese nivel llegaría a algún destino antes de construir sus argumentos
- **Configuration**: `Daf.Configuration.CommandLine` — cuarto proveedor: los argumentos del proceso leídos como claves (`clave=valor`, `--clave=valor`, `--clave valor`, `/clave=valor`, `/clave valor`, alias corto). Cierra la paridad con `Microsoft.Extensions.Configuration`
- **Daf.CommandLine**: Módulo nuevo — porting de `System.CommandLine`: comandos, subcomandos, opciones y argumentos posicionales, cuyos valores se leen con los mismos objetos que se declararon y ya tipados (`FileOpt.ValueIn(Parsed)`). Añade posicionales, opciones recursivas y alias de comando, que al módulo anterior le faltaban
- **Daf.CommandLine**: La ayuda se genera de la gramática, así que no puede desajustarse de lo que el programa acepta. `--help` viene de serie, resuelto antes de validar
- **Daf.CommandLine**: Motor de tipos propio: un número negativo se puede pasar, y `--port 80xyz` se rechaza nombrando la opción en vez de valer `80`
- **Daf.CommandLine**: Divergencia con .NET, razonada en el README: `fileOption.ValueIn(parsed)` en vez de `result.GetValue(fileOption)`, porque una interfaz de Delphi no puede declarar un método genérico
- **Daf.CommandLine**: Sample `BuildTool` — una gramática de dos niveles con alias, argumento posicional, opción recursiva y ayuda generada; documentación bilingüe (EN/ES)
- **Daf.CommandLine**: Aridad declarada, los cinco valores de .NET: opciones con varios valores, símbolos requeridos, y repetir una opción de un solo valor pasa a ser error en vez de último-gana. También tipos enumerados y `AcceptOnly`
- **Daf.CommandLine**: `--version` de serie en el comando raíz y `--` como fin de opciones. La ayuda muestra valores por defecto, valores aceptados y qué símbolos son obligatorios
- **DependencyInjection**: `TryAddSingleton` / `TryAddScoped` / `TryAddTransient`, `Replace` y `RemoveAll` — «la librería pone el default, la aplicación manda», con la semántica de `ServiceCollectionDescriptorExtensions` de .NET
- **DependencyInjection**: `Descriptors` — los registros de un servicio, en sólo lectura: un tercero puede componer sus propias operaciones sin tocar DAF
- **DependencyInjection**: `Decorate<S>` — envuelve los registros existentes de un servicio conservando su ciclo de vida y sin duplicar su factory. Es el `Decorate` de Scrutor
- **DependencyInjection**: `AddModule<T>` y `AddModule(Module)` — registros agrupados en una clase, aplicados en el orden escrito; la sobrecarga de instancia es la única vía de pasarle configuración. Es el `RegisterModule` de Autofac
- **DependencyInjection**: `BuildServiceProvider(TServiceProviderOptions)` con `ValidateOnBuild` y `ValidateScopes`, opt-in: alcanzan a los registros por clase y reportan todos los fallos juntos. Los registros por factory quedan fuera
- **DependencyInjection**: Un contrato genérico se registra **sin fijar sus argumentos de tipo** —`AddSingleton<IRepo<_>, TRepo<_>>`— y ese registro sirve a todos sus cierres, con el ciclo de vida contando por tipo cerrado y un registro concreto ganándole siempre. Qué cubre y qué no, en el README
- **DependencyInjection**: Una factoría puede recibir el **tipo que se le está pidiendo** además del proveedor —`AddSingleton<IRepo<_>>(function(P; RequestedType) ...)`, y lo mismo en `AddScoped`, `AddTransient` y sus `TryAdd`—, así que una sola factoría construye lo que corresponde a cada cierre. Estrictamente aditivo: la de un solo parámetro se registra igual
- **DependencyInjection**: `IOptions<T>` — un servicio declara en su constructor los ajustes que necesita y los recibe construidos, validados y **con dueño**: los libera el contenedor. Los valores se ponen con `AppOptions.Configure<T>` o se leen de una sección de configuración
- **DependencyInjection**: Los ajustes se pueden validar al construirse, una sola vez, y un fallo aborta nombrando el ajuste. Divergencia con .NET, dicha en el README: pedir unos ajustes que nadie registró falla nombrando el tipo en vez de devolver un objeto con todo a cero
- **DependencyInjection**: `IContainerAccess.OwnedInstanceCount` — cuántos objetos sigue poseyendo el contenedor: los singletons más los de cada ámbito vivo, sin los transient, que no retiene. Cero después de cerrar hace comprobable «se liberó todo» sobre cualquier servicio
- **DependencyInjection**: Inyección por propiedad — `[DafInjectProperties]` en la clase y `[DafInject]` en cada propiedad, que el contenedor rellena cuando el constructor ya ha terminado. Es lo que hace posible que **dos servicios se necesiten mutuamente**: un ciclo por constructor es inconstruible, y hasta ahora la única salida era meter el proveedor dentro de uno de los dos y resolver a mano. Una propiedad marcada que no se puede rellenar aborta nombrándola, en vez de entregarse en `nil`. Viene de los contenedores completos: `PropertiesAutowired` de Autofac, `[Dependency]` de Unity
- **DependencyInjection**: `TActivatorUtilities.InjectInto(Provider, Instance)` — cablea una instancia que el contenedor no construyó, como un formulario que crea la plataforma. El contenedor **no** toma posesión: no la cuenta, cerrarlo no la destruye, y los servicios que ella sostenga viven mientras los sostenga
- **DependencyInjection**: `AddInjector<T>` — cablear por código, sin marcar nada: alcanza a la clase y a sus descendientes, corre después del constructor y de la inyección por atributo, y no usa RTTI, así que es la única vía que llega a una clase de otra librería o a una unit con la RTTI de propiedades recortada. Es el `RegisterInitializer` de Simple Injector
- **DependencyInjection**: `ValidateOnBuild` pasa a cubrir las propiedades marcadas además de los parámetros del constructor —una clase cuya única dependencia insatisfecha estaba en una propiedad se declaraba sana y se entregaba con la propiedad en `nil`— y rechaza los ciclos que el contenedor no puede deshacer, enumerando el camino completo: los que tienen alguna arista por constructor y los que pasan por un `Transient`, que se resuelven y no se pueden desmontar
- **DependencyInjection**: Servicios con clave — `AddKeyedSingleton` / `AddKeyedScoped` / `AddKeyedTransient` y sus `TryAddKeyed*`, `GetKeyedService`, `GetRequiredKeyedService`, `GetKeyedServices` y `RemoveAllKeyed`, con los nombres de `Microsoft.Extensions.DependencyInjection`. Dos implementaciones del mismo contrato distinguidas por una clave, que hasta ahora obligaban a partir el contrato en dos interfaces o a pedirlas todas y elegir por `is` en el consumidor. La clave puede ser una cadena, un entero, un booleano, un enumerado, una metaclase o un `PTypeInfo`, y dos claves son la misma solo si coinciden su tipo y su valor
- **DependencyInjection**: El espacio con clave está aislado del que no la tiene, en los dos sentidos: `GetServices` no arrastra los registros con clave, `GetRequiredService` no los alcanza, y `TryAdd*` pregunta por el par (tipo, clave), así que una clave no bloquea a otra ni al registro sin clave
- **DependencyInjection**: La clave se pide desde un constructor o desde una propiedad marcada, en el mismo `[DafInject]`. Para una clave enumerada —la forma recomendada, porque un typo en una cadena no lo ve el compilador— la aplicación declara su marca en una línea sobre `DafInjectAttribute<T>`, y el contenedor la reconoce por descendencia sin conocer ese enumerado
- **DependencyInjection**: `ServiceKey.Any` — un registro que atiende cualquier clave de su tipo de servicio que no tenga registro propio, con su factoría enterada de cuál se pidió y una instancia por clave servida. Es `KeyedService.AnyKey` de .NET
- **DependencyInjection**: `ValidateOnBuild` y `ValidateScopes` cubren los registros con clave, y todo mensaje que nombre un servicio nombra también su clave cuando la tiene
- **Application**: El sample `TicTacToeApp` recibe su ViewModel **por propiedad**: el formulario lo construye la VCL, así que se cablea con `InjectInto` desde `FormCreate`
- **Configuration**: `ConfigOptions.Bind<T>` — una sección de configuración llega tipada y con dueño a quien la necesita, y el servicio deja de nombrar su sección: la decide el arranque. Los valores son una foto tomada al construirse, no un espejo de la configuración viva; enlazar a mano con `TConfigurationBinder` sigue igual
- **NNLog**: El proveedor se puede construir con ajustes **escritos con código**, y no solo leídos de una sección
- **Logging**: `ILogger<T>` funciona: un servicio pide `ILogger<TMiServicio>` por constructor y recibe un registrador que ya lleva su propia categoría, sin declararla y sin una línea de composición por clase. El contrato estaba declarado desde hacía tiempo y no había forma de registrarlo
- **Hosting**: `IStartupTask` y `AddStartupTask<T>` — inicialización que corre una vez al arrancar, con el proveedor ya construido. `THost.Start` las ejecuta en orden de registro, antes de los `IHostedService` y de `ApplicationStarted`; si una falla, el arranque aborta
- **Hosting**: Sample `PackageInit` — un paquete que trae su propia inicialización: el ejecutable llama a su `AddXxx` y no menciona ni el tipo de la tarea ni lo que hace
- **Docs**: Página nueva «Composición del arranque» (EN/ES) — las dos fases del arranque, las dos formas de agrupar registros, la tabla de equivalencias con .NET y las divergencias justificadas
- **MiniSpec**: `--allow-undefined` — un step undefined deja de fijar el código de salida, y se sigue contando e informando igual; una suite lo puede declarar por su cuenta con `MiniSpec.AllowUndefined(True)`. Por defecto rompe, porque un step sin binding es una omisión no intencional
- **MiniSpec**: `TSpecRun` — una ejecución con suite, contexto, inyector, opciones y runner propios, que se puede lanzar dentro de otra sin alterar sus recuentos ni escribir en su informe. `Feature(Descripción, Suite)` declara la feature en esa suite en vez de en la del proceso
- **MiniSpec**: El World de un escenario queda disponible como contexto de escenario: `SpecContext.ScenarioContext` lo devuelve y un `[Inject]` de su tipo lo recibe
- **Daf.Linq**: Módulo nuevo — secuencias perezosas con conteo de referencias sobre `System.IEnumerable<T>`: `TQuery<T>`, `IGrouping<TKey, T>` y `Linq.From<T>`. Es el equivalente de LINQ-to-Objects; el nombre `IQueryable<T>` queda **reservado** para lo que significa en .NET, que DAF todavía no tiene
- **Daf.Linq**: Operadores `Where`/`Filter`, `Select`/`Map`, `Take`, `Skip`, `Zip`, `Distinct`, `OrderBy`, `GroupBy`
- **Daf.Linq**: Terminales `IsEmpty`, `Any`, `FirstOrDefault`/`LastOrDefault`, `TryFirst`, `Single`, `SingleOrDefault`, `TrySingle`, `ToArray`, `ToList`, `ToSmartArray`
- **SQLCute.InMemory**: Módulo nuevo — segundo back-end: ejecuta un `IQuery` sobre colecciones en memoria. Carril tipado sobre `TList<T>` y carril general con `IQueryContext`
- **SQLCute.InMemory**: `IRow` con adaptadores `TRttiRow<T>`, `TDictRow`, `TJoinRow`, `TNullRow`; materialización por RTTI a clase de resultado
- **SQLCute.InMemory**: Tres puntos de entrada por lo que devuelven: `TInMemory.Elements<T>` (los elementos originales), `Rows<T>` (filas dinámicas) y `Project<TSource, TResult>` (materializadas por RTTI)
- **SQLCute.InMemory**: JOINs (INNER/LEFT/RIGHT/CROSS/FULL OUTER), GROUP BY/HAVING, subconsultas, UNION/INTERSECT/EXCEPT
- **SQLCute.InMemory**: Semántica ANSI de NULL — lógica de tres valores, agregados que ignoran NULL, DISTINCT con NULLs iguales, GROUP BY con un grupo para NULL, ORDER BY con NULL como menor
- **SQLCute.InMemory**: Samples `InMemoryQuerySample` e `InMemoryJoinSample`; documentación bilingüe (EN/ES)
- **SQLCute**: `Daf.SQLCute.WhereTree` — normalizador de precedencia WHERE compartido por ambos back-ends, en el paquete `Daf.SQLCute`
- **Daf.Specification**: Módulo nuevo — condiciones de consulta escritas con operadores Delphi que siguen siendo un **árbol inspeccionable**, sin SQL y sin referenciar SQLCute ni `Daf.Linq`: `IExpr` y su juego acotado de nodos, `Prop`/`Prm`, `TEntityExpr<T>` (proxy RTTI que valida cada miembro donde se escribe) y el metamodelo opcional `TPropExpr<T>`
- **Daf.Specification**: `TSpecification<T>` — una regla reutilizable con filtro, orden y paginación, afirmable en un test sin base de datos; `IRepository<T>` como puerto, sin restricción de clase base
- **Daf.Specification**: `INameResolver` — la única costura de nombres: miembro → columna y tipo → tabla, con identidad y «nombre del tipo sin la T inicial» por defecto
- **SQLCute.Specification**: Módulo nuevo — `TExprEmitter` emite una expresión como cláusulas WHERE sobre un `IQuery`, de modo que la misma regla va a la base de datos o a una colección cargada sin traducirse dos veces
- **SQLCute.Specification**: `TInMemoryRepository<T>` — adaptador de `IRepository<T>` sobre una colección ya cargada, con caché de planes por regla; sample `CatalogServiceSample` y documentación bilingüe (EN/ES)
- **SQLCute**: `WhereNot(Callback)` y `OrWhereNot(Callback)` — grupos WHERE entre paréntesis negados como una unidad, `NOT (…)`
- **SQLCute**: Orden de NULL por columna en ORDER BY — sobrecargas `OrderBy`/`OrderByDesc` con `TNullsOrder` y modificadores `NullsFirst`/`NullsLast`. Postgres, Oracle, Firebird y SQLite lo emiten nativo; MySQL y SQL Server, que no tienen esa sintaxis, lo emulan con una clave de orden previa
- **Commons**: `TNullable<T>` en `Daf.Nullable`
- **Commons**: `TDeferredEnumerable<T>` en `Daf.Enumerable`
- **Commons**: `Daf.Comparers` — comparadores conscientes de NULL para `TValue` y `TNullable<T>`
- **Daf.Web**: Módulo nuevo en paquete — servidor HTTP ligero sobre Indy y WebBroker, controladores enrutados por atributos y construidos por el contenedor, y action results. Las fuentes ya se publicaban; faltaba el `.dpk` con el que instalarlo
- **Daf.Web**: El paquete **no enlaza DataSnap ni DBX**: el `uses` heredado de la plantilla *DataSnap WebBroker Application* arrastraba 59 unidades de remoting. Quien lo instale se lleva un servidor HTTP
- **Daf.FMX**: Módulo nuevo — lo que en DAF se apoya en FireMonkey, en un paquete que se instala y se distribuye como cualquier otro; hoy, `TPlatformLogBootstrapSink`. Ningún otro paquete de DAF lo enlaza, así que una aplicación VCL no se lo lleva dentro. No es soporte de plataforma: los targets del framework siguen siendo Win32 y Win64. Documentación bilingüe (EN/ES)
- **Logging**: Canal de arranque — un `ILogger` disponible desde que arranca el proceso, sin inyección de dependencias y sin configuración, con más de un destino. Cubre la fase anterior a `BuildServiceProvider`, donde todavía no hay nada registrado que pueda informar de un fallo
- **Logging**: La aplicación informa de su propio arranque **bajo su categoría**, con `BootstrapLog.LoggerFor('MiApp')`, la única vía de pedir un logger de arranque. Antes el canal exigía que la categoría empezara por `DAF`
- **Logging**: Destinos que no dependen de que haya un depurador conectado: la salida de error del proceso y la salida de depuración del sistema. Quien despliega añade el suyo con `BootstrapLog.AddSink`, antes de construir el host
- **Logging**: `TFileBootstrapSink`, el destino duradero, **sin registrar**: recibe la ruta de quien lo construye. DAF ofrece el mecanismo y no elige dónde escribe la aplicación
- **Logging**: `TPlatformLogBootstrapSink` en `Daf.FMX.BootstrapSink` — la salida de depuración del sistema fuera de Windows, sobre `IFMXLoggingService`. Su disponibilidad la responde la plataforma, no un `{$IFDEF}` mantenido a mano
- **Docs**: La página «Composición del arranque» (EN/ES) gana el relevo de dos etapas: quién informa antes de `BuildServiceProvider` y quién después, qué recibe cada forma de despliegue —escritorio, consola, desatendido— y el patrón del `.dpr`
- **Logging**: `Builder.AddNullProvider` y `TNullLoggerProvider` — descartar las entradas a propósito, dicho al componer: así quien no pidió silencio se puede tratar como un error
- **Docs**: La convivencia con NNLog y el criterio de reparto de los paquetes de logging (EN/ES) — por qué no hay `Daf.Logging.Console` ni `Daf.Logging.Debug`, y por qué el destino de FireMonkey tiene paquete propio
- **Daf.CommandLine**: Formas de invocación equivalentes: el valor tras `:` (`--verbosity:quiet`), un alias de un carácter pegado a su valor (`-vquiet`), opciones cortas agrupadas (`-fdx`) y el prefijo `/` de Windows. Un grupo con una letra que nadie declaró se rechaza entero, nombrándola
- **Daf.CommandLine**: Ficheros de respuesta — `@fichero.rsp` mete las líneas del fichero como tokens donde estaba el `@`. Una línea, un token; `#` comenta; un fichero puede nombrar a otro con el anidamiento acotado, y uno que no está es un error de invocación
- **Daf.CommandLine**: `AllowMultipleArgumentsPerToken` — `--items a b c` sin repetir el nombre. Es opt-in: si toda opción con sitio para varios valores fuera glotona, un posicional escrito detrás dejaría de serlo
- **Daf.CommandLine**: Comandos envoltorio — `Comando.AllowUnmatchedTokens` y `UnmatchedTokens` en el resultado, que es lo que permite escribir un `sudo -u admin apt update`. Por defecto, lo que no casa sigue siendo un error
- **Daf.CommandLine**: Tipos de serie nuevos — `Int64`, `Currency` (decimal exacto, sin redondear por su cuenta), `TDateTime`, `TGUID` y colecciones (`TArray<T>`, que derivan su aridad). Las rutas siguen siendo `string`, con la validación que aportaban `FileInfo` y `DirectoryInfo`; una fecha se lee solo en ISO 8601, divergencia declarada en el README
- **Daf.CommandLine**: Un tipo que el motor no conoce se declara junto a la opción —`CommandLine.Option<TSemVer>('--min', ParseSemVer)`— y su rechazo sale por los mismos errores de invocación que el resto. Va en el símbolo y no en un registro global de tipos
- **Daf.CommandLine**: `Validate` — validaciones propias que corren **dentro** del parseo, así que «un entero mayor que cero» falla como error de invocación y en la misma lista que los demás. Dos de serie: `AcceptExistingOnly` y `AcceptLegalFileNamesOnly`
- **Daf.CommandLine**: Un valor por defecto puede ser un cálculo y no solo una constante, evaluado solo si la invocación no trae el símbolo. La ayuda no lo ejecuta: muestra el texto que se declare junto a él
- **Daf.CommandLine**: `Hidden` — un símbolo que se declara, se parsea y se lee igual, pero no aparece en la ayuda. Si recibe un valor inválido, el error lo nombra
- **Application**: Sample `QuickStart` — el arranque mínimo: construir, registrar un servicio, ejecutar. Es la fuente del snippet de inicio rápido de la documentación, que lo cita literalmente

### Changed

- ⚠️ **DependencyInjection**: **`Replace` y `RemoveAll` alcanzan ahora únicamente los registros sin clave.** Antes no había claves, así que «todos los del tipo» y «todos los del tipo sin clave» eran lo mismo; para los otros está `RemoveAllKeyed`. **Rompe** a quien lo use para desmontar un registro en un arnés de specs: desmonta menos de lo que desmontaba, y lo descubre en verde
- ⚠️ **DependencyInjection**: **Soltar un ámbito destruye ahora los servicios Scoped que creó.** Hasta ahora el contenedor retenía todos los ámbitos que repartía y no los soltaba hasta cerrarse, así que un proceso que abre un ámbito por petición —el patrón que la guía recomienda— acumulaba una instancia por petición durante toda su vida: mil peticiones, mil instancias vivas. **Rompe** a quien guardase una referencia a un servicio Scoped sin sostenerla, porque hasta hoy sobrevivía a su ámbito
- ⚠️ **Logging**: Una línea que ningún destino aceptaría **deja de componerse**: la decisión por nivel se toma en el embudo, antes de analizar la plantilla y antes de entregar nada a un proveedor. `ILogger` gana `IsEnabled`, y eso **rompe** a quien implemente el contrato sin heredar de `TLogger` — el diff de ejemplo, en `MIGRATION-2.0.md`
- ⚠️ **DependencyInjection**: **La resolución publica la instancia antes de rellenar sus propiedades**, y el orden cambia para todo registro, participe o no en la inyección por propiedad. Es lo que hace resoluble un ciclo: quien reentre a resolver el mismo servicio recibe la instancia ya publicada. **Rompe** el caso de un constructor que resuelve su propio servicio: antes recursaba sin fondo, ahora recibe la instancia a medio construir
- ⚠️ **DependencyInjection**: **Cerrar el contenedor vacía las propiedades inyectadas de lo que posee antes de soltar nada.** Sin esa pasada, dos servicios que se inyectaron mutuamente se sostienen entre ellos, el desmontaje termina sin liberar ninguno y no se destruyen nunca
- ⚠️ **DependencyInjection**: **Un registro repetido pasa a dar dos instancias.** Pedirlos todos devolvía la primera dos veces: la segunda se declaraba, se contaba y no se construía nunca. Resolver uno solo no cambia —sigue ganando el último registrado—, ni cambia el orden de cierre
- **DependencyInjection**: Un tipo de servicio ya no necesita estar declarado donde la reflexión pueda nombrarlo para poder resolverse: antes se registraba sin protestar y reventaba al resolver, lejos de su causa
- ⚠️ **Daf.CommandLine**: Las formas nuevas **cambian cómo se leen invocaciones que hoy funcionan**, y fallan al invocar, no al compilar: `-abc` deja de ser una opción con ese nombre donde `a`, `b` y `c` son alias declarados, y un posicional que empiece por `/` o por `@` se lee como opción o como fichero de respuesta. Cada forma actúa solo si el token no nombra ya algo declarado, y `--` las devuelve todas
- **Commons**: El módulo pasa a contener **solo lo que extiende la biblioteca estándar**. `Daf.SystemProcess` sale al paquete nuevo del mismo nombre, sin cambiar de unidad ni de tipos: basta añadirlo al `requires` o al `DCCReference`. Por el mismo criterio sale `Daf.CmdLn.Parser`, y esa no continúa: la línea de comandos es `Daf.CommandLine`, con otro API
- **Hosting**: `THost.Start` y `THost.Stop` informan por el pipeline de `ILogger` en vez de por el canal de arranque, que se queda de respaldo por si `ILoggerFactory` no se puede resolver. `THostBuilder.Build` sigue en el canal
- **Hosting / Logging**: Los informes de arranque de DAF salen bajo la categoría de la clase que informa —`THostBuilder`, `TCompositeLogger`— en vez de bajo `DAF` con el nombre de la clase metido en el mensaje
- **Logging**: Componer el pipeline sin un solo proveedor **falla al construir el host**, con `ELoggingCompositionError`, en vez de entregar un `ILogger` que acepta todo y no escribe nada. El silencio deliberado se declara con `AddNullProvider`; quien no llame a `AddLogging` sigue informando por el canal de arranque
- **Logging**: Un destino del canal de arranque que no puede escribir **se declara no disponible** en lugar de aceptar el informe y perderlo, y uno que lanza queda fuera sin llevarse por delante el arranque. `BootstrapLog.HasAvailableSink` distingue que no esté escuchando nadie de haber informado
- ⚠️ **SQLCute**: Una consulta **declara** qué sentencia es —`IQuery.Kind`, con `Select` por defecto— en vez de deducirse de sus cláusulas. Declarar dos tipos en la misma consulta se queda con el último (antes ganaba el primero); una de un solo tipo emite lo mismo. El miembro nuevo **rompe** a quien implemente `IQuery` por su cuenta
- **SQLCute**: `CompileWhere` y `CompileHaving` emiten paréntesis explícitos de precedencia
- **Commons**: `TSmartArray<T>.Select` toma `TFunc<T, TTarget>` en lugar de `TDeferred<TMapper<T, TTarget>>`; `TMapper<T, U>` sigue declarado
- **Daf.Linq**: `Where` / `Filter` toman `TFilterSpec<T>`
- **Commons**: `Daf.Enumerable.ToArray` usa colector `TList<T>` (O(n) en lugar de O(n²))
- **SQLCute**: El módulo pasa de **5 paquetes a 4**: `Daf.SQLCute.Compilers` desaparece y sus seis compiladores de dialecto se integran en `Daf.SQLCute`, que ya tenía el ANSI. Las unidades no cambian de nombre; quien referenciara ese paquete en un `requires` o un `DCCReference` debe usar `Daf.SQLCute`
- **Specification**: El módulo pasa a llamarse `Daf.Specification` (antes `Daf.Query.Expressions`) y el emisor, `Daf.SQLCute.Specification` (antes `Daf.SQLCute.Expressions`). Ningún tipo cambia de nombre ni de firma; hay que tocar el `uses`, el `requires` y las rutas `src/Specification/` y `src/SQLCute.Specification/`
- **Commons**: `TFilterSpec<T>` —el filtro por clausuras— pasa a su propia unidad, `Daf.FilterSpec`
- **Commons**: `TExpresion<T>` pasa a llamarse **`TDeferred<T>`** y se muda a `Daf.Types`. La firma no cambia y `TMacroExpression` sigue siendo el alias, así que quien use macros no toca nada
- **Hosting**: El sample `ConsoleSample` se muda de `src/Application/samples/` a `src/Hosting/samples/`: lo que ilustra es el host —composición, configuración y ciclo de vida—, no la capa de aplicación en cuya carpeta estaba
- ⚠️ **MiniSpec**: Cuando más de un contexto satisface un `[Inject]`, gana el más cercano al step —escenario, luego feature, luego suite— y una coincidencia exacta gana a cualquier descendiente. Antes contestaba el primero que encajara según el orden interno de un diccionario, que no se podía observar porque solo había un contexto registrado a la vez
- ⚠️ **MiniSpec**: `TInjectorService` deja de admitir servicios sueltos y pasa a ser una instancia de la ejecución con los tres ámbitos, `Register(Scope, Instance)`. El colaborador que necesite un escenario se construye en el World, o en el contexto de feature o de suite si hay que compartirlo

### Removed

- **Commons**: `TGenericRTTI`. Su `MakeGenericType` prometía fabricar un genérico en ejecución, que es justamente lo que Delphi no hace: fallaba con violación de acceso incluso en el caso para el que se escribió. Lo que sí servía —leer los argumentos de una instanciación— vuelve como `_T.GenericArgs`
- **DependencyInjection**: `AddModules(Package)`, el descubrimiento por RTTI de los `IAppModule`. El sustituto es `AddModule<T>`, una llamada por módulo y en el orden que la aplicación quiera; `IAppModule`, `TAppModule` y el cuerpo de cada módulo no cambian. Una `TDafApplication` **ya no registra módulos por su cuenta**; el diff de ejemplo, en `MIGRATION-2.0.md`
- **Logging**: La global `DAFInternalLogger`, sustituida por `BootstrapLog.LoggerFor(<categoría>)`, que es por donde informa ahora también el propio DAF. Quien la llamara se lleva un error de compilación, no un cambio silencioso de comportamiento
- **Commons**: `TBooleanExpresion`
- **Commons**: La unidad `Daf.Expression`, disuelta en `Daf.FilterSpec` y `Daf.Types`

### Fixed

- **NNLog**: Una regla obedece los dos campos de su configuración que se ignoraban en silencio: `enabled` ausente vale ahora **activada** —solo el literal `false` la desactiva—, y `name` filtra de verdad (`*`, ausente o vacío nombra todas; cualquier otro valor es una expresión regular anclada). **Cambio de comportamiento observable**: un config con alguna regla de `name` distinto de `*` deja de recibirlo todo
- **DependencyInjection**: Cerrar el contenedor lo deja **cerrado**: resolver después de `ShutDown` lanza `EContainerShutDownError` nombrando el servicio, en vez de `EAccessViolation` o de servirlo en silencio. Cerrar dos veces deja de romper, y cerrarlo cierra todos sus ámbitos y no ninguno
- **DependencyInjection**: Un ciclo de dependencias que el contenedor no puede romper **deja de matar el proceso**. Agotaba la pila y el proceso moría sin excepción, sin mensaje y con la salida pendiente perdida en el búfer; ahora la resolución falla nombrando el camino del ciclo, esté la validación al construir encendida o apagada — que por defecto lo está la segunda
- **DependencyInjection**: Cerrar un ámbito soltaba **una referencia de más por cada servicio que había creado**, así que el servicio se destruía mientras su titular lo seguía usando y la siguiente llamada aterrizaba en una VMT liberada. El síntoma es sensible al layout —aparece y desaparece al tocar una clase que no interviene—, así que se manifiesta muy lejos de su causa
- **DependencyInjection**: Resolver desde varios hilos a la vez entrega **una sola instancia por registro**, y su constructor corre una vez. Antes corría **una vez por hilo** —ocho hilos pidiendo el mismo singleton construían ocho, y quien abre un fichero o un pool de conexiones en el constructor abría ocho— y siete de los ocho recibían un `EListError: Duplicates not allowed` salido del interior del contenedor. Un ámbito puede usarse desde cualquier hilo y desde varios a la vez, y cerrar mientras alguien resuelve acaba entregando el servicio o con `EContainerShutDownError`, nunca con un Access Violation. Componer la colección sigue siendo de un solo hilo, y ahora el README lo dice en vez de dejarlo deducir de un error. De paso, resolver un singleton ya construido cuesta la mitad que antes
- **MediatR**: Componer dos veces en un mismo proceso da el mismo resultado. La contabilidad de paquetes ya escaneados era estado global de unit —dos variables en el registro de MediatR—, así que la segunda colección compuesta en un proceso se saltaba lo ya visitado. Ahora es estado de la colección
- **Hosting**: Un proceso puede construir más de un host. «Build already called» y el «ya parado» del host eran estado de clase en vez de estado de instancia, así que un arnés que monta y desmonta hosts solo podía montar el primero
- **SQLCute**: `WhereIn` con un conjunto de valores **vacío** emitía `col IN ()`, sintácticamente inválido en casi cualquier motor. Ahora no casa ninguna fila y `WhereNotIn` las casa todas, incluida aquella cuya columna es NULL, con los dos back-ends dando la misma respuesta. Afecta también a `OrWhereIn`/`OrWhereNotIn` y a `IsIn([])`
- **Commons**: `TMacrosList.GetValueFromIndex` no asignaba `Result`; toda lectura de macro devolvía cadena vacía
- **MiniSpec**: Una feature que falla antes de sus escenarios —World que no se puede construir, hook `Before` que lanza— sale en el informe y pone la suite roja. Antes desaparecía sin rastro y el proceso salía con 0
- **MiniSpec**: `UseSuiteContext<T>` construye el contexto de suite, y un World lo puede pedir con `[Inject]`. No lo construía: `SpecContext.SuiteContext` valía `nil` en todos los steps, y la inyección que la documentación prometía no resolvía
- **MiniSpec**: Un tipo genérico en la descripción de un step —`AsType<T>`, `TList<T>`— se leía como placeholder de Scenario Outline y el step fallaba con `T not found in World`, apuntando al World en vez de al texto. Ahora un placeholder solo se reconoce cuando no va pegado a un identificador; los `<Expected>` de un Outline siguen expandiendo igual
- **Application / Docs**: El arranque que enseñaba la documentación **no compilaba**: de cinco deletreos en circulación, tres eran inválidos. La guía describía además una superficie de `IDafApplication` que no era la real
- **Application**: `TVersionInfo.Tag` estaba **siempre vacío**: el constructor hacía `FTag := Tag`, leyendo la propiedad que iba a escribir, así que quien informaba la versión con `Tag` publicaba una cadena vacía. De paso, `GetFrom` inicializa su resultado: una clase sin atributo devolvía lo que hubiera en la memoria del destino
- **Hosting / Docs**: La guía decía que con `TDafApplication` los módulos `IAppModule` «se descubren y cargan automáticamente», que dejó de ser cierto al retirar `AddModules(Package)`, y su snippet llamaba `TInfraModule.Create.AddServices(Services)` a mano: un `TInterfacedObject` con contador a cero que nadie libera. Ahora enseña `AddModule<T>`

## [1.6.0] - 2026-07-20

### Fixed

- **SQLCute**: `SqlServerCompiler` no emitía la cláusula `TOP` cuando se especificaba `LIMIT` sin `OFFSET`

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

### Fixed

- **MediatR**: Closure variable capture bug en la cadena de behaviors
- **MediatR**: Descubrimiento de behaviors globales con `StrictExtensions=False`
- **MediatR**: Warnings de ocultación en jerarquía `TPipelineBehavior`
- **MiniSpec**: Step sin lambda en `ScenarioOutline` se trataba como `Undefined`; ahora es no-op (display-only) porque `ExampleInit` inyecta los valores vía RTTI
- **MiniSpec**: Tabla de ejemplos de Outline: cabecera desalineada respecto a las filas de datos
- **MiniSpec**: Features sin escenarios ejecutados aparecían igualmente al filtrar con `-f`; ahora se suprimen en todos los reporters (console, JSON, Gherkin, JUnit)
- **MiniSpec**: Pie de resumen pegado a la última línea Gherkin; ahora lleva una línea en blanco antes

### Changed

- **MiniSpec**: Pie de resumen renombrado de `Pass/Fail/Skip` a `Passed/Failed/Pending/Undefined/Skipped`
- **MiniSpec**: `MarkAsSkipped` / `MarkAsUndefined` en `ISpecItem`
- **MiniSpec**: `UndefinedCount` en el runner y en el pie de resumen
- **MiniSpec**: Flag `--strict-pending`: hace que specs pendientes cuenten como fallo del proceso
- **MiniSpec**: Console reporter progresivo — Feature y Scenario se emiten al encontrarse, steps con símbolos Gherkin (`✓ ✗ ~ P ?`) al completarse

### Fixed

- **MiniSpec**: Step sin lambda en `ScenarioOutline` se trataba como `Undefined`; ahora es no-op (display-only) porque `ExampleInit` inyecta los valores vía RTTI
- **MiniSpec**: Tabla de ejemplos de Outline: cabecera desalineada respecto a las filas de datos
- **MiniSpec**: Features sin escenarios ejecutados aparecían igualmente al filtrar con `-f`; ahora se suprimen en todos los reporters (console, JSON, Gherkin, JUnit)
- **MiniSpec**: Pie de resumen pegado a la última línea Gherkin; ahora lleva una línea en blanco antes

### Changed

- **MiniSpec**: Pie de resumen renombrado de `Pass/Fail/Skip` a `Passed/Failed/Pending/Undefined/Skipped`

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

- **Samples**: TicTacToe - BDD sample app with MVVM + Clean Architecture (VCL dark-theme Form, ViewModel, ViewPort)
- **Samples**: TicTacToe - Feature specs: Movement, NewGame, UX (8 scenarios), with `@e2e`/`@integration` tags
- **Samples**: TicTacToe - Algebraic notation (`a1`–`c3`), visual board DataTables, ScenarioOutline with Examples
- **MiniSpec**: DataTable binding for ScenarioOutline; multiple reporters in INI config

### Fixed

- **MiniSpec**: Deferred binding in ScenarioOutline; Live Dashboard Rule/DataTable rendering

## [1.3.0] - 2026-02-02

### Added

- **MiniSpec**: Test Doubles - `Stub<T>`, `Mock<T>`, `Spy<T>` for interface mocking
- **MiniSpec**: Fluent API for stubs with `.Setup().Returns()`, `.WithArgs()`, `.Raises()`
- **MiniSpec**: Mock expectations: `.Expects().Once`, `.AtLeastOnce`, `.Never`, `.Exactly(N)`
- **MiniSpec**: Argument matchers: `Arg.Any<T>`, `Arg.Is<T>(predicate)`, `Arg.IsNil`
- **MiniSpec**: Spy tracking with `.Track()` and call history inspection
- **MiniSpec**: `.Pending` fluent method - mark specs as not yet implemented
- **MiniSpec**: `.NoAction` fluent method - descriptive steps without code
- **MiniSpec**: `@skip` reserved tag - always excluded from runs (mark broken tests)
- **MiniSpec**: JUnit XML reporter for CI/CD integration
- **MiniSpec**: SpecSuite - root node containing all Features with title and hooks
- **MiniSpec**: `Category(Title)` - fluent API to set Suite title
- **MiniSpec**: `Before(Description, Hook)` / `After(Description, Hook)` - Suite-level hooks
- **MiniSpec**: `ISpecSuite` interface and `TSpecSuite` implementation
- **MiniSpec**: Suite events: `OnBeginSuite` / `OnEndSuite` for listeners
- **MiniSpec**: All reporters display Suite title (Console, JSON, Gherkin comment, Live header)
- **MiniSpec**: DataTables support - inline data tables for steps (Gherkin standard)
- **MiniSpec**: `TDataTable` type and `TDataTableObj` wrapper with Row/Cell/Headers access
- **MiniSpec**: All reporters display DataTables (Console ASCII, JSON array, Gherkin pipes)
- **MiniSpec**: `TFeatureWorld` base class for Worlds needing execution context access
- **MiniSpec**: `ISpecContext` interface with `CurrentStep`, `CurrentScenario`, `CurrentRule`, `CurrentFeature`, `DataTable`
- **MiniSpec**: Extended filter system with `Feat:`, `Scen:`, `Rule:`, `Cat:` prefixes
- **MiniSpec**: `EndRule` method to return to Feature context from Rule
- **MiniSpec**: Live Dashboard auto-reconnect for new test runs
- **MiniSpec**: Live Dashboard export/print support
- **MiniSpec**: Cleaner exception testing API - `Expect(Raised).ToBe<EException>`
- **MiniSpec**: Dedicated test project for framework tests (`src/MiniSpec/test/`)
- **MiniSpec**: `Before`/`After` hooks for feature-level setup/teardown (run once per feature)

### Changed

- **MiniSpec**: Refactored `InUnit` → `Category` concept for flexible feature grouping
- **MiniSpec**: Filter prefixes changed: `F:`→`Feat:`, `S:`→`Scen:`, `R:`→`Rule:`, `U:`→`Cat:`
- **MiniSpec**: `TSourceUnit` marker renamed to `TUnitMarker`
- **MiniSpec**: Extracted reporters to individual units for better modularity
- **MiniSpec**: Uses `ISpecReporter` interface instead of class reference
- **Samples**: Renamed `.Feature.pas` → `.Feat.pas` per naming convention

### Fixed

- **MiniSpec**: AST parent navigation and phantom World elimination
- **MiniSpec**: Listeners `FCliOptions` dangling pointer and config logic
- **MiniSpec**: World create/destroy lifecycle balance verified with tests

### Removed

- **MiniSpec**: HTML reporter (replaced by Live Dashboard)
- **Tools**: MiniSpecWatcher project (replaced by Live reporter)

### Documentation

- **MiniSpec**: Updated README with new exception testing API
- **MiniSpec**: Added LiveDashboard unit to README file list

## [1.2.0] - 2026-01-24

### Added

- **MiniSpec**: And/But step keywords for more expressive scenarios
- **MiniSpec**: Tag filtering with expression parser (`@tag`, `not @tag`, `@a and @b`)
- **MiniSpec**: Rule support for enhanced scenario organization
- **MiniSpec**: ScenarioOutline with grouped table format
- **MiniSpec**: Skip state and enhanced reporting
- **MiniSpec**: Expanded Expect assertions
- **MiniSpec**: TGherkinReporter - generates `.feature` files from specs
- **MiniSpec**: TLiveReporter - real-time dashboard reporter
- **MiniSpec**: Per-reporter help via `-r <reporter>:help`
- **MiniSpec**: Configuration persistence via `.cfg` files
- **MiniSpec**: Logo display in console output

### Changed

- **MiniSpec**: New reporter CLI syntax for v1.2.0
- **MiniSpec**: TLiveReporter refactored to independent reporter
- **MiniSpec**: CLI cleanup - DryRun, Pause, CompletedAt options
- **BDS.bat**: Updated to v4.1.0 with `--project` option

### Fixed

- **MiniSpec**: UTF-8 encoding issues in reporters
- **MiniSpec**: Add Indy packages to requires (eliminates implicit import warnings)
- **MiniSpec**: Use fixed config filename `MiniSpec.ini`

### Documentation

- **MiniSpec**: Complete README rewrite for v1.5.0

## [1.1.0] - 2026-01-12

### Added

- Initial public release of DAF Community Edition
- Core modules: Commons, Configuration, DependencyInjection, Hosting, Logging, MediatR
- MiniSpec BDD testing framework
- Sample projects: Calculator, Config, Console, HostedService, Mastermind, MediatR, Ping

[Unreleased]: https://github.com/Delphi-DAF/dafce/compare/v1.5.0...HEAD
[1.5.0]: https://github.com/Delphi-DAF/dafce/compare/v1.4.0...v1.5.0
[1.4.0]: https://github.com/Delphi-DAF/dafce/compare/v1.3.0...v1.4.0
[1.3.0]: https://github.com/Delphi-DAF/dafce/compare/v1.2.0...v1.3.0
[1.2.0]: https://github.com/Delphi-DAF/dafce/compare/v1.1.0...v1.2.0
[1.1.0]: https://github.com/Delphi-DAF/dafce/releases/tag/v1.1.0
