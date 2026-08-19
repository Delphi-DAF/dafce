# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html)
and [Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/).

## [Unreleased]

### Added

- **DependencyInjection**: `TryAddSingleton` / `TryAddScoped` / `TryAddTransient`, `Replace` y `RemoveAll` — el contrato «la librería pone el default, la aplicación manda», con la semántica de `ServiceCollectionDescriptorExtensions` de .NET. Sin ellos, una agrupación de librería solo podía pisar en silencio lo que el arranque hubiera configurado antes
- **DependencyInjection**: `Descriptors` — los registros de un servicio, en sólo lectura. Es lo que hace escribible la composición propia: un tercero puede añadir sus operaciones sin tocar DAF
- **DependencyInjection**: `Decorate<S>` — envuelve los registros existentes de un servicio conservando su ciclo de vida, sin conocer ni duplicar su factory. Es el `Decorate` de Scrutor, que en .NET tampoco está en el framework
- **DependencyInjection**: `AddModule<T>` y `AddModule(Module)` — el módulo se aplica por llamada, en el orden escrito, y con su constructor propio. Dos sobrecargas, como el `RegisterModule` de Autofac: la genérica, y la de instancia, que es la única vía de pasarle configuración
- **DependencyInjection**: `BuildServiceProvider(TServiceProviderOptions)` con `ValidateOnBuild` y `ValidateScopes`, ambos opt-in. Alcanzan a los registros por clase y reportan todos los fallos juntos; los registros por factory son opacos, y la documentación lo dice en vez de fingir que los cubre
- **Hosting**: `IStartupTask` y `AddStartupTask<T>` — inicialización que corre una vez al arrancar, con el proveedor ya construido. `THost.Start` las ejecuta en orden de registro, antes de los `IHostedService` y antes de `ApplicationStarted`; si una falla, el arranque aborta. .NET no tiene este contrato, y el hueco se tapa allí usando `IHostedService` con un `Stop` vacío que miente sobre lo que el objeto es
- **Hosting**: Sample `PackageInit` — un paquete que trae su propia inicialización, de modo que el ejecutable llama a su `AddXxx` y no menciona ni el tipo de la tarea ni lo que hace
- **Docs**: Página nueva «Composición del arranque» (EN/ES) — las dos fases del arranque, las dos formas de agrupar registros, la tabla de equivalencias con .NET y las divergencias justificadas
- **Daf.Linq**: Módulo nuevo — secuencias perezosas con conteo de referencias sobre `System.IEnumerable<T>`: `TQuery<T>` (la superficie de operadores), `IGrouping<TKey, T>` y `Linq.From<T>`. La secuencia **es** un `IEnumerable<T>`, no un tipo propio: este es el mundo de los closures, el equivalente de LINQ-to-Objects, y así se llama en .NET. El nombre `IQueryable<T>` queda **reservado** para lo que significa allí —un árbol componible que se difiere a un back-end—, que DAF todavía no tiene
- **Daf.Linq**: Operadores `Where`/`Filter`, `Select`/`Map`, `Take`, `Skip`, `Zip`, `Distinct`, `OrderBy`, `GroupBy`
- **Daf.Linq**: Terminales `IsEmpty`, `Any`, `FirstOrDefault`/`LastOrDefault`, `TryFirst`, `Single`, `SingleOrDefault`, `TrySingle`, `ToArray`, `ToList`, `ToSmartArray`
- **SQLCute.InMemory**: Módulo nuevo — segundo back-end: ejecuta un `IQuery` sobre colecciones en memoria. Carril tipado sobre `TList<T>` y carril general con `IQueryContext`
- **SQLCute.InMemory**: `IRow` con adaptadores `TRttiRow<T>`, `TDictRow`, `TJoinRow`, `TNullRow`; materialización por RTTI a clase de resultado
- **SQLCute.InMemory**: Tres puntos de entrada por lo que devuelven: `TInMemory.Elements<T>` (los elementos originales), `Rows<T>` (filas dinámicas) y `Project<TSource, TResult>` (materializadas por RTTI)
- **SQLCute.InMemory**: JOINs (INNER/LEFT/RIGHT/CROSS/FULL OUTER), GROUP BY/HAVING, subconsultas, UNION/INTERSECT/EXCEPT
- **SQLCute.InMemory**: Semántica ANSI de NULL — lógica de tres valores, agregados que ignoran NULL, DISTINCT con NULLs iguales, GROUP BY con un grupo para NULL, ORDER BY con NULL como menor
- **SQLCute.InMemory**: Samples `InMemoryQuerySample` e `InMemoryJoinSample`; documentación bilingüe (EN/ES)
- **SQLCute**: `Daf.SQLCute.WhereTree` — normalizador de precedencia WHERE compartido por ambos back-ends, en el paquete `Daf.SQLCute`
- **Daf.Specification**: Módulo nuevo — condiciones de consulta escritas con operadores Delphi que siguen siendo un **árbol inspeccionable**, sin SQL y sin referenciar SQLCute: `IExpr` y su juego acotado de nodos, `Prop`/`Prm`, `TEntityExpr<T>` (proxy RTTI que valida cada miembro donde se escribe) y el metamodelo opcional `TPropExpr<T>`
- **Daf.Specification**: `TSpecification<T>` — una regla reutilizable con filtro, orden y paginación, afirmable en un test sin base de datos; `IRepository<T>` como puerto, sin restricción de clase base
- **Daf.Specification**: `INameResolver` — la única costura de nombres: miembro → columna y tipo → tabla, con identidad y «nombre del tipo sin la T inicial» por defecto
- **SQLCute.Specification**: Módulo nuevo — `TExprEmitter` emite una expresión como cláusulas WHERE sobre un `IQuery`, de modo que la misma regla va a la base de datos o a una colección cargada sin traducirse dos veces
- **SQLCute.Specification**: `TInMemoryRepository<T>` — adaptador de `IRepository<T>` sobre una colección ya cargada, con caché de planes por regla; sample `CatalogServiceSample` y documentación bilingüe (EN/ES)
- **SQLCute**: `WhereNot(Callback)` y `OrWhereNot(Callback)` — grupos WHERE entre paréntesis negados como una unidad, `NOT (…)`
- **SQLCute**: Orden de NULL por columna en ORDER BY — sobrecargas `OrderBy`/`OrderByDesc` con `TNullsOrder` y modificadores `NullsFirst`/`NullsLast`. Postgres, Oracle, Firebird y SQLite emiten `NULLS FIRST`/`NULLS LAST` nativo; MySQL y SQL Server, que no tienen esa sintaxis, lo **emulan** con una clave de orden previa, así que el orden pedido se respeta en todos los dialectos
- **Commons**: `TNullable<T>` en `Daf.Nullable`
- **Commons**: `TDeferredEnumerable<T>` en `Daf.Enumerable`
- **Commons**: `Daf.Comparers` — comparadores conscientes de NULL para `TValue` y `TNullable<T>`
- **Daf.Web**: Módulo nuevo en paquete — servidor HTTP ligero sobre Indy y WebBroker, controladores enrutados por atributos y construidos por el contenedor, y action results. Sus fuentes y su README ya se publicaban; lo que faltaba era el `.dpk` con el que instalarlo
- **Daf.Web**: El paquete **no enlaza DataSnap ni DBX**. El `uses` del servidor era el de la plantilla *DataSnap WebBroker Application* y traía 59 unidades —gestores de sesión, transportes, lectores de metadatos, un escáner SQL— al servicio de un `TerminateThreads` que sin DataSnap no hacía nada y de una comprobación de puerto que ahora responde el propio servidor Indy. Quien lo instale se lleva un servidor HTTP, no una pila de remoting
- **Logging**: Canal de arranque —un `ILogger` disponible desde que arranca el proceso, sin inyección de dependencias y sin configuración— con más de un destino y abierto a la aplicación. Cubre la fase anterior a `BuildServiceProvider`, donde no existe `IServiceProvider` y por tanto nada de lo registrado puede informar de un fallo. Es el hueco que en .NET tapa el `CreateBootstrapLogger` de Serilog
- **Logging**: La aplicación informa de su propio arranque **bajo su categoría**, con `BootstrapLog.LoggerFor('MiApp')` — la única vía de pedir un logger de arranque, y la que usa también el propio DAF. Antes el canal exigía que la categoría empezara por `DAF`, así que un informe propio solo podía elegir entre disfrazarse o descartarse en silencio
- **Logging**: Destinos que no dependen de que haya un depurador conectado: la salida de error del proceso —lo que captura un envoltorio o el programador de tareas— y la salida de depuración del sistema. Quien despliega añade el suyo con `BootstrapLog.AddSink`, antes de construir el host
- **Logging**: `TFileBootstrapSink`, el destino duradero, **sin registrar**: recibe la ruta de quien lo construye. La aplicación sabe dónde guarda sus datos y el framework no, así que DAF ofrece el mecanismo y no elige la ruta — que es también lo que hace .NET
- **Logging**: `TPlatformLogBootstrapSink` en `DAF.Extensions.Logging.PlatformSink` — la salida de depuración del sistema fuera de Windows, sobre `IFMXLoggingService`. Vive en su propia unit para que FireMonkey no le llegue a quien no lo pidió, y su disponibilidad la responde la plataforma, no un `{$IFDEF}` mantenido a mano
- **Docs**: La página «Composición del arranque» (EN/ES) gana el relevo de dos etapas: quién informa antes de `BuildServiceProvider` y quién después, qué recibe cada forma de despliegue —escritorio, consola, desatendido— y el patrón del `.dpr`

### Changed

- **Hosting**: `THost.Start` y `THost.Stop` informan por el pipeline de `ILogger` en vez de por el canal de arranque. Ahí el proveedor ya está construido, así que seguir en el canal era saltarse los proveedores y la configuración que el usuario escribió; el canal se queda de respaldo por si `ILoggerFactory` no se puede resolver. `THostBuilder.Build` sigue en el canal, que es donde tiene que estar
- **Hosting / Logging**: Los informes de arranque de DAF salen bajo la categoría de la clase que informa —`THostBuilder`, `TCompositeLogger`— en vez de bajo `DAF` con el nombre de la clase metido en el mensaje. La categoría dice quién y el mensaje dice qué, igual a los dos lados del relevo
- **Logging**: Un destino del canal de arranque que no puede escribir **se declara no disponible** en lugar de aceptar el informe y perderlo, y uno que lanza queda fuera sin llevarse por delante el arranque. Fuera de Windows la escritura a la salida de depuración no fallaba, no avisaba y no escribía: era un modo mudo que solo se descubría desplegando. `BootstrapLog.HasAvailableSink` distingue además el caso peor —que no esté escuchando nadie— de haber informado
- **SQLCute**: `CompileWhere` y `CompileHaving` emiten paréntesis explícitos de precedencia
- **Commons**: `TSmartArray<T>.Select` toma `TFunc<T, TTarget>` en lugar de `TDeferred<TMapper<T, TTarget>>`
- **Daf.Linq**: `Where` / `Filter` toman `TFilterSpec<T>`
- **Commons**: `Daf.Enumerable.ToArray` usa colector `TList<T>` (O(n) en lugar de O(n²))
- **SQLCute**: El módulo pasa de **5 paquetes a 4**. `Daf.SQLCute.Compilers` desaparece y sus seis compiladores de dialecto se integran en `Daf.SQLCute`, que ya contenía el compilador ANSI: la frontera no aislaba ninguna dependencia —requería exactamente lo mismo que `Daf.SQLCute`— y a cambio separaba ANSI de los otros seis sin motivo. Los nombres de las **unidades no cambian**, así que ningún `uses` se toca; quien referenciara el **paquete** `Daf.SQLCute.Compilers` en un `requires` o un `DCCReference` debe usar `Daf.SQLCute`
- **SQLCute**: Tests, samples y documentación del módulo se mudan de `src/SQLCute.Abstractions/` a `src/SQLCute/`, y las dos suites (`SQLCuteSpecs` y `SQLCuteCompilersSpecs`) se fusionan en un runner único de 199 specs. Es la convención del proyecto: un módulo partido en `.Abstractions` + implementación pone tests y samples solo en la implementación, con un runner por módulo
- **Specification**: El módulo pasa a llamarse `Daf.Specification` (antes `Daf.Query.Expressions`), y el emisor, `Daf.SQLCute.Specification` (antes `Daf.SQLCute.Expressions`). Nombran el patrón que implementan y el valor que bajan a SQL. `Query.` decía justo lo contrario de lo que el módulo es: `IQuery` es el AST de SQLCute, la única dependencia que este paquete no puede tener. Ningún tipo cambia de nombre ni de firma; hay que tocar el `uses`, el `requires` y la ruta `src/Specification/` y `src/SQLCute.Specification/`
- **Commons**: `TFilterSpec<T>` —el filtro por clausuras— pasa a su propia unidad, `Daf.FilterSpec`. La unidad `Daf.Expression` guardaba dos tipos sin relación y llevaba el nombre del que tiene un solo consumidor, además de nombrar el mundo de los árboles para una clausura
- **Commons**: `TExpresion<T>` pasa a llamarse **`TDeferred<T>`** y se muda a `Daf.Types`, junto a su único consumidor. El nombre nuevo dice lo que hace —el valor se evalúa al leerlo, y en cada lectura— y deja de sugerir el `Expression<TDelegate>` de .NET, que es un árbol inspeccionable; tampoco es `Lazy<T>`, que memoiza. La firma no cambia y `TMacroExpression` sigue siendo el alias, así que quien use macros no toca nada

### Deprecated

- **DependencyInjection**: `AddModules(Package)`, el descubrimiento por RTTI de los `IAppModule`. Sigue funcionando; el sustituto es `AddModule<T>`, una llamada por módulo y en el orden que la aplicación quiera. En compilación nativa el descubrimiento no ahorraba la declaración: la escondía en un `uses` con comentario `// force-link:`

### Removed

- **Logging**: La global `DAFInternalLogger`. Era la única vía mientras la categoría del canal estuvo clavada a `DAF`; sin ese filtro es una segunda puerta que además etiqueta como DAF a quien no lo es. Se sustituye por `BootstrapLog.LoggerFor(<categoría>)`, que es por donde informa ahora también el propio DAF. Quien la llamara se lleva un error de compilación, no un cambio silencioso de comportamiento
- **Commons**: `TBooleanExpresion`
- **Commons**: La unidad `Daf.Expression`, disuelta en `Daf.FilterSpec` y `Daf.Types`

### Fixed

- **DependencyInjection / MediatR**: Componer dos veces en un mismo proceso da el mismo resultado. La contabilidad de paquetes ya procesados era estado global de unit —una variable en `AddModules`, dos en el registro de MediatR—, así que la segunda colección compuesta en un proceso se saltaba lo ya visitado. Ahora es estado de la colección
- **Hosting**: Un proceso puede construir más de un host. «Build already called» y el «ya parado» del host eran estado de clase en vez de estado de instancia, así que un arnés que monta y desmonta hosts solo podía montar el primero
- **SQLCute**: `WhereIn` con un conjunto de valores **vacío** emitía `col IN ()`, sintácticamente inválido en casi cualquier motor, así que el fallo llegaba desde el servidor y lejos de la llamada que lo causó. Un conjunto vacío es la disyunción vacía, así que ahora `WhereIn` no casa ninguna fila y `WhereNotIn` casa todas — incluida aquella cuya columna es NULL, donde antes el carril en memoria respondía *desconocido* y descartaba la fila de ambos. Los dos back-ends dan ya la misma respuesta. Afecta también a `OrWhereIn`/`OrWhereNotIn` y a `IsIn([])` de la capa de expresiones
- **Commons**: `TMacrosList.GetValueFromIndex` no asignaba `Result`; toda lectura de macro devolvía cadena vacía
- **MiniSpec**: Un tipo genérico en la descripción de un step —`AsType<T>`, `TList<T>`— se leía como placeholder de Scenario Outline y el step fallaba con `T not found in World`, apuntando al World en lugar de al texto. Un placeholder nunca continúa una palabra, así que ahora solo se reconoce cuando no va pegado a un identificador; los `<Expected>` de un Outline siguen expandiendo igual, incluso tras coma o paréntesis. Un placeholder que no casa con ningún campo sigue siendo error, y el mensaje nombra las dos lecturas posibles

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
