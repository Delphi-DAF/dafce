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
Builder.ConfigureServices(
  procedure(Context: IHostBuilderContext; Services: IServiceCollection)
  begin
    Services
      .AddModule<TEntornoModule>
      .AddModule<TEvaluationModule>;
  end);
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
| `Daf.CmdLn.Parser` | paquete `Daf.CmdLn` |
| `Daf.SystemProcess` | paquete `Daf.SystemProcess` |

### 3. Tipos de `Daf.Commons` renombrados o mudados

| Antes | Ahora |
|---|---|
| `TExpresion<T>` | **`TDeferred<T>`**, en `Daf.Types` |
| `TFilterSpec<T>`, en `Daf.Expression` | misma clase, en `Daf.FilterSpec` |
| unidad `Daf.Expression` | disuelta en `Daf.FilterSpec` y `Daf.Types` |
| `TBooleanExpresion` | retirado |
| `TSmartArray<T>.Select(TDeferred<TMapper<T, TTarget>>)` | `Select(TFunc<T, TTarget>)` |

`Daf.Linq.Where` / `Filter` toman `TFilterSpec<T>`, el tipo que ya vivía en Commons: `Daf.Linq` no
introduce un vocabulario de filtrado propio, evalúa el que ya estaba nombrado.

### 4. Módulos de consulta renombrados

| Antes | Ahora |
|---|---|
| `Daf.Query.Expressions` | `Daf.Specification` |
| `Daf.SQLCute.Expressions` | `Daf.SQLCute.Specification` |
| `Daf.SQLCute.Compilers` | disuelto en `Daf.SQLCute` |

Los nombres nuevos dicen el patrón que implementan. `Daf.SQLCute.Compilers` desaparece como
paquete: sus seis compiladores de dialecto viven en `Daf.SQLCute`, junto al ANSI que ya estaba
ahí, así que hay una línea menos en los `requires`.

### 5. Logging: el modo mudo ya no existe

- **`DAFInternalLogger` retirada.** Era la única vía mientras la categoría del canal estuvo
  clavada a `DAF`; sin ese filtro es una segunda puerta que además etiqueta como DAF a quien no
  lo es.
- **Componer el pipeline sin un solo proveedor falla al construir el host**, con
  `ELoggingCompositionError`, en vez de entregar un `ILogger` que acepta todo y no escribe nada.
  Si tu arranque componía logging sin proveedores y no te habías dado cuenta, esto es lo que te lo
  va a decir.
- Los informes de arranque salen bajo la categoría de la clase que informa y por el pipeline de
  `ILogger`. No es API, es salida: si filtrabas por la categoría `DAF`, revisa el filtro.

## Cómo hacerlo

1. Compila. Lo que falte sale como error del compilador, no como comportamiento raro en
   ejecución.
2. Sustituye cada `AddModules` por una llamada por módulo, y quita los `uses` de force-link.
3. Añade a los `requires` los paquetes nuevos que necesites (`Daf.CmdLn`, `Daf.SystemProcess`) y
   quita `Daf.SQLCute.Compilers`.
4. Renombra los tipos y unidades de las tablas de arriba.
5. Vuelve a compilar y pasa tus tests. `ValidateOnBuild` ayuda a confirmar que la composición
   registra lo mismo que antes.
