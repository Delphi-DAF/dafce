# ConsoleSample — configuración, opciones y un servicio que las pide por constructor

Una herramienta de consola que lee sus ajustes de un `appsettings.json` y los entrega tipados al
servicio que los usa. El nivel siguiente a [`ServiceWiring`](../ServiceWiring/): el mismo arranque,
más configuración y el patrón de opciones.

## Qué enseña

- **El builder responde en cuanto se crea.** `B.Environment.BinPath` y `B.Environment.ApplicationName`
  están disponibles antes de construir nada, así que el `.dpr` puede usarlos para componer la ruta
  del fichero que va a cargar: leer configuración para decidir qué configuración cargar se escribe
  de arriba abajo.
- `JsonConfig.AddFile(B.Configuration, Ruta)` es una fachada, no un método de extensión — Delphi
  solo admite un `class helper` por tipo en ámbito, y ninguno aplica a una variable de interfaz.
- `ConfigOptions.Bind<TAppSettings>` declara los ajustes una vez: el contenedor construye el
  objeto, lo posee y lo libera. El `as IConfiguration` está porque el gestor es a la vez lector y
  constructor, y una interfaz de Delphi solo admite un padre.
- `TGreeter` pide `IOptions<TAppSettings>` por constructor, como cualquier otra dependencia, y
  **copia** los valores que necesita: `Value` devuelve un objeto crudo cuyo dueño es la opción, así
  que quedárselo y soltar la opción deja un puntero colgando. La otra forma correcta es sostener la
  `IOptions<T>` en un campo.
- La acción que declara `B.SetAction` recibe el `IServiceProvider` ya construido y el
  `ICommandLineParseResult`, y lo que devuelve es el código de salida — `TCliExit.Ok` aquí.
- `TActivatorUtilities.CreateInstance<T>` construye un objeto **no contado**: el dueño es quien
  llama. Sin el `Free` se fugan dos cosas, el accesor y la interfaz que sostiene dentro, porque su
  campo no se finaliza nunca.

## El trío `IWrapper`, que no se ve mirándolo

Un parámetro de constructor puede ser un **record que nadie registró**, si el record declara una
conversión implícita desde una interfaz que sí está registrada. `TWrapperAccessor` pide un
`IWrapper`; lo registrado es `IWrapperImpl`, y el contenedor lo resuelve y aplica
`IWrapper.Implicit`.

De ahí sale el resto: los operadores `Equal` y `NotEqual` contra `Pointer` existen para poder
escribir `Wrapper <> nil` sobre un record que se comporta como una referencia de interfaz. Es el
mismo patrón con el que DAF envuelve su propio `IServiceProvider`.

La comprobación se hace con un `raise` y no con un `Assert`: con las aserciones desactivadas un
`Assert` desaparece del binario, y la comprobación del sample se va con él.

## Ficheros

| Fichero | Papel |
|---|---|
| `ConsoleSample.dpr` | La composición entera: configuración, registros, la acción |
| `ConsoleSample.AppSettings.pas` | `TAppSettings`, con `{$M+}` para que sus propiedades tengan RTTI |
| `ConsoleSample.Greeter.pas` | `IGreeter`/`TGreeter`, y el trío `IWrapperImpl`/`IWrapper`/`TWrapperAccessor` |
| `Files/appsettings.json` | Los valores que acaban en `TAppSettings` |

## Ejecutar

```
BDS make --project:src/Cli/samples/ConsoleSample/ConsoleSample.dproj
out\Win32\samples\bin\ConsoleSample.exe
```

Salida esperada:

```
==============================
  My Cool App v1.0.0
==============================
¡Hola desde configuración con DAF!
```
