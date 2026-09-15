# NNLog — Guía de uso

**🌍 Idioma: [English](GUIDE.md) | Español**

---

## Tabla de contenidos

1. [Arquitectura](#arquitectura)
2. [Componer un proveedor](#componer-un-proveedor)
3. [Configuración](#configuración)
4. [TLogRule](#tlogrule)
5. [Patrones de categoría](#patrones-de-categoría)
6. [TTarget](#ttarget)
7. [Tokens de layout](#tokens-de-layout)
8. [Targets incluidos](#targets-incluidos)
9. [Varios targets y reglas](#varios-targets-y-reglas)
10. [Crear un target propio](#crear-un-target-propio)
11. [Integración con ILoggingBuilder](#integración-con-iloggingbuilder)

---

## Arquitectura

```
ILogger.LogXxx(...)
        │
        ▼
  TNNLogLogger (uno por categoría)
        │
        │  por cada una de SUS reglas que acepte el nivel:
        ├─► Target A  (Console)
        ├─► Target B  (File)
        └─► Target C  (propio)
```

La categoría se empareja **una vez**, cuando `TNNLogProvider.CreateLogger` construye el logger: el
logger se queda solo con las reglas cuyo patrón empareja su categoría. Por entrada queda la
comprobación de nivel —`MinLevel`, `MaxLevel`, `Levels`— y `Enabled`.

Una regla que acepta la entrada la escribe en **todos** los targets de su `WriteTo`. Con
`IsFinal = True` el logger se detiene tras esa regla; si no, sigue con la siguiente, así que una
entrada puede llegar a varias reglas.

`ILogger.IsEnabled(Level)` contesta con el mismo predicado que usa la escritura: es `True` cuando
alguna regla activa de ese logger acepta el nivel.

---

## Componer un proveedor

`TNNLogLoader` construye el proveedor a partir de unos ajustes, y es la entrada de todas las rutas:

```pascal
uses
  Daf.Extensions.Logging,
  Daf.NNLog.Loader;

// Desde una sección de configuración
Provider := TNNLogLoader.BuildProvider(Environment, Config.GetSection('Logging:NLog'));

// Desde el contexto del builder de host, nombrando la sección
Provider := TNNLogLoader.BuildProvider(Context, 'Logging:NLog');

// Desde unos ajustes montados con código
Provider := TNNLogLoader.BuildProvider(Environment, Settings);
```

Las dos primeras leen sus ajustes y los poseen. La tercera recibe un `TNNLogConfiguration` que sigue
siendo de quien llama — `BuildProvider` no lo libera.

`BuildProvider` es quien registra los renderers de layout, resuelve los tipos de target, aplica los
valores por defecto de cada campo de regla y da cuenta de un fallo de composición.
**`TNNLogProvider.Create` se salta todo eso**: no queda ningún renderer registrado, así que cada
token `${…}` renderiza cadena vacía, y los campos de la regla conservan los valores de un objeto
recién creado — `Enabled = False` y `MaxLevel = Trace`, que no aceptan nada. Para construir un
proveedor que escriba, pasar por `TNNLogLoader`.

---

## Configuración

```json
{
  "Logging": {
    "NLog": {
      "targets": {
        "console": {
          "targetType": "Console",
          "layout": "${level} [${category}] ${message}",
          "UseColors": "true"
        },
        "file": {
          "targetType": "File",
          "fileName": "${binPath}logs/app.log"
        },
        "udp": {
          "targetType": "UDP",
          "host": "localhost",
          "port": 2020
        }
      },
      "rules": [
        { "name": "*", "writeTo": "console", "minLevel": "Trace", "isFinal": false },
        { "name": "*", "writeTo": "udp,file", "minLevel": "Information", "isFinal": false }
      ]
    }
  }
}
```

### Claves de un target

`targetType` elige la clase; cualquier otra clave se asigna al target como una propiedad de ese
nombre, así que la superficie configurable de un target son sus propiedades escribibles. Las claves
se emparejan sin distinguir mayúsculas.

`targetType` se resuelve por RTTI a `Daf.NNLog.Targets.<Tipo>.T<Tipo>Target`, **y esa búsqueda sí
distingue mayúsculas**: `Console` encuentra `TConsoleTarget`, `console` no. `File` es la única
excepción en el nombre de la unit — `file` es palabra reservada, así que la unit es
`Daf.NNLog.Targets.File_`. Un `targetType` sin clase detrás es un fallo de composición:
`Daf.NNLog Error: Unknown target type <Tipo>`.

El tipo tiene que sobrevivir al enlazado inteligente para que el RTTI lo encuentre — también los
targets incluidos. Añadir `{$STRONGLINKTYPES ON}` al `.dpr` y nombrar en su `uses` cada unit de target que
se configure:

```pascal
{$STRONGLINKTYPES ON}
uses
  Daf.NNLog.Loader,
  Daf.NNLog.Targets.Console,
  Daf.NNLog.Targets.File_,
  Daf.NNLog.Targets.UDP;
```

### Claves de una regla

| Clave | Valor | Ausente |
|-------|-------|---------|
| `name` | patrón de categoría | empareja todas las categorías |
| `minLevel` | nivel más bajo que acepta | `Trace` |
| `maxLevel` | nivel más alto que acepta | `Critical` |
| `levels` | lista de niveles separados por comas | ninguna lista |
| `writeTo` | lista de nombres de target separados por comas | la regla no escribe en ningún sitio |
| `isFinal` | `true` detiene la evaluación tras esta regla | `false` |
| `enabled` | `false` —y solo ese literal— desactiva la regla | la regla está activa |

Los nombres de `writeTo` son las claves de la sección `targets`, emparejados **exactamente**:
importan la caja y la ausencia de espacios alrededor de la coma, y un nombre sin target detrás se
descarta en silencio. `"udp,file"` llega a dos targets; `"udp, file"` llega a uno.

---

## TLogRule

```pascal
TLogRule = class
  property Name: string;                  // patrón de categoría; '' y '*' emparejan todas
  property MinLevel: TLogLevel;           // nivel más bajo que acepta
  property MaxLevel: TLogLevel;           // nivel más alto que acepta
  property Levels: TArray<TLogLevel>;     // si no está vacía, el nivel tiene que estar en la lista
  property WriteTo: TArray<TTarget>;      // targets en los que escribe esta regla
  property Enabled: Boolean;              // una regla desactivada no acepta nada
  property IsFinal: Boolean;              // detener la evaluación tras aceptar esta regla
  function Matches(const Category: string; const Level: TLogLevel): Boolean;
end;
```

Los tres campos de nivel son una **conjunción**, no una precedencia: el nivel de la entrada tiene
que estar entre `MinLevel` y `MaxLevel` **y**, cuando `Levels` no está vacía, ser uno de sus
miembros. Un `MinLevel` de `Warning` con `Levels` de `Debug` no acepta nada.

Los valores por defecto de [Claves de una regla](#claves-de-una-regla) son los que `TNNLogLoader`
aplica a una clave ausente.
Una `TLogRule` construida con `TLogRule.Create` parte de los valores de un objeto nuevo, así que
`Enabled` es `False` y `MaxLevel` es `Trace`.

---

## Patrones de categoría

El `Name` de una regla es una **expresión regular anclada a la categoría completa**: el patrón tiene
que emparejar la categoría de principio a fin.

| Patrón | Empareja |
|--------|----------|
| `*`, y un nombre ausente o vacío | todas las categorías |
| `MyApp\..*` | `MyApp.` seguido de cualquier cosa |
| `MyApp\.Services\.UserService` | esa categoría, y solo ella |
| `.*Repository` | todas las categorías que terminan en `Repository` |
| `(?i)myapp\..*` | lo mismo que la segunda fila, ignorando la caja |

La caja la decide el propio patrón, con un `(?i)` en línea; por defecto distingue mayúsculas. Un `.`
sin escapar es el comodín del regex, así que `MyApp.Services` empareja también `MyAppXServices`.

Un `Name` que no sea una expresión regular válida es un fallo de composición, y se dice al cargar la
configuración, nombrando la regla y el patrón:
`Daf.NNLog Error: rule #0 has an invalid category pattern "*Repository": …`.

---

## TTarget

Clase base de todo destino de NNLog.

```pascal
TTarget = class
  const DefaultLayout = '${timestamp} #${pid}:${thread} [${level}] ${category} | ${message} ${exception}';
  constructor Create; virtual;
  destructor Destroy; override;
  procedure Write(const Entry: TLogEntry);          // toma el candado y llama a DoWrite
  procedure DoWrite(const Entry: TLogEntry); virtual; abstract;   // protected
  function RenderEvent(Entry: TLogEntry): string;   // protected
  property Name: string;
  property Layout: string;
end;
```

Una subclase sobrescribe `DoWrite` y llama a `RenderEvent` para convertir la entrada en una línea con
su propio `Layout`. `Name` es a lo que se refiere el `writeTo` de una regla.

`Write` no es virtual: toma una sección crítica de esa instancia de destino y llama a `DoWrite`, así
que un destino escrito hoy queda serializado sin que su autor haga nada. Todos los loggers del
proceso comparten las mismas instancias de destino, así que el destino es donde se encuentran las
escrituras concurrentes.

El candado es **por instancia de destino**, así que dos destinos nunca se esperan, y es **dentro de
un proceso**: dos ejecuciones del mismo programa escribiendo en un fichero pueden entrelazarse
igual, y compartir fichero de log entre procesos no está cubierto.

---

## Tokens de layout

Un layout es texto con marcas `${token}`; un token puede llevar un argumento tras dos puntos. Los
nombres de token se emparejan sin distinguir mayúsculas, y **un token sin renderer detrás se queda
sin sustituir**, así que un `${mesage}` mal escrito sale en la línea en vez de costar una columna en
silencio.

| Token | Valor |
|-------|-------|
| `${timestamp}` | el momento de renderizar la línea, como `yyyy-MM-dd HH:mm:ss.zzz` |
| `${date}`, `${date:formato}` | el momento de renderizar, con formato; `yyyy-MM-dd` sin argumento |
| `${level}` | nombre del nivel: `TRACE`, `DEBUG`, `INFO`, `WARN`, `ERROR`, `CRIT` |
| `${category}` | la categoría del logger |
| `${scope}` | los ámbitos abiertos alrededor de la entrada, el más interno el último, cada uno precedido de ` \| ` |
| `${message}` | el mensaje renderizado |
| `${exception}` | `ClaseExcepción: Mensaje`, vacío cuando la entrada no trae excepción |
| `${exception:stacktrace}` | la pila de la excepción |
| `${pid}` | ID del proceso actual |
| `${thread}` | ID del hilo actual |
| `${newline}` | un salto de línea |
| `${env:NOMBRE}` | la variable de entorno `NOMBRE` |
| `${event-properties:clave}` | `clave` del estado de la entrada, vacío si no la trae |
| `${environment}` | `IHostEnvironment.EnvironmentName` |
| `${contentRootPath}` | `IHostEnvironment.ContentRootPath` |
| `${binPath}` | `IHostEnvironment.BinPath` |
| `${ApplicationName}` | `IHostEnvironment.ApplicationName` |
| `${uppercase:texto}`, `${lowercase:texto}` | el argumento literal, con otra caja |

Los tokens no se anidan: el argumento de un token se toma literal, así que `${uppercase:${level}}`
no cambia la caja del nombre del nivel.

### Layout por defecto

```
${timestamp} #${pid}:${thread} [${level}] ${category} | ${message} ${exception}
```

El `FileName` de `TFileTarget` pasa por el mismo motor, así que `${binPath}`, `${date}` y los demás
funcionan dentro de una ruta.

---

## Targets incluidos

| Clase | `targetType` | Unit | Salida |
|-------|--------------|------|--------|
| `TConsoleTarget` | `Console` | `Daf.NNLog.Targets.Console` | salida estándar |
| `TFileTarget` | `File` | `Daf.NNLog.Targets.File_` | un fichero de log, con rotación |
| `TDebugTarget` | `Debug` | `Daf.NNLog.Targets.Debug` | la salida de depuración del sistema, en Windows |
| `TUDPTarget` | `UDP` | `Daf.NNLog.Targets.UDP` | un endpoint UDP |

### TConsoleTarget

| Propiedad | Por defecto | Significado |
|-----------|-------------|-------------|
| `UseColors` | `False` | colorear cada línea por nivel — atributos de consola Win32 en Windows, secuencias ANSI en el resto |

### TFileTarget

| Propiedad | Por defecto | Significado |
|-----------|-------------|-------------|
| `FileName` | — | ruta del fichero actual; se resuelve como un layout |
| `RotationPolicy` | `None` | `None`, `hourly`, `Daily`, `Days`, `Monthly`, `Size` |
| `MaxFileSize` | `0` | tamaño en bytes que dispara la rotación con la política `Size` |
| `MaxFileAge` | `0` | edad en días que dispara la rotación con la política `Days` |
| `MaxArchivedFiles` | `0` | cuántos ficheros rotados se conservan; los más antiguos se borran |
| `CompressOnRotate` | `False` | comprimir en zip el fichero rotado |
| `AutoFlush` | `True` | volcar tras cada entrada |

Los ficheros rotados van a un directorio `history` junto al actual. Las políticas `Days` y `Size`
comparan contra `MaxFileAge` y `MaxFileSize`, así que cada una necesita su umbral puesto: a `0`
rotan en cada entrada.

Un valor de enumerado que el loader no reconoce se queda en el primer valor del enumerado, así que
un `rotationPolicy` mal escrito se lee como `None`.

### TUDPTarget

| Propiedad | Por defecto | Significado |
|-----------|-------------|-------------|
| `Host` | `127.0.0.1` | host de destino |
| `Port` | `12201` | puerto de destino — el de GELF de Graylog |

---

## Varios targets y reglas

```json
"rules": [
  { "name": "*", "writeTo": "file", "minLevel": "Debug" },
  { "name": "*", "writeTo": "console", "minLevel": "Warning", "isFinal": true }
]
```

Una entrada `Debug` la acepta solo la primera regla y llega al fichero. Una entrada `Warning` la
aceptan las dos: llega al fichero, luego a la consola, y ahí se detiene la evaluación porque la
segunda regla es final.

El orden importa por dos motivos — las reglas se evalúan en el orden en que las lista la sección, y
`isFinal` corta el resto.

---

## Crear un target propio

Heredar de `TTarget` y sobrescribir `Write`:

```pascal
unit Daf.NNLog.Targets.Slack;

interface

uses
  Daf.Extensions.Logging,
  Daf.NNLog;

type
  TSlackTarget = class(TTarget)
  private
    FWebhookUrl: string;
  protected
    procedure DoWrite(const Entry: TLogEntry); override;
  public
    property WebhookUrl: string read FWebhookUrl write FWebhookUrl;
  end;

implementation

uses
  System.SysUtils;

procedure TSlackTarget.DoWrite(const Entry: TLogEntry);
begin
  if Entry.Level < TLogLevel.Warning then
    Exit;
  var Payload := Format('{"text": "%s"}', [RenderEvent(Entry)]);
  // POST HTTP de Payload a WebhookUrl
end;

end.
```

Para que se pueda alcanzar desde la configuración, un target propio sigue la convención del loader:
la unit es `Daf.NNLog.Targets.<Tipo>` y la clase `T<Tipo>Target`, y cada valor configurable es una
propiedad escribible. Entonces se nombra como cualquier otro target:

```json
"targets": {
  "slack": { "targetType": "Slack", "webhookUrl": "https://hooks.slack.com/…" }
}
```

Su unit hay que nombrarla en el `uses` del programa como cualquier otra unit de target, por el
motivo que da [Claves de un target](#claves-de-un-target).

La configuración pasa cadenas, así que una propiedad que vaya a asignar el loader tiene que ser de
un tipo al que `Daf.Configuration.Binder` sepa convertir — una cadena, un número, un booleano o un
enumerado nombrado por uno de sus valores. Un target que necesite otra cosa expone una propiedad de
tipo cadena y la interpreta él.

---

## Integración con ILoggingBuilder

```pascal
uses
  Daf.Extensions.Logging,
  Daf.Logging.Builder,
  Daf.NNLog.Loader;

AddLogging(Services,
  procedure(Builder: ILoggingBuilder)
  begin
    Builder.AddProvider(TNNLogLoader.BuildProvider(Env, Config.GetSection('Logging:NLog')));
  end);
```

El builder de aplicación expone el entorno y la configuración, que es la forma que usa una
aplicación de consola:

```pascal
var B := TCliApplication.CreateBuilder(CommandLineConfig.ProcessArgs);
B.Logging.AddProvider(TNNLogLoader.BuildProvider(B.Environment, B.Configuration.GetSection('Logging:NLog')));
```

`Daf.Hosting` registra un `IHostBuilderContext` que lleva los dos, y es lo que recibe la sobrecarga
`BuildProvider(Context, ConfigKey)`.

El proveedor posee todos los targets y las reglas que recibió, y los libera cuando la factory lo
suelta.
