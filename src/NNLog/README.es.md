# NNLog

**🌍 Idioma: [English](README.md) | Español**

Proveedor de logging basado en reglas — un porte de NLog («NNLog is Not NLog»). Se conecta al
pipeline `ILoggerFactory` mediante `ILoggerProvider` y enruta las entradas de log a uno o más
**targets** (consola, fichero, UDP, salida de depuración) según **reglas** configurables.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![Licencia](https://img.shields.io/badge/licencia-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Inicio rápido

Los targets y las reglas salen de una sección de configuración:

```json
{
  "Logging": {
    "NLog": {
      "targets": {
        "console": {
          "targetType": "Console",
          "layout": "${timestamp} [${level}] ${category} | ${message}",
          "UseColors": "true"
        }
      },
      "rules": [
        { "name": "*", "writeTo": "console", "minLevel": "Debug" }
      ]
    }
  }
}
```

`TNNLogLoader` lee la sección y construye el proveedor:

```pascal
uses
  Daf.Cli,
  Daf.Configuration.CommandLine,
  Daf.Extensions.Configuration,
  Daf.Extensions.Logging,
  Daf.NNLog.Loader;

var B := TCliApplication.CreateBuilder(CommandLineConfig.ProcessArgs);
B.Logging.AddProvider(TNNLogLoader.BuildProvider(B.Environment, B.Configuration.GetSection('Logging:NLog')));
```

`BuildProvider` acepta también un `TNNLogConfiguration` montado con código, para unos ajustes que no
vienen de un fichero.

---

## Tokens de layout

| Token | Descripción |
|-------|-------------|
| `${timestamp}` | el momento de renderizar, como `yyyy-MM-dd HH:mm:ss.zzz` |
| `${date:formato}` | el momento de renderizar, con formato |
| `${pid}` | ID del proceso |
| `${thread}` | ID del hilo |
| `${level}` | nombre del nivel: `TRACE`, `DEBUG`, `INFO`, `WARN`, `ERROR`, `CRIT` |
| `${category}` | categoría del logger |
| `${message}` | mensaje renderizado |
| `${exception}` | clase + mensaje de la excepción, vacío si no hay |
| `${env:NOMBRE}` | variable de entorno |
| `${event-properties:clave}` | un valor del estado de la entrada |
| `${binPath}`, `${contentRootPath}`, `${environment}`, `${ApplicationName}` | de `IHostEnvironment` |

Layout por defecto: `${timestamp} #${pid}:${thread} [${level}] ${category} | ${message} ${exception}`

La tabla completa, con el argumento que acepta cada token, está en la [guía](docs/GUIDE.es.md#tokens-de-layout).

---

## Reglas

Una regla elige a qué loggers se aplica por **categoría**, mediante su `name`:

- `*` —y un `name` **ausente o vacío**— empareja **todas** las categorías.
- Cualquier otro valor es una **expresión regular**, anclada a la categoría **completa**: el patrón
  tiene que emparejar la categoría entera, así que `Daf` nombra una categoría, no todas las que la
  contienen.
- La caja (mayúsculas/minúsculas) la decide el propio patrón, con un `(?i)` en línea; por defecto
  distingue mayúsculas.

Es una **divergencia de NLog**, donde `name` es un glob de dos comodines (`*` y `?`). `Daf.*` sigue
funcionando, porque `.*` y `*` coinciden ahí; un comodín `?` se escribe como `.`, y un punto literal
como `\.`.

Un `name` que no sea una expresión regular válida es un **error de composición**: la configuración
falla al cargar, nombrando la regla y el patrón.

Una regla está **activa salvo que su configuración diga lo contrario**: `enabled` vale `true` por
defecto, y solo el literal `false` la desactiva — la semántica que documenta NLog.

Qué niveles acepta una regla es la conjunción de `minLevel`, `maxLevel` y `levels`. Ausentes, valen
`Trace`, `Critical` y ninguna lista, así que una regla con `name` y `writeTo` lo acepta todo.

---

## Targets incluidos

| `targetType` | Clase | Salida |
|--------------|-------|--------|
| `Console` | `TConsoleTarget` | salida estándar, con color por nivel si se pide |
| `File` | `TFileTarget` | un fichero de log, con rotación por fecha, edad, mes o tamaño |
| `Debug` | `TDebugTarget` | la salida de depuración del sistema, en Windows |
| `UDP` | `TUDPTarget` | un endpoint UDP |

`targetType` se resuelve por RTTI a `Daf.NNLog.Targets.<Tipo>.T<Tipo>Target`, que es también cómo se
alcanza un target propio desde la configuración. Cualquier otra clave de la sección de un target se
asigna al target como una propiedad escribible de ese nombre.

El tipo tiene que sobrevivir al enlazado inteligente para que la búsqueda lo encuentre: `{$STRONGLINKTYPES ON}` en el
`.dpr`, y cada unit de target que configure nombrada en su `uses`. Un `targetType` cuya unit no está
enlazada da `Unknown target type`.

---

## Documentación

- 📖 [Guía de uso](docs/GUIDE.es.md) — composición, configuración, reglas, targets, layout, targets personalizados
