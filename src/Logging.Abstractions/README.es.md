# Logging.Abstractions

**🌍 Idioma: [English](README.md) | Español**

Contratos fundamentales de la infraestructura de logging de DAFce. Referencia estas interfaces al construir componentes que emiten entradas de log o al implementar proveedores personalizados — sin depender de ninguna implementación concreta de logging.

Todos los tipos se encuentran en `Daf.Extensions.Logging`.

---

## Interfaces

### ILogger

Fachada por categoría que emite entradas de log.

```pascal
ILogger = interface(IInvokable)
  function IsEnabled(const Level: TLogLevel): Boolean;

  procedure Log(const Level: TLogLevel; const Msg: string; const Args: TArray<TValue> = nil); overload;
  procedure Log(const Level: TLogLevel; const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;

  procedure LogTrace      (const Msg: string; const Args: TArray<TValue> = nil); overload;
  procedure LogDebug      (const Msg: string; const Args: TArray<TValue> = nil); overload;
  procedure LogInformation(const Msg: string; const Args: TArray<TValue> = nil); overload;
  procedure LogWarning    (const Msg: string; const Args: TArray<TValue> = nil); overload;
  procedure LogError      (const Msg: string; const Args: TArray<TValue> = nil); overload;
  procedure LogError      (const Ex: Exception; const Msg: string = ''; const Args: TArray<TValue> = nil); overload;
  procedure LogCritical   (const Msg: string; const Args: TArray<TValue> = nil); overload;
  procedure LogCritical   (const Ex: Exception; const Msg: string; const Args: TArray<TValue> = nil); overload;

  function BeginScope(const Msg: string; const Args: TArray<TValue>;
                      const ScopedProc: TProc = nil): ILogScopeVoid; overload;
end;
```

Los argumentos viajan como `TArray<TValue>`, así que `['a', 1, True]` conserva el tipo de cada valor
en vez de aplanarlo a un `TVarRec`. Cada uno de estos tiene además una sobrecarga que recibe un
`TEventId` delante.

`IsEnabled(Level)` contesta si una entrada de ese nivel llegaría a algún destino. Pregúntalo antes de
construir argumentos que cuesten más que la propia línea —un volcado de estado, una consulta, un
recorrido—, porque en Delphi los argumentos se evalúan en la llamada, así que `LogDebug(Volcado)` paga
`Volcado` aunque Debug esté apagado:

```pascal
if Log.IsEnabled(TLogLevel.Debug) then
  Log.LogDebug('state = {State}', [VolcadoCaro]);
```

Un compuesto contesta que sí si **algún** destino escribiría; un destino que no sabe contesta que sí,
porque perder una línea por no saber es peor que componer una de más. Es el `ILogger.IsEnabled` de
`Microsoft.Extensions.Logging`.

---

### ILoggerFactory

Crea instancias de `ILogger`; el logger que devuelve escribe en todos los proveedores registrados.

```pascal
ILoggerFactory = interface(IInvokable)
  function CreateLogger(const Category: string): ILogger; overload;
  function CreateLogger(const AClass: TClass): ILogger; overload;
  function CreateLogger(const AType: PTypeInfo): ILogger; overload;
end;
```

`CreateLogger(TMiClase)` toma la categoría de la clase — su unit y su nombre. Los proveedores se
añaden por `ILoggingBuilder`, no aquí.

---

### ILogger&lt;T&gt;

El registrador que un servicio pide nombrándose a sí mismo. No añade ningún método a `ILogger`; lo
que añade es que su **categoría ya es la de quien lo pidió**:

```pascal
type
  TPayrollService = class(TInterfacedObject, IPayroll)
  strict private
    FLog: ILogger<TPayrollService>;
  public
    constructor Create(const Log: ILogger<TPayrollService>);
  end;

procedure TPayrollService.Run;
begin
  FLog.LogInformation('la nómina se ejecutó');   // sale bajo Payroll.TPayrollService
end;
```

No hay nada registrado para ese servicio, ni lo habrá para el siguiente. `AddLogging` pone **un**
registro, con su argumento de tipo sin fijar, que sirve a todos los cierres — la misma forma que en
.NET se escribe `services.AddSingleton(typeof(ILogger<>), typeof(Logger<>))`. Qué es eso y qué
cuesta está en
[registrar un contrato genérico una sola vez](../DependencyInjection.Abstractions/README.es.md#registrar-un-contrato-genérico-una-sola-vez).

Lo que se gana es que la categoría no pueda quedarse atrás. Escrita a mano aparece **dos veces** —el
nombre de la clase y la cadena que la nombra— sin que nada compruebe que coinciden; y cuando la
clase se renombra, la cadena se queda. Aquí sale de la clase, así que el renombrado se la lleva
consigo y la composición no se toca.

El ciclo de vida es por tipo cerrado: `ILogger<TPayroll>` e `ILogger<TInvoicing>` son dos
registradores, y pedir dos veces cualquiera de los dos devuelve el mismo.

---

### ILoggerProvider

Produce instancias de `ILogger` para un destino de salida específico.

```pascal
ILoggerProvider = interface
  function CreateLogger(const Category: string): ILogger;
end;
```

---

### ILoggingBuilder

Interfaz fluida para configurar proveedores antes de que el host arranque (se usa dentro de `AddLogging`).

```pascal
ILoggingBuilder = interface
  procedure AddProvider(Provider: ILoggerProvider);
  procedure AddNullProvider;
  function  Build: ILoggerFactory;
end;
```

`AddNullProvider` es la forma en que una aplicación dice que quiere que sus entradas se descarten.
Componer sin ningún proveedor no es lo mismo: eso levanta `ELoggingCompositionError`, porque un
pipeline sin destino es indistinguible de uno que funciona. Ver
[Logging — Guía de uso](../Logging/docs/GUIDE.es.md#componer-sin-destino-es-un-error).

---

### ILogScope / ILogScopeVoid

Representan un scope de logging activo. `ILogScopeVoid` es la versión devuelta por `BeginScope` cuando el procedimiento de scope no devuelve ningún valor.

---

## Tipos de valor

### TLogLevel

```pascal
{$SCOPEDENUMS ON}
TLogLevel = (Trace, Debug, Information, Warning, Error, Critical, None);
```

Está scoped, así que se escribe `TLogLevel.Debug`. El texto entra y sale por un helper, que es lo que
necesita un nivel leído de un fichero de configuración:

```pascal
S      := Level.ToString;
Level  := TLogLevel.Parse('Debug');                     // lanza: Invalid log level: <texto>
Level  := TLogLevel.Parse(Raw, TLogLevel.Information);  // o un valor de respaldo, sin excepción
Levels := TLogLevel.ParseList('Debug,Error');           // separados por comas
if TLogLevel.TryParse(Raw, Level) then ...
```

⚠️ `ParseList` **descarta en silencio** lo que no sabe leer: una lista de tres con una errata vuelve
con dos niveles y sin error.

### TLogState

El mensaje que lleva una entrada: la plantilla tal y como se escribió, sus argumentos y —cuando la
plantilla los nombra— los pares que produce.

| Miembro | Qué da |
|---|---|
| `Template` | la plantilla tal cual, con sus marcadores |
| `Arguments` | `TArray<TValue>`, en el orden en que se pasaron |
| `KeyValuePairs` | `TArray<TPair<string, TValue>>`; vacío cuando la plantilla no nombra nada |
| `TryGetValue(Key, out Value)` | el valor de un marcador con nombre |
| `Formatter` | el `TLogFormatter` que convierte el estado en una línea |

`LogInformation('user {User} ordered {Count}', [Nombre, N])` produce dos pares, y `TryGetValue` es
como un layout resuelve `{User}` — es lo que llama el renderizador de NNLog. Una plantilla que no
nombra nada no produce pares, y `Arguments` los sigue teniendo en orden, que es lo que usa una
plantilla al estilo `'%s'`.

### TLogEntry

Entrada de log individual entregada al método `Write` de un proveedor.

```pascal
TLogEntry = record
  Level     : TLogLevel;
  Category  : string;
  EventId   : TEventId;
  Exception : Exception;
  Scope     : string;
  State     : TLogState;
  Formatter : TLogFormatter;
end;
```

`Message` es una **propiedad de solo lectura**, no un campo: la primera lectura pasa `Formatter`
sobre `State` y guarda el resultado. Un proveedor que escribe su propio layout a partir de `State`
no lo paga nunca.

`Message` no lleva el scope. `Scope` lleva los scopes abiertos del más interno al más externo, cada
uno precedido de ` | `, y va vacío fuera de todo scope; un proveedor que los quiera los escribe desde
ahí.

---

## Canal de arranque

Un `ILogger` disponible desde `initialization`, sin inyección de dependencias y sin configuración.
Cubre el tramo anterior a `BuildServiceProvider`, donde todavía no hay nada registrado que pueda
informar de un fallo — y solo ese tramo: sin niveles por categoría, sin layouts configurables, sin
escritura asíncrona. **No** se configura por fichero, a propósito: leer configuración para saber
dónde informar de que falló la lectura de la configuración es el mismo huevo y la misma gallina.

Cuándo recurrir a él, qué recibe cada forma de despliegue y el relevo con el pipeline están en
[Composición del arranque](../DependencyInjection/docs/STARTUP-COMPOSITION.es.md).

### IBootstrapSink

Un destino del canal. Recibe una **línea ya formada** —nunca un `TLogEntry`—, así que no puede
inventarse su propio formato ni filtrar: no ve nada que se lo permita.

```pascal
IBootstrapSink = interface(IInvokable)
  function IsAvailable: Boolean;
  procedure Write(const Line: string);
end;
```

Un destino que no puede escribir **se declara no disponible**; nunca acepta una línea que va a
perder en silencio. `IsAvailable` se contesta aquí y ahora, no una vez al registrarse: el handle de error
de un proceso sin consola, o una ruta que dejó de ser escribible, cambian la respuesta.

### BootstrapLog

El canal en sí: el registro de destinos y el reparto. **Nunca lanza** — un canal cuya razón de ser
es informar de un fallo no puede provocar un segundo fallo.

```pascal
BootstrapLog = record
  class procedure AddSink(const Sink: IBootstrapSink); static;
  class procedure RemoveSink(const Sink: IBootstrapSink); static;
  class procedure ClearSinks; static;
  class function Sinks: TArray<IBootstrapSink>; static;
  class function AvailableSinks: TArray<IBootstrapSink>; static;
  class function HasAvailableSink: Boolean; static;
  class procedure Write(const Line: string); static;
  class function FormatLine(const Entry: TLogEntry): string; static;
  class function LoggerFor(const Category: string): ILogger; static;
end;
```

| Miembro | Responde |
|---------|----------|
| `AddSink` `RemoveSink` `ClearSinks` | Los destinos se añaden y se quitan desde `initialization` o desde el main, antes de construir el host. Añadir es aditivo: los informes de DAF siguen saliendo por los suyos |
| `Sinks` | Todos los destinos registrados |
| `AvailableSinks` `HasAvailableSink` | Los que pueden escribir **ahora mismo**. Que esté vacío es el caso peor —nadie va a enterarse de nada— y hay que poder distinguirlo de haber informado |
| `Write` | Reparte una línea ya formada a los destinos disponibles |
| `FormatLine` | El layout del canal, aplicado antes de repartir para que todos los destinos escriban lo mismo |
| `LoggerFor` | Un `ILogger` de arranque con la categoría **de quien llama**: la aplicación informa de su propio arranque sin disfrazarse de DAF. Es la única vía, y los informes de DAF salen también por ella |

El canal mantiene un umbral de `Warning` para arriba.

### Los destinos que viven aquí

| Clase | Escribe en | Registrado por defecto |
|-------|------------|------------------------|
| `TDebugOutputBootstrapSink` | la salida de depuración del sistema — en Windows, `OutputDebugString`, que es del sistema y no del depurador, así que se captura sin uno conectado | sí |
| `TStdErrBootstrapSink` | la salida de error del proceso. Una aplicación sin consola no tiene handle de error, y ahí el destino se declara no disponible; no lanza al escribir | sí |
| `TFileBootstrapSink` | el fichero que se le nombra en el constructor, releíble por su propiedad `FileName` | **no** — una ruta duradera la conoce la aplicación, no el framework |

`TPlatformLogBootstrapSink`, sobre `IFMXLoggingService`, vive en el paquete `Daf.FMX` para que
FireMonkey alcance solo a quien lo referencie — ver [Daf.FMX](../FMX/README.md).

---

## Qué vive aquí, y por qué

Este es el paquete de contratos, y sin embargo también tiene clases concretas: `TNullLogger`,
`TNullLoggerFactory`, `TNullLoggerProvider` y los destinos del canal de arranque
(`TDebugOutputBootstrapSink`, `TStdErrBootstrapSink`, `TFileBootstrapSink`). Parece una contradicción,
así que aquí queda escrita la regla que decide dónde va cada pieza de logging:

> **Una frontera de paquete se gana aislando una dependencia que los demás no necesitan.**

La regla responde en las dos direcciones, y los dos casos ya resueltos son el ejemplo:

| Pieza | Dependencia que arrastra | Dónde vive |
|-------|--------------------------|------------|
| `TStdErrBootstrapSink`, `TDebugOutputBootstrapSink`, `TFileBootstrapSink` | nada más que la RTL | aquí, con el contrato |
| `TPlatformLogBootstrapSink` | FireMonkey | `Daf.FMX` — paquete propio |
| `TNullLogger`, `TNullLoggerFactory`, `TNullLoggerProvider` | nada | aquí, con el contrato |

Un destino que no necesita más que la biblioteca estándar no se gana una frontera; uno que se apoya en
un framework gráfico sí, y por eso el de FireMonkey tiene paquete propio. .NET pone `NullLogger` en
`Microsoft.Extensions.Logging.Abstractions` por la misma razón: un default sin dependencias que todos
necesitan.

Y hay una segunda razón que solo aplica a los destinos del canal de arranque: **sacarlos a otro
paquete los desregistraría por defecto.** Una aplicación que hoy obtiene la salida de error y la de
depuración del sistema sin pedir nada se quedaría sin ningún destino salvo que añadiera una
referencia — que es justo el caso peor que `BootstrapLog.HasAvailableSink` existe para hacer visible.

---

## Relacionado

- [Logging](../Logging/README.es.md) — implementación concreta y registro en DI
- [Logging — Guía de uso](../Logging/docs/GUIDE.es.md)
- [NNLog](../NNLog/README.es.md) — proveedor basado en reglas
