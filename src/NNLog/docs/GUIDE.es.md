# NNLog — Guía de uso

**🌍 Idioma: [English](GUIDE.md) | Español**

---

## Tabla de contenidos

1. [Arquitectura](#arquitectura)
2. [TLogRule](#tlogrule)
3. [TTarget](#ttarget)
4. [Tokens de layout](#tokens-de-layout)
5. [TNNLogProvider](#tnnlogprovider)
6. [Targets incluidos](#targets-incluidos)
7. [Múltiples targets y reglas](#múltiples-targets-y-reglas)
8. [Crear un target personalizado](#crear-un-target-personalizado)
9. [Integración con ILoggingBuilder](#integración-con-iloggingbuilder)

---

## Arquitectura

```
ILogger.LogXxx(...)
        │
        ▼
  TNNLogLogger (por categoría)
        │
        │  por cada TLogRule que coincida:
        ├─► Target A  (Consola)
        ├─► Target B  (Fichero)
        └─► Target C  (Personalizado)
```

Cada `TNNLogLogger` evalúa todas las reglas en orden. Cuando una regla coincide — patrón de categoría + nivel de log — escribe el `TLogEntry` a **todos** los targets listados en `Rule.WriteTo`. Si una regla tiene `IsFinal = True`, la evaluación se detiene cuando se dispara.

---

## TLogRule

```pascal
TLogRule = class
  Name     : string;               // patrón de nombre de categoría (* y ? wildcards)
  MinLevel : TLogLevel;            // nivel mínimo que coincide (por defecto Trace)
  MaxLevel : TLogLevel;            // nivel máximo que coincide (por defecto Critical)
  Levels   : TArray<TLogLevel>;    // lista explícita de niveles (anula Min/Max cuando está establecida)
  WriteTo  : TArray<TTarget>;      // targets a los que escribir
  Enabled  : Boolean;              // por defecto True
  IsFinal  : Boolean;              // detener la evaluación cuando se dispara esta regla
end;
```

### Coincidencia de patrones

| Patrón | Coincide con |
|--------|-------------|
| `*` | Todas las categorías |
| `MyApp.*` | Cualquier categoría que empiece por `MyApp.` |
| `MyApp.Services.UserService` | Coincidencia exacta |
| `*Repository` | Cualquier categoría que termine en `Repository` |

---

## TTarget

Clase base abstracta para todos los destinos de salida de NNLog.

```pascal
TTarget = class abstract
  Name   : string;
  Layout : string;
  procedure Write(const Entry: TLogEntry); virtual; abstract;
end;
```

Sobreescribe `Write` para producir cualquier formato de salida o destino.

---

## Tokens de layout

| Token | Valor |
|-------|-------|
| `${timestamp}` | Timestamp de la entrada (`yyyy-mm-dd hh:nn:ss.zzz`) |
| `${pid}` | ID del proceso actual |
| `${thread}` | ID del hilo actual |
| `${level}` | Nombre del nivel (`TRACE`, `DEBUG`, `INFO`, `WARN`, `ERROR`, `FATAL`) |
| `${category}` | Cadena de categoría del logger |
| `${message}` | Cuerpo del mensaje renderizado |
| `${exception}` | `ClaseExcepcion: Mensaje` — cadena vacía cuando `Entry.Exception = nil` |
| `${scope}` | Cadena de scope renderizada (vacía cuando no hay scope activo) |

### Layout por defecto

```
${timestamp} #${pid}:${thread} [${level}] ${category} | ${message} ${exception}
```

---

## TNNLogProvider

```pascal
TNNLogProvider = class(TInterfacedObject, ILoggerProvider)
  constructor Create(Targets: TArray<TTarget>; Rules: TArray<TLogRule>);
  function CreateLogger(const Category: string): ILogger;
end;
```

`TNNLogProvider` es propietario de todos los `TTarget` y `TLogRule` que se le pasan.

---

## Targets incluidos

| Clase | Unidad | Salida |
|-------|--------|--------|
| `TNNLogConsoleTarget` | `DAF.NNLog.Targets.Console` | `stdout` con codificación de color ANSI por nivel |
| `TNNLogFileTarget` | `DAF.NNLog.Targets.File` | Fichero de log fijo o rotatorio |

---

## Múltiples targets y reglas

```pascal
var File := TNNLogFileTarget.Create;
File.Name     := 'file';
File.FileName := 'app.log';
File.Layout   := '${timestamp} [${level}] ${message} ${exception}';

var Console := TNNLogConsoleTarget.Create;
Console.Name := 'console';

// Regla 1: debug+ solo al fichero
var R1 := TLogRule.Create;
R1.Name     := '*';
R1.MinLevel := TLogLevel.Debug;
R1.WriteTo  := [File];

// Regla 2: warning+ también a consola (IsFinal detiene la evaluación)
var R2 := TLogRule.Create;
R2.Name     := '*';
R2.MinLevel := TLogLevel.Warning;
R2.WriteTo  := [Console];
R2.IsFinal  := True;

var Provider := TNNLogProvider.Create([File, Console], [R1, R2]);
```

---

## Crear un target personalizado

1. Hereda de `TTarget`:

```pascal
type
  TSlackTarget = class(TTarget)
  private
    FWebhookUrl: string;
  public
    constructor Create(const WebhookUrl: string);
    procedure Write(const Entry: TLogEntry); override;
  end;

procedure TSlackTarget.Write(const Entry: TLogEntry);
var Payload: string;
begin
  if Entry.Level < TLogLevel.Warning then Exit;
  Payload := Format('{"text": "%s"}', [RenderLayout(Entry)]);
  // HTTP POST a FWebhookUrl
end;
```

2. Registra como cualquier otro target:

```pascal
var Slack := TSlackTarget.Create('https://hooks.slack.com/…');
Slack.Name := 'slack';

var Rule := TLogRule.Create;
Rule.Name     := '*';
Rule.MinLevel := TLogLevel.Error;
Rule.WriteTo  := [Slack];

Provider := TNNLogProvider.Create([Slack], [Rule]);
```

---

## Integración con ILoggingBuilder

```pascal
uses DAF.Logging.Builder, DAF.NNLog, DAF.NNLog.Targets.Console;

AddLogging(Services,
  procedure(Builder: ILoggingBuilder)
  var
    Target : TNNLogConsoleTarget;
    Rule   : TLogRule;
  begin
    Target           := TNNLogConsoleTarget.Create;
    Target.Name      := 'console';

    Rule          := TLogRule.Create;
    Rule.Name     := '*';
    Rule.MinLevel := TLogLevel.Information;
    Rule.WriteTo  := [Target];

    Builder.AddProvider(TNNLogProvider.Create([Target], [Rule]));
  end);
```
