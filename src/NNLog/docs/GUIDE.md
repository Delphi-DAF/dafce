# NNLog — Usage Guide

**🌍 Language: English | [Español](GUIDE.es.md)**

---

## Table of Contents

1. [Architecture](#architecture)
2. [TLogRule](#tlogrule)
3. [TTarget](#ttarget)
4. [Layout tokens](#layout-tokens)
5. [TNNLogProvider](#tnnlogprovider)
6. [Built-in targets](#built-in-targets)
7. [Multiple targets and rules](#multiple-targets-and-rules)
8. [Creating a custom target](#creating-a-custom-target)
9. [Integration with ILoggingBuilder](#integration-with-iloggingbuilder)

---

## Architecture

```
ILogger.LogXxx(...)
        │
        ▼
  TNNLogLogger (per category)
        │
        │  for each TLogRule that matches:
        ├─► Target A  (Console)
        ├─► Target B  (File)
        └─► Target C  (Custom)
```

Each `TNNLogLogger` evaluates every rule in order. When a rule matches — category pattern + log level — it writes the `TLogEntry` to **all** targets listed in `Rule.WriteTo`. If a rule has `IsFinal = True`, evaluation stops after it fires.

---

## TLogRule

```pascal
TLogRule = class
  Name     : string;               // category name pattern (* and ? wildcards)
  MinLevel : TLogLevel;            // lowest level that matches (default Trace)
  MaxLevel : TLogLevel;            // highest level that matches (default Critical)
  Levels   : TArray<TLogLevel>;    // explicit level list (overrides Min/Max when set)
  WriteTo  : TArray<TTarget>;      // targets to write to
  Enabled  : Boolean;              // default True
  IsFinal  : Boolean;              // stop evaluating after this rule fires
end;
```

### Pattern matching

| Pattern | Matches |
|---------|---------|
| `*` | All categories |
| `MyApp.*` | Any category starting with `MyApp.` |
| `MyApp.Services.UserService` | Exact match |
| `*Repository` | Any category ending with `Repository` |

---

## TTarget

Abstract base class for all NNLog output destinations.

```pascal
TTarget = class abstract
  Name   : string;
  Layout : string;
  procedure Write(const Entry: TLogEntry); virtual; abstract;
end;
```

Override `Write` to produce any output format or destination.

---

## Layout tokens

| Token | Value |
|-------|-------|
| `${timestamp}` | Entry timestamp (`yyyy-mm-dd hh:nn:ss.zzz`) |
| `${pid}` | Current process ID |
| `${thread}` | Current thread ID |
| `${level}` | Level name (`TRACE`, `DEBUG`, `INFO`, `WARN`, `ERROR`, `FATAL`) |
| `${category}` | Logger category string |
| `${message}` | Rendered message body |
| `${exception}` | `ExceptionClass: Message` — empty string when `Entry.Exception = nil` |
| `${scope}` | Rendered scope chain (empty when no scope active) |

### Default layout

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

`TNNLogProvider` owns all `TTarget` and `TLogRule` instances passed to it.

---

## Built-in targets

| Class | Unit | Output |
|-------|------|--------|
| `TNNLogConsoleTarget` | `DAF.NNLog.Targets.Console` | `stdout` with ANSI colour coding by level |
| `TNNLogFileTarget` | `DAF.NNLog.Targets.File` | Rolling or fixed log file |

---

## Multiple targets and rules

```pascal
var File := TNNLogFileTarget.Create;
File.Name     := 'file';
File.FileName := 'app.log';
File.Layout   := '${timestamp} [${level}] ${message} ${exception}';

var Console := TNNLogConsoleTarget.Create;
Console.Name := 'console';

// Rule 1: debug+ to file only
var R1 := TLogRule.Create;
R1.Name     := '*';
R1.MinLevel := TLogLevel.Debug;
R1.WriteTo  := [File];

// Rule 2: warnings+ also to console (IsFinal stops further evaluation)
var R2 := TLogRule.Create;
R2.Name     := '*';
R2.MinLevel := TLogLevel.Warning;
R2.WriteTo  := [Console];
R2.IsFinal  := True;

var Provider := TNNLogProvider.Create([File, Console], [R1, R2]);
```

---

## Creating a custom target

1. Inherit from `TTarget`:

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
  // HTTP POST to FWebhookUrl
end;
```

2. Register like any other target:

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

## Integration with ILoggingBuilder

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
