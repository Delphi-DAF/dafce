# NNLog — Usage Guide

**🌍 Language: English | [Español](GUIDE.es.md)**

---

## Table of Contents

1. [Architecture](#architecture)
2. [Composing a provider](#composing-a-provider)
3. [Configuration](#configuration)
4. [TLogRule](#tlogrule)
5. [Category patterns](#category-patterns)
6. [TTarget](#ttarget)
7. [Layout tokens](#layout-tokens)
8. [Built-in targets](#built-in-targets)
9. [Several targets and rules](#several-targets-and-rules)
10. [Creating a custom target](#creating-a-custom-target)
11. [Integration with ILoggingBuilder](#integration-with-iloggingbuilder)

---

## Architecture

```
ILogger.LogXxx(...)
        │
        ▼
  TNNLogLogger (per category)
        │
        │  for each of ITS rules that accepts the level:
        ├─► Target A  (Console)
        ├─► Target B  (File)
        └─► Target C  (Custom)
```

The category is matched **once**, when `TNNLogProvider.CreateLogger` builds the logger: the logger
keeps only the rules whose pattern matches its category. Per log entry what remains is the level
check — `MinLevel`, `MaxLevel`, `Levels` — and `Enabled`.

A rule that accepts the entry writes it to **all** the targets in its `WriteTo`. With
`IsFinal = True` the logger stops after that rule; otherwise it goes on to the next one, so one
entry can reach several rules.

`ILogger.IsEnabled(Level)` answers with the same predicate the write uses: it is `True` when some
enabled rule of that logger accepts the level.

---

## Composing a provider

`TNNLogLoader` builds the provider from settings, and it is the entry point for every route:

```pascal
uses
  Daf.Extensions.Logging,
  Daf.NNLog.Loader;

// From a configuration section
Provider := TNNLogLoader.BuildProvider(Environment, Config.GetSection('Logging:NLog'));

// From a host builder context, naming the section
Provider := TNNLogLoader.BuildProvider(Context, 'Logging:NLog');

// From settings assembled in code
Provider := TNNLogLoader.BuildProvider(Environment, Settings);
```

The first two read and own their settings. The third takes a `TNNLogConfiguration` that stays the
caller's — `BuildProvider` does not free it.

`BuildProvider` is what registers the layout renderers, resolves target types, applies the
defaults of every rule field, and reports a composition failure. **`TNNLogProvider.Create` skips
all of it**: no renderer is registered, so every `${…}` token renders as the empty string, and the
rule fields keep the values of a freshly created object — `Enabled = False` and
`MaxLevel = Trace`, which accept nothing. To build a provider that writes, go through
`TNNLogLoader`.

---

## Configuration

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

### Target keys

`targetType` picks the class; every other key is set on the target as a property of that name, so a
target's configurable surface is its writable properties. Keys are matched without regard to case.

`targetType` resolves by RTTI to `Daf.NNLog.Targets.<Type>.T<Type>Target`, **and this lookup is
case-sensitive**: `Console` finds `TConsoleTarget`, `console` does not. `File` is the one exception
to the unit name — `file` is a reserved word, so the unit is `Daf.NNLog.Targets.File_`. A
`targetType` with no class behind it is a composition failure:
`Daf.NNLog Error: Unknown target type <Type>`.

The type has to survive smart linking for RTTI to find it — the built-in targets included. Add
`{$STRONGLINKTYPES ON}` to the `.dpr` and name every target unit you configure in its `uses`:

```pascal
{$STRONGLINKTYPES ON}
uses
  Daf.NNLog.Loader,
  Daf.NNLog.Targets.Console,
  Daf.NNLog.Targets.File_,
  Daf.NNLog.Targets.UDP;
```

### Rule keys

| Key | Value | Absent |
|-----|-------|--------|
| `name` | category pattern | matches every category |
| `minLevel` | lowest level accepted | `Trace` |
| `maxLevel` | highest level accepted | `Critical` |
| `levels` | comma-separated list of levels | no list |
| `writeTo` | comma-separated list of target names | the rule writes nowhere |
| `isFinal` | `true` stops evaluation after this rule | `false` |
| `enabled` | `true`, in any case, keeps the rule enabled; any other value disables it | the rule is enabled |

The names in `writeTo` are the keys of the `targets` section, matched **exactly**: both the case and
the absence of spaces around the comma matter, and a name with no target behind it is dropped in
silence. `"udp,file"` reaches two targets; `"udp, file"` reaches one.

---

## TLogRule

```pascal
TLogRule = class
  property Name: string;                  // category pattern; '' and '*' match every category
  property MinLevel: TLogLevel;           // lowest level accepted
  property MaxLevel: TLogLevel;           // highest level accepted
  property Levels: TArray<TLogLevel>;     // when not empty, the level must also be in the list
  property WriteTo: TArray<TTarget>;      // targets this rule writes to
  property Enabled: Boolean;              // a disabled rule accepts nothing
  property IsFinal: Boolean;              // stop evaluating after this rule accepts
  function Matches(const Category: string; const Level: TLogLevel): Boolean;
end;
```

The three level fields are a **conjunction**, not a precedence: the entry's level has to be within
`MinLevel`..`MaxLevel` **and**, when `Levels` is not empty, be one of its members. `MinLevel` of
`Warning` with `Levels` of `Debug` accepts nothing.

The defaults in [Rule keys](#rule-keys) are the ones `TNNLogLoader` applies to an absent key. A `TLogRule`
built with `TLogRule.Create` starts from the values of a new object, so `Enabled` is `False` and
`MaxLevel` is `Trace`.

---

## Category patterns

A rule's `Name` is a **regular expression anchored to the whole category**: the pattern has to match
the category from end to end.

| Pattern | Matches |
|---------|---------|
| `*`, and an absent or empty name | every category |
| `MyApp\..*` | `MyApp.` followed by anything |
| `MyApp\.Services\.UserService` | that category, and only it |
| `.*Repository` | every category ending in `Repository` |
| `(?i)myapp\..*` | the same as the second row, ignoring case |

Case is decided by the pattern itself, with an inline `(?i)`; by default it is case-sensitive. An
unescaped `.` is the regex wildcard, so `MyApp.Services` also matches `MyAppXServices`.

A `Name` that is not a valid regular expression is a composition failure, reported when the
configuration loads and naming the rule and the pattern:
`Daf.NNLog Error: rule #0 has an invalid category pattern "*Repository": …`.

---

## TTarget

Base class for every NNLog destination.

```pascal
TTarget = class
  const DefaultLayout = '${timestamp} #${pid}:${thread} [${level}] ${category} | ${message} ${exception}';
  constructor Create; virtual;
  destructor Destroy; override;
  procedure Write(const Entry: TLogEntry);          // takes the lock, then calls DoWrite
  procedure DoWrite(const Entry: TLogEntry); virtual; abstract;   // protected
  function RenderEvent(Entry: TLogEntry): string;   // protected
  property Name: string;
  property Layout: string;
end;
```

A subclass overrides `DoWrite` and calls `RenderEvent` to turn the entry into a line using its own
`Layout`. `Name` is what a rule's `writeTo` refers to.

`Write` is not virtual: it takes a critical section held by that target instance and then calls
`DoWrite`, so a target written today is serialized without its author doing anything. Every logger
in the process shares the same target instances, so the target is where concurrent writes meet.

The lock is **per target instance**, so two targets never wait for each other, and it is **within
one process**: it does not order the writes of two processes. How `TFileTarget` behaves with
several processes is under [Several processes on one folder](#several-processes-on-one-folder).

### When a target cannot write

A target that cannot write does not raise towards whoever logged, and the other targets of the rule
still receive the entry. The failure goes to the
[bootstrap channel](../../Logging.Abstractions/README.md#bootstrap-channel) at `Error`, with the
exception, naming the target by its `Name` — or by its class, if it has none:
`el destino "file" no pudo escribir`.

It is reported once per run of failures. While the target keeps failing, each entry is lost and
counted, and nothing more is reported. The first entry it writes again reports, at `Warning`, how
many it lost — `el destino "file" vuelve a escribir; perdio 3 lineas` — and closes the run, so a
later failure is reported again.

A file target that cannot move its file to `history` keeps writing in the same file and tries again
on the next entry. The failure is reported once per run — `el destino "file" no pudo archivar su
fichero` — and an archive that succeeds closes the run.

---

## Layout tokens

A layout is text with `${token}` placeholders; a token may carry an argument after a colon. Token
names are matched without regard to case, and **a token with no renderer behind it is left
unsubstituted**, so a misspelled `${mesage}` shows up in the line instead of silently costing a
column.

| Token | Value |
|-------|-------|
| `${timestamp}` | the moment the line is rendered, as `yyyy-MM-dd HH:mm:ss.zzz` |
| `${date}`, `${date:format}` | render time, formatted; `yyyy-MM-dd` without an argument |
| `${level}` | level name: `TRACE`, `DEBUG`, `INFO`, `WARN`, `ERROR`, `CRIT` |
| `${category}` | the logger's category |
| `${scope}` | the scopes open around the entry, innermost first, each prefixed with ` \| `; empty outside any scope |
| `${message}` | the rendered message, without the scope |
| `${exception}` | `ExceptionClass: Message`, empty when the entry carries no exception |
| `${exception:stacktrace}` | the exception's stack trace |
| `${pid}` | current process id |
| `${thread}` | current thread id |
| `${newline}` | a line break |
| `${env:NAME}` | the `NAME` environment variable |
| `${event-properties:key}` | `key` from the entry's state, empty when it carries no such key |
| `${environment}` | `IHostEnvironment.EnvironmentName` |
| `${contentRootPath}` | `IHostEnvironment.ContentRootPath`, by default the current directory |
| `${binPath}` | `IHostEnvironment.BinPath`, the folder of the executable |
| `${ApplicationName}` | `IHostEnvironment.ApplicationName` |
| `${uppercase:text}`, `${lowercase:text}` | the literal argument, recased |

Tokens do not nest: the argument of a token is taken literally, so `${uppercase:${level}}` does not
recase the level name. Token names ignore case: `${BinPath}` and `${binPath}` are the same token.

### Default layout

```
${timestamp} #${pid}:${thread} [${level}] ${category} | ${message} ${exception}
```

It has no `${scope}`: a target without a layout of its own does not write the scopes. To write them
after the message, put `${scope}` right behind `${message}`.

`TFileTarget.FileName` goes through the same engine, so `${binPath}`, `${date}` and the rest work
in a path. `${binPath}` puts the log next to the executable whatever directory the process was
started from; `${contentRootPath}` follows the content root.

---

## Built-in targets

| Class | `targetType` | Unit | Output |
|-------|--------------|------|--------|
| `TConsoleTarget` | `Console` | `Daf.NNLog.Targets.Console` | standard output |
| `TFileTarget` | `File` | `Daf.NNLog.Targets.File_` | a log file, with rotation |
| `TDebugTarget` | `Debug` | `Daf.NNLog.Targets.Debug` | the system debug output, on Windows |
| `TUDPTarget` | `UDP` | `Daf.NNLog.Targets.UDP` | a UDP endpoint |

### TConsoleTarget

| Property | Default | Meaning |
|----------|---------|---------|
| `UseColors` | `False` | colour each line by level — Win32 console attributes on Windows, ANSI escapes elsewhere |

### TFileTarget

| Property | Default | Meaning |
|----------|---------|---------|
| `FileName` | — | path of the current file; resolved as a layout |
| `RotationPolicy` | `None` | `None`, `hourly`, `Daily`, `Days`, `Monthly`, `Size` |
| `MaxFileSize` | `0` | size in bytes that triggers rotation under the `Size` policy |
| `MaxFileAge` | `0` | age in days that triggers rotation under the `Days` policy |
| `MaxArchivedFiles` | `0` | how many rotated files to keep; older ones are removed |
| `CompressOnRotate` | `False` | zip the rotated file |
| `AutoFlush` | `True` | flush after every entry |

Rotated files go to a `history` directory beside the current one. The `Days` and `Size`
policies compare against `MaxFileAge` and `MaxFileSize`, so each one needs its threshold set:
left at `0` they rotate on every entry.

A rotated file is named after the file it archives and the moment that file was started: `app.log`
becomes `history/app_2026-10-23_093015120.log`. When `history` already holds the file with that
name, compressed or not, the rotated file is appended to it.

`MaxArchivedFiles` counts the files in `history` that come from the target's `FileName`, compressed
or not and from any of its runs; other files there are neither counted nor removed. At `0` nothing
is removed.

An enumeration value the loader does not recognise becomes the enumeration's first value, so a
misspelled `rotationPolicy` reads as `None`.

#### Several processes on one folder

A file is open for writing by one process at a time. A second process whose `FileName` resolves to
the same file cannot open it: its entries are lost, and reported as described in
[When a target cannot write](#when-a-target-cannot-write).

With `${pid}` in the file name each process writes its own file: `app-${pid}.log` gives
`app-1234.log`, `app-5678.log`. The target treats as its own the files that its `FileName` produces
with other values in its markers, in the folder its own file resolves to — so a marker in the
folder part, such as `${binPath}`, works the same. The first time it opens its file, and each time
it rotates it, it moves to `history` those whose period has expired under its `RotationPolicy`,
named like any rotated file. A file that another process still has open stays where it is, and with
`RotationPolicy` at `None` nothing is moved.

The target does not look for those files when the file name has no literal text besides its
extension — `${pid}.log`, `${date}.log`.

### TUDPTarget

| Property | Default | Meaning |
|----------|---------|---------|
| `Host` | `127.0.0.1` | destination host |
| `Port` | `12201` | destination port — Graylog's GELF default |

---

## Several targets and rules

```json
"rules": [
  { "name": "*", "writeTo": "file", "minLevel": "Debug" },
  { "name": "*", "writeTo": "console", "minLevel": "Warning", "isFinal": true }
]
```

A `Debug` entry is accepted by the first rule only and reaches the file. A `Warning` entry is
accepted by both: it reaches the file, then the console, and evaluation stops there because the
second rule is final.

Order matters twice over — rules are evaluated in the order the section lists them, and `isFinal`
cuts the rest off.

---

## Creating a custom target

Inherit from `TTarget` and override `DoWrite`:

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
  // HTTP POST of Payload to WebhookUrl
end;

end.
```

To be reachable from configuration a custom target follows the loader's convention: the unit is
`Daf.NNLog.Targets.<Type>` and the class `T<Type>Target`, and every configurable value is a
writable property. Then it is named like any other target:

```json
"targets": {
  "slack": { "targetType": "Slack", "webhookUrl": "https://hooks.slack.com/…" }
}
```

Its unit needs naming in the program's `uses` like any other target unit, for the reason given
under [Target keys](#target-keys).

Configuration passes strings, so a property the loader is to set must be of a type
`Daf.Configuration.Binder` can convert to — a string, a number, a boolean, or an enumeration named
by one of its values. A target needing anything else exposes a string property and parses it itself.

---

## Integration with ILoggingBuilder

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

The application builder exposes the environment and the configuration, which is the shape a
console application uses:

```pascal
var B := TCliApplication.CreateBuilder(CommandLineConfig.ProcessArgs);
B.Logging.AddProvider(TNNLogLoader.BuildProvider(B.Environment, B.Configuration.GetSection('Logging:NLog')));
```

`Daf.Hosting` registers an `IHostBuilderContext` carrying both, which is what the
`BuildProvider(Context, ConfigKey)` overload takes.

The provider owns every target and rule it was given, and frees them when the factory releases it.
