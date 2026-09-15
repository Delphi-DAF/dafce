# Daf.Cli

**🌍 Idioma: [English](README.md) | Español**

El **miembro CLI** de la familia de aplicaciones de DAF. Porta la forma de
[`Host.CreateApplicationBuilder`](https://learn.microsoft.com/dotnet/core/extensions/generic-host)
de .NET y añade lo que una herramienta de consola necesita de verdad y allí queda repartido con
`System.CommandLine`: una gramática, y el **borde alrededor del `Run`**.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Todo, de una vez

<!-- quickstart: src/Cli/samples/GreetTool/GreetTool.dpr -->

```pascal
program GreetTool;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  System.SysUtils,
  Daf.Extensions.DependencyInjection,
  Daf.CommandLine,
  Daf.Configuration.CommandLine,
  Daf.Cli;

// Una herramienta CLI de la familia, con gramatica de verdad. Lo que enseña es el BORDE del Run:
// --version y --help los contesta el parse, una invocacion que no encaja sale con su propio
// codigo, y lo que se escape aplica la politica declarada. Ninguna de esas cuatro lineas se
// escribe en el .dpr: se declaran.
//
//   GreetTool --name Jorge      -> 0, saluda
//   GreetTool --name Jorge --shout -> 0, saluda gritando
//   GreetTool --version         -> 0, la version
//   GreetTool --help            -> 0, la ayuda
//   GreetTool                   -> 2, falta un argumento obligatorio
//   GreetTool --name boom       -> 7, la politica traduce la excepcion
begin
  var NameOpt := CommandLine.Option<string>('--name', 'A quien saludar').Required;
  var ShoutOpt := CommandLine.Option<Boolean>('--shout', 'En mayusculas').WithAlias('-s');

  var B := TCliApplication.CreateBuilder(CommandLineConfig.ProcessArgs);
  B.RootCommand.WithVersion('2.0.0').Add([NameOpt, ShoutOpt]);

  // Solo una instancia: la segunda no llega a construir el contenedor.
  B.SingleInstance('daf-greet-tool');

  // La politica es un callback, asi que una suite puede compartir la suya sin heredar nada mas.
  B.OnUnhandled(
    function(const E: Exception; const Services: IServiceProvider): Integer
    begin
      Writeln(ErrOutput, 'greet: ', E.Message);
      Result := 7;
    end);

  B.OnExecute(
    function(const Services: IServiceProvider; const Parsed: ICommandLineParseResult): Integer
    begin
      var Who := NameOpt.ValueIn(Parsed);
      if Who = 'boom' then
        raise Exception.Create('ese nombre no me gusta');

      var Greeting := 'Hola, ' + Who;
      if ShoutOpt.ValueIn(Parsed) then
        Greeting := Greeting.ToUpper;

      Writeln(Greeting);
      Result := TCliExit.Ok;
    end);

  ExitCode := B.Build.Run;
end.
```

`--version`, `--help`, una opción mal escrita y lo que se escape del trabajo ya están resueltos: los
tres primeros los contesta la **gramática**, el último lo toma la política declarada, y `Run`
traduce cada desenlace a un código de salida. Ninguna de esas cuatro líneas se escribe en el
`.dpr`.

---

## Los samples

| Sample | Qué usa |
|---|---|
| [`QuickStart`](samples/QuickStart/) | el builder, el trabajo y el código de salida, y nada más |
| [`ServiceWiring`](samples/ServiceWiring/) | lo mismo, más un servicio registrado y resuelto |
| [`GreetTool`](samples/GreetTool/) | la gramática, `--help`, códigos de salida por política — el bloque de arriba |
| [`ConsoleSample`](samples/ConsoleSample/) | el estilo inmediato: el entorno compone la ruta del `appsettings.json` antes de construir nada, y `IOptions<T>` llega a un servicio por constructor |
| [`PackageInit`](samples/PackageInit/) | un paquete que registra su propia `IStartupTask`, y el arranque no nombra ni la tarea ni lo que hace |

---

## El estilo inmediato

El builder es utilizable **en cuanto se crea**: `Configuration` y `Environment` ya responden, así que
se puede elegir una fuente a partir de lo ya configurado sin ningún callback diferido.

```pascal
var B := TCliApplication.CreateBuilder(Args);

// El entorno ya responde -- este es el caso por el que existía el estilo por callback.
JsonConfig.AddFile(B.Configuration, TPath.Combine(B.Environment.BinPath, 'appsettings.json'));

if B.Configuration.Find('Features:Beta').GetValueOrDefault('') = 'on' then
  B.Services.AddModule<TBetaModule>;
```

DAF tiene **un solo** estilo de builder, y es este. El diferido —registrar por callbacks de
`ConfigureServices`— se retiró: .NET lo conserva solo por compatibilidad hacia atrás, y dos formas de
expresar lo mismo son el defecto, no la comodidad.

---

## El borde alrededor del `Run`

Esto es lo que la capa añade sobre el host, y lo que cada herramienta de consola se reescribía a mano:

| Miembro | Qué decide | Cuándo |
|---|---|---|
| `SingleInstance(Id)` | Si el proceso llega a arrancar | **Antes** de construir el contenedor |
| `OnUnhandled(Policy)` | Qué pasa con lo que se escapa, y con qué código de salida | Alrededor del `Run` |
| `OnExecute(Handler)` | El trabajo, escrito en el sitio | Dentro del `Run` |
| `ICliHandler` registrado en `Services` | El trabajo, resuelto del contenedor | Dentro del `Run` |
| `RootCommand` | La gramática, dueña de `argv` | En el `Build` |

Declarar el trabajo por las **dos** vías levanta `ECliCompositionError`. Dos formas de decir lo mismo
son una regla de precedencia que nadie recuerda.

### Códigos de salida

| Código | Significado |
|---|---|
| `TCliExit.Ok` (0) | El trabajo terminó bien, o contestaron `--version` / `--help` |
| `TCliExit.Failure` (1) | Lo que devuelve `ExitPolicy.LogAndFail` ante una excepción no capturada |
| `TCliExit.InvocationError` (2) | La invocación no encaja con la gramática declarada |
| `TCliExit.AlreadyRunning` (3) | Ya hay otra instancia con ese identificador |

`Run` devuelve un `Integer`, y no es adorno: una aplicación CLI que no puede decirle al sistema cómo
terminó no sirve en un script.

---

## Dos lectores de `argv`, y un solo dueño

`TCommandLineConfigurationProvider` y la gramática leen los dos el mismo `argv`, con reglas
**distintas**, y discrepan por construcción. Sobre `mytool --verbose informe.txt`:

- la gramática ve la bandera `--verbose` y el posicional `informe.txt`;
- el proveedor de configuración ve `verbose = informe.txt`.

Las dos contestan sin error, y una de las dos es falsa. **En esta variante la gramática es la dueña
de `argv`**, y el builder no añade los argumentos como fuente de configuración por su cuenta. Quien
quiera esa vista la pide, sabiendo que conviven:

```pascal
CommandLineConfig.AddArgs(B.Configuration, CommandLineConfig.ProcessArgs);
```

Las variantes Web, VCL y FMX —que no declaran gramática— sí los añaden: allí no hay con qué chocar.

---

## Dónde diverge de .NET

| .NET | Aquí | Por qué |
|---|---|---|
| Métodos de extensión (`AddX`, `UseX`) | Métodos del propio record, o fachada `class` | Delphi no tiene métodos de extensión, y solo hay **un** `record helper` visible por tipo — una API fluida repartida entre units se rompe en silencio en cuanto alguien reordena un `uses` |
| `IHostApplicationBuilder.Metrics` | **No se porta** | No hay pieza equivalente en DAF a la que apuntar |
| `ConfigureContainer<T>` | **No se porta** | Un método genérico no es expresable en una interfaz de Delphi — no es que se descarte, es que no cabe |
| Despacho de handler dentro de `System.CommandLine` | `OnExecute` / `ICliHandler` | `Daf.CommandLine` parsea y no despacha, a propósito: el desenlace de una invocación es un dato |
| Sin global de aplicación | Igual | En consola no hay ningún `Application` global que el usuario espere: el trabajo recibe el `IServiceProvider` que necesita, y un `ICliHandler` registrado toma sus dependencias por constructor |

La última fila es la que separa esta variante de las de escritorio: **VCL y FMX sí ofrecen un
global**, porque su plataforma ya tiene uno.

---

## Dependencias del paquete

`Daf.Cli` requiere `Daf.Hosting` y `Daf.CommandLine`. Aquí el parser **no es opcional**: `--version`
y `--help` los contesta la gramática, así que toda aplicación CLI de la familia enlaza el parser —
el mismo precio que paga .NET.
