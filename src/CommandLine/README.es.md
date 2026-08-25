# Daf.CommandLine

**🌍 Idioma: [English](README.md) | Español**

Un porting de [`System.CommandLine`](https://learn.microsoft.com/dotnet/standard/commandline) de .NET
—la librería sobre la que está construido el propio CLI de `dotnet`—. Declara comandos, subcomandos,
opciones y argumentos posicionales, y lee sus valores **con los mismos objetos que declaraste**.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![Licencia](https://img.shields.io/badge/licencia-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Por qué el objeto declarado es la llave

```pascal
uses Daf.CommandLine;

var FileOpt := CommandLine.Option<string>('--file', 'Fichero a compilar');
var Root := CommandLine.Root('mitool', 'Una herramienta');
Root.Add(FileOpt);

var Parsed := Root.Parse;                  // lee ParamStr
var Path := FileOpt.ValueIn(Parsed);       // un string, estáticamente
```

`Path` es un `string`, no un `TValue`, y el nombre `--file` se escribe **una sola vez**. Un nombre
escrito dos veces —una al declarar, otra al leer— es un desajuste que ningún compilador ve, y pedir
el equivocado devuelve algo idéntico a una opción que el usuario no pasó. Con el objeto como llave
ese error deja de ser representable.

Leer un `IOption<Integer>` como `string` es un error **de compilación**, no una conversión que falla
en ejecución.

---

## Conceptos

| Concepto | Descripción |
|----------|-------------|
| `ICommand` | Un comando: sus opciones, sus argumentos posicionales y sus subcomandos |
| `IRootCommand` | Donde empieza una gramática; el único que hace `Parse`, y el que lleva la versión |
| `IOption<T>` | Un valor con nombre y guiones —`--file main.pas`—. Y la llave con la que se lee |
| `IArgument<T>` | Un valor **posicional**, identificado por su sitio: `mitool build main.pas` |
| `TArity` | Cuántos valores admite un símbolo, mínimo y máximo |
| `ICommandLineParseResult` | Lo que produjo una invocación: la hoja alcanzada, los valores, los errores, la ayuda, la versión |
| `CommandLine` | El punto de entrada: `Option<T>`, `Argument<T>`, `Command`, `Root` |

---

## Declarar una gramática

Los métodos fluidos viven **en las interfaces**, nunca en un `class helper`: solo un helper por tipo
está en ámbito y los helpers no aplican a interfaces, así que un API fluida repartida entre units se
rompería —en silencio, ganando el último `uses`— en cuanto alguien reordenara una cláusula.

```pascal
var VerboseOpt := CommandLine.Option<Boolean>('--verbose', 'Cuenta más').Recursive;
var OutOpt     := CommandLine.Option<string>('--out', 'Dónde va').WithDefault('bin\out.exe');
var LevelOpt   := CommandLine.Option<TVerbosity>('--level', 'Cuánto contar');
var FilesOpt   := CommandLine.Option<string>('--file', 'Ficheros').WithArity(TArity.ZeroOrMore);
var KeyOpt     := CommandLine.Option<string>('--key', 'Clave de API').Required;
var TargetArg  := CommandLine.Argument<string>('target', 'El proyecto a construir');

var Native := CommandLine.Command('native', 'Construye un binario nativo').Add([TargetArg, OutOpt]);
var Build  := CommandLine.Command('build', 'Construye cosas').WithAlias('b').Add(Native);

var Root := CommandLine.Root('buildtool', 'Una herramienta pequeña').WithVersion('1.0.0');
Root.Add([VerboseOpt, Build]);
```

`Add` acepta un símbolo o un array abierto de ellos — lo más cercano que Delphi tiene al
*collection initializer* de C#.

| Capacidad | Cómo |
|-----------|------|
| Subcomandos, sin límite de profundidad | `Padre.Add(Hijo)`. Un comando que solo agrupa exige que se nombre una hoja |
| Argumentos posicionales | `CommandLine.Argument<T>(...)`, ligados por su sitio en orden de declaración |
| Alias de comando | `.WithAlias('b')` — `mitool b native` alcanza `mitool build native` |
| Opciones recursivas | `.Recursive` — declarada una vez, disponible en todos sus descendientes |
| Valores por defecto | `.WithDefault(Valor)`, devuelto cuando la invocación no traía la opción |
| Requerido | `.Required` en una opción; un argumento lo es por su aridad |
| Aridad | `.WithArity(TArity.ZeroOrMore)` y las otras cuatro |
| Valores acotados | una `T` enumerada, o `.AcceptOnly(['fast', 'safe'])` |
| Versión | `.WithVersion('1.0.0')` en la raíz |

---

## Leer valores

```pascal
var Path  := OutOpt.ValueIn(Parsed);        // un valor, con su tipo estático
var Files := FilesOpt.ValuesIn(Parsed);     // TArray<string>, para una aridad que admite varios
```

`IParseResult.WasProvided` distingue **ausente** de **presente pero vacía**: `--file ''` no es la
misma respuesta que no pasar `--file`.

---

## La aridad, y por qué no es un detalle

La aridad es un mínimo y un máximo, y **se deriva del tipo** para que el caso normal no haya que
escribirlo: un `Boolean` admite cero o uno, cualquier otro tipo exige exactamente uno.

| | Significa |
|---|---|
| `TArity.Zero` | ningún valor |
| `TArity.ZeroOrOne` | cero o uno — el de un `Boolean` |
| `TArity.ExactlyOne` | uno, y tiene que estar — el de todo lo demás |
| `TArity.ZeroOrMore` | cualquier cantidad |
| `TArity.OneOrMore` | al menos uno |

Compra tres cosas que parecían independientes:

- **Varios valores.** Un máximo mayor que uno es lo que hace que `--file a.pas --file b.pas` valga
  por los dos.
- **Requerido, en los argumentos.** Un mínimo de uno **es** requerido; no hace falta un segundo
  concepto.
- **Se acaba el último-gana silencioso.** Con máximo uno, repetir la opción es un **error**. Quedarse
  callado con el último es un valor que elige el parser por su cuenta — el mismo pecado que leer
  `80xyz` como `80`.

En las **opciones**, `Required` es una marca aparte, como en .NET: una opción de un valor puede ser
obligatoria o no, y la aridad no lo dice.

---

## Valores

Una conversión consume el token **entero** o falla. `--port 80xyz` se rechaza nombrando la opción y
el texto recibido, en vez de valer `80`; y `-5` es un valor, no una opción desconocida.

Tipos: `string`, `Integer`, `Boolean`, `Double` y **tipos enumerados**. Un enumerado nombra sus
propios valores aceptados, así que salen en la ayuda y en el error sin que nadie los escriba dos
veces — y se comparan sin distinguir mayúsculas, como en .NET. `.AcceptOnly([...])` es el mismo
cierre para un tipo que no lo expresa.

Una opción `Boolean` es un flag: basta con nombrarla, y valen `--flag false`, `--flag=false` y
`--flag true`.

`--` termina las opciones: todo lo que venga detrás es valor, con guiones y todo. Sin eso, un
posicional cuyo valor sea `--verbose` no se podría pasar.

---

## La ayuda, `--help` y `--version`

La ayuda se genera recorriendo la gramática cada vez que se pide. No se guarda texto compuesto en
ningún sitio, así que no puede desajustarse de lo que el programa acepta: añade una opción y aparece.
Lo único que declaras es cada descripción; los valores aceptados, los defaults y qué símbolos son
obligatorios salen de la propia gramática.

```
$ buildtool --help
A small tool that builds and serves things

Usage:
  buildtool <command> [options]

Commands:
  build, b              Build things
  serve                 Serve what was built

Options:
  --verbose             Say more about what is happening
  --help, -h            Show help and usage information
  --version             Show version information
```

Toda gramática responde a `--help` y `-h`, y toda **raíz** responde a `--version`, sin declarar nada.
Pedir cualquiera de las dos es un **resultado válido**, no un fallo: `HelpRequested` /
`VersionRequested` son ciertos y `Errors` está vacío. La ayuda se resuelve *antes* de validar la
gramática, porque una herramienta que exige acertar la sintaxis para explicar la sintaxis no sirve de
nada. Una aplicación que declare su propio `--help` o `--version` gana: es la que conoce su caso. Y
`--version` va en la raíz y no en cada subcomando, como en .NET: la versión es de la herramienta.

---

## Divergencias con `System.CommandLine`, cada una con su causa

**1 · El valor se lee al revés.** En .NET se escribe `result.GetValue(fileOption)`. Aquí,
`fileOption.ValueIn(parsed)`: el objeto le pregunta al resultado. Medido el 2026-08-24, Delphi 37:

- Una interfaz **no puede declarar un método genérico**, punto — `E2535 Interface methods must not
  have parameterized methods`. Así que `ICommandLineParseResult.GetValue<T>` no puede existir, fuera
  cierto lo demás o no.
- Y ni siquiera en una clase infiere Delphi la `T` a través de un parámetro `IOption<T>` — `E2010
  Incompatible types: 'IOption<T>' and 'IOption<string>'`. La inferencia funciona cuando el parámetro
  **es** `T`, no cuando `T` va anidada dentro de otro genérico.

La llamada de .NET costaría renunciar a las interfaces contadas **y además** escribir `<string>` a
mano en cada lectura — reintroduciendo justo la repetición que este módulo existe para quitar.
`ValueIn` no necesita inferencia porque `T` ya viene fijada por el receptor.

**2 · Los nombres se comparan sin distinguir mayúsculas.** .NET es **sensible** por convención POSIX;
aquí `--OUT` y `--out` son la misma opción. La causa es el entorno, no la comodidad: Delphi es un
lenguaje insensible a mayúsculas y las CLI de Windows lo son por costumbre, así que una herramienta
DAF que distinguiera `--Out` de `--out` sorprendería a su propio público. El coste aceptado: dos
alias que solo difieran en mayúsculas son el mismo alias, y una CLI donde el caso **es** el
significado —`git clean -x` frente a `-X`— no se puede expresar. Para eso, dos opciones.

---

## Lo que todavía no está

Nada de lo de abajo cambia la forma del API, así que todo es aditivo:

- **Tipos y validación** — el resto de lo que .NET enlaza (`Int64`, `DateTime`, `Guid`, `FileInfo`,
  arrays), validadores propios, parsers propios y defaults calculados por lambda.
- **Conveniencias de sintaxis** — agrupar `-fdx`, el delimitador `:`, el prefijo `/opt`, `-vquiet`
  sin delimitador y los ficheros de respuesta `@fichero.rsp`.

**No hay invocación, y esa sí es una decisión y no un hueco.** `SetAction` / `Invoke` —despachar a la
hoja y ejecutar su handler— quedan fuera a propósito: eso convierte la librería en el runner de la
CLI entera y arrastra códigos de salida, cancelación y middleware. `Daf.CommandLine` parsea y entrega
valores tipados; qué hacer con ellos lo decide la aplicación. Las directivas `[diagram]` y `[suggest]`
y el autocompletado van montados sobre la invocación, así que salen con ella.

---

## No confundir con configuración

Leer la línea de comandos como **fuente de configuración** —`--Logging:LogLevel=Debug` sobrescribiendo
un valor de `appsettings.json`— es otro trabajo y pertenece a
[`Daf.Configuration.CommandLine`](../Configuration/README.es.md), igual que
`Microsoft.Extensions.Configuration.CommandLine` no usa `System.CommandLine`.

[`Daf.CmdLn`](../CmdLn/README.es.md) es el módulo anterior, congelado, al que este sustituye. No hay
capa de adaptación ni migración automática: son APIs distintas a propósito, y una capa que las uniera
conservaría precisamente lo que se está sustituyendo.

---

## Sample

[`samples/BuildTool`](samples/BuildTool) declara una gramática de dos niveles y lee sus valores.
Ejecútalo sin argumentos y recorre un conjunto de invocaciones, incluidas las cuatro que fallan.
