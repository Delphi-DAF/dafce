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
escrito dos veces —una al declarar, otra al leer— es un desajuste que ningún compilador ve.

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
| `ICommandLineParseResult` | Lo que produjo una invocación: la hoja alcanzada, los valores, los errores, la ayuda, la versión. `Invoke` la ejecuta |
| `TCommandAction` | Lo que hace un comando cuando una invocación lo alcanza: recibe el resultado del parse y devuelve el código de salida |
| `TInvocationConfiguration` | Dónde escribe `Invoke`, y si un manejador por defecto captura lo que la acción deja escapar |
| `TCommandLineExit` | Los códigos que devuelve `Invoke` cuando no es la acción quien decide |
| `CommandLine` | El punto de entrada: `Option<T>`, `Argument<T>`, `Command`, `Root` |

---

## Declarar una gramática

Los métodos fluidos viven **en las interfaces**, nunca en un `class helper`.

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
| Subcomandos, sin límite de profundidad | `Padre.Add(Hijo)`. Un comando con subcomandos y sin acción exige que se nombre una hoja |
| Argumentos posicionales | `CommandLine.Argument<T>(...)`, ligados por su sitio en orden de declaración |
| Alias de comando | `.WithAlias('b')` — `mitool b native` alcanza `mitool build native` |
| Opciones recursivas | `.Recursive` — declarada una vez, disponible en todos sus descendientes |
| Valores por defecto | `.WithDefault(Valor)`, devuelto cuando la invocación no traía la opción |
| Requerido | `.Required` en una opción; un argumento lo es por su aridad |
| Aridad | `.WithArity(TArity.ZeroOrMore)` y las otras cuatro |
| Valores acotados | una `T` enumerada, o `.AcceptOnly(['fast', 'safe'])` |
| Varios valores por mención | `.AllowMultipleArgumentsPerToken` — `--items a b c` sin repetir el nombre |
| Recoger lo ajeno | `Comando.AllowUnmatchedTokens`, leído en `UnmatchedTokens` |
| Versión | `.WithVersion('1.0.0')` en la raíz; también `.WithVersion(tag, nombre, compañía)` o `.WithVersion(TUnaClase)` para tomarla de una anotación `[Version]`. `ICommandLineParseResult.VersionInfo` la devuelve |

---

## Leer valores

```pascal
var Path  := OutOpt.ValueIn(Parsed);        // un valor, con su tipo estático
var Files := FilesOpt.ValuesIn(Parsed);     // TArray<string>, para una aridad que admite varios
```

`ICommandLineParseResult.WasProvided` distingue **ausente** de **presente pero vacía**: `--file ''` no es la
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
  por los dos — y, con `.AllowMultipleArgumentsPerToken`, también `--file a.pas b.pas`.
- **Requerido, en los argumentos.** Un mínimo de uno **es** requerido; no hace falta un segundo
  concepto.
- **Se acaba el último-gana silencioso.** Con máximo uno, repetir la opción es un **error**.

En las **opciones**, `Required` es una marca aparte, como en .NET: una opción de un valor puede ser
obligatoria o no, y la aridad no lo dice.

---

## Valores

Una conversión consume el token **entero** o falla. `--port 80xyz` se rechaza nombrando la opción y
el texto recibido, en vez de valer `80`; y `-5` es un valor, no una opción desconocida.

### Los tipos de serie, y qué traduce cada uno

La lista está **traducida** de la de .NET, no copiada. Lo que necesita una línea de comandos son
números de los anchos que la gente usa, fechas, identificadores y rutas.

| `System.CommandLine` | Aquí | Por qué |
|---|---|---|
| `int`, `long`, `short`, `byte` y sin signo | `Integer`, `Int64` | los dos que cubren el uso real |
| `float`, `double` | `Double` | un flotante binario, que es lo que son una proporción o un umbral |
| `decimal` | `Currency` | el decimal exacto: un importe escrito con hasta cuatro decimales vuelve con esos cuatro, y no redondea por su cuenta |
| `bool` | `Boolean` | es el flag: basta con nombrarlo |
| enums | tipos enumerados | el tipo nombra sus valores, y se comparan sin distinguir mayúsculas |
| `DateTime`, `DateTimeOffset`, `DateOnly`, `TimeOnly` | `TDateTime` | Delphi tiene **uno**, así que «solo fecha» y «solo hora» no son declarables. Se lee en ISO 8601 y en nada más — divergencia 3, más abajo |
| `Guid` | `TGUID` | directo, y las llaves son opcionales |
| `FileInfo`, `DirectoryInfo`, `FileSystemInfo` | `string` + `AcceptExistingOnly` | aquí una ruta es texto; lo que aportaban esos tipos era la validación, y la validación es lo que se porta |
| arrays y listas | `TArray<T>` | una colección declarada deriva `ZeroOrMore`, como en .NET |

Un enumerado nombra sus propios valores aceptados, así que salen en la ayuda y en el error sin que
nadie los escriba dos veces — y se comparan sin distinguir mayúsculas, como en .NET.
`.AcceptOnly([...])` es el mismo cierre para un tipo que no lo expresa.

Una opción `Boolean` es un flag: basta con nombrarla, y valen `--flag false`, `--flag=false` y
`--flag true`.

Una **colección** es un valor hecho con los tokens que le dieron, así que leerla devuelve el array
entero:

```pascal
FilesOpt := CommandLine.Option<TArray<string>>('--files', 'Ficheros a compilar');
// tool --files a.pas --files b.pas   ->  FilesOpt.ValueIn(Parsed) = ['a.pas', 'b.pas']
```

Su aridad sale del tipo, así que `ZeroOrMore` no se escribe a mano. Repetir el nombre es lo que la
llena; con `.AllowMultipleArgumentsPerToken` vale además `--files a.pas b.pas`.

`--` termina las opciones: todo lo que venga detrás es valor, con guiones y todo. Sin eso, un
posicional cuyo valor sea `--verbose` no se podría pasar.

---

## Un tipo que el motor no conoce, y una validación que no sabe expresar

Dos ganchos, los dos declarados **en el símbolo** y en ningún otro sitio.

### Una conversión propia

Se pasa donde se declara el símbolo, porque un símbolo o sabe convertir su tipo o su declaración se
rechaza ahí mismo:

```pascal
MinOpt := CommandLine.Option<TSemVer>('--min',
  function(const Raw: string; out Value: TSemVer; out Error: string): Boolean
  begin
    Error := '';
    Result := TSemVer.TryParse(Raw, Value);
    if not Result then
      Error := Format('"%s" no es una versión: escríbela como mayor.menor.parche', [Raw]);
  end,
  'Versión mínima aceptada');
```

El rechazo sale por `ParseResult.Errors` como cualquier otro, así que un tipo que añade un tercero
falla igual de bien que uno de serie. Sirve también para un tipo que el motor ya conoce, para leerlo de
otra forma — hexadecimal en un `Integer`, por ejemplo.

**En el símbolo y no en un registro de tipos.** El alcance es exactamente donde se escribió.

El límite **se movió pero no desapareció**: un tipo que el motor no cubre y para el que no se declara
conversión se sigue rechazando **al declararlo**, que es donde quien escribe la gramática puede hacer
algo al respecto.

### Validaciones

Una validación recibe el valor **ya convertido** y puede rechazarlo. Corre **dentro** del parseo:

```pascal
CountOpt := CommandLine.Option<Integer>('--count', 'Cuántos hacer')
  .Validate(
  function(const Value: Integer; out Error: string): Boolean
  begin
    Result := Value > 0;
    if not Result then
      Error := Format('--count tiene que ser mayor que cero, y %d no lo es', [Value]);
  end);
```

Se pueden declarar varias; el primer rechazo es el error de ese valor. Una validación debería ser una
pregunta pura sobre el valor.

Dos vienen de serie, escritas sobre ese mismo gancho:

| | Qué pregunta |
|---|---|
| `.AcceptExistingOnly` | la ruta existe, fichero o directorio. ⚠️ **Consulta el disco** — la excepción consciente a que una validación sea pura, así que parsear deja de ser gratis |
| `.AcceptLegalFileNamesOnly` | es un nombre de fichero legal; los separadores también se rechazan, porque un nombre no es una ruta |

Las dos aplican a una ruta, que aquí es un `string` — incluido un `TArray<string>`, donde corren por
elemento. Declarar cualquiera de las dos sobre algo que no es una ruta se rechaza al declararlo.

⚠️ **Una conversión o una validación propias no se pueden declarar sobre una colección.** Las dos
reciben el valor de **un** token, y en una colección ese valor es un elemento — que `T` no puede
nombrar, porque Delphi no tiene forma de decir «el elemento de `T`» en una interfaz. Se rechaza donde se declara. Declárala sobre el tipo del elemento, en un símbolo aparte.

---

## Un valor por defecto que es un cálculo

`WithDefault(Valor)` toma una constante y se evalúa al declarar la gramática. Hay una forma calculada,
que se evalúa **solo si la invocación no trajo el símbolo**:

```pascal
OutOpt := CommandLine.Option<string>('--out', 'Dónde escribir')
  .WithDefault(
  function: string
  begin
    Result := TPath.GetTempPath;
  end, 'un directorio temporal');
```

Un default que depende del entorno no se puede escribir como constante en una gramática que corre antes
de saberlo. Sin esto, la aplicación deja el default vacío y lo rellena después de leer — y entonces la
ayuda muestra un default que el programa no usa.

El cálculo corre como máximo una vez, y **la ayuda no lo ejecuta**: muestra el texto que se declare
junto a él, y ningún default cuando no hay ninguno.

---

## Un símbolo que no aparece en la ayuda

```pascal
DiagOpt := CommandLine.Option<Integer>('--diag', 'Nivel de diagnóstico').Hidden;
```

Es lo que necesita una opción de diagnóstico, una en transición o una que solo usa un script interno:
existir sin ponerse delante de todo el que pregunta cómo se usa la herramienta.

Ocultar es cosa de **la ayuda** y de nada más. Un símbolo oculto se declara, se parsea y se lee igual,
y cuando recibe un valor inválido el error **lo nombra**.

---

## Cómo se puede escribir una invocación

La misma invocación, escrita como ya la escribe quien la teclea. Ninguna de estas formas dice nada
que no digan las otras.

| Forma | Ejemplo | Equivale a |
|-------|---------|------------|
| Valor tras un espacio | `--out bin\app.exe` | — |
| Valor tras `=` | `--out=bin\app.exe` | `--out bin\app.exe` |
| Valor tras `:` | `--out:bin\app.exe` | `--out bin\app.exe` |
| Alias de un carácter pegado a su valor | `-obin\app.exe` | `-o bin\app.exe` |
| Opciones de un carácter agrupadas | `-fdx` | `-f -d -x` |
| Grupo seguido de un valor | `-fdo bin\app.exe` | `-f -d -o bin\app.exe` |
| Prefijo de Windows | `/out bin\app.exe` | `--out bin\app.exe` |
| Fichero de respuesta | `@build.rsp` | los tokens que traen sus líneas |

Todas están acotadas igual: **cada una actúa solo cuando el token no nombra ya algo declarado**.
`-fdx` se parte en tres porque `f`, `d` y `x` son alias declarados; si existiera una opción llamada
`-fdx`, eso es lo que el token seguiría siendo. Y un grupo con una letra que nadie declaró se rechaza
**entero**, nombrando la letra. La letra de la ayuda cabe dentro de un grupo —`-fh` pide ayuda—, pero
no lo forma: un token ninguna de cuyas letras declara el comando sigue siendo el token que era.

### ⚠️ Qué quita admitirlas

Una forma admitida es un token que significaba una cosa y pasa a significar otra. Tres, concretas:

- **`-abc` puede dejar de ser una opción llamada `-abc`.** Donde `a`, `b` y `c` sean alias declarados
  de un carácter, ahora son tres opciones.
- **Un valor posicional que empiece por `/` puede leerse como opción** — pero solo cuando lo que
  sigue a la barra nombra una declarada. `/tmp/app.dpr` sigue siendo un valor; `/out` no, si existe
  `--out`.
- **Un valor posicional que empiece por `@` se lee como fichero de respuesta**, y si nombra un
  fichero que no está es un error de parseo y no un token.

`--` devuelve las tres: todo lo que venga detrás es valor, arrobas, barras y guiones incluidos. No
hay interruptor para apagar una forma, ni hace falta — una gramática que tenga que recibir uno de
esos valores tal cual tiene `--`.

### Ficheros de respuesta

`@fichero.rsp` mete las líneas del fichero como tokens, donde estaba el `@`. Existe por dos razones
concretas: una línea de comandos que no cabe en el límite de la consola, y una invocación larga que
se repite.

```
# para qué es esta invocación -- una línea que empieza por # no aporta nada
build
native
app.dpr
--out
bin\app.exe
@common.rsp
```

Una línea, un token: no se parte por espacios, no se expande ninguna variable y no se resuelve
ninguna ruta contra nada. Un fichero puede nombrar a otro; el anidamiento está acotado, y uno que se
nombra a sí mismo falla diciéndolo en vez de colgarse.

### Varios valores tras un solo nombre

```pascal
var ItemsOpt := CommandLine.Option<string>('--items', 'Varios de una vez')
  .WithArity(TArity.ZeroOrMore).AllowMultipleArgumentsPerToken;
```

`--items a b c` pasa a valer por los tres. Es **opt-in**: en .NET se llama igual.

### Un comando que envuelve a otro

```pascal
var Sudo := CommandLine.Root('sudo', 'Ejecuta algo como otro');
Sudo.Add(UserOpt);
Sudo.AllowUnmatchedTokens;   // sentencia, no eslabón de una cadena: devuelve ICommand

var Parsed := Sudo.Parse(['-u', 'admin', 'apt', 'update']);
var Resto  := Parsed.UnmatchedTokens;   // ['apt', 'update']
```

Sin eso —y sigue siendo el defecto— un token que no casa con nada es un error de parseo.

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
  --verbose, -v         Say more about what is happening
  --help, -h            Show help and usage information
  --version             Show version information
```

Toda gramática responde a `--help` y `-h`, y toda **raíz** responde a `--version`, sin declarar nada.
Pedir cualquiera de las dos es un **resultado válido**, no un fallo: `HelpRequested` /
`VersionRequested` son ciertos y `Errors` está vacío. La ayuda se resuelve *antes* de validar la
gramática. Una aplicación que declare su propio `--help` o `--version` gana: es la que conoce su
caso. Y `--version` va en la raíz y no en cada subcomando, como en .NET: la versión es de la
herramienta. Lo que escriben se puede envolver con algo propio: ver *Un banner encima de la ayuda y
de la versión*.

---

## Cada comando, su acción

Un comando declara lo que hace cuando una invocación lo alcanza, y el resultado del parse lo ejecuta:

```pascal
var NameOpt := CommandLine.Option<string>('--name', 'Who to greet').WithDefault('world');
var Greet := CommandLine.Command('greet', 'Say hello').Add(NameOpt).SetAction(
  function(const Parsed: ICommandLineParseResult): Integer
  begin
    Writeln('Hello, ', NameOpt.ValueIn(Parsed));
    Result := TCommandLineExit.Ok;
  end);

var Root := CommandLine.Root('greeter', 'Greets people');
Root.Add(Greet);

var Parsed := Root.Parse;                  // lee ParamStr
ExitCode := Parsed.Invoke;
```

La acción recibe el resultado del parse y lee de él los valores como cualquier otro código. `Invoke`
resuelve la invocación y devuelve el código con el que termina el proceso:

| El resultado del parse | Qué hace `Invoke` | Qué devuelve |
|---|---|---|
| pidió `--version` | ejecuta la `VersionAction` de la raíz, que escribe la versión en la salida | el código de la acción: `TCommandLineExit.Ok` (0) la de serie |
| pidió `--help` | ejecuta la `HelpAction` de la raíz, que escribe la ayuda en la salida | el código de la acción: `Ok` (0) la de serie |
| trae errores | los escribe en el error | `InvocationError` (2) |
| alcanzó un comando con acción | la ejecuta | el código que devuelve la acción |
| alcanzó un comando sin ella | nada | `Ok` (0) |

En las tres primeras filas no corre la acción de ningún comando.

- Una acción sin código propio es un `procedure(const Parsed: ICommandLineParseResult)`, y la
  invocación devuelve `Ok` por ella.
- Declarar la acción otra vez la sustituye.
- Un comando con subcomandos **y** acción es una invocación por sí mismo: `mitool` a secas ejecuta la
  acción de `mitool`. Sin ella solo agrupa, y `mitool` a secas falla con
  `Command "mitool" requires a subcommand: …`.
- `SetAction` devuelve `ICommand`, así que en una raíz es una sentencia, como `Describe`.

### Dónde escribe, y qué hace con una excepción

`Invoke(Configuration)` recibe una `TInvocationConfiguration`; `Invoke` a secas usa sus valores por
defecto.

| Campo | Por defecto | Qué dice |
|---|---|---|
| `Output` | `nil`, la consola | dónde se escriben la ayuda y la versión — cualquier `TTextWriter` |
| `Error` | `nil`, el error de la consola | dónde se escriben los errores de parse y las excepciones que se escapan |
| `EnableDefaultExceptionHandler` | `True` | encendido, una excepción que sale de la acción se escribe en `Error` como `EClase: mensaje` e `Invoke` devuelve `TCommandLineExit.Failure` (1); apagado, sale de `Invoke` y la ve quien llamó |

Una `TInvocationConfiguration` declarada y sin asignar ya lleva esos valores:

```pascal
var Configuration: TInvocationConfiguration;
Configuration.Output := Writer;            // cualquier TTextWriter
ExitCode := Parsed.Invoke(Configuration);
```

### Un banner encima de la ayuda y de la versión

La raíz resuelve la ayuda y la versión con dos acciones propias, `HelpAction` y `VersionAction`, del
mismo tipo que la acción de un comando. Las de serie escriben en la salida de la invocación la ayuda
del comando alcanzado, o la versión, y devuelven `Ok`. Para poner algo tuyo encima, sustituye la
acción por una que lo escriba y después llame a la de serie:

```pascal
function WithBanner(const Default: TCommandAction): TCommandAction;
begin
  Result :=
    function(const Parsed: ICommandLineParseResult): Integer
    begin
      Parsed.InvocationConfiguration.WriteOutput('buildtool -- (c) ACME');
      Result := Default(Parsed);
    end;
end;

Root.HelpAction := WithBanner(Root.HelpAction);
Root.VersionAction := WithBanner(Root.VersionAction);
```

- La ayuda sigue siendo la que se deriva de la gramática. Una acción de ayuda sustituida en la raíz
  envuelve también la ayuda de cada subcomando.
- `Parsed.InvocationConfiguration` es la configuración de la invocación en curso, y `WriteOutput`
  escribe una línea en su `Output`, o en la consola si es `nil`: el banner va donde va la ayuda.
- Lo que devuelve la acción es lo que devuelve `Invoke`, y el manejador de excepciones por defecto la
  cubre como cubre la acción de un comando.
- En `System.CommandLine` estas acciones son de `HelpOption` y de `VersionOption`; aquí son de la
  raíz.

⚠️ Captura la acción de serie, nunca la raíz. Una acción que sostiene la raíz en la que está
instalada forma con ella un ciclo de referencias, y ninguna de las dos se libera nunca.

---

## Divergencias con `System.CommandLine`

**1 · El valor se lee al revés.** En .NET se escribe `result.GetValue(fileOption)`. Aquí,
`fileOption.ValueIn(parsed)`: el objeto le pregunta al resultado:

- Una interfaz **no puede declarar un método genérico**, punto — `E2535 Interface methods must not
  have parameterized methods`. Así que `ICommandLineParseResult.GetValue<T>` no puede existir, fuera
  cierto lo demás o no.
- Y ni siquiera en una clase infiere Delphi la `T` a través de un parámetro `IOption<T>` — `E2010
  Incompatible types: 'IOption<T>' and 'IOption<string>'`. La inferencia funciona cuando el parámetro
  **es** `T`, no cuando `T` va anidada dentro de otro genérico.

`ValueIn` no necesita inferencia porque `T` ya viene fijada por el receptor.

**2 · Los nombres se comparan sin distinguir mayúsculas.** .NET es **sensible** por convención POSIX;
aquí `--OUT` y `--out` son la misma opción. El coste aceptado: dos alias que solo difieran en
mayúsculas son el mismo alias, y una CLI donde el caso **es** el significado —`git clean -x` frente a
`-X`— no se puede expresar. Para eso, dos opciones.

**3 · Una fecha se lee en ISO 8601 y de ninguna otra forma.** El `DateTime` de .NET admite además la
convención local. Aquí `--since 2026-08-25` vale y `--since 25/08/2026` se rechaza, nombrando la
opción y el texto. El coste aceptado: quien teclee a mano tiene que escribir la fecha como la lee la
máquina. Y como Delphi tiene un solo tipo de fecha, «solo fecha» y «solo hora» no son declarables;
quien lo necesite lo acota con una validación.

**4 · Un error de parse devuelve 2, no 1.** .NET devuelve 1 tanto a un error de parse como a una
excepción que nadie capturó. Aquí `Invoke` devuelve `TCommandLineExit.InvocationError` (2) a una
invocación que no encaja con la gramática, y el 1 queda para una acción que falló.

**5 · La invocación es síncrona.** No hay acción asíncrona, ni cancelación, ni manejo de la
terminación del proceso —ni Ctrl+C, ni tiempo de gracia antes de terminar—.
`TInvocationConfiguration` lleva la salida, el error y el manejador de excepciones por defecto, y
nada más.

---

## Lo que todavía no está

Nada de lo de abajo cambia la forma del API, así que todo es aditivo:

- **Los anchos que nadie ha pedido todavía** — `short`, `byte` y los sin signo. Un tipo entra cuando
  alguien lo pide; hasta entonces una conversión propia lo cubre en una línea.

Las directivas `[diagram]` y `[suggest]`, las pre-acciones y el autocompletado no se portan.

---

## No confundir con configuración

Leer la línea de comandos como **fuente de configuración** —`--Logging:LogLevel=Debug` sobrescribiendo
un valor de `appsettings.json`— es otro trabajo y pertenece a
[`Daf.Configuration.CommandLine`](../Configuration/README.es.md), igual que
`Microsoft.Extensions.Configuration.CommandLine` no usa `System.CommandLine`.

`Daf.CmdLn` era el módulo anterior al que este sustituye. Se retiró en 2.0.0, y no hay
capa de adaptación ni alias de unidad: son APIs distintas. `MIGRATION-2.0.es.md` lleva las dos
declaraciones enfrentadas.

---

## Sample

[`samples/BuildTool`](samples/BuildTool) declara una gramática de dos niveles, da a cada hoja su acción
e invoca lo que parsea. Ejecútalo sin argumentos y recorre un conjunto de invocaciones, incluidas las
seis que fallan, con el código de salida de cada una.
