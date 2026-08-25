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
| Varios valores por mención | `.AllowMultipleArgumentsPerToken` — `--items a b c` sin repetir el nombre |
| Recoger lo ajeno | `Comando.AllowUnmatchedTokens`, leído en `UnmatchedTokens` |
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
  por los dos — y, con `.AllowMultipleArgumentsPerToken`, también `--file a.pas b.pas`.
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

### Los tipos de serie, y qué traduce cada uno

La lista está **traducida** de la de .NET, no copiada: copiarla sería portar nombres en vez de
capacidades. Lo que necesita una línea de comandos son números de los anchos que la gente usa,
fechas, identificadores y rutas.

| `System.CommandLine` | Aquí | Por qué |
|---|---|---|
| `int`, `long`, `short`, `byte` y sin signo | `Integer`, `Int64` | los dos que cubren el uso real; otro ancho entra cuando alguien lo pida dos veces |
| `float`, `double`, `decimal` | `Double` | `Currency` sería el decimal exacto, y no hay ningún caso que lo pida |
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

**En el símbolo y no en un registro de tipos**, a propósito: un registro lo convierte en configuración
de proceso, así que dos librerías que declaren la conversión del mismo tipo pelean y gana la que
arrancó última, en silencio. En el símbolo, el alcance es exactamente donde se escribió.

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

Dentro, y no después, por un motivo: validar fuera desplaza el fallo a donde ya no es un error de
invocación, y la herramienta acaba reportando unos fallos de una forma y otros de otra — quien la
invocó ve dos listas en vez de una.

Se pueden declarar varias; el primer rechazo es el error de ese valor. Una validación debería ser una
pregunta pura sobre el valor.

Dos vienen de serie, escritas sobre ese mismo gancho y no sobre un mecanismo aparte:

| | Qué pregunta |
|---|---|
| `.AcceptExistingOnly` | la ruta existe, fichero o directorio. ⚠️ **Consulta el disco** — la excepción consciente a que una validación sea pura, así que parsear deja de ser gratis |
| `.AcceptLegalFileNamesOnly` | es un nombre de fichero legal; los separadores también se rechazan, porque un nombre no es una ruta |

Las dos aplican a una ruta, que aquí es un `string` — incluido un `TArray<string>`, donde corren por
elemento. Declarar cualquiera de las dos sobre algo que no es una ruta se rechaza al declararlo.

⚠️ **Una conversión o una validación propias no se pueden declarar sobre una colección.** Las dos
reciben el valor de **un** token, y en una colección ese valor es un elemento — que `T` no puede
nombrar, porque Delphi no tiene forma de decir «el elemento de `T`» en una interfaz. Se rechaza donde
se declara en vez de no correr nunca. Declárala sobre el tipo del elemento, en un símbolo aparte.

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
junto a él, y ningún default cuando no hay ninguno. Un cálculo puede abrir un fichero o leer el
entorno, y la ayuda no tiene forma de saber si este lo hace; no mostrar nada es mejor que inventar un
valor o que pagar un efecto para pintar una línea.

---

## Un símbolo que no aparece en la ayuda

```pascal
DiagOpt := CommandLine.Option<Integer>('--diag', 'Nivel de diagnóstico').Hidden;
```

Es lo que necesita una opción de diagnóstico, una en transición o una que solo usa un script interno:
existir sin ponerse delante de todo el que pregunta cómo se usa la herramienta. Las alternativas eran
no declararla —y entonces no funciona— o declararla y aceptar que salga.

Ocultar es cosa de **la ayuda** y de nada más. Un símbolo oculto se declara, se parsea y se lee igual,
y cuando recibe un valor inválido el error **lo nombra**: esconderlo de la ayuda no es esconderlo de
quien ya lo usa, y un error que no dice qué falló convierte una errata en un acertijo.

---

## Cómo se puede escribir una invocación

La misma invocación, escrita como ya la escribe quien la teclea. Ninguna de estas formas dice nada
que no digan las otras: existen para que una herramienta hecha con esto no necesite una sección del
README explicando por qué su sintaxis es la excepción.

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
**entero**, nombrando la letra — aceptar las que sí y callar la que no te dejaría creyendo que
pediste algo que no pediste.

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
ninguna ruta contra nada — cuanto menos haga, menos hay que explicar sobre de dónde salió un token.
Un fichero puede nombrar a otro; el anidamiento está acotado, y uno que se nombra a sí mismo falla
diciéndolo en vez de colgarse.

### Varios valores tras un solo nombre

```pascal
var ItemsOpt := CommandLine.Option<string>('--items', 'Varios de una vez')
  .WithArity(TArity.ZeroOrMore).AllowMultipleArgumentsPerToken;
```

`--items a b c` pasa a valer por los tres. Es **opt-in, y no podía ser otra cosa**: si toda opción
con sitio para varios valores fuera glotona por defecto, un posicional escrito detrás de una dejaría
de ser posicional. El default correcto depende de la gramática y solo lo sabe quien la escribió — en
.NET se llama igual, por lo mismo.

### Un comando que envuelve a otro

```pascal
var Sudo := CommandLine.Root('sudo', 'Ejecuta algo como otro');
Sudo.Add(UserOpt);
Sudo.AllowUnmatchedTokens;   // sentencia, no eslabón de una cadena: devuelve ICommand

var Parsed := Sudo.Parse(['-u', 'admin', 'apt', 'update']);
var Resto  := Parsed.UnmatchedTokens;   // ['apt', 'update']
```

Sin eso —y sigue siendo el defecto— un token que no casa con nada es un error de parseo. Para todo lo
que no envuelve a nadie, un token sin reconocer es una errata, y tragárselo en silencio es el peor de
los dos fallos.

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

**3 · Una fecha se lee en ISO 8601 y de ninguna otra forma.** El `DateTime` de .NET admite además la
convención local. Aquí `--since 2026-08-25` vale y `--since 25/08/2026` se rechaza, nombrando la
opción y el texto. La causa: `01/02/2026` denota dos días distintos según quién lo lea, y una línea de
comandos no es prosa dependiente del locale — es lo que escribió un script. El coste aceptado: quien
teclee a mano tiene que escribir la fecha como la lee la máquina. Y como Delphi tiene un solo tipo de
fecha, «solo fecha» y «solo hora» no son declarables; quien lo necesite lo acota con una validación.

---

## Lo que todavía no está

Nada de lo de abajo cambia la forma del API, así que todo es aditivo:

- **Los anchos que nadie ha pedido todavía** — `short`, `byte`, sin signo, y `Currency` como el
  decimal exacto. Un tipo entra cuando alguien lo pide dos veces; hasta entonces una conversión
  propia lo cubre en una línea.

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
