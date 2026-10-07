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

var FileOpt := CommandLine.Option<string>('--file').Description('Fichero a compilar');
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
| `TArgumentArity` | Cuántos valores admite un símbolo, mínimo y máximo |
| `IParseResult` | Lo que produjo una invocación: el árbol de resultados, la hoja alcanzada, los errores, la ayuda, la versión. `Invoke` la ejecuta |
| `ICommandResult`, `IOptionResult`, `IArgumentResult` | El resultado de un símbolo que la invocación alcanzó: sus tokens, sus errores, su padre |
| `TParseError` | Un error del parse, y el resultado del símbolo al que pertenece |
| `TToken` | Un token de la invocación, tal como lo leyó el parse |
| `ICommandLineAction` | Lo que ejecuta una invocación: la acción de un comando o de una opción. `TSynchronousCommandLineAction` es la base de una escrita como clase |
| `TCommandAction` | La acción de un comando escrita como función: recibe el resultado del parse y devuelve el código de salida |
| `IHelpOption`, `IVersionOption` | La ayuda y la versión: dos opciones que tiene toda raíz, cada una con una acción que las escribe |
| `IParseErrorAction` | Lo que ejecuta `Invoke` ante un error de parse: lo que se quiso escribir, los errores y la ayuda |
| `TInvocationConfiguration` | Dónde escribe `Invoke`, y si un manejador por defecto captura lo que la acción deja escapar |
| `TCommandLineExit` | Los códigos que devuelve `Invoke` cuando no es la acción quien decide |
| `CommandLine` | El punto de entrada: `Option<T>`, `Argument<T>`, `Command`, `Root`, `HelpOption`, `VersionOption` |

---

## Declarar una gramática

Los métodos fluidos viven **en las interfaces**, nunca en un `class helper`.

```pascal
var VerboseOpt := CommandLine.Option<Boolean>('--verbose').Description('Cuenta más').Recursive;
var OutOpt     := CommandLine.Option<string>('--out').Description('Dónde va').DefaultValue('bin\out.exe');
var LevelOpt   := CommandLine.Option<TVerbosity>('--level').Description('Cuánto contar');
var FilesOpt   := CommandLine.Option<string>('--file').Description('Ficheros').Arity(TArgumentArity.ZeroOrMore);
var KeyOpt     := CommandLine.Option<string>('--key').Description('Clave de API').Required;
var TargetArg  := CommandLine.Argument<string>('target').Description('El proyecto a construir');

var Native := CommandLine.Command('native', 'Construye un binario nativo').Add([TargetArg, OutOpt]);
var Build  := CommandLine.Command('build', 'Construye cosas').Add(Native);
Build.Aliases.Add('b');

var Root := CommandLine.Root('buildtool', 'Una herramienta pequeña').WithVersion('1.0.0');
Root.Add([VerboseOpt, Build]);
```

`Add` acepta un símbolo o un array abierto de ellos — lo más cercano que Delphi tiene al
*collection initializer* de C#.

| Capacidad | Cómo |
|-----------|------|
| Subcomandos, sin límite de profundidad | `Padre.Add(Hijo)`. Un comando con subcomandos y sin acción exige que se nombre una hoja |
| Argumentos posicionales | `CommandLine.Argument<T>(...)`, ligados por su sitio en orden de declaración |
| Alias | `CommandLine.Option<T>('--verbose', ['-v'])` en una opción; `Command.Aliases.Add('b')` en un comando — `mitool b native` alcanza `mitool build native` |
| Opciones recursivas | `.Recursive` — declarada una vez, disponible en todos sus descendientes |
| Valores por defecto | `.DefaultValue(Valor)`, devuelto cuando la invocación no traía la opción |
| Requerido | `.Required` en una opción; un argumento lo es por su aridad |
| Aridad | `.Arity(TArgumentArity.ZeroOrMore)` y las otras cuatro |
| Valores acotados | una `T` enumerada, o `.AcceptOnlyFromAmong(['fast', 'safe'])` |
| Varios valores por mención | `.AllowMultipleArgumentsPerToken` — `--items a b c` sin repetir el nombre |
| Recoger lo ajeno | `Comando.AllowUnmatchedTokens`; `UnmatchedTokens` devuelve lo que nadie reclamó, sea error o no |
| Versión | `.WithVersion('1.0.0')` en la raíz; también `.WithVersion(tag, nombre, compañía)` o `.WithVersion(TUnaClase)` para tomarla de una anotación `[Version]`. `IParseResult.VersionInfo` la devuelve |

---

## Leer valores

```pascal
var Path  := OutOpt.ValueIn(Parsed);        // un valor, con su tipo estático
var Files := FilesOpt.ValuesIn(Parsed);     // TArray<string>, para una aridad que admite varios
```

Lo que devuelve `ValueIn` depende de lo que el parse hizo con el símbolo:

| El símbolo | `ValueIn` devuelve |
|---|---|
| lo trajo la invocación | su valor |
| no lo trajo, tiene default declarado y está en un comando por el que pasó la invocación | el default declarado |
| no lo trajo, en cualquier otro caso | el default de su **tipo**: `0`, `''`, un array vacío |
| el parse lo rechazó: un valor que no convierte, una repetición, una opción requerida que falta | nada: lanza `ECommandLine` con ese error |

El parse valida entero el comando alcanzado, y los que tiene por encima solo en sus opciones
recursivas y en las que tienen default. Una opción de un padre que no es ninguna de las dos, escrita
antes del subcomando, se toma pero no se valida: `tool --port x run` parsea, y leer `--port` lanza
`"x" is not a valid integer for --port`.

`ValueIn` acepta también el resultado de un símbolo —`PortOpt.ValueIn(Parsed.CommandResult)`— y
devuelve lo mismo: el árbol es uno, pregunte el nodo que pregunte. `RequiredValueIn` lee igual y lanza
cuando el símbolo no tiene resultado. En un argumento sin resultado devuelve lo que el
`GetRequiredValue` de .NET devuelve para su tipo: lanza con un `string`, una clase o una interfaz, y
cualquier otro tipo da su default.

`IParseResult.WasProvided` dice si la invocación **nombró** el símbolo. `--file ''` no es la misma
respuesta que no pasar `--file`, y `--port 80xyz` cuenta como pasada aunque leerla lance.

---

## El resultado del parse es un árbol

Cada comando que alcanzó la invocación tiene un resultado, del raíz al más interno, y también cada
opción y argumento que trajo o que tomó su default. `Parsed.RootCommandResult` es donde empieza el
árbol y `Parsed.CommandResult` es el comando más interno. `GetResult` encuentra el resultado de un
símbolo declarado, sobre el resultado del parse o sobre cualquiera de sus nodos, y devuelve `nil`
cuando no lo hay.

```pascal
var Port := Parsed.GetResult(PortOpt);         // un IOptionResult, o nil
if (Port <> nil) and not Port.IsImplicit then
  Writeln('nombrada ', Port.IdentifierTokenCount, ' vez/veces, con ', Length(Port.Tokens), ' valor(es)');
```

| Un resultado | devuelve |
|---|---|
| `Parent` | el resultado del comando del que cuelga; `nil` en el raíz |
| `Tokens` | los tokens de la invocación que lo produjeron |
| `Errors` | los errores del parse que son suyos |
| `Children`, en un comando | los resultados que cuelgan de él |
| `IsImplicit`, en una opción | la invocación no la nombró: el parse la creó por su default, o por ser una requerida que falta, y entonces lleva ese error |
| `IsImplicit`, en un argumento | tomó su default |
| `IdentifierToken`, `IdentifierTokenCount`, en una opción | el primer token que la nombró, y cuántas veces se nombró |

Cada error dice de quién es: `TParseError.SymbolResult` es el resultado de su símbolo, y `nil` en un
error de la lectura de la invocación, como un fichero de respuesta que no existe. Un token que no
reclama nadie es un error del comando alcanzado, salvo que la raíz lo recoja.

Los errores salen en este orden:

1. un subcomando que falta;
2. los errores de leer la invocación;
3. los tokens que nadie reclamó, en el orden en que se escribieron;
4. la validación, del comando más interno al raíz: las validaciones propias del más interno, después
   las opciones de cada comando en el orden en que se declararon, y después sus argumentos.

La aridad de una opción se comprueba antes de convertir su valor, y si falla es lo único que esa
opción informa: repetida con dos valores, falla por la repetición sean cuales sean los valores. Una
opción o un argumento requerido cuyo valor no convierte tiene resultado, así que informa de la
conversión y no de que falta.

---

## La aridad, y por qué no es un detalle

La aridad es un mínimo y un máximo, y **se deriva del tipo** para que el caso normal no haya que
escribirlo: un `Boolean` admite cero o uno, cualquier otro tipo exige exactamente uno.

| | Significa |
|---|---|
| `TArgumentArity.Zero` | ningún valor |
| `TArgumentArity.ZeroOrOne` | cero o uno — el de un `Boolean` |
| `TArgumentArity.ExactlyOne` | uno, y tiene que estar — el de todo lo demás |
| `TArgumentArity.ZeroOrMore` | cualquier cantidad |
| `TArgumentArity.OneOrMore` | al menos uno |

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

Cada tipo de serie de .NET tiene su tipo de Delphi, salvo `DateTimeOffset` — divergencia 9, más abajo.

| `System.CommandLine` | Aquí | Cómo se lee |
|---|---|---|
| `int`, `long` | `Integer`, `Int64` | |
| `sbyte`, `byte`, `short`, `ushort`, `uint`, `ulong` | `ShortInt`, `Byte`, `SmallInt`, `Word`, `Cardinal`, `UInt64` | un valor fuera del rango del tipo se rechaza: `-1` no es un `Cardinal`, y `300` no es un `Byte` |
| `float`, `double` | `Single`, `Double` | un `Single` lee como infinito una magnitud que no le cabe, como .NET |
| `decimal` | `Currency` | el decimal exacto: un importe escrito con hasta cuatro decimales vuelve con esos cuatro, y no redondea por su cuenta |
| `bool` | `Boolean` | es el flag: basta con nombrarlo |
| enums | tipos enumerados | el tipo nombra sus valores, y se comparan sin distinguir mayúsculas |
| enums `[Flags]` | `set of` un enumerado | sus nombres separados por comas: `--letters A,C` |
| `DateTime` | `TDateTime` | ISO 8601 y nada más — divergencia 3, más abajo. Escrita sin zona, es la hora escrita |
| `DateOnly`, `TimeOnly` | `TDate`, `TTime` | ISO 8601: `2026-10-05`; `10:30`, `10:30:15` o `10:30:15.5` |
| `TimeSpan` | `TTimeSpan` | el formato de .NET, `d.hh:mm:ss.f`: `1.02:03:04` es un día, dos horas, tres minutos y cuatro segundos |
| `Guid` | `TGUID` | las llaves son opcionales |
| `FileInfo`, `DirectoryInfo`, `FileSystemInfo` | `TFilePath`, `TDirectoryPath`, `string` | los dos primeros son un `type string` declarado en `Daf.CommandLine`, y `AcceptExistingOnly` lee qué clase de ruta son |
| `T?` de cualquiera de ellos | `TNullable<T>`, de `Daf.Nullable` | vacío cuando la invocación no lo trae, no el valor por defecto de `T`. `TNullable<Boolean>` es un flag, como `bool?` |
| arrays y listas | `TArray<T>` | una colección declarada deriva `ZeroOrMore`, como en .NET |

Un enumerado nombra sus propios valores aceptados, así que salen en la ayuda y en el error sin que
nadie los escriba dos veces — y se comparan sin distinguir mayúsculas, como en .NET.
`.AcceptOnlyFromAmong([...])` es el mismo cierre para un tipo que no lo expresa. Sus valores se
comparan tal como se escriben, mayúsculas incluidas, como en .NET;
`.AcceptOnlyFromAmong(TIStringComparer.Ordinal, [...])` no distingue mayúsculas. Comprueba cada token
tal como llegó, antes de ninguna conversión, una propia incluida.

Una opción `Boolean` es un flag: basta con nombrarla, y valen `--flag false`, `--flag=false` y
`--flag true`. Solo `true` y `false`, en cualquier caja, son booleanos: `--flag 0` nombra el flag, y
el `0` va al siguiente posicional.

Una **colección** es un valor hecho con los tokens que le dieron, así que leerla devuelve el array
entero:

```pascal
FilesOpt := CommandLine.Option<TArray<string>>('--files').Description('Ficheros a compilar');
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

Tiene dos formas. La primera convierte **un token**, y se pasa donde se declara el símbolo:

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

La segunda convierte **el resultado entero**. `CustomParser` recibe el resultado del símbolo —todos sus
tokens, `AddError` para rechazar, el resto de la invocación por `GetResult`— y devuelve el valor
entero. En una colección devuelve el array entero:

```pascal
var SortedOpt := CommandLine.Option<TArray<string>>('--sorted').AllowMultipleArgumentsPerToken
  .CustomParser(
  function(const Found: IArgumentResult): TArray<string>
  var
    I: Integer;
  begin
    SetLength(Result, Length(Found.Tokens));
    for I := 0 to High(Result) do
      Result[I] := Found.Tokens[I].Value;
    TArray.Sort<string>(Result);
  end);
```

En un argumento puede quedarse con parte de los tokens: `Found.OnlyTake(1)` se queda con el primero y
pasa el resto a los argumentos que siguen. Una excepción suya sale del parseo tal cual.

**En el símbolo y no en un registro de tipos.** El alcance es exactamente donde se escribió.

El límite **se movió pero no desapareció**: un tipo que el motor no cubre y para el que no se declara
conversión se rechaza **al añadir el símbolo a un comando**, antes de leer ninguna invocación. Un
`CustomParser` se declara antes de ese `Add`.

### Validaciones

`Validate` recibe el valor **ya convertido** y puede rechazarlo. Corre **dentro** del parseo:

```pascal
CountOpt := CommandLine.Option<Integer>('--count').Description('Cuántos hacer')
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

Tres vienen de serie, escritas sobre ese mismo gancho:

| | Qué pregunta |
|---|---|
| `.AcceptExistingOnly` | la ruta existe. Un `TDirectoryPath` tiene que ser un directorio; un `TFilePath` y un `string` pueden ser un fichero o un directorio, como el `FileInfo` de .NET. ⚠️ **Consulta el disco** — la excepción consciente a que una validación sea pura, así que parsear deja de ser gratis |
| `.AcceptLegalFileNamesOnly` | es un nombre de fichero legal en la plataforma donde corre; los separadores también se rechazan, porque un nombre no es una ruta. El error nombra el carácter |
| `.AcceptLegalFilePathsOnly` | es una ruta legal en la plataforma donde corre. El error nombra el carácter |

Aplican a una ruta —un `string`, un `TFilePath` o un `TDirectoryPath`, y un `TArray` de cualquiera de
ellos, donde corren por elemento—. Declarar una sobre algo que no es una ruta se rechaza al
declararla. Un valor vacío no es ni un nombre ni una ruta legales — divergencia 10, más abajo.

Una validación del **resultado** recibe el resultado de su símbolo y rechaza con `AddError`.
`Validators.Add` la declara en un comando, una opción o un argumento:

```pascal
Root.Validators.Add(
  procedure(const Result: ICommandResult)
  begin
    if (Result.GetResult(JsonOpt) <> nil) and (Result.GetResult(XmlOpt) <> nil) then
      Result.AddError('--json y --xml no se pueden usar juntas');
  end);
```

- Las validaciones de un comando corren solo cuando es el comando más interno que alcanzó la invocación,
  y antes que sus opciones y argumentos: un error suyo hace que estos no se validen.
- Las de una opción o un argumento corren después de comprobar su aridad y antes de convertir su
  valor: un error suyo hace que no se convierta.
- Un error que añade una validación de una opción queda entre los errores, y leer la opción sigue
  dando su valor. `Validate` y las dos de serie hacen fallar el valor mismo, y leerlo lanza.

⚠️ **Una conversión de un token, o un `Validate`, no se pueden declarar sobre una colección.** Los dos
reciben el valor de **un** token, y en una colección ese valor es un elemento — que `T` no puede
nombrar, porque Delphi no tiene forma de decir «el elemento de `T`» en una interfaz. Se rechaza donde
se declara. `CustomParser`, que recibe el resultado entero, se declara en una colección como en
cualquier otro símbolo.

---

## Un valor por defecto que es un cálculo

`DefaultValue(Valor)` toma una constante y se evalúa al declarar la gramática. `DefaultValueFactory`
toma un cálculo, que el parse ejecuta **cuando la invocación alcanza el comando que declara el símbolo
y no lo trae**:

```pascal
OutOpt := CommandLine.Option<string>('--out').Description('Dónde escribir')
  .DefaultValueFactory(
  function: string
  begin
    Result := TPath.GetTempPath;
  end, 'un directorio temporal');
```

Un default que depende del entorno no se puede escribir como constante en una gramática que corre antes
de saberlo. Sin esto, la aplicación deja el default vacío y lo rellena después de leer — y entonces la
ayuda muestra un default que el programa no usa.

Cada parse lo ejecuta una vez, se lea después el valor o no, y **la ayuda no lo ejecuta**: muestra el
texto que se declare junto a él, y ningún default cuando no hay ninguno.

El cálculo también puede recibir el resultado del símbolo, y leer por él el resto de la invocación:

```pascal
FileOpt := CommandLine.Option<string>('--file').Description('Dónde escribir')
  .DefaultValueFactory(
  function(const Found: IArgumentResult): string
  begin
    Result := WhoOpt.ValueIn(Found) + '.txt';
  end, '<who>.txt');
```

---

## Un símbolo que no aparece en la ayuda

```pascal
DiagOpt := CommandLine.Option<Integer>('--diag').Description('Nivel de diagnóstico').Hidden;
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
var ItemsOpt := CommandLine.Option<string>('--items').Description('Varios de una vez')
  .Arity(TArgumentArity.ZeroOrMore).AllowMultipleArgumentsPerToken;
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

Sin eso —y sigue siendo el defecto— un token que no casa con nada es un error de parseo, y
`UnmatchedTokens` lo trae igual. Una raíz que recoge manda sobre un subcomando que no.

### Un argumento que se queda con la cola

Un argumento declarado con `CaptureRemainingTokens` se queda con todos los tokens que siguen a su sitio,
opciones incluidas, así que un comando pasa su cola a otro programa sin pedir `--`:

```pascal
var Exec := CommandLine.Root('exec', 'Ejecuta un comando');
Exec.Add([VerboseOpt, CommandLine.Argument<TArray<string>>('command').CaptureRemainingTokens]);
// exec app --verbose -x   ->   command = ['app', '--verbose', '-x'], y --verbose no se da por pasada
```

Una opción antes del primer valor sigue siendo una opción: `exec --verbose app` da `--verbose`.

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

Toda **raíz** tiene entre sus opciones una de ayuda y otra de versión, sin declarar nada: la ayuda
responde a `--help`, `-h`, `/h`, `-?` y `/?` en cualquier comando, y la versión a `--version`. Pedir
cualquiera de las dos es un **resultado válido**, no un fallo: `IsHelpRequested` /
`IsVersionRequested` son ciertos. La invocación se lee entera igualmente, y pedir la ayuda borra los
errores del resto: `mitool --port abc --help` escribe la ayuda y devuelve 0. `--version` con otra
opción, otro argumento o un subcomando es un error suyo,
`--version option cannot be combined with other arguments.`, que queda en `Errors` mientras la versión
se escribe igual. Una aplicación que declare su propio `--help` o `--version` gana: es la que conoce
su caso. Y `--version` va en la raíz y no en cada subcomando, como en .NET: la versión es de la
herramienta. Lo que escriben se puede envolver con algo propio: ver *Un banner encima de la ayuda y
de la versión*.

---

## Cada comando, su acción

Un comando declara lo que hace cuando una invocación lo alcanza, y el resultado del parse lo ejecuta:

```pascal
var NameOpt := CommandLine.Option<string>('--name').Description('Who to greet').DefaultValue('world');
var Greet := CommandLine.Command('greet', 'Say hello').Add(NameOpt).SetAction(
  function(const Parsed: IParseResult): Integer
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
| nombró una opción con acción terminal —`--help` y `--version` entre ellas— | la ejecuta: las de serie escriben en la salida la ayuda del comando alcanzado, o la versión | el código de la acción: `TCommandLineExit.Ok` (0) la de serie |
| trae errores | escribe en la salida lo que probablemente se quiso escribir y la ayuda del comando alcanzado, y en el error los errores —ver *Lo que escribe un error de parse*— | `InvocationError` (2) |
| alcanzó un comando con acción | la ejecuta | el código que devuelve la acción |
| alcanzó un comando sin ella | nada | `Ok` (0) |

En las dos primeras filas no corre la acción de ningún comando.

- Una acción sin código propio es un `procedure(const Parsed: IParseResult)`, y la
  invocación devuelve `Ok` por ella.
- Declarar la acción otra vez la sustituye.
- Un comando con subcomandos **y** acción es una invocación por sí mismo: `mitool` a secas ejecuta la
  acción de `mitool`. Sin ella solo agrupa, y `mitool` a secas falla con
  `Command "mitool" requires a subcommand: …`.
- `SetAction` devuelve `ICommand`, así que en una raíz es una sentencia, como `Description(Texto)`.
- `SetAction` también recibe una `ICommandLineAction`: una acción escrita como clase, sobre
  `TSynchronousCommandLineAction`. `Command.Action` la devuelve.

### Una opción con acción

Una opción también lleva acción: `Opcion.SetAction(Accion)`, con una `ICommandLineAction`. Una cuyo
`IsTerminating` es cierto se ejecuta en lugar de la acción del comando alcanzado, como la ayuda y la
versión. Las que tienen `IsTerminating` falso se ejecutan antes que ella, en el orden en que se
nombraron, y también la de una opción que tomó su valor por defecto. Cuando una de ellas devuelve un
código distinto de 0, ese código es el que devuelve `Invoke`.

```pascal
type
  TTraceAction = class(TSynchronousCommandLineAction)
  public
    function IsTerminating: Boolean; override;
    function Invoke(const Parsed: IParseResult): Integer; override;
  end;

function TTraceAction.IsTerminating: Boolean;
begin
  Result := False;
end;

function TTraceAction.Invoke(const Parsed: IParseResult): Integer;
begin
  Parsed.InvocationConfiguration.WriteOutput('tracing on');
  Result := TCommandLineExit.Ok;
end;

var TraceOpt := CommandLine.Option<Boolean>('--trace').SetAction(TTraceAction.Create);
```

`IParseResult.Action` es la acción que `Invoke` ejecuta la última: la terminal de una opción nombrada,
la de un error de parse, o si no la acción del comando alcanzado.

### Lo que escribe un error de parse

```
$ mitool --verbse
'--verbse' was not matched. Did you mean one of the following?
--verbose

Unknown option "--verbse" for mitool

<la ayuda de mitool>
```

Las sugerencias y la ayuda van a la salida, y los errores al error, seguidos de una línea en blanco.
Un nombre se sugiere cuando está a 3 ediciones o menos de un token que nadie reclamó, y solo los más
cercanos; una opción o un comando ocultos, nunca. `Parsed.Action` es entonces una
`IParseErrorAction`, cuyos `ShowTypoCorrections` y `ShowHelp` apagan esas dos partes:

```pascal
var OnError: IParseErrorAction;
if Supports(Parsed.Action, IParseErrorAction, OnError) then
  OnError.ShowHelp := False;
```

### Dónde escribe, y qué hace con una excepción

`Invoke(Configuration)` recibe una `TInvocationConfiguration`; `Invoke` a secas usa sus valores por
defecto.

| Campo | Por defecto | Qué dice |
|---|---|---|
| `Output` | `nil`, la consola | dónde se escriben la ayuda, la versión y las sugerencias de lo que se quiso escribir — cualquier `TTextWriter` |
| `Error` | `nil`, el error de la consola | dónde se escriben los errores de parse y las excepciones que se escapan |
| `EnableDefaultExceptionHandler` | `True` | encendido, una excepción que sale de una acción se escribe en `Error` como `Unhandled exception: EClase: mensaje`, con su pila cuando hay instalado un proveedor de pilas, e `Invoke` devuelve `TCommandLineExit.Failure` (1); apagado, sale de `Invoke` y la ve quien llamó |

Una `TInvocationConfiguration` declarada y sin asignar ya lleva esos valores:

```pascal
var Configuration: TInvocationConfiguration;
Configuration.Output := Writer;            // cualquier TTextWriter
ExitCode := Parsed.Invoke(Configuration);
```

### Un banner encima de la ayuda y de la versión

La raíz da las acciones de sus opciones de ayuda y de versión como `HelpAction` y `VersionAction`,
funciones del tipo que toma la acción de un comando. Las de serie escriben en la salida de la
invocación la ayuda del comando alcanzado, o la versión, y devuelven `Ok`. Para poner algo tuyo encima, sustituye la
acción por una que lo escriba y después llame a la de serie:

```pascal
function WithBanner(const Default: TCommandAction): TCommandAction;
begin
  Result :=
    function(const Parsed: IParseResult): Integer
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
- Asignar `nil` devuelve la acción de serie.
- Sustituirlas en la raíz sustituye la acción de su opción de ayuda o de versión, que es donde las
  tiene `System.CommandLine`. La sustituta borra los errores de parse, como la ayuda.

⚠️ Captura la acción de serie, nunca la raíz. Una acción que sostiene la raíz en la que está
instalada forma con ella un ciclo de referencias, y ninguna de las dos se libera nunca.

---

## Una gramática congelada

`Freeze` cierra una gramática. A partir de ahí, añadir un comando, una opción o un argumento, o
cambiar uno ya declarado —un alias, la aridad, los valores admitidos, el valor por defecto, una
validación, la versión— lanza `ECommandLine` nombrando el símbolo. Alcanza a todo lo ya declarado
bajo la raíz. Las acciones siguen abiertas, también las de ayuda y versión, porque se leen al invocar
el resultado:

```pascal
Root.Freeze;
Root.SetAction(Greet);                             // bien
Root.Add(CommandLine.Option<Boolean>('--loud'));   // ECommandLine
```

El código que decide con un resultado del parse puede contar con que la gramática sigue siendo con la
que se parseó. `System.CommandLine` no tiene equivalente.

## Divergencias con `System.CommandLine`

**1 · El valor se lee al revés.** En .NET se escribe `result.GetValue(fileOption)`. Aquí,
`fileOption.ValueIn(parsed)`: el objeto le pregunta al resultado:

- Una interfaz **no puede declarar un método genérico**, punto — `E2535 Interface methods must not
  have parameterized methods`. Así que `IParseResult.GetValue<T>` no puede existir, fuera
  cierto lo demás o no.
- Y ni siquiera en una clase infiere Delphi la `T` a través de un parámetro `IOption<T>` — `E2010
  Incompatible types: 'IOption<T>' and 'IOption<string>'`. La inferencia funciona cuando el parámetro
  **es** `T`, no cuando `T` va anidada dentro de otro genérico.

`ValueIn` no necesita inferencia porque `T` ya viene fijada por el receptor.

**2 · Los nombres se comparan sin distinguir mayúsculas.** .NET es **sensible** por convención POSIX;
aquí `--OUT` y `--out` son la misma opción. El coste aceptado: dos alias que solo difieran en
mayúsculas son el mismo alias, y una CLI donde el caso **es** el significado —`git clean -x` frente a
`-X`— no se puede expresar. Para eso, dos opciones.

**3 · Una fecha y una hora se leen en ISO 8601 y de ninguna otra forma.** El `DateTime`, el
`DateOnly` y el `TimeOnly` de .NET admiten además la convención local. Aquí `--since 2026-08-25` vale
y `--since 25/08/2026` se rechaza, nombrando la opción y el texto; y una hora es `22:30`, no
`10:30 PM`. El coste aceptado: quien teclee a mano tiene que escribir la fecha como la lee la máquina.

**4 · Un error de parse devuelve 2, no 1.** .NET devuelve 1 tanto a un error de parse como a una
excepción que nadie capturó. Aquí `Invoke` devuelve `TCommandLineExit.InvocationError` (2) a una
invocación que no encaja con la gramática, y el 1 queda para una acción que falló.

**5 · La invocación es síncrona.** No hay acción asíncrona, ni cancelación, ni manejo de la
terminación del proceso —ni Ctrl+C, ni tiempo de gracia antes de terminar—.
`TInvocationConfiguration` lleva la salida, el error y el manejador de excepciones por defecto, y
nada más.

**6 · Leer un valor rechazado lanza `ECommandLine`.** .NET lanza `InvalidOperationException`. Aquí es
la excepción propia del módulo, la que lanza ante una gramática mal declarada o cambiada después de
congelarla.

**7 · Nada se escribe en color.** .NET escribe en rojo los errores de parse y una excepción que se
escapa, en una consola. Aquí son texto plano, y una excepción que se escapa lleva su pila solo cuando
hay instalado un proveedor de pilas.

**8 · Una opción que declara la aplicación gana a la ayuda y a la versión de la raíz.** En .NET
responden antes las opciones de ayuda y de versión que trae la raíz; aquí, una opción declarada con
uno de sus nombres.

**9 · No hay `DateTimeOffset`.** Delphi no tiene un tipo para una fecha con su desfase. Un
`TDateTime` escrito con zona —`2026-08-25T23:30+02:00`— se convierte a la hora local.

**10 · Un nombre de fichero o una ruta vacíos se rechazan.** `AcceptLegalFileNamesOnly` y
`AcceptLegalFilePathsOnly` de .NET aceptan un valor vacío; aquí los dos lo rechazan.

---

## Lo que todavía no está

Las directivas `[diagram]` y `[suggest]` y el autocompletado no se portan.

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
