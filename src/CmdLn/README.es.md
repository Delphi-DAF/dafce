# CmdLn

**🌍 Idioma: [English](README.md) | Español**

Una gramática de línea de comandos tipada: declaras comandos, subcomandos y argumentos, y el parseo
sale de esa declaración en vez de recorrer `ParamStr` a mano.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![Licencia](https://img.shields.io/badge/licencia-MIT-blue.svg)](../../legal/LICENSE.md)

---

## ⚠️ Este módulo está congelado

`Daf.CmdLn` funciona y sus specs pasan, pero **no va a recibir funcionalidad nueva**. Está previsto
sustituirlo por un porting de `System.CommandLine` de .NET —la librería que usa el propio `dotnet`
CLI— y retirarlo cuando el sustituto cubra su terreno.

El motivo es la forma del API, no una carencia concreta. En `System.CommandLine` el objeto que
declaras **es** la llave con la que lees el valor, así que un nombre nunca se escribe dos veces.
Aquí escribes `'file'` al declarar y `'build.file'` al leer, y nadie comprueba que coincidan: una
errata devuelve un valor vacío, no un error. Cambiar eso tocaría el corazón de cómo se consume la
gramática, así que se sustituye en vez de ampliarse.

Tampoco están, ni se van a añadir aquí: ayuda generada, argumentos posicionales, opciones heredadas
por los subcomandos y alias de comando.

**Limitaciones conocidas**, que se quedan por el mismo motivo:

- Los argumentos enteros y de coma flotante casan con patrones sin signo, así que **no se puede pasar
  un número negativo**.
- Los patrones se anclan al principio pero no al final, así que `--port 80xyz` se acepta como `80`.

Si vas a empezar una aplicación de línea de comandos nueva, tenlo en cuenta antes de adoptarlo.

---

## Declarar una gramática

```pascal
uses Daf.CmdLn.Parser;

var Parser := TCmdLnParserBuilder.Create
  .Arg<string>('file')      // argumento con nombre, de tipo cadena
  .Arg<Integer>('port')     // argumento con nombre, entero
  .Arg<Boolean>('verbose')  // flag: basta con nombrarlo
  .Build;

var Params := Parser.Parse(GetCommandLine);

var FilePath  := Params['file'].AsString;
var Port      := Params['port'].AsInteger;
var IsVerbose := Params['verbose'].AsBoolean;
```

`Parse` devuelve `nil` cuando la invocación no casa con la gramática.

### Nombres cortos, valores por defecto y obligatoriedad

```pascal
.Arg<string>('output|o', procedure(A: TArgNode)
  begin
    A.Default('out').Help('Directorio destino').Required(True);
  end)
```

El tipo dirige la conversión: un argumento enumerado solo admite los nombres de sus propios valores,
derivados por RTTI.

### Subcomandos

Cada comando es dueño de sus argumentos y de sus subcomandos, y el anidamiento no tiene límite de
profundidad. Un comando que declara subcomandos exige que se nombre uno.

```pascal
var Parser := TCmdLnParserBuilder.Create
  .Command('build')
    .Arg<string>('output')
  .EndCommand
  .Command('run')
    .Arg<Boolean>('watch')
  .EndCommand
  .Build;

var Params := Parser.Parse(GetCommandLine);
if Params.Command = 'build' then
begin
  var Section := Params.GetSection('build');
  var OutDir  := Section['output'].AsString;
end;
```

⚠️ Los argumentos de un comando se consumen **antes** que su subcomando, así que
`app --flag build -o dir` parsea y `app build -o dir --flag` no.

---

## No confundir con la configuración

Leer la línea de comandos como **fuente de configuración** —`--Logging:LogLevel=Debug` anulando un
valor de `appsettings.json`— es otro trabajo, y le corresponde a
[`Daf.Configuration.CommandLine`](../Configuration/README.es.md). Los dos no comparten motor, igual
que `Microsoft.Extensions.Configuration.CommandLine` no usa `System.CommandLine`.
