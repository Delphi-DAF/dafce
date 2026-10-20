# SystemProcess

**🌍 Idioma: [English](README.md) | Español**

`TSystemProcess` — ejecuta procesos externos de forma síncrona o asíncrona, con callbacks para la
salida estándar, el error estándar, la finalización y el fallo. Su análogo en .NET es
`System.Diagnostics.Process`.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![Licencia](https://img.shields.io/badge/licencia-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Ejecutar un proceso

```pascal
uses Daf.SystemProcess;

var Proc := TSystemProcess.Builder
  .Command('git')
  .CmdArgs(['log', '--oneline', '-10'])
  .WorkingDir('C:\MyRepo')
  .Timeout(30000)               // ms; sin esta llamada no hay plazo
  .HideWindow(True)
  .OnStdOut(procedure(Text: string) begin Write(Text); end)
  .OnStdErr(procedure(Text: string) begin Write(Text); end)
  .OnCompleted(procedure(R: TProcessResult)
    begin
      WriteLn('Exit=', R.ExitCode, ' duration=', R.Duration.TotalMilliseconds, 'ms');
    end)
  .OnFailed(procedure(R: TProcessResult)
    begin
      WriteLn('Failed: ', R.LastError);
    end)
  .Build;

var Result := Proc.Execute;      // síncrono
var Future := Proc.ExecuteAsync; // o asíncrono
```

`Timeout` fija un plazo en milisegundos, y no hay ninguno mientras no se llame. **Al vencer, el
proceso muere** y el resultado vuelve como `Timeout`. `Timeout(0)` es un plazo de cero: mata el
proceso en el primer sondeo, a los 250 ms.

`System.Diagnostics.Process` lo escribe en dos pasos —`WaitForExit(ms)` devuelve `false` y el
`Kill()` es cosa tuya—, y .NET 11 los junta en `SafeProcessHandle.WaitForExitOrKillOnTimeout`. Aquí
`Execute` es dueño de la ejecución entera y es él quien hace valer el plazo. Para parar al hijo por
tu cuenta, llama a `Kill` desde otro hilo o desde un hook de apagado, y el resultado dice `Killed` y
no `Timeout`.

`OnStdOut` y `OnStdErr` reciben lo que devolvió cada lectura del canal — hasta 2 KB de golpe, y
**no una línea**. Una línea puede llegar partida en dos llamadas, o varias en una; si necesitas
líneas, pártelas tú.

## Una vez lanzado el hijo

`ExecuteAsync` lanza al hijo antes de volver, en el hilo de quien lo llama, y la tarea que devuelve
solo espera a que termine. Al volver, `Id` responde e `IsRunning` es `True` mientras el hijo corre.
Un lanzamiento que falla devuelve una tarea ya resuelta con `Failed`, y `OnFailed` ya ha corrido en
el hilo de quien llamó.

`Id` es el identificador que el sistema dio al hijo, desde que se lanza y también después de que
termine. Leído sin un hijo lanzado —antes de ejecutar, o tras un lanzamiento que falló— lanza
`EInvalidOpException`: `No process is associated with this object.`

```pascal
var Future := Proc.ExecuteAsync;
WriteLn('Started PID ', Proc.Id);
```

`Process.Id` en .NET es un `int`. Aquí `Id` es un `Cardinal`, el tipo que reciben
`TProcess.KillById` y `TProcess.HasExited`, así que `TProcess.KillById(Proc.Id)` no necesita
conversión.

## Adónde va cada flujo

La salida estándar y la de error van cada una a un destino, un `TProcessOutput`:

| Destino | El hijo escribe en |
|---|---|
| `TProcessOutput.Inherit` | el handle estándar que tiene el padre cuando arranca el hijo |
| `TProcessOutput.Discard` | el dispositivo nulo |
| `TProcessOutput.Capture(Proc)` | una tubería que se lee mientras el hijo vive; `Proc` recibe lo que devuelve cada lectura |
| `TProcessOutput.ToFile(Path, Append)` | un fichero, con `Path` resuelta contra el directorio actual del padre; se vacía antes salvo que `Append` sea `True` |

`StandardOutput` y `StandardError` los declaran, y `OnStdOut(Proc)` es la forma corta de
`StandardOutput(TProcessOutput.Capture(Proc))`:

```pascal
var Proc := TSystemProcess.Builder
  .Command('git')
  .CmdArgs(['fetch'])
  .OnStdOut(procedure(Text: string) begin Write(Text); end)
  .StandardError(TProcessOutput.Discard)
  .Build;
```

- **Un flujo sin declarar hereda**: el hijo escribe donde escribe el proceso que lo lanza. La entrada
  estándar es siempre la del padre.
- **Solo un flujo capturado pasa por una tubería**, y esa tubería se lee mientras el hijo vive:
  escriba lo que escriba, el hijo termina.
- **Un fichero se abre antes de que arranque el hijo.** Si no se puede abrir, la ejecución acaba como
  `Failed`, con la ruta y el motivo en `LastError`, y el hijo no llega a arrancar.
- **Un flujo admite una sola declaración.** La segunda —`OnStdOut` después de `StandardOutput`, o la
  misma llamada dos veces— lanza `EInvalidOpException`:
  `StandardOutput already has a destination: a stream takes exactly one`.

`System.Diagnostics.Process` en .NET 11 recibe un handle por flujo, en `StandardOutputHandle` y
`StandardErrorHandle`. Aquí el valor es un destino, `TProcessOutput`, y los métodos son
`StandardOutput` y `StandardError`.

### El texto capturado

Se decodifica con la página de la consola en que corre el hijo: la de salida de la consola del padre si
la comparte, y la OEM del sistema si tiene una propia —con `HideWindow(True)`, o cuando el padre no
tiene consola—. Un carácter cuyos bytes llegan en dos lecturas se entrega entero, y un byte que no
encaja en la página llega sustituido.

Un hijo que no escribe con la página de su consola necesita su codificación declarada, flujo a flujo,
con `StandardOutputEncoding` y `StandardErrorEncoding`. Un programa Delphi escribe en ANSI salvo que
cambie la página de su `Output`:

```pascal
  .OnStdOut(procedure(Text: string) begin Write(Text); end)
  .StandardOutputEncoding(TEncoding.ANSI)
```

El proceso no libera la codificación: `TEncoding.ANSI`, `TEncoding.UTF8` y las demás estándar son de
la RTL, y una creada con `TEncoding.GetEncoding` la libera quien la creó, después de `Execute`.

`System.Diagnostics.Process` decodifica siempre con la página de salida de la consola del padre.

### La consola del hijo

`HideWindow(False)`, el valor por defecto, corre el hijo en la consola del padre; `HideWindow(True)` lo
corre sin ventana, en una consola propia. Un padre sin consola —una aplicación de ventanas— le da al
hijo una consola propia en los dos casos, y los flujos que hereda escriben en ella.

## TProcessResult

| Campo | Tipo | Descripción |
|-------|------|-------------|
| `Status` | `TStatus` | `Failed`, `Timeout`, `Running`, `Completed`, `Canceled`, `Killed` — ver abajo |
| `ExitCode` | `DWORD` | El código de salida del proceso si `Completed`; `GetLastError` si `Failed`; 0 en el resto |
| `Duration` | `TTimeSpan` | Tiempo de ejecución de reloj |
| `LastError` | `string` | Descripción del error. Solo `Failed` lo rellena |
| `Succeeded` | `function: Boolean` | `Status = Completed` y `ExitCode = 0` |

`TStatus` es de ámbito, así que sus valores se escriben `TProcessResult.TStatus.Completed`:

| Valor | Cuándo |
|-------|--------|
| `Running` | el proceso sigue vivo — es el valor que recibe `OnIdle` en cada sondeo |
| `Completed` | terminó por su cuenta; `ExitCode` es el del proceso |
| `Failed` | no se pudo lanzar, o la espera acabó sin que el proceso señalizara |
| `Timeout` | venció el plazo y el proceso murió |
| `Killed` | se llamó a `Kill`; el plazo no tuvo nada que ver |
| `Canceled` | el token de cancelación se señalizó y el proceso muere. `Failed` si matarlo no sale bien |

Las tres formas de acabar sin que el hijo salga por su pie —plazo, `Kill` y cancelación— tienen
valor propio, así que un resultado dice **por qué** y no solo que se cortó.

## Cooperar con el apagado

`Daf.Threading` aporta el hook; matar el proceso desde ahí evita que un hijo de larga duración
sobreviva a la aplicación.

```pascal
TShutdownHook.OnShutdownRequested := procedure
begin
  Proc.Kill;
end;
```

---

## Sample

- [`PingProcess`](samples/PingProcess/) — lanza `ping` en segundo plano, muestra el `Id` del hijo y
  va mostrando su salida según llega.

## Nota

Esta unit formaba parte de `Daf.Commons` hasta que se mudó a un paquete propio. Ni el nombre de la
unit ni el de sus tipos han cambiado: basta añadir una referencia a `Daf.SystemProcess` y las
cláusulas `uses` existentes siguen compilando. Se fue porque `Daf.Commons` es el suelo que pisan
todos los demás paquetes, y lanzar procesos es algo que hace una *aplicación* — no una extensión de
la biblioteca estándar.
