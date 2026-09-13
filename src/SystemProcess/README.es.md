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
  .KillAfterTimeout(True)
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

`Timeout` fija un plazo en milisegundos, y no hay ninguno mientras no se llame. `Timeout(0)` es un
plazo de cero: mata el proceso en el primer sondeo, a los 250 ms.

`OnStdOut` y `OnStdErr` reciben lo que devolvió cada lectura del canal — hasta 2 KB de golpe, y
**no una línea**. Una línea puede llegar partida en dos llamadas, o varias en una; si necesitas
líneas, pártelas tú.

## TProcessResult

| Campo | Tipo | Descripción |
|-------|------|-------------|
| `Status` | `TStatus` | `Failed`, `Timeout`, `Running`, `Completed`, `Canceled` — ver abajo |
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
| `Timeout` | venció el plazo, o se llamó a `Kill`; en los dos casos el proceso muere |
| `Canceled` | el token de cancelación se señalizó y el proceso muere. `Failed` si matarlo no sale bien |

No hay valor `Killed`: tanto un `Kill` como un plazo vencido caen en `Timeout`.

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

- [`PingProcess`](samples/PingProcess/) — lanza `ping` y va mostrando su salida según llega.

## Nota

Esta unit formaba parte de `Daf.Commons` hasta que se mudó a un paquete propio. Ni el nombre de la
unit ni el de sus tipos han cambiado: basta añadir una referencia a `Daf.SystemProcess` y las
cláusulas `uses` existentes siguen compilando. Se fue porque `Daf.Commons` es el suelo que pisan
todos los demás paquetes, y lanzar procesos es algo que hace una *aplicación* — no una extensión de
la biblioteca estándar.
