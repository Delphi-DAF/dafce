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
  .Timeout(30000)               // ms, 0 = INFINITO
  .KillAfterTimeout(True)
  .HideWindow(True)
  .OnStdOut(procedure(Line: string) begin WriteLn(Line); end)
  .OnStdErr(procedure(Line: string) begin WriteLn('ERR: ', Line); end)
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

## TProcessResult

| Campo | Tipo | Descripción |
|-------|------|-------------|
| `Status` | `TStatus` | `Completed`, `Failed`, `Timeout`, `Canceled`, `Killed` |
| `ExitCode` | `DWORD` | Código de salida del proceso |
| `Duration` | `TTimeSpan` | Tiempo de ejecución de reloj |
| `LastError` | `string` | Descripción del error (casos fallido / matado) |
| `Succeeded` | `function: Boolean` | `Status = Completed` y `ExitCode = 0` |

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
