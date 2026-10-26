# PingProcess — lanzar un proceso y leer su salida según llega

El sample de `TSystemProcess`: arranca `cmd /k ping 127.0.0.1`, va imprimiendo lo que el hijo
escribe, y recorre las tres formas de terminarlo — el plazo, el token de cancelación y el hook de
apagado.

## Qué enseña

- `TSystemProcess.Builder` compone el proceso y `Build` devuelve una `ISystemProcess`. El builder
  es un objeto crudo; la interfaz que devuelve `Build` es la contada por referencia, y por eso el
  `.dpr` acaba con `Process := nil`.
- **Los siete callbacks, todos.** `OnStdOut` y `OnStdErr` reciben la salida mientras el proceso
  corre; `OnIdle` se dispara en cada sondeo con `Status = Running`; y `OnCompleted`, `OnFailed`,
  `OnCancelled` y `OnKilled` cierran cada uno su desenlace. Aquí los cinco últimos comparten un
  mismo `Report`, que es la forma de ver cuál se dispara de verdad.
- **El `Id` del hijo está en cuanto `ExecuteAsync` vuelve**, porque lanza al hijo antes de volver:
  el sample lo imprime en la línea `Process launched, PID …`.
- **Los callbacks no llegan en el hilo principal.** `ExecuteAsync` espera al hijo en un `TTask`, y
  la salida del hijo la leen otros dos. Por eso `Report` envuelve su cuerpo en
  `TThread.Synchronize` y la cuenta atrás llama a `CheckSynchronize`: sin eso la cola de
  sincronización no se vacía nunca.
- **Cancelar es otra cosa que matar.** `ExecuteAsync(Token)` no bloquea; cuando a la cuenta atrás
  le quedan 5 segundos, `Token.Cancel` termina el proceso con `Status = Canceled` sin que nadie
  llame a `Process.Kill`.
- `TShutdownHook.OnShutdownRequested` mata el proceso si cortas la aplicación, para que un hijo de
  larga duración no le sobreviva.
- `TEnum.ToString(Result.Status)`, de `Daf.Rtti`, es lo que imprime el nombre del valor.

## Espera a Ctrl+C: no termina solo

La última línea es `TShutdownHook.WaitForShutdown`, que espera indefinidamente. El sample imprime
`Press Ctrl+C to exit...` y se queda ahí hasta que lo pulses; es lo que deja ejercitar el hook de
apagado.

## Ejecutar

```
BDS make --project:src/SystemProcess/samples/PingProcess/PingProcess.dproj
out\Win32\samples\bin\PingProcess.exe
```
