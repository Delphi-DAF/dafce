# StartupReport — el informe del arranque, y qué se ve cuando falla

Lo que el `.dpr` decide antes de que exista el host: dónde queda el registro del arranque, y qué
enseña un arranque que no llega a completarse. Es el programa que
[Composición del arranque](../../../DependencyInjection/docs/STARTUP-COMPOSITION.md) cita.

## Qué enseña

- `BootstrapLog.AddSink` con un `TFileBootstrapSink`: el canal de arranque escribe donde la
  aplicación diga. DAF no inventa la ruta, porque no sabe dónde viven los datos de nadie.
- El `try…except` alrededor de la composición entera, con las tres cosas que un fallo de arranque
  necesita: el registro por `BootstrapLog.LoggerFor(<categoría>)`, un `ExitCode` distinto de cero
  para la consola y el desatendido, y el `raise` que hace que el escritorio muestre el fallo.
- `AddPayroll(Builder.Services)` — registros agrupados en una función, que es la forma cuando el
  grupo no necesita un tipo. Un grupo que necesita un tipo es un módulo.

## Provocar el fallo

```
out\Win32\samples\bin\StartupReport.exe --fail
```

`AddPayroll` aborta, el `except` escribe el motivo en `data\startup.log`, devuelve `1` y propaga.
Sin el `--fail` el servicio arranca y el programa espera: sale con **Ctrl+C**, que es lo que
`WaitForShutdown` atiende.

## Compilar

```
BDS make --project:src/Hosting/samples/StartupReport/StartupReport.dproj
out\Win32\samples\bin\StartupReport.exe
```

Salida esperada del arranque correcto:

```
La nomina esta en marcha
Press Ctrl+C to exit...
```
