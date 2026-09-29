# HostLifetime — las notificaciones del ciclo de vida del host

Las tres notificaciones de `IHostApplicationLifetime`, y cómo suelta su referencia un componente que
guarda una copia del `IHost`.

## Qué enseña

- `ApplicationStarted`, `ApplicationStopping` y `ApplicationStopped` son tokens, y `Register` les
  añade una acción que se ejecuta al notificarse:
  - `ApplicationStarted` llega al final de `Start`, con los servicios alojados ya arrancados.
  - `ApplicationStopping` llega al pedirse la parada —con `StopApplication` o con Ctrl+C—, o al
    empezar `Stop` si nadie la había pedido.
  - `ApplicationStopped` llega dentro de `Stop`, con los servicios alojados ya parados y el host
    todavía vivo.
- `StopApplication` pide la parada por código, igual que un Ctrl+C: `WaitForShutdown` vuelve en
  cuanto se pide.
- `StopAndNil(Host)` para el host y deja vacía esa variable, y solo esa. `THostDiagnostics` guarda
  una copia de la referencia y la suelta en `ApplicationStopped`: al terminar `Stop` el host está
  liberado, y una copia que sigue puesta apunta a memoria devuelta.
- `THostDiagnostics` se libera después de parar el host: la acción que registra en
  `ApplicationStopped` escribe en él.

## Ejecutar

```
BDS make --project:src/Hosting/samples/HostLifetime/HostLifetime.dproj
out\Win32\samples\bin\HostLifetime.exe
```

Salida esperada:

```
ApplicationStarted : el host esta arrancado
Diagnostico        : el host de HostLifetime esta vivo
ApplicationStopping: se ha pedido la parada
Press Ctrl+C to exit...
ApplicationStopped : el host se ha parado
Diagnostico        : suelta su copia del host
Diagnostico        : sin host
```

`WaitForShutdown` escribe siempre cómo salir. Aquí la parada ya está pedida, así que vuelve en el
acto.
