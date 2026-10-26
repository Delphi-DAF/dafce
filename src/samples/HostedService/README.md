# HostedService — componer el log desde la configuración, y un worker inyectado

Un sample de la edición y no de un módulo: combina `Daf.Cli` y `Daf.NNLog`, y ningún paquete declara
los dos, así que no hay módulo bajo el que archivarlo.

## Qué enseña

- **El builder responde antes de construir nada.** `B.Environment.BinPath` y
  `B.Environment.ApplicationName` componen la ruta del `appsettings.json` ahí mismo, y el proveedor
  de log se compone leyendo lo que se acaba de añadir:
  `B.Logging.AddProvider(TNNLogLoader.BuildProvider(B.Environment, B.Configuration.GetSection('Logging:NLog')))`.
- **El fichero de log configura tres destinos y dos reglas**: consola con `layout` y color, fichero
  bajo `${binPath}logs/`, y UDP. Las tres unidades de destino —`Daf.NNLog.Targets.Console`,
  `.File_` y `.UDP`— están en el `uses` del `.dpr` porque el cargador las resuelve por RTTI y el
  enlazador descarta lo que nadie nombra. Por lo mismo está `{$STRONGLINKTYPES ON}`.
- **El worker se construye por inyección**: `THostedWorker.Create(const LoggerFactory: ILoggerFactory;
  const Lifetime: IHostApplicationLifetime)`, y el contenedor resuelve los dos.
- **La parada es cooperativa.** El worker se registra en `Lifetime.ApplicationStopping` para avisar,
  y su bucle espera con `Token.WaitFor(1000) = wrTimeout` en vez de dormir: así la cancelación se
  atiende en cuanto llega y no al final del segundo.
- **Ámbitos de log anidados**: `BeginScope('working with guid {guid}', […])` envuelve el bucle
  entero, y dentro `BeginScope('scope {number}', [Idx], procedure …)` envuelve cada vuelta.

## La acción de la raíz es esperar

Una aplicación CLI puede no declarar ninguna acción, y para `TCliApplication` eso es un desenlace
válido: la aplicación *son* sus tareas de arranque y sus servicios alojados. Pero entonces `Run`
arranca el host, no encuentra acción y lo para, y como el apagado señaliza `ApplicationStopping` antes de
llamar a `Stop`, **un servicio alojado no llega a dar una vuelta**. Por eso este `.dpr` declara la
acción de la raíz, y lo que hace es esperar:

```pascal
TShutdownHook.PresCtlCMsg := 'Pulse Ctrl+C para parar el servicio...';
B.SetAction(
  function(const Services: IServiceProvider; const Parsed: IParseResult): Integer
  begin
    TShutdownHook.WaitForShutdown;
    Result := TCliExit.Ok;
  end);
```

`WaitForShutdown` imprime ese mensaje y bloquea hasta Ctrl+C. Al volver, `Run` para el host y con él
al worker.

## Qué se ve

Una línea por segundo, con los dos ámbitos anidados a la derecha:

```
INFO [HostedWorker.THostedWorker] 🟢 THostedWorker.Start called
Pulse Ctrl+C para parar el servicio...
INFO [HostedWorker.THostedWorker] I'm working at [12:17:06] ... | scope 1 | working with guid {1F7EA5E5-…}
INFO [HostedWorker.THostedWorker] I'm working at [12:17:07] ... | scope 2 | working with guid {1F7EA5E5-…}
```

Ctrl+C libera la espera: el worker avisa por su registro en `ApplicationStopping`, `Stop` espera a
que su tarea termine y la acción devuelve `TCliExit.Ok`. ⚠️ El gancho atiende **Ctrl+C** y nada
más: cerrar la ventana de la consola no recorre esa parada.

## Ejecutar

```
BDS make --project:src/samples/HostedService/HostedService.dproj
out\Win32\samples\bin\HostedService.exe
```
