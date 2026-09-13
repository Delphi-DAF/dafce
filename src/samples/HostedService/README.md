# HostedService — componer el log desde la configuración, y un worker inyectado

Un sample de la edición y no de un módulo: combina `Daf.Cli` y `Daf.NNLog`, y ningún paquete declara
los dos, así que no hay módulo bajo el que archivarlo.

## Qué enseña

- **El builder responde antes de construir nada.** `B.Environment.BinPath` y
  `B.Environment.ApplicationName` componen la ruta del `appsettings.json` ahí mismo, y el proveedor
  de log se compone leyendo lo que se acaba de añadir:
  `B.Logging.AddProvider(TNNLogLoader.BuildProvider(B.Environment, B.Configuration.GetSection('Logging:NLog')))`.
- **El fichero de log configura tres destinos y dos reglas**: consola con `layout` y color, fichero
  bajo `${binPath}logs/`, y UDP. Las tres unidades de destino —`DAF.NNLog.Targets.Console`,
  `.File_` y `.UDP`— están en el `uses` del `.dpr` porque el cargador las resuelve por RTTI y el
  enlazador descarta lo que nadie nombra. Por lo mismo está `{$STRONGLINKTYPES ON}`.
- **El worker se construye por inyección**: `THostedWorker.Create(const LoggerFactory: ILoggerFactory;
  const Lifetime: IHostApplicationLifetime)`, y el contenedor resuelve los dos.
- **La parada es cooperativa.** El worker se registra en `Lifetime.ApplicationStopping` para avisar,
  y su bucle espera con `Token.WaitFor(1000) = wrTimeout` en vez de dormir: así la cancelación se
  atiende en cuanto llega y no al final del segundo.
- **Ámbitos de log anidados**: `BeginScope('working with guid {guid}', […])` envuelve el bucle
  entero, y dentro `BeginScope('scope {number}', [Idx], procedure …)` envuelve cada vuelta.

## Sin trabajo declarado, el host arranca y para

El `.dpr` no declara `OnExecute` ni registra un `ICliHandler`, y para `TCliApplication` eso es un
desenlace válido: la aplicación *son* sus tareas de arranque y sus servicios alojados, así que `Run`
arranca el host, no encuentra trabajo y lo para. El apagado señaliza `ApplicationStopping` antes de
llamar a `Stop`, de modo que **el bucle del worker no llega a dar una vuelta**:

```
INFO [HostedWorker.THostedWorker] THostedWorker.Start called
WARN [HostedWorker.THostedWorker] El host se está apagando, THostedWorker preparando su salida...
INFO [HostedWorker.THostedWorker] Servicio finalizó correctamente
INFO [HostedWorker.THostedWorker] THostedWorker.Stop called
```

Para ver el worker trabajando hace falta darle al `.dpr` algo que espere — un `OnExecute` que
bloquee hasta la señal de apagado.

## Ejecutar

```
BDS make --project:src/samples/HostedService/HostedService.dproj
out\Win32\samples\bin\HostedService.exe
```
