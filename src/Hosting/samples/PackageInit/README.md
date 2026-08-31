# PackageInit

Un paquete que trae su propia inicialización de arranque.

`TLegacyEngine` es una clase estática que resuelve por su cuenta a través de un
`IServiceProvider` global: hay que sembrarla con el proveedor **ya construido**. Eso es
fase 2 del arranque, y su contrato es `IStartupTask` — no `IHostedService`, porque no hay
nada que parar, ni `IAppModule`, porque cuando el módulo corre todavía no existe el
proveedor.

El paquete registra la tarea dentro de su propia función `AddLegacyEngine`, así que el
ejecutable escribe una línea y no menciona ni el tipo de la tarea ni lo que hace:

```pascal
AddLegacyEngine(B.Services);
```

Dos detalles del mismo ejemplo:

- `AddLegacyEngine` usa `TryAddSingleton<IClock, TClock>` para su default: si el arranque
  ya registró un `IClock`, el del paquete no lo pisa.
- Llamar a la función enlaza su unit. El `uses` con comentario `// force-link:` deja de
  hacer falta.

```
BDS make --project:src/Hosting/samples/PackageInit/PackageInit.dproj
out\Win32\samples\bin\PackageInit.exe
```
