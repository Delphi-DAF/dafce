# HostQuickStart — componer y arrancar el host genérico

El nivel de entrada de Hosting: usa solo lo que `Daf.Hosting` declara, sin línea de comandos y sin
ninguna variante de la familia por encima.

## Qué enseña

- `THostBuilder.Create` devuelve un builder ya vivo: `Services`, `Configuration`, `Environment` y
  `Logging` responden antes de construir nada.
- `AddHostedService<T>` registra algo con ciclo de vida. La regla de elección es una frase: si
  tiene algo que parar es `IHostedService`; si solo se ejecuta una vez, `IStartupTask`.
- `Build` devuelve la `IHost`, y `Start` / `Stop` son el ciclo que el host conduce.

## El dueño del builder no es decorativo

`THostBuilder` es contado por referencia. Encadenar directamente sobre `THostBuilder.Create` deja
la instancia sin dueño y se fuga; el `var Owner: IHostApplicationBuilder := Builder` del `.dpr` es
quien la libera al salir.

## Cuando la aplicación es una herramienta de consola

Entonces la variante es `TCliApplication` y el ejemplo de entrada es
[`Cli/samples/QuickStart`](../../../Cli/samples/QuickStart/): añade el borde alrededor de `Run`
—gramática, `--help`, códigos de salida— sobre este mismo host.

## Ejecutar

```
BDS make --project:src/Hosting/samples/HostQuickStart/HostQuickStart.dproj
out\Win32\samples\bin\HostQuickStart.exe
```

Salida esperada:

```
El servicio arranco
El host esta arrancado
El servicio paro
```
