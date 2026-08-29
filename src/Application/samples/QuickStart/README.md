# QuickStart — arrancar una aplicación DAF

El arranque mínimo: se construye, se registra un servicio, se ejecuta. Nada más.

Este sample es **la fuente del snippet de inicio rápido** de la documentación: el bloque de
`docs/GUIDE.md` sale de `QuickStart.dpr`, y `eng/check-quickstart-snippet.py` compara los dos. Si
el arranque cambia, el sample deja de compilar y el check deja de cuadrar — que es el punto.

## Qué enseña

- `TDafApplication.CreateHostBuilder` devuelve un `TDafApplicationBuilder`, y hay que sostenerlo
  en una variable **de esa clase**. Los `Configure*` devuelven `IHostBuilder`, cuyo `Build` da un
  `IHost`: por esa vía nunca se obtiene la aplicación.
- `Build` devuelve un `IDafApplication`, que **tiene** un host (`App.Host`) pero no *es* un `IHost`.
- `Run(Exec)` ejecuta el proc con el proveedor ya construido y para al terminar. Sin argumentos,
  `Run` no espera el apagado: para eso, `Run(True)`.

## Por qué el servicio vive en su propia unit

`QuickStart.Greeter.pas` existe por obligación del contenedor, no por gusto: un tipo declarado en
el `.dpr` no está en la sección `interface` de ninguna unit, y resolverlo aborta en ejecución con
`ENonPublicType`. Medido al escribir este sample.

## Ejecutar

```
BDS make --project:src/Application/samples/QuickStart/QuickStart.dproj
out\Win32\samples\bin\QuickStart.exe
```

Salida esperada:

```
Hola desde DAF
```
