# QuickStart — arrancar una aplicación DAF

El arranque mínimo: se construye, se registra un servicio, se ejecuta. Nada más.

Este sample es **la fuente del snippet de inicio rápido** de la documentación: el bloque de
`docs/GUIDE.md` sale de `QuickStart.dpr`, y `eng/check-quickstart-snippet.py` compara los dos. Si
el arranque cambia, el sample deja de compilar y el check deja de cuadrar — que es el punto.

## Qué enseña

- `TCliApplication.CreateBuilder` devuelve un `ICliApplicationBuilder` que ya está vivo: `Services`
  responde antes de construir nada, así que el registro ocurre donde está escrito.
- `OnExecute` recibe el proveedor ya construido y el resultado del parseo, y devuelve el código de
  salida — `TCliExit.Ok` cuando el trabajo termina bien.
- `Build` devuelve la `ICliApplication`, y `Run` es lo que se asigna a `ExitCode`.

## Por qué el servicio vive en su propia unit

`QuickStart.Greeter.pas` existe por obligación del contenedor, no por gusto: un tipo declarado en
el `.dpr` no está en la sección `interface` de ninguna unit, y resolverlo aborta en ejecución con
`ENonPublicType`.

## Ejecutar

```
BDS make --project:src/Application/samples/QuickStart/QuickStart.dproj
out\Win32\samples\bin\QuickStart.exe
```

Salida esperada:

```
Hola desde DAF
```
