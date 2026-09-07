# QuickStart — arrancar una herramienta de línea de comandos

Tres pasos: se construye, se declara el trabajo, se ejecuta. Sin servicios y sin una segunda unit.

Este sample es **la fuente del snippet de inicio rápido** de la documentación:
`eng/check-quickstart-snippet.py` compara el bloque del documento con `QuickStart.dpr`. Si el
arranque cambia, el sample deja de compilar y el check deja de cuadrar — que es el punto.

## Qué enseña

- `TCliApplication.CreateBuilder` devuelve un `ICliApplicationBuilder` que ya está vivo.
- `OnExecute` recibe el proveedor ya construido y el resultado del parseo, y devuelve el código de
  salida — `TCliExit.Ok` cuando el trabajo termina bien.
- `Build` devuelve la `ICliApplication`, y `Run` es lo que se asigna a `ExitCode`.

`Daf.Extensions.DependencyInjection` está en el `uses` aunque el sample no registre nada: la firma
de `OnExecute` nombra `IServiceProvider`, y Delphi exige que el tipo esté visible para escribirla.

## Los otros dos niveles de Cli

- [`ServiceWiring`](../ServiceWiring/) — cómo entra un servicio.
- [`GreetTool`](../GreetTool/) — gramática de verdad, `--help`, códigos de salida por política.

## Ejecutar

```
BDS make --project:src/Cli/samples/QuickStart/QuickStart.dproj
out\Win32\samples\bin\QuickStart.exe
```

Salida esperada:

```
Hola desde DAF
```
