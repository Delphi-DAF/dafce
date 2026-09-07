# ServiceWiring — cómo entra un servicio en una app de CLI

El nivel siguiente a [`QuickStart`](../QuickStart/): lo mismo, más un servicio registrado en el
builder y resuelto por el trabajo.

## Qué enseña

- `B.Services` responde **antes** de construir nada, así que el registro ocurre donde está escrito.
- `OnExecute` recibe el `IServiceProvider` ya construido: el trabajo resuelve del contenedor sin
  guardarse una referencia a nada.

## Por qué el servicio vive en su propia unit

`ServiceWiring.Greeter.pas` existe por obligación del contenedor, no por gusto: un tipo declarado
en el `.dpr` no está en la sección `interface` de ninguna unit, y resolverlo aborta en ejecución
con `ENonPublicType`.

Y `{$STRONGLINKTYPES ON}` está en el `.dpr` por la misma razón: sin él el enlazador descarta la
RTTI de los tipos que nadie referencia por nombre, que es exactamente cómo el contenedor los
encuentra.

## Componer registros en grupo

Cuando los registros son varios, agruparlos en un `TAppModule` es lo que enseña
`DependencyInjection/samples/ModuleComposition`.

## Ejecutar

```
BDS make --project:src/Cli/samples/ServiceWiring/ServiceWiring.dproj
out\Win32\samples\bin\ServiceWiring.exe
```

Salida esperada:

```
Hola desde DAF
```
