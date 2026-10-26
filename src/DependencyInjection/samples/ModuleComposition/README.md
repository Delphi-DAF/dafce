# ModuleComposition — agrupar registros en módulos

Tres módulos aplicados en el orden escrito: uno de infraestructura, uno de dominio que consume lo
que el primero registró, y uno con configuración que solo puede entrar por la sobrecarga de
instancia.

Este sample es **la fuente de los dos bloques** de la sección *AppModule Pattern* de
`docs/GUIDE.md`.

## Qué enseña

- `AddModule<T>` instancia el módulo, llama a su `AddServices` **en el acto** y se hace cargo de su
  vida. Nadie llama a `AddServices` a mano: el módulo es un `TInterfacedObject` creado con contador
  a cero, así que hacerlo lo deja sin dueño.
- **El orden de las llamadas es el orden de registro**: si dos módulos registran el mismo servicio,
  gana el último aplicado, y un `TryAdd` escrito después de un módulo respeta lo que el módulo puso.
- `TDomainModule` registra `IGreeter` con una factoría que resuelve `IClock` **al construir el
  greeter**, no al componer: para entonces están aplicados los tres módulos, así que lo que necesita
  es que `TInfrastructureModule` entre en la composición, no que entre antes.
- Un módulo con configuración no la puede recibir del contenedor —corre antes de que exista el
  proveedor—, así que la construye el llamante: `TBannerModule.Create(...)` entregado como
  `IAppModule`.
- Los tipos viven en `ModuleComposition.Services.pas` por obligación del contenedor: un tipo
  declarado en el `.dpr` no está en la sección `interface` de ninguna unit, y resolverlo aborta en
  ejecución con `ENonPublicType`.

## Ejecutar

```
BDS make --project:src/DependencyInjection/samples/ModuleComposition/ModuleComposition.dproj
out\Win32\samples\bin\ModuleComposition.exe
```

Salida esperada, con la fecha del día:

```
Composicion por modulos
Hola, DAF. Hoy es 2026-08-31
```
