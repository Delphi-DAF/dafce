# DependencyInjection.Abstractions

**🌍 Idioma: [English](README.md) | Español**

Contratos principales (interfaces y tipos) del sistema de Inyección de Dependencias de DAF. Este módulo **no contiene implementación** — define la forma de la que dependen los consumidores y que `DependencyInjection` implementa.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![Licencia](https://img.shields.io/badge/licencia-MIT-blue.svg)](../../legal/LICENSE.md)

---

## Qué hay en este módulo

| Unidad | Contenido |
|--------|-----------|
| `Daf.Extensions.DependencyInjection` | `IServiceCollection`, `IServiceProvider`, `IServiceScope`, tiempos de vida, `Factory`, `TDuckDecorator<T>`, `Lazy` |
| `Daf.AppModule` | `IAppModule` — patrón de registro por módulo |
| `Daf.DependencyInjection.ActivatorUtilities` | Helpers para construir objetos mediante RTTI con argumentos resueltos por DI |

---

## Tipos clave

### `IServiceCollection`

Constructor fluido para registrar servicios antes de construir el contenedor.

```pascal
Services
  .AddSingleton<ICache, TMemoryCache>
  .AddTransient<IRepo, TRepoImpl>
  .AddScoped<IUoW, TDbUoW>;

var Provider := Services.BuildServiceProvider;
```

### `IServiceProvider`

El contenedor resuelto. Úsalo para obtener instancias de servicios.

```pascal
var Svc := Provider.GetRequiredService<IMyService>;
var Opt: IMyService;
if Provider.TryGet<IMyService>(Opt) then ...
var All := Provider.GetServices<IPlugin>;
```

### `TServiceLifeTime`

```pascal
type TServiceLifeTime = (Singleton, Scoped, Transient);
```

### `IServiceScope`

Frontera de tiempo de vida. Los servicios Scoped se comparten dentro de un scope y se liberan cuando este termina.

```pascal
var Scope := Provider.CreateScope;
var Svc := Scope.ServiceProvider.GetRequiredService<IScopedService>;
Scope := nil; // libera las instancias Scoped
```

### `IAppModule`

Convención para agrupar registros de servicios:

```pascal
type
  IAppModule = interface(IInvokable)
    procedure AddServices(const Services: IServiceCollection);
  end;
```

### `Factory`

Helper para construir un `TServiceFactory` desde una clase o instancia:

```pascal
Services.Add(TypeInfo(IMyService), Singleton, Factory.From(TypeInfo(IMyService), TMyImpl));
```

### `TDuckDecorator<T>`

Clase base para envolver un servicio con lógica pre/post-llamada sin herencia. Sobreescribe `BeforeCall` / `AfterCall`.

### `Lazy`

```pascal
Result := Lazy.From<IHeavy>(Provider, function(P): IHeavy begin Result := THeavy.Create end);
```

---

## Dependencias

Este módulo **no depende** de ningún otro módulo DAF. Solo requiere la RTL de Delphi (`System.Rtti`, `System.Generics.Collections`).

La implementación está en [`DependencyInjection`](../DependencyInjection/README.es.md).
