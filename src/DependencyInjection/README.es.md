# DependencyInjection

**🌍 Idioma: [English](README.md) | Español**

Contenedor IoC/DI para Delphi con una API fluida inspirada en `Microsoft.Extensions.DependencyInjection` de .NET. Registra servicios una vez, resuélvelos en cualquier parte — con tiempos de vida `Singleton`, `Scoped` y `Transient`.

[![Delphi 12+](https://img.shields.io/badge/Delphi-12%2B-red.svg)](https://www.embarcadero.com/products/delphi)
[![Licencia](https://img.shields.io/badge/licencia-MIT-blue.svg)](../../legal/LICENSE.md)

---

## ¿Por qué usarlo?

- 🔌 **API familiar** — Los mismos métodos `AddSingleton` / `AddTransient` / `AddScoped` que en .NET
- 🎯 **Genérico y type-safe** — Verificación en tiempo de compilación mediante genéricos, sin tokens de cadena
- ⚡ **Tres modos de vida** — Singleton, Scoped (por scope) y Transient (nueva instancia siempre)
- 🏭 **Registro flexible** — Por clase, por lambda de factoría o por instancia existente
- 💤 **Resolución diferida** — `Lazy.From<T>()` pospone la creación hasta el primer uso
- 🦆 **Duck Typing** — `TDuckDecorator<T>` envuelve cualquier objeto que implemente la misma forma de interfaz
- 📦 **Sin dependencias externas** — Solo depende de `DependencyInjection.Abstractions`

---

## Inicio rápido

```pascal
uses
  Daf.Extensions.DependencyInjection,
  Daf.DependencyInjection;

var
  Services: IServiceCollection;
  Provider: IServiceProvider;
begin
  Services := TServiceCollection.Create;

  // Registrar
  Services.AddSingleton<IMyService, TMyService>;

  // Construir el contenedor (congela la colección)
  Provider := Services.BuildServiceProvider;

  // Resolver
  var Svc := Provider.GetRequiredService<IMyService>;
  Svc.DoWork;

  // Siempre hacer Shutdown al finalizar
  Provider.ShutDown;
end;
```

---

## Sobrecargas de registro

```pascal
// Por clase de implementación
Services.AddSingleton<IMyService, TMyServiceImpl>;

// Por lambda de factoría (control total)
Services.AddTransient<IMyService>(
  function(P: IServiceProvider): IMyService
  begin
    Result := TMyServiceImpl.Create(P.GetRequiredService<IConfig>);
  end);

// Por instancia existente (solo Singleton)
Services.AddSingleton<IMyService>(MyExistingInstance);
```

---

## Scopes

```pascal
var Scope := Provider.CreateScope;
try
  var Svc := Scope.ServiceProvider.GetRequiredService<IScopedService>;
  // Las instancias Scoped son únicas dentro de este scope
finally
  Scope := nil; // dispara Shutdown + limpieza gestionada
end;
```

---

## Patrón AppModule

Organiza los registros por módulo:

```pascal
type
  TMyModule = class(TInterfacedObject, IAppModule)
    procedure AddServices(const Services: IServiceCollection);
  end;

procedure TMyModule.AddServices(const Services: IServiceCollection);
begin
  Services.AddSingleton<IMyService, TMyService>;
  Services.AddTransient<IOther, TOtherImpl>;
end;
```

Consulta [DependencyInjection.Abstractions](../DependencyInjection.Abstractions/README.es.md) para la interfaz `IAppModule` y todos los contratos clave.

---

## Documentación

- 📖 [Guía de uso](docs/GUIDE.es.md) — tiempos de vida, scopes, Lazy, DuckDecorator, integración con Hosting
