# Inyección de Dependencias con [Inject]

**🌍 Idioma: [English](injection.md) | Español**

[← Volver a la Guía](../GUIDE.es.md)

---

MiniSpec incluye un sistema ligero de inyección de dependencias para propiedades marcadas con `[Inject]`.

## Uso Básico

Inyección automática del FeatureContext:

```pascal
uses
  Daf.MiniSpec,
  Daf.MiniSpec.Injection;  // Para el atributo [Inject]

type
  TFeatureContext = class
  public
    SharedValue: Integer;
  end;

  TWorld = class
  private
    [Inject] FCtx: TFeatureContext;  // Inyectado automáticamente
  public
    property Ctx: TFeatureContext read FCtx;
  end;

Feature('...')
  .UseFeatureContext<TFeatureContext>  // Registra TFeatureContext en el Injector
  .UseWorld<TWorld>             // Al crear World, inyecta FCtx
```

## Servicios Personalizados a Nivel de Suite

```pascal
MiniSpec
  .Before('Setup services', procedure
    begin
      TInjectorService.Register(TDatabaseMock.Create);
      TInjectorService.Register(THttpClientMock.Create);
    end)
  .After('Cleanup', procedure
    begin
      TInjectorService.Clear;  // Libera todos los servicios
    end);
```

## API del TInjectorService

| Método | Descripción |
|--------|-------------|
| `Register(Instance)` | Registra un servicio (la clase del objeto es la clave) |
| `Unregister(Instance)` | Elimina el registro de un servicio |
| `Resolve(AClass)` | Obtiene el servicio registrado para esa clase |
| `Resolve<T>` | Versión genérica de Resolve |
| `InjectInto(Target)` | Inyecta servicios en propiedades marcadas con `[Inject]` |
| `Clear` | Libera y elimina todos los servicios registrados |

## Errores de Inyección

Si una propiedad marcada con `[Inject]` no puede ser inyectada, se lanza `EInjectionError`:

- Propiedad no es de tipo clase
- Propiedad no tiene setter
- No hay servicio registrado compatible con el tipo

---

[← Configuración](configuration.es.md) | [Siguiente: Reporters →](reporters.es.md)
