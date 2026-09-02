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

## Los tres contextos, y cuál gana

Una propiedad `[Inject]` recibe cualquiera de los tres contextos vivos mientras corre un step: el
World del propio escenario, el contexto de feature declarado con `UseFeatureContext<T>` y el
contexto de suite declarado con `UseSuiteContext<T>`.

Cuando más de uno satisface el tipo pedido, el que gana está declarado:

1. Una coincidencia exacta de tipo, mirando escenario, luego feature y luego suite.
2. Si no la hay, un descendiente del tipo pedido, en ese mismo orden.

Así, una propiedad declarada con una clase base de la que descienden dos contextos recibe el más
cercano al step, y una declarada con el tipo exacto del contexto de suite recibe ese aunque un
contexto de feature descienda de él.

El inyector es de la ejecución: lo que contiene son los contextos de la ejecución en curso, y no le
sobrevive. Una ejecución anidada no ve los contextos de la que la lanzó.

## API del TInjectorService

| Método | Descripción |
|--------|-------------|
| `Register(Scope, Instance)` | Publica el contexto de un ámbito: `csScenario`, `csFeature`, `csSuite` |
| `Unregister(Scope)` | Vacía un ámbito |
| `Resolve(AClass)` | Obtiene el contexto vivo compatible con esa clase, por la precedencia de arriba |
| `Resolve<T>` | Versión genérica de Resolve |
| `InjectInto(Target)` | Inyecta contextos en propiedades marcadas con `[Inject]` |
| `Clear` | Vacía los tres ámbitos |

El runner llama a `Register` y `Unregister` por su cuenta según se crea y se destruye cada contexto.
No hay registro de servicios sueltos: el colaborador que necesite un escenario se construye en el
World, o en el contexto de feature o de suite si hay que compartirlo.

## Errores de Inyección

Si una propiedad marcada con `[Inject]` no puede ser inyectada, se lanza `EInjectionError`:

- Propiedad no es de tipo clase
- Propiedad no tiene setter
- No hay servicio registrado compatible con el tipo

---

[← Configuración](configuration.es.md) | [Siguiente: Reporters →](reporters.es.md)
