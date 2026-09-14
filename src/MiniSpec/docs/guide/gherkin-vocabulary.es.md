# Vocabulario Gherkin

**🌍 Idioma: [English](gherkin-vocabulary.md) | Español**

[← Volver a la Guía](../GUIDE.es.md)

---

Referencia completa del vocabulario Gherkin en MiniSpec:

| Palabra clave | MiniSpec | Descripción |
|---------------|----------|-------------|
| Feature | `Feature('...')` | Funcionalidad que se especifica |
| Rule | `.Rule('...')` | Agrupa escenarios bajo una regla de negocio |
| Background | `.Background` | Pasos comunes a todos los escenarios |
| Scenario | `.Scenario('...')` | Un ejemplo concreto de comportamiento |
| Scenario Outline | `.ScenarioOutline('...')` | Ejemplo parametrizado con tabla |
| Examples | `.Examples([...])` | Tabla de datos para el Outline |
| DataTable | Tras la descripción del step | Tabla de datos estructurados para un step |
| Given | `.Given('...', proc)` | Establece el contexto inicial |
| When | `.When('...', proc)` | La acción que se prueba |
| Then | `.&Then('...', proc)` | El resultado esperado (`&` requerido) |
| And | `.&And('...', proc)` | Paso adicional (`&` requerido) |
| But | `.But('...', proc)` | Excepción o negación |
| @tag | En descripción | Para filtrar escenarios |

> 💡 El `&` antes de `Then` y `And` es necesario porque son palabras reservadas en Delphi.

## Qué afirma cada fase

Un escenario afirma el comportamiento de **un evento**. Las tres fases son esa afirmación repartida,
así que cada una tiene su trabajo y una pieza que no le toca:

| Fase | Afirma | **No** le toca |
|---|---|---|
| `Given` | el estado que el evento necesita para tener sentido | provocar el evento |
| `When` | el evento, uno y nombrado | comprobar nada |
| `Then` | lo que se puede observar después | provocar nada |

**La prueba, escenario a escenario: nombra el evento.** ¿Cuál es la acción cuyo comportamiento afirmo
aquí? Si se puede nombrar, va al `When`. Si no se puede, el escenario no tiene sujeto — y eso no se
arregla moviendo el código a otra fase.

⚠️ **Un escenario con el evento fuera del `When` sigue pasando y fallando**, así que no hay rojo que
te lo diga. Lo que se pierde es esto:

- **Evento en el `Given`** — un fallo del evento se informa como fallo de montaje, y manda al lector
  al sitio equivocado. Y el `Then` solo puede comparar lo que el `Given` dejó escrito, así que la
  aserción mide el montaje en vez del sujeto.
- **Evento en el `Then`** — el fallo se informa como aserción fallida. Con varios `Then`, o cada uno
  repite el evento —y entonces son eventos distintos, no el mismo observado dos veces— o solo el
  primero lo ejecuta.

Ver [Pending y NoAction](pending-noaction.es.md) para qué significa dejar una fase vacía.

## Features con Narrativa

Una Feature puede incluir una narrativa que explica el **por qué**:

```pascal
Feature('''
Calculadora - División

  Como usuario de la calculadora
  Necesito poder dividir números
  Para resolver problemas matemáticos

  @math @division
''')
```

La primera línea es el título. MiniSpec toma el resto como narrativa. Los tags pueden aparecer en cualquier sitio de la descripción pero se recomienda ponerlos al final.

## Category para Filtrado

Asigna categorías para filtrar con `Cat:texto`. Puedes usar un string o una clase como marcador:

```pascal
// Con string
Feature('Login')
  .Category('auth')
  .UseWorld<TLoginWorld>

// Con clase marcadora
type
  TIntegrationTest = class end;  // Clase vacía como marcador

Feature('Database operations')
  .Category(TIntegrationTest)  // Usa QualifiedClassName: 'Unit1.TIntegrationTest'
  .UseWorld<TDbWorld>
```

Filtrar por categoría:

```bash
MiApp.exe -f "Cat:auth"              # Features con categoría 'auth'
MiApp.exe -f "Cat:Integration"       # Coincide con 'TIntegrationTest'
```

> 💡 Las clases marcadoras son útiles para categorías que se usan en múltiples archivos: el compilador detecta errores tipográficos.

---

[← Rules](rules.es.md) | [Siguiente: Pending y NoAction →](pending-noaction.es.md)
