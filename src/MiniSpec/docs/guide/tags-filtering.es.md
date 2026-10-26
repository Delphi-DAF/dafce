# Tags y Filtrado

**🌍 Idioma: [English](tags-filtering.md) | Español**

[← Volver a la Guía](../GUIDE.es.md)

---

Los **tags** permiten categorizar y filtrar escenarios. Añádelos en la descripción, preferiblemente al final:

```pascal
Feature('''
Calculadora

  @math @core
''')

.Scenario('''
  División por cero
  @error @edge-case
''')
```

## Ejecutando con Filtros

```bash
# Solo escenarios con @unit
MiApp.exe -f "@unit"

# Escenarios @unit pero NO @slow
MiApp.exe -f "@unit and ~@slow"

# Por título de feature
MiApp.exe -f "Feat:Calculator"

# Por categoría
MiApp.exe -f "Cat:Login"

# Expresiones complejas
MiApp.exe -f "(Feat:Login or @auth) and ~@slow"
```

## Sintaxis de Filtros

| Expresión | Significado |
|-----------|-------------|
| `@tag` | Tiene el tag |
| `~@tag` | NO tiene el tag |
| `Feat:texto` | Título de feature contiene texto |
| `Scen:texto` | Descripción de scenario contiene texto |
| `Rule:texto` | Descripción de rule contiene texto |
| `Cat:texto` | Categoría contiene texto |
| `and`, `or` | Operadores lógicos |
| `()` | Agrupación |

> ⚠️ Los tags **deben** comenzar con `@`. Escribir `--filter unit` dará error; usa `--filter @unit`.

⚠️ **El texto tras `Feat:`, `Scen:`, `Rule:` o `Cat:` es una sola palabra, y no hay forma de
entrecomillarlo.** La expresión se parte por espacios, así que `-f "Scen:releases one reference"`
lee `Scen:releases` y aborta en la palabra siguiente con
`Token inesperado en expresión de filtro: one` — cero specs ejecutados, que en un script se lee
igual que una suite que pasó. Elige una palabra que la descripción contenga.

---

[← Step Bindings](step-bindings.es.md) | [Siguiente: Assertions →](assertions.es.md)
