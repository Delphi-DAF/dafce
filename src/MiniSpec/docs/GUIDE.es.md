# MiniSpec - Guía de Usuario

**🌍 Idioma: [English](GUIDE.md) | Español**

Bienvenido a la guía de MiniSpec, el framework BDD para Delphi.

> 📖 **Ver también**: [Patrones de Testing](TESTING-PATTERNS.es.md) — Cómo usar BDD para tests unitarios, de integración y E2E.

---

## Tabla de Contenidos

### Primeros Pasos
- [**Primeros Pasos**](guide/getting-started.es.md) — ¿Por qué MiniSpec? Tu primera especificación. Ejecutando specs.

### Conceptos Fundamentales
- [**El World**](guide/world.es.md) — Compartiendo estado entre pasos de un escenario
- [**Scenario Outline**](guide/scenario-outline.es.md) — Múltiples ejemplos con tablas de datos
- [**DataTables**](guide/datatables.es.md) — Datos estructurados para pasos complejos

### Organización y Reutilización
- [**Step Bindings**](guide/step-bindings.es.md) — Pasos reutilizables con patrones regex
- [**Tags y Filtrado**](guide/tags-filtering.es.md) — Categorizar y filtrar escenarios
- [**Rules**](guide/rules.es.md) — Agrupar escenarios por regla de negocio

### Verificaciones
- [**Assertions (Expect)**](guide/assertions.es.md) — API fluida para verificaciones y excepciones

### Vocabulario y Marcadores
- [**Vocabulario Gherkin**](guide/gherkin-vocabulary.es.md) — Referencia completa de keywords
- [**Pending y NoAction**](guide/pending-noaction.es.md) — Steps sin implementar o descriptivos

### Configuración Avanzada
- [**Hooks (Before/After)**](guide/hooks.es.md) — Código de setup/teardown a nivel de Feature
- [**Configuración Global**](guide/configuration.es.md) — MiniSpec, SpecContext, FeatureContext
- [**Inyección de Dependencias**](guide/injection.es.md) — Sistema [Inject] integrado

### Ejecución y Salida
- [**Reporters**](guide/reporters.es.md) — Console, JSON, JUnit, Gherkin, Live Dashboard
- [**Línea de Comandos**](guide/cli.es.md) — Opciones de ejecución

---

## Quick Reference

| Concepto | Uso |
|----------|-----|
| Feature | `Feature('...')` — Funcionalidad a especificar |
| Scenario | `.Scenario('...')` — Ejemplo concreto |
| Given/When/Then | `.Given()`, `.When()`, `.&Then()` — Pasos |
| ScenarioOutline | `.ScenarioOutline('...').Examples([...])` — Data-driven |
| Background | `.Background` — Setup común |
| Rule | `.Rule('...')` — Agrupar por regla |
| Tags | `@tag` en descripción — Filtrado |
| World | `.UseWorld<T>` — Estado del escenario |

---

<p align="center">
  <sub><a href="../README.es.md">← Volver al README</a></sub>
</p>

