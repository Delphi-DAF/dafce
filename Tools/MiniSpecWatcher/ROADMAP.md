# Roadmap MiniSpec

## Piezas principales

---

## A) Daf.MiniSpec Framework

> **Unit Delphi para definir, ejecutar y reportar tests localmente en proyectos Delphi.**

### Responsabilidades
- Declarar features, escenarios y steps (DSL amigable).
- Ejecutar tests (`MiniSpec.Run`).
- Reporting local (consola, HTML, etc).
- Futuro: Reporter HTTP (cuando MiniSpecRunner esté disponible).

### Roadmap
- [x] DSL para features y escenarios (`Feature`, `Scenario`, etc.).
- [x] Reporting consola y HTML con placeholders resaltados.
- [x] Engine singleton y auto-registro de features.
- [ ] Helpers para tags, skips, parametrización (opcional).
- [ ] Reporter HTTP opcional, **solo cuando MiniSpecRunner esté disponible**.
- [ ] Documentación y ejemplos de uso.

---

## B) MiniSpecRunner

> **Ejecutable universal (minispec.exe) para gestionar ejecución, monitorización y reporting centralizado de specs.**

### Responsabilidades
- CLI para lanzar, monitorizar y reportar specs:
  - Ejemplo: `minispec.exe --watch --report html --tags suma --paths src;test`
- Watcher de archivos/directorios (detecta cambios).
- Compilación automática de proyectos de test afectados.
- Ejecución automatizada de los tests (con parámetros para reporter HTTP).
- Servicio HTTP local (API REST) para recibir eventos de reporter HTTP.
- Centralización y presentación de resultados (HTML, dashboard web, CI).

### Roadmap
- [ ] CLI mínima (parámetros: watch, report, tags, paths).
- [ ] Watcher de paths/directorios.
- [ ] Compilación automática (mapeo fuente → proyecto test).
- [ ] Lanzamiento automatizado de ejecutables de test.
- [ ] Servicio HTTP local para recibir eventos y reportar progreso/resultados.
- [ ] Generación y guardado de reportes centralizados (HTML, XML, JSON, etc).
- [ ] Dashboard web opcional (SPA/simple HTML).
- [ ] Documentación y ejemplos de integración.

---

## Interfaz/protocolo entre piezas

- Definido por la API del reporter HTTP: envío de eventos de test como JSON vía POST al runner.
- Contrato/versionado de mensajes documentado (para evolucionar ambas piezas por separado).

---

## Esquema visual

```mermaid
graph TD
  subgraph ProyectoDelphi
    A[Daf.MiniSpec Framework]
  end
  subgraph Standalone
    B[MiniSpecRunner (EXE)]
  end
  A -- opcional, eventos HTTP --> B
