# MiniSpec Runner — Referencia completa

Para la documentación completa del runner de MiniSpec (CLI, filtros, reporters, etc.) consulta:

- [Guía de MiniSpec](../../../src/MiniSpec/docs/GUIDE.md)
- [Patrones de testing](../../../src/MiniSpec/docs/TESTING-PATTERNS.md)
- [README de MiniSpec](../../../src/MiniSpec/README.md)

## Resumen de opciones CLI

```bash
# Filtrar por tag
[Module]Specs.exe -f "@tag"

# Filtrar por nombre de escenario
[Module]Specs.exe -f "nombre del escenario"

# Stack trace en fallos
[Module]Specs.exe --stacktrace

# Reporters disponibles
[Module]Specs.exe -r console          # por defecto
[Module]Specs.exe -r gherkin
[Module]Specs.exe -r junit:output=results.xml
[Module]Specs.exe -r json:output=results.json
[Module]Specs.exe -r live             # live dashboard
```
