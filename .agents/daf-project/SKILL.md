---
name: daf-project
description: Estructura, reglas y principios del monorepo DAFce (Delphi Application Framework - Community Edition). Usar cuando se trabaja en cualquier módulo del framework, se crean nuevos ficheros, o se toman decisiones de arquitectura.
license: MIT
metadata:
  author: Jorge L. Cangas
  email: delphi-daf@proton.me
  repo: Delphi-DAF/dafce
  version: "1.0"
---

# DAFce — Proyecto y Estructura

## Reglas de Oro

1. **Cada paso debe compilar y pasar tests** antes de continuar
2. **Commits pequeños** — un commit por paso completado
3. **Tests primero** cuando sea posible (BDD)
4. **No romper API existente** hasta que el nuevo modelo funcione
5. Los samples y los tests dependen del framework: si se modifica el framework, se debe recompilar antes que los samples y tests

## Estructura objetivo del Monorepo

> Esta es la estructura **deseada**. La migración está en curso: no todos los módulos la cumplen todavía.
> Cuando se cree o modifique un módulo, debe adaptarse a este patrón.

```
src/
├── source.optset              # Optset para código fuente
├── samples.optset             # Optset para samples (centralizado)
├── test.optset                # Optset para tests (centralizado)
├── DAFGroup.groupproj         # Agrupa todos los módulos del framework
├── DAFSamples.groupproj       # Agrupa TODOS los samples (transversal)
├── DAFTestGroup.groupproj     # Agrupa TODOS los tests (transversal)
│
└── [Module]/
    ├── Daf.[Module].dproj             # Proyecto del módulo
    ├── Daf.[Module].dpk               # Paquete del módulo
    ├── [Module].Samples.groupproj     # Compila samples del módulo
    ├── [Module].Tests.groupproj       # Compila tests del módulo
    ├── README.md                      # Documentación principal (en)
    ├── README.es.md                   # Documentación principal (es)
    ├── docs/                          # Documentación extendida
    │   ├── GUIDE.md
    │   ├── GUIDE.es.md
    │   └── ...
    ├── samples/
    │   └── [SampleName]/
    │       └── [SampleName].dproj
    └── test/
        └── [Module]Specs.dproj
```

## Archivos clave

| Archivo | Ubicación | Propósito |
|---------|-----------|-----------|
| `source.optset` | `src/` | Configuración de compilación para código fuente |
| `samples.optset` | `src/` | Configuración para todos los samples |
| `test.optset` | `src/` | Configuración para todos los tests |
| `DAFSamples.groupproj` | `src/` | Compila TODOS los samples del proyecto |
| `DAFTestGroup.groupproj` | `src/` | Compila TODOS los tests del proyecto |
| `[Module].Samples.groupproj` | `src/[Module]/` | Compila solo samples de ese módulo |
| `[Module].Tests.groupproj` | `src/[Module]/` | Compila solo tests de ese módulo |


